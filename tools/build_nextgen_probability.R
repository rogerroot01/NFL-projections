# Standalone build. The Wrangler reads the output; it never fits a model.
options(encoding = "UTF-8")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L || length(args) > 3L) {
  stop("Usage: Rscript tools/build_nextgen_probability.R <completed Next Gen audit directory> <new output.rds> [prior probability.rds]")
}
framework <- match.arg(Sys.getenv("NEXTGEN_PROBABILITY_FRAMEWORK", "early"), c("early", "late"))
if (framework == "late" && length(args) != 3L) {
  stop("The late refit requires a prior probability bundle with locked settings.")
}
audit <- normalizePath(args[1], winslash = "/", mustWork = TRUE)
output <- file.path(normalizePath(dirname(args[2]), winslash = "/", mustWork = TRUE), basename(args[2]))
stopifnot(!file.exists(output))
sha256 <- function(path) digest::digest(file = path, algo = "sha256")
receipt <- function(path) jsonlite::read_json(file.path(audit, path))
features_path <- file.path(audit, "feature_ready/dataV12.RData")
score_path <- file.path(audit, "review/nextgen_clean_refit_models.rds")
feature_gate <- receipt("feature_ready/feature_gate.json")
mutation_gate <- receipt("feature_ready/asymmetric_audit.json")
comparison <- receipt("review/comparison_receipt.json")
review <- receipt("review/manual_numeric_review.json")
feature_hash <- sha256(features_path)
score_hash <- sha256(score_path)
stopifnot(identical(feature_gate$status, "passed"), identical(mutation_gate$status, "passed"),
          isTRUE(feature_gate$training_only_imputation),
          identical(feature_gate$data_sha256, feature_hash),
          identical(mutation_gate$data_sha256, feature_hash),
          identical(comparison$status, "comparison_prepared"),
          identical(comparison$feature_sha256, feature_hash),
          identical(comparison$rds_sha256, score_hash),
          identical(review$status, "approved_for_wrangler"), identical(review$rds_sha256, score_hash),
          identical(receipt("refit_status.json")$status, "fits_complete"))
cat("Verified completed Next Gen handoff", feature_hash, "\n")

# Use the original spread feature selection and preprocessing, with a binary target.
helpers <- new.env()
helper_path <- file.path(audit, "fits", "2024", framework, "A", "tune", "00_v12_modeling_helpers.R")
# The shared helper creates a default output directory on source. Confine it to scratch.
source_helpers <- function() {
  previous <- setwd(tempdir())
  on.exit(setwd(previous))
  sys.source(helper_path, envir = helpers)
}
source_helpers()
helpers$QUIET_MODEL_RUN <- TRUE
min_train_week <- helpers$MIN_TRAIN_WEEK
stopifnot(min_train_week == switch(framework, early = 1, late = 9))
cat("Fitting", framework, "framework using historical Week", min_train_week, "onward\n")
games <- helpers$collapse_to_game_level(helpers$load_v12_data(features_path))
stopifnot(!anyNA(games$game_id), !anyDuplicated(games$game_id))
games$actual <- ifelse(games$home_score > games$away_score, 1L,
                       ifelse(games$home_score < games$away_score, 0L, NA_integer_))
blocked <- unique(c(unlist(feature_gate$blocked_fields), helpers$exact_leakage_cols,
                    helpers$market_cols, "season", "actual"))

training <- function(season) {
  games[games$season < season & !games$season %in% helpers$EXCLUDE_TRAIN_SEASONS &
          games$week >= min_train_week & !is.na(games$actual), , drop = FALSE]
}

# Read the actual audited production spread grids, including the guarded
# weighted-regression feature counts applied by the clean refit controller.
production_controls <- function(family, keys) {
  path <- file.path(audit, "fits", "2024", "early", family, "tune", "model.R")
  expr <- as.list(parse(path, encoding = "UTF-8"))
  assignments <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("<-")) &&
                          is.symbol(x[[2]]) && as.character(x[[2]]) %in% keys, expr)
  values <- new.env(parent = baseenv())
  for (assignment in assignments) eval(assignment, values)
  stopifnot(setequal(ls(values), keys))
  as.list(values)
}
a <- production_controls("A", c("ALPHA_GRID", "LAMBDA_CHOICES", "N_FOLDS"))
b <- production_controls("B", c("FEATURE_COUNT_GRID", "WEIGHT_METHODS"))
c <- production_controls("C", c("MAX_FEATURES_TREE", "CP_GRID", "MAXDEPTH_GRID", "MINBUCKET_GRID"))
d <- production_controls("D", c("MAX_FEATURES_RF", "NUM_TREES", "MTRY_FRACTIONS",
                                "MIN_NODE_GRID", "SAMPLE_FRACTION_GRID"))
e <- production_controls("E", c("MAX_FEATURES_GBM", "N_TREES_GRID", "DEPTH_GRID", "SHRINKAGE_GRID",
                                "MINOBS_GRID", "BAG_FRACTION"))
f <- production_controls("F", c("MAX_FEATURES_XGB", "XGB_GRID", "NROUNDS_MAX",
                                "EARLY_STOPPING_ROUNDS", "NFOLD"))
grid <- list(
  "Elastic Net / Lasso" = expand.grid(alpha = a$ALPHA_GRID, lambda_choice = a$LAMBDA_CHOICES),
  "Weighted Logistic Regression" = expand.grid(features = b$FEATURE_COUNT_GRID, weights = b$WEIGHT_METHODS),
  "Decision Tree" = expand.grid(cp = c$CP_GRID, depth = c$MAXDEPTH_GRID, bucket = c$MINBUCKET_GRID),
  "Random Forest" = expand.grid(mtry_fraction = d$MTRY_FRACTIONS, node = d$MIN_NODE_GRID,
                               sample_fraction = d$SAMPLE_FRACTION_GRID),
  "GBM Boosted Trees" = expand.grid(trees = e$N_TREES_GRID, depth = e$DEPTH_GRID,
                                   rate = e$SHRINKAGE_GRID, minobs = e$MINOBS_GRID),
  "XGBoost" = f$XGB_GRID
)
feature_limits <- c(Inf, Inf, c$MAX_FEATURES_TREE, d$MAX_FEATURES_RF,
                    e$MAX_FEATURES_GBM, f$MAX_FEATURES_XGB)
names(feature_limits) <- names(grid)
seed <- 20260512L

model_data <- function(train, forecast) {
  stopifnot(nrow(train) > 0, nrow(forecast) > 0, max(train$season) < min(forecast$season))
  predictors <- setdiff(helpers$select_predictors(train, "side", "actual"), blocked)
  stopifnot(length(predictors) >= max(b$FEATURE_COUNT_GRID), !any(predictors %in% blocked))
  frames <- helpers$median_impute(train, list(forecast = forecast), predictors)
  list(train = as.matrix(frames$train), forecast = as.matrix(frames$other$forecast),
       actual = train$actual, rows = train)
}

family_matrices <- function(family, config, data) {
  count <- min(ncol(data$train), feature_limits[[family]])
  if (!is.null(config$features)) count <- min(count, config$features)
  columns <- seq_len(count)
  list(train = data$train[, columns, drop = FALSE], forecast = data$forecast[, columns, drop = FALSE])
}

xgb_params <- function(config) {
  list(objective = "binary:logistic", eval_metric = "logloss", max_depth = config$max_depth,
       eta = config$eta, min_child_weight = config$min_child_weight, subsample = config$subsample,
       colsample_bytree = config$colsample_bytree, lambda = 1, alpha = 0, nthread = 2L)
}

predict_probability <- function(family, config, data) {
  matrices <- family_matrices(family, config, data)
  x <- matrices$train
  future <- matrices$forecast
  y <- data$actual
  set.seed(seed)
  probability <- switch(family,
    "Elastic Net / Lasso" = {
      fit <- glmnet::glmnet(x, y, family = "binomial", alpha = config$alpha,
                            lambda = config$lambda, standardize = TRUE)
      predict(fit, future, type = "response", s = config$lambda)
    },
    "Weighted Logistic Regression" = {
      weights <- helpers$sample_weights(data$rows, config$weights)
      # A fixed ridge penalty stabilizes the logistic fit for correlated spread inputs.
      fit <- glmnet::glmnet(x, y, family = "binomial", alpha = 0, lambda = 0.001, weights = weights)
      predict(fit, future, type = "response", s = 0.001)
    },
    "Decision Tree" = {
      fit <- rpart::rpart(outcome ~ ., data.frame(outcome = factor(y), x), method = "class",
                         control = rpart::rpart.control(maxdepth = config$depth,
                           minbucket = config$bucket, cp = config$cp, xval = 10L))
      predict(fit, as.data.frame(future), type = "prob")[, "1"]
    },
    "Random Forest" = {
      fit <- ranger::ranger(x = as.data.frame(x), y = factor(y), probability = TRUE,
                           num.trees = d$NUM_TREES, min.node.size = config$node,
                           sample.fraction = config$sample_fraction,
                           mtry = max(1L, floor(ncol(x) * config$mtry_fraction)),
                           importance = "permutation", num.threads = 2L, seed = seed)
      predict(fit, as.data.frame(future), num.threads = 2L)$predictions[, "1"]
    },
    "GBM Boosted Trees" = {
      fit <- gbm::gbm.fit(x, y, distribution = "bernoulli", n.trees = config$trees,
                         interaction.depth = config$depth, n.minobsinnode = config$minobs,
                         shrinkage = config$rate, bag.fraction = e$BAG_FRACTION, verbose = FALSE)
      gbm::predict.gbm(fit, as.data.frame(future), n.trees = config$trees, type = "response")
    },
    "XGBoost" = {
      fit <- xgboost::xgb.train(xgb_params(config), xgboost::xgb.DMatrix(x, label = y, nthread = 2L),
                                nrounds = config$nrounds, verbose = 0L)
      predict(fit, xgboost::xgb.DMatrix(future, nthread = 2L))
    }
  )
  probability <- as.numeric(probability)
  stopifnot(length(probability) == nrow(future), all(is.finite(probability)),
            all(probability >= 0 & probability <= 1))
  probability
}

log_loss <- function(actual, probability) {
  correct_probability <- ifelse(actual == 1, probability, 1 - probability)
  -mean(log(pmax(1e-6, correct_probability)))
}

tuning_season <- 2023L
search_spec <- list(grid = grid, controls = list(A = a, B = b, C = c, D = d, E = e, F = f), seed = seed)
if (length(args) == 3L) {
  previous <- readRDS(args[3])
  stopifnot(identical(previous$schema, 1L), identical(previous$tuning_season, tuning_season),
            identical(previous$search_spec, search_spec))
  selected <- previous$settings
  tuning_results <- previous$tuning_results
  stopifnot(identical(names(selected), names(grid)))
  tuning_fit_seasons <- previous$tuning_fit_seasons
  settings_source_sha256 <- sha256(args[3])
  cat("Refitting with locked 2023 classification settings. No hyperparameter search.\n")
} else {
  cat("Production search", sum(vapply(grid, nrow, integer(1))), "candidate settings across", length(grid), "families\n")
  tuning_train <- training(tuning_season)
  tuning_fit_seasons <- sort(unique(tuning_train$season))
  tuning_games <- games[games$season == tuning_season & !is.na(games$actual), , drop = FALSE]
  settings_source_sha256 <- NULL
  tune <- model_data(tuning_train, tuning_games)
  script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  search_key <- digest::digest(list(feature_hash, sha256(helper_path), search_spec,
                                    sha256(sub("^--file=", "", script_arg))), algo = "sha256")
  checkpoints <- paste0(output, ".search")
  dir.create(checkpoints, showWarnings = FALSE)
  # CV sees only the pre-2023 fitting partition. The 2023 season selects finalists.
  tune_candidate <- function(family, config) {
    if (family == "Elastic Net / Lasso") {
      set.seed(seed)
      cv <- glmnet::cv.glmnet(tune$train, tune$actual, family = "binomial", alpha = config$alpha,
                             nfolds = a$N_FOLDS, type.measure = "deviance", standardize = TRUE)
      config$lambda <- cv[[as.character(config$lambda_choice)]]
    } else if (family == "XGBoost") {
      set.seed(seed)
      matrices <- family_matrices(family, config, tune)
      cv <- xgboost::xgb.cv(xgb_params(config),
                            xgboost::xgb.DMatrix(matrices$train, label = tune$actual, nthread = 2L),
                            nrounds = f$NROUNDS_MAX, nfold = f$NFOLD, stratified = TRUE,
                            early_stopping_rounds = f$EARLY_STOPPING_ROUNDS, verbose = 0L)
      config$nrounds <- cv$best_iteration
      stopifnot(length(config$nrounds) == 1L, config$nrounds >= 1L)
    }
    config$log_loss <- log_loss(tuning_games$actual, predict_probability(family, config, tune))
    config
  }
  family_range <- Sys.getenv("NEXTGEN_PROBABILITY_SEARCH_FAMILIES")
  family_indices <- if (nzchar(family_range)) match(strsplit(family_range, "", fixed = TRUE)[[1]], LETTERS) else seq_along(grid)
  stopifnot(!anyNA(family_indices), !anyDuplicated(family_indices), all(family_indices <= length(grid)))
  tuning_results <- setNames(lapply(family_indices, function(j) {
    family <- names(grid)[j]
    choices <- grid[[j]]
    cat(sprintf("Tuning %-28s %d candidates\n", family, nrow(choices)))
    results <- lapply(seq_len(nrow(choices)), function(i) {
      checkpoint <- file.path(checkpoints, sprintf("%s-%03d.rds", LETTERS[j], i))
      if (file.exists(checkpoint)) {
        old <- readRDS(checkpoint)
        stopifnot(identical(old$key, search_key), identical(old$family, family))
        return(old$result)
      }
      result <- tune_candidate(family, choices[i, , drop = FALSE])
      scratch <- tempfile(tmpdir = checkpoints)
      saveRDS(list(key = search_key, family = family, result = result), scratch)
      stopifnot(file.rename(scratch, checkpoint))
      if (i %% 12L == 0L) cat(family, i, "of", nrow(choices), "\n")
      result
    })
    do.call(rbind, results)
  }), names(grid)[family_indices])
  if (nzchar(family_range)) {
    cat("Completed assigned family searches", family_range, "\n")
    quit(save = "no")
  }
  selected <- lapply(tuning_results, function(results) {
    best <- which.min(results$log_loss)
    list(config = results[best, setdiff(names(results), "log_loss"), drop = FALSE],
         log_loss = results$log_loss[best])
  })
}
stopifnot(identical(vapply(tuning_results, nrow, integer(1)), vapply(grid, nrow, integer(1))))

predictions <- list()
fit_audit <- list()
for (season in 2024:2025) {
  train <- training(season)
  cat("Training for", season, "on", nrow(train), "prior games\n")
  forecast <- games[games$season == season, , drop = FALSE]
  data <- model_data(train, forecast)
  for (family in names(grid)) {
    config <- selected[[family]]$config
    probability <- predict_probability(family, config, data)
    key <- paste(season, family, sep = "|")
    predictions[[key]] <- data.frame(forecast[c("game_id", "season", "week", "home_team", "away_team",
                                                 "home_score", "away_score", "actual")],
                                     family = family, home_win_probability = probability)
    fit_audit[[key]] <- list(season = season, family = family, train_seasons = sort(unique(train$season)),
                             train_weeks = sort(unique(train$week)), training_games = nrow(train),
                             forecast_games = nrow(forecast),
                             features = colnames(family_matrices(family, config, data)$train))
    cat("Backtest complete", season, family, nrow(forecast), "games\n")
  }
}
bundle <- list(schema = 1L, framework = framework, min_train_week = min_train_week,
               settings_source_sha256 = settings_source_sha256,
               predictions = do.call(rbind, predictions), settings = selected,
               search_spec = search_spec, tuning_results = tuning_results,
               tuning_season = tuning_season, tuning_fit_seasons = tuning_fit_seasons,
               feature_sha256 = feature_hash, spread_export_sha256 = score_hash,
               feature_helper_sha256 = sha256(helper_path), fit_audit = fit_audit)
stopifnot(nrow(bundle$predictions) > 0, !anyDuplicated(bundle$predictions[c("game_id", "family")]))
temporary <- tempfile("nextgen-probability-", tmpdir = dirname(output))
saveRDS(bundle, temporary, version = 3)
stopifnot(!file.exists(output), file.rename(temporary, output))
cat("Probability backtest written to", output, "\n")