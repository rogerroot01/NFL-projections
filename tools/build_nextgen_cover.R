# Standalone build. The Wrangler reads the output; it never fits a model.
# Build the six Early ATS probability families from sealed pregame inputs.
options(encoding = "UTF-8")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4L) {
  stop("Usage: Rscript build_nextgen_cover.R <audited Next Gen build> <weekly build> <win_cover|cover> <output.rds>")
}
framework <- "early"
weekly <- normalizePath(args[[2L]], winslash = "/", mustWork = TRUE)
target <- match.arg(args[[3L]], c("win_cover", "cover"))
args[2L] <- args[4L]
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
# Each game contributes both audited team-perspective rows. All splits keep games intact.
team_games <- function(path) {
  rows <- as.data.frame(helpers$load_v12_data(path))
  is_home <- rows$posteam == rows$home_team
  stopifnot(!anyNA(rows$game_id), !anyNA(is_home),
    !anyDuplicated(rows[c("game_id", "posteam")]), all(table(rows$game_id) == 2L),
    all(rows$posteam == rows$away_team | is_home), all(table(rows$game_id[is_home]) == 1L),
    !anyDuplicated(rows[c("game_id", "posteam")]),
    all(rows$spread_line[is_home][match(rows$game_id[!is_home], rows$game_id[is_home])] ==
        rows$spread_line[!is_home] | is.na(rows$spread_line[!is_home])))
  rows$team_is_home <- as.integer(is_home)
  direction <- ifelse(is_home, 1, -1)
  rows$team_cover_threshold <- direction * helpers$market_home_margin_from_spread(rows$spread_line)
  margin <- direction * (rows$home_score - rows$away_score)
  rows$cover <- as.integer(margin > rows$team_cover_threshold)
  rows$win_cover <- as.integer(margin > 0 & margin > rows$team_cover_threshold)
  rows$actual <- rows[[target]]
  stopifnot(all(rows$win_cover <= rows$cover, na.rm = TRUE))
  rows
}
games <- team_games(features_path)
weekly_ready <- file.path(weekly, "feature_ready")
weekly_features <- file.path(weekly_ready, "dataV12.RData")
weekly_gate <- jsonlite::read_json(file.path(weekly_ready, "feature_gate.json"))
weekly_fit <- jsonlite::read_json(file.path(weekly_ready, "fixed_fit_input_receipt.json"))
weekly_hash <- sha256(weekly_features)
stopifnot(identical(weekly_gate$status, "passed"), identical(weekly_fit$status, "passed"),
          identical(weekly_gate$data_sha256, weekly_hash),
          identical(weekly_fit$sealed_fit_feature_sha256, feature_hash),
          isTRUE(weekly_gate$training_only_imputation), weekly_fit$fit_through == 2025L)
latest <- team_games(weekly_features)
blocked <- unique(c(unlist(feature_gate$blocked_fields), unlist(weekly_gate$blocked_fields),
  helpers$exact_leakage_cols, helpers$market_cols, "season", "actual", "cover", "win_cover"))

training <- function(season) {
  games[games$season < season & !games$season %in% helpers$EXCLUDE_TRAIN_SEASONS &
          games$week >= min_train_week & !is.na(games$actual) &
          is.finite(games$team_cover_threshold), , drop = FALSE]
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
  predictors <- unique(c("team_cover_threshold", "team_is_home",
    setdiff(helpers$select_predictors(train, "side", "actual"), blocked)))
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
                           importance = "none", num.threads = 2L, seed = seed)
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
  cat("Production search", sum(vapply(grid, nrow, integer(1))), "candidate settings across", length(grid), "families\n")
  tuning_train <- training(tuning_season)
  tuning_fit_seasons <- sort(unique(tuning_train$season))
  tuning_games <- games[games$season == tuning_season & !is.na(games$actual) &
    is.finite(games$team_cover_threshold), , drop = FALSE]
  tune <- model_data(tuning_train, tuning_games)
  make_folds <- function(count) {
    ids <- unique(tuning_train$game_id)
    set.seed(seed)
    levels <- sample(rep(seq_len(count), length.out = length(ids)))
    levels[match(tuning_train$game_id, ids)]
  }
  elastic_folds <- make_folds(a$N_FOLDS)
  xgb_folds <- unname(split(seq_len(nrow(tuning_train)), make_folds(f$NFOLD)))
  script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  search_key <- digest::digest(list(feature_hash, sha256(helper_path), search_spec, target,
                                    sha256(sub("^--file=", "", script_arg))), algo = "sha256")
  checkpoints <- paste0(output, ".search")
  dir.create(checkpoints, showWarnings = FALSE)
  # CV sees only the pre-2023 fitting partition. The 2023 season selects finalists.
  elastic_cv <- new.env(parent = emptyenv())
  gbm_path <- new.env(parent = emptyenv())
  tune_candidate <- function(family, config) {
    if (family == "GBM Boosted Trees") {
      key <- paste(config$depth, config$rate, config$minobs, sep = ":")
      previous_fit <- get0(key, envir = gbm_path, inherits = FALSE)
      if (is.null(previous_fit)) {
        x <- family_matrices(family, config, tune)
        set.seed(seed)
        previous_fit <- gbm::gbm.fit(x$train, tune$actual, distribution = "bernoulli",
          n.trees = max(e$N_TREES_GRID), interaction.depth = config$depth,
          n.minobsinnode = config$minobs, shrinkage = config$rate,
          bag.fraction = e$BAG_FRACTION, verbose = FALSE)
        assign(key, previous_fit, envir = gbm_path)
      }
      predictions <- gbm::predict.gbm(previous_fit, as.data.frame(
        family_matrices(family, config, tune)$forecast), n.trees = config$trees, type = "response")
      config$log_loss <- log_loss(tuning_games$actual, predictions)
      return(config)
    }
    if (family == "Elastic Net / Lasso") {
      set.seed(seed)
      key <- as.character(config$alpha)
      cv <- get0(key, envir = elastic_cv, inherits = FALSE)
      if (is.null(cv)) {
        cv <- glmnet::cv.glmnet(tune$train, tune$actual, family = "binomial", alpha = config$alpha,
          foldid = elastic_folds, type.measure = "deviance", standardize = TRUE)
        assign(key, cv, envir = elastic_cv)
      }
      config$lambda <- cv[[as.character(config$lambda_choice)]]
    } else if (family == "XGBoost") {
      set.seed(seed)
      matrices <- family_matrices(family, config, tune)
      cv <- xgboost::xgb.cv(xgb_params(config),
                            xgboost::xgb.DMatrix(matrices$train, label = tune$actual, nthread = 2L),
                            nrounds = f$NROUNDS_MAX, folds = xgb_folds,
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
stopifnot(identical(vapply(tuning_results, nrow, integer(1)), vapply(grid, nrow, integer(1))))

predictions <- list()
fit_audit <- list()
for (season in 2024:2026) {
  train <- training(season)
  snapshot <- if (season == 2026L) latest else games
  forecast <- snapshot[snapshot$season == season & is.finite(snapshot$team_cover_threshold), , drop = FALSE]
  cat("Fitting", target, season, "from", length(unique(train$game_id)), "prior games\n")
  data <- model_data(train, forecast)
  for (family in names(grid)) {
    config <- selected[[family]]$config
    key <- paste(season, family, sep = "|")
    result <- forecast[c("game_id", "season", "week", "home_team", "away_team",
                         "posteam", "team_is_home", "home_score", "away_score",
                         "spread_line", "team_cover_threshold", "actual")]
    predictions[[key]] <- data.frame(result, family = family,
      probability = predict_probability(family, config, data))
    fit_audit[[key]] <- list(season = season, family = family,
      train_seasons = sort(unique(train$season)), training_games = length(unique(train$game_id)),
      forecast_games = length(unique(forecast$game_id)),
      features = colnames(family_matrices(family, config, data)$train))
    cat("Refit complete", target, season, family, nrow(forecast), "team rows\n")
  }
}

rows <- do.call(rbind, predictions)
stopifnot(nrow(rows) > 0L, !anyDuplicated(rows[c("game_id", "family", "posteam")]),
          all(table(rows$game_id) == 2L * length(grid)),
          all(is.na(rows$actual) == (!is.finite(rows$home_score) | !is.finite(rows$away_score))))
bundle <- list(schema = 1L, framework = framework, target = target,
  predictions = rows, settings = selected, tuning_results = tuning_results,
  search_spec = search_spec, tuning_season = tuning_season, tuning_fit_seasons = tuning_fit_seasons,
  feature_sha256 = feature_hash, latest_feature_sha256 = weekly_hash,
  spread_export_sha256 = score_hash, feature_helper_sha256 = sha256(helper_path),
  fit_audit = fit_audit)
temporary <- tempfile("nextgen-cover-", tmpdir = dirname(output))
saveRDS(bundle, temporary, version = 3)
stopifnot(!file.exists(output), file.rename(temporary, output))
cat("Probability bundle complete:", target, output, "\n")
