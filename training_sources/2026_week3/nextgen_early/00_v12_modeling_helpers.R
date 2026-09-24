
# ============================================================
# V12 projection modeling helper functions
# ============================================================
# Shared by the model-family Rmds so the Rmd output stays readable.
# The goal is: fit models, show compact leaderboards, show final variables,
# and score historical performance against spread/total/team-total lines.

options(stringsAsFactors = FALSE)
options(width = 180)

# ---------------- User controls ----------------
DATA_FILE_CANDIDATES <- c(
  "../output/dataV12.RData",
  "output/dataV12.RData",
  "./dataV12.RData",
  "../data/dataV12.RData",
  "data/dataV12.RData"
)

OUT_DIR <- file.path("output", "v12_model_search")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# Train / test / validation split.
# Default requested setup: train = seasons before 2024, test = 2024, validation = 2025.
TEST_SEASON <- 2025
VAL_SEASON <- 2026
VAL_MAX_WEEK <- Inf      # set to 9 if you only want 2025 through week 9
NEXTGEN_PRODUCTION_REFIT <- toupper(Sys.getenv("NEXTGEN_PRODUCTION_REFIT", "FALSE")) %in% c("TRUE", "1", "YES", "Y")
MIN_TRAIN_WEEK <- 1      # set to 9 to recreate the late-season Step 8 style training filter
TRAIN_WEEK_PHASE_CUTOFF <- 7  # MIN_TRAIN_WEEK < 7 => early output tag; MIN_TRAIN_WEEK >= 7 => late output tag
EXCLUDE_2020 <- TRUE     # easiest COVID-year toggle; change to FALSE to include 2020 in training
EXCLUDE_TRAIN_SEASONS <- if (isTRUE(EXCLUDE_2020)) c(2020) else integer(0)

# Data grain / feature controls.
KEEP_HOME_TEAM_ROW_ONLY <- TRUE
INCLUDE_MARKET_LINES_AS_FEATURES <- FALSE
INCLUDE_WEEK_AS_FEATURE <- TRUE

# Market-line convention.
# IMPORTANT: Your legacy Step 8 file uses:
#   home_implied <- total_line / 2 + spread_line / 2
#   away_implied <- total_line / 2 - spread_line / 2
# That means this pipeline's spread_line is being treated as expected HOME MARGIN:
#   positive = home favored, negative = home underdog.
# Keep the default below at "home_margin" to match Step 8.
# Only use "nflverse_home_spread" if your raw spread_line is the nflverse-style home spread
# where negative means home favored.
SPREAD_LINE_CONVENTION <- "home_margin"  # choices: "home_margin" or "nflverse_home_spread"
WRITE_MARKET_BASELINE_CSV <- TRUE

# Output naming convention for early-vs-late training windows.
# The user wants separate output families when running with early-season data included
# versus filtering out early-season noise.
get_training_week_phase <- function() {
  if (is.finite(MIN_TRAIN_WEEK) && MIN_TRAIN_WEEK < TRAIN_WEEK_PHASE_CUTOFF) "early" else "late"
}

model_output_prefix <- function(model_family) {
  paste0(model_family, "_", get_training_week_phase())
}

phase_output_path <- function(filename) {
  file.path(OUT_DIR, filename)
}

# Existing CSV rows are re-read with inferred base-R types, while fresh model
# rows can retain Date/POSIX or other richer classes. Normalize only incompatible
# shared columns before appending so production refreshes can safely preserve the
# historical rows already on disk.
bind_csv_rows_compatible <- function(existing, fresh) {
  existing <- as.data.frame(existing, stringsAsFactors = FALSE, check.names = FALSE)
  fresh <- as.data.frame(fresh, stringsAsFactors = FALSE, check.names = FALSE)
  for (nm in intersect(names(existing), names(fresh))) {
    existing_class <- class(existing[[nm]])
    fresh_class <- class(fresh[[nm]])
    both_numeric <- is.numeric(existing[[nm]]) && is.numeric(fresh[[nm]])
    if (!identical(existing_class, fresh_class) && !both_numeric) {
      existing[[nm]] <- as.character(existing[[nm]])
      fresh[[nm]] <- as.character(fresh[[nm]])
    }
  }
  dplyr::bind_rows(existing, fresh)
}

read_resolved_final_models <- function(model_family) {
  path <- file.path(OUT_DIR, paste0(model_output_prefix(model_family), "_final_models.csv"))
  if (!file.exists(path)) {
    stop(
      "Production refit requires the resolved 2025 model file: ", path,
      ". Run the historical 2025 holdout/model-resolution process first.",
      call. = FALSE
    )
  }
  resolved <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!all(c("target", "model_label") %in% names(resolved))) {
    stop("Resolved model file is missing target/model_label columns: ", path, call. = FALSE)
  }
  if ("sample" %in% names(resolved) && any(resolved$sample == "test")) {
    resolved <- resolved[resolved$sample == "test", , drop = FALSE]
  }
  resolved <- resolved[!duplicated(resolved$target), , drop = FALSE]
  resolved
}

resolved_model_value <- function(resolved_row, key) {
  label <- as.character(resolved_row$model_label[[1]])
  hit <- regmatches(label, regexec(paste0("(?:^|,\\s*)", key, "=([^,]+)"), label, perl = TRUE))[[1]]
  if (length(hit) < 2) stop("Could not read '", key, "' from resolved model label: ", label, call. = FALSE)
  value <- as.numeric(trimws(hit[[2]]))
  if (!is.finite(value)) stop("Resolved model value is not numeric for '", key, "': ", label, call. = FALSE)
  value
}

# Feature-family controls.
# These are intentionally explicit so HFA/context indicators do not get dropped just
# because they do not end in _DIF, _PM1, _TOTO, etc.
INCLUDE_CONTEXT_FEATURES_FOR_ALL_TARGETS <- TRUE
CONTEXT_FEATURE_PATTERNS <- c(
  "(^|_)(is_)?home($|_)",
  "(^|_)away($|_)",
  "(^|_)neutral($|_)",
  "division|divisional|same_div",
  "night|prime|primetime|monday|thursday|sunday_night",
  "rest|short_week|bye",
  "travel|distance|timezone|time_zone|tz",
  "dome|roof|surface|grass|turf",
  "weather|temp|temperature|wind|humidity|precip"
)

# Final model selection controls.
# Final model is chosen by the selected sample and metric, with MAE and abs(Bias) as tie-breakers.
MODEL_SELECTION_SAMPLE <- if (NEXTGEN_PRODUCTION_REFIT) "train" else "val"
MODEL_SELECTION_METRIC <- "RMSE"  # choices: "RMSE" or "MAE"
MODEL_SELECTION_RMSE_TOLERANCE <- 0  # set to e.g. 0.05 or 0.10 to prefer simpler ElasticNet models within that RMSE range

# Console/Rmd display limits. Full details are written to CSV.
PRINT_CANDIDATE_PREDICTORS <- FALSE
PRINT_TOP_N_FINAL_VARIABLES <- 40  # printed per target, not total
PRINT_TOP_N_LEADERBOARD <- 10
MAKE_LEADERBOARD_PLOTS <- FALSE

# Output controls. The ElasticNet v9 Rmd overrides WRITE_FULL_DIAGNOSTIC_CSVS to FALSE
# so the console/files focus on final model scoring, selected variables, and legacy-wide exports.
WRITE_FULL_DIAGNOSTIC_CSVS <- TRUE
WRITE_SMALL_FINAL_PREDICTIONS <- FALSE
PRINT_TARGET_AVAILABILITY <- TRUE
PRINT_MARKET_BASELINE_METRICS <- TRUE
QUIET_MODEL_RUN <- FALSE
WRITE_MARKET_CONVENTION_AUDIT_CSV <- TRUE

set.seed(20260512)

required_packages <- c("dplyr", "ggplot2", "knitr")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste0("Package '", pkg, "' is required. Install it first, then rerun this Rmd."))
  }
  library(pkg, character.only = TRUE)
}

# ---------------- Step 8 style metrics ----------------
LLfunction <- function(targets, predicted_values) {
  p_v_zero <- ifelse(predicted_values <= 0, 0, predicted_values)
  p_v_pos <- ifelse(predicted_values <= 0, 0.000001, predicted_values)
  sum(targets * log(p_v_pos), na.rm = TRUE) - sum(p_v_zero, na.rm = TRUE)
}

RMSE <- function(actual, predicted) sqrt(mean((actual - predicted)^2, na.rm = TRUE))
MAE <- function(actual, predicted) mean(abs(actual - predicted), na.rm = TRUE)
MSE <- function(actual, predicted) mean((actual - predicted)^2, na.rm = TRUE)

safe_cor <- function(actual, predicted) {
  ok <- is.finite(actual) & is.finite(predicted)
  if (sum(ok) < 3) return(NA_real_)
  suppressWarnings(cor(actual[ok], predicted[ok]))
}

compact_table <- function(x, n = PRINT_TOP_N_LEADERBOARD, digits = 4) {
  x <- as.data.frame(x)
  if (isTRUE(QUIET_MODEL_RUN)) return(invisible(x))
  if (nrow(x) > n) x <- head(x, n)
  if (requireNamespace("knitr", quietly = TRUE)) {
    print(knitr::kable(x, digits = digits, row.names = FALSE))
  } else {
    print(x, row.names = FALSE)
  }
}

# ---------------- Loading helpers ----------------
first_existing <- function(paths) {
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0) {
    stop(paste0("Could not find dataV12.RData. Tried:\n", paste(paths, collapse = "\n")))
  }
  hit[[1]]
}

load_v12_data <- function(path_candidates = DATA_FILE_CANDIDATES) {
  data_file <- first_existing(path_candidates)
  if (!isTRUE(QUIET_MODEL_RUN)) cat("Loading:", data_file, "\n")
  e <- new.env(parent = emptyenv())
  load(data_file, envir = e)
  objects <- ls(e)
  df_names <- objects[vapply(objects, function(nm) is.data.frame(get(nm, envir = e)), logical(1))]
  if (length(df_names) == 0) stop("No data.frame found inside dataV12.RData.")
  if ("data" %in% df_names) {
    df_name <- "data"
  } else {
    nrows <- vapply(df_names, function(nm) nrow(get(nm, envir = e)), numeric(1))
    df_name <- df_names[which.max(nrows)]
  }
  data <- get(df_name, envir = e)
  if (!isTRUE(QUIET_MODEL_RUN)) cat("Using data object:", df_name, "with", nrow(data), "rows and", ncol(data), "columns.\n")
  data
}

get_col_or_na <- function(df, nm) {
  if (nm %in% names(df)) df[[nm]] else rep(NA, nrow(df))
}

get_first_col_or_na <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(rep(NA, nrow(df)))
  df[[hit[[1]]]]
}

market_home_margin_from_spread <- function(spread_line) {
  # Returned value is expected home margin: home_score - away_score.
  # home_margin convention: spread_line = +7 means home favored by 7.
  # nflverse_home_spread convention: spread_line = -7 means home favored by 7.
  if (identical(SPREAD_LINE_CONVENTION, "nflverse_home_spread")) {
    return(-as.numeric(spread_line))
  }
  if (identical(SPREAD_LINE_CONVENTION, "home_margin")) {
    return(as.numeric(spread_line))
  }
  stop("Unsupported SPREAD_LINE_CONVENTION. Use 'nflverse_home_spread' or 'home_margin'.")
}

make_game_id_if_needed <- function(df) {
  if (!("game_id" %in% names(df))) {
    parts <- list(
      season = get_col_or_na(df, "season"),
      week = get_col_or_na(df, "week"),
      away_team = get_col_or_na(df, "away_team"),
      home_team = get_col_or_na(df, "home_team")
    )
    df$game_id <- paste(parts$season, parts$week, parts$away_team, parts$home_team, sep = "_")
  }
  df
}

create_v12_targets <- function(df) {
  df <- make_game_id_if_needed(df)

  if (all(c("total_line", "spread_line") %in% names(df))) {
    # Convert raw spread_line into expected home margin first.
    # Default matches legacy Step 8: spread_line already equals expected home margin.
    df$home_margin_line <- market_home_margin_from_spread(df$spread_line)
    df$home_implied <- df$total_line / 2 + df$home_margin_line / 2
    df$away_implied <- df$total_line / 2 - df$home_margin_line / 2
  }

  if (all(c("posteam", "home_team", "home_implied", "away_implied") %in% names(df))) {
    df$posteam_implied <- ifelse(df$posteam == df$home_team, df$home_implied, df$away_implied)
    df$defteam_implied <- ifelse(df$posteam == df$home_team, df$away_implied, df$home_implied)
  }

  if (all(c("posteam", "home_team", "home_score", "away_score") %in% names(df))) {
    df$Score_target <- ifelse(df$posteam == df$home_team, df$home_score, df$away_score)
    df$OppScore_target <- ifelse(df$posteam == df$home_team, df$away_score, df$home_score)
    df$ScoreAllowed_target <- df$OppScore_target
    df$Win_target <- ifelse(df$Score_target > df$ScoreAllowed_target, 1, 0)
    df$ScoreDiff_target <- ifelse(df$posteam == df$home_team, df$home_score - df$away_score, df$away_score - df$home_score)
    if ("home_margin_line" %in% names(df)) {
      # Line from the current posteam perspective. For the home row, it is expected home margin.
      # For the away row, it is expected away margin.
      df$favored_by <- ifelse(df$posteam == df$home_team, df$home_margin_line, -df$home_margin_line)
      df$ScoreDiff_target_cover <- ifelse(df$ScoreDiff_target > df$favored_by, 1, 0)
    } else if ("spread_line" %in% names(df)) {
      df$favored_by <- ifelse(df$posteam == df$home_team, market_home_margin_from_spread(df$spread_line), -market_home_margin_from_spread(df$spread_line))
      df$ScoreDiff_target_cover <- ifelse(df$ScoreDiff_target > df$favored_by, 1, 0)
    }
    if (all(c("posteam_implied", "defteam_implied") %in% names(df))) {
      df$Score_target_cover <- ifelse(df$Score_target > df$posteam_implied, 1, 0)
      df$OppScore_target_cover <- ifelse(df$OppScore_target > df$defteam_implied, 1, 0)
      df$ScoreAllowed_target_cover <- ifelse(df$ScoreAllowed_target > df$defteam_implied, 1, 0)
    }
  }

  if (all(c("home_score", "away_score") %in% names(df))) {
    df$home_margin_target <- df$home_score - df$away_score
    df$score_total_target <- df$home_score + df$away_score
    df$home_score_target <- df$home_score
    df$away_score_target <- df$away_score
    df$ScoreTotal_target <- df$score_total_target
    if ("total_line" %in% names(df)) {
      df$ScoreTotal_target_cover <- ifelse(df$ScoreTotal_target > df$total_line, 1, 0)
    }
  }

  df
}

collapse_to_game_level <- function(df) {
  df <- make_game_id_if_needed(df)
  out <- df
  if (KEEP_HOME_TEAM_ROW_ONLY && all(c("posteam", "home_team") %in% names(out))) {
    home_rows <- out[out$posteam == out$home_team, , drop = FALSE]
    if (nrow(home_rows) > 0) {
      if (!isTRUE(QUIET_MODEL_RUN)) cat("Keeping home-team rows only:", nrow(home_rows), "rows retained from", nrow(out), "rows.\n")
      out <- home_rows
    } else {
      if (!isTRUE(QUIET_MODEL_RUN)) cat("No posteam == home_team rows found; using current row grain.\n")
    }
  }
  if ("game_id" %in% names(out)) {
    before <- nrow(out)
    out <- out[!duplicated(out$game_id), , drop = FALSE]
    if (nrow(out) != before && !isTRUE(QUIET_MODEL_RUN)) cat("Deduplicated game_id rows:", before, "->", nrow(out), "\n")
  }
  out
}

print_na_summary <- function(df, max_rows = 30) {
  na_counts <- sort(colSums(is.na(df)), decreasing = TRUE)
  na_counts <- na_counts[na_counts > 0]
  compact_table(data.frame(variable = names(head(na_counts, max_rows)), na_count = as.integer(head(na_counts, max_rows))))
}

# ---------------- Leakage controls ----------------
base_id_cols <- c(
  "game_id", "old_game_id", "gsis", "season_type", "game_type", "weekday", "gametime",
  "game_date", "start_time", "stadium", "location", "posteam", "defteam", "home_team", "away_team",
  "roof", "surface", "vegas_wp", "home_wp", "away_wp"
)

exact_leakage_cols <- c(
  "cover_margin_diff", "total_margin_diff", "score_diff_diff",
  "home_score", "away_score", "result", "home_margin", "away_margin",
  "home_score_sum", "away_score_sum", "home_score_diff", "away_score_diff",
  "spread_line_sum", "spread_line_diff", "total_line_sum", "total_line_diff",
  "home_implied", "away_implied", "posteam_implied", "defteam_implied", "home_margin_line",
  "Score_target", "OppScore_target", "ScoreAllowed_target", "Win_target", "ScoreDiff_target",
  "ScoreTotal_target", "home_margin_target", "score_total_target", "home_score_target", "away_score_target",
  "Score_target_cover", "OppScore_target_cover", "ScoreAllowed_target_cover", "ScoreDiff_target_cover", "ScoreTotal_target_cover"
)

market_cols <- c("spread_line", "total_line", "favored_by", "home_margin_line")

TARGET_SPECS <- data.frame(
  target = c("home_margin_target", "score_total_target", "home_score_target", "away_score_target"),
  target_type = c("side", "total", "home_score", "away_score"),
  market = c("Home cover vs spread", "Over vs total", "Home team total over", "Away team total over"),
  stringsAsFactors = FALSE
)

make_train_test_val <- function(df) {
  needed <- c("season", "week")
  missing_needed <- setdiff(needed, names(df))
  if (length(missing_needed) > 0) stop(paste("Missing required split columns:", paste(missing_needed, collapse = ", ")))

  val_week_filter <- rep(TRUE, nrow(df))
  if (is.finite(VAL_MAX_WEEK)) val_week_filter <- df$week <= VAL_MAX_WEEK

  val <- df[df$season == VAL_SEASON & val_week_filter, , drop = FALSE]
  test <- df[df$season == TEST_SEASON, , drop = FALSE]
  # Production 2026 rows are validation/prediction only. Completed 2026 games
  # must never enter this fit, because the same file also scores past 2026 rows.
  train_before_season <- if (NEXTGEN_PRODUCTION_REFIT) VAL_SEASON else TEST_SEASON
  train_season_ok <- df$season < train_before_season
  train <- df[
    train_season_ok &
      df$week >= MIN_TRAIN_WEEK &
      !(df$season %in% EXCLUDE_TRAIN_SEASONS),
    , drop = FALSE
  ]

  split_summary <- data.frame(
    setting = c(
      "train_rule", "test_season", "validation_season", "validation_max_week",
      "min_train_week", "train_week_phase_cutoff", "train_week_phase", "exclude_2020", "excluded_train_seasons", "model_selection_sample", "model_selection_metric"
    ),
    value = c(
      if (NEXTGEN_PRODUCTION_REFIT) {
        paste0("season < ", train_before_season, "; 2026 outcomes excluded from production training")
      } else {
        paste0("season < ", train_before_season)
      },
      as.character(TEST_SEASON),
      as.character(VAL_SEASON),
      ifelse(is.finite(VAL_MAX_WEEK), as.character(VAL_MAX_WEEK), "all available weeks"),
      as.character(MIN_TRAIN_WEEK),
      as.character(TRAIN_WEEK_PHASE_CUTOFF),
      get_training_week_phase(),
      as.character(EXCLUDE_2020),
      ifelse(length(EXCLUDE_TRAIN_SEASONS) == 0, "none", paste(EXCLUDE_TRAIN_SEASONS, collapse = ", ")),
      paste(as.character(MODEL_SELECTION_SAMPLE), collapse = " > "),
      MODEL_SELECTION_METRIC
    ),
    stringsAsFactors = FALSE
  )
  utils::write.csv(split_summary, file.path(OUT_DIR, paste0("v12_split_settings_", get_training_week_phase(), ".csv")), row.names = FALSE, na = "")

  cat("\n================ SPLIT SETTINGS ================\n")
  compact_table(split_summary, n = 20)
  cat("TRAIN WEEK PHASE:", get_training_week_phase(), "(MIN_TRAIN_WEEK =", MIN_TRAIN_WEEK, "; cutoff =", TRAIN_WEEK_PHASE_CUTOFF, ")\n")
  cat("TRAIN rows:", nrow(train), "seasons:", paste(sort(unique(train$season)), collapse = ", "), "\n")
  cat("TEST rows:", nrow(test), "season:", TEST_SEASON, "\n")
  cat("VAL rows:", nrow(val), "season:", VAL_SEASON, ifelse(is.finite(VAL_MAX_WEEK), paste("through week", VAL_MAX_WEEK), "all available weeks"), "\n")
  list(train = train, test = test, val = val)
}

remove_bad_predictors <- function(df, predictors) {
  predictors <- intersect(predictors, names(df))
  if (length(predictors) == 0) return(character(0))
  predictors <- predictors[vapply(df[predictors], is.numeric, logical(1))]
  ok <- vapply(df[predictors], function(x) {
    ux <- unique(x[is.finite(x)])
    length(ux) > 1
  }, logical(1))
  predictors[ok]
}

rank_features_by_cor <- function(df, predictors, target, max_features = Inf) {
  predictors <- remove_bad_predictors(df, predictors)
  if (length(predictors) == 0) return(character(0))
  cors <- vapply(predictors, function(p) safe_cor(df[[target]], df[[p]]), numeric(1))
  cors[!is.finite(cors)] <- 0
  predictors <- predictors[order(abs(cors), decreasing = TRUE)]
  if (is.finite(max_features)) predictors <- head(predictors, max_features)
  predictors
}

classify_predictor_family <- function(vars) {
  fam <- rep("other_numeric", length(vars))
  fam[grepl("(_pm1$|_PM1$)", vars)] <- "PM1_side_flag"
  fam[grepl("(_diff$|_dif$|_DIF$)", vars)] <- "difference"
  fam[grepl("(_sum$|_toto$|_Toto$|_TOTO$|_total$|_TOTAL$)", vars)] <- "sum_total"
  fam[grepl(paste(CONTEXT_FEATURE_PATTERNS, collapse = "|"), vars, ignore.case = TRUE)] <- "context_hfa_game"
  fam[grepl("(_opp$|_x_opp$|_for_|_allowed_|posteam|defteam|off|def|drive|points|epa|success|yards|pass|rush|rz|red_zone)", vars, ignore.case = TRUE)] <- ifelse(fam == "context_hfa_game", fam, "team_score_stat")
  fam
}

print_predictor_family_summary <- function(vars, target_name, model_family) {
  if (length(vars) == 0) return(invisible(NULL))
  fam <- classify_predictor_family(vars)
  out <- as.data.frame(sort(table(fam), decreasing = TRUE), stringsAsFactors = FALSE)
  names(out) <- c("feature_family", "n_predictors")
  cat("Predictor family summary for", model_family, "/", target_name, "\n")
  compact_table(out, n = 20)
}

get_context_candidates <- function(candidates) {
  if (!isTRUE(INCLUDE_CONTEXT_FEATURES_FOR_ALL_TARGETS)) return(character(0))
  unique(grep(paste(CONTEXT_FEATURE_PATTERNS, collapse = "|"), candidates, value = TRUE, ignore.case = TRUE))
}

select_predictors <- function(df, target_type, target_name, max_features = Inf, extra_keep = character(0)) {
  numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  blocked <- unique(c(base_id_cols, exact_leakage_cols, target_name))
  if (!INCLUDE_MARKET_LINES_AS_FEATURES) blocked <- unique(c(blocked, market_cols))
  if (!INCLUDE_WEEK_AS_FEATURE) blocked <- unique(c(blocked, "week"))
  blocked <- unique(c(blocked, "season"))

  candidates <- setdiff(numeric_cols, intersect(blocked, numeric_cols))

  side_candidates <- grep("(_diff$|_dif$|_DIF$|_pm1$|_PM1$)", candidates, value = TRUE)
  total_candidates <- grep("(_sum$|_toto$|_Toto$|_TOTO$|_total$|_TOTAL$)", candidates, value = TRUE)
  context_candidates <- get_context_candidates(candidates)
  score_candidates <- unique(c(
    side_candidates,
    total_candidates,
    context_candidates,
    grep("(_opp$|_x_opp$|_for_|_allowed_|posteam|defteam|off|def|drive|points|epa|success|yards|pass|rush|rz|red_zone)", candidates, value = TRUE, ignore.case = TRUE)
  ))

  if (target_type == "side") {
    predictors <- unique(c(side_candidates, context_candidates))
  } else if (target_type == "total") {
    predictors <- unique(c(total_candidates, context_candidates))
  } else if (target_type %in% c("home_score", "away_score", "team_score")) {
    predictors <- score_candidates
  } else {
    predictors <- unique(c(candidates, context_candidates))
  }

  predictors <- unique(c(predictors, intersect(extra_keep, candidates)))
  predictors <- remove_bad_predictors(df, predictors)
  predictors <- rank_features_by_cor(df, predictors, target_name, max_features = max_features)
  predictors
}

write_predictor_list <- function(vars, target_name, model_family, fit_id = NA_character_) {
  out <- data.frame(
    model_family = model_family,
    fit_id = fit_id,
    target = target_name,
    feature_family = classify_predictor_family(vars),
    variable = vars,
    stringsAsFactors = FALSE
  )
  path <- file.path(OUT_DIR, paste0(model_family, "_", target_name, "_candidate_predictors.csv"))
  utils::write.csv(out, path, row.names = FALSE, na = "")
  if (!isTRUE(QUIET_MODEL_RUN)) {
    cat("Candidate predictors for", target_name, ":", length(vars), "variables. Wrote", path, "\n")
    print_predictor_family_summary(vars, target_name, model_family)
  }
  if (PRINT_CANDIDATE_PREDICTORS) compact_table(out, n = length(vars))
}


# ---------------- Manual predictor-list helpers ----------------
# ElasticNet v9 intentionally supports a visible, manually editable predictor workflow.
# The first run can generate 3 literal vectors for audit/review:
#   SIDE_PREDICTORS, TOTAL_PREDICTORS, SCORE_PREDICTORS
# Then the Rmd uses those exact vectors instead of hidden regex discovery.
quote_r_string <- function(x) {
  paste0('"', gsub('"', '\\"', x, fixed = TRUE), '"')
}

r_vector_code <- function(vector_name, vars) {
  vars <- unique(as.character(vars))
  if (length(vars) == 0) return(paste0(vector_name, " <- character(0)\n"))
  body <- paste0("  ", quote_r_string(vars), collapse = ",\n")
  paste0(vector_name, " <- c(\n", body, "\n)\n")
}

write_manual_predictor_template <- function(side_vars, total_vars, score_vars, path = "09A_elastic_net_manual_predictor_lists.R") {
  txt <- paste0(
    "# ============================================================\n",
    "# ElasticNet manual predictor lists\n",
    "# ============================================================\n",
    "# Generated from the current V12 data columns. Review these before fitting.\n",
    "# Delete leakage variables, add missing safe variables, then rerun the Rmd.\n",
    "#\n",
    "# Mapping used by 09A ElasticNet v9:\n",
    "#   home_margin_target  -> SIDE_PREDICTORS\n",
    "#   score_total_target  -> TOTAL_PREDICTORS\n",
    "#   home_score_target   -> SCORE_PREDICTORS\n",
    "#   away_score_target   -> SCORE_PREDICTORS\n",
    "#\n",
    "# NOTE: Exact target/outcome/leakage columns are still blocked defensively by the helper.\n",
    "# ============================================================\n\n",
    r_vector_code("SIDE_PREDICTORS", side_vars), "\n",
    r_vector_code("TOTAL_PREDICTORS", total_vars), "\n",
    r_vector_code("SCORE_PREDICTORS", score_vars), "\n"
  )
  writeLines(txt, con = path)
  cat("\nWrote editable ElasticNet predictor-list template to:", normalizePath(path, winslash = "/", mustWork = FALSE), "\n")
  invisible(path)
}

draft_elasticnet_predictor_lists <- function(train_df, max_features = Inf) {
  list(
    SIDE_PREDICTORS = select_predictors(train_df, "side", "home_margin_target", max_features = max_features),
    TOTAL_PREDICTORS = select_predictors(train_df, "total", "score_total_target", max_features = max_features),
    SCORE_PREDICTORS = unique(c(
      select_predictors(train_df, "home_score", "home_score_target", max_features = max_features),
      select_predictors(train_df, "away_score", "away_score_target", max_features = max_features)
    ))
  )
}

validate_manual_predictors <- function(df, vars, target_name, list_name = "PREDICTORS") {
  vars <- unique(as.character(vars))
  vars <- vars[nzchar(vars)]
  blocked <- unique(c(base_id_cols, exact_leakage_cols, target_name))
  if (!INCLUDE_MARKET_LINES_AS_FEATURES) blocked <- unique(c(blocked, market_cols))
  if (!INCLUDE_WEEK_AS_FEATURE) blocked <- unique(c(blocked, "week"))
  blocked <- unique(c(blocked, "season"))

  missing_vars <- setdiff(vars, names(df))
  blocked_vars <- intersect(vars, blocked)
  present <- intersect(vars, names(df))
  non_numeric <- present[!vapply(df[present], is.numeric, logical(1))]

  keep <- setdiff(present, c(blocked_vars, non_numeric))
  keep <- remove_bad_predictors(df, keep)
  dropped_constant <- setdiff(setdiff(setdiff(present, blocked_vars), non_numeric), keep)

  if ((length(missing_vars) > 0 || length(blocked_vars) > 0 || length(non_numeric) > 0 || length(dropped_constant) > 0) && !isTRUE(QUIET_MODEL_RUN)) {
    cat("\nPredictor validation for", list_name, "/", target_name, "\n")
    cat("  kept:", length(keep), "\n")
    if (length(missing_vars) > 0) cat("  dropped missing:", paste(missing_vars, collapse = ", "), "\n")
    if (length(blocked_vars) > 0) cat("  dropped blocked leakage/id/market fields:", paste(blocked_vars, collapse = ", "), "\n")
    if (length(non_numeric) > 0) cat("  dropped non-numeric fields:", paste(non_numeric, collapse = ", "), "\n")
    if (length(dropped_constant) > 0) cat("  dropped constant/all-missing fields:", paste(dropped_constant, collapse = ", "), "\n")
  }
  keep
}

print_predictor_vector <- function(vector_name, vars, wrap = 3) {
  vars <- unique(as.character(vars))
  cat("\n", vector_name, " <- c(  # n = ", length(vars), "\n", sep = "")
  if (length(vars) > 0) {
    q <- quote_r_string(vars)
    lines <- split(q, ceiling(seq_along(q) / wrap))
    for (ln in lines) cat("  ", paste(ln, collapse = ", "), ",\n", sep = "")
  }
  cat(")\n")
}

show_manual_predictor_lists <- function(predictor_lists, print_full = TRUE) {
  counts <- data.frame(
    list_name = names(predictor_lists),
    n_predictors = vapply(predictor_lists, length, integer(1)),
    stringsAsFactors = FALSE
  )
  cat("\n================ REVIEWED PREDICTOR LIST COUNTS ================\n")
  compact_table(counts, n = 10)
  if (isTRUE(print_full)) {
    for (nm in names(predictor_lists)) print_predictor_vector(nm, predictor_lists[[nm]])
  }
  invisible(counts)
}

load_or_create_elasticnet_predictor_lists <- function(train_df,
                                                      manual_file = "09A_elastic_net_manual_predictor_lists.R",
                                                      max_features = Inf,
                                                      require_reviewed_file = TRUE,
                                                      print_full = TRUE) {
  if (!file.exists(manual_file)) {
    draft <- draft_elasticnet_predictor_lists(train_df, max_features = max_features)
    write_manual_predictor_template(draft$SIDE_PREDICTORS, draft$TOTAL_PREDICTORS, draft$SCORE_PREDICTORS, path = manual_file)
    cat("\nThe template above is the audit point. Review/edit the three vectors, then rerun this Rmd.\n")
    if (isTRUE(require_reviewed_file)) {
      stop("Manual predictor-list file was created. Review it first, then rerun 09A ElasticNet.", call. = FALSE)
    }
  }

  # Use baseenv() as the parent so assignment (<-) and c() work while keeping
  # the manual predictor-list objects isolated from the global environment.
  e <- new.env(parent = baseenv())
  sys.source(manual_file, envir = e)
  needed <- c("SIDE_PREDICTORS", "TOTAL_PREDICTORS", "SCORE_PREDICTORS")
  missing <- needed[!vapply(needed, exists, logical(1), envir = e, inherits = FALSE)]
  if (length(missing) > 0) stop("Manual predictor-list file is missing: ", paste(missing, collapse = ", "))

  lists <- list(
    SIDE_PREDICTORS = get("SIDE_PREDICTORS", envir = e),
    TOTAL_PREDICTORS = get("TOTAL_PREDICTORS", envir = e),
    SCORE_PREDICTORS = get("SCORE_PREDICTORS", envir = e)
  )
  show_manual_predictor_lists(lists, print_full = print_full)
  invisible(lists)
}

elasticnet_predictors_for_target <- function(target_name, predictor_lists, train_df) {
  if (target_name == "home_margin_target") {
    validate_manual_predictors(train_df, predictor_lists$SIDE_PREDICTORS, target_name, "SIDE_PREDICTORS")
  } else if (target_name == "score_total_target") {
    validate_manual_predictors(train_df, predictor_lists$TOTAL_PREDICTORS, target_name, "TOTAL_PREDICTORS")
  } else if (target_name %in% c("home_score_target", "away_score_target")) {
    validate_manual_predictors(train_df, predictor_lists$SCORE_PREDICTORS, target_name, "SCORE_PREDICTORS")
  } else {
    character(0)
  }
}


make_lm_formula <- function(target, predictors) {
  as.formula(paste0("`", target, "` ~ ", paste0("`", predictors, "`", collapse = " + ")))
}

complete_target_rows <- function(df, target) {
  df[is.finite(df[[target]]), , drop = FALSE]
}

ensure_target_column <- function(df, target) {
  # Future/projection samples, such as a 2026 validation schedule, can have
  # rows and model inputs before actual final scores exist. Keep those rows
  # for prediction by adding a blank target column when needed. Training rows
  # should still be passed through complete_target_rows().
  df <- as.data.frame(df)
  if (!(target %in% names(df))) df[[target]] <- NA_real_
  df
}

median_impute <- function(train_df, other_dfs, predictors) {
  med <- vapply(train_df[predictors], function(x) median(x, na.rm = TRUE), numeric(1))
  med[!is.finite(med)] <- 0
  train_out <- train_df[predictors]
  for (p in predictors) train_out[[p]][!is.finite(train_out[[p]]) | is.na(train_out[[p]])] <- med[[p]]
  other_out <- lapply(other_dfs, function(odf) {
    tmp <- odf[predictors]
    for (p in predictors) tmp[[p]][!is.finite(tmp[[p]]) | is.na(tmp[[p]])] <- med[[p]]
    tmp
  })
  list(train = train_out, other = other_out, medians = med)
}

make_model_frames <- function(train_df, test_df, val_df, predictors, target) {
  imputed <- median_impute(train_df, list(test = test_df, val = val_df), predictors)
  list(
    train = data.frame(target_value = train_df[[target]], imputed$train, check.names = FALSE),
    test = data.frame(target_value = test_df[[target]], imputed$other$test, check.names = FALSE),
    val = data.frame(target_value = val_df[[target]], imputed$other$val, check.names = FALSE),
    medians = imputed$medians
  )
}

make_numeric_matrices <- function(train_df, test_df, val_df, predictors, target) {
  imputed <- median_impute(train_df, list(test = test_df, val = val_df), predictors)
  list(
    x_train = as.matrix(imputed$train),
    x_test = as.matrix(imputed$other$test),
    x_val = as.matrix(imputed$other$val),
    y_train = train_df[[target]],
    y_test = test_df[[target]],
    y_val = val_df[[target]],
    medians = imputed$medians
  )
}

sample_weights <- function(df, method = "none") {
  if (method == "none") return(rep(1, nrow(df)))
  if (!("season" %in% names(df))) return(rep(1, nrow(df)))
  season <- df$season
  if (method == "linear_recent") {
    rng <- max(season, na.rm = TRUE) - min(season, na.rm = TRUE)
    if (!is.finite(rng) || rng <= 0) return(rep(1, nrow(df)))
    return(1 + 1.5 * (season - min(season, na.rm = TRUE)) / rng)
  }
  if (method == "exp_recent") {
    max_season <- max(season, na.rm = TRUE)
    return(0.85 ^ pmax(0, max_season - season))
  }
  rep(1, nrow(df))
}

# ---------------- Evaluation and market scoring ----------------
empty_metric_frame <- function() {
  data.frame(
    target = character(),
    sample = character(),
    fit_id = character(),
    model_label = character(),
    n = integer(),
    RMSE = numeric(),
    MAE = numeric(),
    MSE = numeric(),
    Bias = numeric(),
    Correlation = numeric(),
    stringsAsFactors = FALSE
  )
}

evaluate_predictions <- function(df, target, pred, fit_id, model_label, sample_name) {
  df <- as.data.frame(df)
  if (!(target %in% names(df))) df[[target]] <- NA_real_
  actual <- df[[target]]
  pred <- as.numeric(pred)

  # Future/projection samples can validly have rows but no actual 2026 scores yet.
  # They should still get prediction rows, but they should not create fake n=0 or
  # NaN validation leaderboard rows that final-model selection might choose.
  if (nrow(df) == 0 || length(pred) == 0) {
    return(empty_metric_frame())
  }

  if (length(actual) != length(pred)) {
    stop(
      "Prediction length mismatch for target=", target,
      ", sample=", sample_name,
      ": actual rows=", length(actual),
      ", prediction rows=", length(pred),
      call. = FALSE
    )
  }

  ok <- is.finite(actual) & is.finite(pred)
  if (sum(ok) == 0) {
    return(empty_metric_frame())
  }

  data.frame(
    target = target,
    sample = sample_name,
    fit_id = fit_id,
    model_label = model_label,
    n = sum(ok),
    RMSE = RMSE(actual, pred),
    MAE = MAE(actual, pred),
    MSE = MSE(actual, pred),
    Bias = mean(pred[ok] - actual[ok], na.rm = TRUE),
    Correlation = safe_cor(actual, pred),
    stringsAsFactors = FALSE
  )
}

target_line_vector <- function(df, target) {
  if (target == "home_margin_target") {
    if ("home_margin_line" %in% names(df)) return(df$home_margin_line)
    if ("spread_line" %in% names(df)) return(market_home_margin_from_spread(df$spread_line))
  }
  if (target == "score_total_target" && "total_line" %in% names(df)) return(df$total_line)
  if (target == "home_score_target" && "home_implied" %in% names(df)) return(df$home_implied)
  if (target == "away_score_target" && "away_implied" %in% names(df)) return(df$away_implied)
  rep(NA_real_, nrow(df))
}

target_market_name <- function(target) {
  out <- TARGET_SPECS$market[TARGET_SPECS$target == target]
  if (length(out) == 0) target else out[[1]]
}

upper_side_label <- function(target) {
  switch(target,
    home_margin_target = "home_cover",
    score_total_target = "over",
    home_score_target = "home_team_total_over",
    away_score_target = "away_team_total_over",
    "above_line"
  )
}

lower_side_label <- function(target) {
  switch(target,
    home_margin_target = "away_cover",
    score_total_target = "under",
    home_score_target = "home_team_total_under",
    away_score_target = "away_team_total_under",
    "below_line"
  )
}

normalize_prediction_frame_types <- function(x) {
  # Type-stabilize shared prediction rows before model scripts bind them across
  # targets/samples. Future appended schedules can introduce all-missing columns
  # with numeric/logical classes, while completed seasons usually have character
  # team/game identifiers.
  x <- as.data.frame(x, stringsAsFactors = FALSE)

  char_cols <- c(
    "sample", "fit_id", "model_label", "target", "market", "game_id",
    "model_team", "home_team", "away_team", "predicted_side"
  )
  for (nm in intersect(char_cols, names(x))) {
    x[[nm]] <- as.character(x[[nm]])
  }

  num_cols <- c("season", "week", "actual", "predicted", "error", "line", "edge", "abs_edge")
  for (nm in intersect(num_cols, names(x))) {
    x[[nm]] <- suppressWarnings(as.numeric(x[[nm]]))
  }

  int_cols <- c("actual_market_result", "predicted_market_result")
  for (nm in intersect(int_cols, names(x))) {
    x[[nm]] <- suppressWarnings(as.integer(x[[nm]]))
  }

  if ("actual_push" %in% names(x)) x$actual_push <- as.logical(x$actual_push)
  if ("game_date" %in% names(x)) x$game_date <- as.character(x$game_date)

  x
}

empty_prediction_frame <- function() {
  data.frame(
    sample = character(),
    fit_id = character(),
    model_label = character(),
    target = character(),
    market = character(),
    season = numeric(),
    week = numeric(),
    game_date = as.Date(character()),
    game_id = character(),
    model_team = character(),
    home_team = character(),
    away_team = character(),
    actual = numeric(),
    predicted = numeric(),
    error = numeric(),
    line = numeric(),
    edge = numeric(),
    abs_edge = numeric(),
    predicted_side = character(),
    actual_market_result = integer(),
    predicted_market_result = integer(),
    actual_push = logical(),
    stringsAsFactors = FALSE
  )
}

prediction_frame <- function(df, target, pred, fit_id, model_label, sample_name) {
  df <- as.data.frame(df)
  pred <- as.numeric(pred)
  if (!(target %in% names(df))) df[[target]] <- NA_real_

  if (nrow(df) == 0 || length(pred) == 0) {
    return(empty_prediction_frame())
  }
  if (nrow(df) != length(pred)) {
    stop(
      "Prediction length mismatch while building prediction_frame for target=", target,
      ", sample=", sample_name,
      ": df rows=", nrow(df),
      ", prediction rows=", length(pred),
      call. = FALSE
    )
  }

  line <- target_line_vector(df, target)
  actual <- df[[target]]
  edge <- pred - line
  actual_market_result <- ifelse(is.finite(actual) & is.finite(line) & actual != line, as.integer(actual > line), NA_integer_)
  predicted_market_result <- ifelse(is.finite(pred) & is.finite(line) & pred != line, as.integer(pred > line), NA_integer_)
  out <- data.frame(
    sample = rep(sample_name, nrow(df)),
    fit_id = rep(fit_id, nrow(df)),
    model_label = rep(model_label, nrow(df)),
    target = rep(target, nrow(df)),
    market = rep(target_market_name(target), nrow(df)),
    season = get_col_or_na(df, "season"),
    week = get_col_or_na(df, "week"),
    game_date = get_first_col_or_na(df, c("gameday", "game_date", "date", "Date")),
    game_id = get_col_or_na(df, "game_id"),
    model_team = ifelse(!is.na(get_col_or_na(df, "posteam")), get_col_or_na(df, "posteam"), get_col_or_na(df, "home_team")),
    home_team = get_col_or_na(df, "home_team"),
    away_team = get_col_or_na(df, "away_team"),
    actual = actual,
    predicted = pred,
    error = pred - actual,
    line = line,
    edge = edge,
    abs_edge = abs(edge),
    predicted_side = ifelse(edge > 0, upper_side_label(target), ifelse(edge < 0, lower_side_label(target), "no_edge")),
    actual_market_result = actual_market_result,
    predicted_market_result = predicted_market_result,
    actual_push = ifelse(is.finite(actual) & is.finite(line), actual == line, NA),
    stringsAsFactors = FALSE
  )
  normalize_prediction_frame_types(out)
}

summarise_market_results <- function(predictions) {
  if (nrow(predictions) == 0 || !("line" %in% names(predictions))) return(data.frame())
  predictions %>%
    filter(is.finite(line), !is.na(actual_market_result), !is.na(predicted_market_result), predicted_side != "no_edge") %>%
    group_by(target, market, sample, fit_id, model_label) %>%
    summarise(
      n_decisions = n(),
      correct = sum(actual_market_result == predicted_market_result, na.rm = TRUE),
      wrong = sum(actual_market_result != predicted_market_result, na.rm = TRUE),
      accuracy = correct / n_decisions,
      avg_edge = mean(edge, na.rm = TRUE),
      avg_abs_edge = mean(abs_edge, na.rm = TRUE),
      pct_above_line = mean(predicted_market_result == 1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(target, sample, desc(accuracy), desc(n_decisions))
}

summarise_edge_buckets <- function(predictions) {
  if (nrow(predictions) == 0 || !("line" %in% names(predictions))) return(data.frame())
  predictions %>%
    filter(is.finite(line), !is.na(actual_market_result), !is.na(predicted_market_result), predicted_side != "no_edge") %>%
    mutate(edge_bucket = cut(abs_edge, breaks = c(-Inf, 1, 2, 3, 4, 6, Inf), labels = c("0-1", "1-2", "2-3", "3-4", "4-6", "6+"), right = FALSE)) %>%
    group_by(target, market, sample, fit_id, model_label, predicted_side, edge_bucket) %>%
    summarise(
      n_decisions = n(),
      correct = sum(actual_market_result == predicted_market_result, na.rm = TRUE),
      wrong = sum(actual_market_result != predicted_market_result, na.rm = TRUE),
      accuracy = correct / n_decisions,
      avg_edge = mean(edge, na.rm = TRUE),
      avg_abs_edge = mean(abs_edge, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(target, sample, fit_id, edge_bucket, predicted_side)
}

make_market_baseline_metrics <- function(splits) {
  out <- list()
  for (target in TARGET_SPECS$target) {
    for (sample_name in names(splits)) {
      df <- complete_target_rows(splits[[sample_name]], target)
      line <- target_line_vector(df, target)
      if (all(!is.finite(line))) next
      out[[length(out) + 1]] <- evaluate_predictions(df, target, line, "market_line", "market_line", sample_name)
    }
  }
  bind_rows(out)
}

make_market_convention_audit <- function(splits) {
  out <- list()
  for (sample_name in names(splits)) {
    df <- splits[[sample_name]]
    if (!all(c("spread_line", "total_line", "home_score", "away_score") %in% names(df))) next
    base <- df[is.finite(df$spread_line) & is.finite(df$total_line) & is.finite(df$home_score) & is.finite(df$away_score), , drop = FALSE]
    if (nrow(base) == 0) next

    # Convention A: current/Step-8 convention, spread_line = expected home margin.
    home_line_a <- base$total_line / 2 + base$spread_line / 2
    away_line_a <- base$total_line / 2 - base$spread_line / 2

    # Convention B: alternate nflverse-style convention, spread_line = home spread, negative means home favorite.
    home_line_b <- base$total_line / 2 - base$spread_line / 2
    away_line_b <- base$total_line / 2 + base$spread_line / 2

    add_row <- function(target, convention, line_vec, actual_vec) {
      pred <- ifelse(actual_vec > line_vec, 1, 0)  # market baseline team-total "over" result rate vs line.
      data.frame(
        sample = sample_name,
        target = target,
        convention = convention,
        n = sum(is.finite(line_vec) & is.finite(actual_vec)),
        mean_line = mean(line_vec, na.rm = TRUE),
        mean_actual = mean(actual_vec, na.rm = TRUE),
        pct_actual_over_line = mean(pred, na.rm = TRUE),
        avg_actual_minus_line = mean(actual_vec - line_vec, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }

    out[[length(out) + 1]] <- add_row("home_score_target", "step8_home_margin", home_line_a, base$home_score)
    out[[length(out) + 1]] <- add_row("away_score_target", "step8_home_margin", away_line_a, base$away_score)
    out[[length(out) + 1]] <- add_row("home_score_target", "alternate_nflverse_home_spread", home_line_b, base$home_score)
    out[[length(out) + 1]] <- add_row("away_score_target", "alternate_nflverse_home_spread", away_line_b, base$away_score)
  }
  dplyr::bind_rows(out)
}

normalize_model_selection_samples <- function(preferred_sample = MODEL_SELECTION_SAMPLE, available_samples = character()) {
  pref <- as.character(preferred_sample)
  pref <- pref[!is.na(pref) & nzchar(pref)]
  available_samples <- as.character(available_samples)
  hit <- pref[pref %in% available_samples]
  if (length(hit) > 0) return(hit[[1]])
  fallback_order <- c("val", "test", "train")
  hit <- fallback_order[fallback_order %in% available_samples]
  if (length(hit) > 0) return(hit[[1]])
  if (length(available_samples) > 0) return(available_samples[[1]])
  NA_character_
}

choose_final_models <- function(leaderboard, preferred_sample = MODEL_SELECTION_SAMPLE, metric = MODEL_SELECTION_METRIC) {
  leaderboard <- as.data.frame(leaderboard)
  if (nrow(leaderboard) == 0) return(data.frame())

  metric <- toupper(metric)
  if (!(metric %in% c("RMSE", "MAE"))) metric <- "RMSE"
  if (!(metric %in% names(leaderboard))) return(data.frame())

  # Drop unscored future/projection rows. For example, when VAL_SEASON = 2026
  # and no 2026 actual scores exist yet, validation predictions are useful, but
  # validation RMSE is not real and should not be used for model selection.
  leaderboard <- leaderboard[is.finite(leaderboard[[metric]]), , drop = FALSE]
  if (nrow(leaderboard) == 0) return(data.frame())

  # Select the best available sample target-by-target. This lets a 2026 validation
  # prediction run fall back to 2025 test for model choice while still outputting
  # 2026 future-game predictions.
  by_target <- split(leaderboard, leaderboard$target)
  tmp <- dplyr::bind_rows(lapply(by_target, function(x) {
    use_sample <- normalize_model_selection_samples(preferred_sample, unique(x$sample))
    if (is.na(use_sample)) return(x[0, , drop = FALSE])
    x[x$sample == use_sample, , drop = FALSE]
  }))
  if (nrow(tmp) == 0) return(data.frame())

  # If n_variables exists, the optional tolerance lets you choose a simpler model
  # that is within a small RMSE/MAE distance of the best pure-error model.
  # Default tolerance is zero, so this preserves pure RMSE/MAE selection.
  if (metric == "MAE") {
    tmp <- tmp %>% group_by(target) %>% mutate(best_metric = min(MAE, na.rm = TRUE))
    if ("n_variables" %in% names(tmp) && MODEL_SELECTION_RMSE_TOLERANCE > 0) {
      tmp <- tmp %>% filter(MAE <= best_metric + MODEL_SELECTION_RMSE_TOLERANCE)
      out <- tmp %>% arrange(n_variables, MAE, RMSE, abs(Bias), .by_group = TRUE) %>% slice_head(n = 1) %>% ungroup()
    } else {
      out <- tmp %>% arrange(MAE, RMSE, abs(Bias), .by_group = TRUE) %>% slice_head(n = 1) %>% ungroup()
    }
  } else {
    tmp <- tmp %>% group_by(target) %>% mutate(best_metric = min(RMSE, na.rm = TRUE))
    if ("n_variables" %in% names(tmp) && MODEL_SELECTION_RMSE_TOLERANCE > 0) {
      tmp <- tmp %>% filter(RMSE <= best_metric + MODEL_SELECTION_RMSE_TOLERANCE)
      out <- tmp %>% arrange(n_variables, RMSE, MAE, abs(Bias), .by_group = TRUE) %>% slice_head(n = 1) %>% ungroup()
    } else {
      out <- tmp %>% arrange(RMSE, MAE, abs(Bias), .by_group = TRUE) %>% slice_head(n = 1) %>% ungroup()
    }
  }

  out %>% select(-best_metric)
}

plot_leaderboard_by_target <- function(lb, model_family) {
  if (!MAKE_LEADERBOARD_PLOTS || nrow(lb) == 0) return(invisible(NULL))
  plot_df <- lb %>%
    filter(sample %in% c("test", "val")) %>%
    group_by(target, sample) %>%
    arrange(RMSE, .by_group = TRUE) %>%
    slice_head(n = 10) %>%
    ungroup()
  for (tg in unique(plot_df$target)) {
    for (sm in unique(plot_df$sample)) {
      sub <- plot_df %>% filter(target == tg, sample == sm)
      if (nrow(sub) == 0) next
      p <- ggplot(sub, aes(x = reorder(model_label, RMSE), y = RMSE)) +
        geom_col() +
        coord_flip() +
        labs(title = paste(model_family, tg, sm, "top RMSE"), x = "Model", y = "RMSE") +
        theme_minimal(base_size = 11)
      print(p)
      ggsave(file.path(OUT_DIR, paste0(model_output_prefix(model_family), "_", tg, "_", sm, "_leaderboard_rmse.png")), p, width = 12, height = max(5, 0.35 * nrow(sub) + 2))
    }
  }
}


# ---------------- Small and legacy-wide final prediction exports ----------------
# The full *_predictions.csv is intentionally long: one row per target / model / sample / game.
# The small export keeps only the chosen final model for each target and pivots the four targets
# into one readable row per game for quick review.
# The legacy-wide export keeps the full V12 / Step-8-style data frame and appends final
# model predictions, edges, picks, errors, and model labels at the far right for downstream app scripts.
make_clean_final_predictions <- function(predictions, final_models, samples = c("test", "val")) {
  predictions <- as.data.frame(predictions)
  final_models <- as.data.frame(final_models)
  if (nrow(predictions) == 0 || nrow(final_models) == 0) return(data.frame())

  key_cols <- c("target", "fit_id")
  final_keys <- final_models[, key_cols, drop = FALSE]
  fp <- predictions %>%
    inner_join(final_keys, by = key_cols) %>%
    filter(sample %in% samples)

  if (nrow(fp) == 0) return(data.frame())

  id_cols <- c("sample", "season", "week", "game_date", "game_id", "model_team", "home_team", "away_team")
  id_cols <- id_cols[id_cols %in% names(fp)]
  base <- unique(fp[, id_cols, drop = FALSE])

  add_target_block <- function(base_df, target_name, prefix, line_name, side_name) {
    sub <- fp %>% filter(target == target_name)
    if (nrow(sub) == 0) return(base_df)

    keep <- c(id_cols, "model_label", "actual", "predicted", "error", "line", "edge", "predicted_side", "actual_market_result", "predicted_market_result", "actual_push")
    keep <- keep[keep %in% names(sub)]
    sub <- sub[, keep, drop = FALSE]

    # Rename the target-specific columns so the CSV reads naturally in wide format.
    ren <- names(sub)
    ren[ren == "model_label"] <- paste0(prefix, "_model")
    ren[ren == "actual"] <- paste0("actual_", prefix)
    ren[ren == "predicted"] <- paste0("predicted_", prefix)
    ren[ren == "error"] <- paste0("error_", prefix)
    ren[ren == "line"] <- line_name
    ren[ren == "edge"] <- paste0(prefix, "_edge")
    ren[ren == "predicted_side"] <- side_name
    ren[ren == "actual_market_result"] <- paste0(prefix, "_actual_above_line")
    ren[ren == "predicted_market_result"] <- paste0(prefix, "_predicted_above_line")
    ren[ren == "actual_push"] <- paste0(prefix, "_actual_push")
    names(sub) <- ren

    merge(base_df, sub, by = id_cols, all.x = TRUE, sort = FALSE)
  }

  clean <- base
  clean <- add_target_block(clean, "home_margin_target", "home_margin", "home_margin_line", "spread_pick")
  clean <- add_target_block(clean, "score_total_target", "total", "total_line", "total_pick")
  clean <- add_target_block(clean, "home_score_target", "home_score", "home_implied", "home_team_total_pick")
  clean <- add_target_block(clean, "away_score_target", "away_score", "away_implied", "away_team_total_pick")

  # Add a few obvious score/margin/total aliases near the front when the four target blocks exist.
  if (all(c("predicted_home_score", "predicted_away_score") %in% names(clean))) {
    clean$predicted_score_string <- paste0(round(clean$predicted_away_score, 1), " - ", round(clean$predicted_home_score, 1), " (away-home)")
  }

  # Preferred column order: identifiers, actuals, predictions, lines, edges/picks, then models.
  front <- c(
    "sample", "season", "week", "game_date", "game_id", "model_team", "home_team", "away_team",
    "actual_home_margin", "actual_total", "actual_home_score", "actual_away_score",
    "predicted_home_margin", "predicted_total", "predicted_home_score", "predicted_away_score", "predicted_score_string",
    "spread_line", "home_margin_line", "total_line", "home_implied", "away_implied",
    "home_margin_edge", "total_edge", "home_score_edge", "away_score_edge",
    "spread_pick", "total_pick", "home_team_total_pick", "away_team_total_pick",
    "error_home_margin", "error_total", "error_home_score", "error_away_score"
  )
  front <- front[front %in% names(clean)]
  model_cols <- grep("_model$", names(clean), value = TRUE)
  status_cols <- grep("_(actual_above_line|predicted_above_line|actual_push)$", names(clean), value = TRUE)
  remaining <- setdiff(names(clean), c(front, model_cols, status_cols))
  clean <- clean[, c(front, model_cols, status_cols, remaining), drop = FALSE]

  ord_cols <- intersect(c("sample", "season", "week", "game_id"), names(clean))
  if (length(ord_cols) > 0) {
    clean <- clean[do.call(order, clean[ord_cols]), , drop = FALSE]
  }

  clean
}

show_small_final_predictions_preview <- function(results, model_family = "model_family", n = 25) {
  clean <- results$small_final_predictions
  if (is.null(clean) || nrow(clean) == 0) {
    cat("No small final prediction file was created. Check final model selection and target availability.\n")
    return(invisible(NULL))
  }
  cat("\n================ SMALL FINAL TEST/VALIDATION PREDICTION CSV PREVIEW ================\n")
  compact_table(clean, n = n)
  cat("\nSmall final prediction file written to:", file.path(OUT_DIR, paste0(model_output_prefix(model_family), "_FINAL_MODEL_small_predictions_test_val.csv")), "\n")
  invisible(clean)
}


legacy_tag_from_family <- function(model_family) {
  tag <- gsub("[^A-Za-z0-9]+", "_", model_family)
  tag <- gsub("(^_+|_+$)", "", tag)
  ifelse(nchar(tag) == 0, "model", tag)
}

append_one_legacy_target <- function(base, fp, target_name, pred_col, cover_col, edge_col, error_col, pick_col, model_col, line_copy_col = NULL) {
  sub <- fp %>% filter(target == target_name)
  if (nrow(sub) == 0) return(base)

  keep <- c("sample", "game_id", "predicted", "error", "line", "edge", "predicted_side", "predicted_market_result", "model_label")
  keep <- keep[keep %in% names(sub)]
  sub <- sub[, keep, drop = FALSE]
  sub$sample <- as.character(sub$sample)
  sub$game_id <- as.character(sub$game_id)
  sub <- sub[!duplicated(sub[c("sample", "game_id")]), , drop = FALSE]

  out <- data.frame(
    projection_sample = sub$sample,
    game_id = sub$game_id,
    stringsAsFactors = FALSE
  )
  out[[pred_col]] <- sub$predicted
  out[[cover_col]] <- sub$predicted_market_result
  out[[edge_col]] <- sub$edge
  out[[error_col]] <- sub$error
  out[[pick_col]] <- sub$predicted_side
  out[[model_col]] <- sub$model_label
  if (!is.null(line_copy_col) && "line" %in% names(sub)) out[[line_copy_col]] <- sub$line

  merge(base, out, by = c("projection_sample", "game_id"), all.x = TRUE, sort = FALSE)
}

make_legacy_wide_final_predictions <- function(predictions, final_models, prep = NULL, model_family = "model_family", samples = c("test", "val")) {
  predictions <- as.data.frame(predictions)
  final_models <- as.data.frame(final_models)
  if (nrow(predictions) == 0 || nrow(final_models) == 0) return(data.frame())

  if (is.null(prep)) {
    if (exists(".V12_LAST_PREP", envir = .GlobalEnv)) {
      prep <- get(".V12_LAST_PREP", envir = .GlobalEnv)
    } else {
      return(data.frame())
    }
  }

  if (is.null(prep$splits)) return(data.frame())
  samples <- intersect(samples, names(prep$splits))
  if (length(samples) == 0) return(data.frame())

  base_list <- lapply(samples, function(sm) {
    df <- prep$splits[[sm]]
    df <- make_game_id_if_needed(df)
    df$projection_sample <- sm
    df[, c("projection_sample", setdiff(names(df), "projection_sample")), drop = FALSE]
  })
  base <- dplyr::bind_rows(base_list)
  base$projection_sample <- as.character(base$projection_sample)
  base$game_id <- as.character(base$game_id)

  final_keys <- final_models[, c("target", "fit_id"), drop = FALSE]
  fp <- predictions %>%
    inner_join(final_keys, by = c("target", "fit_id")) %>%
    filter(sample %in% samples)
  if (nrow(fp) == 0) return(base)

  tag <- legacy_tag_from_family(model_family)

  # Legacy-style names: at home-team row grain, Score = home score and OppScore = away score.
  # Explicit HomeScore/AwayScore/HomeMargin/TotalScore aliases are also appended for clarity.
  base <- append_one_legacy_target(
    base, fp, "home_score_target",
    pred_col = paste0("Score_", tag),
    cover_col = paste0("Cover_Score_", tag),
    edge_col = paste0("Edge_Score_", tag),
    error_col = paste0("Error_Score_", tag),
    pick_col = paste0("Pick_Score_", tag),
    model_col = paste0("Model_Score_", tag),
    line_copy_col = paste0("Line_Score_", tag)
  )
  if (paste0("Score_", tag) %in% names(base)) base[[paste0("HomeScore_", tag)]] <- base[[paste0("Score_", tag)]]

  base <- append_one_legacy_target(
    base, fp, "away_score_target",
    pred_col = paste0("OppScore_", tag),
    cover_col = paste0("Cover_OppScore_", tag),
    edge_col = paste0("Edge_OppScore_", tag),
    error_col = paste0("Error_OppScore_", tag),
    pick_col = paste0("Pick_OppScore_", tag),
    model_col = paste0("Model_OppScore_", tag),
    line_copy_col = paste0("Line_OppScore_", tag)
  )
  if (paste0("OppScore_", tag) %in% names(base)) base[[paste0("AwayScore_", tag)]] <- base[[paste0("OppScore_", tag)]]

  base <- append_one_legacy_target(
    base, fp, "score_total_target",
    pred_col = paste0("ScoreTotal_", tag),
    cover_col = paste0("Cover_Total_", tag),
    edge_col = paste0("Edge_Total_", tag),
    error_col = paste0("Error_Total_", tag),
    pick_col = paste0("Pick_Total_", tag),
    model_col = paste0("Model_Total_", tag),
    line_copy_col = paste0("Line_Total_", tag)
  )
  if (paste0("ScoreTotal_", tag) %in% names(base)) base[[paste0("TotalScore_", tag)]] <- base[[paste0("ScoreTotal_", tag)]]

  base <- append_one_legacy_target(
    base, fp, "home_margin_target",
    pred_col = paste0("ScoreDiff_", tag),
    cover_col = paste0("Cover_ScoreDiff_", tag),
    edge_col = paste0("Edge_ScoreDiff_", tag),
    error_col = paste0("Error_ScoreDiff_", tag),
    pick_col = paste0("Pick_ScoreDiff_", tag),
    model_col = paste0("Model_ScoreDiff_", tag),
    line_copy_col = paste0("Line_ScoreDiff_", tag)
  )
  if (paste0("ScoreDiff_", tag) %in% names(base)) base[[paste0("HomeMargin_", tag)]] <- base[[paste0("ScoreDiff_", tag)]]

  if (all(c(paste0("Score_", tag), paste0("OppScore_", tag)) %in% names(base))) {
    base[[paste0("PredictedScore_", tag)]] <- paste0(
      base$away_team, " ", round(base[[paste0("OppScore_", tag)]], 1),
      " @ ",
      base$home_team, " ", round(base[[paste0("Score_", tag)]], 1)
    )
  }

  ord_cols <- intersect(c("projection_sample", "season", "week", "game_id"), names(base))
  if (length(ord_cols) > 0) base <- base[do.call(order, base[ord_cols]), , drop = FALSE]
  base
}

write_legacy_wide_prediction_files <- function(legacy_wide, model_family = "model_family", samples = c("test", "val")) {
  if (is.null(legacy_wide) || nrow(legacy_wide) == 0) return(character(0))

  paths <- character(0)
  phase <- get_training_week_phase()
  prefix <- model_output_prefix(model_family)

  all_path <- file.path(OUT_DIR, paste0(prefix, "_FINAL_MODEL_legacy_wide_test_val.csv"))
  if (NEXTGEN_PRODUCTION_REFIT && file.exists(all_path)) {
    existing <- tryCatch(utils::read.csv(all_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
    if (nrow(existing) > 0 && all(c("season", "projection_sample") %in% names(existing))) {
      keep <- !(suppressWarnings(as.integer(existing$season)) == VAL_SEASON & as.character(existing$projection_sample) == "val")
      keep[is.na(keep)] <- TRUE
      legacy_wide <- bind_csv_rows_compatible(existing[keep, , drop = FALSE], legacy_wide)
    }
  }
  utils::write.csv(legacy_wide, all_path, row.names = FALSE, na = "")
  paths <- c(paths, all_path)

  if ("projection_sample" %in% names(legacy_wide)) {
    for (sm in intersect(samples, unique(legacy_wide$projection_sample))) {
      sub <- legacy_wide[legacy_wide$projection_sample == sm, , drop = FALSE]
      sm_path <- file.path(OUT_DIR, paste0(prefix, "_FINAL_MODEL_legacy_wide_", sm, ".csv"))
      utils::write.csv(sub, sm_path, row.names = FALSE, na = "")
      paths <- c(paths, sm_path)

      season_values <- sort(unique(sub$season[is.finite(sub$season)]))
      if (length(season_values) == 1) {
        season_path <- file.path(OUT_DIR, paste0(season_values[[1]], "_", prefix, "_FINAL_MODEL_legacy_wide_", sm, ".csv"))
        utils::write.csv(sub, season_path, row.names = FALSE, na = "")
        paths <- c(paths, season_path)
      }
    }
  }
  paths
}

show_legacy_wide_predictions_preview <- function(results, model_family = "model_family", n = 5) {
  legacy <- results$legacy_wide_predictions
  if (is.null(legacy) || nrow(legacy) == 0) {
    cat("No legacy-wide final prediction file was created. Check final model selection, target availability, and prep data.\n")
    return(invisible(NULL))
  }

  cat("\n================ LEGACY-WIDE FINAL TEST/VALIDATION PREDICTION CSV PREVIEW ================\n")
  cat("Rows:", nrow(legacy), " Columns:", ncol(legacy), "\n")

  tag <- legacy_tag_from_family(model_family)
  preview_cols <- intersect(c(
    "projection_sample", "season", "week", "game_date", "game_id", "posteam", "defteam", "home_team", "away_team",
    "away_score", "home_score", "spread_line", "home_margin_line", "total_line", "home_implied", "away_implied",
    paste0("Score_", tag), paste0("OppScore_", tag), paste0("ScoreTotal_", tag), paste0("ScoreDiff_", tag),
    paste0("Cover_Score_", tag), paste0("Cover_OppScore_", tag), paste0("Cover_Total_", tag), paste0("Cover_ScoreDiff_", tag),
    paste0("Edge_Score_", tag), paste0("Edge_OppScore_", tag), paste0("Edge_Total_", tag), paste0("Edge_ScoreDiff_", tag),
    paste0("PredictedScore_", tag)
  ), names(legacy))

  compact_table(legacy[, preview_cols, drop = FALSE], n = n)
  cat("\nLegacy-wide final prediction file written to:", file.path(OUT_DIR, paste0(model_output_prefix(model_family), "_FINAL_MODEL_legacy_wide_test_val.csv")), "\n")
  invisible(legacy)
}

finish_model_run <- function(model_family, leaderboard, predictions, variable_detail = data.frame(), preferred_sample = MODEL_SELECTION_SAMPLE) {
  output_prefix <- model_output_prefix(model_family)
  output_samples <- if (NEXTGEN_PRODUCTION_REFIT) "val" else c("test", "val")
  leaderboard <- as.data.frame(leaderboard)
  predictions <- as.data.frame(predictions)
  if (nrow(leaderboard) > 0) leaderboard <- leaderboard %>% arrange(target, sample, RMSE, MAE)

  final_models <- choose_final_models(leaderboard, preferred_sample = preferred_sample, metric = MODEL_SELECTION_METRIC)
  market_summary <- summarise_market_results(predictions)
  edge_buckets <- summarise_edge_buckets(predictions)
  small_final_predictions <- make_clean_final_predictions(predictions, final_models, samples = output_samples)
  legacy_wide_predictions <- make_legacy_wide_final_predictions(predictions, final_models, model_family = model_family, samples = output_samples)

  if (isTRUE(WRITE_FULL_DIAGNOSTIC_CSVS)) {
    utils::write.csv(leaderboard, file.path(OUT_DIR, paste0(output_prefix, "_leaderboard.csv")), row.names = FALSE, na = "")
    utils::write.csv(predictions, file.path(OUT_DIR, paste0(output_prefix, "_predictions.csv")), row.names = FALSE, na = "")
    utils::write.csv(market_summary, file.path(OUT_DIR, paste0(output_prefix, "_market_score_summary.csv")), row.names = FALSE, na = "")
    utils::write.csv(edge_buckets, file.path(OUT_DIR, paste0(output_prefix, "_edge_bucket_summary.csv")), row.names = FALSE, na = "")
  }
  final_models_suffix <- if (NEXTGEN_PRODUCTION_REFIT) "_production_refit_models.csv" else "_final_models.csv"
  utils::write.csv(final_models, file.path(OUT_DIR, paste0(output_prefix, final_models_suffix)), row.names = FALSE, na = "")
  if (isTRUE(WRITE_SMALL_FINAL_PREDICTIONS)) {
    small_path <- file.path(OUT_DIR, paste0(model_output_prefix(model_family), "_FINAL_MODEL_small_predictions_test_val.csv"))
    if (NEXTGEN_PRODUCTION_REFIT && file.exists(small_path)) {
      existing_small <- tryCatch(utils::read.csv(small_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
      if (nrow(existing_small) > 0 && all(c("season", "sample") %in% names(existing_small))) {
        keep <- !(suppressWarnings(as.integer(existing_small$season)) == VAL_SEASON & as.character(existing_small$sample) == "val")
        keep[is.na(keep)] <- TRUE
        small_final_predictions <- bind_csv_rows_compatible(existing_small[keep, , drop = FALSE], small_final_predictions)
      }
    }
    utils::write.csv(small_final_predictions, small_path, row.names = FALSE, na = "")
  }
  legacy_paths <- write_legacy_wide_prediction_files(legacy_wide_predictions, model_family = model_family, samples = output_samples)

  final_variables <- data.frame()
  if (!is.null(variable_detail) && nrow(variable_detail) > 0 && nrow(final_models) > 0) {
    variable_detail <- as.data.frame(variable_detail)
    final_keys <- final_models %>% select(target, fit_id)
    final_variables <- variable_detail %>%
      inner_join(final_keys, by = c("target", "fit_id"))
    if ("coefficient" %in% names(final_variables)) {
      final_variables <- final_variables %>% mutate(abs_value = abs(coefficient)) %>% arrange(target, desc(abs_value))
    } else if ("importance" %in% names(final_variables)) {
      final_variables <- final_variables %>% arrange(target, desc(importance))
    } else {
      final_variables <- final_variables %>% arrange(target, variable)
    }
    utils::write.csv(final_variables, file.path(OUT_DIR, paste0(output_prefix, "_final_selected_variables.csv")), row.names = FALSE, na = "")
  }

  final_market <- data.frame()
  if (nrow(market_summary) > 0 && nrow(final_models) > 0) {
    final_market <- market_summary %>%
      inner_join(final_models %>% select(target, fit_id), by = c("target", "fit_id")) %>%
      arrange(target, sample)
    utils::write.csv(final_market, file.path(OUT_DIR, paste0(output_prefix, "_FINAL_MODEL_market_scoring.csv")), row.names = FALSE, na = "")
  }

  final_edge_buckets <- data.frame()
  if (nrow(edge_buckets) > 0 && nrow(final_models) > 0) {
    final_edge_buckets <- edge_buckets %>%
      inner_join(final_models %>% select(target, fit_id), by = c("target", "fit_id")) %>%
      arrange(target, sample, edge_bucket, predicted_side)
    utils::write.csv(final_edge_buckets, file.path(OUT_DIR, paste0(output_prefix, "_FINAL_MODEL_edge_buckets.csv")), row.names = FALSE, na = "")
  }

  final_score_summary <- data.frame()
  if (nrow(leaderboard) > 0 && nrow(final_models) > 0) {
    final_score_summary <- leaderboard %>%
      inner_join(final_models %>% select(target, fit_id), by = c("target", "fit_id")) %>%
      select(any_of(c("target", "sample", "model_label", "n", "n_variables", "RMSE", "MAE", "Bias", "Correlation")))
    if (nrow(final_market) > 0) {
      final_score_summary <- final_score_summary %>%
        left_join(
          final_market %>% select(target, sample, n_decisions, correct, wrong, accuracy, avg_edge, avg_abs_edge),
          by = c("target", "sample")
        )
    }
    final_score_summary <- final_score_summary %>% arrange(target, factor(sample, levels = c("train", "test", "val")))
    utils::write.csv(final_score_summary, file.path(OUT_DIR, paste0(output_prefix, "_FINAL_MODEL_score_summary.csv")), row.names = FALSE, na = "")
  }

  if (!isTRUE(QUIET_MODEL_RUN)) {
    cat("\n================ FINAL MODEL CHOICE BY TARGET ================\n")
    cat("Selection sample:", preferred_sample, " | metric:", MODEL_SELECTION_METRIC, " | tie-breakers: MAE then abs(Bias)\n")
    final_model_cols <- intersect(c("target", "sample", "model_label", "n", "n_variables", "RMSE", "MAE", "Bias", "Correlation"), names(final_models))
    compact_table(final_models[, final_model_cols, drop = FALSE], n = 50)

    cat("\nFiles written to:", OUT_DIR, "\n")
  }
  invisible(list(
    leaderboard = leaderboard,
    predictions = predictions,
    final_models = final_models,
    market_summary = market_summary,
    edge_buckets = edge_buckets,
    final_market = final_market,
    final_edge_buckets = final_edge_buckets,
    final_score_summary = final_score_summary,
    final_variables = final_variables,
    small_final_predictions = small_final_predictions,
    legacy_wide_predictions = legacy_wide_predictions,
    legacy_paths = legacy_paths
  ))
}

show_leaderboard_summary <- function(results, model_family = "model_family", top_n = PRINT_TOP_N_LEADERBOARD) {
  leaderboard <- results$leaderboard
  if (is.null(leaderboard) || nrow(leaderboard) == 0) {
    cat("No leaderboard rows available.\n")
    return(invisible(NULL))
  }
  cat("\n================ TOP TEST LEADERBOARD ROWS ================\n")
  test_rows <- leaderboard %>%
    filter(sample == "test") %>%
    group_by(target) %>%
    arrange(RMSE, MAE, abs(Bias), .by_group = TRUE) %>%
    slice_head(n = top_n) %>%
    ungroup() %>%
    select(any_of(c("target", "sample", "model_label", "n", "n_variables", "RMSE", "MAE", "Bias", "Correlation")))
  compact_table(test_rows, n = 4 * top_n)

  cat("\n================ TOP VALIDATION LEADERBOARD ROWS ================\n")
  val_rows <- leaderboard %>%
    filter(sample == "val") %>%
    group_by(target) %>%
    arrange(RMSE, MAE, abs(Bias), .by_group = TRUE) %>%
    slice_head(n = top_n) %>%
    ungroup() %>%
    select(any_of(c("target", "sample", "model_label", "n", "n_variables", "RMSE", "MAE", "Bias", "Correlation")))
  compact_table(val_rows, n = 4 * top_n)

  invisible(list(test = test_rows, val = val_rows))
}


show_final_success_rates <- function(results, top_n = 50) {
  score <- results$final_score_summary
  if (is.null(score) || nrow(score) == 0) {
    cat("No final score summary is available.\n")
    return(invisible(NULL))
  }
  cat("\n================ FINAL MODEL TRAIN / TEST / VAL SUMMARY ================\n")
  for (nm in c("accuracy", "n_decisions", "correct", "wrong", "avg_edge", "avg_abs_edge")) {
    if (!(nm %in% names(score))) score[[nm]] <- NA_real_
  }
  show <- score %>%
    mutate(
      accuracy = ifelse(is.finite(accuracy), accuracy, NA_real_),
      cover_pct = accuracy
    ) %>%
    select(any_of(c(
      "target", "sample", "n", "n_variables", "RMSE", "MAE", "Bias", "Correlation",
      "n_decisions", "correct", "wrong", "cover_pct", "avg_edge", "avg_abs_edge", "model_label"
    ))) %>%
    arrange(target, factor(sample, levels = c("train", "test", "val")))
  compact_table(show, n = top_n)
  invisible(show)
}

show_final_market_scoring <- function(results, top_n = 200) {
  final_market <- results$final_market
  if (is.null(final_market) || nrow(final_market) == 0) {
    cat("No final-model market scoring rows created. Check that spread_line, total_line, home_implied, and away_implied exist.\n")
    return(invisible(NULL))
  }

  cat("\n================ FINAL MODEL MARKET SCORING: BY TARGET AND SAMPLE ================\n")
  show <- final_market %>%
    select(target, market, sample, model_label, n_decisions, correct, wrong, accuracy, avg_edge, avg_abs_edge, pct_above_line) %>%
    arrange(target, factor(sample, levels = c("train", "test", "val")))
  compact_table(show, n = top_n)

  invisible(show)
}

show_final_edge_buckets <- function(results, top_n = 400) {
  final_edge_buckets <- results$final_edge_buckets
  if (is.null(final_edge_buckets) || nrow(final_edge_buckets) == 0) {
    cat("No final-model edge bucket rows created.\n")
    return(invisible(NULL))
  }

  cat("\n================ FINAL MODEL EDGE BUCKETS / ZONES ================\n")
  show <- final_edge_buckets %>%
    select(target, market, sample, predicted_side, edge_bucket, n_decisions, correct, wrong, accuracy, avg_edge, avg_abs_edge) %>%
    arrange(target, factor(sample, levels = c("train", "test", "val")), edge_bucket, predicted_side)
  compact_table(show, n = top_n)

  invisible(show)
}

plot_final_edge_zones <- function(results, model_family = "model_family") {
  final_edge_buckets <- results$final_edge_buckets
  if (is.null(final_edge_buckets) || nrow(final_edge_buckets) == 0) return(invisible(NULL))

  plot_df <- final_edge_buckets %>%
    filter(sample %in% c("test", "val"), n_decisions > 0) %>%
    mutate(edge_bucket = factor(edge_bucket, levels = c("0-1", "1-2", "2-3", "3-4", "4-6", "6+")))

  if (nrow(plot_df) == 0) return(invisible(NULL))

  p <- ggplot(plot_df, aes(x = edge_bucket, y = accuracy, group = predicted_side)) +
    geom_line() +
    geom_point() +
    facet_grid(target ~ sample) +
    labs(
      title = paste(model_family, "final model market accuracy by edge zone"),
      x = "Absolute model edge bucket",
      y = "Market accuracy"
    ) +
    theme_minimal(base_size = 11)
  print(p)
  ggsave(file.path(OUT_DIR, paste0(model_output_prefix(model_family), "_FINAL_MODEL_edge_zone_accuracy.png")), p, width = 13, height = 9)
  invisible(p)
}

show_final_variables_by_target <- function(results, top_n = PRINT_TOP_N_FINAL_VARIABLES) {
  final_variables <- results$final_variables
  if (is.null(final_variables) || nrow(final_variables) == 0) {
    cat("No final variable detail was produced for this model family.\n")
    return(invisible(NULL))
  }

  display_cols <- intersect(c("target", "model_label", "variable", "coefficient", "importance", "cover", "frequency", "abs_value"), names(final_variables))

  for (tg in unique(final_variables$target)) {
    cat("\n================ FINAL VARIABLES / IMPORTANCE:", tg, "================\n")
    sub <- final_variables %>% filter(target == tg)
    if ("coefficient" %in% names(sub)) {
      sub <- sub %>% arrange(desc(abs(coefficient)))
    } else if ("importance" %in% names(sub)) {
      sub <- sub %>% arrange(desc(importance))
    }
    compact_table(sub[, display_cols, drop = FALSE], n = top_n)
    cat("Total non-zero/used variables for", tg, ":", nrow(sub), "\n")
  }

  invisible(final_variables)
}

prepare_v12_modeling_data <- function() {
  data <- load_v12_data()
  data <- create_v12_targets(data)

  target_avail <- data.frame(
    target = TARGET_SPECS$target,
    n_available = vapply(TARGET_SPECS$target, function(x) if (x %in% names(data)) sum(is.finite(data[[x]])) else 0, numeric(1)),
    stringsAsFactors = FALSE
  )
  if (isTRUE(PRINT_TARGET_AVAILABILITY) && !isTRUE(QUIET_MODEL_RUN)) {
    cat("\nTarget availability:\n")
    compact_table(target_avail, n = 20)
  }

  model_data <- collapse_to_game_level(data)
  splits <- make_train_test_val(model_data)

  baseline <- make_market_baseline_metrics(splits)
  if (nrow(baseline) > 0) {
    if (isTRUE(WRITE_MARKET_BASELINE_CSV)) {
      utils::write.csv(baseline, file.path(OUT_DIR, paste0("market_line_baseline_metrics_", get_training_week_phase(), ".csv")), row.names = FALSE, na = "")
    }
    if (isTRUE(PRINT_MARKET_BASELINE_METRICS) && !isTRUE(QUIET_MODEL_RUN)) {
      cat("\nMarket line baseline metrics, for reference:\n")
      compact_table(baseline %>% select(target, sample, model_label, n, RMSE, MAE, Bias, Correlation), n = 30)
    }
  }

  if (isTRUE(WRITE_MARKET_CONVENTION_AUDIT_CSV)) {
    audit <- make_market_convention_audit(splits)
    if (nrow(audit) > 0) {
      utils::write.csv(audit, file.path(OUT_DIR, paste0("market_implied_total_convention_audit_", get_training_week_phase(), ".csv")), row.names = FALSE, na = "")
    }
  }

  prep <- list(data = data, model_data = model_data, splits = splits, train = splits$train, test = splits$test, val = splits$val)
  assign(".V12_LAST_PREP", prep, envir = .GlobalEnv)
  prep
}
