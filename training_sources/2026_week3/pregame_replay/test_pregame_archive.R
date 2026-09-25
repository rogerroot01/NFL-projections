source("pregame_projection_archive.R")

pipeline_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
root <- dirname(pipeline_dir)
checks <- 0L

check_one <- function(current, original, week, label) {
  for (file in intersect(names(current), names(original))) {
    if (!grepl("^2026_", file)) next
    a <- current[[file]]
    b <- original[[file]]
    b <- b[!is.na(b$week) & b$week == week, , drop = FALSE]
    a <- a[!is.na(a$week) & a$week == week, , drop = FALSE]
    if (!nrow(b)) stop(label, " has no archived games: ", file, call. = FALSE)
    i <- match(pregame_row_key(b), pregame_row_key(a))
    if (anyNA(i)) stop(label, " is missing an archived game: ", file, call. = FALSE)
    prediction_cols <- grep(
      "^(Score_|HomeScore_|AwayScore_|OppScore_|ScoreTotal_|TotalScore_|ScoreDiff_|HomeMargin_|PredictedScore_)",
      names(b), value = TRUE
    )
    for (col in prediction_cols) {
      if (!col %in% names(a) ||
          !isTRUE(all.equal(as.character(a[[col]][i]), as.character(b[[col]]), check.attributes = FALSE))) {
        stop(label, " changed pregame value: ", file, " / ", col, call. = FALSE)
      }
    }
    checks <<- checks + nrow(b)
  }
}

ng_current <- readRDS(file.path(pipeline_dir, "output", "ensemble_model_wrangler_nextgen", "data", "nextgen_compact_models.rds"))
for (week in 1:2) {
  snapshot <- if (week == 1) {
    file.path(root, "Football_projection_pipeline_exclusion_week1", "Football_projection_pipeline_exclusion", "output", "ensemble_model_wrangler_nextgen", "data", "nextgen_compact_models.rds")
  } else {
    file.path(root, "Football_projection_pipeline_exclusion_week2", "output", "ensemble_model_wrangler_nextgen", "data", "nextgen_compact_models.rds")
  }
  check_one(ng_current, readRDS(snapshot), week, paste("Next Gen Week", week))
}
cat("Verified", checks, "Next Gen model-game pregame rows.\n")

legacy_path <- file.path(pipeline_dir, "output", "ensemble_model_wrangler", "data")
legacy_current <- readRDS(file.path(legacy_path, "compact_models.rds"))
legacy_archive <- readRDS(file.path(legacy_path, "legacy_pregame_2026_archive.rds"))
legacy_checks <- 0L
for (file in intersect(names(legacy_current), names(legacy_archive))) {
  if (!grepl("^2026_", file)) next
  a <- legacy_current[[file]]
  b <- legacy_archive[[file]]
  b <- b[!is.na(b$week) & b$week <= 2L, , drop = FALSE]
  a <- a[!is.na(a$week) & a$week <= 2L, , drop = FALSE]
  i <- match(pregame_row_key(b), pregame_row_key(a))
  if (anyNA(i)) stop("Legacy is missing archived game: ", file, call. = FALSE)
  for (col in setdiff(names(b), pregame_key_columns)) {
    if (!col %in% names(a) ||
        !isTRUE(all.equal(as.character(a[[col]][i]), as.character(b[[col]]), check.attributes = FALSE))) {
      stop("Legacy changed pregame value: ", file, " / ", col, call. = FALSE)
    }
  }
  legacy_checks <- legacy_checks + nrow(b)
}
cat("Verified", legacy_checks, "Legacy model-game pregame rows.\n")

# A newly completed 2026 outlier must not alter a pre-2026 imputation.
source(file.path("R", "02_add_rolling_team_averages.R"))
source(file.path("R", "03_normalize_and_add_features.R"))
source(file.path("R", "04_add_normalized_team_averages.R"))
toy <- data.frame(season = c(2025L, 2025L, 2026L),
                  fumble_lost_rate = c(2, NA_real_, 200),
                  other = c(2, NaN, 200))
toy_outlier <- toy
toy_outlier$fumble_lost_rate[[3]] <- 2000
toy_outlier$other[[3]] <- 2000
for (fn in list(impute_selected_feature_na, fix_numeric_nan_with_column_mean, fix_nan_numeric_columns)) {
  a <- suppressWarnings(fn(toy))
  b <- suppressWarnings(fn(toy_outlier))
  if (!identical(a[1:2, , drop = FALSE], b[1:2, , drop = FALSE])) {
    stop("2026 data changed historical imputation in ", deparse(substitute(fn)), call. = FALSE)
  }
}
cat("Verified pre-2026 imputation is invariant to new 2026 outcomes.\n")
