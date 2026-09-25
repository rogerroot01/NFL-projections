library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tidyr)
library(tibble)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

script_path <- tryCatch({
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE)
  } else {
    normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE)
  }
}, error = function(e) NA_character_)
pipeline_dir <- if (!is.na(script_path)) {
  dirname(script_path)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
source(file.path(pipeline_dir, "pregame_projection_archive.R"), local = TRUE)

framework_dirs <- c(
  late = file.path(pipeline_dir, "New_Models_Late_2025", "output", "v12_model_search"),
  early = file.path(pipeline_dir, "New_Models_Early_2025", "output", "v12_model_search")
)

# Route each displayed season to the model run that actually produced that
# season's holdout/projection. This prevents a stale copied 2024 CSV inside a
# 2025 folder from silently taking precedence over the real 2024 model run.
framework_season_dirs <- list(
  late = c(
    `2024` = file.path(pipeline_dir, "New_Models_Late_2024", "output", "v12_model_search"),
    `2025` = file.path(pipeline_dir, "New_Models_Late_2025", "output", "v12_model_search"),
    `2026` = file.path(pipeline_dir, "New_Models_Late_2025", "output", "v12_model_search")
  ),
  early = c(
    `2024` = file.path(pipeline_dir, "New_Models_Early_2024", "output", "v12_model_search"),
    `2025` = file.path(pipeline_dir, "New_Models_Early_2025", "output", "v12_model_search"),
    `2026` = file.path(pipeline_dir, "New_Models_Early_2025", "output", "v12_model_search")
  )
)

weighted_framework_dirs <- list(
  late = c(
    file.path(pipeline_dir, "New_Models_Late_2024", "output", "v12_model_search"),
    file.path(pipeline_dir, "New_Models_Late_2025", "output", "v12_model_search")
  ),
  early = c(
    file.path(pipeline_dir, "New_Models_Early_2024", "output", "v12_model_search"),
    file.path(pipeline_dir, "New_Models_Early_2025", "output", "v12_model_search")
  )
)

export_dir <- file.path(pipeline_dir, "output", "ensemble_model_wrangler_nextgen")
export_data_dir <- file.path(export_dir, "data")

app_deploy_data_dir <- file.path(
  dirname(pipeline_dir),
  "Apps",
  "Projection_Model_Wrangler_RDS_deploy",
  "data"
)

for (nm in names(framework_dirs)) {
  if (!dir.exists(framework_dirs[[nm]])) {
    stop("Missing ", nm, " next-gen model output folder: ", framework_dirs[[nm]], call. = FALSE)
  }
}
for (framework in names(framework_season_dirs)) {
  for (dir in unique(unname(framework_season_dirs[[framework]]))) {
    if (!dir.exists(dir)) {
      stop("Missing ", framework, " season-specific next-gen output folder: ", dir, call. = FALSE)
    }
  }
}
for (framework in names(weighted_framework_dirs)) {
  for (dir in weighted_framework_dirs[[framework]]) {
    if (!dir.exists(dir)) {
      stop("Missing ", framework, " weighted next-gen model output folder: ", dir, call. = FALSE)
    }
  }
}
if (!dir.exists(export_data_dir)) {
  dir.create(export_data_dir, recursive = TRUE)
}
if (!dir.exists(app_deploy_data_dir)) {
  dir.create(app_deploy_data_dir, recursive = TRUE)
}

next_gen_family_labels <- c(
  elastic_net_lasso = "Elastic Net Lasso",
  weighted_linear_regression = "Weighted Linear Regression",
  decision_tree_rpart = "Decision Tree",
  random_forest_ranger = "Random Forest",
  gbm_boosted_trees = "GBM Boosted Trees",
  xgboost_regression = "XGBoost Regression"
)

expected_samples <- c(
  `2024` = "test",
  `2025` = "test",
  `2026` = "val"
)

parse_nextgen_file <- function(path, framework) {
  nm <- basename(path)
  framework_pattern <- paste0("(", framework, ")")
  m <- str_match(
    nm,
    paste0("^(\\d{4})_(.+)_", framework_pattern, "_FINAL_MODEL_legacy_wide_(test|val)\\.csv$")
  )
  if (any(is.na(m))) return(NULL)

  season <- as.integer(m[, 2])
  family <- m[, 3]
  sample <- m[, 5]
  expected_sample <- unname(expected_samples[as.character(season)] %||% NA_character_)
  if (is.na(expected_sample) || !identical(sample, expected_sample)) return(NULL)
  if (!family %in% names(next_gen_family_labels)) return(NULL)
  if (identical(family, "weighted_linear_regression")) return(NULL)

  tibble(
    path = path,
    file = nm,
    source_file = nm,
    season = season,
    framework = framework,
    sample = sample,
    family = family,
    family_label = next_gen_family_labels[[family]]
  )
}

parse_weighted_combined_file <- function(dir, framework) {
  nm <- paste0("weighted_linear_regression_", framework, "_FINAL_MODEL_legacy_wide_test_val.csv")
  path <- file.path(dir, nm)
  if (!file.exists(path)) return(tibble())
  available <- suppressMessages(readr::read_csv(path, col_select = dplyr::all_of(c("season", "projection_sample")), show_col_types = FALSE, progress = FALSE)) %>%
    distinct(season, projection_sample)
  seasons <- as.integer(names(expected_samples))
  samples <- unname(expected_samples)
  keep <- tibble(season = seasons, sample = samples) %>%
    semi_join(available, by = c("season" = "season", "sample" = "projection_sample"))
  if (nrow(keep) == 0) return(tibble())

  tibble(
    path = path,
    file = paste0(
      keep$season,
      "_weighted_linear_regression_",
      framework,
      "_FINAL_MODEL_legacy_wide_",
      keep$sample,
      ".csv"
    ),
    source_file = nm,
    season = keep$season,
    framework = framework,
    sample = keep$sample,
    family = "weighted_linear_regression",
    family_label = next_gen_family_labels[["weighted_linear_regression"]]
  )
}

read_nextgen_source_file <- function(meta, cols = NULL) {
  if (is.null(cols)) {
    df <- suppressMessages(readr::read_csv(meta$path, show_col_types = FALSE, progress = FALSE))
  } else {
    df <- suppressMessages(readr::read_csv(meta$path, col_select = dplyr::all_of(cols), show_col_types = FALSE, progress = FALSE))
  }
  if (!identical(meta$file, meta$source_file) && all(c("season", "projection_sample") %in% names(df))) {
    df <- df %>%
      filter(season == meta$season, projection_sample == meta$sample)
  }
  df
}

projection_candidates_for_cover <- function(cover_col) {
  base <- str_remove(cover_col, "^Cover_")
  c(
    base,
    str_replace(base, "^Total_", "ScoreTotal_"),
    str_replace(base, "^Total_", "TotalScore_"),
    str_replace(base, "^Total_", "ImpliedTotal_"),
    str_replace(base, "^Team_ImpliedTotal_", "ImpliedTeamScored_"),
    str_replace(base, "^Opp_ImpliedTotal_", "ImpliedOppScored_"),
    str_replace(base, "^Score$", "Score_final"),
    str_replace(base, "^ScoreDiff$", "ScoreDiff_final")
  ) %>% unique()
}

prediction_cols_from_names <- function(cols) {
  family_suffix <- paste0("_(", paste(names(next_gen_family_labels), collapse = "|"), ")$")
  cols[
    str_detect(
      cols,
      regex(
        paste(
          "^Score_",
          "^HomeScore_",
          "^AwayScore_",
          "^OppScore_",
          "^ScoreTotal_",
          "^TotalScore_",
          "^ScoreDiff_",
          "^HomeMargin_",
          "^PredictedScore_",
          sep = "|"
        ),
        TRUE
      )
    ) &
      str_detect(cols, regex(family_suffix, TRUE)) &
      !str_detect(cols, regex("^Cover_|_target|_cover$|_pm1$", TRUE))
  ]
}

add_implied_from_team_scores <- function(df) {
  fill_missing_team_scores_from_margin_total <- function(data) {
    home_cols <- names(data)[str_starts(names(data), "HomeScore_")]
    margin_cols <- names(data)[str_starts(names(data), "HomeMargin_")]

    for (home_col in home_cols) {
      suffix <- str_remove(home_col, "^HomeScore_")
      away_col <- paste0("AwayScore_", suffix)
      margin_col <- paste0("HomeMargin_", suffix)
      total_col <- paste0("TotalScore_", suffix)
      if (!all(c(away_col, margin_col, total_col) %in% names(data))) next

      home_val <- suppressWarnings(as.numeric(data[[home_col]]))
      away_val <- suppressWarnings(as.numeric(data[[away_col]]))
      margin_val <- suppressWarnings(as.numeric(data[[margin_col]]))
      total_val <- suppressWarnings(as.numeric(data[[total_col]]))
      can_fill <- !is.na(margin_val) & !is.na(total_val)

      data[[home_col]] <- dplyr::if_else(is.na(home_val) & can_fill, (total_val + margin_val) / 2, home_val)
      data[[away_col]] <- dplyr::if_else(is.na(away_val) & can_fill, (total_val - margin_val) / 2, away_val)
    }

    score_cols <- names(data)[str_starts(names(data), "Score_")]
    for (score_col in score_cols) {
      suffix <- str_remove(score_col, "^Score_")
      opp_col <- paste0("OppScore_", suffix)
      margin_col <- paste0("ScoreDiff_", suffix)
      total_col <- paste0("ScoreTotal_", suffix)
      if (!all(c(opp_col, margin_col, total_col) %in% names(data))) next

      score_val <- suppressWarnings(as.numeric(data[[score_col]]))
      opp_val <- suppressWarnings(as.numeric(data[[opp_col]]))
      margin_val <- suppressWarnings(as.numeric(data[[margin_col]]))
      total_val <- suppressWarnings(as.numeric(data[[total_col]]))
      can_fill <- !is.na(margin_val) & !is.na(total_val)

      data[[score_col]] <- dplyr::if_else(is.na(score_val) & can_fill, (total_val + margin_val) / 2, score_val)
      data[[opp_col]] <- dplyr::if_else(is.na(opp_val) & can_fill, (total_val - margin_val) / 2, opp_val)
    }

    data
  }

  add_pair_derivations <- function(data, left_prefix, right_prefix, margin_prefix, total_prefix) {
    left_cols <- names(data)[str_starts(names(data), left_prefix)]
    if (length(left_cols) == 0) return(data)

    for (left_col in left_cols) {
      suffix <- str_remove(left_col, paste0("^", left_prefix))
      right_col <- paste0(right_prefix, suffix)
      if (!right_col %in% names(data)) next

      margin_col <- paste0(margin_prefix, suffix)
      total_col <- paste0(total_prefix, suffix)
      left_val <- suppressWarnings(as.numeric(data[[left_col]]))
      right_val <- suppressWarnings(as.numeric(data[[right_col]]))

      if (!margin_col %in% names(data)) {
        data[[margin_col]] <- left_val - right_val
      }
      if (!total_col %in% names(data)) {
        data[[total_col]] <- left_val + right_val
      }

      if (all(c("home_score", "away_score", "spread_line") %in% names(data))) {
        margin_cover_col <- paste0("Cover_", margin_col)
        actual_margin <- suppressWarnings(as.numeric(data[["home_score"]])) - suppressWarnings(as.numeric(data[["away_score"]]))
        spread_line <- suppressWarnings(as.numeric(data[["spread_line"]]))
        if (!margin_cover_col %in% names(data)) {
          data[[margin_cover_col]] <- dplyr::case_when(
            is.na(data[[margin_col]]) | is.na(actual_margin) | is.na(spread_line) ~ NA_real_,
            data[[margin_col]] == spread_line | actual_margin == spread_line ~ NA_real_,
            (data[[margin_col]] > spread_line) == (actual_margin > spread_line) ~ 1,
            TRUE ~ 0
          )
        }
      }

      if (all(c("home_score", "away_score", "total_line") %in% names(data))) {
        total_cover_col <- paste0("Cover_", total_col)
        actual_total <- suppressWarnings(as.numeric(data[["home_score"]])) + suppressWarnings(as.numeric(data[["away_score"]]))
        total_line <- suppressWarnings(as.numeric(data[["total_line"]]))
        if (!total_cover_col %in% names(data)) {
          data[[total_cover_col]] <- dplyr::case_when(
            is.na(data[[total_col]]) | is.na(actual_total) | is.na(total_line) ~ NA_real_,
            data[[total_col]] == total_line | actual_total == total_line ~ NA_real_,
            (data[[total_col]] > total_line) == (actual_total > total_line) ~ 1,
            TRUE ~ 0
          )
        }
      }
    }

    data
  }

  df %>%
    fill_missing_team_scores_from_margin_total() %>%
    add_pair_derivations(
      left_prefix = "HomeScore_",
      right_prefix = "AwayScore_",
      margin_prefix = "HomeMargin_ImpliedFromTeamScores_",
      total_prefix = "TotalScore_ImpliedFromTeamScores_"
    ) %>%
    add_pair_derivations(
      left_prefix = "Score_",
      right_prefix = "OppScore_",
      margin_prefix = "HomeMargin_ImpliedFromTeamScores_",
      total_prefix = "TotalScore_ImpliedFromTeamScores_"
    )
}

build_prediction_column_inventory <- function(inventory) {
  purrr::map_dfr(seq_len(nrow(inventory)), function(i) {
    meta <- inventory[i, ]
    df <- read_nextgen_source_file(meta) %>%
      add_implied_from_team_scores()
    pred_cols <- prediction_cols_from_names(names(df))

    if (length(pred_cols) == 0) {
      return(tibble(
        File = meta$file,
        Season = meta$season,
        Framework = meta$framework,
        Sample = meta$sample,
        Family = meta$family_label,
        PredictionColumn = NA_character_,
        ColumnPosition = NA_integer_,
        NonMissingRows = NA_integer_,
        MissingRows = NA_integer_,
        AllMissing = NA
      ))
    }

    tibble(
      File = meta$file,
      Season = meta$season,
      Framework = meta$framework,
      Sample = meta$sample,
      Family = meta$family_label,
      PredictionColumn = pred_cols,
      ColumnPosition = match(pred_cols, names(df)),
      NonMissingRows = vapply(df[pred_cols], function(x) sum(!is.na(x)), integer(1)),
      MissingRows = vapply(df[pred_cols], function(x) sum(is.na(x)), integer(1)),
      AllMissing = vapply(df[pred_cols], function(x) all(is.na(x)), logical(1))
    )
  }) %>%
    arrange(Framework, Family, Season, File, ColumnPosition)
}

key_cols_from_names <- function(cols) {
  intersect(
    c(
      "projection_sample", "market", "game_id", "season_type", "season", "week", "game_date",
      "posteam_type", "posteam", "defteam", "model_team", "home_team", "away_team",
      "home_score", "away_score", "spread_line", "total_line", "home_implied", "away_implied",
      "posteam_implied", "defteam_implied", "line", "actual_market_result",
      "predicted_market_result", "actual_push"
    ),
    cols
  )
}

cols_needed_for_names <- function(cols) {
  cover_cols <- grep("^Cover_", cols, value = TRUE)
  cover_projection_cols <- unique(unlist(purrr::map(cover_cols, projection_candidates_for_cover), use.names = FALSE))
  unique(c(
    key_cols_from_names(cols),
    cover_cols,
    prediction_cols_from_names(cols),
    cover_projection_cols
  )) %>%
    intersect(cols)
}

parsed_files <- imap(framework_season_dirs, function(season_dirs, framework) {
  yearly_files <- purrr::imap(season_dirs, function(dir, season_name) {
    list.files(
      dir,
      pattern = paste0("^", season_name, "_.*\\.csv$"),
      full.names = TRUE
    ) %>%
      purrr::map(parse_nextgen_file, framework = framework)
  }) %>%
    unlist(recursive = FALSE)
  weighted_files <- purrr::map(weighted_framework_dirs[[framework]], parse_weighted_combined_file, framework = framework)
  c(yearly_files, weighted_files)
}) %>%
  unlist(recursive = FALSE)
parsed_files <- parsed_files[!vapply(parsed_files, is.null, logical(1))]

if (length(parsed_files) == 0) {
  stop(
    "No next-gen model CSVs matched the expected naming pattern in: ",
    paste(framework_dirs, collapse = "; "),
    ". Expected names like 2024_xgboost_regression_late_FINAL_MODEL_legacy_wide_test.csv ",
    "and 2026_xgboost_regression_early_FINAL_MODEL_legacy_wide_val.csv.",
    call. = FALSE
  )
}

inventory <- purrr::list_rbind(parsed_files) %>%
  arrange(framework, family, season, sample, file)

expected_grid <- tidyr::expand_grid(
  framework = names(framework_dirs),
  family = names(next_gen_family_labels),
  season = as.integer(names(expected_samples))
) %>%
  mutate(sample = unname(expected_samples[as.character(season)]))

missing_expected <- expected_grid %>%
  anti_join(inventory, by = c("framework", "family", "season", "sample"))

if (nrow(missing_expected) > 0) {
  warning(
    "Missing expected next-gen files:\n",
    paste(
      paste(missing_expected$framework, missing_expected$season, missing_expected$family, missing_expected$sample, sep = " / "),
      collapse = "\n"
    ),
    call. = FALSE
  )
}

prediction_column_inventory <- build_prediction_column_inventory(inventory)
readr::write_csv(prediction_column_inventory, file.path(export_dir, "nextgen_prediction_column_inventory.csv"), na = "")
readr::write_csv(prediction_column_inventory, file.path(export_data_dir, "nextgen_prediction_column_inventory.csv"), na = "")
readr::write_csv(prediction_column_inventory, file.path(app_deploy_data_dir, "nextgen_prediction_column_inventory.csv"), na = "")

compact_models <- purrr::map(seq_len(nrow(inventory)), function(i) {
  meta <- inventory[i, ]
  header <- names(suppressMessages(readr::read_csv(meta$path, n_max = 0, show_col_types = FALSE, progress = FALSE)))
  cols <- cols_needed_for_names(header)
  df <- read_nextgen_source_file(meta, cols)
  df <- add_implied_from_team_scores(df)
  pred_cols <- prediction_cols_from_names(names(df))
  empty_pred_cols <- pred_cols[vapply(df[pred_cols], function(x) all(is.na(x)), logical(1))]
  if (length(empty_pred_cols) > 0) {
    df <- df %>% select(-all_of(empty_pred_cols))
  }
  df
})

names(compact_models) <- inventory$file

archive_path <- file.path(export_data_dir, "nextgen_pregame_2026_archive.rds")
if (file.exists(archive_path)) {
  pregame_archive <- readRDS(archive_path)
} else {
  snapshot_files <- c(
    file.path(dirname(pipeline_dir), "Football_projection_pipeline_exclusion_week1", "Football_projection_pipeline_exclusion", "output", "ensemble_model_wrangler_nextgen", "data", "nextgen_compact_models.rds"),
    file.path(dirname(pipeline_dir), "Football_projection_pipeline_exclusion_week2", "output", "ensemble_model_wrangler_nextgen", "data", "nextgen_compact_models.rds"),
    file.path(dirname(pipeline_dir), "Football_projection_pipeline_exclusion_week3", "output", "ensemble_model_wrangler_nextgen", "data", "nextgen_compact_models.rds")
  )
  if (!all(file.exists(snapshot_files))) stop("Missing original Week 1-3 pregame snapshots", call. = FALSE)
  pregame_archive <- list()
  for (week in seq_along(snapshot_files)) {
    pregame_archive <- pregame_capture(
      pregame_archive, readRDS(snapshot_files[[week]]), week,
      prediction_cols_from_names, paste0("Next Gen Week ", week)
    )
  }
}
pregame_archive <- pregame_update_future(pregame_archive, compact_models, prediction_cols_from_names)
compact_models <- pregame_restore_completed(compact_models, pregame_archive, prediction_cols_from_names)
saveRDS(pregame_archive, archive_path)
saveRDS(pregame_archive, file.path(app_deploy_data_dir, "nextgen_pregame_2026_archive.rds"))

inventory_for_app <- inventory %>%
  select(-source_file) %>%
  mutate(path = file.path("data", file))

saveRDS(inventory_for_app, file.path(export_data_dir, "nextgen_model_inventory.rds"))
saveRDS(compact_models, file.path(export_data_dir, "nextgen_compact_models.rds"))
saveRDS(inventory_for_app, file.path(app_deploy_data_dir, "nextgen_model_inventory.rds"))
saveRDS(compact_models, file.path(app_deploy_data_dir, "nextgen_compact_models.rds"))

cat("Prepared", length(compact_models), "next-gen compact model files.\n")
cat("Frameworks:", paste(names(framework_dirs), collapse = ", "), "\n")
cat("Inventory rows:", nrow(inventory), "\n")
cat("Wrote prediction column inventory with", nrow(prediction_column_inventory), "rows.\n")
cat("Read source CSVs from:\n")
cat(paste(paste0("  - ", names(framework_dirs), ": ", framework_dirs), collapse = "\n"), "\n")
cat("Wrote app-ready next-gen files to:", export_data_dir, "\n")
cat("Also wrote next-gen files to deployed app data folder:", app_deploy_data_dir, "\n")
