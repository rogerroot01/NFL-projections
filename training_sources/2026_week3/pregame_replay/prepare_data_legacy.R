library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

pipeline_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
source(file.path(pipeline_dir, "pregame_projection_archive.R"), local = TRUE)
app_data_dir <- file.path(pipeline_dir, "output")
export_dir <- file.path(app_data_dir, "ensemble_model_wrangler")
export_data_dir <- file.path(export_dir, "data")

if (!dir.exists(app_data_dir)) {
  stop("Missing pipeline output folder: ", app_data_dir, call. = FALSE)
}
if (!dir.exists(export_data_dir)) {
  dir.create(export_data_dir, recursive = TRUE)
}

family_labels <- c(
  ScoresTrees = "Scores Trees",
  BillyTrees = "Billy Trees",
  ScoresLateReg = "Scores Late Regression",
  Billy = "Billy"
)

parse_model_file <- function(path) {
  nm <- basename(path)
  m <- str_match(nm, "^(\\d{4})_(.+)_(home|away|home_score|away_score)\\.csv$")
  if (any(is.na(m))) return(NULL)
  tibble(
    path = path,
    file = nm,
    season = as.integer(m[, 2]),
    family = m[, 3],
    family_label = family_labels[m[, 3]] %||% m[, 3],
    split = m[, 4]
  )
}

projection_candidates_for_cover <- function(cover_col) {
  base <- str_remove(cover_col, "^Cover_")
  c(
    base,
    str_replace(base, "^Total_", "ImpliedTotal_"),
    str_replace(base, "^Team_ImpliedTotal_", "ImpliedTeamScored_"),
    str_replace(base, "^Opp_ImpliedTotal_", "ImpliedOppScored_"),
    str_replace(base, "^Score$", "Score_final"),
    str_replace(base, "^ScoreDiff$", "ScoreDiff_final")
  ) %>% unique()
}

prediction_cols_from_names <- function(cols) {
  cols[str_detect(cols, stringr::regex("^(ScoreDiff|ScoreTotal|TotalScore|Implied|Score_(xgb|forward|stepwise|avg|final)|OppScore|Billy$)", TRUE)) &
         !str_detect(cols, "^Cover_|_target|_cover$")]
}

build_prediction_column_inventory <- function(inventory) {
  purrr::map_dfr(seq_len(nrow(inventory)), function(i) {
    meta <- inventory[i, ]
    df <- suppressMessages(readr::read_csv(meta$path, show_col_types = FALSE, progress = FALSE))
    pred_cols <- prediction_cols_from_names(names(df))

    if (length(pred_cols) == 0) {
      return(tibble(
        File = meta$file,
        Season = meta$season,
        Family = meta$family_label,
        Split = meta$split,
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
      Family = meta$family_label,
      Split = meta$split,
      PredictionColumn = pred_cols,
      ColumnPosition = match(pred_cols, names(df)),
      NonMissingRows = vapply(df[pred_cols], function(x) sum(!is.na(x)), integer(1)),
      MissingRows = vapply(df[pred_cols], function(x) sum(is.na(x)), integer(1)),
      AllMissing = vapply(df[pred_cols], function(x) all(is.na(x)), logical(1))
    )
  }) %>%
    arrange(Family, Season, File, ColumnPosition)
}

key_cols_from_names <- function(cols) {
  intersect(
    c("game_id", "season", "week", "game_date", "posteam_type", "posteam", "defteam", "home_team", "away_team",
      "home_score", "away_score", "spread_line", "total_line"),
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

parsed_files <- list.files(app_data_dir, pattern = "\\.csv$", full.names = TRUE) %>%
  purrr::map(parse_model_file)
parsed_files <- parsed_files[!vapply(parsed_files, is.null, logical(1))]

if (length(parsed_files) == 0) {
  stop(
    "No model CSVs matched the expected naming pattern in ", app_data_dir,
    ". Expected names like 2026_ScoresTrees_home.csv or 2026_Billy_away_score.csv.",
    call. = FALSE
  )
}

inventory <- purrr::list_rbind(parsed_files) %>%
  filter(family %in% names(family_labels)) %>%
  arrange(family, season, split)

if (nrow(inventory) == 0) {
  stop(
    "Model CSVs were found in ", app_data_dir,
    ", but none belonged to the supported families: ",
    paste(names(family_labels), collapse = ", "),
    call. = FALSE
  )
}

prediction_column_inventory <- build_prediction_column_inventory(inventory)
readr::write_csv(prediction_column_inventory, file.path(export_dir, "prediction_column_inventory.csv"), na = "")
readr::write_csv(prediction_column_inventory, file.path(export_data_dir, "prediction_column_inventory.csv"), na = "")

compact_models <- purrr::map(inventory$path, function(path) {
  header <- names(suppressMessages(readr::read_csv(path, n_max = 0, show_col_types = FALSE, progress = FALSE)))
  cols <- cols_needed_for_names(header)
  df <- suppressMessages(readr::read_csv(path, col_select = dplyr::all_of(cols), show_col_types = FALSE, progress = FALSE))
  pred_cols <- prediction_cols_from_names(names(df))
  empty_pred_cols <- pred_cols[vapply(df[pred_cols], function(x) all(is.na(x)), logical(1))]
  if (length(empty_pred_cols) > 0) {
    df <- df %>% dplyr::select(-all_of(empty_pred_cols))
  }
  df
})

names(compact_models) <- inventory$file

archive_path <- file.path(export_data_dir, "legacy_pregame_2026_archive.rds")
if (file.exists(archive_path)) {
  pregame_archive <- readRDS(archive_path)
  pregame_archive <- lapply(pregame_archive, function(df) {
    df[, unique(c(pregame_key_columns, pregame_projection_columns(df, prediction_cols_from_names))), drop = FALSE]
  })
} else {
  pregame_archive <- list()
  model_files <- inventory$file[grepl("^2026_", inventory$file)]
  for (week in 1:3) {
    snapshot_dir <- file.path(dirname(pipeline_dir), "Legacy_models", paste0("nflFast_Model_2026_Scores_week", week), "output")
    snapshot_models <- lapply(model_files, function(file) {
      path <- file.path(snapshot_dir, file)
      if (!file.exists(path)) stop("Missing original Legacy pregame file: ", path, call. = FALSE)
      header <- names(suppressMessages(readr::read_csv(path, n_max = 0, show_col_types = FALSE)))
      cols <- cols_needed_for_names(header)
      suppressMessages(readr::read_csv(path, col_select = dplyr::all_of(cols), show_col_types = FALSE, progress = FALSE))
    })
    names(snapshot_models) <- model_files
    pregame_archive <- pregame_capture(
      pregame_archive, snapshot_models, week,
      prediction_cols_from_names, paste0("Legacy Week ", week)
    )
  }
}
pregame_archive <- pregame_update_future(pregame_archive, compact_models, prediction_cols_from_names)
compact_models <- pregame_restore_completed(compact_models, pregame_archive, prediction_cols_from_names)
saveRDS(pregame_archive, archive_path)

saveRDS(inventory %>% mutate(path = file.path("data", file)), file.path(export_data_dir, "model_inventory.rds"))
saveRDS(compact_models, file.path(export_data_dir, "compact_models.rds"))

cat("Prepared", length(compact_models), "compact model files.\n")
cat("Wrote prediction column inventory with", nrow(prediction_column_inventory), "rows.\n")
cat("Read source CSVs from:", app_data_dir, "\n")
cat("Wrote app-ready files to:", export_data_dir, "\n")
