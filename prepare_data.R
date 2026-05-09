library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

app_data_dir <- file.path(getwd(), "data")

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
  cols[str_detect(cols, regex("ScoreDiff|ScoreTotal|TotalScore|Implied|Score_(xgb|forward|stepwise|avg|final)|OppScore|Billy", TRUE)) &
         !str_detect(cols, "^Cover_|_target|_cover$")]
}

key_cols_from_names <- function(cols) {
  intersect(
    c("game_id", "season", "week", "game_date", "posteam", "defteam", "home_team", "away_team",
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

inventory <- list.files(app_data_dir, pattern = "\\.csv$", full.names = TRUE) %>%
  purrr::map_dfr(parse_model_file) %>%
  filter(family %in% names(family_labels)) %>%
  arrange(family, season, split)

compact_models <- purrr::map(inventory$path, function(path) {
  header <- names(suppressMessages(readr::read_csv(path, n_max = 0, show_col_types = FALSE, progress = FALSE)))
  cols <- cols_needed_for_names(header)
  suppressMessages(readr::read_csv(path, col_select = dplyr::all_of(cols), show_col_types = FALSE, progress = FALSE))
})

names(compact_models) <- inventory$file

saveRDS(inventory %>% mutate(path = file.path("data", file)), file.path(app_data_dir, "model_inventory.rds"))
saveRDS(compact_models, file.path(app_data_dir, "compact_models.rds"))

cat("Prepared", length(compact_models), "compact model files.\n")
