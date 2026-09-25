app_env <- new.env(parent = globalenv())
source("app.R", local = app_env)

stopifnot(
  identical(app_env$canonical_game_id("2026_01_ARI_LAC"), "2026_1_ARI_LAC"),
  identical(app_env$canonical_game_id("2026_10_ARI_LAC"), "2026_10_ARI_LAC")
)

historical <- tibble::tibble(
  game_id = "2026_01_ARI_LAC",
  home_score = 14,
  away_score = 26,
  spread_line = 123,
  total_line = 123,
  home_implied = 123,
  away_implied = 123
)
preserved <- app_env$apply_current_lines(historical, preserve_completed = TRUE)
updated <- app_env$apply_current_lines(historical)
stopifnot(
  identical(preserved$game_id, "2026_1_ARI_LAC"),
  identical(preserved$spread_line, 123),
  identical(preserved$total_line, 123),
  updated$spread_line != 123,
  updated$total_line != 123
)

future <- historical
future$home_score <- NA_real_
future$away_score <- NA_real_
refreshed <- app_env$apply_current_lines(future, preserve_completed = TRUE)
stopifnot(refreshed$spread_line != 123, refreshed$total_line != 123)

cat("GAME_ID_NORMALIZATION_AND_HISTORICAL_LINES_OK\n")
