app_env <- new.env(parent = globalenv())
source("app.R", local = app_env)

# Cover_* records the selected side, not whether that side won.
sample_rows <- tibble::tibble(
  Cover_ScoreDiff_final = c(0, 1),
  ScoreDiff_final = c(5, -5),
  spread_line = c(3, -3),
  total_line = c(45, 45),
  home_score = c(28, 24),
  away_score = c(21, 17)
)
graded <- app_env$detect_cover_summary(sample_rows, "home")
stopifnot(nrow(graded) == 1L, graded$picks == 2L,
          graded$wins == 1L, graded$losses == 1L)

# Completed games keep the lines stored with their model rows.
current <- app_env$current_lines[
  match(app_env$legacy_pregame_lines$game_id[[1]], app_env$current_lines$game_id),
  , drop = FALSE
]
stopifnot(nrow(current) == 1L, !is.na(current$game_id))
completed <- tibble::tibble(
  game_id = current$game_id,
  home_score = 24,
  away_score = 17,
  spread_line = 999,
  total_line = 999
)
held <- app_env$apply_current_lines(completed, preserve_completed = TRUE)
stopifnot(held$spread_line == 999, held$total_line == 999)
archived <- app_env$apply_legacy_pregame_lines(held)
archived_at <- match(current$game_id, app_env$legacy_pregame_lines$game_id)
stopifnot(
  archived$spread_line == app_env$legacy_pregame_lines$archived_spread_line[archived_at],
  archived$total_line == app_env$legacy_pregame_lines$archived_total_line[archived_at]
)
cat("LEGACY_COVER_GRADING_AND_COMPLETED_LINES_OK\n")
