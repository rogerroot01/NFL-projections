app <- source("app.R", local = TRUE)$value
week3_poolhost_lines <- poolhost_read_bundled_lines(2026L, 3L)
stopifnot(grepl("PoolHost Picks", htmltools::renderTags(ui)$html, fixed = TRUE))
stopifnot(grepl("Monday night tiebreaker", htmltools::renderTags(ui)$html, fixed = TRUE))

stopifnot(nrow(week3_poolhost_lines) == 15L,
          nrow(subset(poolhost_schedule, season == 2026L & week == 3L)) == 15L,
          !"2026_3_ATL_GB" %in% week3_poolhost_lines$game_id,
          !any(c("2026_15_CHI_BUF", "2026_15_SEA_PHI") %in% poolhost_schedule$game_id))

compact <- paste(sprintf("%s at %s | %s",
                         week3_poolhost_lines$away_team,
                         week3_poolhost_lines$home_team,
                         week3_poolhost_lines$poolhost_home_line), collapse = "\n")
parsed <- poolhost_parse_pasted_lines(compact, 2026L, 3L)
stopifnot(identical(parsed$game_id, week3_poolhost_lines$game_id),
          identical(parsed$poolhost_home_line, week3_poolhost_lines$poolhost_home_line))

display_name <- function(team) {
  names(poolhost_team_aliases)[match(team, poolhost_team_aliases)]
}
raw_page <- paste(c("Week 3 Picks", "Pick 15 games this week.",
                    unlist(Map(function(away, home, line) {
                      c("Sun. Sep. 27 - 01:00 PM", display_name(away),
                        display_name(home), as.character(line))
                    }, week3_poolhost_lines$away_team,
                    week3_poolhost_lines$home_team,
                    week3_poolhost_lines$poolhost_home_line))), collapse = "\n")
parsed_page <- poolhost_parse_pasted_lines(raw_page, 2026L, 3L)
stopifnot(identical(parsed_page$game_id, week3_poolhost_lines$game_id))
inline_page <- paste(c("Week 3 Picks", "Pick 15 games this week.",
                       sprintf("Sun. Sep. 27  %s  %s  %s",
                               vapply(week3_poolhost_lines$away_team, display_name, character(1)),
                               vapply(week3_poolhost_lines$home_team, display_name, character(1)),
                               week3_poolhost_lines$poolhost_home_line)), collapse = "\n")
parsed_inline <- poolhost_parse_pasted_lines(inline_page, 2026L, 3L)
stopifnot(identical(parsed_inline$game_id, week3_poolhost_lines$game_id))

incomplete <- try(poolhost_parse_pasted_lines(sub("\\n[^\\n]+$", "", compact),
                                               2026L, 3L), silent = TRUE)
wrong_week <- try(poolhost_parse_pasted_lines(raw_page, 2026L, 4L), silent = TRUE)
stopifnot(inherits(incomplete, "try-error"), inherits(wrong_week, "try-error"))

shiny::testServer(app$serverFuncSource(), {
  session$setInputs(poolhost_season = 2026L, poolhost_week = 3L,
                    poolhost_source = "combined")
  session$setInputs(poolhost_build = 1L)
  picks <- poolhost_results_state()
  if (nrow(picks) != 15L) print(poolhost_status_state())
  stopifnot(nrow(picks) == 15L,
            identical(picks$game_id, week3_poolhost_lines$game_id),
            all(is.finite(picks$avg_projection)),
            all(is.finite(picks$current_market_line)),
            all(is.finite(picks$line_difference)),
            all(picks$edge >= 0),
            all(picks$pick_team %in% c(picks$away_team, picks$home_team)),
            !"2026_3_ATL_GB" %in% picks$game_id)
  tiebreakers <- poolhost_tiebreaker_state()
  stopifnot(nrow(tiebreakers) == 1L,
            identical(tiebreakers$game_id, "2026_3_PHI_CHI"),
            is.finite(tiebreakers$projected_total),
            identical(tiebreakers$tiebreaker_entry,
                      as.integer(round(tiebreakers$projected_total))))
  for (source in c("legacy", "next_gen")) {
    session$setInputs(poolhost_source = source)
    session$setInputs(poolhost_build = if (source == "legacy") 2L else 3L)
    stopifnot(nrow(poolhost_results_state()) == 15L)
    stopifnot(nrow(poolhost_tiebreaker_state()) == 1L)
  }
})

cat("POOLHOST_PICKS_OK\n")
