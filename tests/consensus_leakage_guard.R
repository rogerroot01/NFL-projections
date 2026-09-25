app_env <- new.env(parent = globalenv())
source("app.R", local = app_env)

current_game <- app_env$current_lines %>%
  dplyr::filter(game_id == "2026_3_ARI_SF")
stopifnot(nrow(current_game) == 1L)
refreshed <- app_env$apply_current_lines(tibble::tibble(
  game_id = current_game$game_id,
  spread_line = 1,
  total_line = 1,
  home_implied = 99,
  away_implied = 99
))
stopifnot(
  isTRUE(all.equal(refreshed$home_implied, (refreshed$total_line + refreshed$spread_line) / 2)),
  isTRUE(all.equal(refreshed$away_implied, (refreshed$total_line - refreshed$spread_line) / 2))
)

shiny::testServer(app_env$server, {
  session$setInputs(
    ng_cons_frameworks = c("early", "late"),
    ng_cons_families = names(app_env$next_gen_family_labels),
    ng_cons_projection_sources = c("direct", "implied_team_scores"),
    ng_cons_seasons = integer(),
    ng_cons_future_seasons = 2026L,
    ng_cons_line_source = "closing",
    ng_cons_injury_source = "apply",
    ng_cons_apply_amortization = TRUE,
    ng_cons_agree = 60,
    ng_cons_min_win = 40
  )
  rows <- build_nextgen_consensus_rows()
  stopifnot(nrow(rows) > 0L,
            all(rows$season == 2026L),
            any(rows$week == 3L),
            all(is.finite(rows$avg_projection)),
            all(is.finite(rows$agree_pct)))
  cat("NEXTGEN_CONSENSUS_2026_ROWS_OK:", nrow(rows), "rows\n")

  session$setInputs(
    cons_families = c("ScoresTrees", "ScoresLateReg", "Billy"),
    cons_seasons = integer(),
    cons_future_seasons = 2026L,
    cons_line_source = "closing",
    cons_injury_source = "apply",
    cons_apply_amortization = TRUE,
    cons_agree = 60,
    cons_min_win = 40
  )
  legacy_rows <- build_consensus_rows()
  stopifnot(nrow(legacy_rows) > 0L,
            all(legacy_rows$season == 2026L),
            any(legacy_rows$week == 3L),
            all(is.finite(legacy_rows$avg_projection)),
            all(is.finite(legacy_rows$agree_pct)))
  cat("LEGACY_CONSENSUS_2026_ROWS_OK:", nrow(legacy_rows), "rows\n")

  # The UI selects graded seasons as well as the future season by default.
  # Early historical files can have no prior scored season for a model; the
  # consensus must skip that model rather than throw from a missing lookup.
  session$setInputs(
    cons_seasons = app_env$backtest_seasons,
    cons_future_seasons = app_env$future_seasons
  )
  default_rows <- build_consensus_rows()
  stopifnot(nrow(default_rows) > 0L,
            any(default_rows$season == 2026L),
            all(is.finite(default_rows$avg_projection)))
  cat("LEGACY_CONSENSUS_DEFAULT_SELECTION_OK:", nrow(default_rows), "rows\n")

  session$setInputs(
    cons_agree = 50,
    cons_seasons = integer(),
    cons_future_seasons = 2026L,
    ng_cons_agree = 50,
    overall_cons_sources = c("legacy", "next_gen"),
    overall_cons_require_all_sources = TRUE,
    overall_cons_agree = 50,
    overall_cons_min_edge = 0
  )
  legacy_rows <- build_consensus_rows()
  nextgen_rows <- build_nextgen_consensus_rows()
  implied_nextgen <- nextgen_rows %>%
    dplyr::filter(market %in% c("home_implied", "away_implied"), week %in% 3:6)
  stopifnot(nrow(implied_nextgen) > 0L, all(market_line_is_consistent(implied_nextgen)))
  consensus_rows(legacy_rows)
  nextgen_consensus_rows(nextgen_rows)
  combined_rows <- build_overall_consensus_rows()
  expected_games <- c(`3` = 16L, `4` = 16L, `5` = 15L, `6` = 14L)
  for (week_key in names(expected_games)) {
    for (market_key in app_env$dashboard_market_keys) {
      observed <- combined_rows %>%
        dplyr::filter(week == as.integer(week_key), market == market_key) %>%
        nrow()
      stopifnot(observed == expected_games[[week_key]])
    }
  }
  cat("COMBINED_CONSENSUS_COMPLETE_IMPLIED_MARKETS_OK\n")
})
