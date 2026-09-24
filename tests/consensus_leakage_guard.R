app_env <- new.env(parent = globalenv())
source("app.R", local = app_env)

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
})
