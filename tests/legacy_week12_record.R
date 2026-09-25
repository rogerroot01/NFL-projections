app_env <- new.env(parent = globalenv())
source("app.R", local = app_env)

shiny::testServer(app_env$server, {
  session$setInputs(
    cons_families = c("ScoresTrees", "ScoresLateReg", "Billy", "BillyTrees"),
    cons_seasons = 2026L,
    cons_future_seasons = integer(),
    cons_line_source = "closing",
    cons_injury_source = "apply",
    cons_apply_amortization = TRUE,
    cons_agree = 60,
    cons_min_win = 40
  )
  rows <- build_consensus_rows()
  graded <- rows |>
    dplyr::filter(week %in% 1:2, !is.na(correct)) |>
    dplyr::group_by(market) |>
    dplyr::summarise(games = dplyr::n(), wins = sum(correct),
                     losses = sum(!correct), .groups = "drop")
  print(graded)
  stopifnot(nrow(graded) == 5L,
            all(graded$games <= 32L),
            all(graded$games == graded$wins + graded$losses))
})
