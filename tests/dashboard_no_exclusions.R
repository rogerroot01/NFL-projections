app_env <- new.env(parent = globalenv())
source('app.R', local = app_env)
ui <- htmltools::renderTags(app_env$ui)$html
stopifnot(!grepl('dashboard_no_minus_3_5_favorites|dashboard_no_plus_2_5_underdogs|dashboard_no_early_week_games', ui))
shiny::testServer(app_env$server, {
  rows <- tibble::tibble(game_id=c('2026_4_JAX_CIN','favorite','early_week'),
    season=2026L, week=4L, market='spread', consensus_pick=c('Away','Home','Home'),
    market_line=c(2.5,3.5,0))
  overall_consensus_rows(rows)
  session$setInputs(dashboard_source='combined', dashboard_season='2026',
    dashboard_week='4', dashboard_markets='spread',
    dashboard_no_minus_3_5_favorites=TRUE, dashboard_no_plus_2_5_underdogs=TRUE,
    dashboard_no_early_week_games=TRUE)
  stopifnot(identical(dashboard_filtered_rows(), rows))
})
cat('DASHBOARD_ALL_THREE_EXCLUSIONS_REMOVED_OK\n')
