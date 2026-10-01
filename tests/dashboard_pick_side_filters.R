app <- source('app.R', local = TRUE)$value
shiny::testServer(app$serverFuncSource(), {
  session$setInputs(dashboard_no_minus_3_5_favorites = TRUE,
    dashboard_no_plus_2_5_underdogs = TRUE, dashboard_no_early_week_games = FALSE)
  cases <- tibble::tibble(
    game_id = c('jax_favorite_home','jax_favorite_away','dog_home','dog_away','fav_home','fav_away','against_fav_home','against_fav_away','total','no_pick'),
    market = c(rep('spread',8),'total','spread'),
    consensus_pick = c('Home','Away','Home','Away','Home','Away','Home','Away','Over',NA),
    market_line = c(2.5,-2.5,-2.5,2.5,3.5,-3.5,-3.5,3.5,44.5,2.5))
  kept <- apply_dashboard_spread_overrides(cases)
  print(kept)
  expected <- c('jax_favorite_home','jax_favorite_away','against_fav_home','against_fav_away','total','no_pick')
  stopifnot(setequal(kept$game_id, expected))
  session$setInputs(dashboard_no_minus_3_5_favorites = FALSE,
    dashboard_no_plus_2_5_underdogs = FALSE)
  stopifnot(nrow(apply_dashboard_spread_overrides(cases)) == nrow(cases))
  session$setInputs(dashboard_no_minus_3_5_favorites = TRUE,
    dashboard_no_plus_2_5_underdogs = TRUE)
  cases$season <- 2026L
  cases$week <- 4L
  overall_consensus_rows(cases)
  session$setInputs(dashboard_source='combined', dashboard_season='2026',
    dashboard_week='4', dashboard_markets=c('spread','total'))
  stopifnot(setequal(dashboard_filtered_rows()$game_id, expected))
})
cat('DASHBOARD_PICK_SIDE_FILTERS_OK\n')
