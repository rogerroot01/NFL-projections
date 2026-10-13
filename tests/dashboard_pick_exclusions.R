app_env <- new.env(parent=globalenv())
app <- source('app.R',local=app_env)$value
ui <- htmltools::renderTags(app_env$ui)$html
for(id in c('overall_cons_no_minus_3_5_favorites','overall_cons_no_minus_7_5_favorites',
             'overall_cons_no_plus_2_5_underdogs','overall_cons_no_early_week_games'))
  stopifnot(grepl(paste0('id="',id,'"'),ui,fixed=TRUE))

rows <- tibble::tibble(game_id=c(paste0('case_',1:10),'2026_06_SEA_DEN','2026_6_WAS_SF'),
  season=2026L,week=6L,home_team='H',away_team='A',
  market=c(rep('spread',9),'total','total','total'),
  market_line=c(3.5,-3.5,7.5,-7.5,2.5,-2.5,7,-7,8.5,40.5,40.5,50.5),
  consensus_pick=c('Home','Away','Home','Away','Away','Home','Home','Away','Home','Over','Over','Under'),
  avg_projection=20,avg_edge=10,agree_pct=1,models_used=12L,projections=12L,
  spread_line=7.5,total_line=40.5,actual_result=NA_real_,actual_side=NA_character_,correct=NA)
stopifnot(identical(app_env$filter_dashboard_picks(rows,'2026_6_SEA_DEN')$game_id,
  rows$game_id[c(7:10,12)]))
shiny::testServer(app$serverFuncSource(), {
  consensus_rows(rows);nextgen_consensus_rows(rows);overall_consensus_rows(rows)
  for(source in c('legacy','next_gen','combined')) {
    session$setInputs(dashboard_source=source,dashboard_season='2026',dashboard_week='6',
      dashboard_markets=c('spread','total'),overall_cons_no_minus_3_5_favorites=TRUE,
      overall_cons_no_minus_7_5_favorites=TRUE,overall_cons_no_plus_2_5_underdogs=TRUE,
      overall_cons_no_early_week_games=TRUE)
    stopifnot(identical(dashboard_filtered_rows()$game_id,rows$game_id[c(7:10,12)]),
      setequal(dashboard_export_rows()$game_id,rows$game_id[c(7:10,12)]),
      setequal(dashboard_export_rows_for_market('spread')$game_id,rows$game_id[7:9]),
      setequal(dashboard_export_rows_for_market('total')$game_id,rows$game_id[c(10,12)]),
      nrow(dashboard_table_for_market('spread'))==3L)
    session$setInputs(dashboard_markets='total')
    stopifnot(nrow(dashboard_export_rows_for_market('spread'))==3L)
    session$setInputs(dashboard_markets=c('spread','total'),overall_cons_no_minus_7_5_favorites=FALSE)
    stopifnot(all(rows$game_id[3:4] %in% dashboard_filtered_rows()$game_id),
      !any(rows$game_id[c(1,2,5,6)] %in% dashboard_filtered_rows()$game_id))
    session$setInputs(overall_cons_no_minus_3_5_favorites=FALSE,overall_cons_no_plus_2_5_underdogs=FALSE,
      overall_cons_no_early_week_games=FALSE)
    stopifnot(identical(dashboard_filtered_rows(),rows))
  }
})
cat('DASHBOARD_PICK_EXCLUSIONS_OK: selected-team signs, all sources, tables and both download paths.\n')
