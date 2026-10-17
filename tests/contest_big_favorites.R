env<-new.env(parent=globalenv())
app<-source('app.R',local=env)$value
html<-htmltools::renderTags(env$ui)$html
for(prefix in c('overall_cons','circa','dkp'))stopifnot(grepl(paste0('id="',prefix,'_no_big_favorites"'),html,fixed=TRUE))
# Both home and away favorites are blocked at the inclusive boundary; large
# underdogs, -10 favorites, and non-ATS markets remain eligible.
dashboard<-tibble::tibble(game_id=paste0('case_',1:8),market=c(rep('spread',6),'total','home_implied'),
 market_line=c(10,10.5,12.5,-10.5,-10.5,10.5,40.5,11),
 consensus_pick=c('Home','Home','Home','Away','Home','Away','Under','Under'))
stopifnot(identical(env$filter_dashboard_picks(dashboard,character())$game_id,dashboard$game_id[c(1,5:8)]),
 identical(env$filter_dashboard_picks(dashboard,character(),no_big_favorites=FALSE),dashboard))
shiny::testServer(app$serverFuncSource(), {
 # Include a large away favorite and both signs of the threshold so the filter
 # follows the selected team's contest spread, independently of home/away.
 lines<-c(-10,-10.5,-12.5,-10.5,10.5,10.5,-1,-2,-4,-5,-6)
 margin<-c(20,20,20,-20,-20,20,9,9,9,9,9)
 rows<-tibble::tibble(game_id=paste0('2026_6_case_',seq_along(lines)),season=2026L,week=6L,
  away_team=paste0('A',seq_along(lines)),home_team=paste0('H',seq_along(lines)),
  avg_projection=margin,current_market_line=0,circa_market_line=-lines,
  circa_home_line=lines,circa_away_line=-lines,agree_pct=1,models_used=33L)
 circa_results_state(rows);dkp_results_state(rows)
 market_lines_state(tibble::tibble(game_id=rows$game_id,season=2026L,week=6L,market_home_line=lines))
 session$setInputs(circa_no_big_favorites=TRUE,dkp_no_big_favorites=TRUE,
  circa_no_minus_3_5_favorites=FALSE,circa_no_minus_7_5_favorites=FALSE,
  circa_no_plus_2_5_underdogs=FALSE,circa_no_early_week_games=FALSE,circa_min_edge=0,
  dkp_no_minus_3_5_favorites=FALSE,dkp_no_minus_7_5_favorites=FALSE,
  dkp_no_plus_2_5_underdogs=FALSE,dkp_no_early_week_games=FALSE,dkp_min_edge=0)
 kept<-rows$game_id[c(1,4,6:11)]
 stopifnot(setequal(circa_ranked_rows()$game_id,kept),setequal(dkp_ranked_rows()$game_id,kept),
  nrow(circa_final_choices())==8L,nrow(dkp_final_choices())==8L,
  all(circa_ranked_rows()$pick_line> -10.5),all(dkp_ranked_rows()$pick_line> -10.5))
 # A fifth pick can come from the eighth displayed candidate and reaches the
 # same final rows used by the CSV download.
 cids<-circa_final_choices()$game_id[c(1:4,8)];dids<-dkp_final_choices()$game_id[c(1:4,8)]
 session$setInputs(circa_final_picks=cids,dkp_final_picks=dids)
 stopifnot(setequal(circa_final_rows()$game_id,cids),setequal(dkp_final_rows()$game_id,dids),
  output$circa_final_status=='Five plays selected.',output$dkp_final_status=='Five plays selected.')
 session$setInputs(circa_no_big_favorites=FALSE,dkp_no_big_favorites=FALSE)
 stopifnot(nrow(circa_ranked_rows())==11L,nrow(dkp_ranked_rows())==11L)
})
cat('CONTEST_BIG_FAVORITES_AND_EIGHT_CHOICES_OK\n')
