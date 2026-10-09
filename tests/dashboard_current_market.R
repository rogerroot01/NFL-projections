app <- source('app.R',local=TRUE)$value
snapshot <- market_parse_pasted_lines('PHI at JAX | -7.5 | 41.5\nCIN at MIA | +7 | 43.5',2026L,5L)
stopifnot(identical(snapshot$market_total_line,c(41.5,43.5)))
fixture <- paste(readLines('tests/fixtures/current_market_week05_sportsbook.txt',encoding='UTF-8',warn=FALSE),collapse='\n')
copied <- market_parse_pasted_lines(fixture,2026L,5L)
stopifnot(all(is.finite(copied$market_total_line)))
shiny::testServer(app$serverFuncSource(), {
  market_lines_state(snapshot)
  rows <- tibble::tibble(
    game_id=rep(c('2026_5_PHI_JAX','2026_5_CIN_MIA'),3),season=2026L,week=5L,
    away_team=rep(c('PHI','CIN'),3),home_team=rep(c('JAX','MIA'),3),
    market=c('spread','spread','total','total','home_implied','away_implied'),
    consensus_pick=c('Home','Away','Over','Under','Over','Under'),
    market_line=c(7,-6.5,42.5,44.5,24.75,25.5),avg_projection=c(10,-9,46,40,27,20),
    avg_edge=c(3,-2.5,3.5,-4.5,2.25,-5.5),agree_pct=1,models_used=12L,
    actual_result=NA_real_,actual_side=NA_real_,correct=NA,spread_line=rep(c(7,-6.5),3),
    total_line=rep(c(42.5,44.5),3),projections=12L
  )
  comparison <- market_compare_consensus(rows)
  stopifnot(identical(comparison$contest_pick_line[1:2],c(-7,-6.5)),
            identical(comparison$current_market_pick_line,c(-7.5,-7,41.5,43.5,24.5,25.25)),
            identical(comparison$contest_market_difference,c(.5,.5,1,1,.25,.25)),
            identical(comparison$current_market_edge,c(2.5,2,4.5,3.5,2.5,5.25)))
  consensus_rows(rows);nextgen_consensus_rows(rows);overall_consensus_rows(rows)
  for(source in c('legacy','next_gen','combined')) {
    session$setInputs(dashboard_source=source,dashboard_season='2026',dashboard_week='5',
                      dashboard_markets=c('spread','total','home_implied','away_implied'))
    spread <- dashboard_table_for_market('spread')
    stopifnot(nrow(spread)==2L,all(spread$`Line − current`=='+0.5'),
              setequal(spread$`Current market line`,c('JAX -7.5','CIN -7')))
    exported <- dashboard_export_rows()
    stopifnot(all(c('current_market_pick_line','contest_market_difference') %in% names(exported)),
              nrow(exported)==6L)
  }
  # The identical selected-team sign convention applies to every contest.
  card <- rows[1:2,] %>% dplyr::mutate(pick_side=consensus_pick,circa_pick=consensus_pick,
    pick_team=c('JAX','CIN'),pick_line=c(-7,-6.5),poolhost_pick_line=pick_line,
    projected_line=c(-10,-9),projected_pick_line=projected_line,rank=1:2,circa_edge=c(3,2.5),edge=c(3,2.5))
  circa <- format_circa_table(card)
  dk <- format_dkp_table(card)
  pool <- market_compare_rows(card,'pick_side','poolhost_pick_line','projected_pick_line')
  stopifnot(all(circa$`Circa − current`=='+0.5'),all(dk$`DK − current`=='+0.5'),
            all(pool$contest_market_difference==.5))
  # A different week and a partial snapshot must never borrow stale odds.
  missing <- rows[1:2,];missing$game_id[1]<-'2026_6_HOU_JAX'
  missing_comparison <- market_compare_consensus(missing)
  stopifnot(is.na(missing_comparison$contest_market_difference[1]),
            missing_comparison$contest_market_difference[2]==.5)
  market_lines_state(tibble::tibble())
  stopifnot(all(is.na(market_compare_consensus(rows)$contest_market_difference)))
})
cat('DASHBOARD_CURRENT_MARKET_COMPARISONS_OK\n')
