e <- new.env(parent=globalenv())
source('app.R',local=e)

# Completion comes from the game, not whether a Billy export has score fields.
raw_billy <- e$compact_models[['2026_Billy_home.csv']]
past <- raw_billy$season==2026 & raw_billy$week<=4
stopifnot(all(is.na(raw_billy$home_score[past])),sum(past)==64L)
read_billy <- e$read_model_file(e$inventory$path[e$inventory$file=='2026_Billy_home.csv'])
stopifnot(all(is.finite(read_billy$home_score[past])),all(is.finite(read_billy$away_score[past])))
pred_cols <- e$prediction_cols(raw_billy)
stopifnot(isTRUE(all.equal(raw_billy[pred_cols],read_billy[pred_cols])))

# A contradictory scored export must be rejected instead of silently choosing it.
toy <- tibble::tibble(game_id='2026_01_NE_SEA',home_score=13,away_score=10,spread_line=3,total_line=44.5)
bad <- toy; bad$home_score <- 99
stopifnot(inherits(try(e$build_pipeline_game_results(list(toy,bad)),silent=TRUE),'try-error'))
stopifnot(e$build_pipeline_game_results(list(toy))$game_id=='2026_1_NE_SEA')

# Tuesday 2026 values are selected explicitly; missing early lines are unavailable.
base <- tibble::tibble(game_id=c('2026_1_NE_SEA','2099_1_NE_SEA'),spread_line=99,total_line=99,home_implied=99,away_implied=99,line=99)
early <- e$apply_backtest_lines(base,'early')
stopifnot(early$spread_line[1]==3,early$total_line[1]==44.5,
          early$home_implied[1]==23.75,early$away_implied[1]==20.75,
          is.na(early$spread_line[2]),is.na(early$total_line[2]),is.na(early$line[2]),
          identical(e$apply_backtest_lines(base,'closing'),base))
for (year in c('2024','2025')) {
  stored <- readr::read_csv('data/early_lines.csv',show_col_types=FALSE) |>
    dplyr::filter(grepl(paste0('^',year,'_'),game_id))
  idx <- match(e$canonical_game_id(stored$game_id),e$early_lines$game_id)
  stopifnot(all(e$early_lines$early_spread_line[idx]==stored[['Mid-week Spread']],na.rm=TRUE),
            all(e$early_lines$early_total_line[idx]==stored[['Mid-week Total']],na.rm=TRUE))
}

e$dashboard_market_keys <- 'spread'
shiny::testServer(e$server,{
  session$setInputs(cons_families=c('ScoresTrees','ScoresLateReg','Billy','BillyTrees'),
    cons_seasons=2026L,cons_future_seasons=integer(),cons_line_source='closing',
    cons_injury_source='apply',cons_apply_amortization=TRUE,cons_agree=50,cons_min_win=40,
    ng_cons_frameworks=c('early','late'),ng_cons_families=names(e$next_gen_family_labels),
    ng_cons_projection_sources=c('direct','implied_team_scores'),
    ng_cons_seasons=integer(),ng_cons_future_seasons=2026L,ng_cons_line_source='closing',
    ng_cons_injury_source='apply',ng_cons_apply_amortization=TRUE,ng_cons_agree=50,ng_cons_min_win=40)
  close_l <- build_consensus_rows()
  close_n <- build_nextgen_consensus_rows()
  session$setInputs(cons_line_source='early',ng_cons_line_source='early')
  early_l <- build_consensus_rows()
  early_n <- build_nextgen_consensus_rows()
  check <- function(rows,expected,label) {
    past_rows <- rows |> dplyr::filter(week<=4)
    idx <- match(past_rows$game_id,expected$game_id)
    stopifnot(!anyNA(idx),all(past_rows$market_line==expected$spread_line[idx]))
    side <- sign(past_rows$actual_result-past_rows$market_line)
    stopifnot(all(is.na(past_rows$actual_side)==(side==0)),
              all(past_rows$actual_side[side!=0]==side[side!=0]))
    pick <- ifelse(past_rows$consensus_pick=='Home',1,-1)
    valid <- !is.na(past_rows$correct)
    stopifnot(all(past_rows$correct[valid]==(pick[valid]==side[valid])))
    cat(label,':',sum(past_rows$correct %in% TRUE),'wins',sum(past_rows$correct %in% FALSE),'losses',sum(is.na(past_rows$correct)),'push/ungraded\n')
    ne <- past_rows[past_rows$game_id=='2026_1_NE_SEA',]
    stopifnot(nrow(ne)==1L)
    ne
  }
  expected_l <- e$pipeline_game_results
  idx <- match(expected_l$game_id,e$legacy_pregame_lines$game_id)
  held <- !is.na(idx)
  expected_l$spread_line[held] <- e$legacy_pregame_lines$archived_spread_line[idx[held]]
  expected_early <- e$current_lines |> dplyr::transmute(game_id,spread_line=current_spread_line)
  ne_cl <- check(close_l,expected_l,'LEGACY_CLOSING')
  ne_cn <- check(close_n,e$pipeline_game_results,'NEXTGEN_CLOSING')
  ne_el <- check(early_l,expected_early,'LEGACY_EARLY')
  ne_en <- check(early_n,expected_early,'NEXTGEN_EARLY')
  stopifnot(ne_cl$market_line==3.5,ne_cl$correct,
            ne_cn$market_line==3,is.na(ne_cn$correct),
            ne_el$market_line==3,is.na(ne_el$correct),
            ne_en$market_line==3,is.na(ne_en$correct))
  # Both readers and both modes also calculate totals and implied score lines
  # from the selected spread/total, using the actual long-prediction path.
  legacy_meta <- e$inventory |> dplyr::filter(file=='2026_ScoresTrees_home.csv')
  next_meta <- e$nextgen_inventory |> dplyr::filter(season==2026L) |> dplyr::slice_head(n=1)
  for (source in c('closing','early')) {
    for (market in c('spread','total','home_implied','away_implied')) {
      for (engine in c('legacy','nextgen')) {
        z <- if(engine=='legacy') long_predictions_for_file(legacy_meta,market,0,source,'apply',TRUE) else long_nextgen_predictions_for_file(next_meta,market,0,source,'apply',TRUE)
        if(engine=='legacy' && market=='spread' && !nrow(z)) next
        z <- z |> dplyr::filter(game_id=='2026_1_NE_SEA')
        stopifnot(nrow(z)>0,all(z$home_score==13),all(z$away_score==10))
        spread <- if(source=='closing' && engine=='legacy') 3.5 else 3
        line <- switch(market,spread=spread,total=44.5,home_implied=(44.5+spread)/2,away_implied=(44.5-spread)/2)
        stopifnot(all(z$market_line==line))
      }
    }
  }
  # Week 5 future forecasts are independent of the historical-line selector.
  for (pair in list(list(close_l,early_l),list(close_n,early_n))) {
    cols <- c('game_id','avg_projection','market_line','avg_edge','consensus_pick','agree_pct')
    stopifnot(isTRUE(all.equal(pair[[1]][pair[[1]]$week==5,cols],pair[[2]][pair[[2]]$week==5,cols])))
  }
  e$line_source_test_results <- list(legacy=close_l,nextgen=close_n)
})
cat('BACKTEST_LINE_SOURCES_OK\n')
