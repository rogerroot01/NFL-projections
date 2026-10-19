args <- commandArgs(trailingOnly=TRUE)
if(length(args))setwd(args[[1L]])
e <- new.env(parent=globalenv());app <- source('app.R',local=e)$value
shiny::testServer(app$serverFuncSource(), {
  fixture <- tibble::tibble(season=2026L,projections=9L,models_used=9L,agree_pct=.8,
    market='spread',spread_line=3,market_line=3,
    actual_result=c(7,0,3,NA_real_,10),
    consensus_pick=c('Home','Home','Home','Home',NA_character_),
    correct=c(TRUE,FALSE,NA,NA,NA))
  summary <- summarize_consensus_table(fixture)
  splits <- consensus_splits_table(fixture,'spread')
  stopifnot(all(summary$Games==3L),all(summary$Wins==1L),all(summary$Losses==1L),
    all(summary$Pushes==1L),all(summary[['Win %']]=='50.0%'),
    splits$Games[[1L]]==3L,splits$Pushes[[1L]]==1L)
})
cat('ENSEMBLE_HISTORICAL_SUMMARY_OK: future and no-pick rows excluded; actual pushes retained.\n')
