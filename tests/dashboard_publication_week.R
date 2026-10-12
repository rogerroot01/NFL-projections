app <- source('app.R',local=TRUE)$value
stopifnot(dashboard_default_week(2026L,1:18,5:18)==6L,
          dashboard_default_week(2026L,1:18,7:18)==7L,
          dashboard_default_week(2025L,1:18,integer())==1L,
          dashboard_default_week(2026L,6L,integer())==6L)
shiny::testServer(app$serverFuncSource(), {
  # Capture the selector update to test its fresh-session default and manual review.
  selector_updates <- list()
  session$sendInputMessage <- function(inputId,message) {
    selector_updates[[inputId]] <<- message
  }
  rows <- tibble::tibble(season=2026L,week=rep(5:6,each=5L),market=rep(c('spread','straight_up','total','home_implied','away_implied'),2L))
  overall_consensus_rows(rows)
  session$setInputs(dashboard_source='combined',dashboard_season='2026')
  stopifnot(selector_updates$dashboard_week$selected=='6')
  session$setInputs(dashboard_week='5')
  overall_consensus_rows(rows[rev(seq_len(nrow(rows))),])
  session$flushReact()
  stopifnot(selector_updates$dashboard_week$selected=='5')
})
cat('DASHBOARD_PUBLICATION_WEEK_DEFAULT_AND_MANUAL_REVIEW_OK\n')
