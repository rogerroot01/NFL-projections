app <- source('app.R', local=TRUE)$value
expected <- read_circa_bundled_lines(2026L,5L)
stopifnot(nrow(expected)==15L, circa_parse_ocr_spread('+7.5')==7.5,
          circa_parse_ocr_spread('-3.5')==-3.5)
check <- function(actual, reference) {
  actual <- actual[order(actual$game_id),]
  reference <- reference[order(reference$game_id),]
  stopifnot(identical(actual$game_id, reference$game_id),
            isTRUE(all.equal(actual$circa_away_line,reference$circa_away_line)),
            isTRUE(all.equal(actual$circa_home_line,reference$circa_home_line)))
}
pdf_path <- Sys.getenv('CIRCA_WEEK5_PDF',
  'C:/Users/rroot/Downloads/Circa-Sports-Million-VIII-Contest-Point-Spreads-Week-5.pdf')
if (file.exists(pdf_path)) {
  check(load_circa_lines(pdf_path,2026L,5L),expected)
  for (week in c(1L,3L,4L)) {
    old_pdf <- file.path(dirname(pdf_path),paste0('Circa-Sports-Million-VIII-Contest-Point-Spreads-Week-',week,'.pdf'))
    if(file.exists(old_pdf)) check(load_circa_lines(old_pdf,2026L,week),read_circa_bundled_lines(2026L,week))
  }
}
if (identical(Sys.getenv('CIRCA_TEST_WEB'),'1')) {
  check(download_circa_week_from_web(2026L,5L),expected)
}
shiny::testServer(app$serverFuncSource(), {
  session$setInputs(circa_season=2026L,circa_week=5L,circa_source='combined')
  check(circa_lines_state(),expected)
  # Shiny stores uploads under extensionless names: dispatch uses original name.
  if (file.exists(pdf_path)) {
    upload <- tempfile()
    file.copy(pdf_path,upload)
    session$setInputs(circa_lines_file=list(datapath=upload,name=basename(pdf_path)))
    check(circa_lines_state(),expected)
    stopifnot(grepl('Loaded 15 Circa matchups',circa_status_state(),fixed=TRUE))
    unlink(upload)
  }
  session$setInputs(circa_build=1L)
  stopifnot(nrow(circa_results_state())==15L,nrow(circa_ranked_rows())>0L,
            grepl('Complete. Ranked 15',circa_status_state(),fixed=TRUE),
            grepl('alert-success',output$circa_status_notice$html,fixed=TRUE))
  widget <- jsonlite::fromJSON(output$circa_top_five)
  stopifnot(identical(dim(widget$x$data),c(10L,5L)),is.null(widget$x$options$ajax),
            is.null(widget$x$options$serverSide))
  # Reject malformed uploads rather than keeping a stale, apparently successful card.
  bad <- tempfile(fileext='.csv')
  writeLines('away_team,home_team,home_spread\nPHI,JAX,-1.5',bad)
  session$setInputs(circa_lines_file=list(datapath=bad,name='incomplete.csv'))
  stopifnot(nrow(circa_lines_state())==0L,nrow(circa_results_state())==0L,
            grepl('Circa upload error:',circa_status_state(),fixed=TRUE),
            grepl('alert-danger',output$circa_status_notice$html,fixed=TRUE))
  session$setInputs(circa_build=2L)
  stopifnot(nrow(circa_results_state())==0L,
            grepl('Circa upload error:',circa_status_state(),fixed=TRUE))
  session$setInputs(circa_use_default=1L,circa_build=3L)
  check(circa_lines_state(),expected)
  stopifnot(nrow(circa_results_state())==15L)
  unlink(bad)
})
cat('CIRCA_WEEK5_PDF_UPLOAD_BUILD_OK\n')
