app <- source('app.R', local=TRUE)$value
expected <- read_circa_bundled_lines(2026L,6L)
stopifnot(nrow(expected)==14L, identical(circa_match_ocr_team('A9OERS'),'SF'),
          identical(circa_match_ocr_team('JAGUAR'),'JAX'),
          is.na(circa_match_ocr_team('UNKNOWN')), circa_parse_ocr_spread('PK')==0)
check <- function(actual, reference=expected) {
  actual <- actual[order(actual$game_id),]
  reference <- reference[order(reference$game_id),]
  stopifnot(identical(actual$game_id,reference$game_id),
            isTRUE(all.equal(actual$circa_away_line,reference$circa_away_line)),
            isTRUE(all.equal(actual$circa_home_line,reference$circa_home_line)))
}
pdf <- 'tests/fixtures/circa_week6.pdf'
stopifnot(file.exists(pdf))
check(load_circa_lines(pdf,2026L,6L))
upload <- tempfile()
file.copy(pdf,upload)
check(load_circa_lines(upload,2026L,6L,'circa_week6.pdf'))
wrong <- tryCatch(load_circa_lines(upload,2026L,5L,'circa_week6.pdf'),error=identity)
stopifnot(inherits(wrong,'error'),grepl('week does not match',conditionMessage(wrong)))
if (identical(Sys.getenv('CIRCA_TEST_WEB'),'1')) {
  web <- download_circa_week_from_web(2026L,6L)
  check(web)
  stopifnot(grepl('circasports.com',attr(web,'source_url'),fixed=TRUE))
}
shiny::testServer(app$serverFuncSource(), {
  session$setInputs(circa_season=2026L,circa_week=6L,circa_source='combined')
  check(circa_lines_state())
  session$setInputs(circa_lines_file=list(datapath=upload,name='circa_week6.pdf'))
  check(circa_lines_state())
  stopifnot(grepl('Loaded 14 Circa matchups',circa_status_state(),fixed=TRUE))
  session$setInputs(circa_build=1L)
  stopifnot(nrow(circa_results_state())==14L,nrow(circa_ranked_rows())>0L,
            grepl('Complete. Ranked 14',circa_status_state(),fixed=TRUE))
  # A blocked/failed refresh must preserve this week's already verified input.
  original <- download_circa_week_from_web
  assign('download_circa_week_from_web',function(...)stop('HTTP 403 blocked'),envir=.GlobalEnv)
  session$setInputs(circa_refresh_web=1L)
  check(circa_lines_state())
  stopifnot(nrow(circa_results_state())==14L,
            grepl('keeping the loaded verified sheet',circa_status_state(),fixed=TRUE))
  assign('download_circa_week_from_web',original,envir=.GlobalEnv)
})
unlink(upload)
cat('CIRCA_WEEK6_PDF_WEB_UPLOAD_BUILD_OK\n')
