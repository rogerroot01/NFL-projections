app <- source("app.R", local = TRUE)$value
pdf_path <- "C:/Users/rroot/Downloads/Circa-Sports-Million-VIII-Contest-Point-Spreads-Week-4.pdf"
if (file.exists(pdf_path)) {
  from_pdf <- load_circa_lines(pdf_path, 2026L, 4L)
  from_csv <- read_circa_bundled_lines(2026L, 4L)
  stopifnot(nrow(from_pdf) == 16L, nrow(from_csv) == 16L,
            identical(from_pdf$game_id, from_csv$game_id),
            isTRUE(all.equal(from_pdf$circa_away_line, from_csv$circa_away_line)))
}
shiny::testServer(app$serverFuncSource(), {
  session$setInputs(circa_season = 2026L, circa_week = 4L, circa_source = "combined")
  session$setInputs(circa_build = 1L)
  stopifnot(nrow(circa_results_state()) == 16L)
})
cat("CIRCA_WEEK4_OK\n")
