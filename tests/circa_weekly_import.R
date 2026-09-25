source("app.R", local = TRUE)

week1 <- read_circa_bundled_lines(2026L, 1L)
week3 <- read_circa_bundled_lines(2026L, 3L)
stopifnot(nrow(week1) == 16L, nrow(week3) == 16L)
stopifnot(circa_active_season == 2026L)
stopifnot(circa_active_week == 3L || as.Date(Sys.time(), tz = "America/New_York") > as.Date("2026-09-28"))
stopifnot(is.na(circa_parse_ocr_spread("-Th")))
stopifnot(circa_parse_ocr_spread("+4%") == 4.5)
stopifnot(circa_parse_ocr_spread("-11") == -11)

partial <- week3[1, , drop = FALSE]
partial_error <- tryCatch(validate_circa_lines(partial), error = identity)
stopifnot(inherits(partial_error, "error"))

pdf_path <- Sys.getenv("CIRCA_WEEK3_PDF", "")
if (nzchar(pdf_path)) {
  stopifnot(file.exists(pdf_path))
  uploaded_temp <- tempfile(pattern = "shiny-upload-")
  file.copy(pdf_path, uploaded_temp)
  uploaded <- load_circa_lines(
    uploaded_temp, 2026L, 3L,
    "Circa-Sports-Million-VIII-Contest-Point-Spreads-Week-3.pdf"
  )
  check <- function(actual) {
    actual <- actual[order(actual$game_id), , drop = FALSE]
    expected <- week3[order(week3$game_id), , drop = FALSE]
    stopifnot(identical(actual$game_id, expected$game_id))
    stopifnot(isTRUE(all.equal(actual$circa_away_line, expected$circa_away_line)))
    stopifnot(isTRUE(all.equal(actual$circa_home_line, expected$circa_home_line)))
  }
  check(uploaded)
  shiny::testServer(server, {
    session$setInputs(circa_season = 2026L, circa_week = 3L)
    session$setInputs(circa_lines_file = list(
      datapath = uploaded_temp,
      name = "Circa-Sports-Million-VIII-Contest-Point-Spreads-Week-3.pdf"
    ))
    stopifnot(nrow(circa_lines_state()) == 16L)
    stopifnot(grepl("Manual upload:", circa_source_url_state(), fixed = TRUE))
  })
  wrong_week <- tryCatch(load_circa_lines(uploaded_temp, 2026L, 2L,
                                          "Circa-Sports-Million-VIII-Contest-Point-Spreads-Week-3.pdf"),
                         error = identity)
  stopifnot(inherits(wrong_week, "error"))

  if (identical(Sys.getenv("CIRCA_TEST_WEB"), "1")) {
    downloaded <- download_circa_week_from_web(2026L, 3L)
    check(downloaded)
    stopifnot(grepl("circasports.com", attr(downloaded, "source_url"), fixed = TRUE))
  }
  unlink(uploaded_temp)
}

shiny::testServer(server, {
  session$setInputs(circa_season = 2026L, circa_week = 3L)
  stopifnot(nrow(circa_lines_state()) == 16L)
  session$setInputs(circa_build = 1L)
  stopifnot(nrow(circa_ranked_rows()) == 16L)
  stopifnot(grepl("Ranked 16 Circa sides", circa_status_state(), fixed = TRUE))
  session$setInputs(circa_week = 1L)
  stopifnot(nrow(circa_lines_state()) == 16L, all(circa_lines_state()$week == 1L))
  session$setInputs(circa_week = 3L)
  stopifnot(nrow(circa_lines_state()) == 16L, all(circa_lines_state()$week == 3L))
})

cat("Circa weekly import checks passed.\n")
