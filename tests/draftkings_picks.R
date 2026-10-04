app <- source(Sys.getenv("NFL_ENSEMBLE_APP", "app.R"), local = TRUE)$value
stopifnot(grepl("DraftKings Picks", htmltools::renderTags(ui)$html, fixed = TRUE))

week4 <- poolhost_schedule %>%
  dplyr::filter(season == 2026L, week == 4L) %>%
  dplyr::left_join(read_circa_bundled_lines(2026L, 4L) %>%
                     dplyr::select(game_id, circa_home_line), by = "game_id")
stopifnot(nrow(week4) == 15L, all(is.finite(week4$circa_home_line)))
paste_text <- paste(sprintf("%s at %s | %s", week4$away_team, week4$home_team,
                            week4$circa_home_line), collapse = "\n")
parsed <- poolhost_parse_pasted_lines(paste_text, 2026L, 4L)
stopifnot(nrow(parsed) == 15L)

bad <- draftkings_lines_from_poolhost(parsed)
bad$draftkings_home_line[1] <- NA_real_
stopifnot(inherits(try(draftkings_validate_lines(bad), silent = TRUE), "try-error"))
bad <- draftkings_lines_from_poolhost(parsed)
bad$draftkings_home_line[1] <- 2.25
stopifnot(inherits(try(draftkings_validate_lines(bad), silent = TRUE), "try-error"))

shiny::testServer(app$serverFuncSource(), {
  session$setInputs(poolhost_season = 2026L, poolhost_week = 4L)
  session$setInputs(poolhost_paste = paste_text)
  session$setInputs(poolhost_load_text = 1L)
  defaults <- dkp_lines_state()
  stopifnot(nrow(defaults) == 15L,
            identical(defaults$draftkings_home_line, poolhost_lines_state()$poolhost_home_line),
            all(defaults$week == 4L))

  edited <- defaults
  edited$draftkings_home_line[1] <- edited$draftkings_home_line[1] + 0.5
  selectors <- stats::setNames(as.list(as.character(edited$draftkings_home_line)),
                               paste0("dkp_line_", edited$game_id))
  do.call(session$setInputs, selectors)
  session$setInputs(dkp_season = 2026L, dkp_week = 4L, dkp_source = "combined",
                    dkp_min_edge = 0, dkp_no_minus_3_5_favorites = FALSE,
                    dkp_no_minus_7_5_favorites = FALSE,
                    dkp_no_plus_2_5_underdogs = FALSE,
                    dkp_no_early_week_games = TRUE, dkp_build = 1L)
  stopifnot(nrow(dkp_results_state()) == 15L,
            identical(dkp_lines_state()$draftkings_home_line, edited$draftkings_home_line),
            identical(poolhost_lines_state()$poolhost_home_line, defaults$draftkings_home_line),
            nrow(dkp_card_rows()) >= 7L,
            nrow(dkp_card_rows() %>% dplyr::slice_head(n = 5)) == 5L,
            nrow(dkp_card_rows() %>% dplyr::slice(6:7)) == 2L)

  first_game <- dkp_card_rows()$game_id[1]
  session$setInputs(dkp_skip_matchups = first_game)
  stopifnot(!first_game %in% dkp_card_rows()$game_id)
  session$setInputs(dkp_skip_matchups = character())
  session$setInputs(dkp_min_edge = 10)
  stopifnot(all(dkp_ranked_rows()$edge >= 10))

  session$setInputs(dkp_reset_poolhost = 1L)
  stopifnot(identical(dkp_lines_state()$draftkings_home_line,
                      poolhost_lines_state()$poolhost_home_line),
            nrow(dkp_results_state()) == 0L)
})

cat("DRAFTKINGS_PICKS_OK\n")
