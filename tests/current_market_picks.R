app <- source("app.R", local = TRUE)$value
stopifnot(grepl("Current Market", htmltools::renderTags(ui)$html, fixed = TRUE))

week4 <- read_circa_bundled_lines(2026L, 4L)
paste_text <- paste(sprintf("%s at %s | %s", week4$away_team, week4$home_team,
                            week4$circa_home_line), collapse = "\n")
parsed <- market_parse_pasted_lines(paste_text, 2026L, 4L)
stopifnot(nrow(parsed) == 16L,
          setequal(parsed$game_id, week4$game_id),
          identical(parsed$market_home_line, as.numeric(week4$circa_home_line)))
stopifnot(nrow(market_parse_pasted_lines("PIT,CLE,+2.5", 2026L, 4L)) == 1L)
sportsbook_text <- paste(c("THU OCT 1st", "Spread", "Total", "Moneyline",
                          "PIT Steelers-logo", "PIT Steelers", "at",
                          "CLE Browns-logo", "CLE Browns", "-2.5", "-120",
                          "O", "38.5", "-105", "-148", "+2.5", "+100",
                          "U", "38.5", "-115", "+124", "More Bets"), collapse = "\n")
sportsbook <- market_parse_pasted_lines(sportsbook_text, 2026L, 4L)
stopifnot(nrow(sportsbook) == 1L, sportsbook$game_id[[1L]] == "2026_4_PIT_CLE",
          sportsbook$market_home_line[[1L]] == 2.5)
city_ambiguous_text <- paste(c(
  "Tomorrow", "NY Jets-logo", "NY Jets", "at", "CHI Bears-logo", "CHI Bears",
  "+3.5", "-108", "O", "43.5", "+100", "+154", "-3.5", "-112", "More Bets",
  "LA Chargers-logo", "LA Chargers", "at", "SEA Seahawks-logo", "SEA Seahawks",
  "+7", "-105", "O", "42.5", "-118", "+295", "-7", "-115", "More Bets",
  "ARI Cardinals-logo", "ARI Cardinals", "at", "NY Giants-logo", "NY Giants",
  "-2.5", "-112", "O", "44.5", "-108", "-135", "+2.5", "-108", "More Bets"
), collapse = "\n")
city_ambiguous <- market_parse_pasted_lines(city_ambiguous_text, 2026L, 4L)
stopifnot(nrow(city_ambiguous) == 3L,
          setequal(city_ambiguous$game_id,
                   c("2026_4_NYJ_CHI", "2026_4_LAC_SEA", "2026_4_ARI_NYG")),
          identical(city_ambiguous$market_home_line, c(-3.5, -7, 2.5)))
week5_sportsbook_text <- paste(readLines('tests/fixtures/current_market_week05_sportsbook.txt',
                                        encoding = 'UTF-8', warn = FALSE), collapse = '\n')
week5_sportsbook <- market_parse_pasted_lines(week5_sportsbook_text, 2026L, 5L)
stopifnot(nrow(week5_sportsbook) == 15L,
  identical(week5_sportsbook$away_team,
    c('TB','PHI','CIN','NYG','HOU','CLE','CHI','LV','IND','MIN','DEN','DET','SF','BAL','BUF')),
  identical(week5_sportsbook$home_team,
    c('DAL','JAX','MIA','WAS','TEN','NYJ','GB','NE','PIT','NO','LAC','ARI','SEA','ATL','LA')),
  identical(week5_sportsbook$market_home_line,
    c(-8.5,-7,6.5,-3.5,7.5,-1.5,2.5,-3.5,-2.5,1.5,3.5,5.5,-2.5,-3.5,-3)))
for (code_point in c(0x2212, 0x2012, 0x2013, 0x2014, 0xFF0D)) {
  unicode_spread <- market_parse_pasted_lines(paste0('SF at SEA | ', intToUtf8(code_point), '2.5'), 2026L, 5L)
  stopifnot(unicode_spread$market_home_line[[1L]] == -2.5)
}
stopifnot(inherits(try(market_parse_pasted_lines("CLE at PIT | -2.5", 2026L, 4L),
                      silent = TRUE), "try-error"))
stopifnot(inherits(try(market_parse_pasted_lines("PIT at CLE | +2.25", 2026L, 4L),
                      silent = TRUE), "try-error"))
stopifnot(inherits(try(market_parse_pasted_lines("PIT at CLE | +2.5\nPIT at CLE | +3",
                                                     2026L, 4L), silent = TRUE), "try-error"))

shiny::testServer(app$serverFuncSource(), {
  session$setInputs(market_load = 0L)
  session$setInputs(market_season = 2026L, market_week = 4L,
                    market_paste = paste_text, market_load = 1L)
  stopifnot(nrow(market_lines_state()) == 16L,
            grepl("Loaded 16", market_status_state(), fixed = TRUE))

  poolhost_lines <- parsed %>% dplyr::filter(game_id %in% poolhost_schedule$game_id)
  poolhost_text <- paste(sprintf("%s at %s | %s", poolhost_lines$away_team,
                                 poolhost_lines$home_team, poolhost_lines$market_home_line),
                         collapse = "\n")
  session$setInputs(poolhost_season = 2026L, poolhost_week = 4L,
                    poolhost_paste = poolhost_text, poolhost_load_text = 1L,
                    poolhost_source = "combined", poolhost_build = 1L)
  picks <- poolhost_results_state()
  stopifnot(nrow(picks) == 15L)
  compared <- market_compare_rows(picks, "pick_side", "poolhost_pick_line",
                                  "projected_pick_line")
  stopifnot(all(is.finite(compared$current_market_pick_line)),
            all(abs(compared$contest_market_difference) < 0.001),
            all(is.finite(compared$current_market_edge)))
  gaps <- market_gap_display(picks, "pick_side", "poolhost_pick_line")
  stopifnot(nrow(gaps) == 7L,
            "Contest − current" %in% names(gaps))

  session$setInputs(market_paste = "IND at WAS | +3.5", market_load = 2L)
  stopifnot(nrow(market_lines_state()) == 1L)
  partial <- market_compare_rows(picks, "pick_side", "poolhost_pick_line")
  stopifnot(sum(is.finite(partial$contest_market_difference)) == 1L,
            sum(is.na(partial$contest_market_difference)) == 14L)
  session$setInputs(market_week = 5L, market_paste = week5_sportsbook_text, market_load = 3L)
  stopifnot(nrow(market_lines_state()) == 15L,
    identical(market_lines_state(), week5_sportsbook),
    grepl('Loaded 15 current market spreads for Week 5', market_status_state(), fixed = TRUE))
  session$setInputs(market_paste = 'SF at SEA | -2.25', market_load = 4L)
  stopifnot(identical(market_lines_state(), week5_sportsbook),
    grepl('previous snapshot retained', market_status_state(), fixed = TRUE))
})

cat("CURRENT_MARKET_PICKS_OK\n")
