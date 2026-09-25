app <- source("app.R", local = TRUE)$value

shiny::testServer(app$serverFuncSource(), {
  rows <- tibble::tibble(
    game_id = paste0("2026_03_", seq_len(8)),
    season = 2026L,
    week = 3L,
    away_team = paste0("A", seq_len(8)),
    home_team = paste0("H", seq_len(8)),
    avg_projection = seq(8, 1),
    current_market_line = 0,
    circa_market_line = 0,
    circa_away_line = 0,
    circa_home_line = 0,
    agree_pct = 1,
    models_used = 12L
  )
  circa_results_state(rows)
  session$setInputs(
    circa_no_minus_3_5_favorites = FALSE,
    circa_no_plus_2_5_underdogs = FALSE,
    circa_no_early_week_games = FALSE,
    circa_min_edge = 0
  )
  stopifnot(identical(circa_card_rows()$game_id[1:5], rows$game_id[1:5]))

  session$setInputs(circa_skip_matchups = rows$game_id[2])
  stopifnot(
    nrow(circa_card_rows()) == 7L,
    identical(circa_card_rows()$game_id[1:5], rows$game_id[c(1, 3, 4, 5, 6)]),
    identical(circa_card_rows()$game_id[6:7], rows$game_id[7:8])
  )
})

cat("CIRCA_SKIP_PROMOTION_OK\n")
