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
    circa_home_line = c(-7, -7.5, -8, rep(0, 5)),
    agree_pct = 1,
    models_used = 12L
  )
  circa_results_state(rows)
  session$setInputs(
    circa_no_minus_3_5_favorites = FALSE,
    circa_no_minus_7_5_favorites = FALSE,
    circa_no_plus_2_5_underdogs = FALSE,
    circa_no_early_week_games = FALSE,
    circa_min_edge = 0
  )
  stopifnot(identical(circa_card_rows()$game_id[1:5], rows$game_id[1:5]))

  session$setInputs(circa_no_minus_7_5_favorites = TRUE)
  stopifnot(
    nrow(circa_ranked_rows()) == 7L,
    !rows$game_id[2] %in% circa_ranked_rows()$game_id,
    all(rows$game_id[c(1, 3)] %in% circa_ranked_rows()$game_id)
  )
  session$setInputs(circa_no_minus_7_5_favorites = FALSE)

  session$setInputs(circa_skip_matchups = rows$game_id[2])
  stopifnot(
    nrow(circa_card_rows()) == 7L,
    identical(circa_card_rows()$game_id[1:5], rows$game_id[c(1, 3, 4, 5, 6)]),
    identical(circa_card_rows()$game_id[6:7], rows$game_id[7:8])
  )

  session$setInputs(circa_final_picks = rows$game_id[c(1, 3, 4, 5, 7)])
  stopifnot(
    nrow(circa_final_rows()) == 5L,
    identical(circa_final_rows()$game_id, rows$game_id[c(1, 3, 4, 5, 7)]),
    identical(output$circa_final_status, "Five plays selected.")
  )
  session$setInputs(circa_final_picks = rows$game_id[c(1, 3, 4, 5)])
  stopifnot(grepl("Select exactly five plays", output$circa_final_status, fixed = TRUE))
})

cat("CIRCA_SKIP_PROMOTION_OK\n")
