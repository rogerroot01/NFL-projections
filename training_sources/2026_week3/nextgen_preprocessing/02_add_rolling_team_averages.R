# 02_add_rolling_team_averages.R
# Clean R-script version of "Step 2 Get Team Averages v5 flat_FIXED_WEEKCUT.RMD".
#
# Main exported function:
#   add_rolling_team_averages(final_game_data, rolling_games = 14L)
#
# This stage creates as-of rolling averages by:
#   1. posteam
#   2. defteam
#   3. league-wide 32-team x rolling_games baseline
#
# Intentional conversion notes:
#   - Uses week-based cutoffs exactly like the *_FIXED_WEEKCUT RMD.
#   - Adds a placeholder next-week row per season so future/current-week
#     projections can join against the latest completed data.
#   - Returns the merged object instead of requiring an immediate write/read.

suppressPackageStartupMessages({
  library(dplyr)
})

get_season_week_grid <- function(data, max_placeholder_week = 22L) {
  actual_weeks <- data %>%
    dplyr::distinct(season, week)

  next_weeks <- data %>%
    dplyr::group_by(season) %>%
    dplyr::summarise(week = max(week, na.rm = TRUE) + 1L, .groups = "drop") %>%
    dplyr::filter(is.finite(week), week >= 1L, week <= max_placeholder_week)

  dplyr::bind_rows(actual_weeks, next_weeks) %>%
    dplyr::distinct(season, week) %>%
    dplyr::arrange(dplyr::desc(season), dplyr::desc(week))
}

rolling_group_average <- function(data,
                                  season_in,
                                  week_in,
                                  group_col,
                                  prefix,
                                  rolling_games = 14L) {
  group_sym <- rlang::sym(group_col)

  prior_games <- data %>%
    dplyr::filter(season < season_in | (season == season_in & week < week_in)) %>%
    dplyr::arrange(!!group_sym, dplyr::desc(season), dplyr::desc(week), dplyr::desc(game_date)) %>%
    dplyr::group_by(!!group_sym) %>%
    dplyr::slice_head(n = rolling_games) %>%
    dplyr::ungroup()

  data_means <- prior_games %>%
    dplyr::group_by(!!group_sym) %>%
    dplyr::summarise(
      dplyr::across(dplyr::where(is.numeric), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  names(data_means) <- ifelse(
    names(data_means) == group_col,
    group_col,
    paste0(prefix, names(data_means))
  )

  data_means$season <- season_in
  data_means$week <- week_in

  data_means
}

rolling_league_average <- function(data,
                                   season_in,
                                   week_in,
                                   rolling_games = 14L) {
  prior_games <- data %>%
    dplyr::filter(season < season_in | (season == season_in & week < week_in)) %>%
    dplyr::arrange(posteam, dplyr::desc(season), dplyr::desc(week), dplyr::desc(game_date)) %>%
    dplyr::group_by(posteam) %>%
    dplyr::slice_head(n = rolling_games) %>%
    dplyr::ungroup()

  data_means <- prior_games %>%
    dplyr::summarise(
      dplyr::across(dplyr::where(is.numeric), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  # Match old intent: season/week are join keys, not league-average fields.
  data_means$season <- season_in
  data_means$week <- week_in

  names(data_means) <- ifelse(
    names(data_means) %in% c("season", "week"),
    names(data_means),
    paste0("NFL_avg_", names(data_means))
  )

  data_means
}

fix_numeric_nan_with_column_mean <- function(df) {
  reference_rows <- !is.na(df$season) & df$season < 2026
  if (!any(reference_rows)) stop("No pre-2026 rows for rolling-average imputation", call. = FALSE)
  df[] <- lapply(df, function(x) {
    if (is.numeric(x)) {
      replacement <- mean(x[reference_rows], na.rm = TRUE)
      if (is.finite(replacement)) x[is.nan(x)] <- replacement
    }
    x
  })
  df
}

add_rolling_team_averages <- function(final_game_data,
                                      rolling_games = 14L,
                                      max_placeholder_week = 22L) {
  message("Adding rolling ", rolling_games, "-game team and league averages...")

  data <- final_game_data %>%
    dplyr::mutate(game_date = as.Date(game_date)) %>%
    dplyr::arrange(dplyr::desc(game_date), posteam_type, posteam)

  unique_season_weeks <- get_season_week_grid(
    data,
    max_placeholder_week = max_placeholder_week
  )

  posteam_avgs <- dplyr::bind_rows(lapply(seq_len(nrow(unique_season_weeks)), function(i) {
    rolling_group_average(
      data = data,
      season_in = unique_season_weeks$season[i],
      week_in = unique_season_weeks$week[i],
      group_col = "posteam",
      prefix = "posteam_avg_",
      rolling_games = rolling_games
    )
  }))

  defteam_avgs <- dplyr::bind_rows(lapply(seq_len(nrow(unique_season_weeks)), function(i) {
    rolling_group_average(
      data = data,
      season_in = unique_season_weeks$season[i],
      week_in = unique_season_weeks$week[i],
      group_col = "defteam",
      prefix = "defteam_avg_",
      rolling_games = rolling_games
    )
  }))

  league_avgs <- dplyr::bind_rows(lapply(seq_len(nrow(unique_season_weeks)), function(i) {
    rolling_league_average(
      data = data,
      season_in = unique_season_weeks$season[i],
      week_in = unique_season_weeks$week[i],
      rolling_games = rolling_games
    )
  })) %>%
    fix_numeric_nan_with_column_mean()

  data %>%
    dplyr::left_join(posteam_avgs, by = c("posteam", "season", "week")) %>%
    dplyr::left_join(defteam_avgs, by = c("defteam", "season", "week")) %>%
    dplyr::left_join(league_avgs, by = c("season", "week"))
}
