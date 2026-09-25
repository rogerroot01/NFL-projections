# 03_normalize_and_add_features.R
# Clean R-script version of:
#   - Step 3 Normalize.RMD
#   - Step 3.5 Add Features.RMD
#
# Main exported functions:
#   normalize_sos_data()
#   drop_rolling_average_columns()
#   add_projection_features()
#
# Important conversion notes:
#   - Preserves the old 1/3 strength-of-schedule adjustment by default.
#   - Replaces the fragile `data.sos <- data.sos[, c(1:110)]` with explicit
#     dropping of support columns starting with posteam_avg_, defteam_avg_, NFL_avg_.
#   - The old Step 3 had two normalization loops that both wrote back into the
#     same base variables. The second loop therefore overwrote the first loop.
#     This first-pass conversion preserves that behavior to avoid silently
#     changing your model. We should review it separately before changing it.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

sos_normalization_variables <- function() {
  c(
    "rush_RB", "rushing_yards_excl_qb_kneel", "rushing_yards_excl_qb_scramble",
    "rushing_yards_qb_scramble", "first_down_plays", "second_down_plays",
    "third_down_plays", "fourth_down_plays", "field_goal_attempts", "punt_attempt",
    "play", "rush_attempt", "rush", "pass_attempt", "pass", "qb_kneel",
    "qb_spike", "qb_scramble", "qb_dropback", "penalty", "penalty_yards",
    "first_down", "first_down_rush", "first_down_pass", "first_down_penalty",
    "yards_gained", "passing_yards", "receiving_yards", "rushing_yards",
    "return_yards", "complete_pass", "interception", "qb_hit", "sack",
    "tackled_for_loss", "safety", "fumble", "fumble_forced", "fumble_not_forced",
    "fumble_lost", "fumble_out_of_bounds", "touchdown", "pass_touchdown",
    "rush_touchdown", "return_touchdown", "epa", "wpa", "air_yards", "air_epa",
    "comp_air_epa", "yards_after_catch", "yac_epa", "comp_yac_epa", "qb_epa",
    "xyac_epa", "xyac_success", "air_wpa", "yac_wpa", "comp_air_wpa",
    "comp_yac_wpa", "pass_def", "kick_distance_mean", "drive_summary",
    "drive_inside20", "drive_ended_with_score", "posteam_score", "defteam_score",
    "drive_ended_with_punt", "drive_ended_with_turnover", "drive_three_and_out",
    "drive_ended_with_missedFG", "drive_ended_with_safety", "drive_ended_with_kneel",
    "drive_ended_with_half", "SuccOff_earlydown", "SuccOff_latedown",
    "SuccOff_earlydown_rush", "SuccOff_earlydown_pass", "SuccOff_latedown_rush",
    "SuccOff_latedown_pass", "AvgYdsToGo_Off", "Ints_Off_Inside20", "RZ_YAC_Off",
    "explosive_plays"
  )
}

safe_div <- function(num, den) {
  ifelse(is.na(num) | is.na(den) | den <= 0, NA_real_, num / den)
}

clamp01 <- function(p, eps = 1e-6) {
  pmin(pmax(p, eps), 1 - eps)
}

logit_safe <- function(p) {
  log(clamp01(p) / (1 - clamp01(p)))
}

require_columns <- function(df, cols, data_name = deparse(substitute(df))) {
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "%s is missing required column(s): %s",
        data_name,
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

normalize_sos_data <- function(merged_data,
                               sos_divisor = 3,
                               min_model_season = 2014L) {
  message("Normalizing SOS-adjusted data with divisor: ", sos_divisor)

  data <- merged_data %>%
    dplyr::filter(season >= min_model_season) %>%
    dplyr::mutate(game_date = as.Date(game_date)) %>%
    dplyr::arrange(dplyr::desc(game_date), game_id, posteam_type, posteam)

  variables <- sos_normalization_variables()

  require_columns(data, variables, "merged_data")
  require_columns(data, paste0("NFL_avg_", variables), "merged_data")
  require_columns(data, paste0("defteam_avg_", variables), "merged_data")
  require_columns(data, paste0("posteam_avg_", variables), "merged_data")

  data_sos <- data

  # Old Step 3 first loop: normalize posteam stats against opposing defense.
  for (variable in variables) {
    nfl_avg_var <- paste0("NFL_avg_", variable)
    defteam_avg_var <- paste0("defteam_avg_", variable)

    data_sos[[variable]] <- ifelse(
      data[[variable]] == 0,
      data[[variable]],
      data[[variable]] - (data[[defteam_avg_var]] - data[[nfl_avg_var]]) / sos_divisor
    )
  }

  # Old Step 3 second loop: this overwrote the same variables using posteam averages.
  # Preserved for first-pass parity; review separately before changing.
  for (variable in variables) {
    nfl_avg_var <- paste0("NFL_avg_", variable)
    posteam_avg_var <- paste0("posteam_avg_", variable)

    data_sos[[variable]] <- ifelse(
      data[[variable]] == 0,
      data[[variable]],
      data[[variable]] - (data[[posteam_avg_var]] - data[[nfl_avg_var]]) / sos_divisor
    )
  }

  data_sos <- data_sos %>%
    dplyr::mutate(
      away_score = ifelse(away_team == posteam, posteam_score, defteam_score),
      home_score = ifelse(home_team == posteam, posteam_score, defteam_score)
    )

  nonnegative_cols <- c(
    "pass_def", "explosive_plays", "RZ_YAC_Off", "Ints_Off_Inside20",
    "SuccOff_earlydown_pass", "SuccOff_earlydown_rush", "SuccOff_latedown_pass",
    "SuccOff_latedown_rush", "penalty", "first_down_penalty", "penalty_yards",
    "qb_hit", "sack", "interception", "tackled_for_loss", "fumble",
    "rush_touchdown", "fumble_out_of_bounds", "safety"
  )

  for (col in intersect(nonnegative_cols, names(data_sos))) {
    data_sos[[col]] <- pmax(data_sos[[col]], 0)
  }

  data_sos <- data_sos %>%
    dplyr::mutate(safety = pmax(0, safety, na.rm = TRUE))

  data_sos
}

drop_rolling_average_columns <- function(data_sos) {
  data_sos %>%
    dplyr::select(
      -dplyr::starts_with("posteam_avg_"),
      -dplyr::starts_with("defteam_avg_"),
      -dplyr::starts_with("NFL_avg_")
    )
}

add_basic_ratio_features <- function(df) {
  df %>%
    dplyr::mutate(
      yds_per_play             = safe_div(yards_gained, play),
      yds_per_rush             = safe_div(rushing_yards, rush_attempt),
      yds_per_pass_att         = safe_div(passing_yards, pass_attempt),
      comp_pct                 = safe_div(complete_pass, pass_attempt),
      sack_rate                = safe_div(sack, qb_dropback),
      int_rate                 = safe_div(interception, pass_attempt),
      pass_td_rate             = safe_div(pass_touchdown, pass_attempt),
      rush_td_rate             = safe_div(rush_touchdown, rush_attempt),
      scoring_drive_pct        = safe_div(drive_ended_with_score, drive_summary),
      turnover_drive_pct       = safe_div(drive_ended_with_turnover, drive_summary),
      three_and_out_rate       = safe_div(drive_three_and_out, drive_summary),
      punt_rate                = safe_div(drive_ended_with_punt, drive_summary),
      missed_fg_drive_pct      = safe_div(drive_ended_with_missedFG, drive_summary),
      first_down_conv_rate     = safe_div(first_down, first_down_plays + second_down_plays + third_down_plays + fourth_down_plays),
      run_pass_ratio           = safe_div(rush_attempt, rush_attempt + pass_attempt),
      first_down_rush_share    = safe_div(first_down_rush, first_down),
      first_down_pass_share    = safe_div(first_down_pass, first_down),
      first_down_pen_share     = safe_div(first_down_penalty, first_down),
      yac_per_comp             = safe_div(yards_after_catch, complete_pass),
      air_yds_per_att          = safe_div(air_yards, pass_attempt),
      air_to_yac_ratio         = ifelse(is.na(yards_after_catch) | yards_after_catch == 0, NA_real_, air_yards / yards_after_catch),
      epa_per_dropback         = safe_div(epa, qb_dropback),
      qb_hit_rate              = safe_div(qb_hit, qb_dropback),
      wpa_per_play             = safe_div(wpa, play),
      qb_epa_per_dropback      = safe_div(qb_epa, qb_dropback),
      air_epa_per_att          = safe_div(air_epa, pass_attempt),
      yac_epa_per_comp         = safe_div(yac_epa, complete_pass),
      pen_yds_per_play         = safe_div(penalty_yards, play),
      fumble_rate              = safe_div(fumble, play),
      fumble_lost_rate         = safe_div(fumble_lost, fumble),
      forced_fumble_share      = safe_div(fumble_forced, fumble_forced + fumble_not_forced),
      rz_trips                 = drive_inside20,
      rz_td                    = pass_touchdown + rush_touchdown,
      rz_td_rate               = safe_div(rz_td, rz_trips),
      rz_int_rate              = safe_div(Ints_Off_Inside20, rz_trips),
      points_for_per_play      = safe_div(posteam_score, play),
      points_allowed_per_play  = safe_div(defteam_score, play),
      cover_margin             = posteam_score - defteam_score + spread_line,
      total_margin             = posteam_score + defteam_score - total_line,
      rush_share               = safe_div(rush_attempt, play),
      pass_share               = safe_div(pass_attempt, play),
      rush_vs_pass_ratio       = safe_div(rush_attempt, pass_attempt),
      tfl_rate                 = safe_div(tackled_for_loss, play),
      sack_tfl_rate            = safe_div(sack + tackled_for_loss, play)
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ ifelse(is.nan(.x) | is.infinite(.x), NA_real_, .x)))
}

add_engineered_projection_features <- function(data_sos) {
  data_sos %>%
    dplyr::mutate(
      third_down_conv_rate      = safe_div(first_down, third_down_plays + fourth_down_plays),
      early_vs_late_split       = safe_div(SuccOff_earlydown, SuccOff_latedown),
      rz_td_to_int_ratio        = ifelse(is.na(rz_int_rate) | rz_int_rate == 0, NA_real_, rz_td_rate / rz_int_rate),
      points_per_rz_trip        = safe_div(posteam_score, rz_trips),
      points_per_drive          = safe_div(posteam_score, drive_summary),
      points_per_100_yards      = safe_div(posteam_score, yards_gained / 100),
      air_vs_yac_dominance      = safe_div(air_yds_per_att, air_yds_per_att + yac_per_comp),
      turnover_worthy_play_rate = int_rate + fumble_rate,
      negative_play_rate        = safe_div(sack + tackled_for_loss + interception + fumble, play),
      pressure_to_sack_conv     = safe_div(sack, sack + qb_hit),
      score_diff                = posteam_score - defteam_score,
      spread_adjusted_perf      = score_diff - spread_line,
      total_adjusted_scoring    = safe_div(posteam_score + defteam_score, total_line),
      rush_eff_x_share          = yds_per_rush * rush_share,
      pass_epa_x_share          = epa_per_dropback * pass_share,
      fd_conv_x_score_drive     = first_down_conv_rate * scoring_drive_pct,
      rush_eff_weighted         = yds_per_rush * rush_share,
      pass_eff_weighted         = epa_per_dropback * pass_share,
      neg_play_rate             = safe_div(sack + tackled_for_loss + interception + fumble, play),
      turnover_play_rate        = int_rate + fumble_rate,
      explosive_play_rate       = safe_div(explosive_plays, play),
      balance_tilt              = pass_epa_x_share - rush_eff_x_share,
      eff_weight_gap            = pass_eff_weighted - rush_eff_weighted,
      finishing_vs_yards        = safe_div(points_per_drive, points_per_100_yards),
      explosive_finishing       = explosive_play_rate * points_per_drive,
      sustain_vs_explosive      = safe_div(third_down_conv_rate, explosive_play_rate),
      early_late_synergy        = early_vs_late_split * third_down_conv_rate,
      ball_security_risk        = turnover_play_rate + neg_play_rate,
      pressure_turnover_couple  = pressure_to_sack_conv * turnover_worthy_play_rate,
      pressure_damage           = pressure_to_sack_conv * negative_play_rate,
      clutch_factor             = third_down_conv_rate * fd_conv_x_score_drive,
      spread_signal_weighted    = spread_adjusted_perf * fd_conv_x_score_drive,
      over_signal               = total_adjusted_scoring * (pass_epa_x_share + explosive_play_rate),
      third_down_logit          = logit_safe(third_down_conv_rate),
      explosive_logit           = logit_safe(explosive_play_rate),
      p2s_logit                 = logit_safe(pressure_to_sack_conv),
      air_vs_yac_logit          = logit_safe(air_vs_yac_dominance),
      ln_points_per_rz_trip     = log1p(points_per_rz_trip),
      ln_points_per_drive       = log1p(points_per_drive),
      td_rate_rz                = dplyr::coalesce(rz_td_rate, safe_div(rz_td, rz_trips)),
      int_rate_rz               = dplyr::coalesce(rz_int_rate, safe_div(Ints_Off_Inside20, rz_trips)),
      eps_rate                  = 0.5 / pmax(rz_trips, 1),
      rz_td_to_int_ratio_eps    = (td_rate_rz + eps_rate) / (int_rate_rz + eps_rate),
      red_zone_leverage         = points_per_rz_trip * rz_td_to_int_ratio_eps
    )
}

impute_selected_feature_na <- function(data_sos) {
  # Production training ends with 2025. Never let newly completed 2026 games
  # change the imputation constants used by older games or future forecasts.
  reference_rows <- !is.na(data_sos$season) & data_sos$season < 2026
  if (!any(reference_rows)) {
    stop("No pre-2026 rows available for feature imputation", call. = FALSE)
  }
  cols_to_fix <- c(
    "red_zone_leverage", "ln_points_per_rz_trip", "p2s_logit", "pressure_damage",
    "pressure_turnover_couple", "sustain_vs_explosive", "finishing_vs_yards",
    "pressure_to_sack_conv", "points_per_rz_trip", "rz_td_rate", "rz_int_rate",
    "fumble_lost_rate", "forced_fumble_share", "rz_td_to_int_ratio", "td_rate_rz",
    "early_late_synergy", "int_rate_rz", "rz_td_to_int_ratio_eps"
  )

  missing_feature_cols <- setdiff(cols_to_fix, names(data_sos))
  if (length(missing_feature_cols) > 0) {
    warning(
      "Skipping NA imputation for missing feature column(s): ",
      paste(missing_feature_cols, collapse = ", "),
      call. = FALSE
    )
  }

  for (col in intersect(cols_to_fix, names(data_sos))) {
    m <- mean(data_sos[[col]][reference_rows], na.rm = TRUE)
    if (is.finite(m)) {
      data_sos[[col]][is.na(data_sos[[col]])] <- m
    }
  }

  data_sos
}

add_projection_features <- function(data_sos,
                                    drop_support_average_columns = TRUE) {
  message("Adding ratio and engineered projection features...")

  if (isTRUE(drop_support_average_columns)) {
    data_sos <- drop_rolling_average_columns(data_sos)
  }

  data_sos %>%
    add_basic_ratio_features() %>%
    add_engineered_projection_features() %>%
    impute_selected_feature_na()
}
