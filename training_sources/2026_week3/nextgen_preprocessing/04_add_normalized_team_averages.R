# 04_add_normalized_team_averages.R
# Clean R-script version of:
#   Step 4 Get Normalized Team Averages_FIXED_WEEKCUT_ASOFNEXT.RMD
#
# Main exported function:
#   add_normalized_team_averages()
#
# This stage takes the Step 3.5 model_data object, builds weighted last-14
# normalized team/defense/league averages as-of each season/week, merges those
# averages back onto the historical rows, and creates explicit "as-of next week"
# snapshots for future-game appends.

suppressPackageStartupMessages({
  library(dplyr)
})

DEFAULT_WEIGHTS_14 <- c(
  0.093433333,
  0.091766667,
  0.0855,
  0.0823,
  0.0791,
  0.0759,
  0.0727,
  0.0695,
  0.0663,
  0.0631,
  0.0599,
  0.0567,
  0.0535,
  0.0503
)

weighted_avg_pipeline <- function(x, w) {
  denom <- sum(w, na.rm = TRUE)
  if (is.na(denom) || denom == 0) {
    return(NA_real_)
  }
  sum(x * w, na.rm = TRUE) / denom
}

ensure_season_week <- function(data) {
  if (!all(c("season", "week") %in% names(data))) {
    if ("game_id" %in% names(data)) {
      data <- data %>%
        dplyr::mutate(
          season = as.integer(substr(game_id, 1, 4)),
          week = as.integer(substr(game_id, 6, 7))
        )
    } else {
      stop("Step 4 requires columns season & week, or game_id to derive them.", call. = FALSE)
    }
  }

  data %>%
    dplyr::mutate(
      season = as.integer(season),
      week = as.integer(week)
    )
}

build_week_map_with_next <- function(data, max_placeholder_week = 22L) {
  week_map_all <- data %>%
    dplyr::distinct(season, week) %>%
    dplyr::filter(
      is.finite(season),
      is.finite(week),
      week >= 1L,
      week <= max_placeholder_week
    ) %>%
    dplyr::arrange(season, week)

  next_weeks <- week_map_all %>%
    dplyr::group_by(season) %>%
    dplyr::summarise(week = max(week, na.rm = TRUE) + 1L, .groups = "drop") %>%
    dplyr::filter(is.finite(week), week >= 1L, week <= max_placeholder_week)

  week_map_all <- dplyr::bind_rows(week_map_all, next_weeks) %>%
    dplyr::distinct(season, week) %>%
    dplyr::arrange(season, week) %>%
    dplyr::mutate(week_index = dplyr::row_number())

  list(week_map_all = week_map_all, next_weeks = next_weeks)
}

add_week_index <- function(data, week_map_all) {
  data <- data %>%
    dplyr::left_join(week_map_all, by = c("season", "week"))

  if (any(is.na(data$week_index))) {
    bad <- data %>%
      dplyr::filter(is.na(week_index)) %>%
      dplyr::select(dplyr::any_of(c("game_date", "season", "week", "posteam", "defteam"))) %>%
      utils::head(10)
    print(bad)
    stop("Some rows are missing week_index. Ensure season/week are valid for all rows.", call. = FALSE)
  }

  data
}

build_last_n_weighted <- function(data, team_col, season_in, week_in, week_map_all, weights) {
  as_of <- week_map_all %>%
    dplyr::filter(season == season_in, week == week_in) %>%
    dplyr::pull(week_index)

  if (length(as_of) != 1L || !is.finite(as_of)) {
    stop(
      "Could not resolve week_index for season=", season_in, " week=", week_in,
      ". Check that season/week exist or placeholder week was added.",
      call. = FALSE
    )
  }

  data %>%
    dplyr::filter(week_index < as_of) %>%
    dplyr::arrange(.data[[team_col]], dplyr::desc(week_index), dplyr::desc(game_date)) %>%
    dplyr::group_by(.data[[team_col]]) %>%
    dplyr::slice_head(n = length(weights)) %>%
    dplyr::mutate(
      rn = dplyr::row_number(),
      w_raw = weights[rn],
      weight = w_raw / sum(w_raw, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
}

weighted_team_week_avg <- function(data, team_col, prefix, season_in, week_in, week_map_all, weights) {
  df <- build_last_n_weighted(data, team_col, season_in, week_in, week_map_all, weights)

  num_cols <- df %>%
    dplyr::select(where(is.numeric)) %>%
    names()
  num_cols <- setdiff(num_cols, c("season", "week", "week_index", "rn", "w_raw", "weight"))

  out <- df %>%
    dplyr::group_by(.data[[team_col]]) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(num_cols), ~ weighted_avg_pipeline(.x, weight)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(season = season_in, week = week_in)

  names(out) <- ifelse(
    names(out) %in% c(team_col, "season", "week"),
    names(out),
    paste0(prefix, names(out))
  )

  out
}

weighted_league_week_avg <- function(data, season_in, week_in, week_map_all, weights) {
  df <- build_last_n_weighted(data, "posteam", season_in, week_in, week_map_all, weights)

  num_cols <- df %>%
    dplyr::select(where(is.numeric)) %>%
    names()
  num_cols <- setdiff(num_cols, c("season", "week", "week_index", "rn", "w_raw", "weight"))

  team_means <- df %>%
    dplyr::group_by(posteam) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(num_cols), ~ weighted_avg_pipeline(.x, weight)),
      .groups = "drop"
    )

  out <- team_means %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(num_cols), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(season = season_in, week = week_in)

  names(out) <- ifelse(
    names(out) %in% c("season", "week"),
    names(out),
    paste0("NFL_avg_", names(out))
  )

  out
}

fix_nan_numeric_columns <- function(df) {
  reference_rows <- !is.na(df$season) & df$season < 2026
  if (!any(reference_rows)) stop("No pre-2026 rows for normalized-average imputation", call. = FALSE)
  df %>%
    dplyr::mutate(
      dplyr::across(where(is.numeric), ~ {
        x <- .x
        if (any(is.nan(x))) {
          replacement <- mean(x[reference_rows], na.rm = TRUE)
          if (is.finite(replacement)) x[is.nan(x)] <- replacement
        }
        x
      })
    )
}

build_asof_next_snapshots <- function(data, posteam_avgs, defteam_avgs, league_avgs, next_weeks) {
  anchor_dates <- data %>%
    dplyr::group_by(season) %>%
    dplyr::summarise(anchor_game_date = max(game_date, na.rm = TRUE) + 1, .groups = "drop")

  asof_next_post <- posteam_avgs %>%
    dplyr::semi_join(next_weeks, by = c("season", "week")) %>%
    dplyr::left_join(anchor_dates, by = "season") %>%
    dplyr::transmute(posteam, game_date = anchor_game_date, dplyr::across(dplyr::starts_with("posteam_avg_")))

  asof_next_def <- defteam_avgs %>%
    dplyr::semi_join(next_weeks, by = c("season", "week")) %>%
    dplyr::left_join(anchor_dates, by = "season") %>%
    dplyr::transmute(defteam, game_date = anchor_game_date, dplyr::across(dplyr::starts_with("defteam_avg_")))

  asof_next_nfl <- league_avgs %>%
    dplyr::semi_join(next_weeks, by = c("season", "week")) %>%
    dplyr::left_join(anchor_dates, by = "season") %>%
    dplyr::transmute(game_date = anchor_game_date, dplyr::across(dplyr::starts_with("NFL_avg_")))

  list(
    asof_next_post = asof_next_post,
    asof_next_def = asof_next_def,
    asof_next_nfl = asof_next_nfl
  )
}

add_normalized_team_averages <- function(model_data,
                                         weights = DEFAULT_WEIGHTS_14,
                                         max_placeholder_week = 22L) {
  message("Adding weighted normalized team/defense/league averages...")

  if (length(weights) != 14L || abs(sum(weights) - 1) > 1e-6) {
    stop("Step 4 weights must have length 14 and sum to approximately 1.", call. = FALSE)
  }

  data <- model_data %>%
    ensure_season_week() %>%
    dplyr::mutate(game_date = as.Date(game_date)) %>%
    dplyr::arrange(dplyr::desc(game_date), posteam_type, posteam)

  week_parts <- build_week_map_with_next(data, max_placeholder_week = max_placeholder_week)
  week_map_all <- week_parts$week_map_all
  next_weeks <- week_parts$next_weeks

  data <- add_week_index(data, week_map_all)

  season_week_list <- week_map_all %>%
    dplyr::arrange(season, week)

  posteam_avgs <- dplyr::bind_rows(lapply(seq_len(nrow(season_week_list)), function(i) {
    weighted_team_week_avg(
      data = data,
      team_col = "posteam",
      prefix = "posteam_avg_",
      season_in = season_week_list$season[i],
      week_in = season_week_list$week[i],
      week_map_all = week_map_all,
      weights = weights
    )
  }))

  defteam_avgs <- dplyr::bind_rows(lapply(seq_len(nrow(season_week_list)), function(i) {
    weighted_team_week_avg(
      data = data,
      team_col = "defteam",
      prefix = "defteam_avg_",
      season_in = season_week_list$season[i],
      week_in = season_week_list$week[i],
      week_map_all = week_map_all,
      weights = weights
    )
  }))

  league_avgs <- dplyr::bind_rows(lapply(seq_len(nrow(season_week_list)), function(i) {
    weighted_league_week_avg(
      data = data,
      season_in = season_week_list$season[i],
      week_in = season_week_list$week[i],
      week_map_all = week_map_all,
      weights = weights
    )
  }))

  asof_next <- build_asof_next_snapshots(
    data = data,
    posteam_avgs = posteam_avgs,
    defteam_avgs = defteam_avgs,
    league_avgs = league_avgs,
    next_weeks = next_weeks
  )

  normalized_avg_data <- data %>%
    dplyr::select(-week_index) %>%
    dplyr::left_join(posteam_avgs, by = c("posteam", "season", "week")) %>%
    dplyr::left_join(defteam_avgs, by = c("defteam", "season", "week")) %>%
    dplyr::left_join(league_avgs, by = c("season", "week")) %>%
    fix_nan_numeric_columns()

  list(
    data = normalized_avg_data,
    asof_next = asof_next,
    posteam_avgs = posteam_avgs,
    defteam_avgs = defteam_avgs,
    league_avgs = league_avgs
  )
}
