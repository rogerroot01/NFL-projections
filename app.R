library(shiny)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tidyr)
library(DT)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

canonical_game_id <- function(x) {
  sub("^(\\d{4})_0([1-9])_", "\\1_\\2_", as.character(x), perl = TRUE)
}

app_data_dir <- file.path(getwd(), "data")
early_lines_path <- file.path(app_data_dir, "early_lines.csv")
current_lines_path <- file.path(app_data_dir, "current_lines_2026.csv")

early_lines <- if (file.exists(early_lines_path)) {
  read_csv(early_lines_path, show_col_types = FALSE) %>%
    transmute(
      game_id = canonical_game_id(.data[["game_id"]]),
      early_spread_line = suppressWarnings(as.numeric(.data[["Mid-week Spread"]])),
      early_total_line = suppressWarnings(as.numeric(.data[["Mid-week Total"]]))
    )
} else {
  tibble(
    game_id = character(),
    early_spread_line = numeric(),
    early_total_line = numeric()
  )
}

current_lines <- if (file.exists(current_lines_path)) {
  read_csv(current_lines_path, show_col_types = FALSE) %>%
    transmute(
      game_id = canonical_game_id(.data[["game_id"]]),
      current_spread_line = suppressWarnings(as.numeric(.data[["spread_line"]])),
      current_total_line = suppressWarnings(as.numeric(.data[["total_line"]]))
    ) %>%
    distinct(game_id, .keep_all = TRUE)
} else {
  tibble(
    game_id = character(),
    current_spread_line = numeric(),
    current_total_line = numeric()
  )
}

legacy_pregame_lines_path <- file.path(app_data_dir, "legacy_pregame_lines_2026.csv")
if (!file.exists(legacy_pregame_lines_path)) {
  stop("Archived 2026 Legacy pregame lines are missing.")
}
legacy_pregame_lines <- read_csv(legacy_pregame_lines_path, show_col_types = FALSE) %>%
  mutate(game_id = paste(season, week, away_team, home_team, sep = "_")) %>%
  select(game_id, archived_spread_line = spread_line,
         archived_total_line = total_line)
if (anyDuplicated(legacy_pregame_lines$game_id) ||
    any(!is.finite(legacy_pregame_lines$archived_spread_line)) ||
    any(!is.finite(legacy_pregame_lines$archived_total_line))) {
  stop("Archived 2026 Legacy pregame lines are invalid.")
}

apply_legacy_pregame_lines <- function(df) {
  if (!all(c("game_id", "home_score", "away_score") %in% names(df))) return(df)
  idx <- match(canonical_game_id(df$game_id), legacy_pregame_lines$game_id)
  completed <- is.finite(suppressWarnings(as.numeric(df$home_score))) &
    is.finite(suppressWarnings(as.numeric(df$away_score)))
  use <- completed & !is.na(idx)
  if ("spread_line" %in% names(df)) {
    df$spread_line[use] <- legacy_pregame_lines$archived_spread_line[idx[use]]
  }
  if ("total_line" %in% names(df)) {
    df$total_line[use] <- legacy_pregame_lines$archived_total_line[idx[use]]
  }
  if ("home_implied" %in% names(df)) {
    df$home_implied[use] <- (df$total_line[use] + df$spread_line[use]) / 2
  }
  if ("away_implied" %in% names(df)) {
    df$away_implied[use] <- (df$total_line[use] - df$spread_line[use]) / 2
  }
  df
}

apply_current_lines <- function(df, preserve_completed = FALSE) {
  if (is.data.frame(df) && "game_id" %in% names(df)) df$game_id <- canonical_game_id(df$game_id)
  if (!is.data.frame(df) || nrow(df) == 0 || !"game_id" %in% names(df) || nrow(current_lines) == 0) {
    return(df)
  }

  line_row <- match(as.character(df$game_id), current_lines$game_id)
  allow_update <- rep(TRUE, nrow(df))
  if (isTRUE(preserve_completed) && all(c("home_score", "away_score") %in% names(df))) {
    allow_update <- !(is.finite(suppressWarnings(as.numeric(df$home_score))) &
      is.finite(suppressWarnings(as.numeric(df$away_score))))
  }
  if ("spread_line" %in% names(df)) {
    replacement <- current_lines$current_spread_line[line_row]
    replace_at <- allow_update & !is.na(line_row) & !is.na(replacement)
    df$spread_line[replace_at] <- replacement[replace_at]
  }
  if ("total_line" %in% names(df)) {
    replacement <- current_lines$current_total_line[line_row]
    replace_at <- allow_update & !is.na(line_row) & !is.na(replacement)
    df$total_line[replace_at] <- replacement[replace_at]
  }
  if (all(c("spread_line", "total_line") %in% names(df))) {
    has_market_lines <- allow_update & !is.na(df$spread_line) & !is.na(df$total_line)
    if ("home_implied" %in% names(df)) {
      df$home_implied[has_market_lines] <- (df$total_line[has_market_lines] + df$spread_line[has_market_lines]) / 2
    }
    if ("away_implied" %in% names(df)) {
      df$away_implied[has_market_lines] <- (df$total_line[has_market_lines] - df$spread_line[has_market_lines]) / 2
    }
  }
  df
}

apply_consensus_edge_threshold <- function(rows, minimum_edge = 2) {
  if (!is.data.frame(rows) || nrow(rows) == 0 || !"avg_edge" %in% names(rows)) return(rows)
  threshold <- suppressWarnings(as.numeric(minimum_edge))
  if (length(threshold) == 0 || !is.finite(threshold[[1L]])) threshold <- 2
  threshold <- max(0, threshold[[1L]])
  rows[!is.na(rows$avg_edge) & abs(rows$avg_edge) >= threshold, , drop = FALSE]
}

read_injury_csv <- function(path) {
  if (file.exists(path)) read_csv(path, show_col_types = FALSE) else tibble()
}

tbl_col <- function(df, nm, default = NA) {
  if (nm %in% names(df)) df[[nm]] else rep(default, nrow(df))
}

game_injury_path <- file.path(app_data_dir, "hiddengame_injury_adjustments.csv")
team_injury_2025_path <- file.path(app_data_dir, "hiddengame_team_injury_adjustments_2025.csv")
forecast_team_adjustments_2026_path <- file.path(app_data_dir, "forecast_team_off_def_injuries_2026.csv")

game_injury_raw <- read_injury_csv(game_injury_path)
game_injuries <- tibble(
  game_id = canonical_game_id(coalesce(tbl_col(game_injury_raw, "game_id_input"), tbl_col(game_injury_raw, "game_id"))),
  injury_adj = suppressWarnings(as.numeric(coalesce(
    tbl_col(game_injury_raw, "injury_adj"),
    tbl_col(game_injury_raw, "injury_adjust"),
    tbl_col(game_injury_raw, "injury_adjustment")
  )))
)

team_injury_2025_raw <- read_injury_csv(team_injury_2025_path)
team_injuries_2025 <- tibble(
  game_id = canonical_game_id(tbl_col(team_injury_2025_raw, "game_id")),
  home_off_injury_adj = suppressWarnings(as.numeric(tbl_col(team_injury_2025_raw, "home_off_injury_adj"))),
  home_def_injury_adj = suppressWarnings(as.numeric(tbl_col(team_injury_2025_raw, "home_def_injury_adj"))),
  away_off_injury_adj = suppressWarnings(as.numeric(tbl_col(team_injury_2025_raw, "away_off_injury_adj"))),
  away_def_injury_adj = suppressWarnings(as.numeric(tbl_col(team_injury_2025_raw, "away_def_injury_adj")))
)

forecast_team_adjustments_2026_raw <- read_injury_csv(forecast_team_adjustments_2026_path)
forecast_team_adjustments_2026 <- tibble(
  season = suppressWarnings(as.integer(tbl_col(forecast_team_adjustments_2026_raw, "season"))),
  week = suppressWarnings(as.integer(tbl_col(forecast_team_adjustments_2026_raw, "week"))),
  home_team = toupper(trimws(as.character(tbl_col(forecast_team_adjustments_2026_raw, "home_team")))),
  away_team = toupper(trimws(as.character(tbl_col(forecast_team_adjustments_2026_raw, "away_team")))),
  future_home_spread_adj = suppressWarnings(as.numeric(tbl_col(forecast_team_adjustments_2026_raw, "home_spread_adj"))),
  future_away_spread_adj = suppressWarnings(as.numeric(tbl_col(forecast_team_adjustments_2026_raw, "away_spread_adj"))),
  future_home_score_adj = suppressWarnings(as.numeric(tbl_col(forecast_team_adjustments_2026_raw, "home_score_adj"))),
  future_away_score_adj = suppressWarnings(as.numeric(tbl_col(forecast_team_adjustments_2026_raw, "away_score_adj"))),
  home_adjust_amortization = suppressWarnings(as.numeric(tbl_col(forecast_team_adjustments_2026_raw, "home_adjust_amortization"))),
  away_adjust_amortization = suppressWarnings(as.numeric(tbl_col(forecast_team_adjustments_2026_raw, "away_adjust_amortization")))
)

apply_projection_adjustments <- function(base, injury_source = "apply", apply_amortization = TRUE) {
  injury_enabled <- identical(injury_source, "apply")
  amortization_enabled <- isTRUE(apply_amortization)
  if (!injury_enabled && !amortization_enabled) {
    return(base %>%
      mutate(
        spread_projection_adj = 0,
        total_projection_adj = 0,
        home_score_projection_adj = 0,
        away_score_projection_adj = 0
      ))
  }

  base %>%
    left_join(game_injuries, by = "game_id") %>%
    left_join(team_injuries_2025, by = "game_id") %>%
    left_join(
      forecast_team_adjustments_2026,
      by = c("season", "week", "home_team", "away_team")
    ) %>%
    mutate(
      home_score_injury_component = if (injury_enabled) {
        coalesce(
          future_home_score_adj,
          coalesce(home_off_injury_adj, 0) + coalesce(away_def_injury_adj, 0),
          0
        )
      } else 0,
      away_score_injury_component = if (injury_enabled) {
        coalesce(
          future_away_score_adj,
          coalesce(away_off_injury_adj, 0) + coalesce(home_def_injury_adj, 0),
          0
        )
      } else 0,
      spread_injury_component = if (injury_enabled) {
        coalesce(
          future_home_score_adj - future_away_score_adj,
          injury_adj,
          home_score_injury_component - away_score_injury_component,
          0
        )
      } else 0,
      spread_amortization_component = if (amortization_enabled) {
        coalesce(home_adjust_amortization, 0) - coalesce(away_adjust_amortization, 0)
      } else 0,
      forecast_spread_available = !is.na(future_home_spread_adj) | !is.na(future_away_spread_adj),
      home_score_projection_adj = home_score_injury_component,
      away_score_projection_adj = away_score_injury_component,
      spread_projection_adj = if (injury_enabled && amortization_enabled) {
        if_else(
          forecast_spread_available,
          coalesce(future_home_spread_adj, 0) - coalesce(future_away_spread_adj, 0),
          spread_injury_component + spread_amortization_component
        )
      } else {
        spread_injury_component + spread_amortization_component
      },
      total_projection_adj = home_score_injury_component + away_score_injury_component
    ) %>%
    select(
      -injury_adj,
      -home_off_injury_adj,
      -home_def_injury_adj,
      -away_off_injury_adj,
      -away_def_injury_adj,
      -future_home_spread_adj,
      -future_away_spread_adj,
      -future_home_score_adj,
      -future_away_score_adj,
      -home_adjust_amortization,
      -away_adjust_amortization,
      -home_score_injury_component,
      -away_score_injury_component,
      -spread_injury_component,
      -spread_amortization_component,
      -forecast_spread_available
    )
}

family_labels <- c(
  ScoresTrees = "Scores Trees",
  BillyTrees = "Billy Trees",
  ScoresLateReg = "Scores Late Regression",
  Billy = "Billy"
)

next_gen_family_labels <- c(
  elastic_net_lasso = "Elastic Net Lasso",
  weighted_linear_regression = "Weighted Linear Regression",
  decision_tree_rpart = "Decision Tree",
  random_forest_ranger = "Random Forest",
  gbm_boosted_trees = "GBM Boosted Trees",
  xgboost_regression = "XGBoost Regression"
)

market_labels <- c(
  all = "All markets",
  spread = "Spread / score diff",
  straight_up = "Straight up",
  total = "Total",
  home_implied = "Home implied",
  away_implied = "Away implied",
  team_implied = "Team implied",
  opp_implied = "Opponent implied",
  score = "Team score"
)

market_choices <- stats::setNames(names(market_labels), market_labels)
dashboard_market_keys <- c("spread", "straight_up", "total", "home_implied", "away_implied")

circa_team_aliases <- c(
  CARDINALS = "ARI", FALCONS = "ATL", RAVENS = "BAL", BILLS = "BUF",
  PANTHERS = "CAR", BEARS = "CHI", BENGALS = "CIN", BROWNS = "CLE",
  COWBOYS = "DAL", BRONCOS = "DEN", LIONS = "DET", PACKERS = "GB",
  TEXANS = "HOU", COLTS = "IND", JAGUARS = "JAX", CHIEFS = "KC",
  RAIDERS = "LV", CHARGERS = "LAC", RAMS = "LA", DOLPHINS = "MIA",
  VIKINGS = "MIN", PATRIOTS = "NE", SAINTS = "NO", GIANTS = "NYG",
  JETS = "NYJ", EAGLES = "PHI", STEELERS = "PIT", `49ERS` = "SF",
  SEAHAWKS = "SEA", BUCS = "TB", TITANS = "TEN", COMMANDERS = "WAS",
  A9ERS = "SF"
)

circa_normalize_team <- function(x) {
  key <- toupper(gsub("[^A-Z0-9]", "", trimws(as.character(x))))
  mapped <- unname(circa_team_aliases[key])
  ifelse(!is.na(mapped), mapped, key)
}

circa_parse_ocr_spread <- function(token) {
  token <- as.character(token %||% "")
  cleaned <- token
  for (code_point in c(0x2212, 0x2013, 0x2014, 0x2018, 0x2019, 0x201C, 0x201D)) {
    cleaned <- str_replace_all(cleaned, fixed(intToUtf8(code_point)), "-")
  }
  upper <- toupper(cleaned)
  if (str_detect(upper, "PK|PICK")) return(0)

  sign <- case_when(
    str_detect(upper, fixed("+")) ~ 1,
    str_detect(upper, fixed("-")) ~ -1,
    TRUE ~ NA_real_
  )
  if (is.na(sign)) return(NA_real_)

  digits <- str_extract(upper, "[0-9]+")
  whole <- suppressWarnings(as.numeric(digits))
  if (is.na(whole) && str_detect(upper, "A")) whole <- 4
  half_mark <- str_detect(upper, "[%HVY,]") | str_detect(upper, fixed(intToUtf8(0x00BD)))
  if (is.na(whole) && gsub("[+-]", "", upper) %in% c("½", "H", "%")) whole <- 0
  if (is.na(whole)) return(NA_real_)
  sign * (whole + ifelse(half_mark, 0.5, 0))
}

validate_circa_lines <- function(rows) {
  required <- c("contest", "season", "week", "game_id", "away_team", "home_team", "circa_away_line", "circa_home_line")
  missing <- setdiff(required, names(rows))
  if (length(missing) > 0) stop("Circa lines are missing: ", paste(missing, collapse = ", "))
  if (nrow(rows) == 0) stop("No Circa matchups were found.")
  if (any(is.na(rows$season) | is.na(rows$week)) || n_distinct(rows$season) != 1L || n_distinct(rows$week) != 1L) {
    stop("The Circa sheet must contain exactly one season and one week.")
  }
  if (any(is.na(rows$circa_away_line)) || any(is.na(rows$circa_home_line))) stop("At least one Circa spread could not be read.")
  if (any(abs(rows$circa_away_line + rows$circa_home_line) > 0.001)) stop("At least one Circa matchup has non-opposite spreads.")
  teams <- c(rows$away_team, rows$home_team)
  if (any(is.na(teams) | teams == "") || anyDuplicated(teams)) stop("Each team must appear exactly once on the Circa weekly sheet.")
  expected_ids <- paste(rows$season, rows$week, rows$away_team, rows$home_team, sep = "_")
  if (any(is.na(rows$game_id)) || any(rows$game_id != expected_ids) || anyDuplicated(rows$game_id)) {
    stop("Circa game IDs must match the season, week, away team, and home team exactly.")
  }
  scheduled_ids <- current_lines$game_id[str_detect(current_lines$game_id, paste0("^", first(rows$season), "_", first(rows$week), "_"))]
  if (length(scheduled_ids) > 0 && !setequal(rows$game_id, scheduled_ids)) {
    stop("The Circa sheet does not match the complete scheduled slate or home/away orientation for the selected week.")
  }
  rows
}

read_circa_lines_csv <- function(path, default_season = 2026L, default_week = 1L) {
  raw <- read_csv(path, show_col_types = FALSE)
  find_col <- function(options) {
    found <- intersect(options, names(raw))
    if (length(found) == 0) NULL else found[[1L]]
  }
  away_team_col <- find_col(c("away_team", "away", "visitor_team", "visitor"))
  home_team_col <- find_col(c("home_team", "home"))
  away_line_col <- find_col(c("circa_away_line", "away_line", "away_spread", "visitor_line"))
  home_line_col <- find_col(c("circa_home_line", "home_line", "home_spread"))
  if (is.null(away_team_col) || is.null(home_team_col)) stop("CSV must include away_team and home_team columns.")
  if (is.null(away_line_col) && is.null(home_line_col)) stop("CSV must include a Circa spread column for at least one team.")

  season <- if ("season" %in% names(raw)) suppressWarnings(as.integer(raw$season)) else rep(as.integer(default_season), nrow(raw))
  week <- if ("week" %in% names(raw)) suppressWarnings(as.integer(raw$week)) else rep(as.integer(default_week), nrow(raw))
  away_line <- if (!is.null(away_line_col)) suppressWarnings(as.numeric(raw[[away_line_col]])) else -suppressWarnings(as.numeric(raw[[home_line_col]]))
  home_line <- if (!is.null(home_line_col)) suppressWarnings(as.numeric(raw[[home_line_col]])) else -away_line
  rows <- tibble(
    contest = if ("contest" %in% names(raw)) as.character(raw$contest) else "Circa Sports Million",
    season = season,
    week = week,
    away_team = circa_normalize_team(raw[[away_team_col]]),
    home_team = circa_normalize_team(raw[[home_team_col]]),
    circa_away_line = away_line,
    circa_home_line = home_line
  ) %>%
    mutate(
      game_id = if ("game_id" %in% names(raw)) as.character(raw$game_id) else paste(season, week, away_team, home_team, sep = "_")
    ) %>%
    select(contest, season, week, game_id, away_team, home_team, circa_away_line, circa_home_line)
  validate_circa_lines(rows)
}

parse_circa_pdf <- function(path, season = 2026L, week = 1L) {
  needed <- c("pdftools", "tesseract", "magick")
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) stop("PDF import requires: ", paste(missing, collapse = ", "), ". A CSV upload is also supported.")

  png_pattern <- paste0(tempfile(pattern = "circa-"), "-%d.%s")
  png_files <- pdftools::pdf_convert(path, format = "png", dpi = 300, pages = 1, filenames = png_pattern, verbose = FALSE)
  on.exit(unlink(png_files, force = TRUE), add = TRUE)
  image <- magick::image_read(png_files[[1L]])
  image_width <- magick::image_info(image)$width[[1L]]
  ocr <- tesseract::ocr_data(image, engine = tesseract::tesseract(options = list(tessedit_pageseg_mode = 11)))
  bbox <- str_split_fixed(as.character(ocr$bbox), ",", 4)
  words <- tibble(
    word = as.character(ocr$word),
    x1 = suppressWarnings(as.numeric(bbox[, 1])),
    y1 = suppressWarnings(as.numeric(bbox[, 2])),
    x2 = suppressWarnings(as.numeric(bbox[, 3])),
    y2 = suppressWarnings(as.numeric(bbox[, 4]))
  ) %>%
    mutate(x = (x1 + x2) / 2, y = (y1 + y2) / 2, x_ratio = x / image_width, key = gsub("[^A-Z0-9]", "", toupper(word)))

  header_words <- words %>% filter(y < magick::image_info(image)$height[[1L]] * 0.15)
  week_labels <- header_words %>% filter(key == "WEEK")
  printed_week <- map_int(seq_len(nrow(week_labels)), function(index) {
    candidates <- header_words %>%
      filter(x > week_labels$x[[index]], x < week_labels$x[[index]] + 180,
             abs(y - week_labels$y[[index]]) < 45, str_detect(word, "^[0-9]{1,2}$")) %>%
      arrange(x)
    if (nrow(candidates) == 0) NA_integer_ else as.integer(candidates$word[[1L]])
  })
  printed_week <- printed_week[!is.na(printed_week)]
  if (length(printed_week) > 0 && !as.integer(week) %in% printed_week) {
    stop("The PDF header's week does not match the selected Circa week.")
  }

  team_rows <- words %>%
    filter(key %in% names(circa_team_aliases)) %>%
    transmute(team = unname(circa_team_aliases[key]), y, side = ifelse(x_ratio < 0.5, "left", "right")) %>%
    distinct(team, .keep_all = TRUE)
  side_counts <- table(team_rows$side)
  if (nrow(team_rows) < 2 || nrow(team_rows) %% 2 != 0 || any(side_counts %% 2 != 0) || n_distinct(team_rows$team) != nrow(team_rows)) {
    stop("The PDF import did not find a valid set of paired NFL matchups. Upload the official one-page Circa sheet or use CSV.")
  }

  odds_words <- words %>%
    filter(x_ratio > 0.35, x_ratio < 0.86) %>%
    mutate(parsed_line = map_dbl(word, circa_parse_ocr_spread))

  attach_line <- function(team, y, side) {
    candidates <- odds_words %>%
      filter(abs(.data$y - !!y) <= 36) %>%
      filter(if (side == "left") x_ratio > 0.36 & x_ratio < 0.48 else x_ratio > 0.74 & x_ratio < 0.86) %>%
      arrange(abs(.data$y - !!y))
    if (nrow(candidates) == 0) return(NA_real_)
    non_missing <- candidates$parsed_line[!is.na(candidates$parsed_line)]
    if (length(non_missing) == 0) NA_real_ else non_missing[[1L]]
  }

  ordered <- team_rows %>%
    arrange(side, y) %>%
    group_by(side) %>%
    mutate(row_in_column = row_number(), game_in_column = ceiling(row_in_column / 2), team_role = ifelse(row_in_column %% 2 == 1, "away", "home")) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(parsed_line = attach_line(team, y, side)) %>%
    ungroup()

  games <- ordered %>%
    group_by(side, game_in_column) %>%
    summarise(
      away_team = team[team_role == "away"][[1L]],
      home_team = team[team_role == "home"][[1L]],
      parsed_away = parsed_line[team_role == "away"][[1L]],
      parsed_home = parsed_line[team_role == "home"][[1L]],
      .groups = "drop"
    ) %>%
    mutate(ocr_pair_conflict = !is.na(parsed_away) & !is.na(parsed_home) & abs(parsed_away + parsed_home) > 0.51)
  if (any(games$ocr_pair_conflict)) {
    conflicts <- games %>% filter(ocr_pair_conflict)
    stop("The PDF import read conflicting spreads for ",
         paste(paste(conflicts$away_team, "at", conflicts$home_team,
                     "(", conflicts$parsed_away, "/", conflicts$parsed_home, ")"), collapse = "; "),
         ". Check the official sheet or use a verified CSV.")
  }
  games <- games %>%
    rowwise() %>%
    mutate(
      magnitude = if (all(is.na(c(parsed_away, parsed_home)))) NA_real_ else max(abs(c(parsed_away, parsed_home)), na.rm = TRUE),
      away_sign = case_when(!is.na(parsed_away) ~ sign(parsed_away), !is.na(parsed_home) ~ -sign(parsed_home), TRUE ~ NA_real_),
      circa_away_line = away_sign * magnitude,
      circa_home_line = -circa_away_line
    ) %>%
    ungroup() %>%
    transmute(
      contest = "Circa Sports Million",
      season = as.integer(season),
      week = as.integer(week),
      game_id = paste(season, week, away_team, home_team, sep = "_"),
      away_team,
      home_team,
      circa_away_line,
      circa_home_line
    )
  validate_circa_lines(games)
}

load_circa_lines <- function(path, season = 2026L, week = 1L, filename = path) {
  extension <- tolower(tools::file_ext(filename))
  switch(
    extension,
    pdf = parse_circa_pdf(path, season, week),
    csv = read_circa_lines_csv(path, season, week),
    stop("Upload a PDF or CSV file.")
  )
}

download_circa_week_from_web <- function(season = 2026L, week = 1L) {
  season <- as.integer(season)
  week <- as.integer(week)
  if (!is.finite(season) || !is.finite(week)) stop("Enter a valid Circa season and week.")
  contest_number <- season - 2018L
  if (contest_number < 1L) stop("Automatic Circa refresh is available for Million I and later.")
  contest_roman <- as.character(as.roman(contest_number))
  filename <- paste0(
    "Circa-Sports-Million-", contest_roman,
    "-Contest-Point-Spreads-Week-", week, ".pdf"
  )
  estimated_month <- case_when(
    week <= 3 ~ "09",
    week <= 8 ~ "10",
    week <= 13 ~ "11",
    week <= 17 ~ "12",
    TRUE ~ "01"
  )
  months <- unique(c(estimated_month, "09", "10", "11", "12", "01"))
  candidates <- map_chr(months, function(month) {
    upload_year <- if (identical(month, "01")) season + 1L else season
    paste0(
      "https://www.circasports.com/wp-content/uploads/", upload_year, "/", month, "/", filename
    )
  })
  downloaded <- NULL
  source_url <- NULL
  for (candidate in candidates) {
    destination <- tempfile(fileext = ".pdf")
    status <- tryCatch(
      suppressWarnings(utils::download.file(candidate, destination, mode = "wb", quiet = TRUE, method = "libcurl")),
      error = function(e) 1L
    )
    is_pdf <- identical(status, 0L) && file.exists(destination) && file.info(destination)$size > 4 &&
      identical(rawToChar(readBin(destination, what = "raw", n = 4)), "%PDF")
    if (is_pdf) {
      downloaded <- destination
      source_url <- candidate
      break
    }
    unlink(destination, force = TRUE)
  }
  if (is.null(downloaded)) {
    stop("The official Circa weekly sheet was not found yet. It is normally posted Thursday; use the manual PDF/CSV fallback if you already have it.")
  }
  on.exit(unlink(downloaded, force = TRUE), add = TRUE)
  rows <- parse_circa_pdf(downloaded, season, week)
  rows$contest <- paste("Circa Sports Million", contest_roman)
  attr(rows, "source_url") <- source_url
  rows
}

read_circa_bundled_lines <- function(season, week) {
  path <- file.path(app_data_dir, paste0("circa_million_", as.integer(season), "_week_", as.integer(week), ".csv"))
  if (file.exists(path)) read_circa_lines_csv(path, season, week) else tibble()
}

parse_model_file <- function(path) {
  nm <- basename(path)
  m <- str_match(nm, "^(\\d{4})_(.+)_(home|away|home_score|away_score)\\.csv$")
  if (any(is.na(m))) return(NULL)
  tibble(
    path = path,
    file = nm,
    season = as.integer(m[, 2]),
    family = m[, 3],
    family_label = family_labels[m[, 3]] %||% m[, 3],
    split = m[, 4]
  )
}

file_inventory <- function() {
  files <- list.files(app_data_dir, pattern = "\\.csv$", full.names = TRUE)
  purrr::map_dfr(files, parse_model_file) %>%
    filter(family %in% names(family_labels)) %>%
    arrange(family, season, split)
}

inventory_rds <- file.path(app_data_dir, "model_inventory.rds")
compact_models_rds <- file.path(app_data_dir, "compact_models.rds")
nextgen_inventory_rds <- file.path(app_data_dir, "nextgen_model_inventory.rds")
nextgen_compact_models_rds <- file.path(app_data_dir, "nextgen_compact_models.rds")

compact_data_available <- file.exists(inventory_rds) && file.exists(compact_models_rds)
nextgen_data_available <- file.exists(nextgen_inventory_rds) && file.exists(nextgen_compact_models_rds)

inventory <- if (compact_data_available) {
  readRDS(inventory_rds) %>%
    mutate(path = file.path(getwd(), path))
} else {
  tibble(
    path = character(),
    file = character(),
    season = integer(),
    family = character(),
    family_label = character(),
    split = character()
  )
}

compact_models <- if (compact_data_available) readRDS(compact_models_rds) else list()
model_game_dates <- purrr::map_dfr(compact_models, function(model_rows) {
  if (!all(c("game_id", "game_date") %in% names(model_rows))) return(tibble())
  tibble(
    game_id = canonical_game_id(model_rows$game_id),
    game_date = suppressWarnings(as.Date(model_rows$game_date))
  ) %>%
    filter(!is.na(game_id), nzchar(game_id), !is.na(game_date)) %>%
    distinct(game_id, game_date)
}) %>%
  arrange(game_date) %>%
  distinct(game_id, .keep_all = TRUE)
model_week_dates <- model_game_dates %>%
  mutate(
    season = suppressWarnings(as.integer(stringr::str_match(game_id, "^(\\d{4})_(\\d{1,2})_")[, 2])),
    week = suppressWarnings(as.integer(stringr::str_match(game_id, "^(\\d{4})_(\\d{1,2})_")[, 3]))
  ) %>%
  filter(!is.na(season), !is.na(week)) %>%
  group_by(season, week) %>%
  summarise(last_game_date = max(game_date), .groups = "drop")
circa_active_season <- if (nrow(model_week_dates) > 0) max(model_week_dates$season) else 2026L
circa_upcoming_weeks <- model_week_dates %>%
  filter(season == circa_active_season, last_game_date >= as.Date(Sys.time(), tz = "America/New_York")) %>%
  pull(week)
circa_active_week <- if (length(circa_upcoming_weeks) > 0) min(circa_upcoming_weeks) else 1L
circa_default_lines <- read_circa_bundled_lines(circa_active_season, circa_active_week)
early_week_game_ids <- model_game_dates %>%
  filter(as.POSIXlt(game_date)$wday %in% 2:5) %>%
  pull(game_id)
nextgen_inventory <- if (nextgen_data_available) {
  readRDS(nextgen_inventory_rds) %>%
    mutate(path = file.path(getwd(), path))
} else {
  tibble(
    path = character(),
    file = character(),
    season = integer(),
    framework = character(),
    sample = character(),
    family = character(),
    family_label = character()
  )
}
nextgen_compact_models <- if (nextgen_data_available) readRDS(nextgen_compact_models_rds) else list()
nextgen_backtest_seasons <- sort(unique(nextgen_inventory$season[nextgen_inventory$sample == "test"]))
nextgen_future_seasons <- sort(unique(nextgen_inventory$season[nextgen_inventory$sample == "val"]))
nextgen_backtest_season_choices <- c(
  `All backtest seasons` = "all",
  stats::setNames(as.character(nextgen_backtest_seasons), as.character(nextgen_backtest_seasons))
)
nextgen_future_season_choices <- if (length(nextgen_future_seasons) == 0) {
  c(`No future seasons available` = "none")
} else {
  c(`All future seasons` = "all", stats::setNames(as.character(nextgen_future_seasons), as.character(nextgen_future_seasons)))
}
file_cache <- new.env(parent = emptyenv())
header_cache <- new.env(parent = emptyenv())

file_header <- function(path) {
  nm <- basename(path)
  if (nm %in% names(compact_models)) return(names(compact_models[[nm]]))
  stop("Compact prepared data is missing for ", nm, ". Run prepare_data.R locally and deploy data/compact_models.rds plus data/model_inventory.rds.")
}

prediction_cols_from_names <- function(cols) {
  cols[str_detect(cols, regex("ScoreDiff|ScoreTotal|TotalScore|Implied|Score_(xgb|forward|stepwise|avg|final)|OppScore|Billy", TRUE)) &
         !str_detect(cols, regex("^Cover_|_target|_cover$|_pm1$", TRUE))]
}

key_cols_from_names <- function(cols) {
  intersect(
    c("game_id", "season", "week", "game_date", "posteam", "defteam", "home_team", "away_team",
      "home_score", "away_score", "spread_line", "total_line"),
    cols
  )
}

cols_needed_for_file <- function(path) {
  cols <- file_header(path)
  cover_cols <- grep("^Cover_", cols, value = TRUE)
  cover_projection_cols <- unique(unlist(purrr::map(cover_cols, projection_candidates_for_cover), use.names = FALSE))
  unique(c(
    key_cols_from_names(cols),
    cover_cols,
    prediction_cols_from_names(cols),
    cover_projection_cols
  )) %>%
    intersect(cols)
}

read_model_file <- function(path) {
  nm <- basename(path)
  if (nm %in% names(compact_models)) {
    return(apply_legacy_pregame_lines(
      apply_current_lines(compact_models[[nm]], preserve_completed = TRUE)
    ))
  }
  stop("Compact prepared data is missing for ", nm, ". Run prepare_data.R locally and deploy data/compact_models.rds plus data/model_inventory.rds.")
}

file_has_graded_results <- function(path) {
  df <- read_model_file(path)
  if (!all(c("home_score", "away_score") %in% names(df))) return(FALSE)
  any(is.finite(suppressWarnings(as.numeric(df$home_score))) &
        is.finite(suppressWarnings(as.numeric(df$away_score))))
}

graded_seasons <- function() {
  if (nrow(inventory) == 0) return(integer())
  inventory %>%
    mutate(has_results = vapply(path, file_has_graded_results, logical(1))) %>%
    filter(has_results) %>%
    pull(season) %>%
    unique() %>%
    sort()
}

backtest_seasons <- graded_seasons()
if (length(backtest_seasons) == 0) {
  backtest_seasons <- sort(unique(inventory$season))
}
backtest_season_choices <- c("All graded seasons" = "all", stats::setNames(as.character(backtest_seasons), as.character(backtest_seasons)))
future_seasons <- setdiff(sort(unique(inventory$season)), backtest_seasons)
future_season_choices <- if (length(future_seasons) == 0) {
  c("No future seasons available" = "none")
} else {
  c("All future seasons" = "all", stats::setNames(as.character(future_seasons), as.character(future_seasons)))
}

cover_market <- function(col) {
  case_when(
    str_detect(col, "Team_ImpliedTotal") ~ "team_implied",
    str_detect(col, "Opp_ImpliedTotal") ~ "opp_implied",
    str_detect(col, "Total|ScoreTotal") ~ "total",
    str_detect(col, "ScoreDiff|ImpliedScoreDiff") ~ "spread",
    str_detect(col, "^Cover_Score") ~ "score",
    TRUE ~ "all"
  )
}

projection_candidates_for_cover <- function(cover_col) {
  base <- str_remove(cover_col, "^Cover_")
  c(
    base,
    str_replace(base, "^Total_", "ImpliedTotal_"),
    str_replace(base, "^Team_ImpliedTotal_", "ImpliedTeamScored_"),
    str_replace(base, "^Opp_ImpliedTotal_", "ImpliedOppScored_"),
    str_replace(base, "^Score$", "Score_final"),
    str_replace(base, "^ScoreDiff$", "ScoreDiff_final")
  ) %>% unique()
}

detect_cover_summary <- function(df, split) {
  cover_cols <- grep("^Cover_", names(df), value = TRUE, ignore.case = TRUE)
  if (length(cover_cols) == 0) return(tibble())

  purrr::map_dfr(cover_cols, function(col) {
    proj <- projection_candidates_for_cover(col)
    proj <- proj[proj %in% names(df)][1] %||% NA_character_
    market <- cover_market(col)
    is_home <- !str_detect(split, "^away")
    numeric_col <- function(name) {
      if (name %in% names(df)) suppressWarnings(as.numeric(df[[name]])) else rep(NA_real_, nrow(df))
    }
    raw <- if (!is.na(proj)) numeric_col(proj) else rep(NA_real_, nrow(df))
    home_score <- numeric_col("home_score")
    away_score <- numeric_col("away_score")
    spread <- numeric_col("spread_line")
    total <- numeric_col("total_line")
    line <- switch(market,
      spread = spread,
      total = total,
      team_implied = (total + if (is_home) spread else -spread) / 2,
      opp_implied = (total + if (is_home) -spread else spread) / 2,
      score = (total + if (is_home) spread else -spread) / 2,
      rep(NA_real_, nrow(df))
    )
    predicted <- if (market == "spread" && !is_home) -raw else raw
    actual <- switch(market,
      spread = home_score - away_score,
      total = home_score + away_score,
      team_implied = if (is_home) home_score else away_score,
      opp_implied = if (is_home) away_score else home_score,
      score = if (is_home) home_score else away_score,
      rep(NA_real_, nrow(df))
    )
    valid <- is.finite(predicted) & is.finite(line) & is.finite(actual) &
      predicted != line & actual != line
    wins <- sum(valid & sign(predicted - line) == sign(actual - line))
    losses <- sum(valid) - wins
    picks <- wins + losses
    tibble(
      market = market,
      market_label = market_labels[market] %||% market,
      result_col = col,
      projection_col = proj,
      picks = picks,
      wins = wins,
      losses = losses,
      win_pct = ifelse(picks > 0, wins / picks, NA_real_)
    )
  }) %>%
    arrange(market_label, desc(win_pct), desc(picks), result_col)
}

file_debug_summary <- function(files) {
  if (nrow(files) == 0) return(tibble(File = character(), Rows = integer(), Columns = integer(), CoverColumns = integer()))
  purrr::pmap_dfr(files, function(path, file, season, family, family_label, split) {
    df <- read_model_file(path)
    tibble(
      File = file,
      Rows = nrow(df),
      Columns = ncol(df),
      CoverColumns = length(grep("^Cover_", names(df), value = TRUE, ignore.case = TRUE)),
      FirstCoverColumns = paste(head(grep("^Cover_", names(df), value = TRUE, ignore.case = TRUE), 5), collapse = ", ")
    )
  })
}

prediction_cols <- function(df) {
  prediction_cols_from_names(names(df))
}

key_cols <- function(df) {
  key_cols_from_names(names(df))
}

norm_key <- function(x) {
  str_replace_all(str_to_lower(as.character(x)), "[^a-z0-9]", "")
}

family_tab_ui <- function(id, label) {
  tabPanel(
    label,
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput(paste0(id, "_season"), "Backtest season", choices = backtest_season_choices, selected = "all"),
        selectInput(paste0(id, "_future_season"), "Future projection season", choices = future_season_choices, selected = if (length(future_seasons) == 0) "none" else "all"),
        selectInput(paste0(id, "_market"), "Market", choices = market_choices, selected = "all"),
        actionButton(paste0(id, "_build"), "Build family summary", class = "btn-primary"),
        tags$hr(),
        downloadButton(paste0(id, "_download_summary"), "Download summary CSV")
      ),
        mainPanel(
          tags$p(tags$small("Click Build family summary to summarize graded files in this family for the selected season. Future seasons with blank cover results are excluded from this backtest selector.")),
          h4("Future projection preview"),
          DTOutput(paste0(id, "_future_preview")),
          tags$hr(),
          h4("Build status"),
          verbatimTextOutput(paste0(id, "_status"), placeholder = TRUE),
          tags$hr(),
          h4("Backtest summary by file and result column"),
          verbatimTextOutput(paste0(id, "_summary"), placeholder = TRUE)
        )
      )
    )
}

nextgen_family_tab_ui <- function(id, label) {
  tabPanel(
    label,
    sidebarLayout(
      sidebarPanel(
        width = 3,
        checkboxGroupInput(
          paste0(id, "_frameworks"),
          "Frameworks",
          choices = c("Early framework" = "early", "Late framework" = "late"),
          selected = c("early", "late")
        ),
        selectInput(paste0(id, "_season"), "Backtest season", choices = nextgen_backtest_season_choices, selected = "all"),
        selectInput(
          paste0(id, "_future_season"),
          "Future projection season",
          choices = nextgen_future_season_choices,
          selected = if (length(nextgen_future_seasons) == 0) "none" else "all"
        ),
        selectInput(paste0(id, "_market"), "Market", choices = market_choices, selected = "all"),
        actionButton(paste0(id, "_build"), "Build family summary", class = "btn-primary"),
        tags$hr(),
        downloadButton(paste0(id, "_download_summary"), "Download summary CSV")
      ),
      mainPanel(
        tags$p(tags$small("Click Build family summary to summarize graded next-gen files in this family for the selected framework and season.")),
        h4("Future projection preview"),
        DTOutput(paste0(id, "_future_preview")),
        tags$hr(),
        h4("Build status"),
        verbatimTextOutput(paste0(id, "_status"), placeholder = TRUE),
        tags$hr(),
        h4("Backtest summary by file and result column"),
        verbatimTextOutput(paste0(id, "_summary"), placeholder = TRUE)
      )
    )
  )
}

ui <- fluidPage(
  tags$head(
    tags$title("NFL Ensemble Model"),
    tags$link(rel = "icon", type = "image/png", href = "ensemble-icon.png"),
    tags$link(rel = "apple-touch-icon", href = "ensemble-icon.png"),
    tags$style(HTML("
      .top-scroll {
        overflow-x: auto;
        overflow-y: hidden;
        height: 18px;
        margin-bottom: 4px;
      }

      .top-scroll-inner {
        height: 1px;
      }

      .app-header {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 16px;
        margin: 8px 0 10px;
      }

      .app-header h2 {
        margin: 0;
        font-size: 26px;
        line-height: 1.2;
        font-weight: 700;
      }

      .prepared-badge {
        margin: 0;
        padding: 6px 10px;
        font-size: 13px;
        line-height: 1.2;
        white-space: nowrap;
      }

      .section-title-row {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 12px;
        margin-bottom: 8px;
      }

      .section-title-row h4 {
        margin: 0;
      }

      body.splash-active {
        overflow: hidden;
      }

      #splash_screen {
        position: fixed;
        inset: 0;
        z-index: 9999;
        display: flex;
        align-items: center;
        justify-content: center;
        padding: 32px;
        color: #f7fbff;
        background:
          linear-gradient(180deg, rgba(0,0,0,0), rgba(0,0,0,0.12)),
          url('stadium-splash.png') center center / cover no-repeat;
      }

      #splash_screen::before {
        content: '';
        position: absolute;
        inset: 0;
        z-index: 0;
        background:
          radial-gradient(ellipse at 50% 48%, rgba(0,0,0,0) 0%, rgba(0,0,0,0.04) 52%, rgba(0,0,0,0.22) 100%),
          linear-gradient(180deg, rgba(0,0,0,0.02), rgba(0,0,0,0) 42%, rgba(0,0,0,0.08));
        opacity: 0.45;
        pointer-events: none;
      }

      #splash_screen::after {
        display: none;
      }

      .splash-card {
        position: relative;
        z-index: 2;
        width: min(760px, 94vw);
        padding: 44px 40px 38px;
        text-align: center;
        border: 0;
        border-radius: 0;
        background: transparent;
        box-shadow: none;
        backdrop-filter: none;
        overflow: hidden;
        isolation: isolate;
      }

      .splash-card::before {
        display: none;
      }

      .splash-nfl-logo {
        width: 192px;
        max-width: 42vw;
        height: auto;
        margin: 0 auto 18px;
        display: block;
        filter: drop-shadow(0 16px 28px rgba(0,0,0,0.5));
      }

      .splash-title {
        margin: 0;
        font-size: clamp(34px, 5.6vw, 58px);
        line-height: 0.95;
        font-weight: 800;
        letter-spacing: 0;
        text-shadow: 0 5px 18px rgba(0,0,0,0.86), 0 1px 2px rgba(0,0,0,0.9);
      }

      .splash-subtitle {
        margin: 16px auto 30px;
        max-width: 620px;
        font-size: 19px;
        line-height: 1.45;
        color: rgba(247,251,255,0.94);
        text-shadow: 0 3px 12px rgba(0,0,0,0.88), 0 1px 2px rgba(0,0,0,0.92);
      }

      #splash_enter {
        min-width: 190px;
        padding: 13px 22px;
        border: 0;
        border-radius: 7px;
        background: #f4f8ff;
        color: #10234e;
        font-weight: 800;
        font-size: 17px;
        box-shadow: 0 10px 24px rgba(0,0,0,0.26);
      }

      #splash_enter:hover,
      #splash_enter:focus {
        background: #ffffff;
        color: #081733;
      }

      #splash_screen.splash-hidden {
        opacity: 0;
        visibility: hidden;
        transition: opacity 220ms ease, visibility 220ms ease;
      }
    ")),
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        document.body.classList.add('splash-active');
        var splash = document.getElementById('splash_screen');
        var enter = document.getElementById('splash_enter');
        if (enter && splash) {
          enter.addEventListener('click', function() {
            splash.classList.add('splash-hidden');
            document.body.classList.remove('splash-active');
          });
        }
      });

      function attachTopScrollbar(tableId) {
        var table = document.getElementById(tableId);
        if (!table) return;
        var wrapper = table.closest('.dataTables_wrapper');
        if (!wrapper || wrapper.querySelector('.top-scroll')) return;
        var body = wrapper.querySelector('.dataTables_scrollBody');
        if (!body) return;

        var top = document.createElement('div');
        top.className = 'top-scroll';
        var inner = document.createElement('div');
        inner.className = 'top-scroll-inner';
        top.appendChild(inner);

        var scroll = wrapper.querySelector('.dataTables_scroll');
        wrapper.insertBefore(top, scroll);

        var syncWidth = function() {
          inner.style.width = body.scrollWidth + 'px';
        };
        syncWidth();
        setTimeout(syncWidth, 250);
        window.addEventListener('resize', syncWidth);

        top.addEventListener('scroll', function() {
          body.scrollLeft = top.scrollLeft;
        });
        body.addEventListener('scroll', function() {
          top.scrollLeft = body.scrollLeft;
        });
      }

      $(document).on('draw.dt init.dt', function(e, settings) {
        if (settings && settings.sTableId) {
          attachTopScrollbar(settings.sTableId);
        }
      });

      Shiny.addCustomMessageHandler('toggleMinWinSlider', function(disabled) {
        var wrapper = $('#cons_min_win').closest('.form-group');
        var sliderObj = $('#cons_min_win').data('ionRangeSlider');
        if (sliderObj) {
          sliderObj.update({ disable: disabled });
        }
        wrapper.css('opacity', disabled ? 0.45 : 1);
        wrapper.find('.irs').css('pointer-events', disabled ? 'none' : 'auto');
      });
    "))
  ),
  div(
    id = "splash_screen",
    div(
      class = "splash-card",
      tags$img(src = "nfl-logo.png", class = "splash-nfl-logo", alt = "NFL logo"),
      h1(class = "splash-title", "NFL Ensemble Model"),
      div(class = "splash-subtitle", "Projection wrangler for model families and consensus signals."),
      actionButton("splash_enter", "Enter Model")
    )
  ),
  div(
    class = "app-header",
    h2("NFL Ensemble Model"),
    if (!compact_data_available) {
      div(
        class = "alert alert-danger prepared-badge",
        strong("Prepared data missing. "),
        "Run prepare_data.R and deploy the compact RDS files."
      )
    } else {
      div(
        class = "alert alert-success prepared-badge",
        paste0(
          "Prepared data loaded: ",
          length(compact_models), " legacy compact model files",
          if (nextgen_data_available) paste0(" and ", length(nextgen_compact_models), " next-gen compact model files.") else "."
        )
      )
    }
  ),
  tabsetPanel(
    id = "main_tabs",
    tabPanel(
      "Legacy Consensus",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          actionButton("cons_run_top", "Build consensus", class = "btn-primary"),
          tags$hr(),
          checkboxGroupInput(
            "cons_families",
            "Families",
            choices = stats::setNames(names(family_labels), family_labels),
            selected = c("ScoresTrees", "ScoresLateReg", "Billy", "BillyTrees")
          ),
          checkboxGroupInput("cons_seasons", "Backtest seasons", choices = backtest_seasons, selected = backtest_seasons),
          checkboxGroupInput("cons_future_seasons", "Future projection seasons", choices = future_seasons, selected = future_seasons),
          selectInput(
            "cons_market",
            "Market",
            choices = c("Spread" = "spread", "Straight up" = "straight_up", "Total" = "total", "Home implied" = "home_implied", "Away implied" = "away_implied"),
            selected = "spread"
          ),
          selectInput(
            "cons_line_source",
            "Backtest line source",
            choices = c("Closing lines" = "closing", "Early lines" = "early"),
            selected = "closing"
          ),
          selectInput(
            "cons_injury_source",
            "Projection injury adjustment",
            choices = c("No injury adjustment" = "none", "Apply injury adjustments" = "apply"),
            selected = "apply"
          ),
          checkboxInput(
            "cons_apply_amortization",
            "Apply 2026 offseason amortization",
            value = TRUE
          ),
          helpText("2026 source: Forecast Team Off Def Injuries. With both adjustments enabled, spread uses home F minus away G; home and away scores use H and I, and the total uses H plus I."),
          actionButton("cons_view_adjustments", "View 2026 adjustments", class = "btn-default"),
          sliderInput("cons_agree", "Minimum agreement", min = 50, max = 100, value = 60, step = 5, post = "%"),
          sliderInput("cons_min_win", "Minimum model win rate", min = 40, max = 60, value = 40, step = 1, post = "%"),
          actionButton("cons_run", "Build consensus", class = "btn-primary"),
          tags$hr(),
          downloadButton("cons_download", "Download consensus rows")
        ),
        mainPanel(
          h4("Legacy consensus status"),
          verbatimTextOutput("cons_status", placeholder = TRUE),
          tags$hr(),
          h4("Legacy consensus summary"),
          tableOutput("cons_summary"),
          tags$hr(),
          h4("Legacy consensus results splits"),
          tableOutput("cons_splits"),
          tags$hr(),
          div(
            class = "section-title-row",
            h4("Legacy consensus game-level rows"),
            downloadButton("cons_games_download", "Download game-level CSV")
          ),
          DTOutput("cons_games")
        )
      )
    ),
    tabPanel(
      "Next-Gen Consensus",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          actionButton("ng_cons_run_top", "Build next-gen consensus", class = "btn-primary"),
          tags$hr(),
          checkboxGroupInput(
            "ng_cons_frameworks",
            "Frameworks",
            choices = c("Early framework" = "early", "Late framework" = "late"),
            selected = c("early", "late")
          ),
          checkboxGroupInput(
            "ng_cons_families",
            "Families",
            choices = stats::setNames(names(next_gen_family_labels), next_gen_family_labels),
            selected = names(next_gen_family_labels)
          ),
          checkboxGroupInput(
            "ng_cons_projection_sources",
            "Spread/total projection sources",
            choices = c("Direct model projections" = "direct", "Implied from team scores" = "implied_team_scores"),
            selected = c("direct", "implied_team_scores")
          ),
          checkboxGroupInput("ng_cons_seasons", "Backtest seasons", choices = c(2024, 2025), selected = c(2024, 2025)),
          checkboxGroupInput("ng_cons_future_seasons", "Future projection seasons", choices = 2026, selected = 2026),
          selectInput(
            "ng_cons_market",
            "Market",
            choices = c("Spread" = "spread", "Straight up" = "straight_up", "Total" = "total", "Home implied" = "home_implied", "Away implied" = "away_implied"),
            selected = "spread"
          ),
          selectInput(
            "ng_cons_line_source",
            "Backtest line source",
            choices = c("Closing lines" = "closing", "Early lines" = "early"),
            selected = "closing"
          ),
          selectInput(
            "ng_cons_injury_source",
            "Projection injury adjustment",
            choices = c("No injury adjustment" = "none", "Apply injury adjustments" = "apply"),
            selected = "apply"
          ),
          checkboxInput(
            "ng_cons_apply_amortization",
            "Apply 2026 offseason amortization",
            value = TRUE
          ),
          helpText("2026 source: Forecast Team Off Def Injuries. With both adjustments enabled, spread uses home F minus away G; home and away scores use H and I, and the total uses H plus I."),
          actionButton("ng_cons_view_adjustments", "View 2026 adjustments", class = "btn-default"),
          sliderInput("ng_cons_agree", "Minimum agreement", min = 50, max = 100, value = 60, step = 5, post = "%"),
          sliderInput("ng_cons_min_win", "Minimum model win rate", min = 40, max = 60, value = 40, step = 1, post = "%"),
          actionButton("ng_cons_run", "Build next-gen consensus", class = "btn-primary"),
          tags$hr(),
          downloadButton("ng_cons_download", "Download next-gen consensus rows")
        ),
        mainPanel(
          h4("Next-gen consensus status"),
          verbatimTextOutput("ng_cons_status", placeholder = TRUE),
          tags$hr(),
          h4("Next-gen consensus summary"),
          tableOutput("ng_cons_summary"),
          tags$hr(),
          h4("Next-gen consensus results splits"),
          tableOutput("ng_cons_splits"),
          tags$hr(),
          div(
            class = "section-title-row",
            h4("Next-gen consensus game-level rows"),
            downloadButton("ng_cons_games_download", "Download game-level CSV")
          ),
          DTOutput("ng_cons_games")
        )
      )
    ),
    tabPanel(
      "Consensus",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          checkboxGroupInput(
            "overall_cons_sources",
            "Consensus sources",
            choices = c("Legacy consensus" = "legacy", "Next-gen consensus" = "next_gen"),
            selected = c("legacy", "next_gen")
          ),
          checkboxInput(
            "overall_cons_require_all_sources",
            "Require every selected source to qualify",
            value = TRUE
          ),
          selectInput(
            "overall_cons_market",
            "Market",
            choices = c("Spread" = "spread", "Straight up" = "straight_up", "Total" = "total", "Home implied" = "home_implied", "Away implied" = "away_implied"),
            selected = "spread"
          ),
          sliderInput("overall_cons_agree", "Minimum agreement", min = 50, max = 100, value = 60, step = 5, post = "%"),
          sliderInput("overall_cons_min_edge", "Minimum edge", min = 0, max = 10, value = 2, step = 0.5, post = " pts"),
          actionButton("overall_cons_run", "Build combined consensus", class = "btn-primary")
        ),
        mainPanel(
          h4("Combined consensus status"),
          verbatimTextOutput("overall_cons_status", placeholder = TRUE),
          tags$hr(),
          h4("Combined consensus summary"),
          tableOutput("overall_cons_summary"),
          tags$hr(),
          h4("Combined consensus results splits"),
          tableOutput("overall_cons_splits"),
          tags$hr(),
          h4("Combined consensus game-level rows"),
          DTOutput("overall_cons_games")
        )
      )
    ),
    tabPanel(
      "Dashboard",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput(
            "dashboard_source",
            "Consensus source",
            choices = c("Legacy consensus" = "legacy", "Next-gen consensus" = "next_gen", "Combined consensus" = "combined"),
            selected = "combined"
          ),
          selectInput("dashboard_season", "Season", choices = character()),
          selectInput("dashboard_week", "Week", choices = character()),
          checkboxGroupInput(
            "dashboard_markets",
            "Markets",
            choices = c("Against the spread" = "spread", "Straight up" = "straight_up", "Over / under" = "total", "Home implied" = "home_implied", "Away implied" = "away_implied"),
            selected = c("spread", "straight_up", "total", "home_implied", "away_implied")
          ),
          checkboxInput("dashboard_no_minus_3_5_favorites", "No -3.5 favorites", value = TRUE),
          checkboxInput("dashboard_no_plus_2_5_underdogs", "No +2.5 underdogs", value = TRUE),
          checkboxInput("dashboard_no_early_week_games", "No early-week games", value = FALSE),
          tags$hr(),
          h4("Downloads"),
          downloadButton("dashboard_download", "Download selected markets", class = "btn-success btn-block"),
          downloadButton("dashboard_download_spread", "Download against the spread", class = "btn-default btn-block"),
          downloadButton("dashboard_download_straight_up", "Download straight up", class = "btn-default btn-block"),
          downloadButton("dashboard_download_total", "Download total", class = "btn-default btn-block"),
          downloadButton("dashboard_download_home_implied", "Download home implied", class = "btn-default btn-block"),
          downloadButton("dashboard_download_away_implied", "Download away implied", class = "btn-default btn-block"),
          helpText("Each CSV uses the selected consensus source, season, and week. A single-market filename ends with that market; a combined selection ends with multiple_markets.")
        ),
        mainPanel(
          h4("Weekly dashboard"),
          tableOutput("dashboard_summary"),
          tags$hr(),
          h4("Against the spread"),
          DTOutput("dashboard_spread"),
          tags$hr(),
          h4("Straight up"),
          DTOutput("dashboard_straight_up"),
          tags$hr(),
          h4("Over / under"),
          DTOutput("dashboard_total"),
          tags$hr(),
          h4("Home implied"),
          DTOutput("dashboard_home_implied"),
          tags$hr(),
          h4("Away implied"),
          DTOutput("dashboard_away_implied")
        )
      )
    ),
    tabPanel(
      "Circa Dashboard",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          fluidRow(
            column(6, numericInput("circa_season", "Season", value = circa_active_season, min = 2024, max = 2100, step = 1)),
            column(6, numericInput("circa_week", "Week", value = circa_active_week, min = 1, max = 22, step = 1))
          ),
          actionButton("circa_refresh_web", "Refresh from Circa Sports", class = "btn-primary btn-block"),
          tags$a(
            "Open official Circa Million page",
            href = "https://www.circasports.com/circa-million",
            target = "_blank",
            rel = "noopener noreferrer"
          ),
          tags$hr(),
          h5("Manual fallback"),
          fileInput(
            "circa_lines_file",
            "Upload weekly Circa lines",
            accept = c(".pdf", ".csv")
          ),
          actionButton("circa_use_default", "Use bundled selected-week lines", class = "btn-default btn-block"),
          helpText("The Circa week follows the Dashboard week. The app checks Circa's official PDF when the selected week changes; Refresh retries, and upload remains a fallback."),
          tags$hr(),
          selectInput(
            "circa_source",
            "Consensus source",
            choices = c("Legacy consensus" = "legacy", "Next-gen consensus" = "next_gen", "Combined consensus" = "combined"),
            selected = "combined"
          ),
          sliderInput("circa_min_edge", "Minimum Circa edge", min = 0, max = 10, value = 0, step = 0.5, post = " pts"),
          checkboxInput("circa_no_minus_3_5_favorites", "No -3.5 favorites", value = TRUE),
          checkboxInput("circa_no_plus_2_5_underdogs", "No +2.5 underdogs", value = TRUE),
          checkboxInput("circa_no_early_week_games", "No early-week games", value = TRUE),
          selectizeInput("circa_skip_matchups", "Skip matchups", choices = NULL, multiple = TRUE),
          actionButton("circa_build", "Build Circa dashboard", class = "btn-primary btn-block"),
          tags$hr(),
          downloadButton("circa_download_top_five", "Download top five", class = "btn-success btn-block"),
          downloadButton("circa_download_all", "Download all Circa plays", class = "btn-default btn-block")
        ),
        mainPanel(
          h4("Circa Sports Million VIII"),
          p("Official contest spreads are fixed for the week. The dashboard reapplies the active consensus projections to those stale lines."),
          verbatimTextOutput("circa_status", placeholder = TRUE),
          tags$hr(),
          h4("Top five shadow card"),
          DTOutput("circa_top_five"),
          h4("Next two alternates"),
          DTOutput("circa_alternates"),
          tags$hr(),
          h4("Final five-pick card"),
          checkboxGroupInput("circa_final_picks", "Select five plays", choices = NULL),
          textOutput("circa_final_status"),
          DTOutput("circa_final_card"),
          downloadButton("circa_download_final", "Download final card", class = "btn-success"),
          tags$hr(),
          h4("All potential Circa plays"),
          DTOutput("circa_all_plays"),
          tags$hr(),
          h4("Loaded contest lines"),
          DTOutput("circa_lines_preview")
        )
      )
    ),
    tabPanel(
      "Files",
      h4("Loaded app data files"),
      tableOutput("file_table")
    )
  )
)

server <- function(input, output, session) {
  show_forecast_adjustments <- function() {
    showModal(modalDialog(
      title = "2026 forecast adjustments",
      DTOutput("forecast_adjustments_preview"),
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        downloadButton("forecast_adjustments_download", "Download CSV"),
        modalButton("Close")
      )
    ))
  }

  observeEvent(input$cons_view_adjustments, show_forecast_adjustments(), ignoreInit = TRUE)
  observeEvent(input$ng_cons_view_adjustments, show_forecast_adjustments(), ignoreInit = TRUE)

  output$forecast_adjustments_preview <- renderDT({
    if (nrow(forecast_team_adjustments_2026_raw) == 0) {
      return(datatable(tibble(Message = "The deployed 2026 adjustment source is empty."), rownames = FALSE))
    }
    datatable(
      forecast_team_adjustments_2026_raw,
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 16, lengthMenu = c(16, 32, 64, 272), scrollX = TRUE)
    )
  })

  output$forecast_adjustments_download <- downloadHandler(
    filename = function() paste0("forecast_team_off_def_injuries_2026_", Sys.Date(), ".csv"),
    content = function(file) write_csv(forecast_team_adjustments_2026_raw, file, na = "")
  )

  bind_family <- function(id, family_key) {
    family_key_norm <- norm_key(family_key)
    target_label_norm <- norm_key(family_labels[[family_key]] %||% family_key)

    family_files_for_season <- reactive({
      req(input[[paste0(id, "_season")]])
      selected_season <- input[[paste0(id, "_season")]]
      inventory %>%
        mutate(
          family_norm = norm_key(family),
          family_label_norm = norm_key(family_label),
          file_norm = norm_key(file)
        ) %>%
        filter(
          if (identical(selected_season, "all")) season %in% backtest_seasons else season == as.integer(selected_season),
          family_norm == family_key_norm |
            family_label_norm == target_label_norm |
            str_detect(file_norm, fixed(family_key_norm))
        ) %>%
        select(path, file, season, family, family_label, split)
    })

    family_future_files <- reactive({
      req(input[[paste0(id, "_future_season")]])
      selected_season <- input[[paste0(id, "_future_season")]]
      if (identical(selected_season, "none") || length(future_seasons) == 0) {
        return(tibble(path = character(), file = character(), season = integer(), family = character(), family_label = character(), split = character()))
      }
      inventory %>%
        mutate(
          family_norm = norm_key(family),
          family_label_norm = norm_key(family_label),
          file_norm = norm_key(file)
        ) %>%
        filter(
          if (identical(selected_season, "all")) season %in% future_seasons else season == as.integer(selected_season),
          family_norm == family_key_norm |
            family_label_norm == target_label_norm |
            str_detect(file_norm, fixed(family_key_norm))
        ) %>%
        select(path, file, season, family, family_label, split)
    })

    family_summary <- reactive({
      req(input[[paste0(id, "_build")]] > 0)
      market <- input[[paste0(id, "_market")]] %||% "all"
      files <- family_files_for_season()
      if (nrow(files) == 0) return(tibble())

      summary_rows <- pmap_dfr(files, function(path, file, season, family, family_label, split) {
        out <- detect_cover_summary(read_model_file(path), split)
        if (nrow(out) == 0) return(tibble())
        if (!identical(market, "all")) out <- filter(out, market == !!market)
        if (nrow(out) == 0) return(tibble())
        mutate(out, Season = season, File = file, Split = split, .before = 1)
      })

      if (nrow(summary_rows) == 0) return(tibble())

      # Legacy spread and total predictions are game-level signals copied into
      # both the home and away exports. Away spreads become exact copies after
      # normalization to the home perspective. Keep every CSV for spreadsheet
      # compatibility, but show each independent signal only once in the app.
      # The two XGBoost tree families also export ImpliedScoreDiff_xgb as an
      # exact alias of ScoreDiff_xgb_score.
      summary_rows <- summary_rows %>%
        mutate(
          .signal_key = case_when(
            market == "spread" & projection_col == "ImpliedScoreDiff_xgb" ~ "ScoreDiff_xgb_score",
            market %in% c("spread", "total") ~ projection_col,
            TRUE ~ paste(File, projection_col, sep = "::")
          ),
          .home_first = !str_detect(Split, "^home")
        ) %>%
        arrange(.home_first) %>%
        group_by(Season, market, .signal_key) %>%
        slice_head(n = 1) %>%
        ungroup() %>%
        select(-.signal_key, -.home_first)

      summary_rows <- mutate(summary_rows, WinPct = round(100 * win_pct, 1))
      summary_rows <- select(
        summary_rows,
        Season,
        File,
        Split,
        Market = market_label,
        Result = result_col,
        Projection = projection_col,
        Picks = picks,
        Wins = wins,
        Losses = losses,
        WinPct
      )
      arrange(summary_rows, desc(WinPct), desc(Picks), File, Result)
    })

    output[[paste0(id, "_status")]] <- renderText({
      req(input[[paste0(id, "_build")]] > 0)
      s <- family_summary()
      f <- family_files_for_season()
      fd <- file_debug_summary(f)
      paste(
        paste("Family:", family_key),
        paste("Season:", input[[paste0(id, "_season")]]),
        paste("Market:", input[[paste0(id, "_market")]]),
        paste("Files included:", nrow(f)),
        paste("Summary rows:", nrow(s)),
        paste("Available families:", paste(sort(unique(inventory$family)), collapse = ", ")),
        paste("Available seasons:", paste(sort(unique(inventory$season)), collapse = ", ")),
        "File debug:",
        paste(capture.output(print(fd, n = 20, width = 180)), collapse = "\n"),
        sep = "\n"
      )
    })

    output[[paste0(id, "_summary")]] <- renderPrint({
      if (is.null(input[[paste0(id, "_build")]]) || input[[paste0(id, "_build")]] <= 0) {
        return(invisible(NULL))
      }
      tryCatch({
        s <- family_summary() %>% dplyr::slice_head(n = 80)
        if (nrow(s) == 0) {
          fd <- file_debug_summary(family_files_for_season())
          cat("No summary rows for this selection.\n")
          cat("File debug:\n")
          print(as.data.frame(fd), row.names = FALSE)
          return(invisible(NULL))
        }
        print(as.data.frame(s), row.names = FALSE)
      }, error = function(err) {
        cat("Summary render error:\n")
        cat(conditionMessage(err), "\n")
        cat("\nFile debug:\n")
        print(as.data.frame(file_debug_summary(family_files_for_season())), row.names = FALSE)
      })
    })

    output[[paste0(id, "_download_summary")]] <- downloadHandler(
      filename = function() paste0(family_key, "_summary_", input[[paste0(id, "_season")]], "_", Sys.Date(), ".csv"),
      content = function(file) write_csv(family_summary(), file)
    )

    output[[paste0(id, "_future_preview")]] <- renderDT({
      req(input[[paste0(id, "_build")]] > 0)
      files <- family_future_files()
      if (nrow(files) == 0) {
        return(datatable(tibble(Message = "No future projection files for this family/season selection."), rownames = FALSE))
      }
      market <- input[[paste0(id, "_market")]] %||% "all"
      rows <- pmap_dfr(files, function(path, file, season, family, family_label, split) {
        df <- read_model_file(path)
        cols <- if (identical(market, "all")) prediction_cols(df) else projection_columns_for_market(df, market)
        cols <- cols[cols %in% names(df)]
        cols <- cols[vapply(df[cols], function(x) any(!is.na(x)), logical(1))]
        if (length(cols) == 0) return(tibble())
        keep <- unique(c("game_id", "season", "week", "home_team", "away_team", "spread_line", "total_line", cols))
        keep <- keep[keep %in% names(df)]
        df %>%
          select(all_of(keep)) %>%
          mutate(File = file, Split = split, .before = 1)
      })
      if (nrow(rows) == 0) {
        return(datatable(tibble(Message = "No future projection columns for this selection."), rownames = FALSE))
      }
      rows <- rows %>%
        mutate(
          season = as.integer(season),
          week = as.integer(week)
        )
      decimal_cols <- setdiff(
        names(rows)[vapply(rows, is.numeric, logical(1))],
        c("season", "week")
      )
      rows <- rows %>%
        mutate(across(all_of(decimal_cols), ~ ifelse(is.na(.x), NA_character_, sprintf("%.1f", .x))))
      dt <- datatable(
        rows,
        rownames = FALSE,
        filter = "top",
        options = list(
          dom = '<"top"lfrip>t<"bottom"lfrip>',
          pageLength = 25,
          lengthMenu = c(10, 25, 50, 100),
          scrollX = TRUE
        )
      )
      dt
    })
  }

  bind_family("scorestrees", "ScoresTrees")
  bind_family("billytrees", "BillyTrees")
  bind_family("scoreslatereg", "ScoresLateReg")
  bind_family("billy", "Billy")

  consensus_status <- reactiveVal("Ready.")
  nextgen_consensus_status <- reactiveVal(if (nextgen_data_available) "Ready." else "Next-gen prepared data is missing.")
  overall_consensus_status <- reactiveVal("Build legacy and/or next-gen consensus first.")
  nextgen_consensus_rows <- reactiveVal(tibble())
  overall_consensus_rows <- reactiveVal(tibble())
  circa_lines_state <- reactiveVal(circa_default_lines)
  circa_results_state <- reactiveVal(tibble())
  circa_source_url_state <- reactiveVal(if (nrow(circa_default_lines) > 0) paste("Bundled verified Week", circa_active_week, "copy") else "")
  circa_status_state <- reactiveVal(
    if (nrow(circa_default_lines) > 0) {
      paste0("Bundled Week ", circa_active_week, " contest sheet loaded: ", nrow(circa_default_lines), " matchups. Click Build Circa dashboard.")
    } else {
      paste0("Checking Circa Sports for the Week ", circa_active_week, " contest sheet.")
    }
  )

  observeEvent(circa_lines_state(), {
    lines <- circa_lines_state()
    if (nrow(lines) == 0) {
      updateSelectizeInput(session, "circa_skip_matchups", choices = character(), selected = character())
      return()
    }
    choices <- setNames(lines$game_id, paste(lines$away_team, "@", lines$home_team))
    selected <- intersect(input$circa_skip_matchups %||% character(), lines$game_id)
    updateSelectizeInput(session, "circa_skip_matchups", choices = choices, selected = selected)
  }, ignoreInit = FALSE)

  observe({
    session$sendCustomMessage("toggleMinWinSlider", identical(input$cons_market, "straight_up"))
  })

  output$file_table <- renderTable({
    legacy_files <- inventory %>%
      transmute(Source = "Legacy", Framework = NA_character_, Season = season, Family = family_label, Split = split, File = file)
    nextgen_files <- nextgen_inventory %>%
      transmute(Source = "Next-gen", Framework = framework, Season = season, Family = family_label, Split = sample, File = file)
    bind_rows(legacy_files, nextgen_files)
  })

  get_line <- function(df, col) if (col %in% names(df)) suppressWarnings(as.numeric(df[[col]])) else rep(NA_real_, nrow(df))

  first_non_na <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA_real_ else x[[1]]
  }

  summarize_consensus_table <- function(df) {
    if (nrow(df) == 0) return(tibble(Message = "No consensus rows for the current selections."))
    summarize_consensus <- function(data, label) {
      tibble(
        Season = label,
        Games = nrow(data),
        `Avg projections per game` = mean(data$projections),
        `Avg models per game` = mean(data$models_used),
        `Avg agreement` = mean(data$agree_pct),
        Wins = sum(data$correct %in% TRUE, na.rm = TRUE),
        Losses = sum(data$correct %in% FALSE, na.rm = TRUE),
        Pushes = sum(is.na(data$correct)),
        `Win %` = ifelse(sum(!is.na(data$correct)) > 0, sum(data$correct %in% TRUE, na.rm = TRUE) / sum(!is.na(data$correct)), NA_real_)
      )
    }
    bind_rows(
      summarize_consensus(df, "Total"),
      df %>%
        group_split(season) %>%
        map_dfr(~ summarize_consensus(.x, as.character(first(.x$season))))
    ) %>%
      mutate(
        `Avg projections per game` = sprintf("%.0f", `Avg projections per game`),
        `Avg models per game` = sprintf("%.0f", `Avg models per game`),
        `Avg agreement` = ifelse(is.na(`Avg agreement`), NA_character_, paste0(sprintf("%.1f", 100 * `Avg agreement`), "%")),
        `Win %` = ifelse(is.na(`Win %`), NA_character_, paste0(sprintf("%.1f", 100 * `Win %`), "%"))
      )
  }

  consensus_splits_table <- function(df, market) {
    if (nrow(df) == 0) return(tibble(Message = "No consensus rows for the current selections."))

    summarize_split <- function(data, category, split_label) {
      graded <- sum(!is.na(data$correct))
      tibble(
        Category = category,
        Split = split_label,
        Games = nrow(data),
        Wins = sum(data$correct %in% TRUE, na.rm = TRUE),
        Losses = sum(data$correct %in% FALSE, na.rm = TRUE),
        Pushes = sum(is.na(data$correct)),
        `Win %` = ifelse(graded > 0, sum(data$correct %in% TRUE, na.rm = TRUE) / graded, NA_real_)
      )
    }

    classify_favorite_side <- function(spread_line) {
      case_when(
        spread_line > 0 ~ "Home",
        spread_line < 0 ~ "Away",
        spread_line == 0 ~ "Pick'em",
        TRUE ~ NA_character_
      )
    }

    rows <- df %>%
      mutate(
        favorite_side = classify_favorite_side(spread_line),
        selected_team_side = case_when(
          market %in% c("spread", "straight_up") ~ consensus_pick,
          market == "home_implied" ~ "Home",
          market == "away_implied" ~ "Away",
          TRUE ~ NA_character_
        ),
        favorite_role = case_when(
          is.na(selected_team_side) | is.na(favorite_side) ~ NA_character_,
          favorite_side == "Pick'em" ~ "Pick'em",
          selected_team_side == favorite_side ~ "Favorite",
          selected_team_side %in% c("Home", "Away") ~ "Underdog",
          TRUE ~ NA_character_
        ),
        total_favorite_context = case_when(
          favorite_side == "Home" ~ "Home favorite games",
          favorite_side == "Away" ~ "Away favorite games",
          favorite_side == "Pick'em" ~ "Pick'em games",
          TRUE ~ NA_character_
        )
      )

    overall <- summarize_split(rows, "Overall", "All consensus rows")
    pick_splits <- rows %>%
      filter(!is.na(consensus_pick)) %>%
      group_split(consensus_pick) %>%
      map_dfr(~ summarize_split(.x, "Consensus pick", first(.x$consensus_pick)))

    home_away_splits <- if (market %in% c("spread", "straight_up", "home_implied", "away_implied")) {
      rows %>%
        filter(!is.na(selected_team_side)) %>%
        group_split(selected_team_side) %>%
        map_dfr(~ summarize_split(.x, "Home/Away", first(.x$selected_team_side)))
    } else {
      rows %>%
        filter(!is.na(total_favorite_context)) %>%
        group_split(total_favorite_context) %>%
        map_dfr(~ summarize_split(.x, "Favorite context", first(.x$total_favorite_context)))
    }

    favorite_splits <- if (market == "total") {
      tibble()
    } else {
      rows %>%
        filter(!is.na(favorite_role)) %>%
        group_split(favorite_role) %>%
        map_dfr(~ summarize_split(.x, "Favorite/Underdog", first(.x$favorite_role)))
    }

    bind_rows(overall, pick_splits, home_away_splits, favorite_splits) %>%
      mutate(`Win %` = ifelse(is.na(`Win %`), NA_character_, paste0(sprintf("%.1f", 100 * `Win %`), "%")))
  }

  projection_columns_for_market <- function(df, market) {
    cols <- prediction_cols(df)
    cols <- cols[!str_detect(cols, "^Cover_")]
    if (market == "spread") {
      cols[str_detect(cols, "ScoreDiff|ImpliedScoreDiff|Billy")]
    } else if (market == "straight_up") {
      cols[str_detect(cols, "ScoreDiff|ImpliedScoreDiff|Billy")]
    } else if (market == "total") {
      cols[str_detect(cols, "Total|ScoreTotal|TotalScore")]
    } else if (market == "home_implied" || market == "away_implied") {
      cols[str_detect(cols, "Score_|Score$|OppScore|ImpliedTeamScored|ImpliedOppScored|Score_final")]
    } else {
      character()
    }
  }

  read_nextgen_model_file <- function(path) {
    nm <- basename(path)
    if (nm %in% names(nextgen_compact_models)) return(apply_current_lines(nextgen_compact_models[[nm]], preserve_completed = TRUE))
    stop("Next-gen compact prepared data is missing for ", nm, ". Run prepare_data_nextgen.R and deploy data/nextgen_compact_models.rds plus data/nextgen_model_inventory.rds.")
  }

  nextgen_prediction_cols <- function(df) {
    family_suffix <- paste0("_(", paste(names(next_gen_family_labels), collapse = "|"), ")$")
    names(df)[
      str_detect(
        names(df),
        regex("^Score_|^HomeScore_|^AwayScore_|^OppScore_|^ScoreTotal_|^TotalScore_|^ScoreDiff_|^HomeMargin_", TRUE)
      ) &
        str_detect(names(df), regex(family_suffix, TRUE)) &
        !str_detect(names(df), regex("^Cover_|_target|_cover$|_pm1$", TRUE))
    ]
  }

  dedupe_exact_prediction_cols <- function(df, cols, tolerance = 1e-12) {
    kept <- character()
    for (col in cols) {
      candidate <- suppressWarnings(as.numeric(df[[col]]))
      is_duplicate <- any(vapply(kept, function(existing) {
        prior <- suppressWarnings(as.numeric(df[[existing]]))
        if (length(candidate) != length(prior) || !identical(is.na(candidate), is.na(prior))) return(FALSE)
        complete <- !is.na(candidate)
        !any(complete) || max(abs(candidate[complete] - prior[complete])) <= tolerance
      }, logical(1)))
      if (!is_duplicate) kept <- c(kept, col)
    }
    kept
  }

  nextgen_projection_source <- function(cols) {
    ifelse(
      str_detect(cols, regex("ImpliedFromTeamScores", TRUE)),
      "Implied from team scores",
      "Direct model projection"
    )
  }

  nextgen_filter_projection_sources <- function(cols, market, projection_sources) {
    projection_sources <- projection_sources %||% c("direct", "implied_team_scores")
    if (!market %in% c("spread", "straight_up", "total")) return(cols)
    if (length(projection_sources) == 0) return(character())

    is_implied <- str_detect(cols, regex("ImpliedFromTeamScores", TRUE))
    keep <- rep(FALSE, length(cols))
    if ("direct" %in% projection_sources) keep <- keep | !is_implied
    if ("implied_team_scores" %in% projection_sources) keep <- keep | is_implied
    cols[keep]
  }

  nextgen_projection_columns_for_market <- function(df, market, projection_sources = c("direct", "implied_team_scores")) {
    cols <- nextgen_prediction_cols(df)
    market_cols <- if (market == "spread" || market == "straight_up") {
      cols[str_detect(cols, regex("^ScoreDiff_|^HomeMargin_", TRUE))]
    } else if (market == "total") {
      cols[str_detect(cols, regex("^ScoreTotal_|^TotalScore_", TRUE))]
    } else if (market == "home_implied") {
      cols[str_detect(cols, regex("^Score_|^HomeScore_", TRUE))]
    } else if (market == "away_implied") {
      cols[str_detect(cols, regex("^OppScore_|^AwayScore_", TRUE))]
    } else {
      character()
    }
    market_cols <- nextgen_filter_projection_sources(market_cols, market, projection_sources)
    dedupe_exact_prediction_cols(df, market_cols)
  }

  nextgen_cover_market <- function(col) {
    case_when(
      str_detect(col, "ScoreDiff|HomeMargin") ~ "spread",
      str_detect(col, "Total") ~ "total",
      str_detect(col, "^Cover_Score_") ~ "home_implied",
      str_detect(col, "^Cover_OppScore_") ~ "away_implied",
      TRUE ~ "all"
    )
  }

  nextgen_projection_candidates_for_cover <- function(cover_col) {
    base <- str_remove(cover_col, "^Cover_")
    c(
      base,
      str_replace(base, "^Total_", "ScoreTotal_"),
      str_replace(base, "^Total_", "TotalScore_")
    ) %>% unique()
  }

  detect_nextgen_cover_summary <- function(df) {
    cover_cols <- grep("^Cover_", names(df), value = TRUE, ignore.case = TRUE)
    if (length(cover_cols) == 0) return(tibble())
    out <- purrr::map_dfr(cover_cols, function(col) {
      vals <- suppressWarnings(as.numeric(df[[col]]))
      proj <- nextgen_projection_candidates_for_cover(col)
      proj <- proj[proj %in% names(df)][1] %||% NA_character_
      tibble(
        market = nextgen_cover_market(col),
        result_col = col,
        projection_col = proj,
        projection_source = nextgen_projection_source(proj),
        picks = sum(!is.na(vals)),
        wins = sum(vals == 1, na.rm = TRUE),
        losses = sum(vals == 0, na.rm = TRUE),
        win_pct = ifelse(picks > 0, wins / picks, NA_real_)
      )
    })
    if (nrow(out) == 0) return(out)

    # If direct and team-score-derived projections are numerically identical,
    # keep the first canonical result instead of reporting/scoring it twice.
    out %>%
      group_by(market) %>%
      group_modify(~ {
        candidates <- unique(stats::na.omit(.x$projection_col))
        keep <- dedupe_exact_prediction_cols(df, candidates)
        filter(.x, is.na(projection_col) | projection_col %in% keep)
      }) %>%
      ungroup()
  }

  historical_nextgen_model_scores <- function(market) {
    selected <- nextgen_inventory %>% filter(season %in% c(2024, 2025))
    rows <- pmap_dfr(selected, function(path, file, season, framework, sample, family, family_label) {
      detect_nextgen_cover_summary(read_nextgen_model_file(path)) %>%
        filter(!is.na(projection_col)) %>%
        mutate(
          framework = framework,
          family = family,
          family_label = family_label,
          file = file,
          season = season,
          .before = 1
        )
    })
    if (nrow(rows) == 0) return(tibble())
    score_market <- if (identical(market, "straight_up")) "spread" else market
    rows %>%
      filter(market == score_market) %>%
      group_by(framework, family, family_label, market, projection_col) %>%
      summarise(
        model_result_col = first(result_col),
        model_picks = sum(picks, na.rm = TRUE),
        model_wins = sum(wins, na.rm = TRUE),
        model_losses = sum(losses, na.rm = TRUE),
        model_win_pct = ifelse(model_picks > 0, model_wins / model_picks, NA_real_),
        .groups = "drop"
      )
  }

  nextgen_line_for_market <- function(base, market) {
    if (market == "spread") {
      ifelse(!is.na(base$spread_line), base$spread_line, NA_real_)
    } else if (market == "straight_up") {
      rep(0, nrow(base))
    } else if (market == "total") {
      ifelse(!is.na(base$total_line), base$total_line, NA_real_)
    } else if (market == "home_implied") {
      coalesce(ifelse(!is.na(base$total_line) & !is.na(base$spread_line), (base$total_line + base$spread_line) / 2, NA_real_), base$home_implied, base$line)
    } else {
      coalesce(ifelse(!is.na(base$total_line) & !is.na(base$spread_line), (base$total_line - base$spread_line) / 2, NA_real_), base$away_implied, base$line)
    }
  }

  long_nextgen_predictions_for_file <- function(meta, market, min_win_pct = 0.40, line_source = "closing", injury_source = "apply", apply_amortization = TRUE, projection_sources = c("direct", "implied_team_scores")) {
    df <- read_nextgen_model_file(meta$path)
    cols <- nextgen_projection_columns_for_market(df, market, projection_sources)
    if (length(cols) == 0) return(tibble())

    score_market <- if (identical(market, "straight_up")) "spread" else market
    use_historical_score_lookup <- !identical(meta$sample, "test") || suppressWarnings(as.integer(meta$season)) >= 2026L
    if (isTRUE(use_historical_score_lookup)) {
      score_lookup <- historical_nextgen_model_scores(market) %>%
        filter(framework == meta$framework, family == meta$family) %>%
        select(projection_col, model_result_col, model_picks, model_win_pct)
    } else {
      score_lookup <- detect_nextgen_cover_summary(df) %>%
        filter(market == score_market, !is.na(projection_col)) %>%
        select(projection_col, model_result_col = result_col, model_picks = picks, model_win_pct = win_pct)
    }
    if ((nrow(score_lookup) == 0 || all(is.na(score_lookup$model_win_pct))) &&
        market == "home_implied" &&
        all(c("actual_market_result", "predicted_market_result") %in% names(df))) {
      actual_market_result <- suppressWarnings(as.numeric(df[["actual_market_result"]]))
      predicted_market_result <- suppressWarnings(as.numeric(df[["predicted_market_result"]]))
      actual_push <- if ("actual_push" %in% names(df)) as.logical(df[["actual_push"]]) else rep(FALSE, nrow(df))
      scored <- !is.na(actual_market_result) & !is.na(predicted_market_result) & !actual_push
      if (any(scored)) {
        score_lookup <- tibble(
          projection_col = cols,
          model_result_col = "predicted_market_result",
          model_picks = sum(scored),
          model_win_pct = mean(predicted_market_result[scored] == actual_market_result[scored])
        )
      }
    }
    if (nrow(score_lookup) == 0 || all(is.na(score_lookup$model_win_pct))) {
      score_lookup <- historical_nextgen_model_scores(market) %>%
        filter(framework == meta$framework, family == meta$family) %>%
        select(projection_col, model_result_col, model_picks, model_win_pct)
    }

    base <- df %>%
      transmute(
        game_id = .data[["game_id"]],
        season = .data[["season"]],
        week = .data[["week"]],
        home_team = .data[["home_team"]],
        away_team = .data[["away_team"]],
        home_score = get_line(df, "home_score"),
        away_score = get_line(df, "away_score"),
        spread_line = get_line(df, "spread_line"),
        total_line = get_line(df, "total_line"),
        home_implied = get_line(df, "home_implied"),
        away_implied = get_line(df, "away_implied"),
        line = get_line(df, "line"),
        actual_market_result = get_line(df, "actual_market_result"),
        actual_push = if ("actual_push" %in% names(df)) as.logical(df[["actual_push"]]) else rep(FALSE, nrow(df))
      )
    if (identical(line_source, "early") && nrow(early_lines) > 0) {
      base <- base %>%
        left_join(early_lines, by = "game_id") %>%
        mutate(
          spread_line = coalesce(early_spread_line, spread_line),
          total_line = coalesce(early_total_line, total_line)
        ) %>%
        select(-early_spread_line, -early_total_line)
    }
    base <- apply_projection_adjustments(base, injury_source, apply_amortization)

    out <- map_dfr(cols, function(col) {
      model_score <- score_lookup %>% filter(projection_col == col) %>% dplyr::slice_head(n = 1)
      if (nrow(model_score) == 0) {
        model_result_col <- NA_character_
        model_picks <- NA_integer_
        model_win_pct <- NA_real_
      } else {
        model_result_col <- model_score$model_result_col
        model_picks <- model_score$model_picks
        model_win_pct <- model_score$model_win_pct
      }

      pred <- suppressWarnings(as.numeric(df[[col]]))
      if (market %in% c("spread", "straight_up")) {
        pred <- pred + base$spread_projection_adj
      } else if (market == "total") {
        pred <- pred + base$total_projection_adj
      } else if (market == "home_implied") {
        pred <- pred + base$home_score_projection_adj
      } else if (market == "away_implied") {
        pred <- pred + base$away_score_projection_adj
      }

      line <- nextgen_line_for_market(base, market)
      actual <- if (market %in% c("spread", "straight_up")) {
        base$home_score - base$away_score
      } else if (market == "total") {
        base$home_score + base$away_score
      } else if (market == "home_implied") {
        base$home_score
      } else {
        base$away_score
      }

      pick <- dplyr::case_when(pred > line ~ 1, pred < line ~ -1, TRUE ~ NA_real_)
      actual_side <- dplyr::case_when(
        !is.na(actual) & actual > line ~ 1,
        !is.na(actual) & actual < line ~ -1,
        is.na(actual) & !is.na(base$actual_market_result) & !base$actual_push & base$actual_market_result == 1 ~ 1,
        is.na(actual) & !is.na(base$actual_market_result) & !base$actual_push & base$actual_market_result == 0 ~ -1,
        TRUE ~ NA_real_
      )

      bind_cols(base %>% select(game_id, season, week, home_team, away_team, home_score, away_score, spread_line, total_line), tibble(
        framework = meta$framework,
        family = meta$family_label,
        file = meta$file,
        split = meta$sample,
        projection_col = col,
        projection_source = nextgen_projection_source(col),
        projection = pred,
        market_line = line,
        actual = actual,
        pick = pick,
        actual_side = actual_side,
        correct = ifelse(!is.na(pick) & !is.na(actual_side), pick == actual_side, NA),
        model_result_col = model_result_col,
        model_picks = model_picks,
        model_win_pct = model_win_pct
      ))
    })

    out %>%
      filter(!is.na(projection), !is.na(market_line)) %>%
      filter(!is.na(model_win_pct), model_win_pct >= min_win_pct)
  }

  bind_nextgen_family <- function(id, family_key) {
    nextgen_files_for_season <- reactive({
      req(input[[paste0(id, "_season")]])
      selected_season <- input[[paste0(id, "_season")]]
      frameworks <- input[[paste0(id, "_frameworks")]] %||% unique(nextgen_inventory$framework)
      nextgen_inventory %>%
        filter(
          framework %in% frameworks,
          family == family_key,
          sample == "test",
          if (identical(selected_season, "all")) season %in% nextgen_backtest_seasons else season == as.integer(selected_season)
        ) %>%
        select(path, file, season, framework, sample, family, family_label)
    })

    nextgen_future_files <- reactive({
      req(input[[paste0(id, "_future_season")]])
      selected_season <- input[[paste0(id, "_future_season")]]
      frameworks <- input[[paste0(id, "_frameworks")]] %||% unique(nextgen_inventory$framework)
      if (identical(selected_season, "none") || length(nextgen_future_seasons) == 0) {
        return(tibble(path = character(), file = character(), season = integer(), framework = character(), sample = character(), family = character(), family_label = character()))
      }
      nextgen_inventory %>%
        filter(
          framework %in% frameworks,
          family == family_key,
          sample == "val",
          if (identical(selected_season, "all")) season %in% nextgen_future_seasons else season == as.integer(selected_season)
        ) %>%
        select(path, file, season, framework, sample, family, family_label)
    })

    nextgen_family_summary <- reactive({
      req(input[[paste0(id, "_build")]] > 0)
      market <- input[[paste0(id, "_market")]] %||% "all"
      files <- nextgen_files_for_season()
      if (nrow(files) == 0) return(tibble())

      summary_rows <- pmap_dfr(files, function(path, file, season, framework, sample, family, family_label) {
        out <- detect_nextgen_cover_summary(read_nextgen_model_file(path))
        if (nrow(out) == 0) return(tibble())
        if (!identical(market, "all")) {
          score_market <- if (identical(market, "straight_up")) "spread" else market
          out <- filter(out, market == !!score_market)
        }
        if (nrow(out) == 0) return(tibble())
        mutate(out, Season = season, Framework = framework, File = file, Sample = sample, .before = 1)
      })

      if (nrow(summary_rows) == 0) return(tibble())

      summary_rows <- mutate(
        summary_rows,
        Market = market_labels[market] %||% market,
        WinPct = round(100 * win_pct, 1)
      )
      summary_rows <- select(
        summary_rows,
        Season,
        Framework,
        File,
        Sample,
        Market,
        Result = result_col,
        Projection = projection_col,
        Picks = picks,
        Wins = wins,
        Losses = losses,
        WinPct
      )
      arrange(summary_rows, Framework, desc(WinPct), desc(Picks), File, Result)
    })

    output[[paste0(id, "_status")]] <- renderText({
      req(input[[paste0(id, "_build")]] > 0)
      s <- nextgen_family_summary()
      f <- nextgen_files_for_season()
      paste(
        paste("Family:", next_gen_family_labels[[family_key]] %||% family_key),
        paste("Frameworks:", paste(input[[paste0(id, "_frameworks")]] %||% character(), collapse = ", ")),
        paste("Season:", input[[paste0(id, "_season")]]),
        paste("Market:", input[[paste0(id, "_market")]]),
        paste("Files included:", nrow(f)),
        paste("Summary rows:", nrow(s)),
        paste("Available next-gen frameworks:", paste(sort(unique(nextgen_inventory$framework)), collapse = ", ")),
        paste("Available next-gen seasons:", paste(sort(unique(nextgen_inventory$season)), collapse = ", ")),
        sep = "\n"
      )
    })

    output[[paste0(id, "_summary")]] <- renderPrint({
      if (is.null(input[[paste0(id, "_build")]]) || input[[paste0(id, "_build")]] <= 0) {
        return(invisible(NULL))
      }
      s <- nextgen_family_summary() %>% dplyr::slice_head(n = 80)
      if (nrow(s) == 0) {
        cat("No next-gen summary rows for this selection.\n")
        return(invisible(NULL))
      }
      print(as.data.frame(s), row.names = FALSE)
    })

    output[[paste0(id, "_download_summary")]] <- downloadHandler(
      filename = function() paste0(family_key, "_nextgen_summary_", input[[paste0(id, "_season")]], "_", Sys.Date(), ".csv"),
      content = function(file) write_csv(nextgen_family_summary(), file)
    )

    output[[paste0(id, "_future_preview")]] <- renderDT({
      req(input[[paste0(id, "_build")]] > 0)
      files <- nextgen_future_files()
      if (nrow(files) == 0) {
        return(datatable(tibble(Message = "No future next-gen projection files for this family/framework/season selection."), rownames = FALSE))
      }
      market <- input[[paste0(id, "_market")]] %||% "all"
      rows <- pmap_dfr(files, function(path, file, season, framework, sample, family, family_label) {
        df <- read_nextgen_model_file(path)
        cols <- if (identical(market, "all")) {
          dedupe_exact_prediction_cols(df, nextgen_prediction_cols(df))
        } else {
          nextgen_projection_columns_for_market(df, market)
        }
        cols <- cols[cols %in% names(df)]
        cols <- cols[vapply(df[cols], function(x) any(!is.na(x)), logical(1))]
        if (length(cols) == 0) return(tibble())
        keep <- unique(c("game_id", "season", "week", "home_team", "away_team", "spread_line", "total_line", "home_implied", "away_implied", "line", cols))
        keep <- keep[keep %in% names(df)]
        df %>%
          select(all_of(keep)) %>%
          mutate(File = file, Framework = framework, .before = 1)
      })
      if (nrow(rows) == 0) {
        return(datatable(tibble(Message = "No future next-gen projection columns for this selection."), rownames = FALSE))
      }
      rows <- rows %>%
        mutate(season = as.integer(season), week = as.integer(week))
      decimal_cols <- setdiff(names(rows)[vapply(rows, is.numeric, logical(1))], c("season", "week"))
      rows <- rows %>%
        mutate(across(all_of(decimal_cols), ~ ifelse(is.na(.x), NA_character_, sprintf("%.1f", .x))))
      datatable(
        rows,
        rownames = FALSE,
        filter = "top",
        options = list(
          dom = '<"top"lfrip>t<"bottom"lfrip>',
          pageLength = 25,
          lengthMenu = c(10, 25, 50, 100),
          scrollX = TRUE,
          autoWidth = FALSE,
          columnDefs = list(
            list(targets = 2, width = "190px", className = "dt-nowrap")
          )
        )
      )
    })
  }

  bind_nextgen_family("ng_elasticnet", "elastic_net_lasso")
  bind_nextgen_family("ng_weightedlinear", "weighted_linear_regression")
  bind_nextgen_family("ng_decisiontree", "decision_tree_rpart")
  bind_nextgen_family("ng_randomforest", "random_forest_ranger")
  bind_nextgen_family("ng_gbm", "gbm_boosted_trees")
  bind_nextgen_family("ng_xgboost", "xgboost_regression")

  historical_model_scores <- function(market, cutoff_season = Inf) {
    score_market <- if (identical(market, "straight_up")) {
      "spread"
    } else if (market %in% c("home_implied", "away_implied")) {
      c("team_implied", "opp_implied")
    } else {
      market
    }
    selected <- inventory %>% filter(season %in% backtest_seasons, season < cutoff_season)
    rows <- pmap_dfr(selected, function(path, file, season, family, family_label, split) {
      detect_cover_summary(read_model_file(path), split) %>%
        filter(!is.na(projection_col)) %>%
        mutate(
          family = family,
          family_label = family_label,
          split = split,
          file = file,
          season = season,
          .before = 1
        )
    })
    if (nrow(rows) == 0) return(tibble())
    if (!identical(score_market, "all")) rows <- rows %>% filter(market %in% score_market)
    rows %>%
      group_by(family, family_label, split, market, projection_col) %>%
      summarise(
        model_result_col = first(result_col),
        model_picks = sum(picks, na.rm = TRUE),
        model_wins = sum(wins, na.rm = TRUE),
        model_losses = sum(losses, na.rm = TRUE),
        model_win_pct = ifelse(model_picks > 0, model_wins / model_picks, NA_real_),
        .groups = "drop"
      )
  }

  long_predictions_for_file <- function(meta, market, min_win_pct = 0.40, line_source = "closing", injury_source = "apply", apply_amortization = TRUE) {
    df <- read_model_file(meta$path)
    cols <- projection_columns_for_market(df, market)
    if (length(cols) == 0) return(tibble())

    score_season <- suppressWarnings(as.integer(meta$season %||% NA_integer_))
    target_family <- meta$family[[1L]]
    target_split <- meta$split[[1L]]
    historical_score_lookup <- function(cutoff_season) {
      scored <- historical_model_scores(market, cutoff_season = cutoff_season)
      if (nrow(scored) == 0L) {
        return(tibble(
          projection_col = character(), model_result_col = character(),
          model_picks = integer(), model_win_pct = double()
        ))
      }
      scored %>%
        filter(.data$family == .env$target_family, .data$split == .env$target_split) %>%
        select(projection_col, model_result_col, model_picks, model_win_pct)
    }
    score_market <- if (identical(market, "straight_up")) {
      "spread"
    } else if (market %in% c("home_implied", "away_implied")) {
      c("team_implied", "opp_implied")
    } else {
      market
    }
    # A 2026 row must not select/weight its model using outcomes from the
    # same 2026 prediction file, even when the model fit itself is frozen.
    if (is.finite(score_season) && score_season >= 2026L) {
      score_lookup <- historical_score_lookup(score_season)
    } else {
      score_lookup <- detect_cover_summary(df, target_split)
      if (!identical(score_market, "all")) score_lookup <- filter(score_lookup, market %in% score_market)
      score_lookup <- score_lookup %>%
        filter(!is.na(projection_col)) %>%
        select(projection_col, model_result_col = result_col, model_picks = picks, model_win_pct = win_pct)
    }
    if (nrow(score_lookup) == 0 || all(is.na(score_lookup$model_win_pct))) {
      score_lookup <- historical_score_lookup(if (is.finite(score_season)) score_season else Inf)
    }

    base <- df %>%
      transmute(
        game_id = .data[["game_id"]],
        season = .data[["season"]],
        week = .data[["week"]],
        home_team = .data[["home_team"]],
        away_team = .data[["away_team"]],
        home_score = get_line(df, "home_score"),
        away_score = get_line(df, "away_score"),
        spread_line = get_line(df, "spread_line"),
        total_line = get_line(df, "total_line")
      )
    if (identical(line_source, "early") && nrow(early_lines) > 0) {
      base <- base %>%
        left_join(early_lines, by = "game_id") %>%
        mutate(
          spread_line = coalesce(early_spread_line, spread_line),
          total_line = coalesce(early_total_line, total_line)
        ) %>%
        select(-early_spread_line, -early_total_line)
    }
    base <- apply_projection_adjustments(base, injury_source, apply_amortization)

    out <- map_dfr(cols, function(col) {
      model_score <- score_lookup %>% filter(projection_col == col) %>% dplyr::slice_head(n = 1)
      if (nrow(model_score) == 0) {
        model_result_col <- NA_character_
        model_picks <- NA_integer_
        model_win_pct <- NA_real_
      } else {
        model_result_col <- model_score$model_result_col
        model_picks <- model_score$model_picks
        model_win_pct <- model_score$model_win_pct
      }

      raw <- suppressWarnings(as.numeric(df[[col]]))
      split <- meta$split
      pred <- raw
      if (market %in% c("spread", "straight_up") && str_detect(split, "^away")) pred <- -pred
      if (market == "home_implied" && str_detect(split, "^away") && str_detect(col, "^(Score_|Score$|Score_final|ImpliedTeamScored)")) pred <- NA_real_
      if (market == "home_implied" && str_detect(split, "^home") && str_detect(col, "OppScore|ImpliedOppScored")) pred <- NA_real_
      if (market == "away_implied" && str_detect(split, "^home") && str_detect(col, "^(Score_|Score$|Score_final|ImpliedTeamScored)")) pred <- NA_real_
      if (market == "away_implied" && str_detect(split, "^away") && str_detect(col, "OppScore|ImpliedOppScored")) pred <- NA_real_
      if (market %in% c("spread", "straight_up")) {
        pred <- pred + base$spread_projection_adj
      } else if (market == "total") {
        pred <- pred + base$total_projection_adj
      } else if (market == "home_implied") {
        pred <- pred + base$home_score_projection_adj
      } else if (market == "away_implied") {
        pred <- pred + base$away_score_projection_adj
      }

      line <- if (market == "spread") {
        base$spread_line
      } else if (market == "straight_up") {
        rep(0, nrow(base))
      } else if (market == "total") {
        base$total_line
      } else if (market == "home_implied") {
        (base$total_line + base$spread_line) / 2
      } else {
        (base$total_line - base$spread_line) / 2
      }

      actual <- if (market %in% c("spread", "straight_up")) {
        base$home_score - base$away_score
      } else if (market == "total") {
        base$home_score + base$away_score
      } else if (market == "home_implied") {
        base$home_score
      } else {
        base$away_score
      }

      pick <- dplyr::case_when(pred > line ~ 1, pred < line ~ -1, TRUE ~ NA_real_)
      actual_side <- dplyr::case_when(actual > line ~ 1, actual < line ~ -1, TRUE ~ NA_real_)

      bind_cols(base, tibble(
        family = meta$family_label,
        file = meta$file,
        split = meta$split,
        projection_col = col,
        projection = pred,
        market_line = line,
        actual = actual,
        pick = pick,
        actual_side = actual_side,
        correct = ifelse(!is.na(pick) & !is.na(actual_side), pick == actual_side, NA),
        model_result_col = model_result_col,
        model_picks = model_picks,
        model_win_pct = model_win_pct
      ))
    })

    out %>%
      filter(!is.na(projection)) %>%
      filter(!is.na(model_win_pct), model_win_pct >= min_win_pct)
  }

  consensus_rows <- reactiveVal(tibble())

  build_consensus_rows <- function() {
    consensus_status("Building consensus...")
    families <- input$cons_families %||% names(family_labels)
    markets <- dashboard_market_keys
    line_source <- input$cons_line_source %||% "closing"
    injury_source <- input$cons_injury_source %||% "apply"
    apply_amortization <- isTRUE(input$cons_apply_amortization)
    min_agree <- (input$cons_agree %||% 50) / 100
    withProgress(message = "Building consensus, please wait...", value = 0, {
      selected_seasons <- unique(c(as.integer(input$cons_seasons %||% integer()), as.integer(input$cons_future_seasons %||% integer())))
      if (length(selected_seasons) == 0 || all(is.na(selected_seasons))) {
        consensus_status("No seasons selected.")
        return(tibble())
      }
      selected <- inventory %>%
        filter(family %in% families, season %in% selected_seasons)
      if (nrow(selected) == 0) {
        consensus_status("No model files matched the selected families and seasons.")
        return(tibble())
      }

      incProgress(0.2, detail = "Collecting model projections")
      long <- purrr::map_dfr(markets, function(market) {
        min_win <- if (identical(market, "straight_up")) 0 else (input$cons_min_win %||% 40) / 100
        pmap_dfr(selected, function(path, file, season, family, family_label, split) {
          long_predictions_for_file(
            tibble(path = path, file = file, season = season, family = family, family_label = family_label, split = split),
            market,
            min_win,
            line_source,
            injury_source,
            apply_amortization
          )
        }) %>%
          mutate(market = .env$market)
      })

      if (nrow(long) == 0) {
        consensus_status("No projections passed the current selections.")
        return(tibble())
      }

      # Do not let duplicated legacy home/away exports or XGBoost aliases count
      # as separate votes. Home/away team-score markets remain distinct.
      long <- long %>%
        mutate(
          .signal_key = case_when(
            market %in% c("spread", "straight_up") & projection_col == "ImpliedScoreDiff_xgb" ~ "ScoreDiff_xgb_score",
            market %in% c("spread", "straight_up", "total") ~ projection_col,
            TRUE ~ paste(file, projection_col, sep = "::")
          ),
          .home_first = !str_detect(split, "^home")
        ) %>%
        arrange(.home_first) %>%
        group_by(game_id, season, week, market, family, .signal_key) %>%
        slice_head(n = 1) %>%
        ungroup() %>%
        select(-.signal_key, -.home_first)

      incProgress(0.7, detail = "Averaging model signals")
      rows <- long %>%
        group_by(game_id, season, week, home_team, away_team, market) %>%
        summarise(
          projections = n(),
          models_used = n_distinct(paste(file, projection_col, sep = "::")),
          avg_projection = mean(projection, na.rm = TRUE),
          market_line = first_non_na(market_line),
          spread_line = first_non_na(spread_line),
          total_line = first_non_na(total_line),
          avg_edge = ifelse(is.na(market_line), NA_real_, avg_projection - market_line),
          agree_pct = ifelse(all(is.na(pick)), NA_real_, max(mean(pick == 1, na.rm = TRUE), mean(pick == -1, na.rm = TRUE))),
          consensus_pick = case_when(
            all(is.na(pick)) ~ NA_character_,
            first(market) %in% c("spread", "straight_up") & sum(pick == 1, na.rm = TRUE) > sum(pick == -1, na.rm = TRUE) ~ "Home",
            first(market) %in% c("spread", "straight_up") & sum(pick == -1, na.rm = TRUE) > sum(pick == 1, na.rm = TRUE) ~ "Away",
            first(market) %in% c("spread", "straight_up") & avg_projection > market_line ~ "Home",
            first(market) %in% c("spread", "straight_up") & avg_projection < market_line ~ "Away",
            first(market) %in% c("spread", "straight_up") ~ NA_character_,
            sum(pick == 1, na.rm = TRUE) > sum(pick == -1, na.rm = TRUE) ~ "Over",
            sum(pick == -1, na.rm = TRUE) > sum(pick == 1, na.rm = TRUE) ~ "Under",
            avg_projection > market_line ~ "Over",
            avg_projection < market_line ~ "Under",
            TRUE ~ NA_character_
          ),
          actual_side = first_non_na(actual_side),
          actual_result = first_non_na(actual),
          correct = ifelse(!is.na(actual_side), ifelse(consensus_pick %in% c("Home", "Over"), 1, -1) == actual_side, NA),
          .groups = "drop"
        ) %>%
        filter(is.na(agree_pct) | agree_pct >= min_agree) %>%
        arrange(market, season, week, game_id)
      consensus_status(paste0("Complete. Built ", nrow(rows), " consensus rows across ", length(markets), " markets from ", nrow(selected), " model files. Use the Market dropdown to view one market at a time."))
      rows
    })
  }

  observeEvent(input$cons_run, {
    rows <- tryCatch(
      build_consensus_rows(),
      error = function(e) {
        consensus_status(paste("Consensus build error:", conditionMessage(e)))
        tibble()
      }
    )
    consensus_rows(rows)
  }, ignoreInit = TRUE, priority = 100)

  observeEvent(input$cons_run_top, {
    rows <- tryCatch(
      build_consensus_rows(),
      error = function(e) {
        consensus_status(paste("Consensus build error:", conditionMessage(e)))
        tibble()
      }
    )
    consensus_rows(rows)
  }, ignoreInit = TRUE, priority = 100)

  build_nextgen_consensus_rows <- function() {
    nextgen_consensus_status("Building next-gen consensus...")
    if (!nextgen_data_available) {
      nextgen_consensus_status("Next-gen prepared data is missing. Run prepare_data_nextgen.R and deploy the nextgen RDS files.")
      return(tibble())
    }
    frameworks <- input$ng_cons_frameworks %||% unique(nextgen_inventory$framework)
    families <- input$ng_cons_families %||% names(next_gen_family_labels)
    projection_sources <- input$ng_cons_projection_sources %||% c("direct", "implied_team_scores")
    markets <- dashboard_market_keys
    line_source <- input$ng_cons_line_source %||% "closing"
    injury_source <- input$ng_cons_injury_source %||% "apply"
    apply_amortization <- isTRUE(input$ng_cons_apply_amortization)
    min_agree <- (input$ng_cons_agree %||% 50) / 100

    withProgress(message = "Building next-gen consensus, please wait...", value = 0, {
      selected_seasons <- unique(c(as.integer(input$ng_cons_seasons %||% integer()), as.integer(input$ng_cons_future_seasons %||% integer())))
      if (length(selected_seasons) == 0 || all(is.na(selected_seasons))) {
        nextgen_consensus_status("No seasons selected.")
        return(tibble())
      }
      selected <- nextgen_inventory %>%
        filter(framework %in% frameworks, family %in% families, season %in% selected_seasons)
      if (nrow(selected) == 0) {
        nextgen_consensus_status("No next-gen model files matched the selected frameworks, families, and seasons.")
        return(tibble())
      }

      incProgress(0.2, detail = "Collecting next-gen model projections")
      long <- purrr::map_dfr(markets, function(market) {
        min_win <- if (identical(market, "straight_up")) 0 else (input$ng_cons_min_win %||% 40) / 100
        pmap_dfr(selected, function(path, file, season, framework, sample, family, family_label) {
          long_nextgen_predictions_for_file(
            tibble(path = path, file = file, season = season, framework = framework, sample = sample, family = family, family_label = family_label),
            market,
            min_win,
            line_source,
            injury_source,
            apply_amortization,
            projection_sources
          )
        }) %>%
          mutate(market = .env$market)
      })

      if (nrow(long) == 0) {
        nextgen_consensus_status("No next-gen projections passed the current selections.")
        return(tibble())
      }

      incProgress(0.7, detail = "Averaging next-gen model signals")
      rows <- long %>%
        group_by(game_id, season, week, home_team, away_team, market) %>%
        summarise(
          projections = n(),
          models_used = n_distinct(paste(file, projection_col, sep = "::")),
          frameworks_used = paste(sort(unique(framework)), collapse = ", "),
          projection_sources_used = paste(sort(unique(projection_source)), collapse = ", "),
          direct_models_used = n_distinct(paste(file[projection_source == "Direct model projection"], projection_col[projection_source == "Direct model projection"], sep = "::")),
          implied_models_used = n_distinct(paste(file[projection_source == "Implied from team scores"], projection_col[projection_source == "Implied from team scores"], sep = "::")),
          avg_projection = mean(projection, na.rm = TRUE),
          market_line = first_non_na(market_line),
          spread_line = first_non_na(spread_line),
          total_line = first_non_na(total_line),
          avg_edge = ifelse(is.na(market_line), NA_real_, avg_projection - market_line),
          agree_pct = ifelse(all(is.na(pick)), NA_real_, max(mean(pick == 1, na.rm = TRUE), mean(pick == -1, na.rm = TRUE))),
          consensus_pick = case_when(
            all(is.na(pick)) ~ NA_character_,
            first(market) %in% c("spread", "straight_up") & sum(pick == 1, na.rm = TRUE) > sum(pick == -1, na.rm = TRUE) ~ "Home",
            first(market) %in% c("spread", "straight_up") & sum(pick == -1, na.rm = TRUE) > sum(pick == 1, na.rm = TRUE) ~ "Away",
            first(market) %in% c("spread", "straight_up") & avg_projection > market_line ~ "Home",
            first(market) %in% c("spread", "straight_up") & avg_projection < market_line ~ "Away",
            first(market) %in% c("spread", "straight_up") ~ NA_character_,
            sum(pick == 1, na.rm = TRUE) > sum(pick == -1, na.rm = TRUE) ~ "Over",
            sum(pick == -1, na.rm = TRUE) > sum(pick == 1, na.rm = TRUE) ~ "Under",
            avg_projection > market_line ~ "Over",
            avg_projection < market_line ~ "Under",
            TRUE ~ NA_character_
          ),
          actual_side = first_non_na(actual_side),
          actual_result = first_non_na(actual),
          correct = ifelse(!is.na(actual_side), ifelse(consensus_pick %in% c("Home", "Over"), 1, -1) == actual_side, NA),
          .groups = "drop"
        ) %>%
        filter(is.na(agree_pct) | agree_pct >= min_agree) %>%
        arrange(market, season, week, game_id)
      nextgen_consensus_status(paste0(
        "Complete. Built ", nrow(rows), " next-gen consensus rows across ", length(markets), " markets from ", nrow(selected),
        " model files using ", paste(projection_sources, collapse = " + "), " projection sources. Use the Market dropdown to view one market at a time."
      ))
      rows
    })
  }

  observeEvent(input$ng_cons_run, {
    rows <- tryCatch(
      build_nextgen_consensus_rows(),
      error = function(e) {
        nextgen_consensus_status(paste("Next-gen consensus build error:", conditionMessage(e)))
        tibble()
      }
    )
    nextgen_consensus_rows(rows)
  }, ignoreInit = TRUE, priority = 100)

  observeEvent(input$ng_cons_run_top, {
    rows <- tryCatch(
      build_nextgen_consensus_rows(),
      error = function(e) {
        nextgen_consensus_status(paste("Next-gen consensus build error:", conditionMessage(e)))
        tibble()
      }
    )
    nextgen_consensus_rows(rows)
  }, ignoreInit = TRUE, priority = 100)

  market_line_is_consistent <- function(rows) {
    if (nrow(rows) == 0) return(rep(TRUE, 0))
    close_enough <- function(a, b) is.na(a) | is.na(b) | abs(a - b) < 0.001
    expected_home_implied <- ifelse(!is.na(rows$total_line) & !is.na(rows$spread_line), (rows$total_line + rows$spread_line) / 2, NA_real_)
    expected_away_implied <- ifelse(!is.na(rows$total_line) & !is.na(rows$spread_line), (rows$total_line - rows$spread_line) / 2, NA_real_)
    case_when(
      rows$market == "spread" ~ close_enough(rows$market_line, rows$spread_line),
      rows$market == "straight_up" ~ is.na(rows$market_line) | abs(rows$market_line) < 0.001,
      rows$market == "total" ~ close_enough(rows$market_line, rows$total_line),
      rows$market == "home_implied" ~ close_enough(rows$market_line, expected_home_implied),
      rows$market == "away_implied" ~ close_enough(rows$market_line, expected_away_implied),
      TRUE ~ TRUE
    )
  }

  aggregate_circa_model_rows <- function(long, source_label, min_agree) {
    if (nrow(long) == 0) return(tibble())
    long %>%
      mutate(
        circa_vote = case_when(
          projection > circa_market_line ~ 1,
          projection < circa_market_line ~ -1,
          TRUE ~ NA_real_
        )
      ) %>%
      group_by(
        game_id, season, week, home_team, away_team,
        circa_away_line, circa_home_line, circa_market_line
      ) %>%
      summarise(
        projections = n(),
        models_used = n_distinct(paste(file, projection_col, sep = "::")),
        avg_projection = mean(projection, na.rm = TRUE),
        current_market_line = first_non_na(market_line),
        positive_votes = sum(circa_vote == 1, na.rm = TRUE),
        negative_votes = sum(circa_vote == -1, na.rm = TRUE),
        vote_count = sum(!is.na(circa_vote)),
        agree_pct = ifelse(
          all(is.na(circa_vote)),
          NA_real_,
          max(mean(circa_vote == 1, na.rm = TRUE), mean(circa_vote == -1, na.rm = TRUE))
        ),
        .groups = "drop"
      ) %>%
      mutate(
        source = source_label,
        source_pick = case_when(
          vote_count == 0 ~ NA_character_,
          positive_votes > negative_votes ~ "Home",
          negative_votes > positive_votes ~ "Away",
          avg_projection > circa_market_line ~ "Home",
          avg_projection < circa_market_line ~ "Away",
          TRUE ~ NA_character_
        )
      ) %>%
      select(-positive_votes, -negative_votes, -vote_count) %>%
      filter(is.na(agree_pct) | agree_pct >= min_agree)
  }

  build_circa_legacy_source <- function(lines) {
    families <- input$cons_families %||% names(family_labels)
    seasons <- unique(as.integer(lines$season))
    selected <- inventory %>% filter(family %in% families, season %in% seasons)
    if (nrow(selected) == 0) return(tibble())
    min_win <- (input$cons_min_win %||% 40) / 100
    long <- pmap_dfr(selected, function(path, file, season, family, family_label, split) {
      long_predictions_for_file(
        tibble(path = path, file = file, season = season, family = family, family_label = family_label, split = split),
        "spread",
        min_win,
        input$cons_line_source %||% "closing",
        input$cons_injury_source %||% "apply",
        isTRUE(input$cons_apply_amortization)
      )
    }) %>%
      inner_join(
        lines %>% select(game_id, circa_away_line, circa_home_line),
        by = "game_id"
      ) %>%
      mutate(
        circa_market_line = -circa_home_line,
        .signal_key = case_when(
          projection_col == "ImpliedScoreDiff_xgb" ~ "ScoreDiff_xgb_score",
          TRUE ~ projection_col
        ),
        .home_first = !str_detect(split, "^home")
      ) %>%
      arrange(.home_first) %>%
      group_by(game_id, season, week, family, .signal_key) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      select(-.signal_key, -.home_first)
    aggregate_circa_model_rows(long, "Legacy", (input$cons_agree %||% 50) / 100)
  }

  build_circa_nextgen_source <- function(lines) {
    if (!nextgen_data_available) return(tibble())
    frameworks <- input$ng_cons_frameworks %||% unique(nextgen_inventory$framework)
    families <- input$ng_cons_families %||% names(next_gen_family_labels)
    projection_sources <- input$ng_cons_projection_sources %||% c("direct", "implied_team_scores")
    seasons <- unique(as.integer(lines$season))
    selected <- nextgen_inventory %>%
      filter(framework %in% frameworks, family %in% families, season %in% seasons)
    if (nrow(selected) == 0) return(tibble())
    min_win <- (input$ng_cons_min_win %||% 40) / 100
    long <- pmap_dfr(selected, function(path, file, season, framework, sample, family, family_label) {
      long_nextgen_predictions_for_file(
        tibble(path = path, file = file, season = season, framework = framework, sample = sample, family = family, family_label = family_label),
        "spread",
        min_win,
        input$ng_cons_line_source %||% "closing",
        input$ng_cons_injury_source %||% "apply",
        isTRUE(input$ng_cons_apply_amortization),
        projection_sources
      )
    }) %>%
      inner_join(
        lines %>% select(game_id, circa_away_line, circa_home_line),
        by = "game_id"
      ) %>%
      mutate(circa_market_line = -circa_home_line)
    aggregate_circa_model_rows(long, "Next-gen", (input$ng_cons_agree %||% 50) / 100)
  }

  combine_circa_sources <- function(source_rows, selected_source) {
    if (nrow(source_rows) == 0) return(tibble())
    if (!identical(selected_source, "combined")) {
      return(source_rows %>%
        transmute(
          game_id, season, week, home_team, away_team,
          circa_away_line, circa_home_line, circa_market_line,
          projections, models_used, sources_used = source,
          sources_used_count = 1L,
          avg_projection, current_market_line, agree_pct,
          consensus_pick = source_pick
        ))
    }

    required <- input$overall_cons_sources %||% c("legacy", "next_gen")
    required_labels <- c(if ("legacy" %in% required) "Legacy", if ("next_gen" %in% required) "Next-gen")
    require_all <- isTRUE(input$overall_cons_require_all_sources %||% TRUE)
    min_agree <- (input$overall_cons_agree %||% 50) / 100
    source_rows %>%
      filter(source %in% required_labels) %>%
      mutate(source_vote = case_when(source_pick == "Home" ~ 1, source_pick == "Away" ~ -1, TRUE ~ NA_real_)) %>%
      group_by(
        game_id, season, week, home_team, away_team,
        circa_away_line, circa_home_line, circa_market_line
      ) %>%
      summarise(
        projections = sum(projections, na.rm = TRUE),
        models_used = sum(models_used, na.rm = TRUE),
        sources_used = paste(sort(unique(source)), collapse = ", "),
        sources_used_count = n_distinct(source),
        avg_projection = mean(avg_projection, na.rm = TRUE),
        current_market_line = first_non_na(current_market_line),
        positive_votes = sum(source_vote == 1, na.rm = TRUE),
        negative_votes = sum(source_vote == -1, na.rm = TRUE),
        vote_count = sum(!is.na(source_vote)),
        agree_pct = ifelse(
          all(is.na(source_vote)),
          NA_real_,
          max(mean(source_vote == 1, na.rm = TRUE), mean(source_vote == -1, na.rm = TRUE))
        ),
        .groups = "drop"
      ) %>%
      mutate(
        consensus_pick = case_when(
          vote_count == 0 ~ NA_character_,
          positive_votes > negative_votes ~ "Home",
          negative_votes > positive_votes ~ "Away",
          avg_projection > circa_market_line ~ "Home",
          avg_projection < circa_market_line ~ "Away",
          TRUE ~ NA_character_
        )
      ) %>%
      select(-positive_votes, -negative_votes, -vote_count) %>%
      filter(!require_all | sources_used_count == length(required_labels)) %>%
      filter(is.na(agree_pct) | agree_pct >= min_agree)
  }

  set_circa_lines <- function(rows, source_note) {
    circa_lines_state(rows)
    circa_results_state(tibble())
    circa_source_url_state(source_note)
    circa_status_state(paste0(
      "Loaded ", nrow(rows), " Circa matchups for ", first(rows$season),
      " Week ", first(rows$week), ". Click Build Circa dashboard."
    ))
  }

  refresh_circa_week <- function(season, week) {
    circa_status_state("Checking Circa Sports for the official weekly sheet...")
    rows <- tryCatch(
      withProgress(
        message = "Refreshing official Circa lines, please wait...",
        value = 0.35,
        download_circa_week_from_web(season, week)
      ),
      error = function(e) {
        circa_status_state(paste("Circa web refresh error:", conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(rows)) {
      source_url <- attr(rows, "source_url") %||% "https://www.circasports.com/circa-million"
      set_circa_lines(rows, source_url)
    }
  }

  observeEvent(list(input$dashboard_season, input$dashboard_week), {
    season <- suppressWarnings(as.integer(input$dashboard_season))
    week <- suppressWarnings(as.integer(input$dashboard_week))
    if (length(season) != 1 || length(week) != 1 || is.na(season) || is.na(week)) return()
    if (!identical(as.integer(input$circa_season), season)) updateNumericInput(session, "circa_season", value = season)
    if (!identical(as.integer(input$circa_week), week)) updateNumericInput(session, "circa_week", value = week)
  }, ignoreInit = TRUE)

  observeEvent(list(input$circa_season, input$circa_week), {
    season <- suppressWarnings(as.integer(input$circa_season))
    week <- suppressWarnings(as.integer(input$circa_week))
    if (length(season) != 1 || length(week) != 1 || is.na(season) || is.na(week)) return()
    loaded <- circa_lines_state()
    if (nrow(loaded) > 0 && all(loaded$season == season, loaded$week == week)) return()
    circa_lines_state(tibble())
    circa_results_state(tibble())
    circa_source_url_state("")
    bundled <- tryCatch(read_circa_bundled_lines(season, week), error = function(e) tibble())
    if (nrow(bundled) > 0) {
      set_circa_lines(bundled, paste("Bundled verified Week", week, "copy"))
    } else {
      refresh_circa_week(season, week)
    }
  }, ignoreInit = FALSE)

  observeEvent(input$circa_refresh_web, {
    refresh_circa_week(input$circa_season %||% circa_active_season, input$circa_week %||% circa_active_week)
  }, ignoreInit = TRUE)

  observeEvent(input$circa_lines_file, {
    req(input$circa_lines_file$datapath)
    circa_status_state("Reading the uploaded Circa sheet...")
    rows <- tryCatch(
      withProgress(
        message = "Reading Circa contest lines, please wait...",
        value = 0.5,
        load_circa_lines(
          input$circa_lines_file$datapath,
          input$circa_season %||% 2026L,
          input$circa_week %||% 1L,
          input$circa_lines_file$name
        )
      ),
      error = function(e) {
        circa_status_state(paste("Circa upload error:", conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(rows)) {
      set_circa_lines(rows, paste("Manual upload:", input$circa_lines_file$name))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$circa_use_default, {
    season <- input$circa_season %||% circa_active_season
    week <- input$circa_week %||% circa_active_week
    bundled <- tryCatch(read_circa_bundled_lines(season, week), error = function(e) tibble())
    if (nrow(bundled) == 0) {
      circa_status_state(paste0("No bundled Circa lines are available for ", season, " Week ", week, "."))
    } else {
      set_circa_lines(bundled, paste("Bundled verified Week", week, "copy"))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$circa_build, {
    lines <- circa_lines_state()
    if (nrow(lines) == 0) {
      if (!str_detect(circa_status_state(), "error")) {
        circa_status_state(paste0("No Circa lines are loaded for ", input$circa_season, " Week ", input$circa_week, ". Refresh or upload the official sheet."))
      }
      circa_results_state(tibble())
      return()
    }
    if (!all(lines$season == as.integer(input$circa_season), lines$week == as.integer(input$circa_week))) {
      circa_status_state("The loaded Circa sheet is for a different week. Refresh or upload the selected week's sheet.")
      circa_results_state(tibble())
      return()
    }
    selected_source <- input$circa_source %||% "combined"
    circa_status_state("Building Circa plays from the active model settings...")
    rows <- tryCatch(
      withProgress(message = "Building Circa dashboard, please wait...", value = 0, {
        required <- if (identical(selected_source, "combined")) input$overall_cons_sources %||% c("legacy", "next_gen") else selected_source
        incProgress(0.1, detail = "Applying the stale contest lines")
        source_rows <- bind_rows(
          if ("legacy" %in% required) build_circa_legacy_source(lines) else tibble(),
          if ("next_gen" %in% required) build_circa_nextgen_source(lines) else tibble()
        )
        incProgress(0.8, detail = "Ranking the contest sides")
        combine_circa_sources(source_rows, selected_source)
      }),
      error = function(e) {
        circa_status_state(paste("Circa dashboard error:", conditionMessage(e)))
        tibble()
      }
    )
    circa_results_state(rows)
    if (nrow(rows) > 0) {
      circa_status_state(paste0(
        "Complete. Ranked ", nrow(rows), " Circa sides from ",
        if (identical(selected_source, "combined")) "the selected consensus sources" else paste0("the ", selected_source, " consensus"),
        ". The top five are the shadow card."
      ))
    } else if (!str_detect(circa_status_state(), "error")) {
      circa_status_state("No Circa plays passed the active model and agreement settings.")
    }
  }, ignoreInit = TRUE)

  format_spread_price <- function(x) {
    rounded <- round(as.numeric(x), 1)
    magnitude <- sub("\\.0$", "", sprintf("%.1f", abs(rounded)))
    case_when(
      is.na(rounded) ~ "-",
      abs(rounded) < 0.001 ~ "PK",
      rounded > 0 ~ paste0("+", magnitude),
      TRUE ~ paste0("-", magnitude)
    )
  }

  circa_ranked_rows <- reactive({
    rows <- circa_results_state()
    if (nrow(rows) == 0) return(tibble())
    minimum_edge <- suppressWarnings(as.numeric(input$circa_min_edge %||% 0))
    if (!is.finite(minimum_edge)) minimum_edge <- 0
    current_threshold <- if (identical(input$circa_source %||% "combined", "combined")) suppressWarnings(as.numeric(input$overall_cons_min_edge %||% 2)) else 0
    rows %>%
      mutate(
        current_pick = case_when(
          avg_projection > current_market_line ~ "Home",
          avg_projection < current_market_line ~ "Away",
          TRUE ~ NA_character_
        ),
        current_signed_edge = avg_projection - current_market_line,
        current_edge = abs(current_signed_edge),
        circa_signed_edge = avg_projection - circa_market_line,
        circa_edge = abs(circa_signed_edge),
        circa_pick = case_when(circa_signed_edge > 0 ~ "Home", circa_signed_edge < 0 ~ "Away", TRUE ~ NA_character_),
        pick_team = ifelse(circa_pick == "Home", home_team, away_team),
        pick_line = ifelse(circa_pick == "Home", circa_home_line, circa_away_line),
        projected_line = ifelse(circa_pick == "Home", -avg_projection, avg_projection),
        current_pick_team = ifelse(current_pick == "Home", home_team, away_team),
        current_pick_line = ifelse(current_pick == "Home", -current_market_line, current_market_line),
        current_qualifies = !is.na(current_edge) & current_edge >= current_threshold,
        circa_qualifies = !is.na(circa_edge) & circa_edge >= minimum_edge,
        status = case_when(
          is.na(current_pick) ~ "Circa only",
          circa_pick != current_pick ~ "Side flips",
          circa_qualifies & !current_qualifies ~ "New at Circa",
          !circa_qualifies & current_qualifies ~ "Drops at Circa",
          circa_qualifies & current_qualifies ~ "Same side qualifies",
          TRUE ~ "Below threshold"
        )
      ) %>%
      filter(
        circa_qualifies,
        !is.na(circa_pick),
        !(isTRUE(input$circa_no_minus_3_5_favorites) & dplyr::near(pick_line, -3.5)),
        !(isTRUE(input$circa_no_plus_2_5_underdogs) & dplyr::near(pick_line, 2.5)),
        !(isTRUE(input$circa_no_early_week_games) & game_id %in% early_week_game_ids)
      ) %>%
      arrange(desc(circa_edge), away_team, home_team) %>%
      mutate(rank = row_number())
  })

  circa_card_rows <- reactive({
    rows <- circa_ranked_rows()
    if (nrow(rows) == 0) return(rows)
    skipped <- input$circa_skip_matchups %||% character()
    rows %>% filter(!game_id %in% skipped) %>% mutate(rank = row_number())
  })

  observeEvent(circa_card_rows(), {
    choices <- circa_card_rows() %>% slice_head(n = 7)
    if (!nrow(choices)) {
      updateCheckboxGroupInput(session, "circa_final_picks", choices = character(), selected = character())
      return()
    }
    labels <- sprintf("%s @ %s: %s %s (%.1f-point edge)",
      choices$away_team, choices$home_team, choices$pick_team,
      format_spread_price(choices$pick_line), choices$circa_edge)
    selected <- intersect(isolate(input$circa_final_picks) %||% character(), choices$game_id)
    if (!length(selected)) selected <- head(choices$game_id, 5)
    updateCheckboxGroupInput(session, "circa_final_picks",
      choices = stats::setNames(choices$game_id, labels), selected = selected)
  })

  circa_final_rows <- reactive({
    selected <- input$circa_final_picks %||% character()
    circa_card_rows() %>% filter(game_id %in% selected)
  })

  output$circa_final_status <- renderText({
    n <- nrow(circa_final_rows())
    if (n == 5L) "Five plays selected." else sprintf("Select exactly five plays (%d selected).", n)
  })

  output$circa_final_card <- renderDT({
    rows <- circa_final_rows()
    display <- if (!nrow(rows)) tibble(Message = "Select plays from the list above.") else format_circa_table(rows)
    datatable(display, rownames = FALSE, options = list(dom = "t", pageLength = 5, scrollX = TRUE))
  })

  format_circa_table <- function(rows) {
    if (nrow(rows) == 0) return(tibble(Message = "Build the Circa dashboard to rank the uploaded contest lines."))
    rows %>%
      transmute(
        Rank = rank,
        Matchup = paste(away_team, "@", home_team),
        Pick = paste(pick_team, format_spread_price(pick_line)),
        `Projected line` = paste(pick_team, format_spread_price(projected_line)),
        `Circa edge` = sprintf("%.1f", circa_edge),
        `Current dashboard` = paste(current_pick_team, format_spread_price(current_pick_line)),
        Status = status,
        Agreement = ifelse(is.na(agree_pct), "-", paste0(sprintf("%.0f", 100 * agree_pct), "%")),
        Models = models_used
      )
  }

  output$circa_status <- renderText({
    source_note <- circa_source_url_state()
    paste0(circa_status_state(), if (nzchar(source_note)) paste0("\nSource: ", source_note) else "")
  })

  output$circa_lines_preview <- renderDT({
    rows <- circa_lines_state()
    if (nrow(rows) == 0) return(datatable(tibble(Message = "No Circa lines loaded."), rownames = FALSE))
    display <- rows %>%
      transmute(
        Season = season,
        Week = week,
        Matchup = paste(away_team, "@", home_team),
        Away = paste(away_team, format_spread_price(circa_away_line)),
        Home = paste(home_team, format_spread_price(circa_home_line))
      )
    datatable(display, rownames = FALSE, options = list(dom = "t", pageLength = nrow(display), scrollX = TRUE))
  })

  output$circa_top_five <- renderDT({
    datatable(
      format_circa_table(circa_card_rows() %>% slice_head(n = 5)),
      rownames = FALSE,
      options = list(dom = "t", pageLength = 5, scrollX = TRUE)
    )
  })

  output$circa_alternates <- renderDT({
    rows <- circa_card_rows() %>% slice(6:7)
    display <- if (nrow(rows) == 0) tibble(Message = "No additional eligible plays.") else format_circa_table(rows)
    datatable(display, rownames = FALSE, options = list(dom = "t", pageLength = 2, scrollX = TRUE))
  })

  output$circa_all_plays <- renderDT({
    datatable(
      format_circa_table(circa_ranked_rows()),
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 16, lengthMenu = c(16, 32, 64), scrollX = TRUE)
    )
  })

  output$circa_download_top_five <- downloadHandler(
    filename = function() paste0("circa_shadow_card_", first(circa_lines_state()$season), "_week_", first(circa_lines_state()$week), ".csv"),
    content = function(file) {
      rows <- circa_card_rows() %>% slice_head(n = 5)
      req(nrow(rows) > 0)
      write_csv(rows, file)
    }
  )

  output$circa_download_final <- downloadHandler(
    filename = function() paste0("circa_final_card_", first(circa_lines_state()$season), "_week_", first(circa_lines_state()$week), ".csv"),
    content = function(file) {
      rows <- circa_final_rows()
      req(nrow(rows) == 5L)
      write_csv(rows, file)
    }
  )

  output$circa_download_all <- downloadHandler(
    filename = function() paste0("circa_all_plays_", first(circa_lines_state()$season), "_week_", first(circa_lines_state()$week), ".csv"),
    content = function(file) {
      rows <- circa_ranked_rows()
      req(nrow(rows) > 0)
      write_csv(rows, file)
    }
  )

  build_overall_consensus_rows <- function() {
    overall_consensus_status("Building combined consensus...")
    sources <- input$overall_cons_sources %||% c("legacy", "next_gen")
    markets <- dashboard_market_keys
    min_agree <- (input$overall_cons_agree %||% 50) / 100
    min_edge <- input$overall_cons_min_edge %||% 2
    require_all_sources <- isTRUE(input$overall_cons_require_all_sources %||% TRUE)
    required_source_labels <- c(
      if ("legacy" %in% sources) "Legacy",
      if ("next_gen" %in% sources) "Next-gen"
    )

    source_rows <- bind_rows(
      if ("legacy" %in% sources && nrow(consensus_rows()) > 0) consensus_rows() %>% mutate(source = "Legacy") else tibble(),
      if ("next_gen" %in% sources && nrow(nextgen_consensus_rows()) > 0) nextgen_consensus_rows() %>% mutate(source = "Next-gen") else tibble()
    ) %>%
      filter(!is.na(.data$market), .data$market %in% .env$markets) %>%
      filter(market_line_is_consistent(.))

    if (nrow(source_rows) == 0) {
      built_markets <- bind_rows(
        if ("legacy" %in% sources && nrow(consensus_rows()) > 0) consensus_rows() %>% mutate(source = "Legacy") else tibble(),
        if ("next_gen" %in% sources && nrow(nextgen_consensus_rows()) > 0) nextgen_consensus_rows() %>% mutate(source = "Next-gen") else tibble()
      ) %>%
        filter(!is.na(.data$market)) %>%
        distinct(source, market) %>%
        arrange(source, market)
      market_msg <- if (nrow(built_markets) == 0) {
        "No source consensus rows have been built yet."
      } else {
        paste(
          paste(built_markets$source, built_markets$market, sep = ": "),
          collapse = "; "
        )
      }
      overall_consensus_status(paste0(
        "No built source consensus rows are available for the dashboard markets. Built source markets: ",
        market_msg
      ))
      return(tibble())
    }

    rows <- source_rows %>%
      mutate(source_pick = case_when(consensus_pick %in% c("Home", "Over") ~ 1, consensus_pick %in% c("Away", "Under") ~ -1, TRUE ~ NA_real_)) %>%
      group_by(game_id, season, week, home_team, away_team, market) %>%
      summarise(
        projections = sum(projections, na.rm = TRUE),
        models_used = sum(models_used, na.rm = TRUE),
        sources_used = paste(sort(unique(source)), collapse = ", "),
        sources_used_count = dplyr::n_distinct(source),
        avg_projection = mean(avg_projection, na.rm = TRUE),
        market_line = first_non_na(market_line),
        spread_line = first_non_na(spread_line),
        total_line = first_non_na(total_line),
        positive_source_picks = sum(source_pick == 1, na.rm = TRUE),
        negative_source_picks = sum(source_pick == -1, na.rm = TRUE),
        source_pick_count = sum(!is.na(source_pick)),
        agree_pct = ifelse(all(is.na(source_pick)), NA_real_, max(mean(source_pick == 1, na.rm = TRUE), mean(source_pick == -1, na.rm = TRUE))),
        positive_avg_projection = mean(avg_projection[source_pick == 1], na.rm = TRUE),
        negative_avg_projection = mean(avg_projection[source_pick == -1], na.rm = TRUE),
        actual_side = first_non_na(actual_side),
        actual_result = first_non_na(actual_result),
        .groups = "drop"
      ) %>%
      mutate(
        positive_avg_projection = ifelse(is.nan(positive_avg_projection), NA_real_, positive_avg_projection),
        negative_avg_projection = ifelse(is.nan(negative_avg_projection), NA_real_, negative_avg_projection),
        avg_edge = ifelse(is.na(market_line), NA_real_, avg_projection - market_line),
        consensus_pick = case_when(
          source_pick_count == 0 ~ NA_character_,
          market %in% c("spread", "straight_up") & positive_source_picks > negative_source_picks ~ "Home",
          market %in% c("spread", "straight_up") & negative_source_picks > positive_source_picks ~ "Away",
          market %in% c("spread", "straight_up") & avg_projection > market_line ~ "Home",
          market %in% c("spread", "straight_up") & avg_projection < market_line ~ "Away",
          market %in% c("spread", "straight_up") ~ NA_character_,
          positive_source_picks > negative_source_picks ~ "Over",
          negative_source_picks > positive_source_picks ~ "Under",
          avg_projection > market_line ~ "Over",
          avg_projection < market_line ~ "Under",
          TRUE ~ NA_character_
        ),
        correct = ifelse(!is.na(actual_side), ifelse(consensus_pick %in% c("Home", "Over"), 1, -1) == actual_side, NA)
      ) %>%
      select(-positive_source_picks, -negative_source_picks, -source_pick_count, -positive_avg_projection, -negative_avg_projection) %>%
      filter(!require_all_sources | sources_used_count == length(required_source_labels)) %>%
      filter(is.na(agree_pct) | agree_pct >= min_agree) %>%
      apply_consensus_edge_threshold(min_edge) %>%
      arrange(market, season, week, game_id)

    overall_consensus_status(paste0(
      "Complete. Built ", nrow(rows), " combined consensus rows across ", length(markets),
      " markets from ", paste(unique(source_rows$source), collapse = " + "),
      ". Source rule: ", if (require_all_sources) "intersection (every selected source must qualify)." else "union (any selected source may qualify).",
      " Minimum edge: ", format(as.numeric(min_edge), trim = TRUE), " points.",
      " Use the Market dropdown to view one market at a time, or the Dashboard to view them together."
    ))
    rows
  }

  observeEvent(input$overall_cons_run, {
    rows <- tryCatch(
      build_overall_consensus_rows(),
      error = function(e) {
        overall_consensus_status(paste("Combined consensus build error:", conditionMessage(e)))
        tibble()
      }
    )
    overall_consensus_rows(rows)
  }, ignoreInit = TRUE, priority = 100)

  output$cons_status <- renderText({
    consensus_status()
  })

  output$ng_cons_status <- renderText({
    nextgen_consensus_status()
  })

  output$overall_cons_status <- renderText({
    overall_consensus_status()
  })

  legacy_consensus_built <- reactive({
    (input$cons_run %||% 0) + (input$cons_run_top %||% 0) > 0
  })

  nextgen_consensus_built <- reactive({
    (input$ng_cons_run %||% 0) + (input$ng_cons_run_top %||% 0) > 0
  })

  consensus_display_rows <- reactive({
    rows <- consensus_rows()
    market <- input$cons_market %||% "spread"
    if (nrow(rows) == 0 || !"market" %in% names(rows)) return(tibble())
    rows %>%
      filter(!is.na(.data$market), .data$market == .env$market)
  })

  nextgen_consensus_display_rows <- reactive({
    rows <- nextgen_consensus_rows()
    market <- input$ng_cons_market %||% "spread"
    if (nrow(rows) == 0 || !"market" %in% names(rows)) return(tibble())
    rows %>%
      filter(!is.na(.data$market), .data$market == .env$market)
  })

  overall_consensus_display_rows <- reactive({
    rows <- overall_consensus_rows()
    market <- input$overall_cons_market %||% "spread"
    if (nrow(rows) == 0 || !"market" %in% names(rows)) return(tibble())
    rows %>%
      filter(!is.na(.data$market), .data$market == .env$market) %>%
      filter(market_line_is_consistent(.))
  })

  output$ng_cons_summary <- renderTable({
    req(nextgen_consensus_built())
    summarize_consensus_table(nextgen_consensus_display_rows())
  })

  output$ng_cons_splits <- renderTable({
    req(nextgen_consensus_built())
    consensus_splits_table(nextgen_consensus_display_rows(), input$ng_cons_market %||% "spread")
  })

  output$ng_cons_games <- renderDT({
    req(nextgen_consensus_built())
    if (nrow(nextgen_consensus_display_rows()) == 0) {
      return(datatable(tibble(Message = "No next-gen consensus rows for the current selections."), rownames = FALSE))
    }
    rows <- nextgen_consensus_display_rows() %>%
      mutate(
        season = as.integer(season),
        week = as.integer(week),
        avg_projection = sprintf("%.1f", avg_projection),
        market_line = ifelse(is.na(market_line), NA_character_, sprintf("%.1f", market_line)),
        avg_edge = ifelse(is.na(avg_edge), NA_character_, sprintf("%.1f", avg_edge)),
        actual_result = ifelse(is.na(actual_result), NA_character_, sprintf("%.1f", actual_result)),
        agree_pct = ifelse(is.na(agree_pct), NA_character_, paste0(sprintf("%.0f", 100 * agree_pct), "%")),
        actual_side = ifelse(is.na(actual_side), NA_character_, as.character(as.integer(actual_side)))
      )
    datatable(
      rows,
      rownames = FALSE,
      filter = "top",
      options = list(
        dom = '<"top"lfrip>t<"bottom"lfrip>',
        pageLength = 25,
        lengthMenu = c(10, 25, 50, 100),
        scrollX = TRUE
      )
    )
  })

  output$overall_cons_summary <- renderTable({
    req(input$overall_cons_run > 0)
    rows <- overall_consensus_display_rows()
    if (nrow(rows) == 0) return(tibble(Message = "No combined consensus rows for the currently selected market. Click Build combined consensus."))
    summarize_consensus_table(rows)
  })

  output$overall_cons_splits <- renderTable({
    req(input$overall_cons_run > 0)
    rows <- overall_consensus_display_rows()
    if (nrow(rows) == 0) return(tibble(Message = "No combined consensus rows for the currently selected market. Click Build combined consensus."))
    consensus_splits_table(rows, input$overall_cons_market %||% "spread")
  })

  output$overall_cons_games <- renderDT({
    req(input$overall_cons_run > 0)
    if (nrow(overall_consensus_display_rows()) == 0) {
      return(datatable(tibble(Message = "No combined consensus rows for the currently selected market. Click Build combined consensus."), rownames = FALSE))
    }
    rows <- overall_consensus_display_rows() %>%
      mutate(
        season = as.integer(season),
        week = as.integer(week),
        avg_projection = sprintf("%.1f", avg_projection),
        market_line = ifelse(is.na(market_line), NA_character_, sprintf("%.1f", market_line)),
        avg_edge = ifelse(is.na(avg_edge), NA_character_, sprintf("%.1f", avg_edge)),
        actual_result = ifelse(is.na(actual_result), NA_character_, sprintf("%.1f", actual_result)),
        agree_pct = ifelse(is.na(agree_pct), NA_character_, paste0(sprintf("%.0f", 100 * agree_pct), "%")),
        actual_side = ifelse(is.na(actual_side), NA_character_, as.character(as.integer(actual_side)))
      )
    datatable(
      rows,
      rownames = FALSE,
      filter = "top",
      options = list(
        dom = '<"top"lfrip>t<"bottom"lfrip>',
        pageLength = 25,
        lengthMenu = c(10, 25, 50, 100),
        scrollX = TRUE
      )
    )
  })

  dashboard_source_rows <- reactive({
    source <- input$dashboard_source %||% "combined"
    rows <- switch(
      source,
      legacy = consensus_rows(),
      next_gen = nextgen_consensus_rows(),
      combined = overall_consensus_rows(),
      tibble()
    )
    if (nrow(rows) == 0) return(tibble())
    if (!"market" %in% names(rows)) rows <- mutate(rows, market = NA_character_)
    rows
  })

  observe({
    rows <- dashboard_source_rows()
    seasons <- if (nrow(rows) == 0) character() else sort(unique(as.integer(rows$season)))
    current <- isolate(input$dashboard_season)
    selected <- if (length(seasons) == 0) character() else if (length(current) > 0 && current %in% as.character(seasons)) current else as.character(max(seasons, na.rm = TRUE))
    updateSelectInput(session, "dashboard_season", choices = stats::setNames(as.character(seasons), as.character(seasons)), selected = selected)
  })

  observe({
    rows <- dashboard_source_rows()
    selected_season <- suppressWarnings(as.integer(input$dashboard_season))
    weeks <- if (nrow(rows) == 0 || length(selected_season) == 0 || is.na(selected_season)) {
      character()
    } else {
      sort(unique(as.integer(rows$week[rows$season == selected_season])))
    }
    current <- isolate(input$dashboard_week)
    upcoming <- if (length(weeks) == 0 || length(selected_season) == 0 || is.na(selected_season)) {
      integer()
    } else {
      model_week_dates %>%
        filter(
          season == selected_season,
          week %in% weeks,
          last_game_date >= as.Date(Sys.time(), tz = "America/New_York")
        ) %>%
        pull(week)
    }
    default_week <- if (length(upcoming) > 0) min(upcoming) else if (length(weeks) > 0) min(weeks) else NA_integer_
    selected <- if (length(weeks) == 0) character() else if (length(current) > 0 && current %in% as.character(weeks)) current else as.character(default_week)
    week_choices <- if (length(weeks) == 0) character() else stats::setNames(as.character(weeks), paste("Week", weeks))
    updateSelectInput(session, "dashboard_week", choices = week_choices, selected = selected)
  })

  apply_dashboard_spread_overrides <- function(rows) {
    rows %>%
      mutate(
        dashboard_pick_line = case_when(
          market != "spread" ~ NA_real_,
          consensus_pick == "Home" ~ -market_line,
          consensus_pick == "Away" ~ market_line,
          TRUE ~ NA_real_
        )
      ) %>%
      filter(
        !(market == "spread" & isTRUE(input$dashboard_no_minus_3_5_favorites) & dplyr::near(dashboard_pick_line, -3.5)),
        !(market == "spread" & isTRUE(input$dashboard_no_plus_2_5_underdogs) & dplyr::near(dashboard_pick_line, 2.5)),
        !(isTRUE(input$dashboard_no_early_week_games) & game_id %in% early_week_game_ids)
      ) %>%
      select(-dashboard_pick_line)
  }

  dashboard_filtered_rows <- reactive({
    rows <- dashboard_source_rows()
    selected_season <- suppressWarnings(as.integer(input$dashboard_season))
    selected_week <- suppressWarnings(as.integer(input$dashboard_week))
    markets <- input$dashboard_markets %||% character()
    if (nrow(rows) == 0 || length(selected_season) == 0 || length(selected_week) == 0 || is.na(selected_season) || is.na(selected_week)) return(tibble())
    rows %>%
      filter(season == selected_season, week == selected_week, market %in% markets) %>%
      apply_dashboard_spread_overrides()
  })

  format_dashboard_export_rows <- function(rows) {
    if (nrow(rows) == 0) return(tibble())

    source_key <- input$dashboard_source %||% "combined"
    source_label <- recode(
      source_key,
      legacy = "Legacy consensus",
      next_gen = "Next-gen consensus",
      combined = "Combined consensus",
      .default = source_key
    )
    line_source <- switch(
      source_key,
      legacy = input$cons_line_source %||% "closing",
      next_gen = input$ng_cons_line_source %||% "closing",
      combined = paste(unique(c(input$cons_line_source %||% "closing", input$ng_cons_line_source %||% "closing")), collapse = "+"),
      NA_character_
    )
    injury_adjustment <- switch(
      source_key,
      legacy = input$cons_injury_source %||% "apply",
      next_gen = input$ng_cons_injury_source %||% "apply",
      combined = paste(unique(c(input$cons_injury_source %||% "apply", input$ng_cons_injury_source %||% "apply")), collapse = "+"),
      NA_character_
    )
    offseason_amortization <- switch(
      source_key,
      legacy = isTRUE(input$cons_apply_amortization),
      next_gen = isTRUE(input$ng_cons_apply_amortization),
      combined = paste(unique(c(isTRUE(input$cons_apply_amortization), isTRUE(input$ng_cons_apply_amortization))), collapse = "+"),
      NA
    )

    rows %>%
      mutate(
        consensus_source = source_label,
        line_source = line_source,
        injury_adjustment = injury_adjustment,
        offseason_amortization = offseason_amortization,
        matchup = paste(away_team, "@", home_team),
        market_label = recode(
          market,
          spread = "Against the spread",
          straight_up = "Straight up",
          total = "Over / under",
          home_implied = "Home implied",
          away_implied = "Away implied",
          .default = market
        ),
        agreement_pct = 100 * agree_pct,
        result_status = case_when(
          correct %in% TRUE ~ "Win",
          correct %in% FALSE ~ "Loss",
          !is.na(actual_result) ~ "Push",
          TRUE ~ "Future"
        )
      ) %>%
      select(
        consensus_source,
        line_source,
        injury_adjustment,
        offseason_amortization,
        game_id,
        season,
        week,
        away_team,
        home_team,
        matchup,
        market,
        market_label,
        consensus_pick,
        spread_line,
        total_line,
        market_line,
        avg_projection,
        avg_edge,
        agreement_pct,
        projections,
        models_used,
        any_of(c("sources_used", "sources_used_count")),
        actual_result,
        actual_side,
        result_status,
        correct
      ) %>%
      arrange(market, desc(abs(avg_edge)), away_team, home_team)
  }

  dashboard_export_rows <- reactive({
    format_dashboard_export_rows(dashboard_filtered_rows())
  })

  dashboard_export_rows_for_market <- function(market_key) {
    rows <- dashboard_source_rows()
    selected_season <- suppressWarnings(as.integer(input$dashboard_season))
    selected_week <- suppressWarnings(as.integer(input$dashboard_week))
    if (
      nrow(rows) == 0 ||
        length(selected_season) == 0 ||
        length(selected_week) == 0 ||
        is.na(selected_season) ||
        is.na(selected_week)
    ) {
      return(tibble())
    }

    rows %>%
      filter(
        season == selected_season,
        week == selected_week,
        market == market_key
      ) %>%
      apply_dashboard_spread_overrides() %>%
      format_dashboard_export_rows()
  }

  dashboard_market_file_suffix <- function(markets) {
    markets <- unique(markets %||% character())
    if (length(markets) == 1) return(markets[[1]])
    if (length(markets) > 1) return("multiple_markets")
    "no_markets"
  }

  dashboard_download_filename <- function(market_suffix) {
    source_key <- input$dashboard_source %||% "combined"
    selected_season <- input$dashboard_season %||% "season"
    selected_week <- input$dashboard_week %||% "week"
    paste0(
      "nfl_consensus_", source_key, "_", selected_season,
      "_week_", selected_week, "_", Sys.Date(), "_", market_suffix, ".csv"
    )
  }

  dashboard_table_for_market <- function(market_key) {
    rows <- dashboard_filtered_rows()
    source_rows <- dashboard_source_rows()
    if (nrow(rows) == 0 || !market_key %in% (input$dashboard_markets %||% character())) {
      return(tibble(Message = "No built consensus rows for this source, season, week, and market."))
    }
    market_rows <- rows %>% filter(market == market_key)
    if (nrow(market_rows) == 0) {
      return(tibble(Message = "No built consensus rows for this market. Build that market in its consensus tab first."))
    }
    history <- source_rows %>%
      filter(market == market_key, !is.na(correct), !is.na(consensus_pick)) %>%
      group_by(consensus_pick) %>%
      summarise(
        hist_n = n(),
        hist_win_pct = mean(correct %in% TRUE),
        .groups = "drop"
      )
    market_rows %>%
      left_join(history, by = "consensus_pick") %>%
      transmute(
        Season = as.integer(season),
        Week = as.integer(week),
        Game = paste(away_team, "@", home_team),
        Pick = consensus_pick,
        Projection = sprintf("%.1f", avg_projection),
        Line = ifelse(is.na(market_line), NA_character_, sprintf("%.1f", market_line)),
        Edge = ifelse(is.na(avg_edge), NA_character_, sprintf("%.1f", avg_edge)),
        Agreement = ifelse(is.na(agree_pct), NA_character_, paste0(sprintf("%.0f", 100 * agree_pct), "%")),
        Models = models_used,
        `Historical Win %` = ifelse(is.na(hist_win_pct), NA_character_, paste0(sprintf("%.1f", 100 * hist_win_pct), "%")),
        `Historical N` = ifelse(is.na(hist_n), NA_integer_, hist_n),
        Result = case_when(
          correct %in% TRUE ~ "Win",
          correct %in% FALSE ~ "Loss",
          is.na(correct) ~ "Future/Push",
          TRUE ~ NA_character_
        ),
        `Actual Result` = ifelse(is.na(actual_result), NA_character_, sprintf("%.1f", actual_result))
      ) %>%
      arrange(desc(abs(suppressWarnings(as.numeric(Edge)))), Game)
  }

  render_dashboard_market <- function(market_key) {
    renderDT({
      datatable(
        dashboard_table_for_market(market_key),
        rownames = FALSE,
        filter = "top",
        options = list(
          dom = '<"top"lfrip>t<"bottom"lfrip>',
          pageLength = 25,
          lengthMenu = c(10, 25, 50, 100),
          scrollX = TRUE
        )
      )
    })
  }

  output$dashboard_summary <- renderTable({
    rows <- dashboard_filtered_rows()
    if (nrow(rows) == 0) {
      return(tibble(Message = "Build a consensus first, then choose a season and week."))
    }
    summary_rows <- rows %>%
      group_by(Market = market) %>%
      summarise(
        Plays = n(),
        Wins = sum(correct %in% TRUE, na.rm = TRUE),
        Losses = sum(correct %in% FALSE, na.rm = TRUE),
        `Future/Push` = sum(is.na(correct)),
        `Win %` = ifelse(sum(!is.na(correct)) > 0, sum(correct %in% TRUE, na.rm = TRUE) / sum(!is.na(correct)), NA_real_),
        .groups = "drop"
      ) %>%
      mutate(
        Market = recode(Market, spread = "Against the spread", straight_up = "Straight up", total = "Over / under", home_implied = "Home implied", away_implied = "Away implied"),
        `Win %` = ifelse(is.na(`Win %`), NA_character_, paste0(sprintf("%.1f", 100 * `Win %`), "%"))
      )

    graded_total <- sum(!is.na(rows$correct))
    bind_rows(
      summary_rows,
      tibble(
        Market = "Total",
        Plays = nrow(rows),
        Wins = sum(rows$correct %in% TRUE, na.rm = TRUE),
        Losses = sum(rows$correct %in% FALSE, na.rm = TRUE),
        `Future/Push` = sum(is.na(rows$correct)),
        `Win %` = ifelse(
          graded_total > 0,
          paste0(sprintf("%.1f", 100 * sum(rows$correct %in% TRUE, na.rm = TRUE) / graded_total), "%"),
          NA_character_
        )
      )
    )
  })

  output$dashboard_spread <- render_dashboard_market("spread")
  output$dashboard_straight_up <- render_dashboard_market("straight_up")
  output$dashboard_total <- render_dashboard_market("total")
  output$dashboard_home_implied <- render_dashboard_market("home_implied")
  output$dashboard_away_implied <- render_dashboard_market("away_implied")

  output$dashboard_download <- downloadHandler(
    filename = function() {
      dashboard_download_filename(dashboard_market_file_suffix(input$dashboard_markets))
    },
    content = function(file) {
      rows <- dashboard_export_rows()
      req(nrow(rows) > 0)
      write_csv(rows, file)
    }
  )

  bind_dashboard_market_download <- function(output_id, market_key) {
    output[[output_id]] <- downloadHandler(
      filename = function() dashboard_download_filename(market_key),
      content = function(file) {
        rows <- dashboard_export_rows_for_market(market_key)
        req(nrow(rows) > 0)
        write_csv(rows, file)
      }
    )
  }

  bind_dashboard_market_download("dashboard_download_spread", "spread")
  bind_dashboard_market_download("dashboard_download_straight_up", "straight_up")
  bind_dashboard_market_download("dashboard_download_total", "total")
  bind_dashboard_market_download("dashboard_download_home_implied", "home_implied")
  bind_dashboard_market_download("dashboard_download_away_implied", "away_implied")

  output$cons_summary <- renderTable({
    req(legacy_consensus_built())
    df <- consensus_display_rows()
    if (nrow(df) == 0) return(tibble(Message = "No consensus rows for the current selections."))
    summarize_consensus <- function(data, label) {
      tibble(
        Season = label,
        Games = nrow(data),
        `Avg projections per game` = mean(data$projections),
        `Avg models per game` = mean(data$models_used),
        `Avg agreement` = mean(data$agree_pct),
        Wins = sum(data$correct %in% TRUE, na.rm = TRUE),
        Losses = sum(data$correct %in% FALSE, na.rm = TRUE),
        Pushes = sum(is.na(data$correct)),
        `Win %` = ifelse(sum(!is.na(data$correct)) > 0, sum(data$correct %in% TRUE, na.rm = TRUE) / sum(!is.na(data$correct)), NA_real_)
      )
    }
    bind_rows(
      summarize_consensus(df, "Total"),
      df %>%
        group_split(season) %>%
        map_dfr(~ summarize_consensus(.x, as.character(first(.x$season))))
    ) %>%
      mutate(
        `Avg projections per game` = sprintf("%.0f", `Avg projections per game`),
        `Avg models per game` = sprintf("%.0f", `Avg models per game`),
        `Avg agreement` = ifelse(is.na(`Avg agreement`), NA_character_, paste0(sprintf("%.1f", 100 * `Avg agreement`), "%")),
        `Win %` = ifelse(is.na(`Win %`), NA_character_, paste0(sprintf("%.1f", 100 * `Win %`), "%"))
      )
  })

  output$cons_splits <- renderTable({
    req(legacy_consensus_built())
    df <- consensus_display_rows()
    if (nrow(df) == 0) return(tibble(Message = "No consensus rows for the current selections."))

    market <- input$cons_market %||% "spread"
    summarize_split <- function(data, category, split_label) {
      graded <- sum(!is.na(data$correct))
      tibble(
        Category = category,
        Split = split_label,
        Games = nrow(data),
        Wins = sum(data$correct %in% TRUE, na.rm = TRUE),
        Losses = sum(data$correct %in% FALSE, na.rm = TRUE),
        Pushes = sum(is.na(data$correct)),
        `Win %` = ifelse(graded > 0, sum(data$correct %in% TRUE, na.rm = TRUE) / graded, NA_real_)
      )
    }

    classify_favorite_side <- function(spread_line) {
      case_when(
        spread_line > 0 ~ "Home",
        spread_line < 0 ~ "Away",
        spread_line == 0 ~ "Pick'em",
        TRUE ~ NA_character_
      )
    }

    rows <- df %>%
      mutate(
        favorite_side = classify_favorite_side(spread_line),
        selected_team_side = case_when(
          market %in% c("spread", "straight_up") ~ consensus_pick,
          market == "home_implied" ~ "Home",
          market == "away_implied" ~ "Away",
          TRUE ~ NA_character_
        ),
        favorite_role = case_when(
          is.na(selected_team_side) | is.na(favorite_side) ~ NA_character_,
          favorite_side == "Pick'em" ~ "Pick'em",
          selected_team_side == favorite_side ~ "Favorite",
          selected_team_side %in% c("Home", "Away") ~ "Underdog",
          TRUE ~ NA_character_
        ),
        total_favorite_context = case_when(
          favorite_side == "Home" ~ "Home favorite games",
          favorite_side == "Away" ~ "Away favorite games",
          favorite_side == "Pick'em" ~ "Pick'em games",
          TRUE ~ NA_character_
        )
      )

    overall <- summarize_split(rows, "Overall", "All consensus rows")

    pick_splits <- rows %>%
      filter(!is.na(consensus_pick)) %>%
      group_split(consensus_pick) %>%
      map_dfr(~ summarize_split(.x, "Consensus pick", first(.x$consensus_pick)))

    home_away_splits <- if (market %in% c("spread", "straight_up", "home_implied", "away_implied")) {
      rows %>%
        filter(!is.na(selected_team_side)) %>%
        group_split(selected_team_side) %>%
        map_dfr(~ summarize_split(.x, "Home/Away", first(.x$selected_team_side)))
    } else {
      rows %>%
        filter(!is.na(total_favorite_context)) %>%
        group_split(total_favorite_context) %>%
        map_dfr(~ summarize_split(.x, "Favorite context", first(.x$total_favorite_context)))
    }

    favorite_splits <- if (market == "total") {
      tibble()
    } else {
      rows %>%
        filter(!is.na(favorite_role)) %>%
        group_split(favorite_role) %>%
        map_dfr(~ summarize_split(.x, "Favorite/Underdog", first(.x$favorite_role)))
    }

    bind_rows(overall, pick_splits, home_away_splits, favorite_splits) %>%
      mutate(`Win %` = ifelse(is.na(`Win %`), NA_character_, paste0(sprintf("%.1f", 100 * `Win %`), "%")))
  })

  output$cons_games <- renderDT({
    req(legacy_consensus_built())
    if (nrow(consensus_display_rows()) == 0) {
      return(datatable(tibble(Message = "No consensus rows for the current selections."), rownames = FALSE))
    }
    rows <- consensus_display_rows() %>%
      mutate(
        season = as.integer(season),
        week = as.integer(week),
        avg_projection = sprintf("%.1f", avg_projection),
        market_line = ifelse(is.na(market_line), NA_character_, sprintf("%.1f", market_line)),
        avg_edge = ifelse(is.na(avg_edge), NA_character_, sprintf("%.1f", avg_edge)),
        actual_result = ifelse(is.na(actual_result), NA_character_, sprintf("%.1f", actual_result)),
        agree_pct = ifelse(is.na(agree_pct), NA_character_, paste0(sprintf("%.0f", 100 * agree_pct), "%")),
        actual_side = ifelse(is.na(actual_side), NA_character_, as.character(as.integer(actual_side)))
      )
    dt <- datatable(
      rows,
      rownames = FALSE,
      filter = "top",
      options = list(
        dom = '<"top"lfrip>t<"bottom"lfrip>',
        pageLength = 25,
        lengthMenu = c(10, 25, 50, 100),
        scrollX = TRUE
      )
    )
    dt
  })

  output$cons_download <- downloadHandler(
    filename = function() paste0("projection_consensus_", Sys.Date(), ".csv"),
    content = function(file) {
      req(legacy_consensus_built())
      write_csv(consensus_rows(), file)
    }
  )

  output$cons_games_download <- downloadHandler(
    filename = function() paste0("projection_consensus_game_rows_", Sys.Date(), ".csv"),
    content = function(file) {
      req(legacy_consensus_built())
      write_csv(consensus_rows(), file)
    }
  )

  output$ng_cons_download <- downloadHandler(
    filename = function() paste0("next_gen_consensus_", Sys.Date(), ".csv"),
    content = function(file) {
      req(nextgen_consensus_built())
      write_csv(nextgen_consensus_rows(), file)
    }
  )

  output$ng_cons_games_download <- downloadHandler(
    filename = function() paste0("next_gen_consensus_game_rows_", Sys.Date(), ".csv"),
    content = function(file) {
      req(nextgen_consensus_built())
      write_csv(nextgen_consensus_rows(), file)
    }
  )
}

shinyApp(ui, server)
