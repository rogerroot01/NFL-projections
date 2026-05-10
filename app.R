library(shiny)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tidyr)
library(DT)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

app_data_dir <- file.path(getwd(), "data")
early_lines_path <- file.path(app_data_dir, "early_lines.csv")

early_lines <- if (file.exists(early_lines_path)) {
  read_csv(early_lines_path, show_col_types = FALSE) %>%
    transmute(
      game_id = as.character(.data[["game_id"]]),
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

read_injury_csv <- function(path) {
  if (file.exists(path)) read_csv(path, show_col_types = FALSE) else tibble()
}

tbl_col <- function(df, nm, default = NA) {
  if (nm %in% names(df)) df[[nm]] else rep(default, nrow(df))
}

game_injury_path <- file.path(app_data_dir, "hiddengame_injury_adjustments.csv")
team_injury_2025_path <- file.path(app_data_dir, "hiddengame_team_injury_adjustments_2025.csv")
future_team_injury_2026_path <- file.path(app_data_dir, "hiddengame_future_team_injury_adjustments_2026.csv")

game_injury_raw <- read_injury_csv(game_injury_path)
game_injuries <- tibble(
  game_id = as.character(coalesce(tbl_col(game_injury_raw, "game_id_input"), tbl_col(game_injury_raw, "game_id"))),
  injury_adj = suppressWarnings(as.numeric(coalesce(
    tbl_col(game_injury_raw, "injury_adj"),
    tbl_col(game_injury_raw, "injury_adjust"),
    tbl_col(game_injury_raw, "injury_adjustment")
  )))
)

team_injury_2025_raw <- read_injury_csv(team_injury_2025_path)
team_injuries_2025 <- tibble(
  game_id = as.character(tbl_col(team_injury_2025_raw, "game_id")),
  home_off_injury_adj = suppressWarnings(as.numeric(tbl_col(team_injury_2025_raw, "home_off_injury_adj"))),
  home_def_injury_adj = suppressWarnings(as.numeric(tbl_col(team_injury_2025_raw, "home_def_injury_adj"))),
  away_off_injury_adj = suppressWarnings(as.numeric(tbl_col(team_injury_2025_raw, "away_off_injury_adj"))),
  away_def_injury_adj = suppressWarnings(as.numeric(tbl_col(team_injury_2025_raw, "away_def_injury_adj")))
)

future_team_injury_2026_raw <- read_injury_csv(future_team_injury_2026_path)
future_team_injuries_2026 <- tibble(
  season = suppressWarnings(as.integer(tbl_col(future_team_injury_2026_raw, "season"))),
  week = suppressWarnings(as.integer(tbl_col(future_team_injury_2026_raw, "week"))),
  team = as.character(tbl_col(future_team_injury_2026_raw, "team")),
  off_injury_adj = suppressWarnings(as.numeric(tbl_col(future_team_injury_2026_raw, "off_injury_adj"))),
  def_injury_adj = suppressWarnings(as.numeric(tbl_col(future_team_injury_2026_raw, "def_injury_adj")))
)

family_labels <- c(
  ScoresTrees = "Scores Trees",
  BillyTrees = "Billy Trees",
  ScoresLateReg = "Scores Late Regression",
  Billy = "Billy"
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

compact_data_available <- file.exists(inventory_rds) && file.exists(compact_models_rds)

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
  if (nm %in% names(compact_models)) return(compact_models[[nm]])
  stop("Compact prepared data is missing for ", nm, ". Run prepare_data.R locally and deploy data/compact_models.rds plus data/model_inventory.rds.")
}

file_has_graded_results <- function(path) {
  df <- read_model_file(path)
  cover_cols <- grep("^Cover_", names(df), value = TRUE, ignore.case = TRUE)
  if (length(cover_cols) == 0) return(FALSE)
  any(vapply(cover_cols, function(col) {
    vals <- suppressWarnings(as.numeric(df[[col]]))
    any(!is.na(vals))
  }, logical(1)))
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

detect_cover_summary <- function(df) {
  cover_cols <- grep("^Cover_", names(df), value = TRUE, ignore.case = TRUE)
  if (length(cover_cols) == 0) return(tibble())

  purrr::map_dfr(cover_cols, function(col) {
    vals <- suppressWarnings(as.numeric(df[[col]]))
    proj <- projection_candidates_for_cover(col)
    proj <- proj[proj %in% names(df)][1] %||% NA_character_
    tibble(
      market = cover_market(col),
      market_label = market_labels[cover_market(col)] %||% cover_market(col),
      result_col = col,
      projection_col = proj,
      picks = sum(!is.na(vals)),
      wins = sum(vals == 1, na.rm = TRUE),
      losses = sum(vals == 0, na.rm = TRUE),
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
    h2("Projection Model Wrangler"),
    if (!compact_data_available) {
      div(
        class = "alert alert-danger prepared-badge",
        strong("Prepared data missing. "),
        "Run prepare_data.R and deploy the compact RDS files."
      )
    } else {
      div(
        class = "alert alert-success prepared-badge",
        paste0("Prepared data loaded: ", length(compact_models), " compact model files.")
      )
    }
  ),
  tabsetPanel(
    id = "main_tabs",
    family_tab_ui("scorestrees", "Scores Trees"),
    family_tab_ui("billytrees", "Billy Trees"),
    family_tab_ui("scoreslatereg", "Scores Late Regression"),
    family_tab_ui("billy", "Billy"),
    tabPanel(
      "Consensus",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          checkboxGroupInput("cons_families", "Families", choices = stats::setNames(names(family_labels), family_labels), selected = names(family_labels)),
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
            selected = "none"
          ),
          sliderInput("cons_agree", "Minimum agreement", min = 50, max = 100, value = 60, step = 5, post = "%"),
          sliderInput("cons_min_win", "Minimum model win rate", min = 40, max = 60, value = 40, step = 1, post = "%"),
          actionButton("cons_run", "Build consensus", class = "btn-primary"),
          tags$hr(),
          downloadButton("cons_download", "Download consensus rows")
        ),
        mainPanel(
          h4("Consensus status"),
          verbatimTextOutput("cons_status", placeholder = TRUE),
          tags$hr(),
          h4("Consensus summary"),
          tableOutput("cons_summary"),
          tags$hr(),
          div(
            class = "section-title-row",
            h4("Consensus game-level rows"),
            downloadButton("cons_games_download", "Download game-level CSV")
          ),
          DTOutput("cons_games")
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
        out <- detect_cover_summary(read_model_file(path))
        if (nrow(out) == 0) return(tibble())
        if (!identical(market, "all")) out <- filter(out, market == !!market)
        if (nrow(out) == 0) return(tibble())
        mutate(out, Season = season, File = file, Split = split, .before = 1)
      })

      if (nrow(summary_rows) == 0) return(tibble())

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

  observe({
    session$sendCustomMessage("toggleMinWinSlider", identical(input$cons_market, "straight_up"))
  })

  output$file_table <- renderTable({
    inventory %>%
      select(Season = season, Family = family_label, Split = split, File = file)
  })

  get_line <- function(df, col) if (col %in% names(df)) suppressWarnings(as.numeric(df[[col]])) else rep(NA_real_, nrow(df))

  first_non_na <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA_real_ else x[[1]]
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

  historical_model_scores <- function(market) {
    score_market <- if (identical(market, "straight_up")) {
      "spread"
    } else if (market %in% c("home_implied", "away_implied")) {
      c("team_implied", "opp_implied")
    } else {
      market
    }
    selected <- inventory %>% filter(season %in% backtest_seasons)
    rows <- pmap_dfr(selected, function(path, file, season, family, family_label, split) {
      detect_cover_summary(read_model_file(path)) %>%
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

  long_predictions_for_file <- function(meta, market, min_win_pct = 0.40, line_source = "closing", injury_source = "none") {
    df <- read_model_file(meta$path)
    cols <- projection_columns_for_market(df, market)
    if (length(cols) == 0) return(tibble())

    score_lookup <- detect_cover_summary(df)
    score_market <- if (identical(market, "straight_up")) {
      "spread"
    } else if (market %in% c("home_implied", "away_implied")) {
      c("team_implied", "opp_implied")
    } else {
      market
    }
    if (!identical(score_market, "all")) score_lookup <- filter(score_lookup, market %in% score_market)
    score_lookup <- score_lookup %>%
      filter(!is.na(projection_col)) %>%
      select(projection_col, model_result_col = result_col, model_picks = picks, model_win_pct = win_pct)
    if (nrow(score_lookup) == 0 || all(is.na(score_lookup$model_win_pct))) {
      score_lookup <- historical_model_scores(market) %>%
        filter(family == meta$family, split == meta$split) %>%
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
    if (identical(injury_source, "apply")) {
      future_home_injuries <- future_team_injuries_2026 %>%
        rename(
          home_team = team,
          future_home_off_injury_adj = off_injury_adj,
          future_home_def_injury_adj = def_injury_adj
        )
      future_away_injuries <- future_team_injuries_2026 %>%
        rename(
          away_team = team,
          future_away_off_injury_adj = off_injury_adj,
          future_away_def_injury_adj = def_injury_adj
        )

      base <- base %>%
        left_join(game_injuries, by = "game_id") %>%
        left_join(team_injuries_2025, by = "game_id") %>%
        left_join(future_home_injuries, by = c("season", "week", "home_team")) %>%
        left_join(future_away_injuries, by = c("season", "week", "away_team")) %>%
        mutate(
          home_off_injury_adj = coalesce(home_off_injury_adj, future_home_off_injury_adj, 0),
          home_def_injury_adj = coalesce(home_def_injury_adj, future_home_def_injury_adj, 0),
          away_off_injury_adj = coalesce(away_off_injury_adj, future_away_off_injury_adj, 0),
          away_def_injury_adj = coalesce(away_def_injury_adj, future_away_def_injury_adj, 0),
          home_score_injury_adj = home_off_injury_adj + away_def_injury_adj,
          away_score_injury_adj = away_off_injury_adj + home_def_injury_adj,
          split_spread_injury_adj = home_score_injury_adj - away_score_injury_adj,
          spread_injury_adj = coalesce(injury_adj, split_spread_injury_adj, 0),
          total_injury_adj = home_score_injury_adj + away_score_injury_adj
        ) %>%
        select(
          -injury_adj,
          -home_off_injury_adj,
          -home_def_injury_adj,
          -away_off_injury_adj,
          -away_def_injury_adj,
          -future_home_off_injury_adj,
          -future_home_def_injury_adj,
          -future_away_off_injury_adj,
          -future_away_def_injury_adj
        )
    } else {
      base <- base %>%
        mutate(
          spread_injury_adj = 0,
          total_injury_adj = 0,
          home_score_injury_adj = 0,
          away_score_injury_adj = 0
        )
    }

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
        pred <- pred + base$spread_injury_adj
      } else if (market == "total") {
        pred <- pred + base$total_injury_adj
      } else if (market == "home_implied") {
        pred <- pred + base$home_score_injury_adj
      } else if (market == "away_implied") {
        pred <- pred + base$away_score_injury_adj
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
    market <- input$cons_market %||% "spread"
    line_source <- input$cons_line_source %||% "closing"
    injury_source <- input$cons_injury_source %||% "none"
    min_win <- if (identical(market, "straight_up")) 0 else (input$cons_min_win %||% 40) / 100
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
      long <- pmap_dfr(selected, function(path, file, season, family, family_label, split) {
        long_predictions_for_file(
          tibble(path = path, file = file, season = season, family = family, family_label = family_label, split = split),
          market,
          min_win,
          line_source,
          injury_source
        )
      })

      if (nrow(long) == 0) {
        consensus_status("No projections passed the current selections.")
        return(tibble())
      }

      incProgress(0.7, detail = "Averaging model signals")
      rows <- long %>%
        group_by(game_id, season, week, home_team, away_team) %>%
        summarise(
          projections = n(),
          models_used = n_distinct(paste(file, projection_col, sep = "::")),
          avg_projection = mean(projection, na.rm = TRUE),
          market_line = first_non_na(market_line),
          avg_edge = ifelse(is.na(market_line), NA_real_, avg_projection - market_line),
          agree_pct = ifelse(all(is.na(pick)), NA_real_, max(mean(pick == 1, na.rm = TRUE), mean(pick == -1, na.rm = TRUE))),
          consensus_pick = case_when(
            all(is.na(pick)) ~ NA_character_,
            market %in% c("spread", "straight_up") & sum(pick == 1, na.rm = TRUE) > sum(pick == -1, na.rm = TRUE) ~ "Home",
            market %in% c("spread", "straight_up") & sum(pick == -1, na.rm = TRUE) > sum(pick == 1, na.rm = TRUE) ~ "Away",
            market %in% c("spread", "straight_up") & avg_projection > market_line ~ "Home",
            market %in% c("spread", "straight_up") & avg_projection < market_line ~ "Away",
            market %in% c("spread", "straight_up") ~ NA_character_,
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
        arrange(season, week, game_id)
      consensus_status(paste0("Complete. Built ", nrow(rows), " consensus rows from ", nrow(selected), " model files."))
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

  output$cons_status <- renderText({
    consensus_status()
  })

  output$cons_summary <- renderTable({
    req(input$cons_run > 0)
    df <- consensus_rows()
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

  output$cons_games <- renderDT({
    req(input$cons_run > 0)
    if (nrow(consensus_rows()) == 0) {
      return(datatable(tibble(Message = "No consensus rows for the current selections."), rownames = FALSE))
    }
    rows <- consensus_rows() %>%
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
      req(input$cons_run > 0)
      write_csv(consensus_rows(), file)
    }
  )

  output$cons_games_download <- downloadHandler(
    filename = function() paste0("projection_consensus_game_rows_", Sys.Date(), ".csv"),
    content = function(file) {
      req(input$cons_run > 0)
      write_csv(consensus_rows(), file)
    }
  )
}

shinyApp(ui, server)
