library(shiny)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tidyr)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

app_data_dir <- file.path(getwd(), "data")

family_labels <- c(
  ScoresTrees = "Scores Trees",
  BillyTrees = "Billy Trees",
  ScoresLateReg = "Scores Late Regression",
  Billy = "Billy"
)

market_labels <- c(
  all = "All markets",
  spread = "Spread / score diff",
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
         !str_detect(cols, "^Cover_|_target|_cover$")]
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
        selectInput(paste0(id, "_season"), "Backtest season", choices = backtest_seasons, selected = max(backtest_seasons)),
        selectInput(paste0(id, "_market"), "Market", choices = market_choices, selected = "all"),
        actionButton(paste0(id, "_build"), "Build family summary", class = "btn-primary"),
        tags$hr(),
        downloadButton(paste0(id, "_download_summary"), "Download summary CSV")
      ),
        mainPanel(
          tags$p(tags$small("Click Build family summary to summarize graded files in this family for the selected season. Future seasons with blank cover results are excluded from this backtest selector.")),
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
  tags$head(tags$title("Projection Model Wrangler")),
  titlePanel("Projection Model Wrangler"),
  if (!compact_data_available) {
    div(
      class = "alert alert-danger",
      strong("Prepared data missing. "),
      "Run prepare_data.R locally, then deploy data/compact_models.rds and data/model_inventory.rds with the app."
    )
  } else {
    div(
      class = "alert alert-success",
      paste0("Prepared data loaded: ", length(compact_models), " compact model files.")
    )
  },
  tags$p("Reads the finished projection CSVs in the app data folder and summarizes model output by family, season, file, and market."),
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
          checkboxGroupInput("cons_seasons", "Seasons", choices = sort(unique(inventory$season)), selected = sort(unique(inventory$season))[sort(unique(inventory$season)) < 2026]),
          selectInput(
            "cons_market",
            "Market",
            choices = c("Spread" = "spread", "Total" = "total", "Home implied" = "home_implied", "Away implied" = "away_implied"),
            selected = "spread"
          ),
          sliderInput("cons_agree", "Minimum agreement", min = 50, max = 100, value = 60, step = 5, post = "%"),
          actionButton("cons_run", "Build consensus", class = "btn-primary"),
          tags$hr(),
          downloadButton("cons_download", "Download consensus rows")
        ),
        mainPanel(
          h4("Consensus summary"),
          tableOutput("cons_summary"),
          tags$hr(),
          h4("Consensus game-level rows"),
          tableOutput("cons_games")
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
      inventory %>%
        mutate(
          family_norm = norm_key(family),
          family_label_norm = norm_key(family_label),
          file_norm = norm_key(file)
        ) %>%
        filter(
          season == as.integer(input[[paste0(id, "_season")]]),
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
      files %>%
        pmap_dfr(function(path, file, season, family, family_label, split) {
          out <- detect_cover_summary(read_model_file(path))
          if (nrow(out) == 0) return(tibble())
          if (!identical(market, "all")) out <- out %>% filter(market == !!market)
          if (nrow(out) == 0) return(tibble())
          out %>% mutate(Season = season, File = file, Split = split, .before = 1)
        }) %>% {
          if (nrow(.) == 0) {
            tibble()
          } else {
            . %>%
              mutate(WinPct = round(100 * win_pct, 1)) %>%
              select(Season, File, Split, Market = market_label, Result = result_col, Projection = projection_col, Picks = picks, Wins = wins, Losses = losses, WinPct) %>%
              arrange(desc(WinPct), desc(Picks), File, Result)
          }
        }
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

    output[[paste0(id, "_summary")]] <- renderText({
      req(input[[paste0(id, "_build")]] > 0)
      s <- family_summary() %>% dplyr::slice_head(n = 80)
      if (nrow(s) == 0) {
        fd <- file_debug_summary(family_files_for_season())
        return(paste(
          "No summary rows for this selection.",
          "File debug:",
          paste(capture.output(print(fd, n = 20, width = 180)), collapse = "\n"),
          sep = "\n"
        ))
      }
      paste(capture.output(print(s, n = 80, width = 180)), collapse = "\n")
    })

    output[[paste0(id, "_download_summary")]] <- downloadHandler(
      filename = function() paste0(family_key, "_summary_", input[[paste0(id, "_season")]], "_", Sys.Date(), ".csv"),
      content = function(file) write_csv(family_summary(), file)
    )
  }

  bind_family("scorestrees", "ScoresTrees")
  bind_family("billytrees", "BillyTrees")
  bind_family("scoreslatereg", "ScoresLateReg")
  bind_family("billy", "Billy")

  output$file_table <- renderTable({
    inventory %>%
      select(Season = season, Family = family_label, Split = split, File = file)
  })

  get_line <- function(df, col) if (col %in% names(df)) suppressWarnings(as.numeric(df[[col]])) else rep(NA_real_, nrow(df))

  projection_columns_for_market <- function(df, market) {
    cols <- prediction_cols(df)
    cols <- cols[!str_detect(cols, "^Cover_")]
    if (market == "spread") {
      cols[str_detect(cols, "ScoreDiff|ImpliedScoreDiff|Billy")]
    } else if (market == "total") {
      cols[str_detect(cols, "Total|ScoreTotal|TotalScore")]
    } else if (market == "home_implied" || market == "away_implied") {
      cols[str_detect(cols, "Score_|Score$|OppScore|ImpliedTeamScored|ImpliedOppScored|Score_final")]
    } else {
      character()
    }
  }

  long_predictions_for_file <- function(meta, market) {
    df <- read_model_file(meta$path)
    cols <- projection_columns_for_market(df, market)
    if (length(cols) == 0) return(tibble())

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

    out <- map_dfr(cols, function(col) {
      raw <- suppressWarnings(as.numeric(df[[col]]))
      split <- meta$split
      pred <- raw
      if (market == "spread" && str_detect(split, "^away")) pred <- -pred
      if (market == "home_implied" && str_detect(split, "^away") && str_detect(col, "^(Score_|Score$|Score_final|ImpliedTeamScored)")) pred <- NA_real_
      if (market == "home_implied" && str_detect(split, "^home") && str_detect(col, "OppScore|ImpliedOppScored")) pred <- NA_real_
      if (market == "away_implied" && str_detect(split, "^home") && str_detect(col, "^(Score_|Score$|Score_final|ImpliedTeamScored)")) pred <- NA_real_
      if (market == "away_implied" && str_detect(split, "^away") && str_detect(col, "OppScore|ImpliedOppScored")) pred <- NA_real_

      line <- if (market == "spread") {
        -base$spread_line
      } else if (market == "total") {
        base$total_line
      } else if (market == "home_implied") {
        (base$total_line - base$spread_line) / 2
      } else {
        (base$total_line + base$spread_line) / 2
      }

      actual <- if (market == "spread") {
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
        pick = pick,
        actual_side = actual_side,
        correct = ifelse(!is.na(pick) & !is.na(actual_side), pick == actual_side, NA)
      ))
    })

    out %>% filter(!is.na(projection), !is.na(pick))
  }

  consensus_rows <- eventReactive(input$cons_run, {
    req(input$cons_families, input$cons_seasons, input$cons_market)
    selected <- inventory %>%
      filter(family %in% input$cons_families, season %in% as.integer(input$cons_seasons))

    long <- pmap_dfr(selected, function(path, file, season, family, family_label, split) {
      long_predictions_for_file(tibble(path = path, file = file, season = season, family = family, family_label = family_label, split = split), input$cons_market)
    })

    if (nrow(long) == 0) return(tibble())

    long %>%
      group_by(game_id, season, week, home_team, away_team) %>%
      summarise(
        projections = n(),
        avg_projection = mean(projection, na.rm = TRUE),
        market_line = first(na.omit(market_line)),
        avg_edge = avg_projection - market_line,
        agree_pct = max(mean(pick == 1, na.rm = TRUE), mean(pick == -1, na.rm = TRUE)),
        consensus_pick = ifelse(mean(pick == 1, na.rm = TRUE) >= mean(pick == -1, na.rm = TRUE), "Over/Home", "Under/Away"),
        actual_side = first(na.omit(actual_side)),
        correct = ifelse(!is.na(actual_side), ifelse(consensus_pick == "Over/Home", 1, -1) == actual_side, NA),
        .groups = "drop"
      ) %>%
      filter(agree_pct >= input$cons_agree / 100) %>%
      arrange(season, week, game_id)
  }, ignoreInit = TRUE)

  output$cons_summary <- renderTable({
    req(input$cons_run > 0)
    df <- consensus_rows()
    if (nrow(df) == 0) return(tibble(Message = "No consensus rows for the current selections."))
    tibble(
      Games = nrow(df),
      `Avg projections per game` = mean(df$projections),
      `Avg agreement` = mean(df$agree_pct),
      Wins = sum(df$correct %in% TRUE, na.rm = TRUE),
      Losses = sum(df$correct %in% FALSE, na.rm = TRUE),
      `Win %` = ifelse(sum(!is.na(df$correct)) > 0, sum(df$correct %in% TRUE, na.rm = TRUE) / sum(!is.na(df$correct)), NA_real_)
    ) %>%
      mutate(`Avg agreement` = round(100 * `Avg agreement`, 1), `Win %` = round(100 * `Win %`, 1))
  })

  output$cons_games <- renderTable({
    req(input$cons_run > 0)
    consensus_rows() %>%
      dplyr::slice_head(n = 1000) %>%
      mutate(agree_pct = round(100 * agree_pct, 1)) %>%
      dplyr::slice_head(n = 50)
  })

  output$cons_download <- downloadHandler(
    filename = function() paste0("projection_consensus_", Sys.Date(), ".csv"),
    content = function(file) {
      req(input$cons_run > 0)
      write_csv(consensus_rows(), file)
    }
  )
}

shinyApp(ui, server)
