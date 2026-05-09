library(shiny)
library(DT)
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
  cover_cols <- grep("^Cover_", names(df), value = TRUE)
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

prediction_cols <- function(df) {
  prediction_cols_from_names(names(df))
}

key_cols <- function(df) {
  key_cols_from_names(names(df))
}

family_tab_ui <- function(id, label) {
  tabPanel(
    label,
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput(paste0(id, "_season"), "Season", choices = sort(unique(inventory$season)), selected = max(inventory$season)),
        selectInput(paste0(id, "_split"), "File", choices = NULL),
        selectInput(paste0(id, "_market"), "Market", choices = market_labels, selected = "all"),
        selectInput(paste0(id, "_result_col"), "Game table result column", choices = NULL),
        actionButton(paste0(id, "_load"), "Load selected file", class = "btn-primary"),
        actionButton(paste0(id, "_reload"), "Clear cache", class = "btn-default"),
        tags$hr(),
        downloadButton(paste0(id, "_download_file"), "Download selected file")
      ),
        mainPanel(
          tags$p(tags$small("Choose a file and click Load selected file. This diagnostic version renders only small base tables first so we can avoid server disconnects.")),
          h4("Backtest summary"),
          tableOutput(paste0(id, "_summary")),
          tags$hr(),
          h4("Prediction columns"),
          tableOutput(paste0(id, "_pred_cols")),
          tags$hr(),
          h4("Game-level rows"),
          tableOutput(paste0(id, "_games"))
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
          selectInput("cons_market", "Market", choices = c(spread = "Spread", total = "Total", home_implied = "Home implied", away_implied = "Away implied"), selected = "spread"),
          sliderInput("cons_agree", "Minimum agreement", min = 50, max = 100, value = 60, step = 5, post = "%"),
          actionButton("cons_run", "Build consensus", class = "btn-primary"),
          tags$hr(),
          downloadButton("cons_download", "Download consensus rows")
        ),
        mainPanel(
          h4("Consensus summary"),
          DTOutput("cons_summary"),
          tags$hr(),
          h4("Consensus game-level rows"),
          DTOutput("cons_games")
        )
      )
    ),
    tabPanel(
      "Files",
      h4("Loaded app data files"),
      DTOutput("file_table")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$scorestrees_reload, { rm(list = ls(file_cache), envir = file_cache) }, ignoreInit = TRUE)
  observeEvent(input$billytrees_reload, { rm(list = ls(file_cache), envir = file_cache) }, ignoreInit = TRUE)
  observeEvent(input$scoreslatereg_reload, { rm(list = ls(file_cache), envir = file_cache) }, ignoreInit = TRUE)
  observeEvent(input$billy_reload, { rm(list = ls(file_cache), envir = file_cache) }, ignoreInit = TRUE)

  bind_family <- function(id, family_key) {
    fam_files <- inventory %>% filter(family == family_key)

    observe({
      season <- input[[paste0(id, "_season")]]
      choices <- fam_files %>% filter(season == !!as.integer(season)) %>% mutate(label = paste(split, "-", file))
      updateSelectInput(session, paste0(id, "_split"), choices = stats::setNames(choices$path, choices$label), selected = choices$path[1])
    })

    current_path <- eventReactive(input[[paste0(id, "_load")]], {
      path <- input[[paste0(id, "_split")]]
      req(path)
      path
    }, ignoreInit = TRUE)

    current_df <- reactive({
      read_model_file(current_path())
    })

    current_summary <- reactive({
      out <- detect_cover_summary(current_df())
      market <- input[[paste0(id, "_market")]] %||% "all"
      if (!identical(market, "all")) out <- out %>% filter(market == !!market)
      out
    })

    observe({
      s <- current_summary()
      choices <- stats::setNames(s$result_col, paste0(s$market_label, " - ", s$result_col))
      updateSelectInput(session, paste0(id, "_result_col"), choices = choices, selected = choices[1])
    })

    output[[paste0(id, "_summary")]] <- renderTable({
      req(input[[paste0(id, "_load")]] > 0)
      current_summary() %>%
        mutate(win_pct = round(100 * win_pct, 1)) %>%
        select(Market = market_label, Result = result_col, Projection = projection_col, Picks = picks, Wins = wins, Losses = losses, WinPct = win_pct)
    })

    output[[paste0(id, "_pred_cols")]] <- renderTable({
      req(input[[paste0(id, "_load")]] > 0)
      tibble(PredictionColumn = head(prediction_cols_from_names(file_header(current_path())), 25))
    })

    output[[paste0(id, "_games")]] <- renderTable({
      req(input[[paste0(id, "_load")]] > 0)
      df <- current_df()
      result_col <- input[[paste0(id, "_result_col")]]
      req(result_col)
      proj_col <- projection_candidates_for_cover(result_col)
      proj_col <- proj_col[proj_col %in% names(df)][1] %||% NA_character_
      cols <- unique(stats::na.omit(c(key_cols(df), proj_col, result_col)))
      display <- df %>% select(any_of(cols))
      display %>% dplyr::slice_head(n = 20)
    })

    output[[paste0(id, "_download_file")]] <- downloadHandler(
      filename = function() basename(input[[paste0(id, "_split")]]),
      content = function(file) file.copy(input[[paste0(id, "_split")]], file, overwrite = TRUE)
    )
  }

  bind_family("scorestrees", "ScoresTrees")
  bind_family("billytrees", "BillyTrees")
  bind_family("scoreslatereg", "ScoresLateReg")
  bind_family("billy", "Billy")

  output$file_table <- renderDT({
    inventory %>%
      mutate(size_mb = round(file.info(path)$size / 1024^2, 2)) %>%
      select(Season = season, Family = family_label, Split = split, File = file, `Size MB` = size_mb) %>%
      datatable(rownames = FALSE, options = list(pageLength = 30, scrollX = TRUE))
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

  output$cons_summary <- renderDT({
    req(input$cons_run > 0)
    df <- consensus_rows()
    if (nrow(df) == 0) return(datatable(tibble(Message = "No consensus rows for the current selections."), rownames = FALSE))
    tibble(
      Games = nrow(df),
      `Avg projections per game` = mean(df$projections),
      `Avg agreement` = mean(df$agree_pct),
      Wins = sum(df$correct %in% TRUE, na.rm = TRUE),
      Losses = sum(df$correct %in% FALSE, na.rm = TRUE),
      `Win %` = ifelse(sum(!is.na(df$correct)) > 0, sum(df$correct %in% TRUE, na.rm = TRUE) / sum(!is.na(df$correct)), NA_real_)
    ) %>%
      mutate(`Avg agreement` = scales::percent(`Avg agreement`, accuracy = 0.1), `Win %` = scales::percent(`Win %`, accuracy = 0.1)) %>%
      datatable(rownames = FALSE, options = list(dom = "t"))
  })

  output$cons_games <- renderDT({
    req(input$cons_run > 0)
    consensus_rows() %>%
      dplyr::slice_head(n = 1000) %>%
      mutate(agree_pct = scales::percent(agree_pct, accuracy = 0.1)) %>%
      datatable(rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE, autoWidth = TRUE))
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
