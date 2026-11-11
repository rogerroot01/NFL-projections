nextgen_probability_summary <- function(rows) {
  p <- rows$home_win_probability
  y <- rows$actual
  picked <- !is.na(y) & p != 0.5
  wins <- sum((p[picked] > 0.5) == y[picked])
  graded <- !is.na(y)
  correct_probability <- ifelse(y[graded] == 1, p[graded], 1 - p[graded])
  data.frame(Games = nrow(rows), Graded = sum(graded), Wins = wins,
             Losses = sum(picked) - wins, Passes = sum(graded & p == 0.5),
             Accuracy = if (any(picked)) sprintf("%.1f%%", 100 * wins / sum(picked)) else "—",
             Brier = if (any(graded)) round(mean((p[graded] - y[graded])^2), 4) else NA_real_,
             "Log loss" = if (any(graded)) round(-mean(log(pmax(1e-6, correct_probability))), 4) else NA_real_,
             check.names = FALSE)
}

nextgen_probability_ensemble <- function(rows) {
  rows |>
    group_by(.data$game_id, .data$season, .data$week, .data$away_team, .data$home_team,
             .data$away_score, .data$home_score, .data$actual) |>
    summarise(home_win_probability = mean(.data$home_win_probability), .groups = "drop")
}

nextgen_probability_comparison <- function(ensembles) {
  summaries <- lapply(ensembles, function(rows) {
    slices <- list("All weeks" = rows, "Weeks 1–8" = rows[rows$week < 9L, ],
                   "Weeks 9+" = rows[rows$week >= 9L, ])
    bind_rows(lapply(slices, nextgen_probability_summary), .id = "Weeks")
  })
  bind_rows(summaries, .id = "Framework")
}

nextgen_probability_period_rows <- function(rows, period) {
  if (identical(period, "2026")) {
    rows[rows$season == 2026L & is.finite(rows$home_score) & is.finite(rows$away_score), , drop = FALSE]
  } else {
    rows[rows$season %in% c(2024L, 2025L), , drop = FALSE]
  }
}

nextgen_probability_ui <- function(id, bundles) {
  ns <- NS(id)
  if (!length(bundles)) {
    return(tags$p(class = "alert alert-info",
                  "No probability backtest is loaded. Run the standalone probability build after the reviewed Next Gen export is ready."))
  }
  bundle <- bundles[[1L]]
  rows <- bundle$predictions
  current <- nextgen_probability_period_rows(rows, "2026")
  has_current <- nrow(current) > 0L
  periods <- c("2024–2025 backtest" = "backtest")
  if (has_current) periods <- c(periods, "2026 results" = "2026")
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(ns("period"), "Detail period", choices = periods,
                  selected = if (has_current) "2026" else "backtest"),
      selectInput(ns("framework"), "Training framework", choices = names(bundles)),
      checkboxGroupInput(ns("families"), "Families", choices = names(bundle$settings),
                         selected = names(bundle$settings)),
      selectInput(ns("week"), "Game table week", choices = c("All weeks" = "", sort(unique(rows$week)))),
      helpText("The ensemble averages the selected families’ probabilities equally.")
    ),
    mainPanel(
      tags$p(class = "alert alert-info",
             "Both frameworks use the audited Next Gen spread inputs and the same settings selected on 2023. Early trains on all eligible prior weeks. Late trains on Week 9 onward. Both forecast the same held-out games. Scoring excludes tied games."),
      if (length(bundles) > 1L) tagList(
        h4("Framework comparison · 2024–2025 backtest"),
        tableOutput(ns("comparison")),
        if (has_current) tagList(
          h4(sprintf("Framework comparison · 2026 through Week %d", max(current$week))),
          tableOutput(ns("comparison_2026"))
        )
      ),
      textOutput(ns("summary_title"), container = h4),
      tableOutput(ns("summary")),
      h4("Weekly results"),
      DTOutput(ns("weekly")),
      div(class = "section-title-row", h4("Game probabilities"),
          downloadButton(ns("download"), "Download game-level CSV")),
      DTOutput(ns("games"))
    )
  )
}

nextgen_probability_server <- function(id, bundles) {
  if (!length(bundles)) return(invisible(NULL))
  moduleServer(id, function(input, output, session) {
    filtered <- reactive({
      validate(need(length(input$families) > 0L, "Select at least one family."))
      lapply(bundles, function(bundle) {
        filter(bundle$predictions, .data$family %in% input$families)
      })
    })
    comparison <- function(period) {
      ensembles <- lapply(filtered(), function(rows) {
        nextgen_probability_ensemble(nextgen_probability_period_rows(rows, period))
      })
      nextgen_probability_comparison(ensembles)
    }
    selected <- reactive(nextgen_probability_period_rows(
      filtered()[[req(input$framework)]], req(input$period)))
    ensemble <- reactive(nextgen_probability_ensemble(selected()))
    output$comparison <- renderTable(comparison("backtest"), striped = TRUE, bordered = TRUE, digits = 4)
    output$comparison_2026 <- renderTable(comparison("2026"), striped = TRUE, bordered = TRUE, digits = 4)
    output$summary_title <- renderText({
      period <- if (identical(input$period, "2026")) "2026" else "2024–2025 backtest"
      paste("Family results", req(input$framework), period, sep = " · ")
    })
    output$summary <- renderTable({
      by_family <- lapply(split(selected(), selected()$family), nextgen_probability_summary)
      bind_rows(c(list(Ensemble = nextgen_probability_summary(ensemble())), by_family), .id = "Family")
    }, striped = TRUE, bordered = TRUE, digits = 4)
    output$weekly <- renderDT({
      rows <- ensemble()
      by_week <- lapply(split(rows, rows$week), nextgen_probability_summary)
      summary <- bind_rows(by_week, .id = "Week")
      summary$Week <- as.integer(summary$Week)
      datatable(summary, rownames = FALSE, options = list(pageLength = 20))
    })
    game_table <- reactive({
      rows <- ensemble() |>
        transmute(Season = .data$season, Week = .data$week, Away = .data$away_team, Home = .data$home_team,
                  HomeWinPct = round(100 * .data$home_win_probability, 2),
                  Pick = case_when(.data$home_win_probability > 0.5 ~ .data$home_team,
                                   .data$home_win_probability < 0.5 ~ .data$away_team,
                                   TRUE ~ "Pass"),
                  Winner = case_when(.data$actual == 1L ~ .data$home_team,
                                     .data$actual == 0L ~ .data$away_team,
                                     is.finite(.data$home_score) & is.finite(.data$away_score) ~ "Tie",
                                     TRUE ~ "Pending"),
                  Final = if_else(is.finite(.data$home_score) & is.finite(.data$away_score),
                                  paste(.data$away_score, .data$home_score, sep = "–"), NA_character_))
      if (length(input$week) && nzchar(input$week)) rows <- filter(rows, .data$Week == as.integer(input$week))
      names(rows)[names(rows) == "HomeWinPct"] <- "Home win %"
      arrange(rows, .data$Season, .data$Week, .data$Away)
    })
    output$games <- renderDT({
      datatable(game_table(), rownames = FALSE, options = list(pageLength = 20))
    })
    output$download <- downloadHandler(
      filename = function() {
        period <- if (identical(input$period, "2026")) "2026" else "2024-2025"
        paste0("nextgen-probability-", tolower(input$framework), "-", period, ".csv")
      },
      content = function(path) write.csv(game_table(), path, row.names = FALSE, na = "")
    )
  })
}
