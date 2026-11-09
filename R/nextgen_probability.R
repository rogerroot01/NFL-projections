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

nextgen_probability_ui <- function(id, bundles) {
  ns <- NS(id)
  if (!length(bundles)) {
    return(tags$p(class = "alert alert-info",
                  "No probability backtest is loaded. Run the standalone probability build after the reviewed Next Gen export is ready."))
  }
  bundle <- bundles[[1L]]
  rows <- bundle$predictions
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(ns("season"), "Backtest season", choices = sort(unique(rows$season), decreasing = TRUE)),
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
        h4("Framework comparison"),
        tableOutput(ns("comparison"))
      ),
      h4("Backtest summary"),
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
        bundle$predictions |>
          filter(.data$season == as.integer(input$season), .data$family %in% input$families)
      })
    })
    selected <- reactive(filtered()[[req(input$framework)]])
    ensemble <- reactive(nextgen_probability_ensemble(selected()))
    output$comparison <- renderTable({
      nextgen_probability_comparison(lapply(filtered(), nextgen_probability_ensemble))
    }, striped = TRUE, bordered = TRUE, digits = 4)
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
        transmute(Week = .data$week, Away = .data$away_team, Home = .data$home_team,
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
      arrange(rows, .data$Week, .data$Away)
    })
    output$games <- renderDT({
      datatable(game_table(), rownames = FALSE, options = list(pageLength = 20))
    })
    output$download <- downloadHandler(
      filename = function() paste0("nextgen-probability-", tolower(input$framework), "-", input$season, ".csv"),
      content = function(path) write.csv(game_table(), path, row.names = FALSE, na = "")
    )
  })
}
