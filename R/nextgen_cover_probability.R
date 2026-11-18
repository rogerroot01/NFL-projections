nextgen_ats_reconcile_pairs <- function(probability, home, away, decisive) {
  total <- probability[home] + probability[away]
  scale <- pmax(1, total)
  scale[decisive] <- total[decisive]
  for (side in list(home, away)) {
    probability[side] <- ifelse(scale > 0, probability[side] / scale, 0.5)
  }
  probability
}


nextgen_ats_probability_data <- function(cover, joint, win) {
  keys <- c("game_id", "family", "posteam")
  cover <- cover$predictions
  joint <- joint$predictions
  stopifnot(!anyDuplicated(cover[keys]), !anyDuplicated(joint[keys]))
  make_key <- function(rows) do.call(paste, c(rows[keys], sep = "|"))
  match_joint <- match(make_key(cover), make_key(joint))
  stopifnot(nrow(cover) == nrow(joint), !anyNA(match_joint))
  stopifnot(all(cover$team_cover_threshold == joint$team_cover_threshold[match_joint]),
            all(cover$actual >= joint$actual[match_joint], na.rm = TRUE))
  win_rows <- win$predictions
  win_key <- paste(win_rows$game_id, win_rows$family, sep = "|")
  stopifnot(!anyDuplicated(win_key))
  home_win <- win_rows$home_win_probability[match(paste(cover$game_id, cover$family, sep = "|"), win_key)]
  stopifnot(!anyNA(home_win))
  pair_key <- paste(cover$game_id, cover$family, sep = "|")
  home <- which(cover$team_is_home == 1L)
  away <- which(cover$team_is_home == 0L)
  stopifnot(!anyDuplicated(pair_key[home]), !anyDuplicated(pair_key[away]))
  away <- away[match(pair_key[home], pair_key[away])]
  stopifnot(!anyNA(away), length(home) * 2L == nrow(cover))
  decisive <- cover$team_cover_threshold[home] %% 1 != 0
  cover$probability <- nextgen_ats_reconcile_pairs(cover$probability, home, away, decisive)
  joint_probability <- nextgen_ats_reconcile_pairs(joint$probability[match_joint], home, away, FALSE)

  dplyr::rename(cover, cover_probability = probability, actual_cover = actual) |>
    dplyr::mutate(joint_probability = joint_probability,
                  actual_joint = joint$actual[match_joint],
                  win_probability = ifelse(team_is_home == 1L, home_win, 1 - home_win))
}

nextgen_ats_events <- c("Cover" = "cover", "Win and cover" = "joint",
                        "Win × cover (independence benchmark)" = "product")

nextgen_ats_ensemble <- function(rows) {
  rows |>
    dplyr::group_by(game_id, season, week, home_team, away_team, posteam, team_is_home,
                    home_score, away_score, team_cover_threshold, actual_cover, actual_joint) |>
    dplyr::summarise(dplyr::across(c(cover_probability, joint_probability, win_probability), mean),
                     .groups = "drop")
}

nextgen_ats_selected_event <- function(rows, event) {
  rows$probability <- switch(event, cover = rows$cover_probability, joint = rows$joint_probability,
                             product = rows$win_probability * rows$cover_probability)
  rows$actual <- if (event == "cover") rows$actual_cover else rows$actual_joint
  rows
}

nextgen_ats_game_rows <- function(rows) {
  id <- c("game_id", "season", "week", "away_team", "home_team", "away_score", "home_score")
  home <- rows[rows$team_is_home == 1L, ]
  away <- rows[rows$team_is_home == 0L, ]
  stopifnot(!anyDuplicated(home$game_id), !anyDuplicated(away$game_id))
  paired <- match(home$game_id, away$game_id)
  stopifnot(!anyNA(paired), nrow(home) == nrow(away))
  away <- away[paired, ]
  res <- home[id]
  res$home_spread <- -home$team_cover_threshold
  res$home_probability <- home$probability
  res$away_probability <- away$probability
  res$picked_team <- ifelse(home$probability == away$probability, "Pass",
                           ifelse(home$probability > away$probability, as.character(home$home_team), as.character(home$away_team)))
  res$event_hits <- ifelse(res$picked_team == res$home_team, home$actual,
                           ifelse(res$picked_team == res$away_team, away$actual, NA_integer_))
  cover_margin <- home$home_score - home$away_score - home$team_cover_threshold
  res$ats_result <- ifelse(is.na(cover_margin), "Pending",
                    ifelse(res$picked_team == "Pass", "Pass",
                    ifelse(cover_margin == 0, "Push",
                    ifelse((cover_margin > 0) == (res$picked_team == res$home_team), "Win", "Loss"))))
  res
}

nextgen_ats_probability_summary <- function(rows, event) {
  rows <- nextgen_ats_selected_event(rows, event)
  graded <- rows[!is.na(rows$actual), ]
  games <- nextgen_ats_game_rows(rows)
  wins <- sum(games$ats_result == "Win")
  losses <- sum(games$ats_result == "Loss")
  pushes <- sum(games$ats_result == "Push")
  decided <- !is.na(games$event_hits)
  correct <- ifelse(graded$actual == 1L, graded$probability, 1 - graded$probability)
  data.frame(Games = nrow(games), Graded = nrow(graded) %/% 2L,
    Hits = sum(games$event_hits[decided] == 1L),
    "Hit %" = if (any(decided)) sprintf("%.1f%%", 100 * mean(games$event_hits[decided])) else "—",
    "ATS record" = paste(wins, losses, pushes, sep = "-"),
    "ATS win %" = if (wins + losses) sprintf("%.1f%%", 100 * wins / (wins + losses)) else "—",
    Brier = if (nrow(graded)) round(mean((graded$probability - graded$actual)^2), 4) else NA_real_,
    "Log loss" = if (nrow(graded)) round(-mean(log(pmax(1e-6, correct))), 4) else NA_real_,
    check.names = FALSE)
}

nextgen_ats_probability_ui <- function(id, data) {
  ns <- shiny::NS(id)
  periods <- c("2026 upcoming" = "upcoming", "2026 results" = "2026",
               "2024–2025 backtest" = "backtest")
  shiny::sidebarLayout(
    shiny::sidebarPanel(width = 3,
      shiny::selectInput(ns("period"), "Detail period", periods),
      shiny::selectInput(ns("event"), "Outcome", nextgen_ats_events),
      shiny::checkboxGroupInput(ns("families"), "Early model families", unique(data$family),
                                selected = unique(data$family)),
      shiny::selectInput(ns("week"), "Game table week", c("All weeks" = "", sort(unique(data$week)))),
      shiny::helpText("The ensemble averages the selected families equally.")),
    shiny::mainPanel(
      shiny::tags$p(class = "alert alert-info",
        "Each model uses the saved spread. Pushes count as neither team covering. The ATS record shows pushes separately. The win × cover view is an independence benchmark."),
      shiny::h4("Outcome comparison"),
      shiny::tableOutput(ns("comparison")),
      shiny::h4("Family results"),
      shiny::tableOutput(ns("summary")),
      shiny::h4("Weekly results"),
      DT::DTOutput(ns("weekly")),
      shiny::div(class = "section-title-row", shiny::h4("Game probabilities"),
                 shiny::downloadButton(ns("download"), "Download game-level CSV")),
      DT::DTOutput(ns("games"))
    ))
}

nextgen_ats_probability_server <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    selected <- shiny::reactive({
      shiny::validate(shiny::need(length(input$families) > 0L, "Select at least one family."))
      rows <- data[data$family %in% input$families, ]
      current <- rows$season == 2026L
      pending <- is.na(rows$actual_cover)
      keep <- switch(shiny::req(input$period), upcoming = current & pending,
                     "2026" = current & !pending, backtest = !current & !pending)
      shiny::validate(shiny::need(any(keep), "No games match this period."))
      rows[keep, ]
    })
    ensemble <- shiny::reactive(nextgen_ats_ensemble(selected()))
    event <- shiny::reactive(shiny::req(input$event))

    output$comparison <- shiny::renderTable({
      rows <- ensemble()
      comparison <- lapply(nextgen_ats_events, function(event) nextgen_ats_probability_summary(rows, event))
      dplyr::bind_rows(comparison, .id = "Outcome")
    }, striped = TRUE, bordered = TRUE, digits = 4)

    output$summary <- shiny::renderTable({
      families <- split(selected(), selected()$family)
      summary <- lapply(c(list(Ensemble = ensemble()), families),
                        nextgen_ats_probability_summary, event = event())
      dplyr::bind_rows(summary, .id = "Family")
    }, striped = TRUE, bordered = TRUE, digits = 4)

    output$weekly <- DT::renderDT({
      rows <- ensemble()
      summary <- lapply(split(rows, rows$week), nextgen_ats_probability_summary, event = event())
      table <- dplyr::bind_rows(summary, .id = "Week")
      table$Week <- as.integer(table$Week)
      DT::datatable(table, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
    })

    game_table <- shiny::reactive({
      rows <- nextgen_ats_game_rows(nextgen_ats_selected_event(ensemble(), event()))
      if (!is.null(input$week) && nzchar(input$week)) {
        rows <- rows[rows$week == as.integer(input$week), ]
      }
      rows <- rows[order(rows$season, rows$week, rows$away_team), ]
      data.frame(Season = rows$season, Week = rows$week, Away = rows$away_team, Home = rows$home_team,
        "Home spread" = rows$home_spread, "Away %" = round(100 * rows$away_probability, 2),
        "Home %" = round(100 * rows$home_probability, 2), Pick = rows$picked_team,
        "Event hit" = ifelse(is.na(rows$event_hits), "", ifelse(rows$event_hits == 1L, "Yes", "No")),
        "ATS result" = rows$ats_result, check.names = FALSE)
    })
    output$games <- DT::renderDT({
      DT::datatable(game_table(), rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
    })
    output$download <- shiny::downloadHandler(
      filename = function() paste0("nextgen-", event(), "-", input$period, ".csv"),
      content = function(path) write.csv(game_table(), path, row.names = FALSE, na = ""))
  })
}
