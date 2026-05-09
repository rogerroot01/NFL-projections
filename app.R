
suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(DT)
})

engine_candidates <- c(
  "sim_engine.R",
  "sim_engine_score_time_overlay_guardrails_v2_sample_pbp_penalty_cols_optimized_fix3.R",
  "sim_engine_score_time_overlay_guardrails_v2_sample_pbp_penalty_cols_optimized_fix.R",
  "sim_engine_score_time_overlay_guardrails_v2_sample_pbp_penalty_cols_optimized.R",
  "sim_engine_score_time_overlay_guardrails_v2_sample_pbp_penalty_cols.R",
  "sim_engine_score_time_overlay_guardrails_v2_sample_pbp.R",
  "sim_engine_score_time_overlay_guardrails_v2.R",
  "sim_engine_score_time_overlay_guardrails.R",
  "sim_engine_score_time_overlay_scope_audit_readable_overlay.R",
  "sim_engine_score_time_overlay_scope_audit.R",
  "sim_engine_score_time_overlay_diagnostics_clockfix.R",
  "sim_engine_score_time_overlay_diagnostics.R",
  "sim_engine_score_time_overlay_toggle.R",
  "sim_engine_score_time_overlay_fixed.R",
  "sim_engine_score_time_overlay.R",
  "sim_engine_common_auto_halfpoint.R",
  "sim_engine_seed_b2b_update.R",
  "sim_engine_pace_override_update.R",
  "sim_engine_hurry_fallback.R"
)
engine_file <- engine_candidates[file.exists(engine_candidates)][1]
if (is.na(engine_file) || !nzchar(engine_file)) {
  fallback_engines <- list.files(pattern = "^sim_engine.*\\.R$", ignore.case = FALSE)
  if (length(fallback_engines) > 0) {
    engine_file <- fallback_engines[1]
  } else {
    stop("Missing sim engine file in app directory. Expected sim_engine.R or one of the known sim_engine_* files.")
  }
}
source(engine_file)

app_static_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)
shiny::addResourcePath("app-assets", app_static_dir)

default_cfg <- build_sim_config()

make_schedule_choice_names <- function(df) {
  lapply(seq_len(nrow(df)), function(i) {
    label <- as.character(df$label[[i]] %||% "")
    if (length(label) == 0 || is.na(label)) label <- ""

    parts <- strsplit(label, " | ", fixed = TRUE)[[1]]
    parts <- parts[!is.na(parts) & nzchar(parts)]

    if (length(parts) <= 1) {
      shiny::HTML(label)
    } else {
      main_line <- parts[1]
      detail_line <- paste(parts[2:length(parts)], collapse = " | ")

      shiny::HTML(
        paste0(
          main_line,
          "<br/><span style='display:inline-block;margin-left:0;color:#666;font-size:0.9em;'>",
          detail_line,
          "</span>"
        )
      )
    }
  })
}

preserve_selected <- function(current_selected, default_selected = character(), choices = character()) {
  selected <- if (is.null(current_selected)) default_selected else current_selected
  intersect(as.character(selected), as.character(choices))
}

safe_table <- function(x, digits = 3, integer_cols = c("sims"), fixed = FALSE) {
  if (is.null(x)) return(NULL)
  df <- as.data.frame(x)
  num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  round_cols <- setdiff(num_cols, integer_cols)
  if (fixed) {
    for (nm in round_cols) df[[nm]] <- formatC(df[[nm]], format = "f", digits = digits)
    for (nm in intersect(integer_cols, num_cols)) df[[nm]] <- as.integer(round(df[[nm]], 0))
  } else {
    for (nm in round_cols) df[[nm]] <- round(df[[nm]], digits)
    for (nm in intersect(integer_cols, num_cols)) df[[nm]] <- as.integer(round(df[[nm]], 0))
  }
  df
}

format_probability_columns <- function(x, digits = 1) {
  if (is.null(x)) return(NULL)
  df <- as.data.frame(x)
  prob_cols <- names(df)[grepl("(_prob|_share)$", names(df))]
  if (length(prob_cols) == 0) return(df)
  for (nm in prob_cols) {
    df[[nm]] <- paste0(formatC(100 * df[[nm]], format = "f", digits = digits), "%")
  }
  df
}


format_game_clock <- function(elapsed_seconds) {
  secs <- suppressWarnings(as.numeric(elapsed_seconds))
  secs[is.na(secs)] <- 0
  secs <- pmax(0, pmin(3600, secs))
  quarter <- dplyr::case_when(
    secs < 900 ~ "Q1",
    secs < 1800 ~ "Q2",
    secs < 2700 ~ "Q3",
    TRUE ~ "Q4"
  )
  secs_into_quarter <- secs %% 900
  clock_remaining <- pmax(0, 900 - secs_into_quarter)
  minutes <- floor(clock_remaining / 60)
  seconds <- round(clock_remaining %% 60)
  list(
    quarter = quarter,
    clock = sprintf("%d:%02d", minutes, seconds)
  )
}

make_sample_play_by_play_table <- function(drives, cfg) {
  if (is.null(drives) || NROW(drives) == 0) return(data.frame(stringsAsFactors = FALSE))

  tryCatch({
    if (!is.data.frame(drives)) {
      drives <- as.data.frame(drives, stringsAsFactors = FALSE)
    }

    n <- nrow(drives)
    if (n == 0) return(data.frame(stringsAsFactors = FALSE))

    ensure_col <- function(df, nm, default = NA) {
      if (!nm %in% names(df)) {
        df[[nm]] <- rep(default, nrow(df))
      }
      df
    }

    stringify_col <- function(x) {
      if (is.null(x)) return(rep("", n))
      vapply(x, function(val) {
        if (length(val) == 0 || all(is.na(val))) return("")
        paste(as.character(val), collapse = " ")
      }, character(1))
    }

    normalize_logicalish <- function(x) {
      if (is.null(x)) return(rep(FALSE, n))
      if (is.logical(x)) return(ifelse(is.na(x), FALSE, x))
      if (is.numeric(x)) return(!is.na(x) & x != 0)
      x_chr <- trimws(tolower(as.character(x)))
      x_chr %in% c("true", "t", "1", "yes", "y")
    }

    numify <- function(x, default = NA_real_) {
      if (is.null(x)) return(rep(default, n))
      if (is.list(x)) x <- stringify_col(x)
      out <- suppressWarnings(as.numeric(as.character(x)))
      out[is.na(out)] <- default
      out
    }

    intify <- function(x, default = NA_integer_) {
      out <- suppressWarnings(as.integer(round(numify(x, default = default))))
      out[is.na(out)] <- default
      out
    }

    for (nm in c("play_time", "homepoints", "awaypoints", "yards_to_go", "yard_line", "yards_gained",
                 "field_goal_attempt", "is_field_goal", "is_td_offense", "safety",
                 "fourth_down_base_go_prob", "fourth_down_adjusted_go_prob", "implicit_penalty_yards", "new_down")) {
      drives <- ensure_col(drives, nm, 0)
    }
    for (nm in c("turnover_on_downs", "interception", "fumble_lost", "end_drive", "implicit_penalty_applied")) {
      drives <- ensure_col(drives, nm, FALSE)
    }
    for (nm in c("posteam", "play_type", "field_goal_result", "fourth_down_overlay_source", "play", "desc")) {
      drives <- ensure_col(drives, nm, "")
    }
    drives <- ensure_col(drives, "down_original", NA_integer_)
    drives <- ensure_col(drives, "down", NA_integer_)

    play_col <- if ("play" %in% names(drives)) drives$play else drives$desc
    overlay_source_col <- if ("fourth_down_overlay_source" %in% names(drives)) drives$fourth_down_overlay_source else rep("", n)
    field_goal_result_col <- stringify_col(drives$field_goal_result)
    play_type_col <- stringify_col(drives$play_type)
    posteam_col <- stringify_col(drives$posteam)
    play_desc_col <- stringify_col(play_col)
    overlay_source_fmt <- stringify_col(overlay_source_col)

    implicit_penalty_applied <- normalize_logicalish(drives$implicit_penalty_applied)
    turnover_on_downs <- normalize_logicalish(drives$turnover_on_downs)
    end_drive <- normalize_logicalish(drives$end_drive)
    interception <- normalize_logicalish(drives$interception)
    fumble_lost <- normalize_logicalish(drives$fumble_lost)

    play_time <- numify(drives$play_time, default = 0)
    elapsed_game_seconds <- cumsum(play_time)

    homepoints <- numify(drives$homepoints, default = 0)
    awaypoints <- numify(drives$awaypoints, default = 0)
    post_home_score <- cumsum(homepoints)
    post_away_score <- cumsum(awaypoints)
    pre_home_score <- c(0, head(post_home_score, -1))
    pre_away_score <- c(0, head(post_away_score, -1))

    down_raw <- drives$down_original
    down_missing <- is.null(down_raw) || all(is.na(down_raw))
    if (down_missing) down_raw <- drives$down
    down <- intify(down_raw, default = NA_integer_)
    to_go <- intify(drives$yards_to_go, default = NA_integer_)
    yard_line_100 <- intify(drives$yard_line, default = NA_integer_)
    yards_gained <- intify(drives$yards_gained, default = NA_integer_)
    new_down <- intify(drives$new_down, default = NA_integer_)

    is_td_offense <- normalize_logicalish(drives$is_td_offense)
    is_field_goal <- normalize_logicalish(drives$is_field_goal)
    safety <- normalize_logicalish(drives$safety)
    field_goal_attempt <- normalize_logicalish(drives$field_goal_attempt)

    base_go_prob_num <- numify(drives$fourth_down_base_go_prob, default = NA_real_)
    adjusted_go_prob_num <- numify(drives$fourth_down_adjusted_go_prob, default = NA_real_)
    implicit_penalty_yards_num <- numify(drives$implicit_penalty_yards, default = NA_real_)

    drive <- cumsum(c(TRUE, head(end_drive, -1)))
    play_seq <- seq_len(n)

    result <- rep("Continue", n)
    result[is_td_offense] <- "Touchdown"
    result[!is_td_offense & is_field_goal] <- "Field goal"
    result[!is_td_offense & !is_field_goal & safety] <- "Safety"
    result[!is_td_offense & !is_field_goal & !safety & turnover_on_downs] <- "Turnover on downs"
    result[!is_td_offense & !is_field_goal & !safety & !turnover_on_downs & interception] <- "Interception"
    result[!is_td_offense & !is_field_goal & !safety & !turnover_on_downs & !interception & fumble_lost] <- "Lost fumble"
    missed_fg <- field_goal_attempt & !(tolower(field_goal_result_col) %in% c("made", "good"))
    result[!is_td_offense & !is_field_goal & !safety & !turnover_on_downs & !interception & !fumble_lost & missed_fg] <- "Missed field goal"
    result[!is_td_offense & !is_field_goal & !safety & !turnover_on_downs & !interception & !fumble_lost & !missed_fg & tolower(play_type_col) == "punt"] <- "Punt"
    fd_mask <- !is_td_offense & !is_field_goal & !safety & !turnover_on_downs & !interception & !fumble_lost & !missed_fg & tolower(play_type_col) != "punt" & !end_drive & !is.na(new_down) & new_down == 1
    result[fd_mask] <- "First down"

    # Real penalties: combine explicit penalty text on the visible play with any
    # hidden sampled penalty rows surfaced by the engine. Keep hidden state
    # jumps separate so we do not mislabel ordinary reconciliation noise as a
    # penalty.
    extract_explicit_penalty_info <- function(desc_vec, offense_vec) {
      yards <- rep(NA_real_, length(desc_vec))
      penalized_team <- rep("", length(desc_vec))
      explicit_flag <- rep(FALSE, length(desc_vec))
      if (length(desc_vec) == 0) return(list(flag = explicit_flag, yards = yards, team = penalized_team))
      for (i in seq_along(desc_vec)) {
        txt <- desc_vec[[i]]
        if (is.na(txt) || !nzchar(txt)) next
        explicit_flag[[i]] <- grepl("(?i)penalty on\\s+", txt, perl = TRUE)
        if (!explicit_flag[[i]]) next
        m <- regexec("(?i)penalty on\\s+([A-Z]{2,3})-[^,]*,\\s*[^,]*,\\s*([0-9]+)\\s*yard", txt, perl = TRUE)
        hit <- regmatches(txt, m)[[1]]
        if (length(hit) >= 3) {
          pen_team <- toupper(hit[[2]])
          pen_yards <- suppressWarnings(as.numeric(hit[[3]]))
          penalized_team[[i]] <- pen_team
          off_team <- toupper(trimws(offense_vec[[i]]))
          if (!is.na(pen_yards)) {
            if (nzchar(off_team) && identical(pen_team, off_team)) {
              yards[[i]] <- -pen_yards
            } else if (nzchar(off_team)) {
              yards[[i]] <- pen_yards
            }
          }
        }
      }
      list(flag = explicit_flag, yards = yards, team = penalized_team)
    }

    explicit_penalty_info <- extract_explicit_penalty_info(play_desc_col, posteam_col)
    explicit_penalty_applied <- explicit_penalty_info$flag
    explicit_penalty_yards_num <- explicit_penalty_info$yards

    penalty_applied <- explicit_penalty_applied | implicit_penalty_applied
    penalty_yards_num <- explicit_penalty_yards_num
    penalty_yards_num[is.na(penalty_yards_num) & implicit_penalty_applied] <- implicit_penalty_yards_num[is.na(penalty_yards_num) & implicit_penalty_applied]

    terminal_result <- result %in% c("Touchdown", "Field goal", "Safety", "Turnover on downs", "Interception", "Lost fumble", "Missed field goal", "Punt")
    next_same_drive <- c(drive[-1] == drive[-n], FALSE)
    next_same_offense <- c(posteam_col[-1] == posteam_col[-n], FALSE)
    compare_next <- next_same_drive & next_same_offense & !terminal_result

    expected_yard_line <- yard_line_100 - yards_gained
    gained_first_down <- (result == "First down") |
      (!terminal_result & !is.na(to_go) & !is.na(yards_gained) & (yards_gained >= to_go))

    expected_down <- rep(NA_integer_, n)
    expected_down[!terminal_result & !gained_first_down & !is.na(down)] <- pmin(down[!terminal_result & !gained_first_down & !is.na(down)] + 1L, 4L)
    expected_down[gained_first_down] <- 1L

    expected_to_go <- rep(NA_integer_, n)
    goal_to_go_idx <- gained_first_down & !is.na(expected_yard_line)
    expected_to_go[goal_to_go_idx] <- pmax(1L, pmin(10L, expected_yard_line[goal_to_go_idx]))
    continue_idx <- !terminal_result & !gained_first_down & !is.na(to_go) & !is.na(yards_gained)
    expected_to_go[continue_idx] <- pmax(1L, to_go[continue_idx] - yards_gained[continue_idx])

    next_down <- c(down[-1], NA_integer_)
    next_to_go <- c(to_go[-1], NA_integer_)
    next_yard_line <- c(yard_line_100[-1], NA_integer_)

    yard_gap_num <- ifelse(!is.na(expected_yard_line) & !is.na(next_yard_line), expected_yard_line - next_yard_line, NA_real_)
    meaningful_yard_gap <- !is.na(yard_gap_num) & abs(yard_gap_num) >= 3
    down_gap <- !is.na(expected_down) & !is.na(next_down) & (expected_down != next_down)
    to_go_gap <- !is.na(expected_to_go) & !is.na(next_to_go) & (expected_to_go != next_to_go)

    hidden_state_jump <- compare_next & !penalty_applied & (meaningful_yard_gap | down_gap | to_go_gap)
    hidden_state_jump_yards_num <- ifelse(hidden_state_jump, yard_gap_num, NA_real_)

    fmt_pct <- function(x) {
      out <- rep("", length(x))
      keep <- !is.na(x)
      out[keep] <- paste0(formatC(100 * x[keep], format = "f", digits = 1), "%")
      out
    }

    penalty_yards_fmt <- rep("", n)
    known_penalty <- penalty_applied & !is.na(penalty_yards_num)
    penalty_yards_fmt[known_penalty & penalty_yards_num > 0] <- paste0("+", as.integer(round(penalty_yards_num[known_penalty & penalty_yards_num > 0])))
    penalty_yards_fmt[known_penalty & penalty_yards_num <= 0] <- as.character(as.integer(round(penalty_yards_num[known_penalty & penalty_yards_num <= 0])))

    hidden_jump_yards_fmt <- rep("", n)
    known_jump <- hidden_state_jump & !is.na(hidden_state_jump_yards_num)
    hidden_jump_yards_fmt[known_jump & hidden_state_jump_yards_num > 0] <- paste0("+", as.integer(round(hidden_state_jump_yards_num[known_jump & hidden_state_jump_yards_num > 0])))
    hidden_jump_yards_fmt[known_jump & hidden_state_jump_yards_num <= 0] <- as.character(as.integer(round(hidden_state_jump_yards_num[known_jump & hidden_state_jump_yards_num <= 0])))

    clock_info <- format_game_clock(elapsed_game_seconds)

    data.frame(
      play_seq = play_seq,
      drive = drive,
      quarter = clock_info$quarter,
      clock_after_play = clock_info$clock,
      offense = posteam_col,
      down = down,
      to_go = to_go,
      yard_line_100 = yard_line_100,
      play_type = play_type_col,
      yards_gained = yards_gained,
      result = result,
      score_before = paste0(cfg$away_team, " ", pre_away_score, " - ", cfg$home_team, " ", pre_home_score),
      score_after = paste0(cfg$away_team, " ", post_away_score, " - ", cfg$home_team, " ", post_home_score),
      base_go_prob = fmt_pct(base_go_prob_num),
      adjusted_go_prob = fmt_pct(adjusted_go_prob_num),
      overlay_source = ifelse(is.na(overlay_source_fmt), "", overlay_source_fmt),
      penalty_applied = ifelse(penalty_applied, "TRUE", "FALSE"),
      penalty_yards = penalty_yards_fmt,
      hidden_state_jump = ifelse(hidden_state_jump, "TRUE", "FALSE"),
      hidden_state_jump_yards = hidden_jump_yards_fmt,
      play = play_desc_col,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }, error = function(e) {
    data.frame(Message = paste0("Sample play-by-play build error: ", conditionMessage(e)), stringsAsFactors = FALSE)
  })
}


dist_plot_theme <- function() {
  ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 14),
      axis.text = ggplot2::element_text(size = 12),
      plot.title = ggplot2::element_text(size = 16)
    )
}

sample_pbp_tab_panel <- function() {
  tabPanel(
    "Sample Sim Play-by-Play",
    value = "sample_pbp_tab",
    p(
      style = "font-size: 0.9em; color: #666; margin-top: 8px;",
      "This shows the last simulated game from the most recent run only. It is a sample trace for validation, not an average across all simulations."
    ),
    DT::DTOutput("sample_sim_pbp")
  )
}


ui <- fluidPage(
  tags$head(
    tags$title("NFL Play-by-Play Simulation"),
    tags$link(rel = "icon", type = "image/png", href = "icon-192.png"),
    tags$link(rel = "apple-touch-icon", href = "icon-192.png"),
    tags$link(rel = "manifest", href = "site.webmanifest"),
    tags$meta(name = "theme-color", content = "#000000"),
    
    tags$style(HTML("
      .left-control-tabs .nav-tabs {
        border-bottom: 2px solid #1f2a36;
        margin-bottom: 12px;
        display: flex;
        flex-wrap: wrap;
        align-items: flex-end;
        gap: 8px 8px;
      }
      #main_tabs.nav.nav-tabs {
        border-bottom: 2px solid #1f2a36;
        margin-bottom: 12px;
      }
      .left-control-tabs .nav-tabs > li {
        float: none;
        margin-bottom: -1px;
      }
      .left-control-tabs .nav-tabs > li > a,
      #main_tabs.nav.nav-tabs > li > a {
        color: #1f2a36;
        background: #e9eef5;
        border: 1px solid #b8c4d1;
        border-bottom-color: #1f2a36;
        border-radius: 8px 8px 0 0;
        margin-right: 8px;
        font-weight: 700;
      }
      .left-control-tabs .nav-tabs > li > a {
        margin-right: 0;
        margin-bottom: 0;
      }
      .left-control-tabs .nav-tabs > li > a:hover,
      #main_tabs.nav.nav-tabs > li > a:hover {
        background: #dce6f2;
        border-color: #8ea5be;
        color: #0f1720;
      }
      .left-control-tabs .nav-tabs > li.active > a,
      .left-control-tabs .nav-tabs > li.active > a:hover,
      .left-control-tabs .nav-tabs > li.active > a:focus,
      #main_tabs.nav.nav-tabs > li.active > a,
      #main_tabs.nav.nav-tabs > li.active > a:hover,
      #main_tabs.nav.nav-tabs > li.active > a:focus {
        color: #fff;
        background: linear-gradient(90deg, #111827 0%, #1f4b2f 100%);
        border: 1px solid #111827;
        border-bottom-color: transparent;
      }
      .left-control-tabs .tab-pane { padding-top: 4px; }
      .schedule-tab-box {
        max-height: 520px;
        overflow-y: auto;
        padding-right: 6px;
      }
      .top-run-box {
        margin-bottom: 12px;
      }
      .run-box {
        margin-top: 14px;
      }
      .advanced-fixed-select,
      .advanced-fixed-select .form-group,
      .advanced-fixed-select .selectize-control,
      .advanced-fixed-select .selectize-input,
      .advanced-fixed-select .selectize-dropdown {
        width: 100% !important;
        max-width: 100% !important;
        min-width: 0 !important;
        box-sizing: border-box;
      }
      .advanced-fixed-select .selectize-input {
        overflow: hidden;
      }
      .advanced-fixed-select .selectize-input > div.item {
        max-width: 100%;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
        display: block;
      }
      .advanced-fixed-select .selectize-dropdown-content .option {
        white-space: normal;
        word-break: break-word;
      }
      .dataTables_wrapper .dataTable.compact thead th,
      .dataTables_wrapper .dataTable.compact tbody td {
        padding: 4px 8px;
        white-space: nowrap;
        vertical-align: middle;
      }
      .dataTables_wrapper .dataTable.compact tbody td {
        line-height: 1.15;
      }
      .dataTables_wrapper .dataTables_scrollHeadInner table,
      .dataTables_wrapper .dataTables_scrollBody table {
        width: 100% !important;
        margin-bottom: 0 !important;
      }
      .dataTables_wrapper .dataTables_scrollHead table.dataTable,
      .dataTables_wrapper .dataTables_scrollBody table.dataTable {
        border-collapse: separate !important;
        border-spacing: 0 !important;
      }
      .dataTables_wrapper .dataTables_scrollBody {
        overflow: auto !important;
      }
      .dataTables_wrapper .DTFC_LeftBodyLiner,
      .dataTables_wrapper .DTFC_RightBodyLiner {
        overflow-y: hidden !important;
      }
      .splash-overlay {
        position: fixed;
        inset: 0;
        background:
          linear-gradient(rgba(0, 0, 0, 0.18), rgba(0, 0, 0, 0.34)),
          url('app-assets/pbp_sim_stadium_bg.png') center center / cover no-repeat;
        z-index: 9999;
        display: flex;
        align-items: center;
        justify-content: center;
        overflow: hidden;
        padding: 0;
      }
      .splash-card {
        width: 100vw;
        min-height: 100vh;
        text-align: center;
        color: #ffffff;
        margin: 0;
        background: transparent;
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        padding: 24px 24px 36px 24px;
        box-sizing: border-box;
      }
      .splash-logo-stage {
        width: min(96vw, 1200px);
        background: transparent;
        display: flex;
        align-items: center;
        justify-content: center;
        margin: 0 auto 12px auto;
        padding: 12px 0;
      }
      .splash-logo {
        width: min(82vw, 700px);
        max-width: 100%;
        max-height: 72vh;
        object-fit: contain;
        height: auto;
        display: block;
        margin: 0 auto;
        border-radius: 0;
        box-shadow: none;
        background: transparent;
      }
      .splash-note {
        font-size: 15px;
        color: #d7dbe3;
        margin: 0 auto 18px auto;
        max-width: 700px;
      }
      .splash-enter-btn {
        font-size: 18px;
        padding: 10px 26px;
      }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('scrollToTopAndSelectSummary', function(message) {
        var summaryTabLink = document.querySelector('a[data-value=\"Summary\"]');
        if (summaryTabLink) {
          summaryTabLink.click();
        }
        window.setTimeout(function() {
          window.scrollTo({ top: 0, behavior: 'smooth' });
        }, 50);
      });
    "))
  ),
  div(
    style = "display: none;",
    textInput("app_view", label = NULL, value = "splash")
  ),
  conditionalPanel(
    condition = "input.app_view !== 'app'",
    div(
      class = "splash-overlay",
      div(
        class = "splash-card",
        div(
          class = "splash-logo-stage",
          tags$img(src = "app-assets/pbp_sim_logo.png", class = "splash-logo", alt = "NFL Play-by-Play Simulation")
        ),
        div(class = "splash-note", "Use the controls on the next screen to configure and run your matchup."),
        actionButton("enter_app", "Enter", class = "btn-primary splash-enter-btn")
      )
    )
  ),
  titlePanel("NFL Play-by-Play Game Simulator"),

  fluidRow(
    column(
      width = 4,
      wellPanel(
        div(
          class = "top-run-box",
          div(
            style = "display: flex; gap: 10px; flex-wrap: wrap;",
            actionButton("run_sim_top", "Run simulation", class = "btn-primary"),
            actionButton("back_to_splash_top", "Cancel", class = "btn-default")
          ),
          tags$br()
        ),
        div(
          class = "left-control-tabs",
          tabsetPanel(
            id = "control_tabs",
            tabPanel(
              "Game setup",
              selectizeInput(
                "seasons",
                "Seasons",
                choices = as.character(2020:2026),
                selected = as.character(default_cfg$seasons),
                multiple = TRUE
              ),
              uiOutput("team_selectors"),
              numericInput("n_sims", "Number of simulations", value = default_cfg$n_sims, min = 1, step = 1),
              numericInput("market_spread", "Market spread (home-team line)", value = default_cfg$market_spread, step = 0.5),
              numericInput("market_total", "Market total (over/under)", value = default_cfg$market_total, step = 0.5),
              numericInput("home_field_advantage", "Home field advantage (points)", value = default_cfg$home_field_advantage, step = 0.5),
              radioButtons(
                "opening_kickoff_side",
                "Opening kickoff received by",
                choices = c("Random" = "random", "Away team" = "away", "Home team" = "home"),
                selected = "random",
                inline = TRUE
              ),
              selectInput(
                "def_adjustment_mode",
                "Defense adjustment mode",
                choices = c("All opponents" = "all_opponents", "Common opponents" = "common_opponents", "Off" = "off"),
                selected = default_cfg$def_adjustment_mode
              ),
              tags$p(
                style = "font-size: 0.9em; color: #666; margin-top: -4px;",
                "Auto-default: if both team schedules are restricted to common opponents, this switches to Off unless you manually change it."
              )
            ),
            tabPanel(
              "Advanced settings",
              div(
                style = "display: flex; justify-content: flex-start; margin-bottom: 10px;",
                actionButton("restore_advanced_defaults", "Restore defaults")
              ),
              checkboxInput(
                "show_sample_pbp_tab",
                "Create Sample Sim Play-by-Play tab",
                value = FALSE
              ),
              tags$p(
                style = "font-size: 0.9em; color: #666; margin-top: -4px;",
                "Turning this off removes the sample trace tab from the results area. The last simulated game is still kept in memory, so turning it back on will show the latest available trace."
              ),
              tags$hr(),
              p("The simulator now widens yard-line windows adaptively behind the scenes when the local play sample is too thin. These inputs set the starting windows only."),
              numericInput("standard_window", "Starting standard yard-line window", value = default_cfg$yard_windows$standard, min = 0, step = 1),
              numericInput("two_minute_window", "Starting two-minute yard-line window", value = default_cfg$yard_windows$two_minute, min = 0, step = 1),
              textInput("starting_seed", "Starting random seed (optional)", value = ""),
              tags$p(
                style = "font-size: 0.9em; color: #666; margin-top: -4px;",
                "Leave blank to start with a random seed. Enter an integer to make a run reproducible."
              ),
              tags$hr(),
              radioButtons(
                "exclude_turnover_diff",
                "Exclude simulations by turnover differential",
                choices = c("No" = "no", "Yes" = "yes"),
                selected = if (isTRUE(default_cfg$turnover_filter$enabled)) "yes" else "no",
                inline = TRUE
              ),
              conditionalPanel(
                condition = "input.exclude_turnover_diff == 'yes'",
                numericInput(
                  "turnover_diff_threshold",
                  "Redraw game if turnover differential is greater than or equal to",
                  value = default_cfg$turnover_filter$differential_threshold,
                  min = 1,
                  step = 1
                )
              ),
              tags$hr(),
              checkboxInput(
                "use_away_play_call_override",
                "Override away-team pass/run tendency",
                value = isTRUE(default_cfg$play_call_override$away_enabled)
              ),
              conditionalPanel(
                condition = "input.use_away_play_call_override == true",
                sliderInput(
                  "away_pass_rate_override_pct",
                  "Forced away-team pass percentage",
                  min = 1,
                  max = 99,
                  value = as.integer(default_cfg$play_call_override$away_pass_pct),
                  step = 1,
                  post = "%"
                )
              ),
              checkboxInput(
                "use_home_play_call_override",
                "Override home-team pass/run tendency",
                value = isTRUE(default_cfg$play_call_override$home_enabled)
              ),
              conditionalPanel(
                condition = "input.use_home_play_call_override == true",
                sliderInput(
                  "home_pass_rate_override_pct",
                  "Forced home-team pass percentage",
                  min = 1,
                  max = 99,
                  value = as.integer(default_cfg$play_call_override$home_pass_pct),
                  step = 1,
                  post = "%"
                )
              ),
              tags$p(
                style = "font-size: 0.9em; color: #666; margin-top: 8px;",
                "When an override is off, that team uses its historical pass/run split and still increases pass tendency in trailing and hurry-up situations. When an override is on, that team's pass/run tendency is fixed to its slider value."
              ),
              tags$hr(),
              numericInput(
                "away_pace_seconds_adjustment",
                "Away team pace adjustment (seconds per play)",
                value = as.numeric(default_cfg$pace_override$away_seconds),
                min = -7,
                max = 7,
                step = 1
              ),
              numericInput(
                "home_pace_seconds_adjustment",
                "Home team pace adjustment (seconds per play)",
                value = as.numeric(default_cfg$pace_override$home_seconds),
                min = -7,
                max = 7,
                step = 1
              ),
              tags$p(
                style = "font-size: 0.9em; color: #666; margin-top: 8px;",
                "Adds or subtracts a flat number of seconds from each team's non-hurry-up plays. Negative values speed a team up. Positive values slow it down. This override is automatically turned off in the two-minute drill and in hurry-up game states."
              ),
              tags$hr(),
              div(
                class = "advanced-fixed-select",
                selectInput(
                  "away_fourth_down_mode",
                  "Away team fourth-down mode",
                  choices = c(
                    "Historical" = "historical",
                    "Always go for it" = "always_go",
                    "Field goal in range, otherwise go for it" = "fg_in_range_else_go",
                    "Go for it in field-goal range, otherwise punt" = "go_in_range_else_punt"
                  ),
                  selected = default_cfg$fourth_down_override$away_mode,
                  selectize = FALSE,
                  width = "100%"
                )
              ),
              div(
                class = "advanced-fixed-select",
                selectInput(
                  "home_fourth_down_mode",
                  "Home team fourth-down mode",
                  choices = c(
                    "Historical" = "historical",
                    "Always go for it" = "always_go",
                    "Field goal in range, otherwise go for it" = "fg_in_range_else_go",
                    "Go for it in field-goal range, otherwise punt" = "go_in_range_else_punt"
                  ),
                  selected = default_cfg$fourth_down_override$home_mode,
                  selectize = FALSE,
                  width = "100%"
                )
              ),
              checkboxInput(
                "use_fourth_down_score_time_overlay",
                "Use score/time overlay in Historical fourth-down mode",
                value = isTRUE(default_cfg$fourth_down_override$score_time_overlay_enabled)
              ),
              tags$p(
                style = "font-size: 0.9em; color: #666; margin-top: 8px;",
                "Historical always uses the current field-position and distance baseline. When this switch is on, Historical also adds a light score/time overlay on go-for-it aggressiveness. Field-goal range is defined here as the opponent 35-yard line or closer."
              )
            ),
            tabPanel(
              "Home schedule",
              div(
                class = "schedule-tab-box",
                div(
                  style = "display: flex; gap: 8px; flex-wrap: wrap; margin-bottom: 10px;",
                  actionButton("home_select_all_top", "Select all"),
                  actionButton("home_home_only_top", "Home games"),
                  actionButton("home_away_only_top", "Away games"),
                  actionButton("home_common_only_top", "Common opponents"),
                  actionButton("home_clear_all_top", "Clear all")
                ),
                uiOutput("home_games_ui"),
                div(
                  style = "display: flex; gap: 8px; flex-wrap: wrap;",
                  actionButton("home_select_all", "Select all"),
                  actionButton("home_home_only", "Home games"),
                  actionButton("home_away_only", "Away games"),
                  actionButton("home_common_only", "Common opponents"),
                  actionButton("home_clear_all", "Clear all")
                )
              )
            ),
            tabPanel(
              "Away schedule",
              div(
                class = "schedule-tab-box",
                div(
                  style = "display: flex; gap: 8px; flex-wrap: wrap; margin-bottom: 10px;",
                  actionButton("away_select_all_top", "Select all"),
                  actionButton("away_home_only_top", "Home games"),
                  actionButton("away_away_only_top", "Away games"),
                  actionButton("away_common_only_top", "Common opponents"),
                  actionButton("away_clear_all_top", "Clear all")
                ),
                uiOutput("away_games_ui"),
                div(
                  style = "display: flex; gap: 8px; flex-wrap: wrap;",
                  actionButton("away_select_all", "Select all"),
                  actionButton("away_home_only", "Home games"),
                  actionButton("away_away_only", "Away games"),
                  actionButton("away_common_only", "Common opponents"),
                  actionButton("away_clear_all", "Clear all")
                )
              )
            ),
            tabPanel(
              "Player exclusions",
              p("Choose players to exclude from the selected teams and seasons. No exclusions are selected by default. Lists load from play-by-play data when this tab is opened."),
              uiOutput("player_exclusions_ui")
            )
          )
        ),
        div(
          class = "run-box",
          tags$hr(),
          div(
            style = "display: flex; gap: 10px; flex-wrap: wrap;",
            actionButton("run_sim", "Run simulation", class = "btn-primary"),
            actionButton("back_to_splash", "Cancel", class = "btn-default")
          ),
          tags$br(),
          verbatimTextOutput("run_status")
        )
      )
    ),

    column(
      width = 8,
      tabsetPanel(
        id = "main_tabs",
        tabPanel(
          "Summary",
          h4("Outcome probabilities"),
          tableOutput("summary_side_probabilities"),
          tags$br(),
          h4("Scoring summary"),
          tableOutput("summary_scoring"),
          tags$br(),
          h4("Volume and turnover summary"),
          tableOutput("summary_volume"),
          tags$br(),
          h4("Advanced probabilities"),
          tableOutput("summary_advanced_side_probabilities"),
          tags$br(),
          tableOutput("summary_total_probabilities"),
          tags$br(),
          tableOutput("summary_back_to_back"),
          tags$p(
            style = "font-size: 0.9em; color: #666; margin-top: 8px;",
            "Back-to-back win rates are reported for the evaluated team shown in the table, which is the team receiving the second-half kickoff."
          )
        ),
        tabPanel(
          "Distributions",
          plotOutput("score_distribution", height = "320px"),
          plotOutput("spread_distribution", height = "320px"),
          plotOutput("total_distribution", height = "320px"),
          plotOutput("pass_ypa_distribution", height = "320px"),
          plotOutput("rush_ypa_distribution", height = "320px")
        ),
        tabPanel(
          "Convergence",
          p(
            style = "font-size: 0.9em; color: #666; margin-top: 8px;",
            "Running averages through the latest simulation run. These are useful for checking how quickly the estimated scores, margin, and total settle as the number of simulations increases."
          ),
          plotOutput("home_points_convergence", height = "300px"),
          plotOutput("away_points_convergence", height = "300px"),
          plotOutput("margin_convergence", height = "300px"),
          plotOutput("total_points_convergence", height = "300px")
        ),
        tabPanel(
          "Fourth Downs",
          h4("Team summary"),
          tableOutput("fourth_down_team_summary"),
          tags$br(),
          h4("Field-zone summary"),
          tableOutput("fourth_down_field_zone_summary"),
          tags$br(),
          h4("Success summary by distance to go"),
          tableOutput("fourth_down_distance_summary"),
          tags$br(),
          h4("Situation summary by score state"),
          tableOutput("fourth_down_score_state_summary"),
          tags$br(),
          h4("Adjusted fourth downs: why overlay applied + base vs adjusted go probability"),
          tableOutput("fourth_down_overlay_diagnostics"),
          tags$p(
            style = "font-size: 0.9em; color: #666; margin-top: 8px;",
            "This slimmer table shows why the overlay applied in each score/time bucket and how much it changed the go-for-it rate. Base is the local historical rate from field position and distance. Adjusted is the score/time-overlay version used in the simulation when Historical fourth-down mode is selected."
          ),
          tags$br(),
          h4("Normal fourth downs outside overlay scope"),
          tableOutput("fourth_down_normal_diagnostics"),
          tags$p(
            style = "font-size: 0.9em; color: #666; margin-top: 8px;",
            "This companion table shows fourth downs that stayed on the normal path because they were outside the overlay scope, such as 6+ yards to go or late Q4 blowouts."
          )
        ),
        tabPanel(
          "Fantasy",
          tableOutput("top_dk_by_team")
        ),
        tabPanel(
          "Simulation rows",
          DT::DTOutput("sim_results_head")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  last_error <- reactiveVal(NULL)
  running <- reactiveVal(FALSE)
  sim_result_rv <- reactiveVal(NULL)
  def_adjustment_manual_override <- reactiveVal(FALSE)
  def_adjustment_auto_update <- reactiveVal(FALSE)

  parse_optional_seed <- function(x) {
    x <- trimws(as.character(x %||% ""))
    if (!nzchar(x)) {
      return(list(valid = TRUE, value = NA_integer_))
    }

    if (!grepl("^[0-9]+$", x)) {
      return(list(valid = FALSE, value = NA_integer_, message = "Starting seed must be blank or a whole-number integer."))
    }

    seed_num <- suppressWarnings(as.numeric(x))
    if (is.na(seed_num) || seed_num < 0 || seed_num > .Machine$integer.max) {
      return(list(valid = FALSE, value = NA_integer_, message = paste0("Starting seed must be between 0 and ", .Machine$integer.max, ".")))
    }

    list(valid = TRUE, value = as.integer(seed_num))
  }


  same_selection_set <- function(selected, reference) {
    selected <- sort(unique(as.character(selected %||% character())))
    reference <- sort(unique(as.character(reference %||% character())))
    identical(selected, reference)
  }

  set_def_adjustment_mode <- function(value) {
    def_adjustment_auto_update(TRUE)
    updateSelectInput(session, "def_adjustment_mode", selected = value)
    session$onFlushed(function() {
      def_adjustment_auto_update(FALSE)
    }, once = TRUE)
  }

  reset_app_state <- function(show_splash = TRUE) {
    default_schedule <- load_schedule_data(as.integer(default_cfg$seasons))
    default_team_choices <- get_team_choices(default_schedule)

    home_default <- if ("NE" %in% default_team_choices) "NE" else default_team_choices[[1]]
    away_candidates <- setdiff(default_team_choices, home_default)
    away_default <- if ("SEA" %in% away_candidates) "SEA" else away_candidates[[1]]

    home_default_games <- get_team_games(default_schedule, home_default)$game_id
    away_default_games <- get_team_games(default_schedule, away_default)$game_id

    sim_result_rv(NULL)
    last_error(NULL)
    running(FALSE)
    def_adjustment_manual_override(FALSE)
    def_adjustment_auto_update(FALSE)

    updateTabsetPanel(session, "control_tabs", selected = "Game setup")
    updateSelectizeInput(session, "seasons", selected = as.character(default_cfg$seasons))
    updateSelectInput(session, "home_team", selected = home_default)
    updateSelectInput(session, "away_team", selected = away_default)
    updateNumericInput(session, "n_sims", value = default_cfg$n_sims)
    updateNumericInput(session, "market_spread", value = default_cfg$market_spread)
    updateNumericInput(session, "market_total", value = default_cfg$market_total)
    updateNumericInput(session, "home_field_advantage", value = default_cfg$home_field_advantage)
    updateRadioButtons(session, "opening_kickoff_side", selected = "random")
    set_def_adjustment_mode(default_cfg$def_adjustment_mode)
    updateNumericInput(session, "standard_window", value = default_cfg$yard_windows$standard)
    updateNumericInput(session, "two_minute_window", value = default_cfg$yard_windows$two_minute)
    updateTextInput(session, "starting_seed", value = "")
    updateRadioButtons(
      session,
      "exclude_turnover_diff",
      selected = if (isTRUE(default_cfg$turnover_filter$enabled)) "yes" else "no"
    )
    updateNumericInput(
      session,
      "turnover_diff_threshold",
      value = default_cfg$turnover_filter$differential_threshold
    )
    updateCheckboxInput(
      session,
      "use_away_play_call_override",
      value = isTRUE(default_cfg$play_call_override$away_enabled)
    )
    updateSliderInput(
      session,
      "away_pass_rate_override_pct",
      value = as.integer(default_cfg$play_call_override$away_pass_pct)
    )
    updateCheckboxInput(
      session,
      "use_home_play_call_override",
      value = isTRUE(default_cfg$play_call_override$home_enabled)
    )
    updateSliderInput(
      session,
      "home_pass_rate_override_pct",
      value = as.integer(default_cfg$play_call_override$home_pass_pct)
    )
    updateNumericInput(
      session,
      "away_pace_seconds_adjustment",
      value = as.numeric(default_cfg$pace_override$away_seconds)
    )
    updateNumericInput(
      session,
      "home_pace_seconds_adjustment",
      value = as.numeric(default_cfg$pace_override$home_seconds)
    )
    updateSelectInput(
      session,
      "away_fourth_down_mode",
      selected = default_cfg$fourth_down_override$away_mode
    )
    updateSelectInput(
      session,
      "home_fourth_down_mode",
      selected = default_cfg$fourth_down_override$home_mode
    )
    updateCheckboxInput(
      session,
      "use_fourth_down_score_time_overlay",
      value = isTRUE(default_cfg$fourth_down_override$score_time_overlay_enabled)
    )
    updateCheckboxInput(
      session,
      "show_sample_pbp_tab",
      value = FALSE
    )

    session$onFlushed(function() {
      updateCheckboxGroupInput(session, "home_included_games", selected = as.character(home_default_games))
      updateCheckboxGroupInput(session, "away_included_games", selected = as.character(away_default_games))
      updateSelectizeInput(session, "home_qb_exclusions", selected = character())
      updateSelectizeInput(session, "away_qb_exclusions", selected = character())
      updateSelectizeInput(session, "home_skill_exclusions", selected = character())
      updateSelectizeInput(session, "away_skill_exclusions", selected = character())
    }, once = TRUE)

    if (isTRUE(show_splash)) {
      updateTextInput(session, "app_view", value = "splash")
    }
  }

  observeEvent(input$enter_app, {
    updateTextInput(session, "app_view", value = "app")
  }, ignoreInit = TRUE)

  observeEvent(input$back_to_splash, {
    reset_app_state(show_splash = TRUE)
  }, ignoreInit = TRUE)

  observeEvent(input$back_to_splash_top, {
    reset_app_state(show_splash = TRUE)
  }, ignoreInit = TRUE)

  schedule_data <- reactive({
    req(input$seasons)
    load_schedule_data(as.integer(input$seasons))
  })

  team_choices <- reactive({
    choices <- get_team_choices(schedule_data())
    validate(need(length(choices) >= 2, "Need at least two teams in the selected seasons."))
    choices
  })

player_choice_data <- reactive({
  req(input$control_tabs == "Player exclusions")
  req(input$seasons, input$home_team, input$away_team)

  withProgress(message = "Loading player lists", detail = "Reading player names from play-by-play data...", value = 0.2, {
    choices <- get_player_choices_for_teams(
      seasons = as.integer(input$seasons),
      teams = c(input$home_team, input$away_team)
    )
    incProgress(0.8)
    choices
  })
})

output$player_exclusions_ui <- renderUI({
  req(input$home_team, input$away_team)

  choices_data <- player_choice_data()
  home_choices <- choices_data[[input$home_team]] %||% list(qb = character(), skill = character())
  away_choices <- choices_data[[input$away_team]] %||% list(qb = character(), skill = character())

  home_qb_selected <- preserve_selected(
    current_selected = isolate(input$home_qb_exclusions),
    default_selected = default_cfg$exclusions$qb$home,
    choices = home_choices$qb
  )
  away_qb_selected <- preserve_selected(
    current_selected = isolate(input$away_qb_exclusions),
    default_selected = default_cfg$exclusions$qb$away,
    choices = away_choices$qb
  )
  home_skill_selected <- preserve_selected(
    current_selected = isolate(input$home_skill_exclusions),
    default_selected = default_cfg$exclusions$skill$home,
    choices = home_choices$skill
  )
  away_skill_selected <- preserve_selected(
    current_selected = isolate(input$away_skill_exclusions),
    default_selected = default_cfg$exclusions$skill$away,
    choices = away_choices$skill
  )

  tagList(
    selectizeInput(
      "home_qb_exclusions",
      label = paste("Home QB exclusions:", input$home_team),
      choices = home_choices$qb,
      selected = home_qb_selected,
      multiple = TRUE,
      options = list(plugins = list("remove_button"), placeholder = "None selected")
    ),
    selectizeInput(
      "away_qb_exclusions",
      label = paste("Away QB exclusions:", input$away_team),
      choices = away_choices$qb,
      selected = away_qb_selected,
      multiple = TRUE,
      options = list(plugins = list("remove_button"), placeholder = "None selected")
    ),
    selectizeInput(
      "home_skill_exclusions",
      label = paste("Home skill exclusions:", input$home_team),
      choices = home_choices$skill,
      selected = home_skill_selected,
      multiple = TRUE,
      options = list(plugins = list("remove_button"), placeholder = "None selected")
    ),
    selectizeInput(
      "away_skill_exclusions",
      label = paste("Away skill exclusions:", input$away_team),
      choices = away_choices$skill,
      selected = away_skill_selected,
      multiple = TRUE,
      options = list(plugins = list("remove_button"), placeholder = "None selected")
    )
  )
})

  output$team_selectors <- renderUI({
    choices <- team_choices()

    home_selected <- isolate(input$home_team)
    away_selected <- isolate(input$away_team)

    if (is.null(home_selected) || !home_selected %in% choices) {
      home_selected <- if ("NE" %in% choices) "NE" else choices[[1]]
    }

    if (is.null(away_selected) || !away_selected %in% choices || identical(away_selected, home_selected)) {
      away_selected <- if ("SEA" %in% choices && home_selected != "SEA") "SEA" else setdiff(choices, home_selected)[[1]]
    }

    tagList(
      selectInput("home_team", "Home team", choices = choices, selected = home_selected),
      selectInput("away_team", "Away team", choices = choices, selected = away_selected)
    )
  })

  home_games_tbl <- reactive({
    req(input$home_team)
    get_team_games(schedule_data(), input$home_team)
  })

  away_games_tbl <- reactive({
    req(input$away_team)
    get_team_games(schedule_data(), input$away_team)
  })

  common_opponent_games_home <- reactive({
    req(input$home_team, input$away_team)
    get_common_opponent_game_ids(
      schedule_df = schedule_data(),
      subject_team = input$home_team,
      other_team = input$away_team
    )
  })

  common_opponent_games_away <- reactive({
    req(input$home_team, input$away_team)
    get_common_opponent_game_ids(
      schedule_df = schedule_data(),
      subject_team = input$away_team,
      other_team = input$home_team
    )
  })


  both_schedules_common_only <- reactive({
    req(input$home_team, input$away_team)

    home_selected <- as.character(input$home_included_games %||% character())
    away_selected <- as.character(input$away_included_games %||% character())
    home_common <- as.character(common_opponent_games_home() %||% character())
    away_common <- as.character(common_opponent_games_away() %||% character())

    if (length(home_common) == 0 || length(away_common) == 0) {
      return(FALSE)
    }

    same_selection_set(home_selected, home_common) && same_selection_set(away_selected, away_common)
  })

  observeEvent(input$def_adjustment_mode, {
    if (!isTRUE(def_adjustment_auto_update())) {
      def_adjustment_manual_override(TRUE)
    }
  }, ignoreInit = TRUE)

  observeEvent(both_schedules_common_only(), {
    if (isTRUE(def_adjustment_manual_override())) {
      return(invisible(NULL))
    }

    if (isTRUE(both_schedules_common_only())) {
      if (!identical(input$def_adjustment_mode, "common_opponents")) {
        set_def_adjustment_mode("common_opponents")
        showNotification(
          "Defense adjustment mode auto-switched to Common opponents because both schedules are restricted to common opponents.",
          type = "message",
          duration = 5
        )
      }
    } else {
      if (!identical(input$def_adjustment_mode, default_cfg$def_adjustment_mode)) {
        set_def_adjustment_mode(default_cfg$def_adjustment_mode)
      }
    }
  }, ignoreInit = TRUE)

  output$home_games_ui <- renderUI({
    games <- home_games_tbl()
    current_selected <- isolate(input$home_included_games)
    selected <- if (is.null(current_selected)) games$game_id else intersect(current_selected, games$game_id)
    if (length(selected) == 0) selected <- games$game_id

    checkboxGroupInput(
      "home_included_games",
      label = paste("Home team included games:", input$home_team),
      choiceNames = make_schedule_choice_names(games),
      choiceValues = as.character(games$game_id),
      selected = as.character(selected)
    )
  })

  output$away_games_ui <- renderUI({
    games <- away_games_tbl()
    current_selected <- isolate(input$away_included_games)
    selected <- if (is.null(current_selected)) games$game_id else intersect(current_selected, games$game_id)
    if (length(selected) == 0) selected <- games$game_id

    checkboxGroupInput(
      "away_included_games",
      label = paste("Away team included games:", input$away_team),
      choiceNames = make_schedule_choice_names(games),
      choiceValues = as.character(games$game_id),
      selected = as.character(selected)
    )
  })

  observeEvent(list(input$home_select_all, input$home_select_all_top), {
    updateCheckboxGroupInput(session, "home_included_games", selected = home_games_tbl()$game_id)
  }, ignoreInit = TRUE)

  observeEvent(list(input$home_home_only, input$home_home_only_top), {
    selected <- home_games_tbl() %>%
      dplyr::filter(team_role == "home") %>%
      dplyr::pull(game_id)
    updateCheckboxGroupInput(session, "home_included_games", selected = selected)
  }, ignoreInit = TRUE)

  observeEvent(list(input$home_away_only, input$home_away_only_top), {
    selected <- home_games_tbl() %>%
      dplyr::filter(team_role == "away") %>%
      dplyr::pull(game_id)
    updateCheckboxGroupInput(session, "home_included_games", selected = selected)
  }, ignoreInit = TRUE)

  observeEvent(list(input$home_common_only, input$home_common_only_top), {
    home_selected <- common_opponent_games_home()
    away_selected <- common_opponent_games_away()

    updateCheckboxGroupInput(session, "home_included_games", selected = home_selected)
    updateCheckboxGroupInput(session, "away_included_games", selected = away_selected)

    if (length(home_selected) == 0 || length(away_selected) == 0) {
      showNotification("No common-opponent games found for one or both selected teams and seasons.", type = "message", duration = 4)
    }
  }, ignoreInit = TRUE)

  observeEvent(list(input$home_clear_all, input$home_clear_all_top), {
    updateCheckboxGroupInput(session, "home_included_games", selected = character())
  }, ignoreInit = TRUE)

  observeEvent(list(input$away_select_all, input$away_select_all_top), {
    updateCheckboxGroupInput(session, "away_included_games", selected = away_games_tbl()$game_id)
  }, ignoreInit = TRUE)

  observeEvent(list(input$away_home_only, input$away_home_only_top), {
    selected <- away_games_tbl() %>%
      dplyr::filter(team_role == "home") %>%
      dplyr::pull(game_id)
    updateCheckboxGroupInput(session, "away_included_games", selected = selected)
  }, ignoreInit = TRUE)

  observeEvent(list(input$away_away_only, input$away_away_only_top), {
    selected <- away_games_tbl() %>%
      dplyr::filter(team_role == "away") %>%
      dplyr::pull(game_id)
    updateCheckboxGroupInput(session, "away_included_games", selected = selected)
  }, ignoreInit = TRUE)

  observeEvent(list(input$away_common_only, input$away_common_only_top), {
    home_selected <- common_opponent_games_home()
    away_selected <- common_opponent_games_away()

    updateCheckboxGroupInput(session, "home_included_games", selected = home_selected)
    updateCheckboxGroupInput(session, "away_included_games", selected = away_selected)

    if (length(home_selected) == 0 || length(away_selected) == 0) {
      showNotification("No common-opponent games found for one or both selected teams and seasons.", type = "message", duration = 4)
    }
  }, ignoreInit = TRUE)

  observeEvent(list(input$away_clear_all, input$away_clear_all_top), {
    updateCheckboxGroupInput(session, "away_included_games", selected = character())
  }, ignoreInit = TRUE)

  observeEvent(input$restore_advanced_defaults, {
    updateNumericInput(session, "standard_window", value = default_cfg$yard_windows$standard)
    updateNumericInput(session, "two_minute_window", value = default_cfg$yard_windows$two_minute)
    updateTextInput(session, "starting_seed", value = "")
    updateRadioButtons(
      session,
      "exclude_turnover_diff",
      selected = if (isTRUE(default_cfg$turnover_filter$enabled)) "yes" else "no"
    )
    updateNumericInput(
      session,
      "turnover_diff_threshold",
      value = default_cfg$turnover_filter$differential_threshold
    )
    updateCheckboxInput(
      session,
      "use_away_play_call_override",
      value = isTRUE(default_cfg$play_call_override$away_enabled)
    )
    updateSliderInput(
      session,
      "away_pass_rate_override_pct",
      value = as.integer(default_cfg$play_call_override$away_pass_pct)
    )
    updateCheckboxInput(
      session,
      "use_home_play_call_override",
      value = isTRUE(default_cfg$play_call_override$home_enabled)
    )
    updateSliderInput(
      session,
      "home_pass_rate_override_pct",
      value = as.integer(default_cfg$play_call_override$home_pass_pct)
    )
    updateNumericInput(
      session,
      "away_pace_seconds_adjustment",
      value = as.numeric(default_cfg$pace_override$away_seconds)
    )
    updateNumericInput(
      session,
      "home_pace_seconds_adjustment",
      value = as.numeric(default_cfg$pace_override$home_seconds)
    )
    updateSelectInput(
      session,
      "away_fourth_down_mode",
      selected = default_cfg$fourth_down_override$away_mode
    )
    updateSelectInput(
      session,
      "home_fourth_down_mode",
      selected = default_cfg$fourth_down_override$home_mode
    )
    updateCheckboxInput(
      session,
      "use_fourth_down_score_time_overlay",
      value = isTRUE(default_cfg$fourth_down_override$score_time_overlay_enabled)
    )
    updateCheckboxInput(
      session,
      "show_sample_pbp_tab",
      value = FALSE
    )
    showNotification("Advanced settings restored to defaults.", type = "message", duration = 3)
  }, ignoreInit = TRUE)


  sample_pbp_tab_visible <- reactiveVal(FALSE)

  observeEvent(input$show_sample_pbp_tab, {
    show_tab <- isTRUE(input$show_sample_pbp_tab)
    currently_visible <- isTRUE(sample_pbp_tab_visible())

    if (show_tab && !currently_visible) {
      insertTab(
        inputId = "main_tabs",
        tab = sample_pbp_tab_panel(),
        target = "Simulation rows",
        position = "after",
        select = FALSE
      )
      sample_pbp_tab_visible(TRUE)
    } else if (!show_tab && currently_visible) {
      if (identical(input$main_tabs, "sample_pbp_tab")) {
        updateTabsetPanel(session, "main_tabs", selected = "Summary")
      }
      removeTab(inputId = "main_tabs", target = "sample_pbp_tab")
      sample_pbp_tab_visible(FALSE)
    }
  }, ignoreInit = TRUE)


observeEvent(list(input$run_sim, input$run_sim_top), {
  if (isTRUE(running())) {
    showNotification("A simulation is already running. Please wait for it to finish.", type = "message", duration = 5)
    return(invisible(NULL))
  }

  last_error(NULL)

  starting_seed_info <- parse_optional_seed(input$starting_seed)

  validate(
    need(length(input$seasons) > 0, "Select at least one season."),
    need(!is.null(input$home_team) && !is.null(input$away_team), "Choose both teams."),
    need(input$home_team != input$away_team, "Home and away teams must be different."),
    need(input$n_sims >= 1, "Number of simulations must be at least 1."),
    need(input$exclude_turnover_diff != "yes" || (!is.null(input$turnover_diff_threshold) && input$turnover_diff_threshold >= 1),
         "Turnover differential cutoff must be at least 1."),
    need(
      !isTRUE(input$use_away_play_call_override) ||
        (
          !is.null(input$away_pass_rate_override_pct) &&
          input$away_pass_rate_override_pct >= 1 &&
          input$away_pass_rate_override_pct <= 99
        ),
      "Away forced pass percentage must be between 1% and 99%."
    ),
    need(
      !isTRUE(input$use_home_play_call_override) ||
        (
          !is.null(input$home_pass_rate_override_pct) &&
          input$home_pass_rate_override_pct >= 1 &&
          input$home_pass_rate_override_pct <= 99
        ),
      "Home forced pass percentage must be between 1% and 99%."
    ),
    need(!is.null(input$away_pace_seconds_adjustment) && input$away_pace_seconds_adjustment >= -7 && input$away_pace_seconds_adjustment <= 7,
         "Away pace adjustment must be between -7 and 7 seconds per play."),
    need(!is.null(input$home_pace_seconds_adjustment) && input$home_pace_seconds_adjustment >= -7 && input$home_pace_seconds_adjustment <= 7,
         "Home pace adjustment must be between -7 and 7 seconds per play."),
    need(isTRUE(starting_seed_info$valid), starting_seed_info$message %||% "Starting seed is invalid.")
  )

  home_all_games <- home_games_tbl()$game_id
  away_all_games <- away_games_tbl()$game_id
  effective_def_adjustment_mode <- if (isTRUE(both_schedules_common_only()) && !isTRUE(def_adjustment_manual_override())) {
    "off"
  } else {
    input$def_adjustment_mode
  }

  cfg_input <- list(
    seasons = as.integer(input$seasons),
    home_team = input$home_team,
    away_team = input$away_team,
    n_sims = as.integer(input$n_sims),
    market_spread = as.numeric(input$market_spread),
    market_total = as.numeric(input$market_total),
    home_field_advantage = as.numeric(input$home_field_advantage),
    opening_kickoff_side = input$opening_kickoff_side,
    def_adjustment_mode = effective_def_adjustment_mode,
    random_seed = starting_seed_info$value,
    yard_windows = list(
      standard = as.numeric(input$standard_window),
      two_minute = as.numeric(input$two_minute_window)
    ),
    turnover_filter = list(
      enabled = identical(input$exclude_turnover_diff, "yes"),
      differential_threshold = as.integer(input$turnover_diff_threshold %||% default_cfg$turnover_filter$differential_threshold)
    ),
    play_call_override = list(
      enabled = isTRUE(input$use_away_play_call_override) || isTRUE(input$use_home_play_call_override),
      away_enabled = isTRUE(input$use_away_play_call_override),
      home_enabled = isTRUE(input$use_home_play_call_override),
      away_pass_pct = as.integer(input$away_pass_rate_override_pct %||% default_cfg$play_call_override$away_pass_pct),
      home_pass_pct = as.integer(input$home_pass_rate_override_pct %||% default_cfg$play_call_override$home_pass_pct)
    ),
    pace_override = list(
      away_seconds = as.numeric(input$away_pace_seconds_adjustment %||% default_cfg$pace_override$away_seconds),
      home_seconds = as.numeric(input$home_pace_seconds_adjustment %||% default_cfg$pace_override$home_seconds)
    ),
    fourth_down_override = list(
      away_mode = as.character(input$away_fourth_down_mode %||% default_cfg$fourth_down_override$away_mode),
      home_mode = as.character(input$home_fourth_down_mode %||% default_cfg$fourth_down_override$home_mode),
      score_time_overlay_enabled = isTRUE(input$use_fourth_down_score_time_overlay %||% default_cfg$fourth_down_override$score_time_overlay_enabled)
    ),
    exclusions = list(
      qb = list(
        home = input$home_qb_exclusions %||% character(),
        away = input$away_qb_exclusions %||% character()
      ),
      skill = list(
        home = input$home_skill_exclusions %||% character(),
        away = input$away_skill_exclusions %||% character()
      ),
      games = list(
        home = setdiff(home_all_games, input$home_included_games %||% character()),
        away = setdiff(away_all_games, input$away_included_games %||% character())
      )
    )
  )

  running(TRUE)
  on.exit(running(FALSE), add = TRUE)

  progress_val <- 0
  tryCatch({
    withProgress(message = "Running simulation", detail = "Starting...", value = 0, {
      result <- run_game_simulation(
        cfg_input,
        progress_callback = local({
          last_shown_value <- 0
          function(value = NULL, detail = NULL) {
            value <- max(0, min(1, as.numeric(value %||% progress_val)))
            should_update <- TRUE
            if (!is.null(detail) && grepl("^Completed [0-9]+ of [0-9]+ simulations", detail)) {
              m <- regexec("^Completed ([0-9]+) of ([0-9]+) simulations", detail)
              parts <- regmatches(detail, m)[[1]]
              if (length(parts) >= 3) {
                completed <- suppressWarnings(as.integer(parts[2]))
                total <- suppressWarnings(as.integer(parts[3]))
                should_update <- !is.na(completed) && !is.na(total) && (completed %% 10L == 0L || completed >= total)
              }
            }
            progress_val <<- value
            if (!isTRUE(should_update)) return(invisible(NULL))
            shown_value <- max(last_shown_value, value)
            delta <- max(0, shown_value - last_shown_value)
            last_shown_value <<- shown_value
            if (!is.null(detail)) {
              incProgress(delta, detail = detail)
            } else {
              incProgress(delta)
            }
          }
        })
      )
      sim_result_rv(result)
      updateTabsetPanel(session, "main_tabs", selected = "Summary")
      session$sendCustomMessage("scrollToTopAndSelectSummary", list())
    })
  }, error = function(e) {
    msg <- paste0("Simulation error: ", conditionMessage(e))
    last_error(msg)
    showNotification(msg, type = "error", duration = NULL)
  })
}, ignoreInit = TRUE)

sim_result <- reactive({
  sim_result_rv()
})


  output$run_status <- renderText({
    if (isTRUE(running())) {
      return("Simulation running...")
    }
    if (!is.null(last_error())) {
      return(last_error())
    }
    res <- sim_result()
    req(res)
    seed_line <- if (isTRUE(res$cfg$random_seed_supplied)) {
      paste0("Seed used: ", res$cfg$random_seed_used, " (user supplied)")
    } else {
      paste0("Seed used: ", res$cfg$random_seed_used, " (random)")
    }

    paste0(
      "Completed in ", round(as.numeric(res$elapsed, units = "secs"), 2), " seconds.\n",
      "Home: ", res$cfg$home_team, " | Away: ", res$cfg$away_team, "\n",
      "Sims: ", res$cfg$n_sims, " | Seasons: ", paste(res$cfg$seasons, collapse = ", "), "\n",
      seed_line
    )
  })

  output$summary_scoring <- renderTable({
    req(sim_result())
    sim_result()$team_avg %>%
      dplyr::select(
        side,
        team,
        sims,
        avg_points_pre_adj,
        def_adjustment,
        home_field_adjustment,
        total_adjustment,
        avg_points_post_adj,
        avg_TDs,
        avg_rush_TDs,
        avg_pass_TDs,
        avg_field_goals
      ) %>%
      safe_table(digits = 1, fixed = TRUE)
  }, digits = 1)

  output$summary_volume <- renderTable({
    req(sim_result())
    sim_result()$team_avg %>%
      dplyr::select(
        side,
        team,
        sims,
        avg_drives,
        avg_pass_yds,
        adjusted_avg_pass_yds,
        avg_rush_yds,
        adjusted_avg_rush_yds,
        avg_turnovers,
        avg_INTs,
        avg_fumbles_lost
      ) %>%
      safe_table(digits = 1, fixed = TRUE)
  }, digits = 1)

  output$summary_side_probabilities <- renderTable({
    req(sim_result())
    side_prob_df <- sim_result()$side_probabilities %>%
      as.data.frame(stringsAsFactors = FALSE)
    total_prob_df <- sim_result()$total_probabilities %>%
      as.data.frame(stringsAsFactors = FALSE)

    display_df <- side_prob_df %>%
      dplyr::select(dplyr::any_of(c("side", "team", "sims", "win_prob", "cover_spread_prob"))) %>%
      dplyr::mutate(
        over_prob = NA_real_,
        under_prob = NA_real_
      )

    if (nrow(display_df) >= 1) {
      display_df$over_prob[[1]] <- total_prob_df$over_prob[[1]] %||% NA_real_
      display_df$under_prob[[1]] <- total_prob_df$under_prob[[1]] %||% NA_real_
    }

    prob_cols <- intersect(c("win_prob", "cover_spread_prob", "over_prob", "under_prob"), names(display_df))
    for (nm in prob_cols) {
      vals <- suppressWarnings(as.numeric(display_df[[nm]]))
      display_df[[nm]] <- ifelse(
        is.na(vals),
        "",
        paste0(formatC(100 * vals, format = "f", digits = 1), "%")
      )
    }

    safe_table(display_df, digits = 1, fixed = TRUE)
  }, digits = 1)

  output$summary_advanced_side_probabilities <- renderTable({
    req(sim_result())
    side_prob_df <- sim_result()$side_probabilities %>%
      as.data.frame(stringsAsFactors = FALSE)

    id_cols <- intersect(c("side", "team", "sims"), names(side_prob_df))
    outcome_prob_cols <- names(side_prob_df)[
      grepl("(win|cover).*_prob$", names(side_prob_df), ignore.case = TRUE) &
        !grepl("push|back_to_back|no_back_to_back|over|under|total", names(side_prob_df), ignore.case = TRUE)
    ]
    advanced_cols <- setdiff(
      names(side_prob_df),
      c(id_cols, outcome_prob_cols, "cover_spread_prob", "cover_game_spread_prob")
    )

    if (length(advanced_cols) == 0) {
      return(NULL)
    }

    side_prob_df %>%
      dplyr::select(dplyr::any_of(c(id_cols, advanced_cols))) %>%
      format_probability_columns(digits = 1) %>%
      safe_table(digits = 1, fixed = TRUE)
  }, digits = 1)

  output$summary_total_probabilities <- renderTable({
    req(sim_result())
    sim_result()$total_probabilities %>%
      format_probability_columns(digits = 1) %>%
      safe_table(digits = 1, fixed = TRUE)
  }, digits = 1)

  output$summary_back_to_back <- renderTable({
    req(sim_result())
    sim_result()$back_to_back_summary %>%
      dplyr::select(
        dplyr::any_of(c(
          "opening_kickoff_team",
          "second_half_kickoff_team",
          "evaluated_team",
          "evaluated_team_overall_win_prob",
          "back_to_back_possession_prob",
          "win_when_back_to_back_prob",
          "win_when_no_back_to_back_prob"
        ))
      ) %>%
      format_probability_columns(digits = 1) %>%
      safe_table(digits = 1, fixed = TRUE)
  }, digits = 1)

  output$score_distribution <- renderPlot({
    req(sim_result())
    plot_df <- sim_result()$results_adj %>%
      dplyr::select(home_team, away_team, home_score, away_score) %>%
      tidyr::pivot_longer(cols = c(home_score, away_score), names_to = "side", values_to = "score") %>%
      dplyr::mutate(team = dplyr::if_else(side == "home_score", home_team, away_team))

    ggplot(plot_df, aes(x = score, fill = team)) +
      geom_histogram(position = "identity", alpha = 0.4, bins = 25) +
      labs(title = "Adjusted score distribution", x = "Score", y = "Count") +
      dist_plot_theme()
  })

  output$spread_distribution <- renderPlot({
    req(sim_result())
    plot_df <- sim_result()$results_adj
    sim_mean_spread <- mean(plot_df$home_win_spread, na.rm = TRUE)
    market_spread_target <- -unique(plot_df$market_spread_home)[1]
    y_top <- max(ggplot2::ggplot_build(
      ggplot(plot_df, aes(x = home_win_spread)) + geom_histogram(bins = 25)
    )$data[[1]]$count, na.rm = TRUE)

    ggplot(plot_df, aes(x = home_win_spread)) +
      geom_histogram(bins = 25, fill = "grey70", color = "grey35") +
      geom_vline(xintercept = sim_mean_spread, linetype = 1, linewidth = 1.1, color = "black") +
      geom_vline(xintercept = market_spread_target, linetype = 2, linewidth = 1.1, color = "black") +
      annotate("text", x = sim_mean_spread, y = y_top, label = "Sim average", vjust = -0.4, size = 4.5) +
      annotate("text", x = market_spread_target, y = y_top * 0.9, label = "Market line", vjust = -0.4, size = 4.5) +
      labs(
        title = "Adjusted home spread distribution",
        x = "Home scoring margin",
        y = "Count"
      ) +
      dist_plot_theme()
  })

  output$total_distribution <- renderPlot({
    req(sim_result())
    plot_df <- sim_result()$results_adj
    sim_mean_total <- mean(plot_df$total_points_scored, na.rm = TRUE)
    market_total_target <- unique(plot_df$market_total)[1]
    y_top <- max(ggplot2::ggplot_build(
      ggplot(plot_df, aes(x = total_points_scored)) + geom_histogram(bins = 25)
    )$data[[1]]$count, na.rm = TRUE)

    ggplot(plot_df, aes(x = total_points_scored)) +
      geom_histogram(bins = 25, fill = "grey70", color = "grey35") +
      geom_vline(xintercept = sim_mean_total, linetype = 1, linewidth = 1.1, color = "black") +
      geom_vline(xintercept = market_total_target, linetype = 2, linewidth = 1.1, color = "black") +
      annotate("text", x = sim_mean_total, y = y_top, label = "Sim average", vjust = -0.4, size = 4.5) +
      annotate("text", x = market_total_target, y = y_top * 0.9, label = "Market total", vjust = -0.4, size = 4.5) +
      labs(
        title = "Adjusted total score distribution",
        x = "Total points scored",
        y = "Count"
      ) +
      dist_plot_theme()
  })

  output$pass_ypa_distribution <- renderPlot({
    req(sim_result())
    plot_df <- sim_result()$results_adj %>%
      dplyr::select(home_team, away_team, home_pass_ypa, away_pass_ypa) %>%
      tidyr::pivot_longer(cols = c(home_pass_ypa, away_pass_ypa), names_to = "side", values_to = "pass_ypa") %>%
      dplyr::mutate(team = dplyr::if_else(side == "home_pass_ypa", home_team, away_team)) %>%
      dplyr::filter(!is.na(pass_ypa))

    ggplot(plot_df, aes(x = pass_ypa, fill = team)) +
      geom_histogram(position = "identity", alpha = 0.4, bins = 25) +
      labs(title = "Passing yards per attempt distribution", x = "Yards per attempt", y = "Count") +
      dist_plot_theme()
  })

  output$rush_ypa_distribution <- renderPlot({
    req(sim_result())
    plot_df <- sim_result()$results_adj %>%
      dplyr::select(home_team, away_team, home_rush_ypa, away_rush_ypa) %>%
      tidyr::pivot_longer(cols = c(home_rush_ypa, away_rush_ypa), names_to = "side", values_to = "rush_ypa") %>%
      dplyr::mutate(team = dplyr::if_else(side == "home_rush_ypa", home_team, away_team)) %>%
      dplyr::filter(!is.na(rush_ypa))

    ggplot(plot_df, aes(x = rush_ypa, fill = team)) +
      geom_histogram(position = "identity", alpha = 0.4, bins = 25) +
      labs(title = "Rushing yards per attempt distribution", x = "Yards per attempt", y = "Count") +
      dist_plot_theme()
  })

  make_convergence_plot <- function(metric_col, title, y_label) {
    req(sim_result())

    results_df <- sim_result()$results_adj %>%
      dplyr::arrange(sim)

    validate(
      need(metric_col %in% names(results_df), paste0("Missing convergence metric: ", metric_col)),
      need(nrow(results_df) > 0, "Run a simulation to view convergence plots.")
    )

    metric_values <- suppressWarnings(as.numeric(results_df[[metric_col]]))
    valid_counts <- cumsum(!is.na(metric_values))
    running_totals <- cumsum(ifelse(is.na(metric_values), 0, metric_values))
    running_average <- ifelse(valid_counts > 0, running_totals / valid_counts, NA_real_)

    plot_df <- data.frame(
      sim_number = seq_along(metric_values),
      running_average = running_average,
      stringsAsFactors = FALSE
    )

    final_avg <- tail(stats::na.omit(plot_df$running_average), 1)
    subtitle <- if (length(final_avg) == 0) {
      paste0("No valid values across ", nrow(plot_df), " simulations")
    } else {
      paste0("Final running average: ", round(final_avg, 2), " after ", nrow(plot_df), " simulations")
    }

    p <- ggplot(plot_df, aes(x = sim_number, y = running_average)) +
      geom_line(linewidth = 1)

    if (length(final_avg) > 0) {
      p <- p + geom_hline(yintercept = final_avg, linetype = 2)
    }

    p +
      labs(
        title = title,
        subtitle = subtitle,
        x = "Simulation number",
        y = y_label
      ) +
      dist_plot_theme()
  }

  output$home_points_convergence <- renderPlot({
    req(sim_result())
    home_team <- sim_result()$cfg$home_team
    make_convergence_plot("home_score", paste0(home_team, " points convergence"), "Running average points")
  })

  output$away_points_convergence <- renderPlot({
    req(sim_result())
    away_team <- sim_result()$cfg$away_team
    make_convergence_plot("away_score", paste0(away_team, " points convergence"), "Running average points")
  })

  output$margin_convergence <- renderPlot({
    req(sim_result())
    make_convergence_plot("home_win_spread", "Home-team margin convergence", "Running average home margin")
  })

  output$total_points_convergence <- renderPlot({
    req(sim_result())
    make_convergence_plot("total_points_scored", "Total points convergence", "Running average total points")
  })



  output$fourth_down_team_summary <- renderTable({
    req(sim_result())

    fd_summary <- sim_result()$fourth_down_summary %>%
      as.data.frame(stringsAsFactors = FALSE)

    if ("raw_go_for_it_prob" %in% names(fd_summary) && !"go_for_it_prob" %in% names(fd_summary)) {
      fd_summary <- dplyr::rename(fd_summary, go_for_it_prob = raw_go_for_it_prob)
    }
    if ("raw_punt_prob" %in% names(fd_summary) && !"punt_prob" %in% names(fd_summary)) {
      fd_summary <- dplyr::rename(fd_summary, punt_prob = raw_punt_prob)
    }
    if ("raw_field_goal_attempt_prob" %in% names(fd_summary) && !"field_goal_attempt_prob" %in% names(fd_summary)) {
      fd_summary <- dplyr::rename(fd_summary, field_goal_attempt_prob = raw_field_goal_attempt_prob)
    }

    display_cols <- c(
      "side",
      "team",
      "sims",
      "total_fourth_downs",
      "avg_fourth_downs_per_sim",
      "go_for_it_prob",
      "qualifying_opportunities",
      "qualifying_go_calls",
      "qualifying_go_for_it_prob",
      "conversion_when_going_prob",
      "turnover_on_downs_when_going_prob",
      "punt_prob",
      "field_goal_attempt_prob",
      "made_field_goal_when_trying_prob"
    )

    integer_cols <- intersect(
      c("sims", "total_fourth_downs", "qualifying_opportunities", "qualifying_go_calls"),
      names(fd_summary)
    )

    fd_summary %>%
      dplyr::select(dplyr::any_of(display_cols)) %>%
      format_probability_columns(digits = 1) %>%
      safe_table(
        digits = 1,
        integer_cols = integer_cols,
        fixed = TRUE
      )
  }, digits = 1)

  build_fourth_down_rollup <- function(detail_df, group_cols) {
    if (is.null(detail_df) || nrow(detail_df) == 0) {
      return(data.frame())
    }

    score_state_levels <- c("Trailing 9+", "Trailing 1-8", "Tied", "Leading 1-8", "Leading 9+")
    field_zone_levels <- c("Own territory", "Fringe", "FG range", "High red zone")
    ydstogo_levels <- c("1", "2", "3", "4-5", "6+")

    df <- detail_df %>%
      dplyr::mutate(
        score_state = factor(score_state, levels = score_state_levels, ordered = TRUE),
        field_zone = factor(field_zone, levels = field_zone_levels, ordered = TRUE),
        ydstogo_bucket = factor(ydstogo_bucket, levels = ydstogo_levels, ordered = TRUE),
        est_converted_go_calls = dplyr::coalesce(conversions_when_going_prob, 0) * dplyr::coalesce(go_calls, 0)
      ) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::summarise(
        opportunities = sum(opportunities, na.rm = TRUE),
        go_calls = sum(go_calls, na.rm = TRUE),
        punts = sum(punts, na.rm = TRUE),
        field_goal_attempts = sum(field_goal_attempts, na.rm = TRUE),
        est_converted_go_calls = sum(est_converted_go_calls, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        actual_go_for_it_prob = dplyr::if_else(opportunities > 0, go_calls / opportunities, NA_real_),
        conversions_when_going_prob = dplyr::if_else(go_calls > 0, est_converted_go_calls / go_calls, NA_real_)
      ) %>%
      dplyr::select(-est_converted_go_calls)

    if ("side" %in% names(df)) {
      df$side <- factor(df$side, levels = c("Home", "Away"), ordered = TRUE)
    }
    if ("team" %in% names(df)) {
      df$team <- as.character(df$team)
    }

    df <- df %>% dplyr::arrange(dplyr::across(dplyr::all_of(intersect(c("side", "team", "ydstogo_bucket", "score_state", "field_zone"), names(df)))))

    df
  }

  output$fourth_down_field_zone_summary <- renderTable({
    req(sim_result())
    build_fourth_down_rollup(
      detail_df = sim_result()$fourth_down_bucket_detail,
      group_cols = c("side", "team", "field_zone")
    ) %>%
      format_probability_columns(digits = 1) %>%
      safe_table(
        digits = 1,
        integer_cols = c("opportunities", "go_calls", "punts", "field_goal_attempts"),
        fixed = TRUE
      )
  }, digits = 1)

  output$fourth_down_score_state_summary <- renderTable({
    req(sim_result())
    build_fourth_down_rollup(
      detail_df = sim_result()$fourth_down_bucket_detail,
      group_cols = c("side", "team", "score_state")
    ) %>%
      format_probability_columns(digits = 1) %>%
      safe_table(
        digits = 1,
        integer_cols = c("opportunities", "go_calls", "punts", "field_goal_attempts"),
        fixed = TRUE
      )
  }, digits = 1)

  output$fourth_down_overlay_diagnostics <- renderTable({
    req(sim_result())

    overlay_df <- sim_result()$fourth_down_overlay_detail %>%
      as.data.frame(stringsAsFactors = FALSE)

    if (is.null(overlay_df) || nrow(overlay_df) == 0) {
      return(data.frame())
    }

    score_state_levels <- c("Trailing 9+", "Trailing 1-8", "Tied", "Leading 1-8", "Leading 9+")
    clock_bucket_levels <- c("Q1", "Q2", "Q3", "Q4 early", "Q4 late")

    overlay_df %>%
      dplyr::mutate(
        side = factor(side, levels = c("Home", "Away"), ordered = TRUE),
        score_state = factor(score_state, levels = score_state_levels, ordered = TRUE),
        clock_bucket = factor(clock_bucket, levels = clock_bucket_levels, ordered = TRUE),
        overlay_applied_reason = dplyr::case_when(
          dplyr::coalesce(full_context_share, 0) > 0 & dplyr::coalesce(score_time_share, 0) == 0 ~ "Full context overlay",
          dplyr::coalesce(score_time_share, 0) > 0 & dplyr::coalesce(full_context_share, 0) == 0 ~ "Score/time fallback overlay",
          dplyr::coalesce(full_context_share, 0) > 0 & dplyr::coalesce(score_time_share, 0) > 0 ~ paste0(
            "Mixed overlay (full ", formatC(100 * full_context_share, format = "f", digits = 0),
            "%, score/time ", formatC(100 * score_time_share, format = "f", digits = 0), "%)"
          ),
          dplyr::coalesce(disabled_share, 0) > 0 ~ "Overlay disabled",
          dplyr::coalesce(no_overlay_share, 0) > 0 ~ "No overlay source found",
          TRUE ~ "Other overlay path"
        ),
        overlay_effect = dplyr::case_when(
          avg_overlay_pct_points >= 3 ~ "Higher go rate",
          avg_overlay_pct_points >= 0.5 ~ "Slightly higher go rate",
          avg_overlay_pct_points <= -3 ~ "Lower go rate",
          avg_overlay_pct_points <= -0.5 ~ "Slightly lower go rate",
          TRUE ~ "Little change"
        )
      ) %>%
      dplyr::select(
        side,
        team,
        overlay_applied_reason,
        score_state,
        clock_bucket,
        opportunities,
        avg_base_go_for_it_prob,
        avg_adjusted_go_for_it_prob,
        avg_overlay_pct_points,
        overlay_effect,
        actual_go_for_it_prob
      ) %>%
      dplyr::arrange(side, team, score_state, clock_bucket) %>%
      format_probability_columns(digits = 1) %>%
      safe_table(
        digits = 2,
        integer_cols = c("opportunities"),
        fixed = TRUE
      )
  }, digits = 1)

  output$fourth_down_normal_diagnostics <- renderTable({
    req(sim_result())

    normal_df <- sim_result()$fourth_down_normal_detail %>%
      as.data.frame(stringsAsFactors = FALSE)

    if (is.null(normal_df) || nrow(normal_df) == 0) {
      return(data.frame())
    }

    score_state_levels <- c("Trailing 9+", "Trailing 1-8", "Tied", "Leading 1-8", "Leading 9+")
    clock_bucket_levels <- c("Q1", "Q2", "Q3", "Q4 early", "Q4 late")
    reason_levels <- c(
      "Too long (6+ yards)",
      "Late blowout excluded",
      "Overtime excluded",
      "Missing context",
      "Qualifying play, no overlay diagnostics",
      "Other normal play"
    )

    normal_df %>%
      dplyr::mutate(
        side = factor(side, levels = c("Home", "Away"), ordered = TRUE),
        overlay_scope_reason = factor(overlay_scope_reason, levels = reason_levels, ordered = TRUE),
        score_state = factor(score_state, levels = score_state_levels, ordered = TRUE),
        clock_bucket = factor(clock_bucket, levels = clock_bucket_levels, ordered = TRUE)
      ) %>%
      dplyr::arrange(side, team, overlay_scope_reason, score_state, clock_bucket) %>%
      format_probability_columns(digits = 1) %>%
      safe_table(
        digits = 1,
        integer_cols = c("opportunities", "go_calls", "punts", "field_goal_attempts"),
        fixed = TRUE
      )
  }, digits = 1)

  output$fourth_down_distance_summary <- renderTable({
    req(sim_result())
    build_fourth_down_rollup(
      detail_df = sim_result()$fourth_down_bucket_detail,
      group_cols = c("side", "team", "ydstogo_bucket")
    ) %>%
      format_probability_columns(digits = 1) %>%
      safe_table(
        digits = 1,
        integer_cols = c("opportunities", "go_calls", "punts", "field_goal_attempts"),
        fixed = TRUE
      )
  }, digits = 1)

  output$top_dk_by_team <- renderTable({
    req(sim_result())
    safe_table(sim_result()$top_dk_by_team)
  }, digits = 2)

  output$sim_results_head <- DT::renderDT({
    req(sim_result())

    cfg_row <- sim_result()$param_snapshot %>% dplyr::slice(1)
    results_df <- sim_result()$results_adj %>%
      dplyr::mutate(
        def_adj_home = cfg_row$def_add_to_home_score[[1]],
        def_adj_away = cfg_row$def_add_to_away_score[[1]],
        hfa_to_home = cfg_row$hfa_add_to_home_score[[1]],
        total_adj_home = def_adj_home + hfa_to_home,
        total_adj_away = def_adj_away - hfa_to_home,
        raw_home_score = home_score - total_adj_home,
        raw_away_score = away_score - total_adj_away,
        final_adjusted_home_score = home_score,
        final_adjusted_away_score = away_score
      ) %>%
      dplyr::select(
        sim,
        home_team,
        away_team,
        raw_home_score,
        raw_away_score,
        final_adjusted_home_score,
        final_adjusted_away_score,
        def_adj_home,
        def_adj_away,
        hfa_to_home,
        total_adj_home,
        total_adj_away,
        home_turnovers,
        away_turnovers,
        turnover_differential,
        opening_kickoff_team,
        end_first_half_team,
        second_half_kickoff_team,
        has_back_to_back_possession,
        end_of_game_team,
        dplyr::everything(),
        -home_score,
        -away_score
      )

    integer_cols <- intersect(
      c(
        "sim", "raw_home_score", "raw_away_score", "final_adjusted_home_score",
        "final_adjusted_away_score", "home_turnovers", "away_turnovers",
        "turnover_differential", "home_win_spread", "total_points_scored",
        "home_field_goals", "away_field_goals"
      ),
      names(results_df)
    )
    one_decimal_cols <- intersect(
      c(
        "def_adj_home", "def_adj_away", "hfa_to_home", "total_adj_home", "total_adj_away",
        "market_spread_home", "market_spread_away", "home_cover_margin",
        "away_cover_margin", "market_total", "total_margin"
      ),
      names(results_df)
    )
    two_decimal_cols <- setdiff(names(results_df)[vapply(results_df, is.numeric, logical(1))], c(integer_cols, one_decimal_cols))
    numeric_targets <- which(vapply(results_df, is.numeric, logical(1))) - 1L

    dt <- DT::datatable(
      results_df,
      rownames = FALSE,
      class = 'compact stripe hover nowrap',
      extensions = c('FixedHeader', 'FixedColumns'),
      options = list(
        pageLength = 25,
        lengthMenu = list(c(25, 100, -1), c('25', '100', 'All')),
        scrollX = TRUE,
        scrollY = '65vh',
        scrollCollapse = TRUE,
        autoWidth = FALSE,
        fixedHeader = TRUE,
        fixedColumns = list(leftColumns = 3),
        order = list(list(0, 'asc')),
        columnDefs = list(
          list(className = 'dt-body-right dt-head-right', targets = numeric_targets)
        )
      )
    )

    if (length(integer_cols) > 0) dt <- DT::formatRound(dt, columns = integer_cols, digits = 0)
    if (length(one_decimal_cols) > 0) dt <- DT::formatRound(dt, columns = one_decimal_cols, digits = 1)
    if (length(two_decimal_cols) > 0) dt <- DT::formatRound(dt, columns = two_decimal_cols, digits = 2)
    dt
  }, server = FALSE)

  output$sample_sim_pbp <- DT::renderDT({
    req(sim_result())
    tryCatch({
      pbp_df <- make_sample_play_by_play_table(sim_result()$sample_game_drives, sim_result()$cfg)

      if (is.null(pbp_df) || ncol(pbp_df) == 0) {
        pbp_df <- data.frame(Message = "No sample play-by-play was captured for this run.", stringsAsFactors = FALSE)
      }

      if (identical(names(pbp_df), "Message")) {
        return(
          DT::datatable(
            pbp_df,
            rownames = FALSE,
            class = 'compact stripe hover nowrap',
            options = list(dom = 't', ordering = FALSE, paging = FALSE, searching = FALSE, info = FALSE)
          )
        )
      }

      DT::datatable(
        pbp_df,
        rownames = FALSE,
        class = 'compact stripe hover nowrap',
        options = list(
          pageLength = 50,
          lengthMenu = list(c(25, 50, 100, -1), c('25', '50', '100', 'All')),
          scrollX = TRUE,
          scrollY = '68vh',
          scrollCollapse = TRUE,
          autoWidth = TRUE,
          order = list(list(0, 'asc')),
          columnDefs = list(
            list(className = 'dt-body-right dt-head-right', targets = c(0, 1, 5, 6, 7, 9, 17, 19)),
            list(className = 'dt-body-left dt-head-left', targets = c(2, 3, 4, 8, 10, 11, 12, 13, 14, 15, 16, 18, 20))
          )
        ),
        callback = JS("
        var api = table.api();
        setTimeout(function(){ api.columns.adjust(); }, 0);
        $(window).on('resize.samplePbp', function(){ api.columns.adjust(); });
        $('a[data-toggle=\"tab\"], button[data-bs-toggle=\"tab\"], a[data-bs-toggle=\"tab\"]').on('shown.bs.tab.samplePbp shown.bs.tab', function(){
          setTimeout(function(){ api.columns.adjust(); }, 0);
        });
      ")
      )
    }, error = function(e) {
      DT::datatable(
        data.frame(Message = paste0("Sample play-by-play render error: ", conditionMessage(e)), stringsAsFactors = FALSE),
        rownames = FALSE,
        class = 'compact stripe hover nowrap',
        options = list(dom = 't', ordering = FALSE, paging = FALSE, searching = FALSE, info = FALSE)
      )
    })
  }, server = FALSE)

}

shinyApp(ui, server)
