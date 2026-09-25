# R/14_xgboost_billy_total_score_trees.R
# Wrapper for Step 11 Billy Walters XGBoost total/score tree family.
#
# This wrapper adapts legacy Step 11 Billy-tree templates so they can run in the
# unified pipeline with target-season-specific params and current Step 10 outputs.
# The main compatibility bridge is explicit: before the legacy template selects
# columns, data/dataTrees are guaranteed to include Score_avg, OppScore_avg, and
# ScoreTotal_avg when Step 10 outputs contain enough information to derive them.

xgb_billy_tree_param_file <- function(target_season, model_key, output_dir) {
  model_key <- match.arg(model_key, c("diff", "total", "score", "score_home", "score_away"))
  file.path(output_dir, paste0("xgb_billy_trees_", target_season, "_", model_key, "_best_params.rds"))
}

xgb_billy_tree_param_files <- function(target_season, output_dir) {
  c(
    diff       = xgb_billy_tree_param_file(target_season, "diff", output_dir),
    total      = xgb_billy_tree_param_file(target_season, "total", output_dir),
    score      = xgb_billy_tree_param_file(target_season, "score", output_dir),
    score_home = xgb_billy_tree_param_file(target_season, "score_home", output_dir),
    score_away = xgb_billy_tree_param_file(target_season, "score_away", output_dir)
  )
}

xgb_billy_tree_bind_rows_fill <- function(x, y) {
  # read.csv(check.names = FALSE) preserves the unnamed CSV row-index column;
  # selecting that empty name later fails with "undefined columns selected".
  x <- as.data.frame(x[, nzchar(names(x)), drop = FALSE], check.names = FALSE)
  y <- as.data.frame(y[, nzchar(names(y)), drop = FALSE], check.names = FALSE)
  all_cols <- union(names(x), names(y))
  for (nm in setdiff(all_cols, names(x))) x[[nm]] <- NA
  for (nm in setdiff(all_cols, names(y))) y[[nm]] <- NA
  rbind(x[, all_cols, drop = FALSE], y[, all_cols, drop = FALSE])
}

xgb_billy_tree_match_col <- function(data, candidates) {
  if (!is.data.frame(data)) return(character(0))
  nms <- names(data)
  idx <- match(tolower(candidates), tolower(nms), nomatch = 0L)
  unique(nms[idx[idx > 0L]])
}

xgb_billy_tree_numeric_cols <- function(data, cols) {
  cols <- cols[cols %in% names(data)]
  cols[vapply(cols, function(nm) is.numeric(data[[nm]]), logical(1))]
}

xgb_billy_tree_bad_score_col <- function(cols) {
  grepl(
    paste(c(
      "^home_score$", "^away_score$", "actual", "target", "result", "final",
      "posteam_score$", "defteam_score$", "drive", "ended_with", "rate", "pct",
      "points_allowed", "points_for", "spread", "line", "diff", "margin"
    ), collapse = "|"),
    cols,
    ignore.case = TRUE
  )
}

xgb_billy_tree_find_score_cols <- function(data) {
  if (!is.data.frame(data)) return(character(0))

  home_candidates <- c(
    "Score_home_avg", "score_home_avg", "Score_Home_avg", "Score_Home_Avg",
    "Home_Score_avg", "HomeScore_avg", "ScoreHome_avg", "home_score_avg",
    "NFL_avg_home_score", "nfl_avg_home_score",
    "xgb_home_score_avg", "home_xgb_score_avg", "Home_XGB_Score_Avg",
    "home_score_pred", "Home_score_pred", "Score_home_pred", "score_home_pred",
    "pred_home_score", "Pred_home_score", "home_pred_score", "Home_pred_score",
    "xgb_home_score", "home_xgb_score", "Home_XGB_Score",
    "score_tree_home", "ScoreTree_home", "Score_Tree_home", "home_score_tree"
  )
  away_candidates <- c(
    "Score_away_avg", "score_away_avg", "Score_Away_avg", "Score_Away_Avg",
    "Away_Score_avg", "AwayScore_avg", "ScoreAway_avg", "away_score_avg",
    "NFL_avg_away_score", "nfl_avg_away_score",
    "xgb_away_score_avg", "away_xgb_score_avg", "Away_XGB_Score_Avg",
    "away_score_pred", "Away_score_pred", "Score_away_pred", "score_away_pred",
    "pred_away_score", "Pred_away_score", "away_pred_score", "Away_pred_score",
    "xgb_away_score", "away_xgb_score", "Away_XGB_Score",
    "score_tree_away", "ScoreTree_away", "Score_Tree_away", "away_score_tree"
  )

  home_hits <- xgb_billy_tree_numeric_cols(data, xgb_billy_tree_match_col(data, home_candidates))
  away_hits <- xgb_billy_tree_numeric_cols(data, xgb_billy_tree_match_col(data, away_candidates))
  home_hits <- home_hits[!xgb_billy_tree_bad_score_col(home_hits)]
  away_hits <- away_hits[!xgb_billy_tree_bad_score_col(away_hits)]

  if (length(home_hits) > 0 && length(away_hits) > 0) {
    return(c(home = home_hits[1], away = away_hits[1]))
  }
  character(0)
}

xgb_billy_tree_find_generic_score_col <- function(data) {
  if (!is.data.frame(data)) return(character(0))
  exact <- c(
    "Score_avg", "score_avg", "Score_Avg", "score_Avg", "ScoreAverage", "score_average",
    "Score_Tree_Avg", "score_tree_avg", "ScoresTree_avg", "scores_tree_avg",
    "xgb_score_avg", "XGB_Score_Avg", "xgb_Score_avg", "XGBScore_avg",
    "Score_pred", "score_pred", "PredScore", "pred_score", "predicted_score", "PredictedScore",
    "score_prediction", "Score_prediction", "xgb_score", "XGB_Score", "Score_XGB", "score_xgb",
    "score_model", "Score_model", "model_score", "Model_score", "forecast_score", "Score_forecast"
  )
  hits <- xgb_billy_tree_numeric_cols(data, xgb_billy_tree_match_col(data, exact))

  fallback <- names(data)[
    grepl("score", names(data), ignore.case = TRUE) &
      grepl("xgb|pred|tree|model|forecast", names(data), ignore.case = TRUE)
  ]
  fallback <- xgb_billy_tree_numeric_cols(data, fallback)
  hits <- unique(c(hits, fallback))
  hits[!xgb_billy_tree_bad_score_col(hits) & !grepl("opp|opponent|total", hits, ignore.case = TRUE)]
}

xgb_billy_tree_find_total_col <- function(data) {
  if (!is.data.frame(data)) return(character(0))
  exact <- c(
    "ScoreTotal_avg", "scoretotal_avg", "Score_Total_avg", "score_total_avg",
    "TotalScore_avg", "total_score_avg", "Total_avg", "total_avg",
    "ScoreTotal_pred", "score_total_pred", "total_pred", "PredTotal", "pred_total",
    "xgb_total_avg", "XGB_Total_Avg", "xgb_total", "XGB_Total",
    "total_tree_avg", "ScoreTotal_Tree_Avg", "score_total_tree_avg"
  )
  hits <- xgb_billy_tree_numeric_cols(data, xgb_billy_tree_match_col(data, exact))
  fallback <- names(data)[
    grepl("total", names(data), ignore.case = TRUE) &
      grepl("xgb|pred|tree|model|forecast|avg", names(data), ignore.case = TRUE)
  ]
  fallback <- xgb_billy_tree_numeric_cols(data, fallback)
  hits <- unique(c(hits, fallback))
  hits[!grepl("actual|target|result|line|over|under|diff|margin|rate|pct", hits, ignore.case = TRUE)]
}

xgb_billy_tree_pair_opp_score_from_rows <- function(data) {
  if (!is.data.frame(data) || !all(c("Score_avg", "posteam_type") %in% names(data))) return(data)
  if ("OppScore_avg" %in% names(data) && any(!is.na(data$OppScore_avg))) return(data)

  key_sets <- list(
    c("game_id"),
    c("season", "week", "home_team", "away_team"),
    c("season", "week", "posteam", "defteam"),
    c("season", "week", "team", "opponent")
  )

  for (keys in key_sets) {
    if (!all(keys %in% names(data))) next
    dat <- data
    dat$.__row_id <- seq_len(nrow(dat))
    dat$.__opp_type <- ifelse(tolower(as.character(dat$posteam_type)) == "home", "away", "home")

    opp <- data[, c(keys, "posteam_type", "Score_avg"), drop = FALSE]
    opp$.__opp_type <- tolower(as.character(opp$posteam_type))
    opp$OppScore_avg <- opp$Score_avg
    opp <- opp[, c(keys, ".__opp_type", "OppScore_avg"), drop = FALSE]
    opp <- opp[!duplicated(opp[, c(keys, ".__opp_type"), drop = FALSE]), , drop = FALSE]

    merged <- merge(dat, opp, by = c(keys, ".__opp_type"), all.x = TRUE, sort = FALSE)
    if (nrow(merged) == nrow(data) && "OppScore_avg" %in% names(merged) && any(!is.na(merged$OppScore_avg))) {
      merged <- merged[order(merged$.__row_id), , drop = FALSE]
      merged$.__row_id <- NULL
      merged$.__opp_type <- NULL
      return(merged)
    }
  }

  data
}

xgb_billy_tree_complete_legacy_score_columns <- function(data, stop_on_fail = TRUE, source_label = "Step 10 bridge") {
  if (!is.data.frame(data)) return(data)

  side_cols <- xgb_billy_tree_find_score_cols(data)

  if (!"Score_avg" %in% names(data)) {
    if (length(side_cols) == 2 && "posteam_type" %in% names(data)) {
      data$Score_avg <- ifelse(
        tolower(as.character(data$posteam_type)) == "home",
        data[[side_cols[["home"]]]],
        data[[side_cols[["away"]]]]
      )
      message("Step 11 Billy XGBoost: created legacy Score_avg from ", side_cols[["home"]], " / ", side_cols[["away"]], ".")
    } else {
      hit <- xgb_billy_tree_find_generic_score_col(data)
      if (length(hit) > 0) {
        data$Score_avg <- data[[hit[1]]]
        message("Step 11 Billy XGBoost: created legacy Score_avg from ", hit[1], ".")
      }
    }
  }

  if (!"OppScore_avg" %in% names(data)) {
    opp_exact <- c(
      "OppScore_avg", "opp_score_avg", "OpponentScore_avg", "opponent_score_avg",
      "Opp_Score_avg", "Opponent_Score_avg", "OppScore_pred", "opp_score_pred",
      "OpponentScore_pred", "opponent_score_pred", "Opp_XGB_Score", "opponent_xgb_score"
    )
    opp_hit <- xgb_billy_tree_numeric_cols(data, xgb_billy_tree_match_col(data, opp_exact))
    opp_hit <- setdiff(opp_hit, "Score_avg")
    if (length(opp_hit) > 0) {
      data$OppScore_avg <- data[[opp_hit[1]]]
      message("Step 11 Billy XGBoost: created legacy OppScore_avg from ", opp_hit[1], ".")
    } else if (length(side_cols) == 2 && "posteam_type" %in% names(data)) {
      data$OppScore_avg <- ifelse(
        tolower(as.character(data$posteam_type)) == "home",
        data[[side_cols[["away"]]]],
        data[[side_cols[["home"]]]]
      )
      message("Step 11 Billy XGBoost: created legacy OppScore_avg from ", side_cols[["home"]], " / ", side_cols[["away"]], ".")
    } else {
      data <- xgb_billy_tree_pair_opp_score_from_rows(data)
      if ("OppScore_avg" %in% names(data) && any(!is.na(data$OppScore_avg))) {
        message("Step 11 Billy XGBoost: created legacy OppScore_avg by pairing home/away Score_avg rows.")
      }
    }
  }

  if (!"ScoreTotal_avg" %in% names(data)) {
    total_hit <- xgb_billy_tree_find_total_col(data)
    if (length(total_hit) > 0) {
      data$ScoreTotal_avg <- data[[total_hit[1]]]
      message("Step 11 Billy XGBoost: created legacy ScoreTotal_avg from ", total_hit[1], ".")
    } else if (all(c("Score_avg", "OppScore_avg") %in% names(data))) {
      data$ScoreTotal_avg <- data$Score_avg + data$OppScore_avg
      message("Step 11 Billy XGBoost: created legacy ScoreTotal_avg from Score_avg + OppScore_avg.")
    }
  }

  # Extra compatibility columns that some converted Billy templates may reference.
  if (!"OppScoreTotal_avg" %in% names(data) && "ScoreTotal_avg" %in% names(data)) {
    data$OppScoreTotal_avg <- data$ScoreTotal_avg
  }
  if (!"ScoreDiff_avg" %in% names(data) && all(c("Score_avg", "OppScore_avg") %in% names(data))) {
    data$ScoreDiff_avg <- data$Score_avg - data$OppScore_avg
  }
  if (!"OppScoreDiff_avg" %in% names(data) && "ScoreDiff_avg" %in% names(data)) {
    data$OppScoreDiff_avg <- -data$ScoreDiff_avg
  }

  required <- c("Score_avg", "OppScore_avg", "ScoreTotal_avg")
  ok <- all(required %in% names(data))
  if (!ok && isTRUE(stop_on_fail)) {
    score_like <- names(data)[grepl("score|total|xgb|pred|avg|tree|model|forecast", names(data), ignore.case = TRUE)]
    stop(
      "Step 11 Billy XGBoost expected legacy columns ", paste(required, collapse = ", "),
      " after ", source_label, ". Available score/total-like columns: ", paste(score_like, collapse = ", "),
      call. = FALSE
    )
  }

  data
}

# Backwards-compatible name used by older generated replacement blocks.
xgb_billy_tree_add_score_avg_if_needed <- function(data, stop_on_fail = TRUE, source_label = "Step 10 bridge") {
  xgb_billy_tree_complete_legacy_score_columns(data, stop_on_fail = stop_on_fail, source_label = source_label)
}

xgb_billy_tree_merge_step10_to_base <- function(base_data, step10_data) {
  if (!is.data.frame(base_data) || !is.data.frame(step10_data)) return(step10_data)
  # Use base data.frame column selection; data.table's character-vector
  # subsetting otherwise throws and silently sends this bridge to a fallback.
  base_data <- as.data.frame(base_data, check.names = FALSE)
  step10_data <- as.data.frame(step10_data, check.names = FALSE)
  step10_data <- xgb_billy_tree_complete_legacy_score_columns(step10_data, stop_on_fail = FALSE, source_label = "Step 10 merge input")
  if (!all(c("Score_avg", "OppScore_avg", "ScoreTotal_avg") %in% names(step10_data))) return(step10_data)

  key_sets <- list(
    c("game_id", "posteam_type"),
    c("season", "week", "home_team", "away_team", "posteam_type"),
    c("season", "week", "posteam", "defteam", "posteam_type")
  )

  for (keys in key_sets) {
    if (!all(keys %in% names(base_data)) || !all(keys %in% names(step10_data))) next

    add_cols <- unique(c(keys, "Score_avg", "OppScore_avg", "ScoreTotal_avg", "OppScoreTotal_avg", "ScoreDiff_avg", "OppScoreDiff_avg"))
    add_cols <- add_cols[add_cols %in% names(step10_data)]
    add <- step10_data[, add_cols, drop = FALSE]
    add <- add[!duplicated(add[, keys, drop = FALSE]), , drop = FALSE]

    base <- base_data
    base$.__row_id <- seq_len(nrow(base))
    for (nm in setdiff(add_cols, keys)) {
      if (nm %in% names(base)) base[[nm]] <- NULL
    }

    merged <- merge(base, add, by = keys, all.x = FALSE, sort = FALSE)
    if (nrow(merged) > 0 && all(c("Score_avg", "OppScore_avg", "ScoreTotal_avg") %in% names(merged))) {
      merged <- merged[order(merged$.__row_id), , drop = FALSE]
      merged$.__row_id <- NULL
      message(
        "Step 11 Billy XGBoost: merged Step 10 Score_avg/OppScore_avg/ScoreTotal_avg into data_v12 using keys: ",
        paste(keys, collapse = ", "), "; matched rows: ", nrow(merged), "."
      )
      return(merged)
    }
  }

  step10_data
}

xgb_billy_tree_read_step10_csv_bridge <- function(output_dir, target_season, base_data = NULL) {
  home_file <- file.path(output_dir, paste0(target_season, "_ScoresTrees_home.csv"))
  away_file <- file.path(output_dir, paste0(target_season, "_ScoresTrees_away.csv"))
  if (!file.exists(home_file) || !file.exists(away_file)) return(NULL)

  home <- utils::read.csv(home_file, stringsAsFactors = FALSE, check.names = FALSE)
  away <- utils::read.csv(away_file, stringsAsFactors = FALSE, check.names = FALSE)
  if (!"posteam_type" %in% names(home)) home$posteam_type <- "home"
  if (!"posteam_type" %in% names(away)) away$posteam_type <- "away"

  home <- xgb_billy_tree_complete_legacy_score_columns(home, stop_on_fail = FALSE, source_label = basename(home_file))
  away <- xgb_billy_tree_complete_legacy_score_columns(away, stop_on_fail = FALSE, source_label = basename(away_file))

  # Pair by game identity. Step 10 home and away files have the same games but
  # are not guaranteed to have the same row order.
  if ("Score_avg" %in% names(home) && "Score_avg" %in% names(away) && nrow(home) == nrow(away)) {
    if (!all(c("game_id") %in% names(home)) || !all(c("game_id") %in% names(away)) ||
        anyDuplicated(home$game_id) || anyDuplicated(away$game_id) ||
        !setequal(home$game_id, away$game_id)) {
      stop("Step 10 home/away score files do not have unique matching game IDs", call. = FALSE)
    }
    if (!"OppScore_avg" %in% names(home) || all(is.na(home$OppScore_avg))) {
      home$OppScore_avg <- away$Score_avg[match(home$game_id, away$game_id)]
    }
    if (!"OppScore_avg" %in% names(away) || all(is.na(away$OppScore_avg))) {
      away$OppScore_avg <- home$Score_avg[match(away$game_id, home$game_id)]
    }
    message("Step 11 Billy XGBoost: paired Step 10 home/away scores by game_id.")
  }

  data <- xgb_billy_tree_bind_rows_fill(home, away)
  data <- xgb_billy_tree_complete_legacy_score_columns(data, stop_on_fail = FALSE, source_label = "Step 10 CSV outputs")
  if (!all(c("Score_avg", "OppScore_avg", "ScoreTotal_avg") %in% names(data))) {
    stop(
      "Step 10 CSV outputs did not provide enough columns to create Score_avg/OppScore_avg/ScoreTotal_avg. ",
      "Home columns: ", paste(names(home), collapse = ", "),
      " | Away columns: ", paste(names(away), collapse = ", "),
      call. = FALSE
    )
  }

  merged <- xgb_billy_tree_merge_step10_to_base(base_data, data)
  if (is.data.frame(merged) && all(c("Score_avg", "OppScore_avg", "ScoreTotal_avg") %in% names(merged))) return(merged)
  data
}

xgb_billy_tree_rdata_candidate_score <- function(nm, obj) {
  if (!is.data.frame(obj)) return(-Inf)
  nms <- names(obj)
  score <- 0
  if (grepl("template|skeleton|schedule", nm, ignore.case = TRUE)) score <- score - 10000
  if (all(c("season", "posteam_type") %in% nms)) score <- score + 100
  if ("week" %in% nms) score <- score + 25
  if ("game_id" %in% nms) score <- score + 10
  if (all(c("Score_avg", "OppScore_avg", "ScoreTotal_avg") %in% nms)) score <- score + 20000
  if (all(c("Score_avg", "OppScore_avg") %in% nms)) score <- score + 10000
  if (length(xgb_billy_tree_find_score_cols(obj)) == 2) score <- score + 5000
  if (length(xgb_billy_tree_find_generic_score_col(obj)) > 0) score <- score + 1000
  if (length(xgb_billy_tree_find_total_col(obj)) > 0) score <- score + 500
  only_actual_score_cols <- any(c("home_score", "away_score") %in% nms) &&
    length(xgb_billy_tree_find_generic_score_col(obj)) == 0 &&
    length(xgb_billy_tree_find_score_cols(obj)) != 2 &&
    !all(c("Score_avg", "OppScore_avg") %in% nms)
  if (only_actual_score_cols) score <- score - 5000
  score
}

xgb_billy_tree_get_step10_data <- function(output_dir, target_season, step10_env = NULL, loaded_step10_objects = character(0), base_data = NULL) {
  csv_try <- tryCatch(
    xgb_billy_tree_read_step10_csv_bridge(output_dir, target_season, base_data = base_data),
    error = function(e) e
  )
  if (is.data.frame(csv_try) && all(c("Score_avg", "OppScore_avg", "ScoreTotal_avg") %in% names(csv_try))) {
    message("Step 11 Billy XGBoost: using Step 10 CSV outputs for score bridge.")
    return(csv_try)
  }
  if (inherits(csv_try, "error")) {
    message("Step 11 Billy XGBoost: Step 10 CSV fallback could not be used: ", conditionMessage(csv_try))
  }

  if (is.null(step10_env) || length(loaded_step10_objects) == 0) {
    stop("Step 11 Billy XGBoost could not use Step 10 CSVs and no Step 10 RData objects were available.", call. = FALSE)
  }

  candidates <- loaded_step10_objects[vapply(loaded_step10_objects, function(nm) {
    exists(nm, envir = step10_env, inherits = FALSE) && is.data.frame(get(nm, envir = step10_env, inherits = FALSE))
  }, logical(1))]

  if (length(candidates) > 0) {
    scores <- vapply(candidates, function(nm) {
      obj <- get(nm, envir = step10_env, inherits = FALSE)
      obj2 <- tryCatch(xgb_billy_tree_complete_legacy_score_columns(obj, stop_on_fail = FALSE, source_label = nm), error = function(e) obj)
      xgb_billy_tree_rdata_candidate_score(nm, obj2)
    }, numeric(1))
    candidates <- candidates[is.finite(scores)]
    scores <- scores[is.finite(scores)]

    if (length(candidates) > 0) {
      ord <- order(-scores)
      for (idx in ord) {
        nm <- candidates[idx]
        obj <- get(nm, envir = step10_env, inherits = FALSE)
        obj <- xgb_billy_tree_complete_legacy_score_columns(obj, stop_on_fail = FALSE, source_label = paste0("RData object ", nm))
        obj <- xgb_billy_tree_merge_step10_to_base(base_data, obj)
        obj <- xgb_billy_tree_complete_legacy_score_columns(obj, stop_on_fail = FALSE, source_label = paste0("RData object ", nm))
        if (all(c("Score_avg", "OppScore_avg", "ScoreTotal_avg") %in% names(obj)) && any(!is.na(obj$Score_avg))) {
          message("Step 11 Billy XGBoost: using Step 10 RData object '", nm, "'.")
          return(obj)
        }
      }
    }
  }

  object_summary <- vapply(loaded_step10_objects, function(nm) {
    if (!exists(nm, envir = step10_env, inherits = FALSE)) return(paste0(nm, "=<missing>"))
    obj <- get(nm, envir = step10_env, inherits = FALSE)
    if (is.data.frame(obj)) {
      cols <- names(obj)[grepl("score|total|xgb|pred|avg|tree|model|forecast", names(obj), ignore.case = TRUE)]
      paste0(nm, "=[", paste(utils::head(cols, 30), collapse = ", "), "]")
    } else {
      paste0(nm, "=", class(obj)[1])
    }
  }, character(1))

  csv_msg <- if (inherits(csv_try, "error")) conditionMessage(csv_try) else "not usable"
  stop(
    "Step 11 Billy XGBoost could not find Step 10 scored data with Score_avg, OppScore_avg, and ScoreTotal_avg. ",
    "CSV error: ", csv_msg, ". RData objects: ", paste(object_summary, collapse = " | "),
    call. = FALSE
  )
}

xgb_billy_tree_prepare_template_code <- function(code,
                                                 target_season,
                                                 output_dir,
                                                 holdout_test_season,
                                                 training_min_week,
                                                 exclude_training_seasons) {
  code <- gsub("setwd\\(['\"]\\.\\.['\"]\\)", "# setwd('..') removed by pipeline wrapper", code)

  code <- gsub(
    "load\\(\\s*(?:file\\s*=\\s*)?['\"]\\.\\./output/dataV12\\.RData['\"]\\s*\\)",
    "data <- data_v12",
    code,
    perl = TRUE
  )

  step10_load_re <- "load\\(\\s*(?:file\\s*=\\s*)?['\"]\\.\\./output/ScoresTrees[0-9]{4}(?:_fast)?\\.RData['\"]\\s*\\)"
  step10_load_replacement <- paste(
    "step10_rdata <- file.path(output_dir, paste0(\"ScoresTrees\", target_season, \"_fast.RData\"))",
    "if (!file.exists(step10_rdata)) {",
    "  step10_rdata <- file.path(output_dir, paste0(\"ScoresTrees\", target_season, \".RData\"))",
    "}",
    "step10_env <- new.env(parent = emptyenv())",
    "loaded_step10_objects <- character(0)",
    "if (file.exists(step10_rdata)) {",
    "  loaded_step10_objects <- load(step10_rdata, envir = step10_env)",
    "}",
    "data <- xgb_billy_tree_get_step10_data(output_dir, target_season, step10_env, loaded_step10_objects, base_data = data_v12)",
    "data <- xgb_billy_tree_complete_legacy_score_columns(data, stop_on_fail = TRUE, source_label = \"post Step 10 load\")",
    "dataTrees <- data",
    sep = "\n"
  )
  code2 <- gsub(step10_load_re, step10_load_replacement, code, perl = TRUE)
  if (identical(code2, code)) {
    stop("Step 11 template Step 10 load block was not replaced; refusing to use a hard-coded upstream file.", call. = FALSE)
  }
  code <- code2

  # Make any later legacy dataTrees assignment re-check all compatibility columns.
  code <- gsub(
    "dataTrees\\s*<-\\s*data",
    "dataTrees <- xgb_billy_tree_complete_legacy_score_columns(data, stop_on_fail = TRUE, source_label = \"dataTrees\")",
    code,
    perl = TRUE
  )

  split_re <- paste0(
    "library\\s*\\(\\s*caret\\s*\\)[\\s\\S]*?",
    "val_away\\s*(?:<-|=)\\s*subset\\s*\\(\\s*val\\s*,\\s*posteam_type\\s*==\\s*['\"]away['\"]\\s*\\)"
  )
  split_replacement <- paste(
    "library(caret)",
    "val <- data[data$season == target_season, ]",
    "test <- data[data$season == holdout_test_season, ]",
    "train_season_ok <- data$season < target_season",
    "completed_target_row <- is.finite(data$Score_target) & is.finite(data$ScoreTotal_target) & is.finite(data$ScoreDiff_target)",
    "train <- data[data$week >= training_min_week & train_season_ok & completed_target_row, ]",
    "if (length(exclude_training_seasons) > 0) {",
    "  train <- train[!(train$season %in% exclude_training_seasons), ]",
    "}",
    "train_home <- subset(train, posteam_type == \"home\")",
    "train_away <- subset(train, posteam_type == \"away\")",
    "test_home  <- subset(test, posteam_type == \"home\")",
    "test_away  <- subset(test, posteam_type == \"away\")",
    "val_home   <- subset(val, posteam_type == \"home\")",
    "val_away   <- subset(val, posteam_type == \"away\")",
    sep = "\n"
  )
  code2 <- gsub(split_re, split_replacement, code, perl = TRUE)
  if (identical(code2, code)) {
    stop("Step 11 template split block was not replaced; refusing to run with a hard-coded season split.", call. = FALSE)
  }
  code <- code2
  if (!grepl("train <- data[data$week >= training_min_week & train_season_ok & completed_target_row, ]", code, fixed = TRUE)) {
    stop("Step 11 target-driven training split validation failed.", call. = FALSE)
  }

  replacements <- c(
    '"../output/xgb_Billydiff_best_params.rds"'       = 'xgb_billy_tree_param_file(target_season, "diff", output_dir)',
    '"../output/xgb_Billytotal_best_params.rds"'      = 'xgb_billy_tree_param_file(target_season, "total", output_dir)',
    '"../output/xgb_Billyscore_best_params.rds"'      = 'xgb_billy_tree_param_file(target_season, "score", output_dir)',
    '"../output/xgb_Billyscore_home_best_params.rds"' = 'xgb_billy_tree_param_file(target_season, "score_home", output_dir)',
    '"../output/xgb_Billyscore_away_best_params.rds"' = 'xgb_billy_tree_param_file(target_season, "score_away", output_dir)',
    '"../output/xgb_score_away_best_params.rds"'      = 'xgb_billy_tree_param_file(target_season, "score_away", output_dir)',
    "'../output/xgb_Billydiff_best_params.rds'"       = 'xgb_billy_tree_param_file(target_season, "diff", output_dir)',
    "'../output/xgb_Billytotal_best_params.rds'"      = 'xgb_billy_tree_param_file(target_season, "total", output_dir)',
    "'../output/xgb_Billyscore_best_params.rds'"      = 'xgb_billy_tree_param_file(target_season, "score", output_dir)',
    "'../output/xgb_Billyscore_home_best_params.rds'" = 'xgb_billy_tree_param_file(target_season, "score_home", output_dir)',
    "'../output/xgb_Billyscore_away_best_params.rds'" = 'xgb_billy_tree_param_file(target_season, "score_away", output_dir)',
    "'../output/xgb_score_away_best_params.rds'"      = 'xgb_billy_tree_param_file(target_season, "score_away", output_dir)'
  )
  for (pat in names(replacements)) {
    code <- gsub(pat, replacements[[pat]], code, fixed = TRUE)
  }

  code <- gsub(
    "['\"]\\.\\./output/[0-9]{4}_BillyTrees_home\\.csv['\"]",
    'file.path(output_dir, paste0(target_season, "_BillyTrees_home.csv"))',
    code,
    perl = TRUE
  )
  code <- gsub(
    "['\"]\\.\\./output/[0-9]{4}_BillyTrees_away\\.csv['\"]",
    'file.path(output_dir, paste0(target_season, "_BillyTrees_away.csv"))',
    code,
    perl = TRUE
  )
  code <- gsub(
    "['\"]\\.\\./output/BillyTrees[0-9]{4}(?:_fast)?\\.RData['\"]",
    'file.path(output_dir, paste0("BillyTrees", target_season, ifelse(template_mode == "fast", "_fast", ""), ".RData"))',
    code,
    perl = TRUE
  )
  code <- gsub(
    "['\"]\\.\\./output/ScoresTreeLate[0-9]{4}_data\\.csv['\"]",
    'file.path(output_dir, paste0("BillyTrees", target_season, "_data.csv"))',
    code,
    perl = TRUE
  )
  code <- gsub(
    "['\"]\\.\\./output/ScoresTreeLate[0-9]{4}_data\\.RData['\"]",
    'file.path(output_dir, paste0("BillyTrees", target_season, "_data.RData"))',
    code,
    perl = TRUE
  )

  code
}

xgb_billy_tree_template_file <- function(target_season, mode = c("solver", "fast"), template_dir) {
  mode <- match.arg(mode)
  if (mode == "solver") {
    if (identical(as.integer(target_season), 2024L)) {
      f <- file.path(template_dir, "xgb_billy_trees_solver_2024_template.R")
    } else {
      f <- file.path(template_dir, "xgb_billy_trees_solver_generic_template.R")
    }
  } else {
    candidate <- file.path(template_dir, paste0("xgb_billy_trees_fast_", target_season, "_template.R"))
    if (file.exists(candidate)) {
      f <- candidate
    } else {
      f <- file.path(template_dir, "xgb_billy_trees_fast_2026_template.R")
    }
  }
  if (!file.exists(f)) stop("Missing Step 11 template: ", f, call. = FALSE)
  f
}

save_xgb_billy_tree_params_from_env <- function(env, target_season, output_dir) {
  files <- xgb_billy_tree_param_files(target_season, output_dir)
  object_map <- c(
    diff       = "best_diff",
    total      = "best_total",
    score      = "best",
    score_home = "best_home",
    score_away = "best_away"
  )
  for (nm in names(object_map)) {
    obj_name <- object_map[[nm]]
    if (exists(obj_name, envir = env, inherits = FALSE)) {
      obj <- get(obj_name, envir = env)
      if (is.list(obj) && all(c("params", "nrounds") %in% names(obj))) {
        saveRDS(list(params = obj$params, nrounds = obj$nrounds), files[[nm]])
        message("Step 11 Billy XGBoost: saved ", nm, " params for ", target_season, " to ", files[[nm]])
      }
    }
  }
  invisible(files)
}

run_xgb_billy_tree_template <- function(data_v12,
                                        target_season,
                                        output_dir,
                                        template_file,
                                        template_mode,
                                        holdout_test_season,
                                        training_min_week,
                                        exclude_training_seasons = integer(0)) {
  code <- readLines(template_file, warn = FALSE)
  code <- paste(code, collapse = "\n")
  code <- xgb_billy_tree_prepare_template_code(
    code = code,
    target_season = target_season,
    output_dir = output_dir,
    holdout_test_season = holdout_test_season,
    training_min_week = training_min_week,
    exclude_training_seasons = exclude_training_seasons
  )

  env <- new.env(parent = globalenv())
  env$data_v12 <- data_v12
  env$target_season <- as.integer(target_season)
  env$output_dir <- output_dir
  env$template_mode <- template_mode
  env$holdout_test_season <- as.integer(holdout_test_season)
  env$training_min_week <- as.integer(training_min_week)
  env$exclude_training_seasons <- as.integer(exclude_training_seasons)
  env$include_completed_target_season_training <- FALSE

  # Make helper functions available inside the generated legacy-template environment.
  helper_names <- c(
    "xgb_billy_tree_param_file",
    "xgb_billy_tree_param_files",
    "xgb_billy_tree_bind_rows_fill",
    "xgb_billy_tree_match_col",
    "xgb_billy_tree_numeric_cols",
    "xgb_billy_tree_bad_score_col",
    "xgb_billy_tree_find_score_cols",
    "xgb_billy_tree_find_generic_score_col",
    "xgb_billy_tree_find_total_col",
    "xgb_billy_tree_pair_opp_score_from_rows",
    "xgb_billy_tree_complete_legacy_score_columns",
    "xgb_billy_tree_add_score_avg_if_needed",
    "xgb_billy_tree_merge_step10_to_base",
    "xgb_billy_tree_read_step10_csv_bridge",
    "xgb_billy_tree_rdata_candidate_score",
    "xgb_billy_tree_get_step10_data"
  )
  for (nm in helper_names) {
    env[[nm]] <- get(nm, mode = "function")
  }

  eval(parse(text = code), envir = env)
  if (!exists("train", envir = env, inherits = FALSE) || !is.data.frame(env$train)) {
    stop("Step 11 template did not create a training data frame.", call. = FALSE)
  }
  realized_train_seasons <- sort(unique(as.integer(env$train$season)))
  realized_train_seasons <- realized_train_seasons[is.finite(realized_train_seasons)]
  invalid_training_season <- realized_train_seasons > target_season |
    (!env$include_completed_target_season_training & realized_train_seasons >= target_season)
  if (!length(realized_train_seasons) || any(invalid_training_season)) {
    stop(
      "Step 11 realized training seasons are invalid for target ", target_season,
      ": ", paste(realized_train_seasons, collapse = ", "),
      call. = FALSE
    )
  }
  prior_season <- target_season - 1L
  prior_available <- any(
    as.integer(data_v12$season) == prior_season &
      as.integer(data_v12$week) >= training_min_week,
    na.rm = TRUE
  )
  if (prior_available && !prior_season %in% realized_train_seasons && !prior_season %in% exclude_training_seasons) {
    stop(
      "Step 11 target ", target_season, " failed to include available completed ",
      prior_season, " games in training.",
      call. = FALSE
    )
  }
  message(
    "Step 11 XGBoost: realized training seasons for target ", target_season,
    " = ", paste(realized_train_seasons, collapse = ", "),
    "; rows = ", nrow(env$train), "."
  )
  env
}

run_xgb_billy_total_score_tree_regression <- function(data_v12,
                                                      target_season,
                                                      output_dir,
                                                      template_dir = file.path("R", "step11_templates"),
                                                      solve_params = TRUE,
                                                      run_fast = TRUE,
                                                      force_resolve_params = FALSE,
                                                      holdout_test_season = 2024L,
                                                      training_min_week = 1L,
                                                      exclude_training_seasons = integer(0)) {
  target_season <- as.integer(target_season)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  param_files <- xgb_billy_tree_param_files(target_season, output_dir)
  params_exist <- all(file.exists(param_files))

  solver_env <- NULL
  if (isTRUE(solve_params) && (!params_exist || isTRUE(force_resolve_params))) {
    message(
      "Step 11 Billy XGBoost: solving hyperparameters for target season ", target_season,
      " using completed seasons through ", target_season - 1L, "."
    )
    solver_template <- xgb_billy_tree_template_file(target_season, mode = "solver", template_dir = template_dir)
    solver_env <- run_xgb_billy_tree_template(
      data_v12 = data_v12,
      target_season = target_season,
      output_dir = output_dir,
      template_file = solver_template,
      template_mode = "solver",
      holdout_test_season = holdout_test_season,
      training_min_week = training_min_week,
      exclude_training_seasons = exclude_training_seasons
    )
    save_xgb_billy_tree_params_from_env(solver_env, target_season, output_dir)
    params_exist <- all(file.exists(param_files))
  } else if (params_exist) {
    message("Step 11 Billy XGBoost: reusing existing hyperparameters for target season ", target_season, ".")
  }

  if (!all(file.exists(param_files))) {
    stop(
      "Step 11 Billy XGBoost parameter files are missing for target season ", target_season, ": ",
      paste(param_files[!file.exists(param_files)], collapse = ", "),
      call. = FALSE
    )
  }

  fast_env <- NULL
  if (isTRUE(run_fast)) {
    message("Step 11 Billy XGBoost: running fast prediction for target season ", target_season, ".")
    fast_template <- xgb_billy_tree_template_file(target_season, mode = "fast", template_dir = template_dir)
    fast_env <- run_xgb_billy_tree_template(
      data_v12 = data_v12,
      target_season = target_season,
      output_dir = output_dir,
      template_file = fast_template,
      template_mode = "fast",
      holdout_test_season = holdout_test_season,
      training_min_week = training_min_week,
      exclude_training_seasons = exclude_training_seasons
    )
  }

  home_file <- file.path(output_dir, paste0(target_season, "_BillyTrees_home.csv"))
  away_file <- file.path(output_dir, paste0(target_season, "_BillyTrees_away.csv"))

  if (isTRUE(run_fast)) {
    if (!file.exists(home_file) || !file.exists(away_file)) {
      stop(
        "Step 11 Billy XGBoost did not write expected output files for ", target_season,
        ": ", home_file, " / ", away_file,
        call. = FALSE
      )
    }
    message("Step 11 Billy XGBoost total/score trees ", target_season, ": wrote ", home_file, " and ", away_file, ".")
  }

  invisible(list(
    target_season = target_season,
    param_files = param_files,
    home_file = home_file,
    away_file = away_file,
    solver_env = solver_env,
    fast_env = fast_env
  ))
}
