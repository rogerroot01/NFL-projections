# Preserve the last genuinely pregame projection for each 2026 game/model.
# Actual scores and market lines remain in the current run; only prediction
# columns are restored after a game is complete.

pregame_key_columns <- c("season", "week", "home_team", "away_team", "posteam_type")

pregame_row_key <- function(df) {
  missing <- setdiff(pregame_key_columns, names(df))
  if (length(missing)) stop("Missing pregame identity fields: ", paste(missing, collapse = ", "), call. = FALSE)
  paste(
    as.integer(df$season), as.integer(df$week),
    toupper(trimws(as.character(df$home_team))),
    toupper(trimws(as.character(df$away_team))),
    tolower(trimws(as.character(df$posteam_type))), sep = "|"
  )
}

pregame_projection_columns <- function(df, prediction_columns) {
  setdiff(prediction_columns(names(df)), c(
    "home_implied", "away_implied", "posteam_implied", "defteam_implied",
    "line", "actual_market_result", "predicted_market_result"
  ))
}

pregame_validate_unique <- function(df, label) {
  keys <- pregame_row_key(df)
  if (anyNA(keys) || anyDuplicated(keys)) {
    stop("Missing or duplicate game identity in ", label, call. = FALSE)
  }
}

pregame_capture <- function(archive, models, week, prediction_columns, label) {
  for (file in names(models)) {
    if (!grepl("^2026_", file)) next
    df <- models[[file]]
    rows <- !is.na(df$season) & df$season == 2026L & !is.na(df$week) & df$week == week
    if (!any(rows)) stop("No Week ", week, " rows in ", label, " / ", file, call. = FALSE)
    df <- df[rows, , drop = FALSE]
    if (any(!is.na(df$home_score) | !is.na(df$away_score))) {
      stop("Refusing postgame snapshot in ", label, " / ", file, call. = FALSE)
    }
    pregame_validate_unique(df, paste(label, file))
    cols <- pregame_projection_columns(df, prediction_columns)
    if (!length(cols)) stop("No projection columns in ", label, " / ", file, call. = FALSE)
    captured <- df[, unique(c(pregame_key_columns, cols)), drop = FALSE]
    old <- archive[[file]]
    if (is.null(old)) {
      archive[[file]] <- captured
    } else {
      old_keys <- pregame_row_key(old)
      new_keys <- pregame_row_key(captured)
      if (any(new_keys %in% old_keys)) stop("Pregame snapshot would overwrite archived game: ", file, call. = FALSE)
      all_cols <- union(names(old), names(captured))
      for (col in setdiff(all_cols, names(old))) old[[col]] <- NA_real_
      for (col in setdiff(all_cols, names(captured))) captured[[col]] <- NA_real_
      archive[[file]] <- rbind(old[, all_cols, drop = FALSE], captured[, all_cols, drop = FALSE])
    }
  }
  archive
}

pregame_update_future <- function(archive, models, prediction_columns, today = Sys.Date()) {
  for (file in names(models)) {
    if (!grepl("^2026_", file)) next
    df <- models[[file]]
    date <- as.Date(df$game_date)
    rows <- !is.na(df$season) & df$season == 2026L &
      !is.na(date) & date > today &
      is.na(df$home_score) & is.na(df$away_score)
    if (!any(rows)) next
    current <- df[rows, , drop = FALSE]
    pregame_validate_unique(current, paste("future", file))
    cols <- pregame_projection_columns(current, prediction_columns)
    current <- current[, unique(c(pregame_key_columns, cols)), drop = FALSE]
    old <- archive[[file]]
    if (is.null(old)) {
      archive[[file]] <- current
    } else {
      all_cols <- union(names(old), names(current))
      for (col in setdiff(all_cols, names(old))) old[[col]] <- NA_real_
      for (col in setdiff(all_cols, names(current))) current[[col]] <- NA_real_
      old <- old[!pregame_row_key(old) %in% pregame_row_key(current), all_cols, drop = FALSE]
      archive[[file]] <- rbind(old, current[, all_cols, drop = FALSE])
    }
  }
  archive
}

pregame_restore_completed <- function(models, archive, prediction_columns) {
  for (file in names(models)) {
    if (!grepl("^2026_", file)) next
    df <- models[[file]]
    game_date <- as.Date(df$game_date)
    completed <- !is.na(df$season) & df$season == 2026L &
      ((!is.na(game_date) & game_date < Sys.Date()) |
         (!is.na(df$home_score) & !is.na(df$away_score)))
    if (!any(completed)) next
    old <- archive[[file]]
    if (is.null(old)) stop("No pregame archive for completed model: ", file, call. = FALSE)
    pregame_validate_unique(old, paste("archive", file))
    idx <- match(pregame_row_key(df[completed, , drop = FALSE]), pregame_row_key(old))
    if (anyNA(idx)) stop("Completed game missing from pregame archive: ", file, call. = FALSE)
    archived_predictions <- setdiff(names(old), pregame_key_columns)
    for (col in setdiff(archived_predictions, names(df))) df[[col]] <- NA_real_
    for (col in union(pregame_projection_columns(df, prediction_columns), archived_predictions)) {
      if (col %in% names(old)) {
        df[[col]][completed] <- old[[col]][idx]
      } else {
        # Never expose a new postgame-derived prediction as historical.
        df[[col]][completed] <- NA_real_
      }
    }
    models[[file]] <- df
  }
  models
}
