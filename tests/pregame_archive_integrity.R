key <- function(df) paste(
  as.integer(df$season), as.integer(df$week),
  toupper(trimws(as.character(df$home_team))),
  toupper(trimws(as.character(df$away_team))),
  tolower(trimws(as.character(df$posteam_type))), sep = "|"
)

check_bundle <- function(models_file, archive_file, label) {
  models <- readRDS(file.path("data", models_file))
  archive <- readRDS(file.path("data", archive_file))
  checked <- 0L
  for (file in names(models)[grepl("^2026_", names(models))]) {
    df <- models[[file]]
    old <- archive[[file]]
    if (is.null(old)) stop(label, " archive missing model: ", file)
    past <- !is.na(df$game_date) & as.Date(df$game_date) < Sys.Date()
    if (!any(past)) next
    idx <- match(key(df[past, , drop = FALSE]), key(old))
    if (anyNA(idx)) stop(label, " archive missing past game: ", file)
    for (col in setdiff(names(old), c("season", "week", "home_team", "away_team", "posteam_type"))) {
      if (!col %in% names(df) ||
          !isTRUE(all.equal(as.character(df[[col]][past]), as.character(old[[col]][idx]),
                            check.attributes = FALSE))) {
        stop(label, " changed past prediction: ", file, " / ", col)
      }
    }
    checked <- checked + sum(past)
  }
  cat(label, "PRE_GAME_ARCHIVE_OK", checked, "past model-game rows\n")
}

check_bundle("compact_models.rds", "legacy_pregame_2026_archive.rds", "LEGACY")
check_bundle("nextgen_compact_models.rds", "nextgen_pregame_2026_archive.rds", "NEXTGEN")
