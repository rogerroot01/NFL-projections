source("pregame_projection_archive.R")

export_dir <- file.path("..", "output", "ensemble_model_wrangler")
models <- readRDS(file.path(export_dir, "compact_models.rds"))
archive <- readRDS(file.path(export_dir, "legacy_pregame_2026_archive.rds"))
checked <- 0L

for (file in names(models)[grepl("^2026_", names(models))]) {
  current <- models[[file]]
  past <- !is.na(current$game_date) & as.Date(current$game_date) < Sys.Date()
  if (!any(past)) next
  original <- archive[[file]]
  if (is.null(original)) stop("No pregame archive for ", file, call. = FALSE)
  indices <- match(pregame_row_key(current[past, , drop = FALSE]), pregame_row_key(original))
  if (anyNA(indices)) stop("Missing archived past game in ", file, call. = FALSE)
  for (column in setdiff(names(original), pregame_key_columns)) {
    if (!column %in% names(current) ||
        !isTRUE(all.equal(as.character(current[[column]][past]),
                          as.character(original[[column]][indices]),
                          check.attributes = FALSE))) {
      stop("Changed past prediction in ", file, " / ", column, call. = FALSE)
    }
  }
  checked <- checked + sum(past)
}

cat("Standalone Legacy pregame archive OK:", checked, "past model-game rows\n")
