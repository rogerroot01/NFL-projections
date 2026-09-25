source("pregame_projection_archive.R")
active <- readRDS("output/ensemble_model_wrangler/data/compact_models.rds")
standalone <- readRDS("../Legacy_models/nflFast_Model_2026_Scores_week3/output/ensemble_model_wrangler/compact_models.rds")

predictors <- function(columns) {
  columns[grepl("^(ScoreDiff|ScoreTotal|TotalScore|Implied|Score_(xgb|forward|stepwise|avg|final)|OppScore|Billy$)",
               columns, ignore.case = TRUE) &
            !grepl("^Cover_|_target|_cover$", columns)]
}

for (file in intersect(names(active), names(standalone))) {
  if (!grepl("^2026_", file)) next
  first <- active[[file]]
  second <- standalone[[file]]
  first$.key <- pregame_row_key(first)
  second$.key <- pregame_row_key(second)
  pred_cols <- intersect(predictors(names(first)), predictors(names(second)))
  shared <- merge(first[, c(".key", "week", pred_cols), drop = FALSE],
                  second[, c(".key", pred_cols), drop = FALSE], by = ".key", suffixes = c(".active", ".standalone"))
  shared <- shared[shared$week >= 3L, , drop = FALSE]
  delta <- unlist(lapply(pred_cols, function(column) {
    a <- suppressWarnings(as.numeric(shared[[paste0(column, ".active")]]))
    b <- suppressWarnings(as.numeric(shared[[paste0(column, ".standalone")]]))
    abs(a[is.finite(a) & is.finite(b)] - b[is.finite(a) & is.finite(b)])
  }), use.names = FALSE)
  cat(file, "rows", nrow(shared), "columns", length(pred_cols),
      "values", length(delta), "exact", sum(delta < 1e-8),
      "MAE", if (length(delta)) mean(delta) else NA_real_,
      "max", if (length(delta)) max(delta) else NA_real_, "\n")
}
