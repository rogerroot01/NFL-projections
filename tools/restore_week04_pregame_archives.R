source("training_sources/2026_week3/legacy/pregame_projection_archive.R")

legacy_prediction_columns <- function(cols) {
  cols[grepl("^(ScoreDiff|ScoreTotal|TotalScore|Implied|Score_(xgb|forward|stepwise|avg|final)|OppScore|Billy$)",
             cols, ignore.case = TRUE) &
         !grepl("^Cover_|_target|_cover$", cols)]
}
nextgen_prediction_columns <- function(cols) {
  cols[grepl("^(Score_|HomeScore_|AwayScore_|OppScore_|ScoreTotal_|TotalScore_|ScoreDiff_|HomeMargin_|PredictedScore_)",
             cols, ignore.case = TRUE) &
         !grepl("^Cover_|_target|_cover$|_pm1$", cols, ignore.case = TRUE)]
}
for (spec in list(
  list("compact_models.rds", "legacy_pregame_2026_archive.rds", legacy_prediction_columns),
  list("nextgen_compact_models.rds", "nextgen_pregame_2026_archive.rds", nextgen_prediction_columns)
)) {
  model_path <- file.path("data", spec[[1L]])
  archive_path <- file.path("data", spec[[2L]])
  models <- readRDS(model_path)
  archive <- readRDS(archive_path)
  restored <- pregame_restore_completed(models, archive, spec[[3L]])
  saveRDS(restored, model_path)
  cat(spec[[1L]], " restored ", length(restored), " model files\n", sep = "")
}
