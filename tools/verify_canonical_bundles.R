football_root <- Sys.getenv(
  "FOOTBALL_2026_ROOT",
  "C:/Users/rroot/OneDrive/Documents/Football_2026"
)
legacy_export <- file.path(
  football_root, "Legacy_models", "nflFast_Model_2026_Scores_week3",
  "output", "ensemble_model_wrangler"
)
nextgen_export <- file.path(
  football_root, "Football_projection_pipeline_exclusion",
  "output", "ensemble_model_wrangler_nextgen", "data"
)

sources <- c(
  "compact_models.rds" = file.path(legacy_export, "compact_models.rds"),
  "legacy_pregame_2026_archive.rds" = file.path(legacy_export, "legacy_pregame_2026_archive.rds"),
  "model_inventory.rds" = file.path(legacy_export, "model_inventory.rds"),
  "prediction_column_inventory.csv" = file.path(legacy_export, "prediction_column_inventory.csv"),
  "nextgen_compact_models.rds" = file.path(nextgen_export, "nextgen_compact_models.rds"),
  "nextgen_pregame_2026_archive.rds" = file.path(nextgen_export, "nextgen_pregame_2026_archive.rds"),
  "nextgen_model_inventory.rds" = file.path(nextgen_export, "nextgen_model_inventory.rds"),
  "nextgen_prediction_column_inventory.csv" = file.path(nextgen_export, "nextgen_prediction_column_inventory.csv")
)

for (name in names(sources)) {
  published <- file.path("data", name)
  source <- sources[[name]]
  if (!file.exists(source) || !file.exists(published) ||
      !identical(unname(tools::md5sum(source)), unname(tools::md5sum(published)))) {
    stop("Gmail Wrangler bundle differs from its canonical source: ", name, call. = FALSE)
  }
  cat("CANONICAL_SOURCE_OK", name, "\n")
}

source(file.path("tests", "pregame_archive_integrity.R"), local = TRUE)
