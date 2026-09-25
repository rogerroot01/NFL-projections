root <- dirname(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
bundle <- readRDS(file.path(getwd(), "output/ensemble_model_wrangler/data/compact_models.rds"))
current <- bundle[["2026_ScoresTrees_home.csv"]]
keys <- c("week", "home_team", "away_team")
fields <- c(keys, "spread_line", "total_line")

for (week in 1:2) {
  original_path <- file.path(
    root, "Legacy_models", paste0("nflFast_Model_2026_Scores_week", week),
    "output", "2026_ScoresTrees_home.csv"
  )
  original <- read.csv(original_path, check.names = FALSE)
  original <- original[original$week == week, fields]
  graded <- current[current$week == week, fields]
  joined <- merge(original, graded, by = keys)
  cat("Week", week, "matched", nrow(joined),
      "spread changed", sum(joined$spread_line.x != joined$spread_line.y, na.rm = TRUE),
      "total changed", sum(joined$total_line.x != joined$total_line.y, na.rm = TRUE),
      "max spread shift", max(abs(joined$spread_line.x - joined$spread_line.y), na.rm = TRUE),
      "max total shift", max(abs(joined$total_line.x - joined$total_line.y), na.rm = TRUE), "\n")
}
