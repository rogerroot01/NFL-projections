options(stringsAsFactors = FALSE)

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Package 'readxl' is required to refresh the 2026 forecast adjustments.", call. = FALSE)
}
if (!requireNamespace("readr", quietly = TRUE)) {
  stop("Package 'readr' is required to refresh the 2026 forecast adjustments.", call. = FALSE)
}

command_args <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", command_args, value = TRUE)
app_dir <- if (length(script_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", script_arg[[1]]), winslash = "/", mustWork = TRUE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

user_args <- commandArgs(trailingOnly = TRUE)
source_xlsx <- if (length(user_args) > 0 && nzchar(user_args[[1]])) {
  user_args[[1]]
} else {
  file.path(app_dir, "..", "..", "Forecast Team Off Def Injuries.xlsx")
}
source_xlsx <- normalizePath(source_xlsx, winslash = "/", mustWork = TRUE)

required_columns <- c(
  "game_id", "season", "week", "home_team", "away_team",
  "home_injury_adj", "away_injury_adj",
  "home_off_injury_adj", "home_def_injury_adj",
  "away_off_injury_adj", "away_def_injury_adj",
  "home_adjust_amortization", "away_adjust_amortization"
)

adjustments <- readxl::read_excel(
  source_xlsx,
  sheet = "Forecast Team Off Def Injuries",
  .name_repair = "minimal"
)

missing_columns <- setdiff(required_columns, names(adjustments))
if (length(missing_columns) > 0) {
  stop("Forecast adjustment workbook is missing columns: ", paste(missing_columns, collapse = ", "), call. = FALSE)
}

adjustments <- adjustments[required_columns]
numeric_columns <- setdiff(required_columns, c("game_id", "home_team", "away_team"))
adjustments[numeric_columns] <- lapply(adjustments[numeric_columns], function(x) suppressWarnings(as.numeric(x)))
adjustments$game_id <- as.character(adjustments$game_id)
adjustments$home_team <- toupper(trimws(as.character(adjustments$home_team)))
adjustments$away_team <- toupper(trimws(as.character(adjustments$away_team)))

if (nrow(adjustments) == 0 || any(is.na(adjustments$game_id)) || anyDuplicated(adjustments$game_id)) {
  stop("Forecast adjustment workbook must contain unique, nonblank game_id values.", call. = FALSE)
}
if (any(adjustments$season != 2026, na.rm = TRUE)) {
  stop("Forecast adjustment workbook contains a season other than 2026.", call. = FALSE)
}

zero_if_na <- function(x) replace(x, is.na(x), 0)
expected_home <- zero_if_na(adjustments$home_off_injury_adj) +
  zero_if_na(adjustments$home_def_injury_adj) +
  zero_if_na(adjustments$home_adjust_amortization)
expected_away <- zero_if_na(adjustments$away_off_injury_adj) +
  zero_if_na(adjustments$away_def_injury_adj) +
  zero_if_na(adjustments$away_adjust_amortization)

home_mismatch <- !is.na(adjustments$home_injury_adj) & abs(adjustments$home_injury_adj - expected_home) > 1e-6
away_mismatch <- !is.na(adjustments$away_injury_adj) & abs(adjustments$away_injury_adj - expected_away) > 1e-6
if (any(home_mismatch) || any(away_mismatch)) {
  stop("Columns F/G do not reconcile to the injury components plus amortization columns.", call. = FALSE)
}

output_csv <- file.path(app_dir, "data", "forecast_team_off_def_injuries_2026.csv")
readr::write_csv(adjustments, output_csv, na = "")

cat("Refreshed ", nrow(adjustments), " 2026 game adjustments from:\n", source_xlsx,
    "\nWrote deployable app source:\n", normalizePath(output_csv, winslash = "/", mustWork = TRUE), "\n", sep = "")
