if (!requireNamespace("rsconnect", quietly = TRUE)) stop("rsconnect is required")
if (!file.exists("app.R") || !dir.exists("data")) {
  stop("Run from the NFL-projections app repository root")
}

# Bundle only active app inputs; historical .bak files must never become live data.
app_files <- c(
  "app.R",
  list.files("data", pattern = "\\.(csv|rds)$", full.names = TRUE, recursive = TRUE),
  list.files("www", full.names = TRUE, recursive = TRUE)
)
app_files <- app_files[file.exists(app_files)]
rsconnect::deployApp(
  appDir = getwd(),
  appName = "nfl-projection-ensemble-wrangler",
  account = "roger-root-nfl",
  server = "connect.posit.cloud",
  appFiles = app_files,
  forceUpdate = TRUE,
  launch.browser = FALSE
)
