# Step 2 - run_nextgen_models.R
# Render every 2025 next-gen model Rmd for early and late frameworks, then
# rebuild the prepared NextGen RDS files consumed by the Projection Wrangler app.
#
# This script resolves its own folder, so a copied weekly pipeline can be
# sourced from RStudio without retaining a stale path to the prior week.
#
# Usage:
#   source("C:/path/to/current_week/Step 2 - run_nextgen_models.R")
#
# Optional:
#   Sys.setenv(NEXTGEN_CONTINUE_ON_ERROR = "TRUE")
#   source("C:/path/to/current_week/Step 2 - run_nextgen_models.R")

resolve_pipeline_script <- function() {
  file_args <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_args) > 0L) {
    return(sub("^--file=", "", file_args[[1L]]))
  }
  frame_files <- vapply(sys.frames(), function(frame) {
    value <- frame$ofile
    if (is.null(value) || length(value) != 1L) NA_character_ else as.character(value)
  }, character(1))
  frame_files <- frame_files[!is.na(frame_files) & nzchar(frame_files)]
  if (length(frame_files) > 0L) tail(frame_files, 1L) else NA_character_
}

pipeline_script <- resolve_pipeline_script()
pipeline_dir <- normalizePath(
  if (is.na(pipeline_script)) getwd() else dirname(pipeline_script),
  winslash = "/",
  mustWork = TRUE
)
rm(resolve_pipeline_script, pipeline_script)

setwd(pipeline_dir)
message("Step 2 pipeline folder: ", pipeline_dir)

# Reuse specifications resolved against the 2025 holdout and fit through 2025.
# Rolling 2026 features update weekly, but 2026 outcomes never train a model
# that also re-predicts completed 2026 games for the consensus summary.
Sys.setenv(NEXTGEN_PRODUCTION_REFIT = "TRUE")
message("NextGen production fit: train through 2025; predict 2026 without outcome leakage.")

# Step 1 leaves MASS attached after dplyr. In the same R session, MASS::select
# masks dplyr::select and makes the first NextGen Rmd fail during load-and-split.
# NextGen does not need MASS attached, so remove it from the search path while
# leaving the namespace loaded for any explicitly qualified MASS calls.
if ("package:MASS" %in% search()) {
  detach("package:MASS", unload = FALSE, character.only = TRUE)
  message("Detached package:MASS so NextGen uses dplyr::select.")
}

if (!file.exists(file.path(pipeline_dir, "prepare_data_nextgen.R"))) {
  stop(
    "Could not locate prepare_data_nextgen.R from working directory: ",
    pipeline_dir,
    "\nRun this script from the Football_projection_pipeline_exclusion folder.",
    call. = FALSE
  )
}

required_packages <- c("rmarkdown", "knitr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Missing required package(s): ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

# RStudio normally supplies RSTUDIO_PANDOC interactively. Scheduled/Rscript
# runs do not, so discover the bundled executable when the environment variable
# is absent. This removes another machine-session-specific weekly setup step.
if (!rmarkdown::pandoc_available()) {
  pandoc_candidates <- c(
    "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
    "C:/Program Files/RStudio/bin/pandoc",
    "C:/Program Files/Quarto/bin/tools",
    "C:/Program Files/Quarto/bin"
  )
  pandoc_dir <- pandoc_candidates[file.exists(file.path(pandoc_candidates, "pandoc.exe"))][1]
  if (!is.na(pandoc_dir)) {
    Sys.setenv(RSTUDIO_PANDOC = pandoc_dir)
    message("Using bundled Pandoc: ", file.path(pandoc_dir, "pandoc.exe"))
  }
}
if (!rmarkdown::pandoc_available()) {
  stop("Pandoc is required for NextGen R Markdown models and was not found.", call. = FALSE)
}

continue_on_error <- identical(toupper(Sys.getenv("NEXTGEN_CONTINUE_ON_ERROR", "FALSE")), "TRUE")

framework_dirs <- c(
  early = file.path(pipeline_dir, "New_Models_Early_2025"),
  late = file.path(pipeline_dir, "New_Models_Late_2025")
)

for (nm in names(framework_dirs)) {
  if (!dir.exists(framework_dirs[[nm]])) {
    stop("Missing next-gen model folder for ", nm, ": ", framework_dirs[[nm]], call. = FALSE)
  }
}

logs_dir <- file.path(pipeline_dir, "output", "nextgen_run_logs")
if (!dir.exists(logs_dir)) {
  dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)
}

run_id <- format(Sys.time(), "%Y%m%d_%H%M%S")
summary_log <- file.path(logs_dir, paste0("nextgen_model_run_", run_id, ".csv"))

message_line <- function(...) {
  msg <- paste0(...)
  message("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", msg)
}

discover_rmds <- function(model_dir) {
  rmds <- list.files(model_dir, pattern = "\\.Rmd$", full.names = TRUE, recursive = FALSE)
  rmds <- sort(rmds)
  if (length(rmds) == 0) {
    stop("No Rmd files found in ", model_dir, call. = FALSE)
  }
  rmds
}

render_model_rmd <- function(rmd_path, framework) {
  model_dir <- dirname(rmd_path)
  model_name <- basename(rmd_path)
  report_dir <- file.path(model_dir, "output", "rendered_reports")
  if (!dir.exists(report_dir)) {
    dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  }
  log_file <- file.path(
    logs_dir,
    paste0(run_id, "_", framework, "_", tools::file_path_sans_ext(model_name), ".log")
  )

  message_line("Rendering ", framework, " model: ", model_name)
  start_time <- Sys.time()

  result <- tryCatch({
    output_file <- rmarkdown::render(
      input = rmd_path,
      output_format = "html_document",
      output_dir = report_dir,
      intermediates_dir = tempdir(),
      knit_root_dir = model_dir,
      envir = new.env(parent = globalenv()),
      quiet = FALSE
    )
    list(ok = TRUE, output_file = output_file, error = NA_character_)
  }, error = function(e) {
    list(ok = FALSE, output_file = NA_character_, error = conditionMessage(e))
  })

  elapsed <- round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 2)

  status_line <- paste0(
    "framework=", framework,
    "\nmodel=", model_name,
    "\nok=", result$ok,
    "\nelapsed_minutes=", elapsed,
    "\noutput_file=", result$output_file,
    "\nerror=", result$error,
    "\n"
  )
  writeLines(status_line, log_file)

  if (!isTRUE(result$ok)) {
    message_line("FAILED ", framework, " model: ", model_name, " :: ", result$error)
    if (!continue_on_error) {
      stop("Next-gen model render failed: ", model_name, "\nLog: ", log_file, call. = FALSE)
    }
  } else {
    message_line("Finished ", framework, " model: ", model_name, " in ", elapsed, " minutes")
  }

  data.frame(
    run_id = run_id,
    framework = framework,
    model_file = model_name,
    model_path = normalizePath(rmd_path, winslash = "/", mustWork = FALSE),
    ok = isTRUE(result$ok),
    elapsed_minutes = elapsed,
    output_file = result$output_file,
    log_file = normalizePath(log_file, winslash = "/", mustWork = FALSE),
    error = result$error,
    stringsAsFactors = FALSE
  )
}

validate_legacy_wide_outputs <- function() {
  expected_families <- c(
    "elastic_net_lasso",
    "weighted_linear_regression",
    "decision_tree_rpart",
    "random_forest_ranger",
    "gbm_boosted_trees",
    "xgboost_regression"
  )
  expected <- expand.grid(
    framework = names(framework_dirs),
    season = c("2024", "2025", "2026"),
    family = expected_families,
    stringsAsFactors = FALSE
  )
  expected$sample <- ifelse(expected$season == "2026", "val", "test")
  expected$file <- paste0(
    expected$season, "_", expected$family, "_", expected$framework,
    "_FINAL_MODEL_legacy_wide_", expected$sample, ".csv"
  )
  expected$path <- file.path(
    framework_dirs[expected$framework],
    "output",
    "v12_model_search",
    expected$file
  )
  expected$exists <- file.exists(expected$path)

  missing <- expected[!expected$exists, , drop = FALSE]
  if (nrow(missing) > 0) {
    warning(
      "Missing expected FINAL_MODEL legacy-wide files:\n",
      paste(missing$path, collapse = "\n"),
      call. = FALSE
    )
  }
  expected
}

message_line("Starting next-gen model batch run.")
message_line("Pipeline folder: ", pipeline_dir)
message_line("Continue on error: ", continue_on_error)

run_results <- do.call(
  rbind,
  lapply(names(framework_dirs), function(framework) {
    rmds <- discover_rmds(framework_dirs[[framework]])
    do.call(rbind, lapply(rmds, render_model_rmd, framework = framework))
  })
)

utils::write.csv(run_results, summary_log, row.names = FALSE)
message_line("Wrote run summary: ", summary_log)

legacy_wide_inventory <- validate_legacy_wide_outputs()
legacy_inventory_log <- file.path(logs_dir, paste0("nextgen_legacy_wide_inventory_", run_id, ".csv"))
utils::write.csv(legacy_wide_inventory, legacy_inventory_log, row.names = FALSE)
message_line("Wrote legacy-wide inventory: ", legacy_inventory_log)

if (any(!run_results$ok) && !continue_on_error) {
  stop("One or more next-gen model renders failed. Prepared data was not rebuilt.", call. = FALSE)
}

message_line("Running prepare_data_nextgen.R")
prepare_start <- Sys.time()
prepare_result <- tryCatch({
  source(file.path(pipeline_dir, "prepare_data_nextgen.R"), local = new.env(parent = globalenv()))
  list(ok = TRUE, error = NA_character_)
}, error = function(e) {
  list(ok = FALSE, error = conditionMessage(e))
})
prepare_elapsed <- round(as.numeric(difftime(Sys.time(), prepare_start, units = "mins")), 2)

prepare_log <- file.path(logs_dir, paste0("prepare_data_nextgen_", run_id, ".log"))
writeLines(
  paste0(
    "run_id=", run_id,
    "\nok=", prepare_result$ok,
    "\nelapsed_minutes=", prepare_elapsed,
    "\nerror=", prepare_result$error,
    "\n"
  ),
  prepare_log
)

if (!isTRUE(prepare_result$ok)) {
  stop("prepare_data_nextgen.R failed: ", prepare_result$error, "\nLog: ", prepare_log, call. = FALSE)
}

message_line("prepare_data_nextgen.R completed in ", prepare_elapsed, " minutes.")
message_line("Next-gen model batch run complete.")
