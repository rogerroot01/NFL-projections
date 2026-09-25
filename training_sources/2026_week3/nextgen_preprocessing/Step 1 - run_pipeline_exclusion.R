      # run_pipeline.R
      # Cleaned runner for Steps 1 through 7 pre-regression datasets.
      #
      # Usage from the project root:
      #   source("run_pipeline.R")
      #
      # Optional override when a new season is actually available:
      #   Sys.setenv(NFL_END_SEASON = "2026")
      #   source("run_pipeline.R")

      # Always run from the folder containing this script. This keeps a copied
      # weekly pipeline self-contained even when RStudio's working directory is
      # still pointed at the prior week's folder.
      .resolve_pipeline_script <- function() {
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
      .pipeline_script <- .resolve_pipeline_script()
      if (!is.na(.pipeline_script)) {
        setwd(dirname(normalizePath(.pipeline_script, winslash = "/", mustWork = TRUE)))
      }
      rm(.resolve_pipeline_script, .pipeline_script)
      
      source(file.path("R", "00_config.R"))
      source(file.path("R", "solver_game_exclusions_helpers.R"))
      source(file.path("R", "01_build_game_level_data.R"))
      source(file.path("R", "02_add_rolling_team_averages.R"))
      source(file.path("R", "03_normalize_and_add_features.R"))
      source(file.path("R", "04_add_normalized_team_averages.R"))
      source(file.path("R", "05_append_future_games.R"))
      source(file.path("R", "06_build_interactions.R"))
      source(file.path("R", "07_build_matchup_differences_sums.R"))
      source(file.path("R", "08_final_preprocessing_v11.R"))
      source(file.path("R", "09_final_preprocessing_v12.R"))
      source(file.path("R", "10_late_season_total_score_regression.R"))
      source(file.path("R", "11_billy_walters_score_diff_regression.R"))
      source(file.path("R", "12_billy_walters_team_score_regression.R"))
      source(file.path("R", "13_xgboost_total_score_trees.R"))
      source(file.path("R", "14_xgboost_billy_total_score_trees.R"))
      source(file.path("R", "15_prepare_ensemble_model_wrangler_data.R"))
      
      if (!dir.exists(OUTPUT_DIR)) {
        dir.create(OUTPUT_DIR, recursive = TRUE)
      }
  
  # One-time exclusion-path diagnostic. This catches stale R session overrides where
  # the pipeline is not actually using data/solver_game_exclusions.csv.
  message("Solver game exclusions configured path: ", SOLVER_GAME_EXCLUSIONS_PATH)
  message(
    "Solver game exclusions absolute path: ",
    normalizePath(SOLVER_GAME_EXCLUSIONS_PATH, winslash = "/", mustWork = FALSE)
  )
  if (nzchar(Sys.getenv("NFL_SOLVER_GAME_EXCLUSIONS_PATH"))) {
    message(
      "WARNING: NFL_SOLVER_GAME_EXCLUSIONS_PATH is set and overrides the default exclusion file: ",
      Sys.getenv("NFL_SOLVER_GAME_EXCLUSIONS_PATH")
    )
  }
  if (nzchar(Sys.getenv("NFL_PIPELINE_DATA_DIR"))) {
    message(
      "WARNING: NFL_PIPELINE_DATA_DIR is set and overrides the default data folder: ",
      Sys.getenv("NFL_PIPELINE_DATA_DIR")
    )
  }
  if (file.exists(SOLVER_GAME_EXCLUSIONS_PATH)) {
    .solver_excl_probe <- try(
      utils::read.csv(SOLVER_GAME_EXCLUSIONS_PATH, stringsAsFactors = FALSE, check.names = FALSE),
      silent = TRUE
    )
    if (inherits(.solver_excl_probe, "try-error")) {
      message("Solver game exclusions probe: file exists but could not be read.")
    } else {
      message(
        "Solver game exclusions probe: ", nrow(.solver_excl_probe),
        " row(s); columns: ", paste(names(.solver_excl_probe), collapse = ", ")
      )
      if ("game_id" %in% names(.solver_excl_probe) && nrow(.solver_excl_probe) > 0) {
        message(
          "Solver game exclusions probe first game_id(s): ",
          paste(utils::head(.solver_excl_probe$game_id, 5), collapse = ", ")
        )
      }
    }
    rm(.solver_excl_probe)
  } else {
    message("Solver game exclusions probe: file does not exist at configured path.")
  }
  
      filter_solver_excluded_games <- function(obj, label, env = parent.frame()) {
        # Initializes solver_excluded_games once, then reuses that same object for
        # every downstream data frame. If obj has no game_id, this is a no-op.
        solver_exclusions_initialize_and_filter(
          obj,
          df_name = label,
          env = env,
          audit = TRUE
        )
      }
      
      save_rds_csv_checkpoint <- function(obj, rds_file, csv_file, label) {
        message(label, ": ", nrow(obj), " rows x ", ncol(obj), " columns")
      
        if (isTRUE(WRITE_RDS_CHECKPOINTS)) {
          saveRDS(obj, rds_file)
          message("Wrote RDS checkpoint: ", rds_file)
        }
      
        if (isTRUE(WRITE_CSV_CHECKPOINTS)) {
          write.csv(obj, csv_file, row.names = FALSE)
          message("Wrote CSV checkpoint: ", csv_file)
        }
      }
      
      save_legacy_rdata <- function(obj, object_name, file) {
        if (!isTRUE(WRITE_LEGACY_RDATA_CHECKPOINTS)) {
          return(invisible(FALSE))
        }
      
        env <- new.env(parent = emptyenv())
        assign(object_name, obj, envir = env)
        save(list = object_name, file = file, envir = env)
        message("Wrote legacy RData checkpoint: ", file, " [object: ", object_name, "]")
        invisible(TRUE)
      }
      
      save_asof_next_outputs <- function(asof_next) {
        if (isTRUE(WRITE_RDS_CHECKPOINTS)) {
          saveRDS(asof_next, ASOF_NEXT_WEEK_AVGS_RDS)
          message("Wrote RDS checkpoint: ", ASOF_NEXT_WEEK_AVGS_RDS)
        }
      
        if (isTRUE(WRITE_CSV_CHECKPOINTS)) {
          write.csv(asof_next$asof_next_post, ASOF_NEXT_WEEK_POSTEAM_AVGS_CSV, row.names = FALSE)
          write.csv(asof_next$asof_next_def, ASOF_NEXT_WEEK_DEFTEAM_AVGS_CSV, row.names = FALSE)
          write.csv(asof_next$asof_next_nfl, ASOF_NEXT_WEEK_NFL_AVGS_CSV, row.names = FALSE)
          message("Wrote as-of next week CSV checkpoints.")
        }
      
        if (isTRUE(WRITE_LEGACY_RDATA_CHECKPOINTS)) {
          asof_next_post <- asof_next$asof_next_post
          asof_next_def <- asof_next$asof_next_def
          asof_next_nfl <- asof_next$asof_next_nfl
          save(asof_next_post, asof_next_def, asof_next_nfl, file = LEGACY_ASOF_NEXT_WEEK_AVGS_RDATA)
          message("Wrote legacy RData checkpoint: ", LEGACY_ASOF_NEXT_WEEK_AVGS_RDATA)
        }
      }
      
      message("Running football projection data pipeline...")
      message("Seasons: ", paste(SEASONS, collapse = ", "))
      
      # Step 1 ----------------------------------------------------------------------
      final_game_data <- build_game_level_data(
        seasons = SEASONS,
        stadium_coordinates_file = STADIUM_COORDINATES_FILE,
        team_coordinates_file = TEAM_COORDINATES_FILE,
        opponent_coordinates_file = OPPONENT_COORDINATES_FILE,
        include_overtime = INCLUDE_OVERTIME
      )
  
      final_game_data <- filter_solver_excluded_games(final_game_data, "Step 1 final_game_data")
      
      save_rds_csv_checkpoint(
        final_game_data,
        FINAL_GAME_DATA_RDS,
        FINAL_GAME_DATA_CSV,
        "Step 1 final_game_data"
      )
      
      if (isTRUE(WRITE_CSV_CHECKPOINTS)) {
        write.csv(final_game_data, LEGACY_FINAL_GAME_DATA_CSV, row.names = FALSE)
        message("Wrote legacy CSV checkpoint: ", LEGACY_FINAL_GAME_DATA_CSV)
      }
      save_legacy_rdata(final_game_data, "final_game_data", LEGACY_FINAL_GAME_DATA_RDATA)
      
      # Step 2 ----------------------------------------------------------------------
      merged_data <- add_rolling_team_averages(
        final_game_data = final_game_data,
        rolling_games = ROLLING_GAMES,
        max_placeholder_week = MAX_PLACEHOLDER_WEEK
      )
  
      merged_data <- filter_solver_excluded_games(merged_data, "Step 2 merged_data")
      
      save_rds_csv_checkpoint(
        merged_data,
        MERGED_DATA_RDS,
        MERGED_DATA_CSV,
        "Step 2 merged_data"
      )
      save_legacy_rdata(merged_data, "merged_data", LEGACY_MERGED_DATA_RDATA)
      
      # Step 3 ----------------------------------------------------------------------
      data_sos1 <- normalize_sos_data(
        merged_data = merged_data,
        sos_divisor = SOS_DIVISOR,
        min_model_season = MIN_MODEL_SEASON
      )
  
      data_sos1 <- filter_solver_excluded_games(data_sos1, "Step 3 normalized data_sos1")
      
      save_rds_csv_checkpoint(
        data_sos1,
        NORMALIZED_DATA_RDS,
        NORMALIZED_DATA_CSV,
        "Step 3 normalized data_sos1"
      )
      
      if (isTRUE(WRITE_CSV_CHECKPOINTS)) {
        write.csv(data_sos1, LEGACY_DATA_SOS1_CSV, row.names = FALSE)
        message("Wrote legacy CSV checkpoint: ", LEGACY_DATA_SOS1_CSV)
      }
      save_legacy_rdata(data_sos1, "data.sos", LEGACY_DATA_SOS1_RDATA)
      
      # Step 3.5 --------------------------------------------------------------------
      model_data <- add_projection_features(
        data_sos = data_sos1,
        drop_support_average_columns = TRUE
      )
  
      model_data <- filter_solver_excluded_games(model_data, "Step 3.5 model_data")
      
      save_rds_csv_checkpoint(
        model_data,
        MODEL_DATA_RDS,
        MODEL_DATA_CSV,
        "Step 3.5 model_data"
      )
      
      if (isTRUE(WRITE_CSV_CHECKPOINTS)) {
        write.csv(model_data, LEGACY_DATA_SOS2_CSV, row.names = FALSE)
        message("Wrote legacy CSV checkpoint: ", LEGACY_DATA_SOS2_CSV)
      }
      save_legacy_rdata(model_data, "data.sos", LEGACY_DATA_SOS2_RDATA)
      
      # Step 4 ----------------------------------------------------------------------
      step4 <- add_normalized_team_averages(
        model_data = model_data,
        weights = DEFAULT_WEIGHTS_14,
        max_placeholder_week = MAX_PLACEHOLDER_WEEK
      )
      
      merged_data_sos <- step4$data
      asof_next <- step4$asof_next
  
      merged_data_sos <- filter_solver_excluded_games(merged_data_sos, "Step 4 merged_data_sos")
      
      save_rds_csv_checkpoint(
        merged_data_sos,
        NORMALIZED_AVG_DATA_RDS,
        NORMALIZED_AVG_DATA_CSV,
        "Step 4 merged_data_sos"
      )
      save_legacy_rdata(merged_data_sos, "merged_data", LEGACY_MERGED_DATA_SOS_RDATA)
      save_asof_next_outputs(asof_next)
      
      # Step 4.5 --------------------------------------------------------------------
      final_data <- append_future_games(
        base_merged_data = merged_data,
        normalized_avg_data = merged_data_sos,
        future_games_file = FUTURE_GAMES_FILE,
        asof_next = asof_next,
        template_col_count = GAME_TEMPLATE_COL_COUNT
      )
  
      final_data <- filter_solver_excluded_games(final_data, "Step 4.5 final_data")
      
      save_rds_csv_checkpoint(
        final_data,
        FINAL_DATA_RDS,
        FINAL_DATA_CSV,
        "Step 4.5 final_data"
      )
      save_legacy_rdata(final_data, "final_data", LEGACY_FINAL_DATA_RDATA)
      
      
      # Step 5 ----------------------------------------------------------------------
      step5 <- build_interaction_data(
        final_data = final_data,
        game_template_source = NULL,
        template_col_count = GAME_TEMPLATE_COL_COUNT,
        interaction_col_count = INTERACTION_COL_COUNT
      )
      
      game_template <- step5$game_template
      data_interact <- step5$data_interact
  
      game_template <- filter_solver_excluded_games(game_template, "Step 5 game_template")
      data_interact <- filter_solver_excluded_games(data_interact, "Step 5 data_interact")
      
      save_rds_csv_checkpoint(
        game_template,
        GAME_TEMPLATE_RDS,
        GAME_TEMPLATE_CSV,
        "Step 5 game_template"
      )
      if (isTRUE(WRITE_CSV_CHECKPOINTS)) {
        write.csv(game_template, LEGACY_GAME_TEMPLATE_CSV, row.names = FALSE)
        message("Wrote legacy CSV checkpoint: ", LEGACY_GAME_TEMPLATE_CSV)
      }
      save_legacy_rdata(game_template, "game.template", LEGACY_GAME_TEMPLATE_RDATA)
      
      save_rds_csv_checkpoint(
        data_interact,
        DATA_INTERACT_RDS,
        DATA_INTERACT_CSV,
        "Step 5 data_interact"
      )
      if (isTRUE(WRITE_CSV_CHECKPOINTS)) {
        write.csv(data_interact, LEGACY_DATA_INTERACT_CSV, row.names = FALSE)
        message("Wrote legacy CSV checkpoint: ", LEGACY_DATA_INTERACT_CSV)
      }
      save_legacy_rdata(data_interact, "data.interact", LEGACY_DATA_INTERACT_RDATA)
      
      # Step 6 ----------------------------------------------------------------------
      data_differences <- build_matchup_differences(
        game_template = game_template,
        data_interact = data_interact
      )
  
      data_differences <- filter_solver_excluded_games(data_differences, "Step 6 data_differences")
      
      save_rds_csv_checkpoint(
        data_differences,
        DATA_DIFFERENCES_RDS,
        DATA_DIFFERENCES_CSV,
        "Step 6 data_differences"
      )
      if (isTRUE(WRITE_CSV_CHECKPOINTS)) {
        write.csv(data_differences, LEGACY_DATA_DIFFERENCES_CSV, row.names = FALSE)
        message("Wrote legacy CSV checkpoint: ", LEGACY_DATA_DIFFERENCES_CSV)
      }
      save_legacy_rdata(data_differences, "merged_data", LEGACY_DATA_DIFFERENCES_RDATA)
      
      data_sums <- build_matchup_sums(
        game_template = game_template,
        data_interact = data_interact
      )
  
      data_sums <- filter_solver_excluded_games(data_sums, "Step 6 data_sums")
      
      save_rds_csv_checkpoint(
        data_sums,
        DATA_SUMS_RDS,
        DATA_SUMS_CSV,
        "Step 6 data_sums"
      )
      if (isTRUE(WRITE_CSV_CHECKPOINTS)) {
        write.csv(data_sums, LEGACY_DATA_SUMS_CSV, row.names = FALSE)
        message("Wrote legacy CSV checkpoint: ", LEGACY_DATA_SUMS_CSV)
      }
      save_legacy_rdata(data_sums, "merged_data", LEGACY_DATA_SUMS_RDATA)
      
      # Step 7 v11 ------------------------------------------------------------------
      data_v11 <- final_preprocess_v11(
        data_interact = data_interact,
        data_sums = data_sums,
        data_differences = data_differences,
        seasons = SEASONS,
        weather_start_season = WEATHER_START_SEASON,
        min_output_season = FINAL_PREPROCESS_MIN_SEASON,
        append_night_games_file = APPEND_NIGHT_GAMES_FILE,
        weather_override_file = WEATHER_OVERRIDE_FILE,
        weather_override_season = WEATHER_OVERRIDE_SEASON,
        team_stadium_surface_file = TEAM_STADIUM_SURFACE_FILE
      )
  
      data_v11 <- filter_solver_excluded_games(data_v11, "Step 7 dataV11")
      
      save_rds_csv_checkpoint(
        data_v11,
        DATA_V11_RDS,
        DATA_V11_CSV,
        "Step 7 dataV11"
      )
      save_legacy_rdata(data_v11, "data", LEGACY_DATA_V11_RDATA)
      
      # Step 7 v12 ------------------------------------------------------------------
      data_v12 <- final_preprocess_v12(
        data_interact = data_interact,
        data_sums = data_sums,
        data_differences = data_differences,
        seasons = SEASONS,
        weather_start_season = WEATHER_START_SEASON,
        min_output_season = FINAL_PREPROCESS_MIN_SEASON,
        append_night_games_file = APPEND_NIGHT_GAMES_FILE,
        weather_override_file = WEATHER_OVERRIDE_FILE,
        weather_override_season = WEATHER_OVERRIDE_SEASON,
        team_stadium_surface_file = TEAM_STADIUM_SURFACE_FILE
      )
  
      data_v12 <- filter_solver_excluded_games(data_v12, "Step 7 dataV12")
      
      save_rds_csv_checkpoint(
        data_v12,
        DATA_V12_RDS,
        DATA_V12_CSV,
        "Step 7 dataV12"
      )
      save_legacy_rdata(data_v12, "data", LEGACY_DATA_V12_RDATA)

      # A completed game's rushing box score must never become its own
      # pregame predictor after the Step 7 merges. These three columns once
      # leaked from the game template into both model-ready data sets.
      asof_key <- function(x) paste(x$game_id, x$posteam_type, sep = "|")
      source_key <- asof_key(data_interact)
      if (anyDuplicated(source_key)) stop("Duplicate Step 5 game/team keys.", call. = FALSE)
      for (model_data in list(data_v11, data_v12)) {
        model_key <- asof_key(model_data)
        match_at <- match(model_key, source_key)
        if (anyNA(match_at) || anyDuplicated(model_key)) {
          stop("Step 7 as-of rushing audit cannot align game/team rows.", call. = FALSE)
        }
        for (column in c("rush_RB", "rushing_yards_excl_qb_kneel",
                         "rushing_yards_excl_qb_scramble")) {
          expected <- as.numeric(data_interact[[column]][match_at])
          observed <- as.numeric(model_data[[column]])
          if (!isTRUE(all.equal(observed, expected, tolerance = 1e-8,
                                check.attributes = FALSE))) {
            stop("Step 7 same-game rushing leakage detected in ", column,
                 ". Model run stopped before training.", call. = FALSE)
          }
        }
      }
      message("Step 7 as-of rushing audit passed for v11 and v12.")
      
      
      
      # Final audit before regression/solver families. This reuses the same
      # solver_excluded_games object initialized after Step 1; it does not re-read or
      # re-resolve the CSV.
      solver_exclusions_apply_checkpoint(
        object_names = c(
          "final_game_data", "merged_data", "data_sos1", "model_data",
          "merged_data_sos", "final_data", "game_template", "data_interact",
          "data_differences", "data_sums", "data_v11", "data_v12"
        ),
        reference_object_names = c("final_game_data"),
        env = environment(),
        audit = TRUE,
        checkpoint_name = "pre-regression solver exclusion audit",
        quiet_if_no_reference = FALSE
      )
  
      # Step 8 late-season score/total regression family ----------------------------
      # This family came from the 2024/2025/2026 Step 8 RMDs and uses dataV11.
      late_score_total_results <- lapply(
        LATE_SCORE_TOTAL_TARGET_SEASONS,
        function(target_season) {
          run_late_season_score_total_regression(
            data_v11 = data_v11,
            target_season = target_season,
            output_dir = OUTPUT_DIR,
            training_min_week = LATE_SCORE_TOTAL_TRAINING_MIN_WEEK,
            exclude_training_seasons = LATE_SCORE_TOTAL_EXCLUDE_TRAINING_SEASONS,
            holdout_test_season = LATE_SCORE_TOTAL_HOLDOUT_TEST_SEASON,
            write_outputs = TRUE,
            source_dataset = "dataV11"
          )
        }
      )
      names(late_score_total_results) <- paste0("late_score_total_", LATE_SCORE_TOTAL_TARGET_SEASONS)
      
      # Step 9 Billy Walters score-difference regression family ----------------------
      # This family came from the 2024/2025/2026 Step 9 RMDs.  All three source files
      # loaded dataV11 and ScoresRegressionLate2024_data, so use the 2024 Step 8
      # scored dataset as the base input unless config says otherwise.
      base_step8_name <- paste0("late_score_total_", BILLY_SCORE_DIFF_BASE_STEP8_TARGET_SEASON)
      if (!base_step8_name %in% names(late_score_total_results)) {
        stop(
          "Step 9 requires Step 8 result '", base_step8_name,
          "'. Add that season to LATE_SCORE_TOTAL_TARGET_SEASONS or change BILLY_SCORE_DIFF_BASE_STEP8_TARGET_SEASON.",
          call. = FALSE
        )
      }
      
      billy_score_diff_results <- lapply(
        BILLY_SCORE_DIFF_TARGET_SEASONS,
        function(target_season) {
          run_billy_walters_score_diff_regression(
            data_v11 = data_v11,
            late_score_total_2024_data = late_score_total_results[[base_step8_name]]$data,
            target_season = target_season,
            output_dir = OUTPUT_DIR,
            holdout_test_season = BILLY_SCORE_DIFF_HOLDOUT_TEST_SEASON,
            training_min_week = BILLY_SCORE_DIFF_TRAINING_MIN_WEEK,
            exclusion_map = BILLY_SCORE_DIFF_EXCLUDE_TRAINING_SEASONS_BY_TARGET,
            output_split_map = BILLY_SCORE_DIFF_OUTPUT_SPLIT_BY_TARGET,
            write_outputs = TRUE,
            source_dataset = paste0("dataV11 + ", base_step8_name, " scored data"),
            team_stadium_surface_file = TEAM_STADIUM_SURFACE_FILE
          )
        }
      )
      names(billy_score_diff_results) <- paste0("billy_score_diff_", BILLY_SCORE_DIFF_TARGET_SEASONS)
      
      
      
      # Step 9 Billy Walters team-score regression family ----------------------------
      # This family came from the 2024/2025/2026 Team Score RMDs. All three source
      # files loaded dataV11 and ScoresRegressionLate2024_data, so use the configured
      # Step 8 scored dataset as the base input.
      base_step8_name_team <- paste0("late_score_total_", BILLY_TEAM_SCORE_BASE_STEP8_TARGET_SEASON)
      if (!base_step8_name_team %in% names(late_score_total_results)) {
        stop(
          "Step 9 Team Score requires Step 8 result '", base_step8_name_team,
          "'. Add that season to LATE_SCORE_TOTAL_TARGET_SEASONS or change BILLY_TEAM_SCORE_BASE_STEP8_TARGET_SEASON.",
          call. = FALSE
        )
      }
      
      billy_team_score_results <- lapply(
        BILLY_TEAM_SCORE_TARGET_SEASONS,
        function(target_season) {
          run_billy_walters_team_score_regression(
            data_v11 = data_v11,
            late_score_total_2024_data = late_score_total_results[[base_step8_name_team]]$data,
            target_season = target_season,
            output_dir = OUTPUT_DIR,
            holdout_test_season = BILLY_TEAM_SCORE_HOLDOUT_TEST_SEASON,
            training_min_week = BILLY_TEAM_SCORE_TRAINING_MIN_WEEK,
            exclusion_map = BILLY_TEAM_SCORE_EXCLUDE_TRAINING_SEASONS_BY_TARGET,
            output_split_map = BILLY_TEAM_SCORE_OUTPUT_SPLIT_BY_TARGET,
            write_outputs = TRUE,
            source_dataset = paste0("dataV11 + ", base_step8_name_team, " scored data"),
            team_stadium_surface_file = TEAM_STADIUM_SURFACE_FILE
          )
        }
      )
      names(billy_team_score_results) <- paste0("billy_team_score_", BILLY_TEAM_SCORE_TARGET_SEASONS)
      
      
      # Step 10 XGBoost total/score tree regression family --------------------------
      # Solver mode tunes target-season-specific hyperparameters; fast mode reuses
      # them to produce the ScoresTrees home/away outputs.
      xgb_tree_results <- lapply(
        XGB_TREE_TARGET_SEASONS,
        function(target_season) {
          excl <- XGB_TREE_EXCLUDE_TRAINING_SEASONS_BY_TARGET[[as.character(target_season)]]
          if (is.null(excl)) excl <- integer(0)
      
          run_xgb_total_score_tree_regression(
            data_v11 = data_v11,
            target_season = target_season,
            output_dir = OUTPUT_DIR,
            template_dir = file.path("R", "step10_templates"),
            solve_params = XGB_TREE_SOLVE_PARAMS,
            run_fast = XGB_TREE_RUN_FAST,
            force_resolve_params = XGB_TREE_FORCE_RESOLVE_PARAMS,
            holdout_test_season = XGB_TREE_HOLDOUT_TEST_SEASON,
            training_min_week = XGB_TREE_TRAINING_MIN_WEEK,
            exclude_training_seasons = excl,
            exclude_feature_columns = XGB_TREE_EXCLUDE_FEATURE_COLUMNS
          )
        }
      )
      names(xgb_tree_results) <- paste0("xgb_trees_", XGB_TREE_TARGET_SEASONS)
      
      
      # Step 11 Billy Walters XGBoost total/score tree regression family ------------
      # Solver mode tunes target-season-specific Billy-tree hyperparameters; fast mode
      # reuses them to produce the BillyTrees home/away outputs.
      xgb_billy_tree_results <- lapply(
        XGB_BILLY_TREE_TARGET_SEASONS,
        function(target_season) {
          excl <- XGB_BILLY_TREE_EXCLUDE_TRAINING_SEASONS_BY_TARGET[[as.character(target_season)]]
          if (is.null(excl)) excl <- integer(0)
      
          run_xgb_billy_total_score_tree_regression(
            data_v12 = data_v12,
            target_season = target_season,
            output_dir = OUTPUT_DIR,
            template_dir = file.path("R", "step11_templates"),
            solve_params = XGB_BILLY_TREE_SOLVE_PARAMS,
            run_fast = XGB_BILLY_TREE_RUN_FAST,
            force_resolve_params = XGB_BILLY_TREE_FORCE_RESOLVE_PARAMS,
            holdout_test_season = XGB_BILLY_TREE_HOLDOUT_TEST_SEASON,
            training_min_week = XGB_BILLY_TREE_TRAINING_MIN_WEEK,
            exclude_training_seasons = excl
          )
        }
      )
      names(xgb_billy_tree_results) <- paste0("xgb_billy_trees_", XGB_BILLY_TREE_TARGET_SEASONS)
      
      # Step 12 Ensemble Model Wrangler export -----------------------------------
      # Creates the compact RDS files and prediction-column inventory used by the
      # Shiny ensemble app. This reads the final exclusion-model CSVs from
      # OUTPUT_DIR and writes the deploy-sized bundle to
      # output/ensemble_model_wrangler/data.
      message("Preparing Ensemble Model Wrangler compact data...")
      # The generic exporter would re-predict completed 2026 games. The
      # guarded exporter restores their archived pregame projections.
      source("prepare_data_legacy.R", local = TRUE)
      ensemble_model_wrangler_export <- list(
        data_dir = file.path(OUTPUT_DIR, "ensemble_model_wrangler", "data")
      )

      # Keep the deployed standalone app synchronized with the freshly prepared
      # legacy bundle, matching the automatic deployment performed by Step 2 for
      # NextGen. Step 1 is already required to run from this pipeline directory.
      pipeline_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
      legacy_app_data_dir <- file.path(
        dirname(pipeline_dir),
        "Apps",
        "Projection_Model_Wrangler_RDS_deploy",
        "data"
      )
      if (!dir.exists(legacy_app_data_dir)) {
        dir.create(legacy_app_data_dir, recursive = TRUE, showWarnings = FALSE)
      }

      legacy_deploy_files <- c(
        "compact_models.rds",
        "model_inventory.rds",
        "prediction_column_inventory.csv"
      )
      legacy_deploy_sources <- file.path(ensemble_model_wrangler_export$data_dir, legacy_deploy_files)
      missing_legacy_deploy_sources <- legacy_deploy_sources[!file.exists(legacy_deploy_sources)]
      if (length(missing_legacy_deploy_sources) > 0) {
        stop(
          "Legacy app deployment files were not created: ",
          paste(missing_legacy_deploy_sources, collapse = ", "),
          call. = FALSE
        )
      }

      legacy_copy_ok <- file.copy(
        from = legacy_deploy_sources,
        to = file.path(legacy_app_data_dir, legacy_deploy_files),
        overwrite = TRUE
      )
      if (!all(legacy_copy_ok)) {
        stop(
          "Could not copy the complete legacy RDS bundle into the standalone app. Failed: ",
          paste(legacy_deploy_files[!legacy_copy_ok], collapse = ", "),
          call. = FALSE
        )
      }
      message(
        "Copied legacy Ensemble Model Wrangler bundle to app data folder: ",
        normalizePath(legacy_app_data_dir, winslash = "/", mustWork = FALSE)
      )
      
      message("Pipeline complete through Step 12 Ensemble Model Wrangler export and app deployment.")
      message("Primary pre-regression outputs: ", DATA_V11_RDS, " and ", DATA_V12_RDS)
      message("Late score/total regression target seasons: ", paste(LATE_SCORE_TOTAL_TARGET_SEASONS, collapse = ", "))
      message("Billy Walters score-difference target seasons: ", paste(BILLY_SCORE_DIFF_TARGET_SEASONS, collapse = ", "))
      message("Billy Walters team-score target seasons: ", paste(BILLY_TEAM_SCORE_TARGET_SEASONS, collapse = ", "))
      message("XGBoost total/score tree target seasons: ", paste(XGB_TREE_TARGET_SEASONS, collapse = ", "))
      message("Billy XGBoost total/score tree target seasons: ", paste(XGB_BILLY_TREE_TARGET_SEASONS, collapse = ", "))
      message("Ensemble compact data files: ", file.path(ensemble_model_wrangler_export$data_dir, "compact_models.rds"),
              " and ", file.path(ensemble_model_wrangler_export$data_dir, "model_inventory.rds"))
