# Targeted replay after correcting the Step 10 home/away game-identity bridge.
source(file.path("R", "00_config.R"))
source(file.path("R", "14_xgboost_billy_total_score_trees.R"))

data_v12 <- readRDS(file.path(OUTPUT_DIR, "dataV12.rds"))
for (target_season in XGB_BILLY_TREE_TARGET_SEASONS) {
  excl <- XGB_BILLY_TREE_EXCLUDE_TRAINING_SEASONS_BY_TARGET[[as.character(target_season)]]
  if (is.null(excl)) excl <- integer(0)
  message("Rebuilding Step 11 for ", target_season)
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
