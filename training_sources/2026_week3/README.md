# 2026 Week 3 production training snapshot

These are byte-for-byte copies of the modified source files in the active local
Week 3 Next Gen and legacy pipelines. They are versioned here so the production
training cutoff is not represented only by serialized model artifacts. The app
deployment script does not bundle or execute these source copies.

- Next Gen source: `Football_2026/Football_projection_pipeline_exclusion/`
- Legacy source: `Football_2026/Legacy_models/nflFast_Model_2026_Scores_week3/code/`
- Production outcome training ends before season 2026. Prior-game features may
  still roll forward, but completed 2026 outcomes are excluded from fitting.
- Both preprocessing paths use pre-2026 reference values for global
  imputation. The existing Gmail Wrangler and the standalone Legacy Step 12
  restore archived pregame predictions for completed 2026 games.
- The converted Step 11 bridge pairs home/away Step 10 scores by `game_id`.
- The Gmail Wrangler's Legacy bundle is exported by the standalone Week 3
  Legacy `Step 12 Prepare small data.R`. The converted Next Gen pipeline's
  Legacy-equivalent forecasts differ materially and must not replace this
  standalone bundle without a separate parity review.
- The source copies in `pregame_replay/`, `nextgen_preprocessing/`, and
  `legacy/` document the exact corrected production code.

Before running a later weekly copy, verify that its training filters still
exclude the active season. These copies document this Week 3 run; they are not
standalone pipelines and should not be substituted into a newer week blindly.
