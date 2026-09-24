# 2026 Week 3 production training snapshot

These are byte-for-byte copies of the modified source files in the active local
Week 3 Next Gen and legacy pipelines. They are versioned here so the production
training cutoff is not represented only by serialized model artifacts. The app
deployment script does not bundle or execute these source copies.

- Next Gen source: `Football_2026/Football_projection_pipeline_exclusion_week3/`
- Legacy source: `Football_2026/Legacy_models/nflFast_Model_2026_Scores_week3/code/`
- Production outcome training ends before season 2026. Prior-game features may
  still roll forward, but completed 2026 outcomes are excluded from fitting.
- The legacy Step 12 copy includes the `stringr::regex` namespace fix required
  to package the completed model outputs.

Before running a later weekly copy, verify that its training filters still
exclude the active season. These copies document this Week 3 run; they are not
standalone pipelines and should not be substituted into a newer week blindly.
