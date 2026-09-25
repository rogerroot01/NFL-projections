# 2026 pregame replay guard

The Wrangler must grade the forecast that existed before each game, not a
projection regenerated after game data arrived. `prepare_data_nextgen.R` and
`prepare_data_legacy.R` use `pregame_projection_archive.R` to enforce this.

- The initial archive captures Weeks 1–3 from their saved pregame folders.
- On later preparation runs, predictions for games dated **after today** and
  still without scores refresh in the archive. Same-day games are not replaced
  automatically; run the preparation step the prior day to capture updates.
- Once a game date is past or both final scores are present, only its saved
  pregame prediction columns are used. Current actual scores and market lines
  remain available for grading.
- Missing archived completed games stop the build. Game identity uses season,
  week, home team, away team, and team side, not the unstable `game_id` format.
- `test_pregame_archive.R` checks every Week 1–2 archived prediction and that
  new 2026 outcomes cannot change pre-2026 imputation constants.

The live standalone Wrangler is the existing Gmail-owned NFL Projections app
at `https://connect.posit.cloud/rogerroot`; its Git source is
`https://github.com/rogerroot01/NFL-projections`. Do not publish this app to
the Yahoo account, create a duplicate app, or move Pro Football Lab to Gmail.
