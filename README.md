# NFL-projections
NFL Ensemble Model

This repository publishes the existing Gmail-owned NFL Projection Ensemble
Wrangler at `https://rogerroot-nfl-projections.share.connect.posit.cloud/`.
Do not deploy this standalone app to the Yahoo Posit account; that account is
for Pro Football Lab only.

The 2026 Legacy and Next Gen bundles keep pregame Week 1 onward predictions in
immutable per-game archives. Current results and lines can refresh, but a
completed game's forecast must come from its saved pregame snapshot. The
preparation scripts and replay checks live in
`training_sources/2026_week3/pregame_replay/`; production preparation must
stop if an archived completed game is missing.
