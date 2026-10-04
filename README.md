# NFL-projections
NFL Ensemble Model

This repository publishes the existing Gmail-owned NFL Projection Ensemble
Wrangler at `https://rogerroot-nfl-projections.share.connect.posit.cloud/`.
Do not deploy this standalone app to the Yahoo Posit account; that account is
for Pro Football Lab only.

The Dashboard and its CSV downloads include every qualifying consensus row
for the selected source, season, week, and markets. They do not exclude rows
based on +2.5 underdogs, -3.5 favorites, or early-week games.

The 2026 Legacy and Next Gen bundles keep pregame Week 1 onward predictions in
immutable per-game archives. Current results and lines can refresh, but a
completed game's forecast must come from its saved pregame snapshot. The
preparation scripts and replay checks live in
`training_sources/2026_week3/pregame_replay/`; production preparation must
stop if an archived completed game is missing.

For 2026, `data/compact_models.rds` (Legacy) comes from the standalone
`Legacy_models/nflFast_Model_2026_Scores_week3` Step 12 export.
`data/nextgen_compact_models.rds` comes from the active Next Gen pipeline.
The converted pipeline's Legacy-equivalent output is not interchangeable with
the standalone Legacy forecast; verify parity before ever substituting it.
Run `tools/verify_canonical_bundles.R` from this repository before publishing;
it checks exact source hashes and frozen past-game predictions.

The PoolHost Picks tab bundles the verified 2026 Week 3 Sunday/Monday spreads.
PoolHost's pick sheet requires the user's signed-in browser session, so later
weeks are loaded by pasting that week's sheet into the tab. The app checks the
week, every matchup, home/away orientation, and completeness against the model
schedule before showing all-game consensus picks. It does not submit picks or
export a card.

The DraftKings Picks tab copies the selected week's loaded PoolHost spreads as
editable starting values; it never changes the PoolHost card. Verify each
home spread against the DraftKings contest, then build a five-pick shadow card
and two alternates using the current consensus model settings. The tab offers
minimum-edge, early-week, and specific spread exclusions, plus downloads for
the top five, chosen final five, and all eligible plays. If the active week has
no bundled PoolHost sheet, paste and load that week's sheet in PoolHost Picks
first. The app does not fetch DraftKings contest lines or submit picks.
