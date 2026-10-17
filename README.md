# NFL-projections
NFL Ensemble Model

This repository publishes the existing Gmail-owned NFL Projection Ensemble
Wrangler at `https://rogerroot-nfl-projections.share.connect.posit.cloud/`.
Do not deploy this standalone app to the Yahoo Posit account; that account is
for Pro Football Lab only.

Circa PDF imports tolerate a single OCR character error only when it resolves
to one NFL team, recognize PK/zero spreads, and validate every matchup against
the selected week's schedule. Web refresh sends browser request headers and
checks the HTTP response and PDF signature. A failed refresh preserves an
already loaded sheet for the same week. Week 6 has a verified bundled fallback.
Run `tests/circa_week6.R` (with `CIRCA_TEST_WEB=1` for live refresh) and
`tests/circa_week5.R` to check the PDF/upload/build flow and older sheets.

The Dashboard and its CSV downloads apply the Consensus Setup exclusions for
every selected source. Optional spread filters exclude -3.5 favorites, -7.5
favorites, favorites laying 10.5 points or more, and +2.5 underdogs. The
early-week filter applies across markets. These filters are enabled by default.

The 2026 Legacy and Next Gen bundles keep pregame Week 1 onward predictions in
immutable per-game archives. Current results and lines can refresh, but a
completed game's forecast must come from its saved pregame snapshot. The
preparation scripts and replay checks live in
`training_sources/2026_week3/pregame_replay/`; production preparation must
stop if an archived completed game is missing.

For 2026, `data/compact_models.rds` (Legacy) comes from the standalone
`Legacy_models/nflFast_Model_2026_Scores_week4` Step 12 export.
`data/nextgen_compact_models.rds` comes from the active
`Football_projection_pipeline_exclusion` Next Gen export. That pipeline also
creates a separate Legacy-equivalent diagnostic export, but it does not replace
the standalone Legacy forecast in this app.
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
and three alternates using the current consensus model settings. The tab offers
minimum-edge, early-week, and specific spread exclusions, plus downloads for
the top five, chosen final five, and all eligible plays. Circa and DraftKings
both allow selecting the final five from the top eight eligible games. Their
10.5+ favorite filter excludes the selected favorite at -10.5 or any larger
handicap; +10.5 underdogs and -10 favorites remain eligible. If the active week has
no bundled PoolHost sheet, paste and load that week's sheet in PoolHost Picks
first. The app does not fetch DraftKings contest lines or submit picks.
