# foxplots 0.10.0

Hardening pass from testing eight apps against real files (Kaggle sets, an
11,000-row earthquake table). One severe defect, four usability fixes, the
first automated coverage for the app layer, and a manual QA checklist.

## Fixed: colouring a chart by an ID column froze the app

Picking a high-cardinality column as "Color / group by" asked ggplot2 for one
geom and one legend key per level. On 11,000 rows that took **111.7 seconds**
for a density plot (boxplot 105s, scatter 29s) -- the single-threaded app
simply stopped responding. Cost was linear in the level count, and hiding the
legend saved nothing.

- Discrete colour groups are now capped at **50**. Past that the colouring is
  dropped and an on-chart note names the column, the count and the limit, and
  suggests faceting instead. Same pattern as the 30-panel facet cap that has
  always been there -- the colour picker had simply never got one.
- **Continuous numeric colours are not capped**: they use one gradient scale
  and stay fast at any cardinality.
- The cap lives in the chart builder, so the **Export and Report downloads are
  fixed too** -- their error handling caught errors, not hangs, so they froze
  as well.
- The copy-ready R code mirrors the cap, so a snippet can no longer carry an
  11,000-element colour vector.

Result on the reported case: **111.7 s -> 0.30 s.**

## Map tool

- **The settings panel scrolls on its own.** Reaching the controls near the
  bottom used to scroll the whole page and push the map out of sight; the
  sidebar now has its own scrollbar and stays pinned beside a fixed map.
- **The settings are grouped.** Twenty-three controls spanning six concerns
  all sat under one "Style" heading; they are now Basemap / Color / Markers /
  Combine & cluster / Layer groups / Popups & labels / Finishing touches /
  Download, with each section hidden when it does not apply.
- **"Region name property" shows an example of each option** --
  `county_state - e.g. Brooks County, Georgia` rather than a bare property
  name -- so you can match it against your own column by eye.
- **The density heatmap now works with "Combine points by area".** Turning on
  the combine mode used to hide the heatmap entirely. The heat surface is
  built from the underlying points, so the two compose: density beneath,
  per-area counts on top. Hiding the markers as well gives a heatmap-only view.

## Mapping data that has no coordinates

Shaded regions never needed latitude or longitude -- a column of state, county
or country names plus a number to shade by is enough -- but nothing said so.

- The Map tab now **tells you** when it finds no coordinate columns, names the
  column that looks like region names, and points at Shaded regions.
- The map-type help no longer describes boundaries as something you upload
  (they have been built in since 0.9.0).
- **Fixed:** switching to Shaded regions before choosing the columns quietly
  drew a *points* map, so the "pick the region property" prompt was
  unreachable on any data that had coordinates.
- **Fixed:** "Zoom to data" was visible in Shaded-regions mode but did nothing
  (it needs coordinates); it is hidden there.

## Testing

- **First automated coverage for the twelve modules and eight launchers.**
  Previously only the helpers were tested, while the bugs that actually
  reached users lived in the wiring. Each launcher now builds, serves a real
  page, and has its whole server run; each module has its contract checked.
- The new tests were **mutation-tested** rather than assumed. Two mutations
  initially escaped -- stripping the new property-picker labels, and mistyping
  a module id so the server runs in a namespace with no UI -- and both gained
  a test. The suite grew from 1,372 checks to 1,607.
- **`TESTING_CHECKLIST.md`** is a new manual pass: per-app things to click,
  edge cases, the deliberate limits, and what good looks like for each.

# foxplots 0.9.0

## Every app can build a report -- and you pick what goes in it

- **All eight apps now have an "Export & Report" tab.** Reshape, Combine,
  Map, Mixed Model Review, and GLMM Review gain reports for the first time;
  each one reports what that app actually produces, so the contents differ
  by tool rather than pretending every app is the Data Explorer.
- **Choose your sections.** The Report tab's fixed checklist becomes a
  picker: tick or untick each section (everything starts ticked). Deselected
  work is skipped entirely, so leaving out a map also skips its slow
  snapshot.
- **The mixed-model tools report their models**: formulas, fit statistics,
  ANOVA (Type II/III for lmer, Type III Wald for GLMM), variance components,
  estimated marginal means with letter groupings and pairwise comparisons,
  the DHARMa residual battery for GLMMs, and the reproducible code. The GLMM
  app reports its general and binary fits as separate subsections when both
  are fitted.

## Built-in map boundaries -- no file hunting

- **Shaded-region maps now ship with their boundaries.** Pick **US states**,
  **US counties**, or **world countries** from the Boundaries dropdown and
  start shading immediately; uploading your own GeoJSON is still there as
  "Upload my own file...".
- **Limit large sets to what you need** -- counties to a single state (67
  Florida polygons instead of 3,222 national ones), countries to a
  continent.
- Each set ships several join properties so your data matches however it is
  keyed: states by name, abbreviation, or FIPS; counties by name,
  "County, State", or 5-digit FIPS (which also disambiguates Virginia's
  independent cities from the counties they share a name with); countries by
  name or ISO code. The natural key is preselected for you.
- Regions with no data are reported as coverage ("55 built-in regions have
  no data") rather than as a join warning, and the generated R code loads
  the same boundaries from the installed package so a pasted script
  reproduces the map exactly.
- Sources are public domain: US Census cartographic boundary files (1:20m)
  and Natural Earth (1:110m). All three sets together add ~620 KB.
- Fixes a latent limit: the renderer capped GeoJSON at 3,000 features, which
  would have silently dropped ~200 US counties. The cap is now 4,000 and
  reports truncation when it does bite.

# foxplots 0.8.0

A polish release: every item from the 0.6.0/0.7.0 field-testing punch list,
plus CRAN-readiness groundwork.

## Compare Groups

- **"Split by" can return to "(none)"** -- the placeholder trapped you
  before (the empty-valued option was never clickable once a variable was
  chosen). The dropdown now works both ways.
- **A live plan line under the pickers** says exactly what will run --
  "3 outcomes x 2 groups = 6 tests will run." -- growing an "x N levels of
  am" factor when split, and warning before you hit the limit. A companion
  line previews what a chosen split will do (levels, cap truncation, rows
  with missing split values) *before* anything computes.
- **The combination limit is now 24** (was 12), matching the pickers' own
  6-outcome x 4-group maximum; a full 6x4 grid runs with family-wide BH
  correction.
- **Connecting letters now show standard errors**: Group / Mean / SE / N /
  Letters (pooled SE on the ANOVA path, per-group on the rank path), in
  the app, both report formats, and the text export.

## Regression

- **The Estimated-means tab no longer clips its controls** in the Data
  Explorer (the panel stopped competing for viewport height; long tabs
  scroll like the standalone tool).
- **Model comparison explains itself**: a framing question, numbered
  steps, a worked example, and a live status line that walks you from
  "nothing fitted yet" to "Comparing A vs B" with both formulas shown.

## Reports

- **Wide tables are readable.** Every table sits in a scroll-in-place
  wrapper; tables past 8 columns get a compact face that fits the page.
  In Word -- which cannot scroll sideways -- wide tables are split into
  chunks that each repeat the label column and announce their column
  range.

## Map

- **Hide the point markers** (new checkbox) to view the density heatmap on
  its own; downloads and generated code follow suit, and a hint warns when
  the map would be basemap-only.
- **Combine points by area**: pick a county/region/zip-style column and
  the map draws ONE bubble per area at the centroid of its points, sized
  by how many it combines and shaded by the area average of your color
  column. The generated R script reproduces the aggregation. Columns with
  up to 100 areas qualify; the sidebar explains anything it refuses.
- The row-count footer now says when **Auto clustering** has actually
  switched on.

## One "Export & Report" tab

- The Export and Report tabs merged into a single navbar entry with two
  sub-tabs ("Data & downloads" / "Full report") in the Data Explorer,
  Compare Groups, and Regression tools.

## CRAN readiness

- `URL`/`BugReports` point at the public repository; `png` declared in
  Suggests; every exported function documents its return value;
  `R CMD check --as-cran` is clean apart from environment-specific NOTEs.

# foxplots 0.7.0

## An eighth app: GLMM Review (`run_glmm_review()`)

- **Generalized linear mixed models via `glmmTMB`**, contributed as a
  standalone app and absorbed into the kit's module pattern: import a table,
  pick a response, a distribution family, fixed and random effects from
  drop-downs, and fit -- no `glmmTMB()` calls to write.
- **Eight families** on the General GLMM tab (Gaussian identity/log, Gamma,
  Poisson, negative binomial nbinom1/nbinom2, Tweedie, Beta), each with a
  live plain-language note on what response values are valid -- and an
  automatic check of your actual response against that range **before** you
  fit.
- **Zero-inflation (`ziformula`) and dispersion (`dispformula`) side
  models**, point-and-click: toggle zero-inflation on (intercept-only or
  with predictors), and let residual dispersion vary with up to two
  predictors -- including user-created combined variables.
- **Binary (0/1) outcomes get their own tab** with a link-function picker
  (logit / probit / cloglog / cauchit) and Bernoulli-specific guardrails --
  no dispersion model is offered (0/1 data has no free dispersion
  parameter), and two-level factors are recoded with the second level as
  "success", stated on screen.
- **DHARMa simulation-based residual diagnostics** -- the right tool for
  glmmTMB, replacing the classic residual panel: QQ + residual plot,
  overdispersion, zero-inflation, and outlier tests, plus a classic Pearson
  chi-square / df cross-check.
- **Type III Wald ANOVA** (`car::Anova`), **EMMeans on the response scale**
  with pairwise comparisons and compact-letter display, an EMMeans plot,
  CSV/PNG downloads, and copy-pasteable R code reproducing the whole
  analysis.
- A seeded **field example dataset** (`make_glmm_example_data()`) with an
  overdispersed count, a zero-heavy count, a proportion, and a 0/1 response
  -- one column per family worth demonstrating.
- New dependencies: `glmmTMB` and `DHARMa` (Imports); `car` (Suggests,
  gracefully absent).

# foxplots 0.6.0

## Map: shaded regions (choropleth), density heatmap, and a scale bar

- **A second map type: Shaded regions.** Upload a **GeoJSON** boundary file
  (e.g. county outlines), match one of its properties to a column in your data,
  and shade each region by the mean / sum / median of a numeric column. The
  sidebar reports **exactly how many regions matched** and names the strays on
  both sides -- a join mismatch is never a mystery blank map. Unmatched regions
  draw pale grey. No GIS system dependencies (no sf/GDAL): styling is written
  into the GeoJSON itself.
- **A density heatmap layer** for point maps (optional `leaflet.extras`
  package): a continuous surface for data where bubbles overlap into a blob,
  optionally weighted by a numeric column, drawn under the markers.
- **A distance scale bar** (km + miles), on by default.
- The generated leaflet code emits all of the above; the choropleth script reads
  your boundary file from disk and reproduces the join + shading standalone.

## Compare Groups: split the analysis by a third variable

- **"Split by (optional)"** runs the whole comparison separately within each
  level of a third variable -- e.g. *mpg by cyl, split by am* gives one analysis
  for am=0 and another for am=1 (JMP's "By" grouping). The summary table gains a
  **Stratum** column, each stratum gets its own accordion panel, and p-values are
  BH-corrected across the full stratified grid. A high-cardinality split variable
  is capped (with a note) so the grid can't explode. (The regression tab already
  had the modelling equivalent via the EMMeans "Within" control.)

## A seventh app: the Regression Tool

- **`run_regression_tool()`** bundles the (now much deeper) Regression tab into
  its own focused app -- Import -> Regression -> Export -> Report -- for users
  who only want to fit models and can skip the rest of the Data Explorer. Like
  the other mini-apps it imports data (with Data Health / type recasting),
  exports the data and the fitted model, and produces a full HTML / Word report.

## Regression: categorical predictors, interactions, marginal means, and a real coefficient table

- **Predictors can now be categorical.** The Regression tab used to accept only
  numeric predictors; a character/factor column now enters the model as a factor
  (ANCOVA-style), with a **reference-level** picker for each one so you choose the
  baseline the other levels are compared against.
- **Interaction terms.** A checkbox adds all pairwise interactions (`a * b`), so
  a predictor's effect can depend on the others.
- **A proper coefficient table** replaces reading numbers off the raw summary:
  estimate, standard error, statistic, p-value, and a **95% confidence interval**
  per term. A fit-statistics card reports n, R2, adjusted R2, RMSE, AIC, BIC and
  the overall F-test.
- **Estimated marginal means with connecting letters.** A new tab gives the
  adjusted means for a categorical predictor (numeric predictors held at their
  means), Tukey/Sidak/Bonferroni pairwise comparisons, a compact-letter-display
  grouping, and a means-with-CI plot -- the same emmeans / multcomp engine the
  Mixed Model tool uses.
- **Logistic regression.** Switch the outcome type to Binary and the tab fits a
  `glm(family = binomial)` for a two-level response (factor / logical / 0-1),
  reporting **odds ratios with 95% CIs**, logistic fit statistics (null and
  residual deviance, McFadden pseudo-R2, classification accuracy), and a
  logistic-appropriate interpretation. The linear-only diagnostics (Q-Q,
  Scale-Location, the assumption panel) step aside with a note. This is the
  biggest gap the Mixed Model tool can't fill -- `lmerTest` is linear-only.
- **Model comparison.** Save the current fit as Model A, change the setup, fit
  again, and the tab runs a nested test (F for linear, likelihood-ratio for
  logistic) with an AIC/BIC delta and a clear caveat that it's valid only for
  nested models on the same rows.
- **A full diagnostics tab.** Beyond the original fitted-vs-actual and
  residuals-vs-fitted plots there are now Normal Q-Q, Scale-Location, and Cook's
  distance (with the 4/n influence line), an **assumption-check panel** with
  pass/check badges (residual normality via Shapiro-Wilk, constant variance via
  Breusch-Pagan, linearity via a quadratic-residual test, and an order-dependent
  Durbin-Watson independence flag), and a **VIF multicollinearity** table --
  hand-rolled to match `car::vif` with no `car` dependency.
- **Copy-ready R code.** Regression was the last major tab without a code export;
  it now emits scripts that reproduce the fit exactly (including factor reference
  levels and interaction/polynomial terms) and the EMMeans/post-hoc block.
- The engine is spec-driven (`reg_spec` -> `reg_validate` -> `reg_fit`) mirroring
  the Mixed Model tool; `fit_model()` stays as a thin, back-compatible wrapper.
  No new package dependencies -- confidence intervals are from `confint()`, and
  emmeans / multcomp were already required.

## The map now appears in the report

- **Reports include your map.** The Report tab gains a **Maps** section, in both
  the HTML and the editable Word output, sitting between Charts and Group
  comparison to match the tab order. The leaflet code is included too when
  "show reproducible code" is on.
- Maps are embedded as **static snapshots**, so a report still works emailed
  around or opened offline. That costs a headless-browser launch per map
  (several seconds), which the progress message now says out loud instead of
  looking frozen.
- **Exported maps now match what's on screen.** Snapshots used to render on a
  fixed 1200x800 canvas -- a different size and shape from the map pane in the
  browser -- so at the same zoom they framed a different area (typically a lot
  of surrounding ocean). Both the Map tab's PNG download and the report snapshot
  now render at the map pane's actual pixel size, so the image covers exactly
  the area you framed. Two follow-on fixes came with it: the leaflet widget is
  sized to the snapshot canvas so the basemap loads tiles for the whole image
  (no more grey rectangles of un-loaded map), and snapshots render at the
  screen's own pixel density (a 2x density made the CartoDB basemap drop about
  half its tiles).
- **A missing snapshot never fails the download.** Where the optional
  `webshot2` / `chromote` packages or Chrome are absent, the Maps section still
  appears and explains what's needed, rather than silently disappearing or
  erroring the whole report.

## Map: size scales and a size legend

- **"Size by" now has a scale.** Alongside the default **Linear**
  (area-proportional) sizing there is **Log**, which spreads out skewed values
  so the small end stops looking identical, and **Quantile**, which gives every
  bubble size an equal share of the points. Both fall back to linear -- with an
  explanation in the hint line -- when the data can't support them (log needs
  positive values; quantile needs at least three distinct ones).
- **Bubbles finally have a key.** Sizing by a column used to draw graduated
  circles with nothing to read them against. There is now a graduated-circle
  size legend in the bottom-left corner (the color legend keeps the
  bottom-right), and it can be switched off. Its circles are computed from the
  same code path as the markers, so the key cannot drift from the map.
- The generated leaflet code emits the chosen scale and the legend, and now
  reproduces the app's radii exactly -- including for a constant column, where
  the old snippet computed `0/0` and drew 4px markers while the app drew 11px
  ones.
- A non-finite value (`Inf`) in the size column is drawn at the smallest radius
  as documented, instead of being clamped to the largest bubble on the map.

## Fixes (several silent-failure paths made loud or correct)

- **PDF chart export produced an empty file on macOS.** The exporter used
  `cairo_pdf()`, and `capabilities("cairo")` reports build-time support even
  where cairo cannot actually load (macOS without XQuartz). The device never
  opened, no error was raised, the download served nothing, and a stray
  `Rplots.pdf` was left in the working directory. PDF now uses base
  `grDevices::pdf()` (no cairo, works everywhere), the device is probed rather
  than trusted, and an empty result is reported instead of returned. SVG, which
  has no base-R equivalent, now fails with an actionable message.
- **Regression died on any column name containing a space.** Excel headers keep
  their spaces (`readxl` preserves them verbatim, unlike `read.csv`), and the
  unbackticked formula failed with an opaque `unexpected symbol`. Response and
  predictors are now backticked, polynomial terms included.
- **Summaries silently corrupted data when a column shared a stat's name.** A
  grouping column called `N`, `Mean`, `Variable` (and so on) was overwritten by
  the statistic, leaving rows grouped correctly but unidentifiable; an outcome
  column named `Total` made `proportions_summary()` error outright. Statistics
  are now computed under reserved internal names, and your column keeps its own
  name if the two ever collide.
- **A stale filter could silently empty every tab.** Numeric operators
  dispatched on the operator alone, so a `pts > 20` filter left over after a
  Change Type -> Factor recast produced an all-NA mask and dropped every row
  while still displaying the filter chip. Comparisons are now type-aware and
  fall back to the documented no-op.
- **Exporting reshaped data with duplicate split keys.** List-columns broke CSV
  export with an opaque 500 and were written to Excel as blank columns; they are
  now flattened to `a; b; c` text for flat-file formats (RDS keeps the real
  structure).
- **Mixed Model fit statistics no longer error on a singular fit** —
  `performance::icc()` returns a bare `NA` there, which broke the whole table.
- `R CMD check` is back to `Status: OK`: the local `.claude/` directory is no
  longer swept into the build.

## Docs

- Corrected "five apps" to six, generalised Windows-only install paths, refreshed
  the test-count figure, and split the environment notes into Windows and macOS
  sections (the PowerShell-only workaround is specific to the Windows dev box).
- The test suite grew from ~666 to ~804 expectations across these changes.

# foxplots 0.5.0

## Map Tool: layer groups, color scales, bulletproof export

- **Group layers by any column.** Pick a column (county, treatment, species —
  up to 12 levels) and each level becomes a toggleable layer with an
  ArcGIS-style checkbox list, **clustering that never mixes groups**, and a
  "Focus on group" zoom. With no color chosen, layers auto-color by the group
  so the map and legend stay readable.
- **Log and quantile color scales.** Skewed values (totals, populations) no
  longer wash out: a log scale spreads them (legend still reads in original
  units), and a quantile scale guarantees every color bin gets used. Both fall
  back to linear — with an explanation — when the data can't support them.
- **The HTML download is now ONE self-contained file, always.** No more zip
  fallback: the app inlines every script, stylesheet, and icon itself, so the
  download opens anywhere with no pandoc and no sidecar `_files` folder.
- **PNG snapshots actually work now.** Root cause found: the example datasets'
  fixed `set.seed()` made the whole session deterministic — including the
  headless browser's "random" debugging-port draw, which then hit the same
  blocked port on every attempt ("Cannot find an available port"). All fixed
  seeds now restore the session RNG stream (`snapshot_rng()`), snapshots use
  the modern headless mode (current Chrome/Edge dropped the old one), and a
  failed launch retries once with a fresh browser before reporting
  diagnostics.
- The generated leaflet code mirrors all of the above (grouped maps emit a
  readable per-group loop) and still runs standalone.

## Fixes

- **Import row-filter values are no longer capped.** The "is any of" value
  picker used a client-side list that silently stopped rendering options on
  high-cardinality columns; it now loads server-side, so every distinct value
  is available and searchable.
- Dropped the `zip` package from Suggests (the HTML fallback that needed it
  is gone).

# foxplots 0.4.0

## New app: Map Tool (`run_map_tool()`)

A sixth app — **Import → Map → Export** — that puts any table with latitude /
longitude columns on an interactive leaflet basemap, point-and-click:

- **Coordinate auto-detection.** Common column names (`lat`, `long`,
  `longitude`, …) are found and range-checked automatically, with a one-click
  swap link and a warning when the two look reversed. The 0–360 longitude
  convention (e.g. `quakes`) is accepted.
- **ArcGIS-style styling.** Four key-free basemaps (light / streets / satellite /
  terrain), color-by-column with a legend that always matches the palette
  (the same palette set as Visualize), **area-proportional bubble sizing**,
  opacity, click popups, and hover labels.
- **Clustering for dense data.** Nearby markers group into expandable bubbles —
  automatic above 500 points, or forced on / off.
- **The view survives styling.** Pan / zoom is preserved across setting changes;
  a "Zoom to data" button flies back to the points.
- **Download or reproduce.** One-click **interactive HTML** (self-contained via
  pandoc, or a zip fallback) and **PNG snapshot** (via headless Chrome/Edge when
  available), plus copy-ready `leaflet` R code that reproduces the map.
- Ships two examples: synthetic **Florida research sites**
  (`make_map_example_data()`, exported) and the **Fiji earthquakes** data.

The full Data Explorer gains the same stage as a **Map tab** between Visualize
and Compare Groups.

New dependencies: `leaflet`, `htmlwidgets`, `htmltools` move into Imports;
`webshot2`, `chromote`, `zip` join Suggests (PNG snapshots and the no-pandoc
HTML fallback degrade gracefully when absent).

## Fixes / internals

- **Faceting a line chart or a bar-chart-with-Y no longer errors.** The
  aggregation behind those two chart types dropped the facet column before
  `facet_wrap()` looked for it; the facet variable now joins the grouping (in
  the rendered chart and in the exported code), and the >30-level facet cap is
  applied once up front.
- **Mixed Model Review messages read correctly again.** Ten user-facing strings
  had lost the backslash from their unicode escapes and printed literal digits
  ("effect 2014 without…"); the em dash / minus / plus-minus glyphs are back.
- **Hexbin chart degrades gracefully.** The chart type is only offered when the
  optional `hexbin` package is installed, and the render paths show a friendly
  install hint instead of a raw error.

# foxplots 0.3.0

## New app: Compare Groups (`run_compare_groups()`)

The Compare Groups stage now also ships as its own mini-app — **Import → Compare
Groups → Export → Report** — for users who only want group comparison and can
skip the reshape / visualize / regression machinery. It is the first mini-app
with a **Report** tab, so a spreadsheet becomes a written-up HTML / Word results
document in a few clicks. Ships `iris` and `mtcars` as built-in examples. The
full Data Explorer keeps the tab exactly as before.

## Compare Groups: test many variables at once

- **Grid testing.** The outcome and grouping pickers are now multi-selects: pick
  several of each and every outcome x group combination is tested (up to 12).
  Results arrive as an **at-a-glance summary table** — one row per combination
  with the test, statistic, p-value, effect size and magnitude — above a
  **collapsible full write-up per combination** (means, ANOVA table, post-hoc,
  letters, plots).
- **Multiple-testing correction.** Because the grid *is* the family of tests, the
  summary carries a `p_adj` column adjusted across all combinations, with a
  method selector (Benjamini-Hochberg FDR by default, plus Bonferroni / Holm /
  None).

## Compare Groups: Steel-Dwass + more

- **Steel-Dwass all-pairs test** (Dwass-Steel-Critchlow-Fligner) added alongside
  Dunn's for non-parametric 3+ group comparisons, selectable in the sidebar
  (Dunn's remains the default). Unlike Dunn's single joint ranking, Steel-Dwass
  ranks each pair on its own and controls the family-wise error rate through the
  studentized range — the rank analogue of Tukey HSD, and what JMP calls
  "All Pairs, Steel-Dwass".
- **Connecting letters on the non-parametric path.** Kruskal-Wallis comparisons
  now get a letters report too, from whichever all-pairs method is selected.
- **Contingency-table percentages.** The chi-square mode gained optional **row %,
  column % and total %** tables (tick the ones you want); they flow through to the
  `.txt` download and the report.
- Letters are now relabelled to run a, b, c... down the rows in descending-mean
  order, so a grouping never reads as a scrambled `b, c, a`.

## Fixes / internals

- The post-hoc label was previously inferred by pattern-matching the test name,
  which became wrong once Kruskal-Wallis could carry either Dunn's or
  Steel-Dwass. Results now carry an explicit `posthoc_name`, used everywhere.
- `reportUI()` / `reportServer()` gained a `default_title` argument, so an app
  other than the Data Explorer no longer defaults to "Data Explorer Report".
- Removed the dead `group_stats_table()` helper (superseded by `oneway_means()`).

# foxplots 0.2.0

## Compare Groups — deeper one-way analysis (JMP "Oneway" parity)

The Compare Groups tab (numeric-outcome-by-group) gained a stack of new output,
all wired through to the `.txt` download and the HTML / Word Report:

- **Group means table** with standard error and a 95% confidence interval
  (pooled error for ANOVA, per-group otherwise) — now shown on screen, not just
  in the export.
- **Full ANOVA table** (Between / Within / Total: Df, Sum Sq, Mean Sq, F, p) and
  a **Summary of Fit** (R-squared, adjusted R-squared, RMSE).
- **Welch's ANOVA** reported alongside the classic ANOVA, highlighted when
  Levene's test flags unequal variances.
- **Connecting-letters report** (Tukey compact-letter display) plus a
  **means-with-letters plot** showing which groups differ at a glance.
- **Non-parametric parity**: Dunn's post-hoc test after Kruskal-Wallis, and rank
  effect sizes (rank-biserial *r* for Wilcoxon, epsilon-squared for Kruskal).
- Chi-square mode now shows **expected counts** and **standardized residuals**
  (cells with |z| > 2 flag the drivers of the association).

All additions reuse existing package dependencies (no new imports).

## Fixes

- **Export tab:** the "Summary table (.csv)" button no longer downloads the model
  summary by mistake (a duplicate output id, `dl_summary`, was shadowing it).
- **Report tab:** report generation is now wrapped in error handling, so a
  missing `officer` install or a render failure surfaces a friendly message
  instead of a raw error.

## Polish

- **Mixed Model Review** tab modernized to match the rest of the kit: bslib
  `navset_card_tab` layout, styled buttons (its Bootstrap-3 `btn-default`
  buttons rendered unstyled under Bootstrap 5), the current `DT::DTOutput` /
  `renderDT` API, and inline info-tips on key inputs.
- Consistent DataTables styling (`compact stripe hover`) and download-button
  styling across every module; Regression diagnostic plots now show a friendly
  empty state before a model is fit.
- The Reshape, Combine, and Mixed Model mini-apps gained an **About** landing tab
  matching the Data Explorer.

# foxplots 0.1.0

- First packaged release: the modular toolkit as an installable R package
  (`foxplots`), replacing the single-file monolith.
- Four ready-to-run apps: **Data Explorer** (the full Import -> Reshape ->
  Summarize -> Visualize -> Compare -> Regression -> Export -> Report pipeline),
  **Reshape Tool**, **Combine Tool**, and **Mixed Model Review**.
- Ten Shiny modules on a shared, unit-tested helper foundation; one-click HTML /
  Word report; session save / restore of the data-prep stage.
