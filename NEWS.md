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
