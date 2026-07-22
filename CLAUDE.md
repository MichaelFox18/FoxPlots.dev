# CLAUDE.md — Data Explorer + Reshape (modular R Shiny)

## What we're building

One project covering two complementary halves of a single workflow: **reshaping** data and **exploring** it. Get data in, reshape it into the form you need, then summarize / visualize / model / export it — one pipeline, built from reusable **Shiny modules** on a **shared foundation**.

The same modules assemble into more than one app: a full **Data Explorer** (the whole pipeline) and focused mini-apps — **Reshape** (import → reshape → export), **Combine** (two-table join/concatenate/update/compare), and **Mixed Model Review** (import → mixed model → export). A module can also be developed on its own via `pkgload::load_all()` + a throwaway `shinyApp()`.

The reshape stage recreates JMP's **Tables menu** (the JMP→R op mapping lives in the reshape/combine helper roxygen and `BUILD_LOG.md`). **Status: the original roadmap is complete** — the modular kit matches the old monolith feature-for-feature. See "Current state" below.

**Scope note.** The educational/learning tools (p-value visualizer, regression learning tool) are a **separate project** and are not part of this repo. If you later want them to share this project's look, `R/components.R` can be promoted to a small package both repos depend on — but that's out of scope here.

Why modular: the existing `DataExplorerApp.R` is ~2,776 lines in a single namespace (58 inputs, 34 outputs, zero modules). The *logic* is already cleanly factored into helper functions; it's the UI/server *wiring* that's tangled. Modules give each feature its own namespace, make that wiring composable, and let reshaping live both as a stage inside the Explorer and as its own standalone app.

---

## Current state

It is now an installable R **package** (`foxplots`); the original roadmap is done. `R CMD check` is clean (`Status: OK`). What exists (all under `R/`):

- **Six apps as launchers**: `run_data_explorer()` (full pipeline: About → Import → Reshape → Summarize → Visualize → Map → Compare Groups → Regression → Export → Report), `run_reshape_tool()` (import → reshape → export), `run_combine_tool()` (import two tables → combine → export), `run_compare_groups()` (import → compare → export → report; the only mini-app with a Report tab), `run_lmer_tool()` (import → mixed model → export), `run_map_tool()` (import → interactive leaflet map → export). Each has a `*_app()` builder returning the `shinyApp` object.
- **Eleven modules**: `mod_import` (upload/clean/recast + Data Health incl. **outlier flagging** + **row filter** + profile + **session save/restore**), `mod_reshape` (stack/split/transpose/sort/subset/**summary**), `mod_summarize`, `mod_visualize` (1–4 plots, **11 chart types** incl. bubble, lazy render, code export), `mod_map` (**interactive leaflet point maps**: coordinate auto-detect + swap, 4 basemaps, color/size-by with legend, popups/hover labels, auto-clustering, pan/zoom-preserving rebuilds, HTML/PNG download + leaflet code export — the map downloads live in the module, NOT mod_export, whose chart slot is ggplot-only), `mod_compare` (t-test/ANOVA/Wilcoxon/Kruskal + chi-square; **grid testing of many outcomes x many groups** with a BH-corrected summary + per-combination accordion; assumptions, effect sizes, Tukey/**Dunn**/**Steel-Dwass** post-hoc + connecting letters, table percentages), `mod_regression`, `mod_export` (data + charts + summary + model; optional `preview` arg), `mod_combine` (concatenate/join/update/compare), `mod_report` (one-click HTML **or Word** report of the whole session), `mod_lmer` (**linear mixed models** via lmerTest/emmeans: ANOVA, fit/variance, residuals, **EMMeans + cld**, interaction test, model comparison).
- **Fifteen helper files** (`R/helpers_*` + `components`), all unit-tested. `helpers_lmer.R` holds the mixed-model engine (formula/spec builders, `lmer_fit`/`lmer_emmeans`/`lmer_anova`/`lmer_compare`/`lmer_fit_stats`/`lmer_cook`, `make_example_data`), validated against the source repo's `validate_engine.R` reference values. `helpers_map.R` holds the map engine (`detect_coord_cols`/`clean_coords`/`build_leaflet_map` incl. **layer groups** + **log/quantile color scales**/`generate_map_code`/`map_palette*`/`make_map_example_data` + the exporters: `inline_html_deps` makes the HTML download **self-contained without pandoc** by data-URI-ing every local script/stylesheet/asset, and `save_map_png` retries once with a fresh browser in modern headless mode, with Edge-as-Chrome fallback); its palette tree mirrors `group_scales`/`palette_code` exactly.
- **~51 exported functions** (12 launchers + `uf_logo_uri` + `make_example_data` + `make_map_example_data` + ~36 helpers); internal functions are `@noRd`.
- **Source files must be ASCII.** Non-ASCII in R strings normally uses `\uXXXX` escapes; where a glyph is needed at runtime, build it from code points instead (`CRAMERS_V`, `SYM_CHI2`, `SYM_ALPHA`, `SYM_TIMES` in `helpers_compare.R` use `intToUtf8()`). `CRAMERS_V` must stay identical to `effect_magnitude()`'s switch key.
- **A testthat suite** (`tests/testthat/`, ~610 expectations) run via `R CMD check` / `testthat::test_local()` (Word-report tests `skip_if_not_installed("officer")`; the mixed-model engine tests `skip_if_not_installed("lmerTest"/"emmeans")`). Modules verified with `shiny::testServer` smoke checks (run ad hoc; note `testServer` can segfault here — fall back to building the `*UI()`/`*_app()` objects + booting the app over HTTP).
- **Session save/restore** (`helpers_state.R`): the Import tab can download a versioned `.rds` of the data-prep stage (working data + raw + filters + reshape settings) and restore it. Wired via a shared `session_store` reactiveValues passed to `importServer`/`reshapeServer` — the one sanctioned app-wide-state use of a shared store (reshape *publishes* its settings; a restore *stages* them for the reshape sync-observer to consume).

To extend it, follow the same path every existing feature took: **pure helper + its test → thin module (`mod_*`) that calls it → wire it into a launcher (`run_*.R`) → `roxygenise()` if you exported anything.**

---

## Architecture & conventions

- **A module is two functions** — `<feature>UI(id)` and `<feature>Server(id, ...)` — namespaced with `NS()` / `moduleServer()`. Neither runs until an app calls it.
- **Reshaping is part of the same pipeline, not a separate world.** In the full app the flow is Import → Reshape → Summarize / Visualize / Regression → Export. The `reshape` module is one stage the Explorer includes, and it also ships on its own as `reshape_tool`. Same code, two homes.
- **Wiring is returns-and-arguments (Pattern A).** A module *returns* its result as a reactive; the parent app *passes* that reactive into the next module as an argument. Use a shared `reactiveValues` store **only** for genuine app-wide state, not as a back door between modules.
- **Helper purity is the core rule.** All data logic lives in plain functions in `R/` with no reactivity and no Shiny calls. Modules are thin wrappers that call helpers. Helpers are unit-tested; modules stay testable by staying thin.
- **One menu, two modules.** `reshape` = single-table ops (stack, split, transpose, sort, subset, summary). `combine` = two-table ops (join, concatenate, update, compare). Don't bolt a "second table" slot onto `reshape`.
- **Packaging:** this is now a proper R **package** (`foxplots`). All code lives in `R/` (helpers + `mod_*` modules + `run_*` launchers); the apps are exported builder functions (`data_explorer_app()` etc.) with `run_*()` launchers, not loose `app.R` files. Use `pkgload::load_all()` to develop and `roxygen2::roxygenise()` after changing `@export`/`@param`/`@import` tags. The three `@import shiny/bslib/ggplot2` live in `R/foxplots-package.R`; everything else is called `pkg::fun()`. Resources resolve via `system.file()` (e.g. the logo in `inst/www/`).

---

## Repo layout

It's a standard R package (`foxplots`):

```
.
├── DESCRIPTION                  # package manifest: deps (Imports/Suggests), version, R (>= 4.4)
├── NAMESPACE                    # roxygen-generated: exports + imports — do not edit by hand
├── LICENSE                      # MIT
├── .Rbuildignore                # keeps dev docs (CLAUDE/BUILD_LOG/HOW_TO) out of the build
├── CLAUDE.md / BUILD_LOG.md / HOW_TO_USE.md / README.md / NEWS.md
├── R/                           # ALL package code (helpers + modules + launchers)
│   ├── foxplots-package.R       # "_PACKAGE" + @import shiny/bslib/ggplot2
│   ├── components.R             # UF theme (uf_theme), uf_logo_uri/uf_title, info_tip, label_or, copy_js, UF_BLUE/ORANGE/COLORS
│   ├── helpers_io.R             # file reading / table-bounds detection
│   ├── helpers_clean.R          # Data Health detect/fix engine (incl. outliers) + convert_column
│   ├── helpers_filter.R         # apply_filters / describe_condition (value-based row filter)
│   ├── helpers_stats.R          # grouped_summary, proportions_summary, column classifiers, profiling
│   ├── helpers_plot.R           # build_full_plot (11 chart types), chart hints, palettes, code gen, grid export
│   ├── helpers_model.R          # fit_model, model_interpretation, diagnostic ggplots
│   ├── helpers_reshape.R        # do_stack/do_split/do_transpose/do_sort/do_subset
│   ├── helpers_combine.R        # do_concatenate/do_join/do_update/compare_tables
│   ├── helpers_compare.R        # compare_groups_numeric/compare_categorical, assumptions, effect sizes
│   ├── helpers_report.R         # report_spec + build_report_html / build_report_docx (pandoc-free HTML + editable Word) + render_report
│   ├── helpers_state.R          # build/validate/summarize a session + save/load (.rds save-restore)
│   ├── helpers_lmer.R           # mixed-model engine: formula/emm builders, lmer_fit/_emmeans/_anova/_compare/_fit_stats/_cook, make_example_data
│   ├── helpers_map.R            # map engine: coord detect/clean, build_leaflet_map, generate_map_code, palettes, make_map_example_data, HTML/PNG export
│   ├── mod_import.R … mod_report.R   # the eleven Shiny modules (<feature>UI/<feature>Server), incl. mod_lmer + mod_map
│   ├── run_data_explorer.R      # data_explorer_app() builder + run_data_explorer() launcher
│   ├── run_reshape_tool.R       # reshape_tool_app()  + run_reshape_tool()
│   ├── run_combine_tool.R       # combine_tool_app()  + run_combine_tool()
│   ├── run_compare_groups.R     # compare_groups_app() + run_compare_groups()
│   ├── run_lmer_tool.R          # lmer_tool_app()     + run_lmer_tool()
│   └── run_map_tool.R           # map_tool_app()      + run_map_tool()
├── inst/
│   ├── www/IFAS-White.png       # logo, found via system.file(); inlined as a data URI
│   └── apps/<name>/app.R        # thin deploy entries (call foxplots::<name>_app())
├── man/                         # roxygen-generated help pages (only exported fns)
└── tests/
    ├── testthat.R               # test runner (test_check)
    └── testthat/                # setup.R (no longer sources R/); test-<area>.R per helper
        ├── test-reshape.R  test-io.R  test-stats.R  test-plot.R  test-report.R
        ├── test-model.R    test-clean.R  test-combine.R  test-compare.R  test-filter.R
        └── test-state.R    test-lmer.R   test-map.R
```

---

## Tech stack

R + `shiny`, `bslib` (layout: `page_navbar`, `layout_sidebar`, `card`, `navset_card_tab` — matches the existing app), `DT`, `tidyr`, `dplyr`, `tidyselect`, `ggplot2`, `plotly` (interactive chart + regression-diagnostic previews via `ggplotly()`), `colourpicker`, `binom` (exact CIs), `base64enc` / `tibble`, `hexbin` (the hexbin chart, Suggests-gated), `writexl` / `readxl` (import/export), `officer` (the editable Word report — pandoc-free), `here`, and `testthat` for tests. The **Mixed Model Review** app adds the mixed-model stack: `lmerTest` (loads `lme4`), `emmeans`, `multcomp` + `multcompView` (the cld letter engine), and `lattice` (the random-effects caterpillar) in Imports; `pbkrtest` (Kenward-Roger df), `performance` / `MuMIn` (R²/ICC), and `influence.ME` are optional (Suggests, graceful `requireNamespace` fallback). The **Map Tool** adds `leaflet` + `htmlwidgets` + `htmltools` (Imports; all leaflet calls `pkg::`-qualified, no `@import`) with `webshot2` / `chromote` (PNG snapshots; needs a system Chrome — falls back to Edge via `CHROMOTE_CHROME`, retries in `CHROMOTE_HEADLESS=new` mode) in Suggests; the HTML download needs nothing external (`inline_html_deps` self-contains it, no pandoc). Add packages as needed and note them in the build log.

---

## Code style

- Use `bslib` components for layout; keep the look consistent with the existing app.
- **Naming:** modules `mod_<feature>.R` exporting `<feature>UI()` / `<feature>Server()`; helpers `do_<verb>()` for transforms (`do_stack`, `do_split`) and `<noun>_<verb>()` for utilities.
- **One theme, one source of truth.** `R/components.R` is the canonical look: `uf_theme()` (flatly + UF-blue navbar), the logo title, `info_tip()`, `label_or()`, and `copy_js`. UF colors live there as `UF_BLUE`/`UF_ORANGE`/`UF_COLORS`. **Blue is `#003087`** (matches the original app; the early plan floated `#0021A5` — change the one constant in `components.R` if the brand guide says otherwise). Every app pulls its theme from here.
- Round any number shown to the user. Guard reactive reads with `req()`. Namespace every input/output id with `ns()` — name collisions are the failure mode modules exist to prevent.
- App state lives in reactives, not files or globals.

---

## Dev workflow

Work from the package root (`DESCRIPTION` anchors `here::here()`). Develop with `pkgload::load_all()` — no install needed:

- **Develop:** `pkgload::load_all("."); run_data_explorer()` (or `run_reshape_tool()` / `run_combine_tool()`).
- **One module alone:** `pkgload::load_all("."); shiny::testServer(visualizeServer, args = list(data_in = reactive(mtcars)), { ... })` — or build its `*UI()`/`*Server()` into a throwaway `shinyApp()`.
- **Regenerate docs/NAMESPACE** after changing `@export`/`@param`/`@import`: `roxygen2::roxygenise()`.
- **Tests:** `testthat::test_local(".")` (loads the package; internal functions are reachable from tests). Full gate: `R CMD build .` then `R CMD check foxplots_*.tar.gz`.
- **Add a feature:** write the `do_*`/helper + its test → thin `mod_*` that calls it → wire it into a launcher (`run_*.R`) → `roxygenise()` if you exported anything.

**Environment gotchas (Windows dev box):**
- **Run anything that loads the package (load_all, roxygenise, `R CMD check`, app launch) via PowerShell `R.exe`/`Rscript.exe`, not git-bash** — Shiny/bslib segfault under git-bash there. Pure text/`testthat`-helper runs are fine in either.
- **Build/check/install are PowerShell:** `R.exe CMD build .`, `R.exe CMD check --no-manual <tarball>`, `R.exe CMD INSTALL .`. `devtools`/`usethis` are NOT installed; `roxygen2` + `pkgload` are.

**Environment notes (macOS dev box — current, since 2026-07-21):**
- macOS/arm64, R 4.5.1. **None of the PowerShell workarounds above apply**: plain `Rscript` in any shell works, and Shiny/bslib do not segfault. Build/check are plain `R CMD build .` / `R CMD check --no-manual <tarball>`.
- **`shiny::testServer` works here** (it segfaulted on the Windows box, which is why module coverage is thin). Read a module's return with `session$returned()`.
- **No XQuartz**, and `capabilities("cairo")` **lies** — it reports build-time support while `cairo.so` fails to load. So `cairo_pdf()`/`svg()` open no device and write nothing, silently. `render_plots_to_file()` therefore uses base `grDevices::pdf()` and probes `dev.cur()` instead of trusting `capabilities()`; never reintroduce a `capabilities("cairo")` gate.
- No pandoc and no quarto — fine, the reports are pandoc-free by design. Chrome is present, so map PNG snapshots work.

**Both platforms:**
- New non-ASCII in **R strings** must use `\uXXXX` escapes (portability); keep comments ASCII. R CMD check must stay clean (currently `Status: OK`).
- Exported functions need full `@param`/`@return`; mark internal documented helpers `@noRd`. A string token that also appears quoted in a roxygen comment can leak a `\u` into the `.Rd` (Rd "unknown macro") — keep doc text plain ASCII.
- Module `conditionalPanel` conditions must use **bracket notation** with `ns()` — `input['<ns-id>']` — because namespaced ids contain a hyphen (dot notation breaks).
- Wrap multi-line `if/else` **function bodies** in `{}` or it fails to parse with "unexpected 'else'".
- `session$returnValue()` is not a method in this Shiny — to read a module's return in `testServer`, name the returned reactive and call it (or call the internal function directly in the test expr).

---

## Reference helper — match this pattern

`do_stack` is the house style for every helper: pure, validated, documented, and tested. Build every new helper (e.g. `do_split`, which wraps `tidyr::pivot_wider`) to mirror it.

```r
#' Stack columns into a label/value pair
#'
#' JMP Tables > Stack. Wraps tidyr::pivot_longer: collapses several columns
#' into one value column plus a label column recording the source column.
#'
#' @param data A data frame.
#' @param cols Character vector of column names to stack.
#' @param label_to Name for the new column holding the source column names.
#' @param value_to Name for the new column holding the stacked values.
#' @return A tibble in long form.
#' @examples
#' do_stack(data.frame(id = 1:2, q1 = c(5, 2), q2 = c(3, 4)), c("q1", "q2"))
do_stack <- function(data, cols, label_to = "Label", value_to = "Data") {
  stopifnot(is.data.frame(data), is.character(cols), length(cols) >= 1L)
  missing <- setdiff(cols, names(data))
  if (length(missing)) {
    stop("Columns not found in data: ", paste(missing, collapse = ", "))
  }
  tidyr::pivot_longer(
    data,
    cols      = tidyselect::all_of(cols),
    names_to  = label_to,
    values_to = value_to
  )
}
```

Its test (every `do_*` helper gets one like this):

```r
test_that("do_stack collapses selected columns into label/value", {
  df  <- data.frame(id = 1:2, q1 = c(5, 2), q2 = c(3, 4))
  out <- do_stack(df, c("q1", "q2"))
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("id", "Label", "Data"))
  expect_equal(nrow(out), 4L)
  expect_setequal(unique(out$Label), c("q1", "q2"))
})

test_that("do_stack errors on unknown columns", {
  expect_error(do_stack(data.frame(a = 1), c("a", "nope")), "not found")
})
```

---

## Roadmap (all complete)

1. ✅ **`reshape` module** — stack/split, then transpose/sort/subset.
2. ✅ **`R/components.R`** — one UF theme + shared UI atoms, both apps pull from it.
3. ✅ **`combine` module** — concatenate / join / update / compare, `combineServer(id, left, right)` + `combine_tool`.
4. ✅ **Monolith → modules** — full `data_explorer` (Import w/ Data Health, Reshape, Summarize, Visualize, Regression, Export w/ chart + model downloads).
5. ✅ **Packaged** — `R/` + `modules/` are now a proper installable R package (`foxplots`); apps are exported builder functions with `run_*()` launchers.

### Possible next steps (not yet done)
- **Full app-state save/restore.** Session save/restore (`helpers_state.R`) currently captures only the **data-prep stage** (working data + raw + filters + reshape settings) — analysis-tab choices are not saved. A future upgrade would also restore the **Visualize chart configs, Summarize selections, Regression model spec, and Compare Groups settings** so reloading a session lands the user back on every tab exactly as they left it. The plumbing already exists: extend the shared `session_store` pattern (each analysis module *publishes* its inputs to the store and *consumes* a staged restore, the way `mod_reshape` already does) to `mod_visualize` / `mod_summarize` / `mod_regression` / `mod_compare`. Watch the same picker repopulate-vs-restore race handled in `mod_reshape` (set choices + selection together). Bump `SESSION_STATE_VERSION` when the saved structure grows. (Deliberately deferred — the data-prep scope was chosen first; results are already preservable via the Report.)
- Deploy (shinyapps.io / Connect): apps are written deployment-safe (system.file/here-anchored paths, inlined logo). The thin entries in `inst/apps/<name>/app.R` call `foxplots::<name>_app()`, so a server deploy needs the `foxplots` package installed there (it is not on CRAN — install from GitHub). Current distribution is **local install** (`install_github("UFSDACU/FoxPlots")` / `install_local`); no rsconnect tooling is committed yet.
