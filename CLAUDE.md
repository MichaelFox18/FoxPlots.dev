# CLAUDE.md — Data Explorer + Reshape (modular R Shiny)

## What we're building

One project covering two complementary halves of a single workflow: **reshaping** data and **exploring** it. Get data in, reshape it into the form you need, then summarize / visualize / model / export it — one pipeline, built from reusable **Shiny modules** on a **shared foundation**.

The same modules assemble into more than one app: a full **Data Explorer** (the whole pipeline) and a focused **reshape mini-app** (import → reshape → export), plus isolated dev harnesses for building a module on its own.

The reshape stage recreates JMP's **Tables menu**; the full JMP→R mapping is in `docs/JMP_Tables_Menu_to_R.md` (the spec). **Status: the original roadmap is complete** — the modular kit matches the old monolith feature-for-feature. See "Current state" below.

**Scope note.** The educational/learning tools (p-value visualizer, regression learning tool) are a **separate project** and are not part of this repo. If you later want them to share this project's look, `R/components.R` can be promoted to a small package both repos depend on — but that's out of scope here.

Why modular: the existing `DataExplorerApp.R` is ~2,776 lines in a single namespace (58 inputs, 34 outputs, zero modules). The *logic* is already cleanly factored into helper functions; it's the UI/server *wiring* that's tangled. Modules give each feature its own namespace, make that wiring composable, and let reshaping live both as a stage inside the Explorer and as its own standalone app.

---

## Current state

The kit is built and the original roadmap is done. What exists:

- **Three runnable apps** (`apps/`): `data_explorer` (the full pipeline: About → Import → Reshape → Summarize → Visualize → Compare Groups → Regression → Export → Report), `reshape_tool` (import → reshape → export), and `combine_tool` (import two tables → combine → export).
- **Nine modules** (`modules/`): `mod_import` (upload/clean/recast + Data Health + **row filter** + profile), `mod_reshape` (stack/split/transpose/sort/subset), `mod_summarize`, `mod_visualize` (1–4 plots, 7 chart types, code export), `mod_compare` (t-test/ANOVA/Wilcoxon/Kruskal + chi-square, assumptions, effect sizes), `mod_regression`, `mod_export` (data + charts + summary + model), `mod_combine` (concatenate/join/update/compare), `mod_report` (one-click self-contained HTML report of the whole session).
- **Twelve pure helper files** (`R/`), all unit-tested — see layout below.
- **A testthat suite** (`tests/testthat/`, ~244 expectations) covering every `do_*`/helper. Modules are verified with `shiny::testServer` smoke checks (not committed; run ad hoc).

To extend it, follow the same path every existing feature took: **pure helper + its test → thin module (`mod_*`) that calls it → a `dev/run_*.R` harness → wire it into an app.**

---

## Architecture & conventions

- **A module is two functions** — `<feature>UI(id)` and `<feature>Server(id, ...)` — namespaced with `NS()` / `moduleServer()`. Neither runs until an app calls it.
- **Reshaping is part of the same pipeline, not a separate world.** In the full app the flow is Import → Reshape → Summarize / Visualize / Regression → Export. The `reshape` module is one stage the Explorer includes, and it also ships on its own as `reshape_tool`. Same code, two homes.
- **Wiring is returns-and-arguments (Pattern A).** A module *returns* its result as a reactive; the parent app *passes* that reactive into the next module as an argument. Use a shared `reactiveValues` store **only** for genuine app-wide state, not as a back door between modules.
- **Helper purity is the core rule.** All data logic lives in plain functions in `R/` with no reactivity and no Shiny calls. Modules are thin wrappers that call helpers. Helpers are unit-tested; modules stay testable by staying thin.
- **One menu, two modules.** `reshape` = single-table ops (stack, split, transpose, sort, subset, summary). `combine` = two-table ops (join, concatenate, update, compare). Don't bolt a "second table" slot onto `reshape`.
- **Packaging:** loose files for now; convert to an R package once `R/` stabilizes. Until then, apps `source()` `R/` and `modules/` explicitly — anchor paths with `here::here()` or run from the repo root.

---

## Repo layout

```
.
├── CLAUDE.md
├── BUILD_LOG.md                 # append a dated entry every session (required)
├── .here                        # anchors here::here() to projects/ (not the outer .git repo)
├── www/IFAS-White.png           # logo, inlined as a data URI by components.R
├── docs/
│   └── JMP_Tables_Menu_to_R.md  # the spec — JMP op → R mapping, reshape/combine split
├── R/                           # shared foundation, sourced by every app — PURE functions only
│   ├── components.R             # UF theme (uf_theme), uf_logo_uri/uf_title, info_tip, label_or, copy_js, UF_BLUE/ORANGE/COLORS
│   ├── helpers_io.R             # file reading / table-bounds detection
│   ├── helpers_clean.R          # Data Health detect/fix engine + convert_column
│   ├── helpers_filter.R         # apply_filters / describe_condition (value-based row filter)
│   ├── helpers_stats.R          # grouped_summary, column classifiers, profiling
│   ├── helpers_plot.R           # build_full_plot, chart hints, palettes, code gen, grid export
│   ├── helpers_model.R          # fit_model, model_interpretation, diagnostic ggplots
│   ├── helpers_reshape.R        # do_stack/do_split/do_transpose/do_sort/do_subset
│   ├── helpers_combine.R        # do_concatenate/do_join/do_update/compare_tables
│   ├── helpers_compare.R        # compare_groups_numeric/compare_categorical, assumptions, effect sizes
│   └── helpers_report.R         # report_spec + build_report_html (pandoc-free self-contained HTML) + render_report
├── modules/                     # mod_<feature>.R, each <feature>UI(id) + <feature>Server(id, ...)
│   ├── mod_import.R   mod_reshape.R   mod_summarize.R   mod_visualize.R
│   ├── mod_compare.R   mod_regression.R   mod_export.R   mod_combine.R   mod_report.R
├── apps/
│   ├── data_explorer/app.R      # full pipeline (all tabs, incl. Report)
│   ├── reshape_tool/app.R       # import → reshape → export
│   └── combine_tool/app.R       # import two tables → combine → export
├── dev/                         # boot ONE module alone with sample data
│   ├── run_reshape.R
│   ├── run_combine.R
│   ├── run_compare.R
│   └── run_report.R
└── tests/testthat/              # setup.R sources R/; test-<area>.R per helper file
    ├── test-reshape.R  test-io.R  test-stats.R  test-plot.R  test-report.R
    ├── test-model.R    test-clean.R  test-combine.R  test-compare.R  test-filter.R
```

---

## Tech stack

R + `shiny`, `bslib` (layout: `page_navbar`, `layout_sidebar`, `card` — matches the existing app), `DT`, `tidyr`, `dplyr`, `tidyselect`, `ggplot2`, `writexl` / `readxl` / `readr` (import/export), `here`, and `testthat` for tests. Add packages as needed and note them in the build log.

---

## Code style

- Use `bslib` components for layout; keep the look consistent with the existing app.
- **Naming:** modules `mod_<feature>.R` exporting `<feature>UI()` / `<feature>Server()`; helpers `do_<verb>()` for transforms (`do_stack`, `do_split`) and `<noun>_<verb>()` for utilities.
- **One theme, one source of truth.** `R/components.R` is the canonical look: `uf_theme()` (flatly + UF-blue navbar), the logo title, `info_tip()`, `label_or()`, and `copy_js`. UF colors live there as `UF_BLUE`/`UF_ORANGE`/`UF_COLORS`. **Blue is `#003087`** (matches the original app; the early plan floated `#0021A5` — change the one constant in `components.R` if the brand guide says otherwise). Every app pulls its theme from here.
- Round any number shown to the user. Guard reactive reads with `req()`. Namespace every input/output id with `ns()` — name collisions are the failure mode modules exist to prevent.
- App state lives in reactives, not files or globals.

---

## Dev workflow

Run everything from the `projects/` root (the `.here` anchor makes `here::here()` resolve here):

- **Full app:** `shiny::runApp("apps/data_explorer")`  ·  **mini-apps:** `runApp("apps/reshape_tool")`, `runApp("apps/combine_tool")`
- **One module alone:** `shiny::runApp("dev/run_reshape.R")` / `runApp("dev/run_combine.R")`
- **Tests:** `testthat::test_dir(here::here("tests/testthat"))`
- **Add a feature:** write the `do_*`/helper + its test → thin `mod_*` that calls it → a `dev/run_*.R` harness → wire it into an app.

**Environment gotchas (this machine):**
- **Run anything that loads Shiny via PowerShell `Rscript.exe`, not git-bash** — Shiny/bslib segfault under git-bash here. Pure-helper / `testthat` runs are fine in either.
- Module `conditionalPanel` conditions must use **bracket notation** with `ns()` — `input['<ns-id>']` — because namespaced ids contain a hyphen (dot notation breaks).
- Wrap multi-line `if/else` **function bodies** in `{}` or sourcing fails with "unexpected 'else'".
- `session$returnValue()` is not a method in this Shiny — to read a module's return in `testServer`, name the returned reactive and call it.

---

## Reference helper — match this pattern

`do_stack` is the house style for every helper: pure, validated, documented, and tested. Build `do_split` (and the rest) to mirror it, using the R mappings in the spec (`do_split` wraps `tidyr::pivot_wider`).

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

1. ✅ **`reshape` module** — stack/split, then transpose/sort/subset; `dev/run_reshape.R`.
2. ✅ **`R/components.R`** — one UF theme + shared UI atoms, both apps pull from it.
3. ✅ **`combine` module** — concatenate / join / update / compare, `combineServer(id, left, right)`; `dev/run_combine.R` + `combine_tool`.
4. ✅ **Monolith → modules** — full `data_explorer` (Import w/ Data Health, Reshape, Summarize, Visualize, Regression, Export w/ chart + model downloads).

### Possible next steps (not yet done)
- Convert `R/` + `modules/` into an actual R **package** now that the foundation is stable.
- A reshape "Summary" op (grouped-summary-as-a-new-table) — currently that logic lives in `mod_summarize`; decide whether it also belongs in `reshape`.
- Deploy (shinyapps.io / Connect): apps are written deployment-safe (here-anchored paths, inlined logo), but bundling each `apps/<name>/` needs the sibling `R/` + `modules/` shipped alongside.
