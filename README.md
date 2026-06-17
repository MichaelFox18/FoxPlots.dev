# FoxPlots — UF/IFAS Data Toolkit (modular R Shiny)

A point-and-click toolkit for UF/IFAS students and staff to **import, clean,
reshape, summarize, visualize, compare, model, and export** tabular data — no R
code required. It is built as reusable Shiny **modules** on a shared
**pure-helper** foundation, so the same pieces assemble into several apps.

> Contributor/architecture notes live in `CLAUDE.md`; a dated history is in
> `BUILD_LOG.md`; plain-English run instructions are in `HOW_TO_USE.md`.

---

## Status & roadmap

**Today:** the toolkit is feature-complete and runs as a set of loose files —
three apps, eight modules, and eleven unit-tested helper files. This is the
working app you run right now (see *Running an app* below).

**Just shipped:**

- ✅ **Downloadable report** — a one-click report (Report tab in Data Explorer)
  that auto-assembles everything you produced in a session: data overview,
  summaries, charts, group comparisons, and regression. Choose **HTML** (one
  polished, self-contained file for sharing/archiving) or **Word `.docx`** (fully
  editable — cut sections, add your own intro, notes, or bios). A **"show the R
  code"** toggle switches either between a clean results write-up and a
  reproducible methods document. Both are **pandoc-free**, so they render the
  same from RStudio, VS Code, a bare `Rscript`, or a deployed app.
- ✅ **Save / restore your progress** — on the Import tab, **Save progress**
  downloads a small `.rds` session file; **Restore** re-loads it later to pick up
  where you left off. It captures your data plus all the data-prep work (Data
  Health fixes, type changes, row filters, and the reshape choice).

**Where it's headed next:**

1. **Become an R package (`foxplots`)** — wrap the whole toolkit into an
   installable R package that exports both the underlying helper functions *and*
   launcher functions (e.g. `run_data_explorer()`), so it can be installed once
   and run anywhere. It ships internally first, but is being built so a public
   release stays straightforward later.

The look stays UF/IFAS-themed (blue/orange + the IFAS logo) throughout.

---

## 1. Prerequisites

- **R 4.6+**
- Packages: `shiny`, `bslib`, `DT`, `ggplot2`, `plotly`, `colourpicker`,
  `dplyr`, `tidyr`, `tidyselect`, `readxl`, `writexl`, `here`, `binom`,
  `hexbin` (for the hexbin chart), `officer` (for the Word report), and
  `testthat` (for tests).

Install any you're missing:

```r
install.packages(c("shiny", "bslib", "DT", "ggplot2", "plotly", "colourpicker",
                   "dplyr", "tidyr", "tidyselect", "readxl", "writexl", "here",
                   "binom", "hexbin", "officer", "testthat"))
```

> The **HTML** report needs no extra packages — it is built from base R +
> ggplot2 + `base64enc` (which ships with Shiny), with no pandoc/rmarkdown
> dependency. The **Word (.docx)** report uses `officer` (also pandoc-free); if
> it isn't installed, the HTML option still works.

---

## 2. Running an app

**Always start from the project folder** (the `.here` anchor lets each app find
its shared `R/` and `modules/` files). In an R console:

```r
setwd("C:/path/to/FoxPlots")          # the folder you cloned/downloaded
shiny::runApp("apps/data_explorer")   # then launch any app
```

| Command | App | Use it when you want to… |
|---|---|---|
| `runApp("apps/data_explorer")` | **Data Explorer** | do the whole workflow in one place |
| `runApp("apps/reshape_tool")`  | **Reshape Tool**  | just restructure one table and export it |
| `runApp("apps/combine_tool")`  | **Combine Tool**  | merge / join / compare **two** tables |

Each app opens in a browser tab. Press **Esc** in the R console (or close the
tab) to stop it. The same launch works from RStudio's **Run App** button, the VS
Code R terminal, or `Rscript`. For step-by-step setup in each program, see
`HOW_TO_USE.md`.

---

## 3. The three apps — separately

### Data Explorer — the full pipeline
Tabs run **left → right**, each feeding the next:

1. **Import** — upload CSV / Excel / TSV / RDS (or load a built-in example).
   Then **Data Health** flags common problems (stray text-numbers, blank
   rows/cols, duplicates, whitespace, NA markers, extreme outliers) as opt-in,
   **reversible** fixes; **Change Variable Types** recasts a column (e.g. a
   numeric code → a factor); **Filter rows** keeps only the rows you want; and
   the **profile** and **summary** describe every column. **Save / restore
   session** here downloads (or reloads) a `.rds` capturing your data and all of
   this prep — plus your reshape choice — so you can continue later.
2. **Reshape** — *(optional)* Stack, Split, Transpose, Sort, Subset, or
   **Summarize** (grouped stats as a new table). Defaults to **None
   (pass-through)**, so by default your data flows on unchanged.
3. **Summarize** — count / mean / median / mode / min / max / SD / SE / IQR by
   group, or category **proportions** with confidence intervals.
4. **Visualize** — 1–4 charts side by side (scatter, line, bar, histogram,
   density, box, violin, mean ± error, pie, hexbin, correlation heatmap), with
   styling and a **"copy the ggplot2 code"** panel for each.
5. **Compare Groups** — t-test / ANOVA (or non-parametric) across groups, or a
   chi-square between two categories, with assumption checks and effect sizes.
6. **Regression** — fit linear / multiple / polynomial models with a
   plain-English interpretation and diagnostic plots.
7. **Export** — download the (reshaped) data (CSV/Excel/RDS), the **charts**
   (PNG/PDF), the **summary**, and the **regression** results.
8. **Report** — one click bundles the whole session into a report: **HTML** (a
   single self-contained file) or an editable **Word `.docx`**, with an optional
   "show the R code" toggle.

> **The pipeline is live:** whatever you do on Import + Reshape is the "working
> data" that Summarize, Visualize, Compare Groups, Regression, and Export all
> read. Reshape it and everything downstream follows.

### Reshape Tool — a focused mini-app
**Import → Reshape → Export**, nothing else. Reach for it when you only need to
restructure a single table (the JMP "Tables" single-table operations) and save
the result.

### Combine Tool — for two tables
**Left table → Right table → Combine → Export.** Import (and optionally clean)
two separate tables, then:
- **Concatenate** — stack their rows;
- **Join** — match side by side by a key (left / inner / full / right / cross);
- **Update** — patch values in the left table from the right;
- **Compare** — diff the two tables.

Built-in demo: load `band_members` on the Left and `band_instruments` on the
Right, then **Join by `name`**.

---

## 4. How the apps work together

There is **one** implementation of each feature, shared by every app:

```
R/   (pure helpers, unit-tested)
       helpers_io / helpers_clean / helpers_filter / helpers_stats /
       helpers_plot / helpers_model / helpers_reshape / helpers_combine /
       helpers_compare / components
            ↓ (each module is a thin wrapper that calls these)
modules/   (Shiny modules: mod_import, mod_reshape, mod_summarize,
            mod_visualize, mod_compare, mod_regression, mod_export, mod_combine)
            ↓ (apps assemble modules and wire them together)
apps/   data_explorer · reshape_tool · combine_tool
```

- The **`mod_reshape`** in `reshape_tool` is the *exact same module* as the
  Reshape tab in `data_explorer`. **`mod_import`** / **`mod_export`** are shared
  by all three apps. Fix or improve a module once, and every app gets it.
- Stages connect by **returns-and-arguments**: each module returns its result as
  a reactive, and the app passes that into the next module. (e.g. Import returns
  the data → Reshape takes it and returns the working data → Visualize/Export
  take that.)
- Everything wears the same **UF theme** from `R/components.R`.

---

## 5. For developers

```r
# Run one module in isolation (with built-in sample data):
shiny::runApp("dev/run_reshape.R")
shiny::runApp("dev/run_combine.R")

# Run the test suite (~190 checks over the pure helpers):
testthat::test_dir(here::here("tests/testthat"))
```

To add a feature, follow the path every existing one took: **pure helper + its
test → a thin `mod_*` module that calls it → a `dev/run_*.R` harness → wire it
into an app.** See `CLAUDE.md` for conventions and environment gotchas (notably:
on this machine, run anything that loads Shiny via PowerShell `Rscript.exe`, not
git-bash).
