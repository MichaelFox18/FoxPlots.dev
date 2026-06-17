# foxplots — UF/IFAS Data Toolkit (R package)

A point-and-click toolkit for UF/IFAS students and staff to **import, clean,
reshape, summarize, visualize, compare, model, and export** tabular data — no R
code required. It is an installable **R package** built from reusable Shiny
**modules** on a shared, unit-tested **helper** foundation.

> Contributor/architecture notes live in `CLAUDE.md`; plain-English run
> instructions are in `HOW_TO_USE.md`.

---

## Status

`foxplots` is an **installable R package**. It bundles the whole toolkit — three
Shiny apps, the modules behind them, and a tested helper foundation — and exports
both **launcher functions** (`run_data_explorer()`, `run_reshape_tool()`,
`run_combine_tool()`) and the underlying **helper API** (`do_stack()`,
`grouped_summary()`, `fit_model()`, …). Install it once and run anywhere.

Highlights:
- **One-click report** — HTML or editable Word (`.docx`), pandoc-free, with a
  "show the R code" toggle.
- **Save / restore your progress** — a `.rds` of your data + all data prep.
- **11 chart types** (incl. a bubble option), group comparisons, regression, and
  a reversible Data Health cleaner with outlier flagging.

The look stays UF/IFAS-themed (blue/orange + the IFAS logo) throughout.

---

## 1. Install & run

You need **R 4.4+** (and optionally RStudio). Install with `remotes`, which pulls
in every dependency automatically:

```r
install.packages("remotes")
remotes::install_local("C:/path/to/FoxPlots")   # the folder you downloaded
# ...or, with repo access:  remotes::install_github("UFSDACU/FoxPlots")
```

Then launch any app:

```r
library(foxplots)
run_data_explorer()      # or run_reshape_tool() / run_combine_tool()
```

| Launcher | App | For… |
|---|---|---|
| `run_data_explorer()` | **Data Explorer** | the whole workflow in one place |
| `run_reshape_tool()`  | **Reshape Tool**  | restructuring one table |
| `run_combine_tool()`  | **Combine Tool**  | merging / joining / comparing two tables |

The package also exports the helper functions for use in your own scripts (e.g.
`do_stack()`, `grouped_summary()`, `build_full_plot()`); see
`help(package = "foxplots")`. Step-by-step setup is in `HOW_TO_USE.md`.

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
   styling and a **"copy the ggplot2 code"** panel for each. A scatter can be
   **sized by a variable** to make a bubble chart.
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
