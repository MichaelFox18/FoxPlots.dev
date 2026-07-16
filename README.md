# foxplots — UF/IFAS Data Toolkit (R package)

A point-and-click toolkit for UF/IFAS students and staff to **import, clean,
reshape, summarize, visualize, compare, model, and export** tabular data — no R
code required. It is an installable **R package** built from reusable Shiny
**modules** on a shared, unit-tested **helper** foundation.

> Contributor/architecture notes live in `CLAUDE.md`; plain-English run
> instructions are in `HOW_TO_USE.md`.

---

## Status

`foxplots` is an **installable R package**. It bundles the whole toolkit — six
Shiny apps, the modules behind them, and a tested helper foundation — and exports
both **launcher functions** (`run_data_explorer()`, `run_reshape_tool()`,
`run_combine_tool()`, `run_compare_groups()`, `run_lmer_tool()`,
`run_map_tool()`) and the
underlying **helper API** (`do_stack()`, `grouped_summary()`, `fit_model()`, …).
Install it once and run anywhere.

Highlights:
- **One-click report** — HTML or editable Word (`.docx`), pandoc-free, with a
  "show the R code" toggle.
- **Save / restore your progress** — a `.rds` of your data + all data prep.
- **11 chart types** (incl. a bubble option), regression, and a reversible Data
  Health cleaner with outlier flagging.
- **JMP-style one-way analysis** — group means with SE/95% CI, a full ANOVA table
  and R-squared, Welch's ANOVA, **connecting letters**, and non-parametric parity
  (Dunn's / **Steel-Dwass** all-pairs, rank effect sizes). Test **many outcomes
  against many groups at once** and see every combination in one table.

The look stays UF/IFAS-themed (blue/orange + the IFAS logo) throughout.

---

## 1. Install & run

You need **R 4.4+** (and optionally RStudio). Install with `remotes`, which pulls
in every dependency automatically:

```r
install.packages("remotes")

# Install straight from GitHub (the usual way):
remotes::install_github("UFSDACU/FoxPlots")

# ...or from a local copy (on the GitHub page: Code -> Download ZIP, unzip, then):
# remotes::install_local("C:/path/to/FoxPlots")
```

> If this repository is **private**, `install_github()` needs a GitHub token in R
> (`usethis::create_github_token()` then `gitcreds::gitcreds_set()`). To skip the
> token, use the **Download ZIP** + `install_local()` route above. If the repo is
> public, `install_github()` just works.

Then launch any app:

```r
library(foxplots)
run_data_explorer()      # or run_reshape_tool() / run_combine_tool() /
                         #    run_compare_groups() / run_lmer_tool() /
                         #    run_map_tool()
```

| Launcher | App | For… |
|---|---|---|
| `run_data_explorer()` | **Data Explorer** | the whole workflow in one place |
| `run_reshape_tool()`  | **Reshape Tool**  | restructuring one table |
| `run_combine_tool()`  | **Combine Tool**  | merging / joining / comparing two tables |
| `run_compare_groups()` | **Compare Groups** | testing whether groups differ (t-test / ANOVA / rank tests / chi-square) |
| `run_lmer_tool()`     | **Mixed Model Review** | fitting linear mixed models (lmer) |
| `run_map_tool()`      | **Map Tool**      | putting lat/lon data on an interactive map (leaflet) |

The package also exports the helper functions for use in your own scripts (e.g.
`do_stack()`, `grouped_summary()`, `build_full_plot()`); see
`help(package = "foxplots")`. Step-by-step setup is in `HOW_TO_USE.md`.

---

## 2. The five apps — separately

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
5. **Compare Groups** — a JMP-style one-way analysis: t-test / ANOVA (or
   non-parametric Wilcoxon / Kruskal with **Dunn's or Steel-Dwass** all-pairs)
   across groups, with a group-means table (SE + 95% CI), the full ANOVA table
   and R-squared, Welch's ANOVA, a means plot with **connecting letters**, and
   assumption checks + effect sizes. Pick **several outcomes and several grouping
   variables** to test every combination at once — a summary table (with p-values
   corrected across the whole set) plus a collapsible full write-up per
   combination. Or a chi-square between two categories, with expected counts,
   standardized residuals and optional row / column / total percentages.
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

### Compare Groups — do these groups actually differ?

**Import → Compare Groups → Export → Report.** The same Compare Groups stage the
Data Explorer has, on its own, for when you only want the statistics and can skip
the reshape / visualize / regression machinery. It's the only mini-app with a
**Report** tab, so you can go from a spreadsheet to a written-up HTML/Word
results document in three clicks.

Two modes: a **number across groups** (t-test / ANOVA, or the rank-based
Wilcoxon / Kruskal-Wallis with Dunn's or Steel-Dwass all-pairs comparisons), or
**two categorical variables** (chi-square). Select several outcomes and several
grouping variables to test every combination at once.

Built-in demo: load **iris**, then compare `Sepal.Length` across `Species`.

### Mixed Model Review — linear mixed models (lmer)

**Import → Mixed Model → Export.** A point-and-click front end for `lmerTest` /
`emmeans` mixed models: pick a numeric response, up to three fixed effects, and
one or more grouping (random) factors from drop-downs — no `lmer()` calls to
write. It returns an ANOVA (Kenward-Roger or Satterthwaite df), fit & variance
stats (AIC/BIC, marginal & conditional R², ICC, a random-effects caterpillar
plot), residual diagnostics, **EMMeans** post-hoc comparisons with a compact
letter display, an omnibus interaction test, model comparison (likelihood-ratio
test), and a copy-paste R script for everything.

- **Import** is the shared importer — upload a CSV or load the built-in
  **3-factor RCBD** example, and use **Change Variable Types** to read a numeric
  code (e.g. `Nitrogen` 0/100/200) as a factor so its levels can be compared.
- The **Mixed Model** tab also builds **combined (interaction) variables** by
  pasting two columns into one grouping factor.
- **Export** downloads the working data (with any created variables); per-table
  CSVs, plot PNGs and the `.R` script download in place on each result tab.

> This app pulls in the mixed-model stack (`lme4`, `lmerTest`, `emmeans`,
> `multcomp`), so the **first install is a little heavier** than the other tools.

---

## 3. How the apps work together

There is **one** implementation of each feature, shared by every app — all in the
package's `R/`:

```
R/   the whole package
       helpers_*  pure, unit-tested data logic (io, clean, filter, stats, plot,
                  model, reshape, combine, compare, report, state)
       mod_*      Shiny modules — thin wrappers over the helpers
       run_*      the app builders + launchers
            |  (a launcher assembles the modules into an app)
            v
   run_data_explorer()  .  run_reshape_tool()  .  run_combine_tool()
```

- The **`mod_reshape`** in the Reshape Tool is the *exact same module* as the
  Reshape tab in the Data Explorer. **`mod_import`** / **`mod_export`** are shared
  by all three apps. Fix or improve a module once, and every app gets it.
- Stages connect by **returns-and-arguments**: each module returns its result as
  a reactive, and the launcher passes that into the next module. (e.g. Import
  returns the data → Reshape takes it and returns the working data →
  Visualize/Export take that.)
- The exported **helper functions** (`do_stack()`, `grouped_summary()`,
  `fit_model()`, …) are usable on their own in a script — see
  `help(package = "foxplots")`.
- Everything wears the same **UF theme** from `R/components.R`.

---

## 4. For developers

It's a standard R package. From the source folder:

```r
# Load the package for interactive development (no install needed):
pkgload::load_all(".")
run_data_explorer()

# Run the full test suite (~330 checks):
testthat::test_local(".")
```

```sh
# Full build + checks from the command line:
R CMD build .
R CMD check foxplots_*.tar.gz
```

To add a feature, follow the path every existing one took: **pure helper + its
test → a thin `mod_*` module that calls it → wire it into a launcher
(`run_*.R`).** See `CLAUDE.md` for conventions and environment gotchas (notably:
on this machine, run anything that loads Shiny via PowerShell `Rscript.exe`, not
git-bash).
