# foxplots — UF/IFAS Data Toolkit (R package)

A point-and-click toolkit for UF/IFAS students and staff to **import, clean,
reshape, summarize, visualize, compare, model, and export** tabular data — no R
code required. It is an installable **R package** built from reusable Shiny
**modules** on a shared, unit-tested **helper** foundation.

> Contributor/architecture notes live in `CLAUDE.md`; plain-English run
> instructions are in `HOW_TO_USE.md`.

---

## Status

`foxplots` is an **installable R package**. It bundles the whole toolkit — eight
Shiny apps, the modules behind them, and a tested helper foundation — and exports
both **launcher functions** (`run_data_explorer()`, `run_reshape_tool()`,
`run_combine_tool()`, `run_compare_groups()`, `run_lmer_tool()`,
`run_glmm_review()`, `run_map_tool()`, `run_regression_tool()`) and the
underlying **helper API** (`do_stack()`, `grouped_summary()`, `fit_model()`, …).
Install it once and run anywhere.

Highlights:
- **One-click report** — HTML or editable Word (`.docx`), pandoc-free, with a
  "show the R code" toggle — now in **every app**, with a **per-section picker**
  so you choose what goes in, and **including your map** as a snapshot of what
  you framed on screen.
- **Maps with boundaries included** — shade **US states, US counties, or world
  countries** without hunting for a GeoJSON (uploading your own still works).
- **Save / restore your progress** — a `.rds` of your data + all data prep.
- **11 chart types** (incl. a bubble option) and a reversible Data Health
  cleaner with outlier flagging.
- **Real regression** — linear or **logistic**, numeric + **categorical**
  predictors, interactions, coefficient table with CIs, estimated marginal means
  with letters, diagnostics + assumption checks + VIF, odds ratios, and model
  comparison.
- **JMP-style one-way analysis** — group means with SE/95% CI, a full ANOVA table
  and R-squared, Welch's ANOVA, **connecting letters**, and non-parametric parity
  (Dunn's / **Steel-Dwass** all-pairs, rank effect sizes). Test **many outcomes
  against many groups at once**, and **split any analysis by a third variable**.
- **Interactive maps** — points with layer groups, log/quantile color **and
  size** scales with a graduated size legend, an optional **density heatmap**,
  or a **choropleth** (regions shaded from built-in or uploaded boundaries) —
  plus one-file interactive HTML + PNG export and copy-ready leaflet code.

The look stays UF/IFAS-themed (blue/orange + the IFAS logo) throughout.

---

## 1. Install & run

You need **R 4.4+** (and optionally RStudio). Install with `remotes`, which pulls
in every dependency automatically:

```r
install.packages("remotes")

# Install straight from GitHub (the usual way). upgrade = "never" skips a
# hidden "update other packages?" prompt that can make the install look stuck:
remotes::install_github("UFSDACU/FoxPlots", upgrade = "never")

# ...or from a local copy (on the GitHub page: Code -> Download ZIP, unzip, then):
# remotes::install_local("~/Downloads/FoxPlots")            # macOS / Linux
# remotes::install_local("C:/Users/you/Downloads/FoxPlots") # Windows

# Optional extras -- Word (.docx) reports, the Map tab's PNG snapshots,
# and the map's density-heatmap layer:
# install.packages(c("officer", "webshot2", "chromote", "leaflet.extras"))
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
                         #    run_glmm_review() / run_map_tool() /
                         #    run_regression_tool()
```

| Launcher | App | For… |
|---|---|---|
| `run_data_explorer()` | **Data Explorer** | the whole workflow in one place |
| `run_reshape_tool()`  | **Reshape Tool**  | restructuring one table |
| `run_combine_tool()`  | **Combine Tool**  | merging / joining / comparing two tables |
| `run_compare_groups()` | **Compare Groups** | testing whether groups differ (t-test / ANOVA / rank tests / chi-square), optionally split by a third variable |
| `run_lmer_tool()`     | **Mixed Model Review** | fitting linear mixed models (lmer) |
| `run_glmm_review()`   | **GLMM Review** | generalized linear mixed models (glmmTMB): counts, proportions, zero-inflated and 0/1 outcomes |
| `run_map_tool()`      | **Map Tool**      | interactive maps — points, density heatmap, or shaded regions (built-in state / county / country boundaries, or your own GeoJSON) |
| `run_regression_tool()` | **Regression Tool** | linear / logistic regression with diagnostics, marginal means, and a report |

The package also exports the helper functions for use in your own scripts (e.g.
`do_stack()`, `grouped_summary()`, `build_full_plot()`); see
`help(package = "foxplots")`. Step-by-step setup is in `HOW_TO_USE.md`.

---

## 2. The eight apps — separately

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
5. **Map** — put rows with coordinates on an interactive basemap (see the **Map
   Tool** below for everything it can do — points, layer groups, density
   heatmap, or shaded regions from built-in or uploaded boundaries).
6. **Compare Groups** — a JMP-style one-way analysis: t-test / ANOVA (or
   non-parametric Wilcoxon / Kruskal with **Dunn's or Steel-Dwass** all-pairs)
   across groups, with a group-means table (SE + 95% CI), the full ANOVA table
   and R-squared, Welch's ANOVA, a means plot with **connecting letters**, and
   assumption checks + effect sizes. Pick **several outcomes and several grouping
   variables** to test every combination at once — a summary table (with p-values
   corrected across the whole set) plus a collapsible full write-up per
   combination — and optionally **split the whole analysis by a third variable**
   (e.g. mpg by cyl, split by am → one analysis per transmission type). Or a
   chi-square between two categories, with expected counts, standardized
   residuals and optional row / column / total percentages.
7. **Regression** — linear **or logistic** models with numeric **and
   categorical** predictors (with a reference-level picker), interactions, and
   polynomial fits: a coefficient table with 95% CIs, fit statistics, estimated
   marginal means with **connecting letters**, full residual diagnostics with
   an **assumption-check panel** and **VIF**, odds ratios (logistic), nested
   model comparison, and copy-ready R code.
8. **Export** — download the (reshaped) data (CSV/Excel/RDS), the **charts**
   (PNG/PDF), the **summary**, and the **regression** results.
9. **Report** — one click bundles the whole session into a report: **HTML** (a
   single self-contained file) or an editable **Word `.docx`**, with an optional
   "show the R code" toggle. **The map is included** as a snapshot of exactly
   what you framed on screen.

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

### GLMM Review — generalized linear mixed models (glmmTMB)

**Import → General GLMM / Binary (0/1) GLMM → Export.** The Mixed Model
Review's sibling for responses a normal distribution can't handle: counts,
proportions, zero-heavy data, and presence/absence. Pick a response, a
**distribution family** (Gaussian, Gamma, Poisson, negative binomial
nbinom1/nbinom2, Tweedie, Beta — each with a live note on what values are
valid, checked against your actual data *before* you fit), fixed effects and
grouping (random) factors from drop-downs — no `glmmTMB()` calls to write.

- Optional **zero-inflation** (`ziformula`) and **dispersion**
  (`dispformula`) side-models, point-and-click.
- **Binary (0/1) outcomes get their own tab** with a link-function picker
  (logit / probit / cloglog / cauchit) and Bernoulli-specific guardrails.
- Diagnostics use **DHARMa simulated residuals** — the right tool for
  glmmTMB — with uniformity, overdispersion, zero-inflation and outlier
  tests, plus a classic Pearson chi-square/df cross-check.
- **Type III Wald ANOVA** (`car::Anova`), **EMMeans on the response scale**
  (pairwise comparisons come out as ratios / odds ratios) with a compact
  letter display and plot, CSV/PNG downloads, and a copy-paste R script.
- Built-in **field example** (`make_glmm_example_data()`): an overdispersed
  count, a zero-heavy count, a proportion, and a 0/1 response — one column
  per family worth demonstrating.

> Pulls in `glmmTMB` + `DHARMa` on top of the mixed-model stack, so the
> first install is on the heavier side too.

### Map Tool — interactive maps from a spreadsheet

**Import → Map → Export.** Two map types:

- **Points** — rows with latitude / longitude become markers on a pannable
  basemap (coordinates auto-detected, with a swap button). Color by any column
  (with log / quantile scales for skewed values), **size by** a numeric column
  (also with log / quantile scales, and a **graduated-circle size legend** so
  bubbles are actually readable), group into **toggleable layers** by a column
  (clustering stays within each group, plus a per-group zoom), click popups and
  hover labels, an optional **density heatmap** layer for dense data (optionally
  weighted by a value column), and a distance **scale bar**.
- **Shaded regions (choropleth)** — pick **built-in boundaries** (US states, US
  counties, world countries — optionally limited to one state or continent) or
  upload your own **GeoJSON**, match a boundary property to a column in your
  data, and shade each region by the mean / sum / median of a numeric column. The app
  reports exactly **how many regions matched** and names the strays on both
  sides, so a join problem is never a mystery blank map.

> **No coordinates? You can still make a map.** Shaded regions needs *no*
> latitude or longitude — just a column of state / county / country names and a
> number to shade by, e.g. poverty rate by state. Boundaries for all three are
> built in, and the property picker shows an example of each option
> (`county_state — e.g. Brooks County, Georgia`) so you can match it against
> your own column by eye. Import data with no coordinates and the Map tab says
> so and points you here.

Downloads: a **one-file interactive HTML** map (works offline, no extra
software) and a **PNG snapshot** framed exactly as on screen (needs the optional
`webshot2` + `chromote` packages plus Chrome). Every map comes with copy-ready
leaflet R code.

### Regression Tool — model fitting on its own

**Import → Regression → Export → Report.** The Data Explorer's (deep) Regression
tab as a focused app: linear or logistic models, numeric + categorical
predictors, interactions, coefficient table with CIs, estimated marginal means
with letters, diagnostics + assumptions + VIF, odds ratios, model comparison,
and a full HTML / Word report. Built-in **iris** (ANCOVA demo: `Sepal.Length ~
Petal.Length + Species`) and **mtcars** (logistic demo: response `am`) examples.

---

## 3. How the apps work together

There is **one** implementation of each feature, shared by every app — all in the
package's `R/`:

```
R/   the whole package
       helpers_*  pure, unit-tested data logic (io, clean, filter, stats, plot,
                  model, reshape, combine, compare, lmer, map, report, state)
       mod_*      Shiny modules — thin wrappers over the helpers
       run_*      the app builders + launchers
            |  (a launcher assembles the modules into an app)
            v
   run_data_explorer() . run_reshape_tool()  . run_combine_tool()
   run_compare_groups() . run_lmer_tool()    . run_glmm_review()
   run_map_tool()       . run_regression_tool()
```

- The **`mod_reshape`** in the Reshape Tool is the *exact same module* as the
  Reshape tab in the Data Explorer. **`mod_import`** / **`mod_export`** are shared
  by every app. Fix or improve a module once, and every app gets it.
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

# Run the full test suite (1,600+ checks):
testthat::test_local(".")
```

```sh
# Full build + checks from the command line:
R CMD build .
R CMD check foxplots_*.tar.gz
```

The suite covers the helpers, each module's contract, and every launcher's
wiring. What it cannot cover is whether the apps are pleasant to use or whether
a real-world spreadsheet lands cleanly — `TESTING_CHECKLIST.md` is the manual
pass for that, with a per-app list of edge cases and what good looks like.

To add a feature, follow the path every existing one took: **pure helper + its
test → a thin `mod_*` module that calls it → wire it into a launcher
(`run_*.R`).** See `CLAUDE.md` for conventions and per-platform environment
notes (the PowerShell-only workaround there applies to the Windows dev box; on
macOS and Linux a plain `Rscript` in any shell is fine).
