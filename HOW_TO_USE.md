# How to run the apps

`foxplots` is an **R package**. Once it's installed, you launch any app with a
single line — no folders to point at, no files to source.

There are eight apps:

| Launch with | App | What it's for |
|---|---|---|
| `run_data_explorer()` | **Data Explorer** | The whole workflow: import → clean → reshape → summarize → visualize → map → compare → model → report |
| `run_reshape_tool()`  | **Reshape Tool**  | Just restructure one table (stack / split / transpose / sort / subset / summarize) and export it |
| `run_combine_tool()`  | **Combine Tool**  | Merge / join / compare **two** tables |
| `run_compare_groups()` | **Compare Groups** | Just the statistics: do these groups really differ? (t-test / ANOVA / rank tests / chi-square), then download a report |
| `run_lmer_tool()`     | **Mixed Model Review** | Fit linear mixed models (lmer): ANOVA, EMMeans post-hoc, diagnostics |
| `run_glmm_review()`   | **GLMM Review** | Fit **generalized** linear mixed models (glmmTMB) for counts, proportions, zero-inflated and 0/1 outcomes: family picker with live guardrails, DHARMa residual checks, Wald ANOVA, EMMeans with letters |
| `run_map_tool()`      | **Map Tool**      | Interactive maps two ways: **points** (color / size by variables with log / quantile scales + a size legend, layer groups with toggle + zoom, popups, clustering, an optional density heatmap, scale bar) or **shaded regions** (built-in US state / county / world-country boundaries, or your own GeoJSON, shaded by your data). One-file HTML + PNG download (basemap tiles need internet) |
| `run_regression_tool()` | **Regression Tool** | Fit linear or logistic regression: numeric / categorical predictors, interactions, coefficient table with CIs, estimated marginal means, diagnostics with assumption checks and VIF, odds ratios, model comparison, then export or report |

---

## Step 1 — Install R (once per computer)

Install **R 4.4 or newer**: <https://cran.r-project.org>
(and **RStudio**, the friendly interface: <https://posit.co/download/rstudio-desktop>).

---

## Step 2 — Install the `foxplots` package (once)

You need the `remotes` package to install foxplots; get it first:

```r
install.packages("remotes")
```

Then install foxplots **straight from GitHub** — this is the normal way, and it
pulls the whole package and every dependency for you:

```r
remotes::install_github("UFSDACU/FoxPlots", upgrade = "never")
```

(`upgrade = "never"` skips a hidden "update other packages?" question that
otherwise makes the install look frozen while it waits for an answer.)

That single line is also how you **update** later: re-run it any time to pull the
newest version.

A few optional extras unlock extra features — install them if you want them:

```r
# Word (.docx) reports, the Map tab's PNG snapshot button, and the map's
# density-heatmap layer:
install.packages(c("officer", "webshot2", "chromote", "leaflet.extras"))
```

<details>
<summary>Alternative: install from a folder you already downloaded</summary>

On the GitHub page choose **Code → Download ZIP**, unzip it, then point at the
folder:

```r
remotes::install_local("~/Downloads/FoxPlots")     # macOS / Linux
remotes::install_local("C:/Users/you/Downloads/FoxPlots")   # Windows
```
</details>

Either way, the other packages foxplots needs are installed automatically. This
can take a few minutes the first time; you only do it once.

---

## Step 3 — Run an app

```r
library(foxplots)
run_data_explorer()      # or run_reshape_tool() / run_combine_tool() /
                         #    run_compare_groups() / run_lmer_tool() /
                         #    run_glmm_review() / run_map_tool() /
                         #    run_regression_tool()
```

The app opens in your web browser. To stop it, press **Esc** in the R console
(or close the browser tab).

---

## Bonus: use the tools in your own R code

The package also exports the underlying helper functions, so you can use them in
a script without the apps — for example:

```r
library(foxplots)
do_stack(my_data, c("q1", "q2", "q3"))        # reshape wide → tall
grouped_summary(my_data, vars = "score", groups = "group")
fit_model(my_data, response = "y", predictors = c("x1", "x2"))

foxplots_examples()                # catalogue of every built-in dataset
d <- foxplots_example("titanic")   # load one without opening an app
```

See `help(package = "foxplots")` for the full list.

---

## If something goes wrong

| What you see | Fix |
|---|---|
| Install runs forever, nothing happens | It's waiting at a question in the R console (scroll down and press **Enter**). Avoid it by installing with `upgrade = "never"` as in Step 2. |
| `WARNING: Rtools is required to build R packages` | Safe to ignore — foxplots is pure R and installs fine without Rtools. |
| `there is no package called 'foxplots'` | The install didn't finish — re-run Step 2. |
| `could not find function "run_data_explorer"` | Run `library(foxplots)` first. |
| Install fails on a dependency | Run `install.packages("<name>")` for the one it names, then retry Step 2. |
| The app opens but the UF logo is missing | Reinstall the package (Step 2) so its bundled files come along. |
| Map tab: no PNG button, or a note about webshot2 | Install the optional extras from Step 2 (`webshot2`, `chromote`) — PNG snapshots also need Chrome or Edge on the computer. |
| Map is grey / no background | Basemap tiles load from the internet — check the connection. Your points still export fine. |
| My data has no latitude/longitude — can I still map it? | Yes. Set **Map type → Shaded regions**: it maps data by *name* (state, county or country) and needs no coordinates at all. Pick a built-in boundary set, match your name column, and choose the number to shade by. The Map tab tells you this itself when it finds no coordinate columns. |
| Shaded-regions map is blank | Check the sidebar's match report — the boundary property must exactly match your data column's values (e.g. "Alachua" vs "ALACHUA" won't match). With built-in US counties, try the `county_state` ("Alachua County, Florida") or `fips` property, or set "Limit to state" and match on plain `county`. |

---

## More detail

- **What each app and tab does:** see `README.md`.
- **Architecture and conventions (for developers):** see `CLAUDE.md`.
- **Run the test suite (from the source folder):** `R CMD check .`, or in R: `testthat::test_local()`.
- **Testing an app by hand before a release:** see `TESTING_CHECKLIST.md` — a per-app list of things to click, including the edge cases that have broken before.
