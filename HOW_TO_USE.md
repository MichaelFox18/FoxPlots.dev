# How to use these apps

Plain-English instructions for running the UF/IFAS data apps — in **RStudio**,
a plain **R console**, or **VS Code**. If you just want the fastest path, read
section 1 and stop.

There are three apps. Pick the one that matches your task:

| App | What it's for | Folder |
|---|---|---|
| **Data Explorer** | The whole workflow: import → clean → reshape → summarize → visualize → model → export | `apps/data_explorer` |
| **Reshape Tool** | Just restructure one table (stack/split/transpose/sort/subset) and export it | `apps/reshape_tool` |
| **Combine Tool** | Merge / join / compare **two** tables | `apps/combine_tool` |

---

## 1. The 30-second version

1. Make sure R and the required packages are installed (section 2, one-time).
2. Open R **in the `projects` folder** (section 3 explains how in each program).
3. Run one line:

```r
shiny::runApp("apps/data_explorer")
```

Swap in `"apps/reshape_tool"` or `"apps/combine_tool"` for the other apps. The
app opens in your web browser. To stop it, press **Esc** in the R console (or
close the browser tab and the console).

---

## 2. One-time setup: install R and the packages

You need **R 4.6 or newer** (and RStudio if you want the friendly interface —
[posit.co/download/rstudio-desktop](https://posit.co/download/rstudio-desktop)).

Open R and paste this once to install everything the apps use:

```r
install.packages(c(
  "shiny", "bslib", "DT", "ggplot2", "plotly", "colourpicker",
  "dplyr", "tidyr", "tidyselect", "readxl", "writexl", "here", "binom"
))
```

(That can take a few minutes the first time. You only do it once per computer.)

---

## 3. The one rule that makes everything work

**Keep the `projects` folder together, and run the apps from inside it.**

These apps are intentionally built from many small files — the apps in `apps/`
borrow shared code from the `R/` and `modules/` folders next to them (so a fix
in one place improves every app). That means:

- ✅ You need the **whole `projects` folder**, not just a single `app.R`.
- ✅ R needs to be pointed at the `projects` folder when it runs.

A small marker file named `.here` sits in the `projects` folder so the apps can
always find their shared code, no matter which app you launch. You don't have to
do anything with it — just don't move the apps out of the folder.

---

## 4. Running it in each program

### A) RStudio — the easy "Run App" button (recommended)

1. In RStudio: **File → Open File…** and open the app you want, e.g.
   `projects/apps/data_explorer/app.R`.
2. A green **▶ Run App** button appears at the top-right of the editor. Click it.
3. The app opens. Click the **▶** dropdown → **Run External** to open it in a
   full browser window instead of RStudio's small viewer.

That's it — RStudio handles the working directory for you, and the `.here`
marker lets the app find its shared code automatically.

> **Even tidier:** RStudio → **File → New Project → Existing Directory →** choose
> the `projects` folder. From then on, opening that project starts you in the
> right place, and you can just run `shiny::runApp("apps/data_explorer")` in the
> Console.

### B) Any R console (RStudio Console, R GUI, or `Rscript`)

Point R at the `projects` folder, then launch:

```r
setwd("C:/Users/michaelfox1/Desktop/FoxPlots")  # <- your path to projects
shiny::runApp("apps/data_explorer")
```

`setwd()` only needs to be done once per session. Use forward slashes `/` in the
path even on Windows.

### C) VS Code

1. Open an **R terminal** (Command Palette → "R: Create R terminal"), then use
   the same two lines as **B** above.
2. Or install the R extension and use its **Run** support the same way RStudio
   does.

---

## 5. Which tab does what (Data Explorer)

The tabs run left to right, and **each one feeds the next**:

1. **Import** — load a CSV/Excel/TSV/RDS file (or a built-in example). The
   **Data Health** panel flags common problems (stray text, blank rows, etc.)
   as one-click, reversible fixes; **Change Variable Types** recasts a column;
   **Filter rows** keeps only the rows matching conditions you set (e.g. team is
   any of LAL/BOS, points ≥ 20) — everything downstream uses the filtered data.
2. **Reshape** *(optional)* — Stack, Split, Transpose, Sort, or Subset. Leave it
   on **None** to pass your data through untouched.
3. **Summarize** — counts/means/medians/SD/SE/IQR by group, or **Proportions**
   (percent of each category, with confidence intervals).
4. **Visualize** — up to four charts at once, with a "copy the R code" button.
5. **Compare Groups** — t-test / ANOVA (or non-parametric) across groups, or a
   chi-square between two categories, with assumption checks and effect sizes.
6. **Regression** — fit a model and read a plain-English interpretation.
7. **Export** — download your data, charts, summary, or model results.

Whatever you do on Import + Reshape is the data the rest of the app uses.

---

## 6. If something goes wrong

| What you see | What it means | Fix |
|---|---|---|
| `could not find function "runApp"` | The `shiny` package isn't loaded | Run `library(shiny)` first, or use `shiny::runApp(...)` |
| `cannot open file 'apps/...'` / path not found | R isn't pointed at the `projects` folder | Do `setwd("…/projects")` (section 4B) or use the Run App button |
| `could not find function "uf_title"` (or similar) | The shared code wasn't loaded — usually the wrong folder | Same as above: run from `projects` |
| The UF logo doesn't show | The app can't find the `www/` image | Make sure the whole `projects` folder is intact and you're launching from it |
| `there is no package called '…'` | A package isn't installed | Re-run the install line in section 2 |
| Paths act strange after lots of testing | R cached an old location | Restart R (Session → Restart R), then try again |

---

## 7. For people maintaining the code

- Architecture, conventions, and the file map: **`CLAUDE.md`**.
- A running history of what was built and why: **`BUILD_LOG.md`**.
- A higher-level overview of the toolkit: **`README.md`**.
- Run the test suite: `testthat::test_dir(here::here("tests/testthat"))`.
