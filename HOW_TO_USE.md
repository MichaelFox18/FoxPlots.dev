# How to run the apps

Plain-English steps to get the UF/IFAS data apps running on your computer. If
you just want the fastest path, do **Step 1**, **Step 2**, and **Step 3** — that's it.

There are three apps. Pick the one that fits your task:

| App | What it's for | Folder |
|---|---|---|
| **Data Explorer** | The whole workflow: import → clean → reshape → summarize → visualize → compare → model → report | `apps/data_explorer` |
| **Reshape Tool** | Just restructure one table (stack / split / transpose / sort / subset / summarize) and export it | `apps/reshape_tool` |
| **Combine Tool** | Merge / join / compare **two** tables | `apps/combine_tool` |

---

## Step 1 — Install R and the packages (once per computer)

1. Install **R 4.6 or newer**: <https://cran.r-project.org>
   (and **RStudio**, the friendly interface: <https://posit.co/download/rstudio-desktop>).
2. Open R (or RStudio) and paste this **once** to install everything the apps use:

```r
install.packages(c(
  "shiny", "bslib", "DT", "ggplot2", "plotly", "colourpicker",
  "dplyr", "tidyr", "tidyselect", "readxl", "writexl", "here", "binom",
  "hexbin", "officer"
))
```

It can take a few minutes the first time. You only do this once.

---

## Step 2 — Get the project folder

Download the project from GitHub and keep it together in one folder (it will be
called `FoxPlots`):

- **Clone it** (if you use git): `git clone <repo-url>`, **or**
- **Download a ZIP**: on the GitHub page click **Code → Download ZIP**, then
  unzip it somewhere easy to find, like your Desktop or Documents.

> **The one rule:** keep the whole `FoxPlots` folder together. The apps share
> code from the `R/` and `modules/` folders next to them, so they need the whole
> folder — not just a single `app.R`. (A hidden `.here` marker file lets them
> find that shared code automatically; you don't need to do anything with it.)

---

## Step 3 — Run an app

### Easiest: RStudio's "Run App" button

1. In RStudio: **File → Open File…** and open `apps/data_explorer/app.R`
   (inside your `FoxPlots` folder).
2. Click the green **▶ Run App** button at the top-right of the editor.
3. To open it in a full browser window, click the **▶** dropdown → **Run External**.

That's it — RStudio handles the working directory for you.

### Or: any R console (RStudio Console, R GUI, VS Code R terminal)

Point R at your `FoxPlots` folder, then launch:

```r
setwd("C:/path/to/FoxPlots")          # <- the folder you downloaded
shiny::runApp("apps/data_explorer")
```

Swap in `"apps/reshape_tool"` or `"apps/combine_tool"` for the other apps.
`setwd()` only needs doing once per session; use forward slashes `/` in the
path, even on Windows.

The app opens in your web browser. To stop it, press **Esc** in the R console
(or close the browser tab).

---

## If something goes wrong

| What you see | Fix |
|---|---|
| `could not find function "runApp"` | Run `library(shiny)` first, or use `shiny::runApp(...)`. |
| `cannot open file 'apps/...'` / path not found | R isn't pointed at the `FoxPlots` folder — use the **Run App** button, or `setwd("…/FoxPlots")`. |
| `could not find function "uf_title"` (or similar) | The shared code wasn't loaded — you're in the wrong folder. Same fix as above. |
| `there is no package called '…'` | A package isn't installed — re-run the install line in Step 1. |
| The UF logo doesn't show | Make sure the whole `FoxPlots` folder is intact and you launched from inside it. |
| Paths act strange after lots of testing | Restart R (**Session → Restart R**) and try again. |

---

## Want more detail?

- **What each app and tab does:** see `README.md`.
- **Architecture and conventions (for developers):** see `CLAUDE.md`.
- **Run the test suite:** `testthat::test_dir(here::here("tests/testthat"))`.
