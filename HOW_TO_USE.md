# How to run the apps

`foxplots` is an **R package**. Once it's installed, you launch any app with a
single line — no folders to point at, no files to source.

There are three apps:

| Launch with | App | What it's for |
|---|---|---|
| `run_data_explorer()` | **Data Explorer** | The whole workflow: import → clean → reshape → summarize → visualize → compare → model → report |
| `run_reshape_tool()`  | **Reshape Tool**  | Just restructure one table (stack / split / transpose / sort / subset / summarize) and export it |
| `run_combine_tool()`  | **Combine Tool**  | Merge / join / compare **two** tables |

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

Then install foxplots. Pick whichever fits how you got the project:

- **From a folder you downloaded/cloned** — point at it:

  ```r
  remotes::install_local("C:/path/to/FoxPlots")
  ```

- **From GitHub** (if you have access to the repository):

  ```r
  remotes::install_github("UFSDACU/FoxPlots")
  ```

Either way, the other packages foxplots needs are installed automatically. This
can take a few minutes the first time; you only do it once (re-run it to update).

---

## Step 3 — Run an app

```r
library(foxplots)
run_data_explorer()      # or run_reshape_tool() / run_combine_tool()
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
```

See `help(package = "foxplots")` for the full list.

---

## If something goes wrong

| What you see | Fix |
|---|---|
| `there is no package called 'foxplots'` | The install didn't finish — re-run Step 2. |
| `could not find function "run_data_explorer"` | Run `library(foxplots)` first. |
| Install fails on a dependency | Run `install.packages("<name>")` for the one it names, then retry Step 2. |
| The app opens but the UF logo is missing | Reinstall the package (Step 2) so its bundled files come along. |

---

## More detail

- **What each app and tab does:** see `README.md`.
- **Architecture and conventions (for developers):** see `CLAUDE.md`.
- **Run the test suite (from the source folder):** `R CMD check .`, or in R: `testthat::test_local()`.
