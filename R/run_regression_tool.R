# ============================================================
# run_regression_tool.R -- focused import -> regression -> export/report app
# ============================================================
# The same mod_regression that lives inside the full Data Explorer, for users
# who only want regression and can skip the reshape / visualize / map / compare
# machinery. mod_regression returns the FITTED MODEL (an lm/glm) reactive: the
# Export stage reads the imported data and takes the model for its regression
# downloads; the Report stage takes the model for its regression section.

#' The Regression app
#'
#' A focused mini-app for regression: import a table, then fit a linear or
#' logistic model from drop-downs -- numeric and categorical predictors,
#' interactions, a polynomial term -- and read a coefficient table with 95%
#' confidence intervals, fit statistics, estimated marginal means with
#' connecting letters, full residual diagnostics with assumption checks and VIF,
#' odds ratios (logistic), nested model comparison, and copy-ready R code.
#' Import and type recasting are handled by the shared import stage; export the
#' data plus the model results, or download a full report.
#'
#' `regression_tool_app()` builds and returns the Shiny app object;
#' `run_regression_tool()` launches it in a browser.
#'
#' @param ... Passed on to [shiny::runApp()].
#' @return `regression_tool_app()` returns a [shiny::shinyApp()] object;
#'   `run_regression_tool()` runs the app (called for its side effect).
#' @examples
#' if (interactive()) run_regression_tool()
#' @export
regression_tool_app <- function() {
  options(shiny.maxRequestSize = 250 * 1024^2)

  # iris gives a numeric response plus a 3-level factor (Species) for ANCOVA;
  # mtcars adds all-numeric predictors and 0/1 columns (am, vs) that work as a
  # binary response for logistic regression.
  ex <- list(iris = datasets::iris, mtcars = datasets::mtcars)
  ex_choices <- c("iris (flowers, 3 species)" = "iris",
                  "mtcars (cars)"             = "mtcars")

  ui <- page_navbar(
    title        = uf_title("Regression"),
    window_title = "UF/IFAS Regression",
    theme        = uf_theme(),
    fillable     = FALSE,   # the regression tab is a long scrolling stack

    about_nav_panel(
      "Regression",
      paste("Fit and interpret regression models without writing R code:",
            "linear (lm) or logistic (glm) with numeric and categorical",
            "predictors, interactions, and polynomial terms. You get a",
            "coefficient table with confidence intervals, estimated marginal",
            "means with letter groupings, residual diagnostics with assumption",
            "checks and VIF, odds ratios for logistic models, nested model",
            "comparison, and copy-ready code."),
      c(paste("Import a CSV or Excel file on the Import tab (or load the",
              "built-in iris / mtcars examples). Use Change Type to make a",
              "number that is really a category into a factor."),
        paste("On the Regression tab pick the outcome type, the response, and",
              "the predictors, then fit the model."),
        paste("Review the coefficients, marginal means, and diagnostics;",
              "export the data and model, or download a full HTML / Word",
              "report."))),
    nav_panel(tagList(icon("file-arrow-up"),  " Import"),     value = "import",
              importUI("imp", ex_choices)),
    nav_panel(tagList(icon("chart-simple"),   " Regression"), value = "regression",
              regressionUI("reg")),
    nav_panel(tagList(icon("file-export"),    " Export"),     value = "export",
              exportUI("ex")),
    nav_panel(tagList(icon("file-lines"),     " Report"),     value = "report",
              reportUI("rep", default_title = "Regression Report"))
  )

  server <- function(input, output, session) {
    imported <- importServer("imp", examples = ex)      # -> reactive(data | NULL)
    model    <- regressionServer("reg", imported)       # -> reactive(fitted lm/glm)
    exportServer("ex", imported, model = model)         # data + model downloads
    reportServer("rep", imported, model = model,
                 default_title = "Regression Report")
  }

  shiny::shinyApp(ui, server)
}

#' @rdname regression_tool_app
#' @export
run_regression_tool <- function(...) {
  shiny::runApp(regression_tool_app(), ...)
}
