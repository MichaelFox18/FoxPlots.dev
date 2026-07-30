# ============================================================
# run_compare_groups.R -- focused import -> compare -> export/report app
# ============================================================
# The same mod_compare that lives inside the full Data Explorer, for users who
# only want group comparison and can skip the reshape / visualize / regression
# machinery. mod_compare returns a RESULT LIST (not a data frame), so the Export
# stage reads the imported data directly; the Report stage takes the comparison.

#' The Compare Groups app
#'
#' A focused mini-app for comparing groups: import a table, then test a numeric
#' outcome across groups (t-test / ANOVA, or the rank-based Wilcoxon /
#' Kruskal-Wallis with Dunn's or Steel-Dwass all-pairs comparisons) or test two
#' categorical variables with a chi-square. Several outcomes and grouping
#' variables can be tested at once. Export the data or download a full report.
#'
#' `compare_groups_app()` builds and returns the Shiny app object;
#' `run_compare_groups()` launches it in a browser.
#'
#' @param ... Passed on to [shiny::runApp()].
#' @return `compare_groups_app()` returns a [shiny::shinyApp()] object;
#'   `run_compare_groups()` runs the app (called for its side effect).
#' @examples
#' if (interactive()) run_compare_groups()
#' @export
compare_groups_app <- function() {
  options(shiny.maxRequestSize = 250 * 1024^2)

  ui <- page_navbar(
    header       = uf_busy(),   # spinners on slow outputs (maps, big charts)
    title        = uf_title("Compare Groups"),
    window_title = "UF/IFAS Compare Groups",
    theme        = uf_theme(),
    fillable     = "compare",   # the results grid fills; import/export scroll

    about_nav_panel(
      "Compare Groups",
      paste("Test whether groups really differ, without writing R code: a",
            "number across groups (t-test / ANOVA, or the rank-based",
            "Wilcoxon / Kruskal-Wallis), or two categorical variables",
            "(chi-square). You get assumption checks, effect sizes, post-hoc",
            "comparisons with connecting letters, and plain-English verdicts."),
      c(paste("Import a CSV or Excel file on the Import tab, or load one of",
              "the built-in examples (iris, ToothGrowth, mpg, Titanic, and",
              "more)."),
        paste("On Compare Groups, pick the outcome(s) and grouping",
              "variable(s). Pick several of either to test every combination",
              "at once and see them summarised in one table."),
        paste("Export the data, or download a full HTML / Word report of the",
              "whole comparison.")),
      tips = c(
        paste("Two categorical variables? Variable 1 becomes the table ROWS,",
              "Variable 2 the COLUMNS - the Swap button flips them."),
        paste("Pick several outcomes and groups at once to grid-test every",
              "combination with a corrected summary table."),
        paste("Split by a third variable to run the whole analysis once per",
              "stratum (e.g. separately for each Season)."),
        paste("Titanic is the built-in chi-square demo: try Sex vs",
              "Survived."))),
    nav_panel(tagList(icon("file-arrow-up"), " Import"),         value = "import",
              importUI("imp", examples = EXAMPLES_COMPARE)),
    nav_panel(tagList(icon("flask-vial"),    " Compare Groups"), value = "compare",
              compareUI("cmp")),
    nav_panel(tagList(icon("file-export"),   " Export & Report"),
              value = "export",
              exportReportUI("ex", "rep",
                             default_title = "Compare Groups Report"))
  )

  server <- function(input, output, session) {
    imported   <- importServer("imp", examples = EXAMPLES_COMPARE)   # -> reactive(data | NULL)
    comparison <- compareServer("cmp", imported)       # -> reactive(result list)
    exportServer("ex", imported)                       # data download
    reportServer("rep", imported, comparison = comparison,
                 default_title = "Compare Groups Report")
  }

  shiny::shinyApp(ui, server)
}

#' @rdname compare_groups_app
#' @export
run_compare_groups <- function(...) {
  shiny::runApp(compare_groups_app(), ...)
}
