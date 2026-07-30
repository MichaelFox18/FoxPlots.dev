# ============================================================
# run_reshape_tool.R -- focused import -> reshape -> export app
# ============================================================
# The same mod_reshape that lives inside the full Data Explorer, bracketed by
# mod_import and mod_export.

#' The Reshape Tool app
#'
#' A focused mini-app: import a table, restructure it (stack / split / transpose
#' / sort / subset / summarize), and export the result.
#'
#' `reshape_tool_app()` builds and returns the Shiny app object;
#' `run_reshape_tool()` launches it in a browser.
#'
#' @param ... Passed on to [shiny::runApp()].
#' @return `reshape_tool_app()` returns a [shiny::shinyApp()] object;
#'   `run_reshape_tool()` runs the app (called for its side effect).
#' @examples
#' if (interactive()) run_reshape_tool()
#' @export
reshape_tool_app <- function() {
  options(shiny.maxRequestSize = 250 * 1024^2)

  ui <- page_navbar(
    header       = uf_busy(),   # spinners on slow outputs (maps, big charts)
    title        = uf_title("Reshape Tool"),
    window_title = "UF/IFAS Reshape Tool",
    theme        = uf_theme(),
    fillable     = "reshape",   # the reshape preview fills; import/export scroll

    about_nav_panel(
      "Reshape Tool",
      paste("Restructure a table without writing code: stack or split columns,",
            "transpose, sort, subset, or summarize -- then export the result."),
      c(paste("Import a CSV or Excel file on the Import tab, or load a",
        "built-in wide / long example (relig_income, billboard,",
        "fish_encounters, and more)."),
        "Choose a reshape operation and preview the new table.",
        "Export the reshaped data as CSV, Excel, or RDS."),
      tips = c(
        paste("Subset keeps COLUMNS and draws a random sample of rows; to",
              "keep rows by VALUE (e.g. only Season = Spring), use Filter",
              "rows on the Import tab first."),
        "Stack turns wide data tall (one column per week -> one week column).",
        paste("Leave the operation on None to pass the data through",
              "unchanged."),
        paste("Data Health on the Import tab fixes numbers-as-text, blank",
              "rows, and duplicates before you reshape."))),
    nav_panel(tagList(icon("file-arrow-up"), " Import"),  value = "import",
              importUI("imp", examples = EXAMPLES_RESHAPE)),
    nav_panel(tagList(icon("table-cells"),   " Reshape"), value = "reshape",
              reshapeUI("rs")),
    nav_panel(tagList(icon("file-export"),   " Export & Report"),
              value = "export",
              exportReportUI("ex", "rep",
                             default_title = "Reshape Report"))
  )

  server <- function(input, output, session) {
    imported <- importServer("imp", examples = EXAMPLES_RESHAPE)   # -> reactive(data | NULL)
    reshaped <- reshapeServer("rs", imported)   # -> reactive(reshaped data)
    exportServer("ex", reshaped)                # terminal stage
    # The reshaped table IS the result here, so its overview + column
    # profile are the whole report.
    reportServer("rep", reshaped, default_title = "Reshape Report")
  }

  shiny::shinyApp(ui, server)
}

#' @rdname reshape_tool_app
#' @export
run_reshape_tool <- function(...) {
  shiny::runApp(reshape_tool_app(), ...)
}
