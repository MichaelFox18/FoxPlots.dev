# ============================================================
# run_combine_tool.R -- import two tables, combine them, export
# ============================================================
# The two-table counterpart to the Reshape Tool: a full mod_import for each of
# the Left and Right tables, then mod_combine, then mod_export.

#' The Combine Tool app
#'
#' A focused mini-app for two tables: import (and optionally clean) a Left and a
#' Right table, then concatenate / join / update / compare them and export.
#'
#' `combine_tool_app()` builds and returns the Shiny app object;
#' `run_combine_tool()` launches it in a browser.
#'
#' @param ... Passed on to [shiny::runApp()].
#' @return `combine_tool_app()` returns a [shiny::shinyApp()] object;
#'   `run_combine_tool()` runs the app (called for its side effect).
#' @examples
#' if (interactive()) run_combine_tool()
#' @export
combine_tool_app <- function() {
  options(shiny.maxRequestSize = 250 * 1024^2)

  ui <- page_navbar(
    header       = uf_busy(),   # spinners on slow outputs (maps, big charts)
    title        = uf_title("Combine Tool"),
    window_title = "UF/IFAS Combine Tool",
    theme        = uf_theme(),
    fillable     = "combine",

    about_nav_panel(
      "Combine Tool",
      paste("Bring two tables together: concatenate rows, join on a key,",
            "update values, or compare the two -- then export the result."),
      c("Import the first table on the Left table tab.",
        "Import the second on the Right table tab.",
        "Pick how to combine them on the Combine tab and preview the result.",
        "Export the combined data as CSV, Excel, or RDS."),
      tips = c(
        paste("Join matches rows by a shared key column (band_members +",
              "band_instruments share `name` - try it)."),
        paste("Concatenate stacks rows; the two mtcars halves are a ready",
              "demo."),
        paste("Compare shows what changed between two versions of the same",
              "table."))),
    nav_panel(tagList(icon("table-columns"), " Left table"),  value = "left",
              importUI("left", examples = EXAMPLES_COMBINE)),
    nav_panel(tagList(icon("table-columns"), " Right table"), value = "right",
              importUI("right", examples = EXAMPLES_COMBINE)),
    nav_panel(tagList(icon("object-group"),  " Combine"),     value = "combine",
              combineUI("cmb")),
    nav_panel(tagList(icon("file-export"),   " Export & Report"),
              value = "export",
              exportReportUI("ex", "rep",
                             default_title = "Combine Report"))
  )

  server <- function(input, output, session) {
    left_data  <- importServer("left",  examples = EXAMPLES_COMBINE)
    right_data <- importServer("right", examples = EXAMPLES_COMBINE)
    combined   <- combineServer("cmb", left_data, right_data)
    exportServer("ex", combined)
    # The combined table is the result; its overview + column profile are
    # the report.
    reportServer("rep", combined, default_title = "Combine Report")
  }

  shiny::shinyApp(ui, server)
}

#' @rdname combine_tool_app
#' @export
run_combine_tool <- function(...) {
  shiny::runApp(combine_tool_app(), ...)
}
