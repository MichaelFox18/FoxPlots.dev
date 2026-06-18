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
    title        = uf_title("Reshape Tool"),
    window_title = "UF/IFAS Reshape Tool",
    theme        = uf_theme(),
    fillable     = "reshape",   # the reshape preview fills; import/export scroll

    nav_panel(tagList(icon("file-arrow-up"), " Import"),  value = "import",
              importUI("imp")),
    nav_panel(tagList(icon("table-cells"),   " Reshape"), value = "reshape",
              reshapeUI("rs")),
    nav_panel(tagList(icon("file-export"),   " Export"),  value = "export",
              exportUI("ex"))
  )

  server <- function(input, output, session) {
    imported <- importServer("imp")             # -> reactive(data | NULL)
    reshaped <- reshapeServer("rs", imported)   # -> reactive(reshaped data)
    exportServer("ex", reshaped)                # terminal stage
  }

  shiny::shinyApp(ui, server)
}

#' @rdname reshape_tool_app
#' @export
run_reshape_tool <- function(...) {
  shiny::runApp(reshape_tool_app(), ...)
}
