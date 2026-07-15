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

  # Join-friendly built-in examples: band_members + band_instruments share
  # `name` (the classic join demo); the two mtcars halves demo Concatenate.
  mt <- as.data.frame(datasets::mtcars); mt$car <- rownames(mt); rownames(mt) <- NULL
  combine_examples <- list(
    band_members     = as.data.frame(dplyr::band_members),
    band_instruments = as.data.frame(dplyr::band_instruments),
    mtcars_top       = utils::head(mt, 16),
    mtcars_bottom    = utils::tail(mt, 16)
  )
  ex_choices <- c("band_members (name, band)"      = "band_members",
                  "band_instruments (name, plays)" = "band_instruments",
                  "mtcars (rows 1\u201316)"             = "mtcars_top",
                  "mtcars (rows 17\u201332)"            = "mtcars_bottom")

  ui <- page_navbar(
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
        "Export the combined data as CSV, Excel, or RDS.")),
    nav_panel(tagList(icon("table-columns"), " Left table"),  value = "left",
              importUI("left", ex_choices)),
    nav_panel(tagList(icon("table-columns"), " Right table"), value = "right",
              importUI("right", ex_choices)),
    nav_panel(tagList(icon("object-group"),  " Combine"),     value = "combine",
              combineUI("cmb")),
    nav_panel(tagList(icon("file-export"),   " Export"),      value = "export",
              exportUI("ex"))
  )

  server <- function(input, output, session) {
    left_data  <- importServer("left",  examples = combine_examples)
    right_data <- importServer("right", examples = combine_examples)
    combined   <- combineServer("cmb", left_data, right_data)
    exportServer("ex", combined)
  }

  shiny::shinyApp(ui, server)
}

#' @rdname combine_tool_app
#' @export
run_combine_tool <- function(...) {
  shiny::runApp(combine_tool_app(), ...)
}
