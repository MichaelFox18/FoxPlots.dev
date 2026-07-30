# ============================================================
# run_map_tool.R -- focused import -> map -> export app
# ============================================================
# The Map module (mod_map) bracketed by the shared mod_import (upload + Data
# Health + type recast + the single data preview) and mod_export (data-only
# download; its own preview is suppressed so there is one preview). The map's
# own HTML / PNG downloads live inside the Map tab.

#' The Map Tool app
#'
#' A focused mapping tool: import a table with latitude / longitude columns
#' (auto-detected), put every row on an interactive leaflet basemap, and style
#' the markers point-and-click -- color by a column with a matching legend,
#' area-proportional bubble sizes, click popups, hover labels, and automatic
#' clustering for dense data. Download the finished map as a self-contained
#' interactive HTML file or a static PNG, and copy ready-to-run leaflet code.
#' Basemap tiles load from the internet.
#'
#' `map_tool_app()` builds and returns the Shiny app object;
#' `run_map_tool()` launches it in a browser.
#'
#' @param ... Passed on to [shiny::runApp()].
#' @return `map_tool_app()` returns a [shiny::shinyApp()] object;
#'   `run_map_tool()` runs the app (called for its side effect).
#' @examples
#' if (interactive()) run_map_tool()
#' @export
map_tool_app <- function() {
  options(shiny.maxRequestSize = 250 * 1024^2)

  ui <- page_navbar(
    header       = uf_busy(),   # spinners on slow outputs (maps, big charts)
    title        = uf_title("Map Tool"),
    window_title = "UF/IFAS Map Tool",
    theme        = uf_theme(),
    fillable     = FALSE,   # the map card has its own fixed height

    about_nav_panel(
      "Map Tool",
      paste("Put your data on an interactive map: any table with latitude and",
            "longitude columns becomes styled markers on a basemap, with",
            "color and size driven by your variables, popups, a legend, and",
            "clustering for dense data. Download the result as an interactive",
            "HTML file or a PNG image, or copy the matching leaflet R code.",
            "Basemap tiles load from the internet."),
      c(paste("Import a CSV or Excel file with coordinate columns on the",
              "Import tab, or load an example (Florida sites, Fiji",
              "earthquakes, Atlantic storm tracks, or the map-ready state",
              "rent + income set for shaded regions)."),
        paste("On the Map tab the coordinate columns are detected",
              "automatically; style the markers and pan / zoom freely.",
              "Your view survives setting changes."),
        paste("Download the interactive map or a PNG snapshot from the Map",
              "tab's sidebar; the data itself downloads from the Export",
              "tab.")),
      tips = c(
        paste("No coordinates? Switch Map type to Shaded regions - it joins",
              "your data to built-in state / county / country boundaries by",
              "name."),
        paste("Skewed values (population, income) color better on the log or",
              "quantile scale."),
        paste("Dense point clouds: clustering keeps the map responsive, or",
              "combine points by admin area."))),
    nav_panel(tagList(icon("file-arrow-up"),    " Import"), value = "import",
              importUI("imp", examples = EXAMPLES_MAP)),
    nav_panel(tagList(icon("map-location-dot"), " Map"),    value = "map",
              mapUI("map")),
    nav_panel(tagList(icon("file-export"),      " Export & Report"),
              value = "export",
              exportReportUI("ex", "rep", preview = FALSE,
                             default_title = "Map Report"))
  )

  server <- function(input, output, session) {
    imported <- importServer("imp", examples = EXAMPLES_MAP)
    map_out <- mapServer("map", imported)            # map + its own downloads
    exportServer("ex", imported, preview = FALSE)    # data-only download
    reportServer("rep", imported,
                 maps     = shiny::reactive(map_out()$maps),
                 map_code = shiny::reactive(map_out()$code),
                 default_title = "Map Report")
  }

  shiny::shinyApp(ui, server)
}

#' @rdname map_tool_app
#' @export
run_map_tool <- function(...) {
  shiny::runApp(map_tool_app(), ...)
}
