# ============================================================
# run_data_explorer.R -- the full Data Explorer app
# ============================================================
# Import -> Reshape -> Summarize -> Visualize -> Compare -> Regression ->
# Export -> Report, each a shared module wired Pattern A (every stage returns a
# reactive feeding the next). The UI + server live in data_explorer_app(); the
# internal mod_* modules resolve because this function is in the package
# namespace.

#' The Data Explorer app
#'
#' The full point-and-click pipeline: About, Import, Reshape, Summarize,
#' Visualize, Compare Groups, Regression, Export, and Report. Each stage is a
#' shared Shiny module; every stage returns a reactive that feeds the next.
#'
#' `data_explorer_app()` builds and returns the Shiny app object (handy for
#' deployment or embedding); `run_data_explorer()` launches it in a browser.
#'
#' @param ... Passed on to [shiny::runApp()].
#' @return `data_explorer_app()` returns a [shiny::shinyApp()] object;
#'   `run_data_explorer()` runs the app (called for its side effect).
#' @examples
#' if (interactive()) run_data_explorer()
#' @export
data_explorer_app <- function() {
  options(shiny.maxRequestSize = 250 * 1024^2)   # lift the 5 MB upload cap

  # The import stage's three built-in examples (a custom `examples` list
  # REPLACES them, so they are restated here) plus the map-ready Florida sites.
  ex <- list(
    mtcars = local({
      d <- as.data.frame(datasets::mtcars); d$car <- rownames(d)
      rownames(d) <- NULL; d
    }),
    relig_income    = as.data.frame(tidyr::relig_income),
    fish_encounters = as.data.frame(tidyr::fish_encounters),
    sites           = make_map_example_data())
  ex_choices <- c("mtcars (cars)"           = "mtcars",
                  "relig_income (wide)"     = "relig_income",
                  "fish_encounters (long)"  = "fish_encounters",
                  "Florida sites (map)"     = "sites")

  about_panel <- nav_panel(
    title = tagList(icon("circle-info"), " About"),
    value = "about",
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header(icon("compass"), " What is Data Explorer?"),
        tags$div(
          class = "px-2",
          tags$p("A point-and-click tool for UF/IFAS students to import, clean, ",
                 "reshape, summarize, visualize, model, and export tabular data \u2014 ",
                 "no R code required."),
          tags$p(class = "mb-1", tags$b("The workflow runs left to right:")),
          tags$ol(
            class = "px-3",
            tags$li(tags$b("Import"), " \u2014 upload CSV/Excel/TSV/RDS or load an example, ",
                    "then run a Data Health check (including extreme-outlier ",
                    "flagging), recast column types, and filter to just the rows ",
                    "you want. You can also save the whole session and restore it ",
                    "later."),
            tags$li(tags$b("Reshape"), " \u2014 stack, split, transpose, sort, subset, or ",
                    "summarize by group (optional)."),
            tags$li(tags$b("Summarize"), " \u2014 count, mean, median, mode, min, max, SD, ",
                    "SE, and IQR by group, or category proportions with confidence ",
                    "intervals."),
            tags$li(tags$b("Visualize"), " \u2014 up to four charts at once (scatter, line, ",
                    "bar, histogram, density, box, violin, mean \u00b1 error, pie, ",
                    "hexbin, correlation heatmap) with copy-ready ggplot2 code; a ",
                    "scatter can be sized by a variable to make a bubble chart."),
            tags$li(tags$b("Map"), sprintf(
              " %s put rows with latitude / longitude columns on an interactive basemap: color and size by your variables, popups, clustering, and HTML / PNG download.",
              SYM_MDASH)),
            tags$li(tags$b("Compare Groups"), " \u2014 t-test / ANOVA (or non-parametric) ",
                    "across groups, or chi-square between two categories, with ",
                    "assumption checks and effect sizes."),
            tags$li(tags$b("Regression"), " \u2014 fit linear, multiple, or polynomial ",
                    "models with diagnostics and a plain-English interpretation."),
            tags$li(tags$b("Export"), " \u2014 download the data, the charts, the summary, ",
                    "and the model results."),
            tags$li(tags$b("Report"), " \u2014 one click bundles everything you made into ",
                    "a report: a self-contained HTML file or an editable Word ",
                    "document (with an optional \u201cshow the R code\u201d toggle).")
          )
        )
      ),
      card(
        card_header(icon("lightbulb"), " Tips"),
        tags$ul(
          class = "px-3",
          tags$li("Each tab feeds the next: what you import and reshape is what gets ",
                  "summarized, visualized, modeled, and exported."),
          tags$li("Leave Reshape on \u201cNone\u201d to pass data straight through unchanged."),
          tags$li("Hover the ", icon("circle-question"), " icons for plain-English help."),
          tags$li("On the Visualize tab, use the ", icon("expand"),
                  " full-screen button on any chart to see it in detail \u2014 handy for ",
                  "faceted (small-multiple) charts."),
          tags$li("Numbers stored as text, missing-value markers, duplicate rows, and ",
                  "extreme outliers are caught by Data Health on the Import tab \u2014 ",
                  "fixes are reversible."),
          tags$li("Use ", tags$b("Save / restore session"), " on the Import tab to ",
                  "download your progress (data + cleaning + filters + reshape) and ",
                  "pick it back up later.")
        )
      )
    )
  )

  ui <- page_navbar(
    title        = uf_title("Data Explorer"),
    window_title = "UF/IFAS Data Explorer",
    theme        = uf_theme(),
    # regression is deliberately NOT fillable: its tabs are long scrolling
    # stacks (same layout as the standalone tool), and a fillable host
    # squeezes the EMMeans controls card into a clipped inner scroller.
    fillable     = c("reshape", "visualize", "compare"),

    about_panel,
    nav_panel(tagList(icon("file-arrow-up"), " Import"),    value = "import",
              importUI("imp", example_choices = ex_choices)),
    nav_panel(tagList(icon("table-cells"),   " Reshape"),   value = "reshape",
              reshapeUI("rs")),
    nav_panel(tagList(icon("layer-group"),   " Summarize"), value = "summarize",
              summarizeUI("sm")),
    nav_panel(tagList(icon("chart-line"),    " Visualize"),  value = "visualize",
              visualizeUI("viz")),
    nav_panel(tagList(icon("map-location-dot"), " Map"),     value = "map",
              mapUI("map")),
    nav_panel(tagList(icon("flask-vial"),    " Compare Groups"), value = "compare",
              compareUI("cmp")),
    nav_panel(tagList(icon("chart-simple"),  " Regression"), value = "regression",
              regressionUI("reg")),
    nav_panel(tagList(icon("file-export"),   " Export & Report"),
              value = "export",
              exportReportUI("ex", "rep"))
  )

  server <- function(input, output, session) {
    # Shared session store (app-wide state): lets Save gather the reshape stage's
    # settings and a Restore stage them back, without modules reaching into each
    # other. Only Import + Reshape touch it.
    session_store <- reactiveValues(reshape_state = NULL, pending_reshape = NULL)

    imported   <- importServer("imp", examples = ex,          # Import -> reactive(data|NULL)
                               store = session_store)
    working    <- reshapeServer("rs", imported, store = session_store)  # Reshape -> working data
    summary_t  <- summarizeServer("sm", working)   # Summarize -> reactive(summary table)
    viz        <- visualizeServer("viz", working)  # Visualize -> reactive(list(plots, code))
    plots      <- reactive(viz()$plots)            #   the ggplot list (for Export + Report)
    plot_code  <- reactive(viz()$code)             #   the matching ggplot2 code (for Report)
    map_out    <- mapServer("map", working)        # Map -> reactive(list(maps, code))
    maps       <- reactive(map_out()$maps)         #   leaflet widgets (Report only --
    map_code   <- reactive(map_out()$code)         #   mod_export's chart slot is
                                                   #   ggplot-only, and the map's own
                                                   #   HTML/PNG downloads live in mod_map)
    comparison <- compareServer("cmp", working)    # Compare   -> reactive(result list | NULL)
    model      <- regressionServer("reg", working) # Regression -> reactive(fitted lm)

    exportServer("ex", working, plots = plots, model = model,  # data + charts +
                 summary_tbl = summary_t)                      # model + summary

    reportServer("rep", working,                   # one-click HTML/Word report of
                 summary_tbl = summary_t, plots = plots, plot_code = plot_code,
                 maps = maps, map_code = map_code,
                 comparison = comparison, model = model)       # the whole session
  }

  shiny::shinyApp(ui, server)
}

#' @rdname data_explorer_app
#' @export
run_data_explorer <- function(...) {
  shiny::runApp(data_explorer_app(), ...)
}
