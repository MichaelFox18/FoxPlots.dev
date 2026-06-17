# ============================================================
# mod_report.R — one-click session report (self-contained HTML)
# ============================================================
# The capstone stage: assembles whatever the user produced this session into a
# single, portable .html report — data overview, summary, charts, group
# comparison, regression — with an optional "show the R code" toggle. Thin
# wrapper over R/helpers_report.R (which is pandoc-free, so this works anywhere).
#
# reportServer(id, data_in, summary_tbl = NULL, plots = NULL, plot_code = NULL,
#              comparison = NULL, model = NULL)
#   data_in     : reactive(data frame | NULL)            — required
#   summary_tbl : reactive(data frame | NULL)            — from mod_summarize
#   plots       : reactive(list of ggplots | NULL)       — from mod_visualize
#   plot_code   : reactive(list of code strings | NULL)  — aligned to `plots`
#   comparison  : reactive(list | NULL)                  — from mod_compare
#   model       : reactive(lm | NULL)                    — from mod_regression
# Each optional reactive may be NULL (a mini-app that omits a stage); a section
# appears in the report only when its reactive yields usable content.

reportUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      h5("Build report"),
      textInput(ns("title"), "Report title", value = "Data Explorer Report"),
      radioButtons(ns("format"),
        tagList("Format", info_tip(
          "Web page = one polished, self-contained HTML file, great for sharing ",
          "or archiving. Word = an editable .docx you can open in Word / Google ",
          "Docs to delete sections and add your own text (intro, bios, notes).")),
        choices = c("Web page (HTML)" = "html", "Word document (.docx)" = "docx"),
        selected = "html"),
      checkboxInput(ns("show_code"),
        tagList("Include the R code", info_tip(
          "Adds reproducible R (ggplot2, the model, the tests) under each ",
          "section — turn off for a clean, non-technical results write-up.")),
        value = FALSE),
      hr(),
      downloadButton(ns("download"), "Download report",
                     class = "btn-primary w-100"),
      helpText("HTML is one self-contained file that opens in any browser. ",
               "Word is fully editable — cut sections, add your own write-up.")
    ),
    card(
      card_header(icon("file-lines"), " Your report"),
      tags$p(class = "px-1",
        "The report auto-assembles everything you've produced this session. ",
        "Sections appear only for the stages you actually used — so build a ",
        "summary, charts, a comparison, or a model on the earlier tabs and ",
        "they'll show up here."),
      tags$h6(class = "mt-2 mb-1", "Included right now:"),
      uiOutput(ns("contents")),
      uiOutput(ns("empty_hint"))
    )
  )
}

reportServer <- function(id, data_in, summary_tbl = NULL, plots = NULL,
                         plot_code = NULL, comparison = NULL, model = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Safely evaluate an optional reactive (validation errors -> "absent").
    read_opt <- function(r) if (is.null(r)) NULL else tryCatch(r(), error = function(e) NULL)

    # Which sections currently have usable content (drives the live checklist).
    avail <- reactive({
      s <- read_opt(summary_tbl); p <- read_opt(plots)
      cmp <- read_opt(comparison); m <- read_opt(model)
      list(
        overview   = is.data.frame(data_in()),
        summary    = is.data.frame(s) && nrow(s) > 0,
        charts     = !is.null(p) && length(p) > 0,
        comparison = is.list(cmp) && !is.null(cmp$mode),
        regression = inherits(m, "lm")
      )
    })

    output$contents <- renderUI({
      a <- avail()
      item <- function(ok, label) tags$li(
        class = "mb-1",
        icon(if (ok) "circle-check" else "circle",
             class = if (ok) "text-success" else "text-muted"),
        tags$span(style = "margin-left:6px;", label),
        if (!ok) tags$span(class = "text-muted small", " — not added yet"))
      tags$ul(class = "list-unstyled",
        item(a$overview,   "Data overview (always included)"),
        item(a$summary,    "Summary table"),
        item(a$charts,     "Charts"),
        item(a$comparison, "Group comparison"),
        item(a$regression, "Regression"))
    })

    output$empty_hint <- renderUI({
      if (isTRUE(avail()$overview)) return(NULL)
      div(class = "alert alert-info py-2 px-3 small",
          icon("circle-info"),
          " Import data on the Import tab to enable the report.")
    })

    safe_stem <- reactive({
      stem <- gsub("[^A-Za-z0-9._-]+", "_", input$title %||% "")
      if (nzchar(stem)) stem else "data-explorer-report"
    })

    output$download <- downloadHandler(
      filename = function()
        paste0(safe_stem(), if (identical(input$format, "docx")) ".docx" else ".html"),
      content  = function(file) {
        d <- data_in()
        validate(need(is.data.frame(d),
                      "Import data on the Import tab before building a report."))
        fmt <- input$format %||% "html"
        spec <- report_spec(
          data        = d,
          summary_tbl = read_opt(summary_tbl),
          plots       = read_opt(plots),
          plot_code   = read_opt(plot_code),
          comparison  = read_opt(comparison),
          model       = read_opt(model),
          title       = label_or(input$title, "Data Explorer Report"),
          show_code   = isTRUE(input$show_code),
          logo        = uf_logo_uri())
        withProgress(message = "Building report…", value = 0.4, {
          render_report(spec, file, format = fmt)
          incProgress(0.6)
        })
      }
    )

    invisible(NULL)
  })
}
