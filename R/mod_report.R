# ============================================================
# mod_report.R -- one-click session report (self-contained HTML)
# ============================================================
# The capstone stage: assembles whatever the user produced this session into a
# single, portable .html report -- data overview, summary, charts, group
# comparison, regression -- with an optional "show the R code" toggle. Thin
# wrapper over R/helpers_report.R (which is pandoc-free, so this works anywhere).
#
# reportServer(id, data_in, summary_tbl = NULL, plots = NULL, plot_code = NULL,
#              comparison = NULL, model = NULL)
#   data_in     : reactive(data frame | NULL)            -- required
#   summary_tbl : reactive(data frame | NULL)            -- from mod_summarize
#   plots       : reactive(list of ggplots | NULL)       -- from mod_visualize
#   plot_code   : reactive(list of code strings | NULL)  -- aligned to `plots`
#   comparison  : reactive(list | NULL)                  -- from mod_compare
#   model       : reactive(lm | NULL)                    -- from mod_regression
#   default_title : the report title an app starts with (also the filename stem)
# Each optional reactive may be NULL (a mini-app that omits a stage); a section
# appears in the report only when its reactive yields usable content.

reportUI <- function(id, default_title = "Data Explorer Report") {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      h5("Build report"),
      textInput(ns("title"), "Report title", value = default_title),
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
          "section \u2014 turn off for a clean, non-technical results write-up.")),
        value = FALSE),
      hr(),
      downloadButton(ns("download"), "Download report",
                     class = "btn-success w-100"),
      helpText("HTML is one self-contained file that opens in any browser. ",
               "Word is fully editable \u2014 cut sections, add your own write-up.")
    ),
    card(
      card_header(icon("file-lines"), " Your report"),
      # Server-rendered: only the stages this app actually wired are named.
      uiOutput(ns("intro")),
      tags$h6(class = "mt-2 mb-1", "Included right now:"),
      uiOutput(ns("contents")),
      uiOutput(ns("empty_hint"))
    )
  )
}

reportServer <- function(id, data_in, summary_tbl = NULL, plots = NULL,
                         plot_code = NULL, maps = NULL, map_code = NULL,
                         comparison = NULL, model = NULL,
                         default_title = "Data Explorer Report") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Safely evaluate an optional reactive (validation errors -> "absent").
    read_opt <- function(r) if (is.null(r)) NULL else tryCatch(r(), error = function(e) NULL)

    # Which stages this app actually WIRED (i.e. supplied a reactive for). A
    # mini-app that omits a stage must not advertise it: the Compare Groups app
    # has no Summarize / Visualize / Regression tab, so listing those sections
    # would be nonsense. Fixed at server start -- the arguments never change.
    wired <- list(summary    = !is.null(summary_tbl),
                  charts     = !is.null(plots),
                  maps       = !is.null(maps),
                  comparison = !is.null(comparison),
                  regression = !is.null(model))
    STAGE_LABEL <- c(summary = "Summary table", charts = "Charts",
                     maps = "Maps", comparison = "Group comparison",
                     regression = "Regression")
    STAGE_VERB  <- c(summary = "a summary", charts = "charts", maps = "a map",
                     comparison = "a comparison", regression = "a model")
    on_stages   <- names(wired)[vapply(wired, isTRUE, logical(1))]

    # "a", "a or b", "a, b, or c"
    and_list <- function(x) {
      x <- unname(x)
      if (length(x) <= 1L) return(paste0(x, collapse = ""))
      if (length(x) == 2L) return(paste(x, collapse = " or "))
      paste0(paste(x[-length(x)], collapse = ", "), ", or ", x[length(x)])
    }

    output$intro <- renderUI({
      lead <- "The report auto-assembles everything you've produced this session. "
      tail <- if (!length(on_stages)) "It covers the data overview."
              else sprintf(
                "Sections appear only for the stages you actually used - so build %s on the earlier tabs and %s show up here.",
                and_list(STAGE_VERB[on_stages]),
                if (length(on_stages) == 1L) "it will" else "they'll")
      tags$p(class = "px-1", lead, tail)
    })

    # Which sections currently have usable content (drives the live checklist).
    avail <- reactive({
      s <- read_opt(summary_tbl); p <- read_opt(plots)
      mp <- read_opt(maps)
      cmp <- read_opt(comparison); m <- read_opt(model)
      list(
        overview   = is.data.frame(data_in()),
        summary    = is.data.frame(s) && nrow(s) > 0,
        charts     = !is.null(p) && length(p) > 0,
        maps       = !is.null(mp) && length(mp) > 0,
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
        if (!ok) tags$span(class = "text-muted small", " - not added yet"))
      items <- list(item(a$overview, "Data overview (always included)"))
      for (k in on_stages)
        items <- c(items, list(item(isTRUE(a[[k]]), unname(STAGE_LABEL[[k]]))))
      do.call(tags$ul, c(list(class = "list-unstyled"), items))
    })

    output$empty_hint <- renderUI({
      if (isTRUE(avail()$overview)) return(NULL)
      div(class = "alert alert-info py-2 px-3 small",
          icon("circle-info"),
          " Import data on the Import tab to enable the report.")
    })

    safe_stem <- reactive({
      stem <- gsub("[^A-Za-z0-9._-]+", "_", input$title %||% "")
      if (nzchar(stem)) stem else gsub("[^A-Za-z0-9._-]+", "_", default_title)
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
          maps        = read_opt(maps),
          map_code    = read_opt(map_code),
          comparison  = read_opt(comparison),
          model       = read_opt(model),
          title       = label_or(input$title, default_title),
          show_code   = isTRUE(input$show_code),
          logo        = uf_logo_uri())
        # Each map costs a headless-browser launch (several seconds), so say so
        # rather than letting the download look frozen.
        msg <- if (isTRUE(spec$sections[["maps"]]))
          "Building report (snapshotting the map\u2026)" else "Building report\u2026"
        tryCatch(
          withProgress(message = msg, value = 0.4, {
            render_report(spec, file, format = fmt)
            incProgress(0.6)
          }),
          error = function(e) {
            showNotification(paste("Report failed:", conditionMessage(e)),
                             type = "error", duration = 8)
            validate(need(FALSE, paste("Could not build the report:",
                                       conditionMessage(e))))
          })
      }
    )

    invisible(NULL)
  })
}
