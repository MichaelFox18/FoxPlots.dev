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
      tags$h6(class = "mt-2 mb-1", "Sections to include:"),
      # The picker renders ONCE (its choices are fixed at startup), so ticks
      # survive every data change; live status lives in `contents` below it.
      uiOutput(ns("section_picker")),
      uiOutput(ns("contents")),
      uiOutput(ns("empty_hint"))
    )
  )
}

reportServer <- function(id, data_in, summary_tbl = NULL, plots = NULL,
                         plot_code = NULL, maps = NULL, map_code = NULL,
                         comparison = NULL, model = NULL, mixed = NULL,
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
                  regression = !is.null(model),
                  mixed      = !is.null(mixed))
    STAGE_LABEL <- c(summary = "Summary table", charts = "Charts",
                     maps = "Maps", comparison = "Group comparison",
                     regression = "Regression", mixed = "Mixed model")
    STAGE_VERB  <- c(summary = "a summary", charts = "charts", maps = "a map",
                     comparison = "a comparison", regression = "a model",
                     mixed = "a mixed model")
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
      # Only evaluate what is still ticked: reading an unticked stage costs
      # real work (a map's returned reactive rebuilds its widget), and the
      # checklist discards the answer for excluded rows anyway.
      sel  <- chosen()
      want <- function(k, r) if (k %in% sel) read_opt(r) else NULL
      s <- want("summary", summary_tbl); p <- want("charts", plots)
      mp <- want("maps", maps)
      cmp <- want("comparison", comparison); m <- want("regression", model)
      mx <- want("mixed", mixed)
      list(
        overview   = is.data.frame(data_in()),
        summary    = is.data.frame(s) && nrow(s) > 0,
        charts     = !is.null(p) && length(p) > 0,
        maps       = !is.null(mp) && length(mp) > 0,
        comparison = is.list(cmp) && !is.null(cmp$mode),
        regression = inherits(m, "lm"),
        mixed      = length(Filter(Negate(is.null), mx %||% list())) > 0
      )
    })

    # The section picker. Rendered once -- `on_stages` is fixed when the app
    # wires the module, so nothing reactive is read here and the user's ticks
    # are never reset by a data change. Everything starts ticked, matching the
    # previous always-include-everything behaviour.
    output$section_picker <- renderUI({
      if (!length(on_stages))
        return(helpText("This report covers the data overview."))
      tagList(lapply(on_stages, function(k)
        checkboxInput(ns(paste0("inc_", k)), unname(STAGE_LABEL[[k]]),
                      value = TRUE)))
    })

    # One checkbox per stage rather than a checkboxGroupInput: a group input
    # reads NULL both before it renders and when the user unticks everything,
    # and those mean opposite things here. A per-stage input is unambiguous --
    # NULL only ever means "not rendered yet", which defaults to included.
    chosen <- reactive({
      on_stages[vapply(on_stages,
                       function(k) isTRUE(input[[paste0("inc_", k)]] %||% TRUE),
                       logical(1))]
    })

    output$contents <- renderUI({
      a <- avail()
      sel <- chosen()
      item <- function(ok, label, included = TRUE) tags$li(
        class = "mb-1",
        icon(if (!included) "minus" else if (ok) "circle-check" else "circle",
             class = if (!included) "text-muted"
                     else if (ok) "text-success" else "text-muted"),
        tags$span(style = "margin-left:6px;",
                  class = if (!included) "text-muted" else NULL, label),
        if (!included) tags$span(class = "text-muted small", " - left out")
        else if (!ok) tags$span(class = "text-muted small", " - not added yet"))
      items <- list(item(a$overview, "Data overview (always included)"))
      for (k in on_stages)
        items <- c(items, list(item(isTRUE(a[[k]]),
                                    unname(STAGE_LABEL[[k]]),
                                    included = k %in% sel)))
      tagList(
        tags$h6(class = "mt-3 mb-1", "In the report:"),
        do.call(tags$ul, c(list(class = "list-unstyled"), items)))
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
        # Deselected sections are dropped HERE, before report_spec() sees
        # them: the spec's own predicates then switch the section (and its
        # table-of-contents entry) off for free, AND render_report never pays
        # to rasterize an artifact nobody asked for -- a deselected map skips
        # a ~7s headless-browser snapshot.
        sel <- chosen()
        inc <- function(k, x) if (k %in% sel) x else NULL
        spec <- report_spec(
          data        = d,
          summary_tbl = inc("summary", read_opt(summary_tbl)),
          plots       = inc("charts", read_opt(plots)),
          plot_code   = inc("charts", read_opt(plot_code)),
          maps        = inc("maps", read_opt(maps)),
          map_code    = inc("maps", read_opt(map_code)),
          comparison  = inc("comparison", read_opt(comparison)),
          model       = inc("regression", read_opt(model)),
          mixed       = inc("mixed", read_opt(mixed)),
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

# ============================================================
# Export & Report -- one nav tab, two sub-tabs
# ============================================================
# Composes the two terminal stages into a single navbar entry: "Data &
# downloads" (today's Export UI) and "Full report" (today's Report UI).
# Servers are untouched -- the launcher still calls exportServer() and
# reportServer() with the same ids -- so Pattern A wiring is preserved and
# the four export-only apps keep plain exportUI() with no navset shell.
# Neither panel sits in any launcher's fillable set, so the sub-tabs scroll
# naturally.
exportReportUI <- function(id_export, id_report = NULL, preview = TRUE,
                           default_title = "Data Explorer Report") {
  panels <- list(
    nav_panel(tagList(icon("file-export"), " Data & downloads"),
              exportUI(id_export, preview = preview)),
    if (!is.null(id_report))
      nav_panel(tagList(icon("file-lines"), " Full report"),
                reportUI(id_report, default_title = default_title)))
  do.call(navset_card_tab, Filter(Negate(is.null), panels))
}
