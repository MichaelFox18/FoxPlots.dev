# ============================================================
# mod_import.R -- the Import stage
# ============================================================
# Upload a file (CSV / Excel / TSV / RDS) or load a built-in example, with the
# text-file parse options and an Excel worksheet picker -- plus a Data Health
# panel (diagnose + opt-in reversible fixes), a Change-Variable-Types tool, a
# Rename-Columns tool, an at-a-glance summary, and a per-column profile.
# Pattern A: importServer()
# RETURNS the working data as a reactive (NULL until loaded) for the next stage.
# Fixes/conversions mutate the working copy; the originally-loaded data is kept
# so "Revert to original" can restore it.
#
# Requires R/helpers_io.R (read_file_data), R/helpers_clean.R (clean_specs,
# convert_column, ...), and R/helpers_stats.R (friendly_type, column_profile,
# data_glance).

# `examples` names the built-in datasets this app's Import menu offers --
# registry keys (see EXAMPLE_SETS in helpers_examples.R), or a named list of
# data frames for ad-hoc use. importUI and importServer must receive the SAME
# value; both resolve it through as_example_spec(), so the menu labels and the
# loadable data come from one lookup and cannot drift apart.
importUI <- function(id, examples = EXAMPLES_DEFAULT) {
  ns <- NS(id)
  example_choices <- example_menu(as_example_spec(examples))
  layout_sidebar(
    sidebar = sidebar(
      width = 290,
      h5("Upload a file"),
      # Rendered server-side so "Clear data" can reset it (no shinyjs needed).
      uiOutput(ns("file_ui")),
      uiOutput(ns("ui_sheet")),
      hr(),
      h6("Text-file options"),
      checkboxInput(ns("header"), "First row is a header", TRUE),
      selectInput(ns("sep"), "Column separator",
                  choices = c("Comma (,)" = ",", "Semicolon (;)" = ";",
                              "Tab" = "\t", "Space" = " ")),
      selectInput(ns("dec"), "Decimal point",
                  choices = c("Period (.)" = ".", "Comma (,)" = ",")),
      hr(),
      h6("\u2026or load an example"),
      selectInput(ns("example"), NULL, choices = example_choices),
      actionButton(ns("load_example"), "Load example",
                   class = "btn-outline-primary w-100", icon = icon("table")),
      hr(),
      actionButton(ns("clear_data"), "Clear data",
                   class = "btn-outline-danger w-100", icon = icon("trash")),
      # Save / restore session controls -- rendered only when the app wires a
      # shared session store into importServer (the mini-apps don't).
      uiOutput(ns("session_ui"))
    ),
    card(
      card_header(icon("broom"), " Data Health"),
      uiOutput(ns("data_health_ui"))
    ),
    card(
      card_header(icon("right-left"), " Change variable types"),
      uiOutput(ns("convert_ui"))
    ),
    card(
      card_header(icon("pen-to-square"), " Rename columns"),
      uiOutput(ns("rename_ui"))
    ),
    card(
      card_header(icon("filter"), " Filter rows"),
      uiOutput(ns("filter_builder")),
      uiOutput(ns("filter_active"))
    ),
    layout_columns(
      col_widths = c(8, 4),
      card(
        card_header(icon("eye"), " Data preview"),
        textOutput(ns("caption")),
        DT::DTOutput(ns("preview"))
      ),
      card(
        card_header(icon("chart-bar"), " Summary"),
        uiOutput(ns("glance_ui")),
        accordion(
          open = FALSE,
          accordion_panel(
            tagList(icon("terminal"), " Advanced summary statistics"),
            value = "adv", verbatimTextOutput(ns("summary_raw"))
          )
        )
      )
    ),
    card(
      card_header(icon("list"), " Column profile"),
      DT::DTOutput(ns("profile"))
    )
  )
}

importServer <- function(id, examples = EXAMPLES_DEFAULT, store = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Built-in sample datasets: one lookup serves both the menu (importUI) and
    # the data (the load handler below). Resolved once -- an unknown key stops
    # here, at server init, not at click time.
    example_spec <- as_example_spec(examples)

    # data     = working copy (after Health fixes / type conversions)
    # data_raw = exactly as loaded, so "Revert to original" can restore it
    rv <- reactiveValues(data = NULL, data_raw = NULL, upload = NULL,
                         sheets = NULL, loaded_sheet = NULL, source = NULL,
                         file_token = 0L, filters = list())

    # Local type predicates (kept here so this module needn't source helpers_plot;
    # reshape_tool / combine_tool don't attach it).
    is_num_col <- function(x) is.numeric(x) && !inherits(x, c("Date", "POSIXct", "POSIXt"))
    is_date_x  <- function(x) inherits(x, c("Date", "POSIXct", "POSIXt"))

    # The file input lives in an output so Clear can re-render an empty one.
    output$file_ui <- renderUI({
      rv$file_token  # take a dependency: bumping the token resets the input
      fileInput(ns("file"), NULL,
                accept      = c(".csv", ".tsv", ".txt", ".xlsx", ".xls", ".rds"),
                buttonLabel = "Browse\u2026",
                placeholder = "CSV, Excel, TSV, RDS\u2026")
    })

    # Read the current upload (rv$upload) for the chosen Excel sheet (or NULL
    # for non-Excel) and install it as the working data.
    load_upload <- function(sheet = NULL) {
      u <- rv$upload
      req(u)
      tryCatch({
        d <- read_file_data(u$path, u$ext, header = input$header,
                            sep = input$sep, dec = input$dec,
                            sheet = sheet %||% 1)
        rv$data <- d; rv$data_raw <- d; rv$filters <- list()
        rv$loaded_sheet <- sheet; rv$source <- u$name
        nh <- attr(d, "n_skip_head") %||% 0L
        nt <- attr(d, "n_skip_tail") %||% 0L
        showNotification(
          if (!is.null(sheet)) sprintf("Loaded %s \u2014 sheet \u201c%s\u201d", u$name, sheet)
          else paste("Loaded:", u$name),
          type = "message")
        if (nh > 0 || nt > 0)
          showNotification(
            sprintf("Auto-skipped %d title line(s) at the top and %d footnote line(s) at the bottom.",
                    nh, nt),
            type = "warning", duration = 8)
      }, error = function(e)
        showNotification(paste("Read error:", e$message), type = "error",
                         duration = 8))
    }

    observeEvent(input$file, {
      req(input$file)
      ext <- tolower(tools::file_ext(input$file$name))
      rv$upload <- list(path = input$file$datapath, ext = ext,
                        name = input$file$name)
      rv$sheets <- if (ext %in% c("xlsx", "xls"))
        tryCatch(readxl::excel_sheets(input$file$datapath),
                 error = function(e) NULL) else NULL
      load_upload(sheet = if (length(rv$sheets)) rv$sheets[[1]] else NULL)
    })

    # Worksheet picker -- only for a multi-sheet Excel workbook.
    output$ui_sheet <- renderUI({
      if (length(rv$sheets) < 2) return(NULL)
      selectInput(ns("sheet"), "Worksheet", choices = rv$sheets,
                  selected = rv$loaded_sheet %||% rv$sheets[[1]])
    })

    observeEvent(input$sheet, {
      req(rv$upload, input$sheet)
      if (!isTRUE(rv$upload$ext %in% c("xlsx", "xls"))) return()
      if (identical(input$sheet, rv$loaded_sheet)) return()
      load_upload(sheet = input$sheet)
    }, ignoreInit = TRUE)

    observeEvent(input$load_example, {
      req(input$example)
      # example_build() refuses a broken/unknown entry, so a failed load can
      # never announce success (the pre-0.11.0 bug: a mistyped key loaded
      # nothing and still notified "Loaded example: <key>").
      d <- tryCatch(example_build(example_spec, input$example),
                    error = function(e) e)
      if (inherits(d, "error")) {
        showNotification(paste("Couldn't load that example -",
                               conditionMessage(d)),
                         type = "error", duration = 8)
        return()
      }
      rv$data <- d; rv$data_raw <- d; rv$filters <- list()
      rv$upload <- NULL; rv$sheets <- NULL; rv$loaded_sheet <- NULL
      rv$source <- paste0("example: ", input$example)
      showNotification(sprintf("Loaded example: %s (%s rows x %d columns)",
                               input$example,
                               format(nrow(d), big.mark = ","), ncol(d)),
                       type = "message")
    })

    observeEvent(input$clear_data, {
      rv$data <- NULL; rv$data_raw <- NULL; rv$upload <- NULL; rv$sheets <- NULL
      rv$loaded_sheet <- NULL; rv$source <- NULL; rv$filters <- list()
      rv$file_token <- rv$file_token + 1L     # forces the file input to reset
      showNotification("Cleared the loaded data.", type = "message")
    })

    # -- Data Health: diagnose + opt-in, reversible fixes ------
    output$data_health_ui <- renderUI({
      if (is.null(rv$data))
        return(helpText("Load a dataset to run a quick health check."))
      cleaned    <- !is.null(rv$data_raw) && !identical(rv$data, rv$data_raw)
      revert_btn <- if (cleaned)
        actionButton(ns("dh_revert"), "Revert to original",
                     class = "btn-outline-secondary btn-sm",
                     icon = icon("rotate-left"))
      iss <- detect_issues(rv$data)
      if (!length(iss))
        return(tagList(
          div(class = "alert alert-success py-2 px-3", icon("circle-check"),
              if (cleaned) " All clear \u2014 fixes applied. Your data is ready to explore."
              else         " No common data issues detected \u2014 your data is ready to explore."),
          revert_btn))
      ids  <- unname(vapply(iss, `[[`, character(1), "id"))
      defs <- unname(vapply(iss, `[[`, logical(1), "default"))
      nms  <- unname(lapply(iss, function(z) HTML(z$desc)))
      tagList(
        tags$p(sprintf("Spotted %d potential issue%s. Tick the fixes you want, then Apply \u2014 everything is reversible:",
                       length(iss), if (length(iss) == 1) "" else "s")),
        checkboxGroupInput(ns("dh_fixes"), NULL, choiceNames = nms,
                           choiceValues = ids, selected = ids[defs]),
        div(class = "d-flex gap-2",
            actionButton(ns("dh_apply"), "Apply selected fixes",
                         class = "btn-primary btn-sm", icon = icon("broom")),
            revert_btn),
        tags$div(class = "form-text mt-2",
                 "Fixes apply to a working copy used by the rest of the app; Revert restores the file exactly as uploaded.")
      )
    })

    observeEvent(input$dh_apply, {
      req(rv$data)
      ids <- input$dh_fixes
      if (is.null(ids) || !length(ids)) {
        showNotification("No fixes selected.", type = "warning"); return()
      }
      rv$data <- clean_apply(rv$data, ids)
      showNotification(sprintf("Applied %d fix%s.", length(ids),
                               if (length(ids) == 1) "" else "es"), type = "message")
    })

    observeEvent(input$dh_revert, {
      req(rv$data_raw)
      rv$data <- rv$data_raw
      showNotification("Reverted to the originally uploaded data.", type = "message")
    })

    # -- Change variable types (e.g. a numeric code -> factor) --
    output$convert_ui <- renderUI({
      if (is.null(rv$data))
        return(helpText("Load a dataset to change column types."))
      cols    <- names(rv$data)
      types   <- vapply(rv$data, friendly_type, character(1))
      choices <- stats::setNames(cols, sprintf("%s  (currently %s)", cols, types))
      tagList(
        tags$p(class = "mb-2",
          "Recast a column to a different type. The most common need: a number ",
          "that's really a category or code (e.g. ", tags$code("cyl"),
          " = 4/6/8) should be a ", tags$b("factor"),
          " so it's treated as distinct groups in charts, grouping, and ",
          "regression \u2014 not as a quantity."),
        layout_columns(
          col_widths = c(6, 6),
          selectInput(ns("ct_col"),  "Column",     choices = choices),
          selectInput(ns("ct_type"), "Convert to", choices = CONVERT_TYPES)
        ),
        actionButton(ns("ct_apply"), "Convert",
                     class = "btn-primary btn-sm", icon = icon("right-left")),
        tags$div(class = "form-text mt-2",
          "Values that can't be converted become missing (NA) \u2014 you'll be told ",
          "how many. Use Data Health's \u201cRevert to original\u201d to undo all changes.")
      )
    })

    observeEvent(input$ct_apply, {
      req(rv$data, input$ct_col, input$ct_type)
      col <- input$ct_col; to <- input$ct_type
      if (!col %in% names(rv$data)) return()
      x   <- rv$data[[col]]
      cur <- friendly_type(x); if (cur == "text") cur <- "character"
      if (identical(cur, to)) {
        showNotification(sprintf("\u201c%s\u201d is already that type (%s).", col, cur),
                         type = "warning"); return()
      }
      res <- tryCatch(convert_column(x, to), error = function(e) e)
      if (inherits(res, "error")) {
        showNotification(sprintf("Couldn't convert \u201c%s\u201d to %s: %s", col, to,
                                 conditionMessage(res)),
                         type = "error", duration = 8); return()
      }
      new_na <- sum(is.na(res) & !is.na(x))
      rv$data[[col]] <- res
      msg <- sprintf("Converted \u201c%s\u201d to %s.", col, to)
      if (new_na > 0)
        msg <- paste0(msg, sprintf(" %d value%s couldn't be parsed and became NA.",
                                   new_na, if (new_na == 1) "" else "s"))
      showNotification(msg, type = if (new_na > 0) "warning" else "message",
                       duration = 6)
    })

    # -- Rename columns (fix a mangled header, or make a join key's spelling
    # -- match the other table's in the combine tool) --
    output$rename_ui <- renderUI({
      if (is.null(rv$data))
        return(helpText("Load a dataset to rename its columns."))
      cols    <- names(rv$data)
      types   <- vapply(rv$data, friendly_type, character(1))
      choices <- stats::setNames(cols, sprintf("%s  (%s)", cols, types))
      tagList(
        tags$p(class = "mb-2",
          "Give a column a better name \u2014 handy when a header arrives mangled ",
          "(", tags$code("Country.Name"), ") or when two files spell the same ",
          "column differently (", tags$code("Country"), " vs ",
          tags$code("country"), "), which blocks joining on it."),
        layout_columns(
          col_widths = c(6, 6),
          selectInput(ns("rn_col"), "Column",   choices = choices),
          textInput(ns("rn_new"),   "New name", placeholder = "new column name")
        ),
        actionButton(ns("rn_apply"), "Rename",
                     class = "btn-primary btn-sm", icon = icon("pen-to-square")),
        tags$div(class = "form-text mt-2",
          "Filters on the renamed column follow it automatically. Use Data ",
          "Health's \u201cRevert to original\u201d to undo all changes.")
      )
    })

    observeEvent(input$rn_apply, {
      req(rv$data, input$rn_col)
      from <- input$rn_col
      to   <- trimws(input$rn_new %||% "")
      if (!from %in% names(rv$data)) return()
      if (!nzchar(to)) {
        showNotification("Type a new name first.", type = "warning"); return()
      }
      if (identical(from, to)) {
        showNotification(sprintf("\u201c%s\u201d is already called that.", from),
                         type = "warning"); return()
      }
      res <- tryCatch(rename_column(rv$data, from, to), error = function(e) e)
      if (inherits(res, "error")) {
        showNotification(sprintf("Couldn't rename \u201c%s\u201d: %s", from,
                                 conditionMessage(res)),
                         type = "error", duration = 8); return()
      }
      # Filters store bare column names (list(col, op, value)); point any that
      # referenced the old name at the new one. Without this, filter_mask's
      # unknown-column no-op silently UN-filters while the chip still shows
      # the old name. Filters are value-based, so a renamed condition stays
      # semantically valid.
      if (length(rv$filters))
        rv$filters <- lapply(rv$filters, function(cond) {
          if (identical(cond$col, from)) cond$col <- to
          cond
        })
      rv$data <- res
      showNotification(sprintf("Renamed \u201c%s\u201d to \u201c%s\u201d.", from, to),
                       type = "message")
    })

    # -- Filter rows (value-based; multiple conditions AND'd) --
    # filtered() = the working copy with the active conditions applied. This is
    # what flows downstream and what the preview/summary/profile below describe.
    filtered <- reactive({
      d <- rv$data
      if (is.null(d)) return(NULL)
      apply_filters(d, rv$filters)
    })

    output$filter_builder <- renderUI({
      if (is.null(rv$data))
        return(helpText("Load a dataset to filter its rows."))
      tagList(
        # No pointer at Reshape > Subset here: mod_import ships in all eight
        # apps and only two of them have a Reshape tab -- advice must stay
        # followable everywhere this module appears (the chart_hint rule).
        tags$p(class = "mb-2",
          "Keep only the rows that match your conditions \u2014 add as many as you ",
          "like and they combine with ", tags$b("AND"), "."),
        layout_columns(
          col_widths = c(4, 4, 4),
          selectInput(ns("filter_col"),
                      tagList("Column", info_tip(
                        "The column the condition tests. The condition list ",
                        "adapts to its type \u2014 numeric columns get ranges ",
                        "and comparisons, categories get pick-lists, dates ",
                        "get a date range.")),
                      choices = names(rv$data)),
          uiOutput(ns("ui_filter_op")),
          uiOutput(ns("ui_filter_val"))
        ),
        actionButton(ns("filter_add"), "Add filter",
                     class = "btn-primary btn-sm", icon = icon("plus"))
      )
    })

    output$ui_filter_op <- renderUI({
      req(rv$data, input$filter_col %in% names(rv$data))
      x  <- rv$data[[input$filter_col]]
      ch <- if (is_num_col(x))
              c("is between" = "between", "is \u2265" = ">=", "is \u2264" = "<=",
                "is >" = ">", "is <" = "<", "equals" = "==", "does not equal" = "!=")
            else if (is_date_x(x)) c("is between" = "between")
            else c("is any of" = "in", "is none of" = "not_in",
                   "contains text" = "contains")
      selectInput(ns("filter_op"), "Condition", choices = ch)
    })

    output$ui_filter_val <- renderUI({
      req(rv$data, input$filter_col %in% names(rv$data), input$filter_op)
      x <- rv$data[[input$filter_col]]; op <- input$filter_op
      if (is_num_col(x)) {
        rng <- suppressWarnings(range(x, na.rm = TRUE))
        if (!all(is.finite(rng))) rng <- c(0, 0)
        if (identical(op, "between"))
          tagList(
            numericInput(ns("filter_v1"), "From", value = signif(rng[1], 4)),
            numericInput(ns("filter_v2"), "To",   value = signif(rng[2], 4)))
        else
          numericInput(ns("filter_v1"), "Value", value = signif(rng[1], 4))
      } else if (is_date_x(x)) {
        rng <- range(as.Date(x), na.rm = TRUE)
        dateRangeInput(ns("filter_dates"), "Between", start = rng[1], end = rng[2])
      } else {
        if (identical(op, "contains"))
          textInput(ns("filter_text"), "Contains", placeholder = "text to match")
        else
          # Choices arrive via the server-side update below -- a client-side
          # selectize caps how many options it renders, silently hiding values
          # on high-cardinality columns.
          selectizeInput(ns("filter_vals"), "Values",
            choices = NULL, multiple = TRUE,
            options = list(placeholder = "pick one or more"))
      }
    })

    # Server-side value list: streams/searches EVERY distinct value, however
    # many there are (the update lands after the renderUI above re-creates
    # the input -- Shiny defers update messages until outputs are drawn).
    observeEvent(list(input$filter_col, input$filter_op, rv$data), {
      req(rv$data, input$filter_col %in% names(rv$data))
      x <- rv$data[[input$filter_col]]
      if (is_num_col(x) || is_date_x(x)) return()
      if (identical(input$filter_op, "contains")) return()
      updateSelectizeInput(session, "filter_vals",
                           choices = sort(unique(as.character(x))),
                           server = TRUE)
    })

    # Assemble a condition from the builder inputs and append it.
    observeEvent(input$filter_add, {
      req(rv$data, input$filter_col %in% names(rv$data), input$filter_op)
      x <- rv$data[[input$filter_col]]; op <- input$filter_op
      cond <- list(col = input$filter_col, op = op); ok <- TRUE
      if (is_num_col(x)) {
        if (identical(op, "between")) {
          if (is.null(input$filter_v1) || is.null(input$filter_v2)) ok <- FALSE
          else cond$value <- c(input$filter_v1, input$filter_v2)
        } else if (is.null(input$filter_v1)) ok <- FALSE
        else cond$value <- input$filter_v1
      } else if (is_date_x(x)) {
        cond$op <- "between"; cond$value <- as.character(input$filter_dates)
      } else if (identical(op, "contains")) {
        if (!nzchar(input$filter_text %||% "")) ok <- FALSE
        else cond$value <- input$filter_text
      } else {
        if (!length(input$filter_vals)) ok <- FALSE
        else cond$value <- input$filter_vals
      }
      if (!ok) {
        showNotification("Choose a value for this filter.", type = "warning"); return()
      }
      rv$filters <- c(rv$filters, list(cond))
    })

    observeEvent(input$filter_clear, { rv$filters <- list() })

    # Bounded pool of remove-buttons (supports up to 20 active filters).
    for (i in seq_len(20)) local({
      ii <- i
      observeEvent(input[[paste0("rmfilter_", ii)]], {
        if (ii <= length(rv$filters)) rv$filters[[ii]] <- NULL
      }, ignoreInit = TRUE)
    })

    output$filter_active <- renderUI({
      req(rv$data)
      n <- nrow(rv$data); m <- nrow(filtered())
      count_txt <- div(
        class = sprintf("mt-2 fw-semibold %s", if (m < n) "text-primary" else "text-muted"),
        sprintf("Keeping %s of %s rows.", format(m, big.mark = ","),
                format(n, big.mark = ",")))
      if (!length(rv$filters))
        return(tagList(tags$hr(),
          helpText("No filters yet \u2014 all rows pass through."), count_txt))
      chips <- lapply(seq_along(rv$filters), function(i)
        div(class = "d-flex align-items-center gap-2 mb-1",
            tags$span(class = "badge text-bg-primary",
                      describe_condition(rv$filters[[i]])),
            actionButton(ns(paste0("rmfilter_", i)), label = NULL,
                         icon = icon("xmark"),
                         class = "btn btn-sm btn-outline-danger py-0 px-1")))
      tagList(tags$hr(), tags$b("Active filters:"), chips,
              actionButton(ns("filter_clear"), "Clear all", icon = icon("trash"),
                           class = "btn-outline-secondary btn-sm mt-1"),
              count_txt)
    })

    # -- At-a-glance summary + column profile (of the filtered data) --
    output$glance_ui <- renderUI({
      req(filtered()); g <- data_glance(filtered())
      pct <- if (g$n) round(100 * g$complete / g$n) else 0
      tags$ul(
        class = "list-unstyled small mb-2",
        tags$li(tags$b(format(g$n, big.mark = ",")), " rows"),
        tags$li(tags$b(format(g$m, big.mark = ",")), " columns ",
                tags$span(class = "text-muted",
                          sprintf("(%d numeric, %d categorical%s)", g$num, g$cat,
                                  if (g$date) sprintf(", %d date", g$date) else ""))),
        tags$li(tags$b(sprintf("%s (%d%%)", format(g$complete, big.mark = ","), pct)),
                " complete rows")
      )
    })

    output$summary_raw <- renderPrint({ req(filtered()); summary(filtered()) })

    output$profile <- DT::renderDT({
      d <- filtered(); req(is.data.frame(d), nrow(d) >= 1L)
      DT::datatable(column_profile(d), rownames = FALSE,
                    class = "compact stripe hover",
                    options = list(scrollX = TRUE, pageLength = 12, dom = "tip"))
    })

    output$caption <- renderText({
      d <- rv$data
      if (is.null(d))
        return("No data loaded yet \u2014 upload a file or load an example.")
      fd  <- filtered()
      txt <- sprintf("%s \u2014 %s rows \u00d7 %s columns", rv$source %||% "data",
                     format(nrow(fd), big.mark = ","), ncol(fd))
      if (nrow(fd) < nrow(d))
        txt <- paste0(txt, sprintf("  (filtered from %s)",
                                   format(nrow(d), big.mark = ",")))
      txt
    })

    output$preview <- DT::renderDT({
      d <- filtered(); req(is.data.frame(d))
      DT::datatable(utils::head(d, 200), rownames = FALSE,
                    class = "compact stripe hover",
                    options = list(pageLength = 10, scrollX = TRUE))
    })

    # -- Save / restore session (only when a shared store is wired in) --
    # The store lets the app gather the reshape stage's settings too: mod_reshape
    # publishes them to store$reshape_state, and a restore stages them in
    # store$pending_reshape for the reshape sync-observer to consume.
    if (!is.null(store)) {
      output$session_ui <- renderUI({
        tagList(
          hr(), h6("Save / restore session"),
          if (!is.null(rv$data))
            downloadButton(ns("save_session"), "Save progress (.rds)",
                           class = "btn-outline-success btn-sm w-100 mb-2")
          else
            helpText("Load data to enable saving."),
          fileInput(ns("restore_session"), NULL, accept = ".rds",
                    buttonLabel = "Restore\u2026", placeholder = "open a saved .rds"),
          helpText("Saves your data, Data Health fixes, type changes, filters, ",
                   "and reshape choice \u2014 reload it later to pick up where you ",
                   "left off.")
        )
      })

      output$save_session <- downloadHandler(
        filename = function() sprintf("foxplots-session_%s.rds", Sys.Date()),
        content  = function(file) {
          validate(need(!is.null(rv$data), "Load data before saving."))
          st <- build_session_state(
            data = rv$data, data_raw = rv$data_raw, filters = rv$filters,
            source = rv$source, reshape = store$reshape_state)
          save_session(st, file)
        }
      )

      observeEvent(input$restore_session, {
        req(input$restore_session)
        st <- load_session(input$restore_session$datapath)
        ok <- validate_session_state(st)
        if (!isTRUE(ok)) {
          showNotification(paste("Couldn't restore that file \u2014", ok),
                           type = "error", duration = 8)
          return()
        }
        # Stage the reshape settings BEFORE the data change, so the reshape
        # sync-observer sees them when data_in() fires.
        store$pending_reshape <- st$reshape
        rv$data_raw     <- st$data_raw %||% st$data
        rv$filters      <- if (is.list(st$filters)) st$filters else list()
        rv$source       <- st$source %||% "restored session"
        rv$upload       <- NULL; rv$sheets <- NULL; rv$loaded_sheet <- NULL
        rv$file_token   <- rv$file_token + 1L          # reset the upload box
        rv$data         <- st$data                     # set last -> triggers downstream
        showNotification(paste("Restored:", session_state_summary(st)),
                         type = "message", duration = 7)
      })
    }

    # Pattern A: the next stage reads the FILTERED working data.
    filtered
  })
}
