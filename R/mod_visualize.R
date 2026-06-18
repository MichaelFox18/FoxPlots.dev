# ============================================================
# mod_visualize.R -- multi-plot charting (1-4 plots)
# ============================================================
# Full-parity port of the original Visualize tab, namespaced as a module:
# 1-4 plots side by side, 7 chart types, per-plot config accordions,
# "apply Plot 1 style to all", smart chart-suitability hints, auto
# static-vs-interactive rendering for big data, and a copy-the-ggplot2-code
# panel per plot. Thin wrapper over R/helpers_plot.R.
#
# Requires R/components.R (info_tip, copy_js, UF_COLORS) and R/helpers_plot.R.
# The app must attach ggplot2, plotly, and colourpicker.

# One plot's control panel. All input ids and conditionalPanel conditions are
# namespaced via `ns`; JS conditions use bracket notation because namespaced
# ids contain a hyphen.
plot_slot_panel <- function(ns, i) {
  pid  <- function(sfx) ns(paste0("mp", i, sfx))
  oid  <- function(sfx) ns(paste0("ui_mp", i, sfx))
  cond <- function(sfx, expr) sprintf("input['%s'] %s", ns(paste0("mp", i, sfx)), expr)

  accordion_panel(
    paste("Plot", i),
    value = paste0("panel", i),
    selectInput(pid("_type"), "Chart type",
                choices = c("Scatter Plot" = "scatter", "Line Graph" = "line",
                            "Bar Chart" = "bar", "Histogram" = "histogram",
                            "Density Plot" = "density", "Box Plot" = "boxplot",
                            "Violin Plot" = "violin", "Mean \u00b1 Error" = "meanerror",
                            "Pie Chart" = "pie", "Hexbin (2D density)" = "hexbin",
                            "Correlation Heatmap" = "heatmap")),
    uiOutput(oid("_x")),
    uiOutput(oid("_y")),
    uiOutput(oid("_color")),
    uiOutput(oid("_sizeby")),
    uiOutput(oid("_hint")),

    conditionalPanel(
      cond("_type", "== 'heatmap'"),
      uiOutput(oid("_corrvars")),
      radioButtons(pid("_corrmethod"),
                   tagList("Correlation method", info_tip(
                     "Pearson measures straight-line association; Spearman ranks ",
                     "the values first, capturing any monotonic relationship.")),
                   choices = c("Pearson" = "pearson", "Spearman" = "spearman"),
                   inline = TRUE),
      checkboxInput(pid("_corrlabel"), "Show correlation values", TRUE)
    ),
    conditionalPanel(
      cond("_type", "== 'bar'"),
      selectInput(pid("_baragg"),
                  tagList("Bar aggregation", info_tip(
                    "How to combine Y values within each category. Ignored when ",
                    "no Y variable is set (then bars show counts).")),
                  choices = c("Sum" = "sum", "Mean" = "mean", "Median" = "median")),
      checkboxInput(pid("_barline"),
                    tagList("Overlay a connecting line", info_tip(
                      "Draws a line through the bar tops to connect the summary ",
                      "stats (needs a Y variable).")),
                    FALSE)
    ),
    conditionalPanel(
      cond("_type", "== 'line'"),
      selectInput(pid("_lineagg"),
                  tagList("Connect (per X)", info_tip(
                    "A line connects one summary value per X category: the mean, ",
                    "median, or sum of Y. For a continuous X it's just the line.")),
                  choices = c("Mean" = "mean", "Median" = "median", "Sum" = "sum"))
    ),
    conditionalPanel(
      sprintf("%s || %s", cond("_type", "== 'histogram'"), cond("_type", "== 'hexbin'")),
      sliderInput(pid("_bins"), "Bins", min = 5, max = 60, value = 30, step = 1)
    ),
    conditionalPanel(
      cond("_type", "== 'meanerror'"),
      selectInput(pid("_errtype"),
                  tagList("Error bars show", info_tip(
                    "Standard error of the mean (\u00b1SE) \u2014 how precisely the mean is ",
                    "estimated; or one standard deviation (\u00b1SD) \u2014 how spread out ",
                    "the data are.")),
                  choices = c("Standard error (\u00b1SE)" = "se",
                              "Standard deviation (\u00b1SD)" = "sd"))
    ),
    uiOutput(oid("_catlimit")),

    tags$hr(), tags$h6("Labels"),
    textInput(pid("_title"), "Title", placeholder = "(optional)"),
    conditionalPanel(
      sprintf("%s && %s", cond("_type", "!= 'pie'"), cond("_type", "!= 'heatmap'")),
      textInput(pid("_xlab"), "X-axis label", placeholder = "auto"),
      textInput(pid("_ylab"), "Y-axis label", placeholder = "auto")
    ),
    conditionalPanel(
      sprintf("%s && %s", cond("_type", "!= 'pie'"), cond("_type", "!= 'heatmap'")),
      tags$hr(), tags$h6("Style"),
      selectInput(pid("_theme"), "Theme",
                  choices = c("Minimal" = "minimal", "Classic" = "classic",
                              "Light" = "light", "B&W" = "bw", "Dark" = "dark")),
      colourpicker::colourInput(pid("_color"), "Default color",
                  value = UF_COLORS[((i - 1) %% length(UF_COLORS)) + 1]),
      sliderInput(pid("_size"), "Point / bar size",
                  min = 0.5, max = 5, value = 2, step = 0.5)
    ),
    conditionalPanel(
      sprintf("%s || %s", cond("_type", "== 'scatter'"), cond("_type", "== 'line'")),
      tags$hr(), tags$h6("Regression overlay"),
      checkboxInput(pid("_reg"), "Add fitted line", FALSE),
      conditionalPanel(
        cond("_reg", "== true"),
        selectInput(pid("_regtype"), "Method",
                    choices = c("Linear (lm)" = "lm", "Polynomial" = "poly",
                                "Loess" = "loess")),
        conditionalPanel(
          cond("_regtype", "== 'poly'"),
          sliderInput(pid("_regdeg"), "Polynomial degree", min = 2, max = 6,
                      value = 2)
        ),
        checkboxInput(pid("_regci"), "Show 95% CI band", TRUE),
        colourpicker::colourInput(pid("_regcol"), "Line color", value = UF_ORANGE),
        checkboxInput(pid("_trendlab"),
                      tagList("Show equation & R\u00b2 on plot", info_tip(
                        "Annotates the chart with the fitted equation (or model ",
                        "type) and its R\u00b2.")),
                      FALSE)
      )
    ),
    accordion(
      open = FALSE,
      accordion_panel(
        tagList(icon("sliders"), " Advanced options"),
        value = paste0("adv", i),
        conditionalPanel(
          cond("_type", "!= 'heatmap'"),
          selectInput(pid("_palette"),
                      tagList("Color palette", info_tip(
                        "Colors for grouped charts and pie slices. \u201cAutomatic\u201d ",
                        "keeps the built-in choice; \u201cColorblind-safe\u201d uses the ",
                        "Okabe\u2013Ito palette.")),
                      choices = PALETTES)
        ),
        conditionalPanel(
          sprintf("%s && %s", cond("_type", "!= 'pie'"), cond("_type", "!= 'heatmap'")),
          sliderInput(pid("_alpha"), "Opacity", min = 0.1, max = 1, value = 0.8,
                      step = 0.05)
        ),
        conditionalPanel(
          cond("_type", "== 'scatter'"),
          checkboxInput(pid("_jitter"),
                        tagList("Jitter points", info_tip(
                          "Nudges overlapping points apart so dense scatters ",
                          "stay readable.")),
                        FALSE)
        ),
        conditionalPanel(
          sprintf("%s && %s", cond("_type", "!= 'pie'"), cond("_type", "!= 'heatmap'")),
          selectInput(pid("_logscale"),
                      tagList("Axis scale", info_tip(
                        "Transform an axis \u2014 log10 for skewed/wide-ranging ",
                        "values, sqrt for counts. Continuous axes only.")),
                      choices = c("None" = "none",
                                  "Log10 \u2014 X" = "logx", "Log10 \u2014 Y" = "logy",
                                  "Log10 \u2014 both" = "logboth",
                                  "Sqrt \u2014 X" = "sqrtx", "Sqrt \u2014 Y" = "sqrty",
                                  "Sqrt \u2014 both" = "sqrtboth"))
        ),
        uiOutput(oid("_facet")),
        selectInput(pid("_legendpos"), "Legend position",
                    choices = c("Right" = "right", "Bottom" = "bottom",
                                "Top" = "top", "Hidden" = "none")),
        conditionalPanel(
          sprintf("%s || %s", cond("_type", "== 'bar'"), cond("_type", "== 'boxplot'")),
          checkboxInput(pid("_flip"),
                        tagList("Horizontal orientation", info_tip(
                          "Flips the chart on its side \u2014 handy when category ",
                          "labels are long or numerous.")),
                        FALSE)
        ),
        conditionalPanel(
          sprintf("%s && %s", cond("_type", "!= 'pie'"), cond("_type", "!= 'heatmap'")),
          checkboxInput(pid("_grid"), "Show gridlines", TRUE)
        )
      )
    )
  )
}

visualizeUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    tags$script(HTML(copy_js)),
    sidebar = sidebar(
      width = 320,
      h5("Chart settings"),
      radioButtons(ns("n_plots"), "Number of plots",
                   choices = c(1, 2, 3, 4), selected = 1, inline = TRUE),
      conditionalPanel(
        sprintf("input['%s'] != '1'", ns("n_plots")),
        actionButton(ns("copy_style"), "Apply Plot 1 style to all",
                     icon = icon("brush"),
                     class = "btn-outline-secondary btn-sm w-100"),
        tags$div(class = "form-text mb-2",
                 "Copies Plot 1's theme, color, and size to the other plots.")
      ),
      actionButton(ns("reset_plots"), "Reset settings to default",
                   icon = icon("rotate-left"),
                   class = "btn-outline-secondary btn-sm w-100 mb-2"),
      hr(),
      uiOutput(ns("plot_config_accordion"))
    ),
    uiOutput(ns("plots_area"))
  )
}

visualizeServer <- function(id, data_in) {
  moduleServer(id, function(input, output, session) {
    ns    <- session$ns
    reset <- reactiveVal(0L)
    # Remembered X/Y picks per slot (keys "1x", "1y", ...). They persist when the
    # picker re-renders on a chart-type change, but are cleared on Reset / new
    # data; a remembered pick is only re-applied if it's still a valid column.
    sel <- reactiveValues()

    cols_all <- reactive({ df <- data_in(); req(is.data.frame(df)); names(df) })
    cols_num <- reactive({
      df <- data_in(); req(is.data.frame(df))
      names(df)[vapply(df, is.numeric, logical(1))]
    })
    cols_cat <- reactive({
      df <- data_in(); req(is.data.frame(df))
      names(df)[vapply(df, function(x)
        (is_discrete_col(x) || (is.numeric(x) && dplyr::n_distinct(x) <= 10)) &&
          dplyr::n_distinct(x) <= 30, logical(1))]
    })

    output$plot_config_accordion <- renderUI({
      tagList(
        accordion(plot_slot_panel(ns, 1), open = "panel1"),
        conditionalPanel(sprintf("input['%s'] >= 2", ns("n_plots")),
                         accordion(plot_slot_panel(ns, 2), open = FALSE)),
        conditionalPanel(sprintf("input['%s'] >= 3", ns("n_plots")),
                         accordion(plot_slot_panel(ns, 3), open = FALSE)),
        conditionalPanel(sprintf("input['%s'] >= 4", ns("n_plots")),
                         accordion(plot_slot_panel(ns, 4), open = FALSE))
      )
    })

    # Per-slot dynamic variable pickers + hint. reset() forces a rebuild.
    for (i in 1:4) {
      local({
        idx <- i
        # A remembered pick, re-applied only if still a valid choice.
        keep <- function(axis, valid) {
          v <- isolate(sel[[paste0(idx, axis)]])
          if (!is.null(v) && v %in% valid) v else ""
        }
        # Record the user's picks so they survive a chart-type change.
        observeEvent(input[[paste0("mp", idx, "_xvar")]], {
          v <- input[[paste0("mp", idx, "_xvar")]]
          if (!is.null(v) && nzchar(v)) sel[[paste0(idx, "x")]] <- v
        }, ignoreInit = TRUE)
        observeEvent(input[[paste0("mp", idx, "_yvar")]], {
          v <- input[[paste0("mp", idx, "_yvar")]]
          if (!is.null(v) && nzchar(v)) sel[[paste0(idx, "y")]] <- v
        }, ignoreInit = TRUE)
        output[[paste0("ui_mp", idx, "_x")]] <- renderUI({
          req(is.data.frame(data_in())); reset()
          ty <- input[[paste0("mp", idx, "_type")]] %||% "scatter"
          if (identical(ty, "heatmap")) return(NULL)
          if (ty %in% c("histogram", "density", "hexbin"))
            return(selectInput(ns(paste0("mp", idx, "_xvar")),
                               "X variable (numeric)",
                               choices = c("Choose a variable\u2026" = "", cols_num()),
                               selected = keep("x", cols_num())))
          lbl <- if (identical(ty, "pie")) "Category (one slice per value)"
                 else "X variable"
          selectInput(ns(paste0("mp", idx, "_xvar")), lbl,
                      choices = c("Choose a variable\u2026" = "", cols_all()),
                      selected = keep("x", cols_all()))
        })
        output[[paste0("ui_mp", idx, "_y")]] <- renderUI({
          req(is.data.frame(data_in())); reset()
          ty <- input[[paste0("mp", idx, "_type")]]; req(ty)
          if (ty == "heatmap") return(NULL)
          if (ty %in% c("histogram", "density"))
            return(helpText("This chart uses only an X variable."))
          if (ty == "pie")
            return(selectInput(ns(paste0("mp", idx, "_yvar")),
              tagList("Slice size", info_tip(
                "Optional. By default each slice is the COUNT of rows in that ",
                "category. Pick a numeric variable to size slices by its SUM ",
                "within each category instead.")),
              choices = c("Count of each category" = "__count__", cols_num())))
          selectInput(ns(paste0("mp", idx, "_yvar")), "Y variable",
                      choices = c("Choose a variable\u2026" = "", cols_num()),
                      selected = keep("y", cols_num()))
        })
        output[[paste0("ui_mp", idx, "_color")]] <- renderUI({
          req(is.data.frame(data_in())); reset()
          ty <- input[[paste0("mp", idx, "_type")]]; req(ty)
          if (ty %in% c("pie", "heatmap", "hexbin")) return(NULL)
          selectInput(ns(paste0("mp", idx, "_colorvar")),
                      "Color / group by (optional)",
                      choices = c("None" = "__none__", cols_all()))
        })
        output[[paste0("ui_mp", idx, "_sizeby")]] <- renderUI({
          req(is.data.frame(data_in())); reset()
          ty <- input[[paste0("mp", idx, "_type")]]; req(ty)
          if (!identical(ty, "scatter")) return(NULL)
          selectInput(ns(paste0("mp", idx, "_sizeby")),
                      tagList("Size by (optional)", info_tip(
                        "Scale each point by a numeric variable \u2014 turns the ",
                        "scatter into a bubble chart.")),
                      choices = c("None" = "__none__", cols_num()))
        })
        output[[paste0("ui_mp", idx, "_corrvars")]] <- renderUI({
          req(is.data.frame(data_in())); reset()
          nums <- cols_num()
          selectizeInput(ns(paste0("mp", idx, "_corrvarsv")),
                      tagList("Variables", info_tip(
                        "Pick the numeric columns to correlate. The heatmap starts ",
                        "empty and fills in as you add variables, so a wide dataset ",
                        "stays readable \u2014 choose two or more.")),
                      choices = nums, selected = character(0), multiple = TRUE,
                      options = list(placeholder = "pick variables to correlate\u2026"))
        })
        output[[paste0("ui_mp", idx, "_hint")]] <- renderUI({
          req(is.data.frame(data_in()), input[[paste0("mp", idx, "_type")]])
          msg <- chart_hint(data_in(), slot_params(idx))
          if (is.null(msg)) return(NULL)
          div(class = "alert alert-warning py-1 px-2 small mb-2", role = "alert",
              icon("triangle-exclamation"), HTML(paste0(" ", msg)))
        })
        output[[paste0("ui_mp", idx, "_facet")]] <- renderUI({
          req(is.data.frame(data_in())); reset()
          ty <- input[[paste0("mp", idx, "_type")]]
          if (isTRUE(ty %in% c("pie", "heatmap"))) return(NULL)
          selectInput(ns(paste0("mp", idx, "_facetvar")),
                      tagList("Facet by (small multiples)", info_tip(
                        "Splits the chart into one panel per category of this ",
                        "variable. Low-cardinality columns only.")),
                      choices = c("None" = "__none__", cols_cat()))
        })
        output[[paste0("ui_mp", idx, "_catlimit")]] <- renderUI({
          req(is.data.frame(data_in())); reset()
          ty <- input[[paste0("mp", idx, "_type")]]
          if (!isTRUE(ty %in% c("bar", "pie"))) return(NULL)
          xv <- input[[paste0("mp", idx, "_xvar")]]
          req(xv, xv %in% names(data_in()))
          nx <- dplyr::n_distinct(data_in()[[xv]])
          if (nx <= 2) return(NULL)
          unit  <- if (ty == "bar") "bars" else "slices"
          cap   <- min(nx, 200L)
          deflt <- min(if (ty == "bar") BAR_MAX else PIE_MAX, nx)
          sliderInput(ns(paste0("mp", idx, "_catlimitv")),
            tagList(sprintf("Maximum %s", unit), info_tip(sprintf(
              "Keeps the largest categories and groups the rest into one \u201cOther\u201d. Defaults to %d; slide up to %d to show more, or down to simplify.",
              deflt, cap))),
            min = 2, max = cap, value = deflt, step = 1)
        })
      })
    }

    # Gather one slot's settings into a plain list (the `p` build_full_plot wants).
    slot_params <- function(i) {
      list(
        type        = input[[paste0("mp", i, "_type")]],
        x           = input[[paste0("mp", i, "_xvar")]],
        y           = input[[paste0("mp", i, "_yvar")]],
        color       = input[[paste0("mp", i, "_colorvar")]],
        size_by     = input[[paste0("mp", i, "_sizeby")]],
        title       = input[[paste0("mp", i, "_title")]],
        xlab        = input[[paste0("mp", i, "_xlab")]],
        ylab        = input[[paste0("mp", i, "_ylab")]],
        theme       = input[[paste0("mp", i, "_theme")]],
        color_hex   = input[[paste0("mp", i, "_color")]],
        size        = input[[paste0("mp", i, "_size")]],
        bins        = input[[paste0("mp", i, "_bins")]],
        err_type    = input[[paste0("mp", i, "_errtype")]] %||% "se",
        bar_agg     = input[[paste0("mp", i, "_baragg")]],
        bar_line    = isTRUE(input[[paste0("mp", i, "_barline")]]),
        line_agg    = input[[paste0("mp", i, "_lineagg")]] %||% "mean",
        cat_limit   = input[[paste0("mp", i, "_catlimitv")]],
        corr_method = input[[paste0("mp", i, "_corrmethod")]] %||% "pearson",
        corr_label  = isTRUE(input[[paste0("mp", i, "_corrlabel")]]),
        corr_vars   = input[[paste0("mp", i, "_corrvarsv")]],
        reg_overlay = isTRUE(input[[paste0("mp", i, "_reg")]]),
        reg_type    = input[[paste0("mp", i, "_regtype")]],
        reg_deg     = input[[paste0("mp", i, "_regdeg")]],
        reg_ci      = isTRUE(input[[paste0("mp", i, "_regci")]]),
        reg_col     = input[[paste0("mp", i, "_regcol")]],
        trend_label = isTRUE(input[[paste0("mp", i, "_trendlab")]]),
        palette     = input[[paste0("mp", i, "_palette")]] %||% "auto",
        alpha       = input[[paste0("mp", i, "_alpha")]],
        jitter      = isTRUE(input[[paste0("mp", i, "_jitter")]]),
        logscale    = input[[paste0("mp", i, "_logscale")]] %||% "none",
        facet       = input[[paste0("mp", i, "_facetvar")]],
        legend_pos  = input[[paste0("mp", i, "_legendpos")]] %||% "right",
        gridlines   = input[[paste0("mp", i, "_grid")]] %||% TRUE,
        flip        = isTRUE(input[[paste0("mp", i, "_flip")]])
      )
    }

    observeEvent(input$copy_style, {
      n <- as.integer(input$n_plots %||% 1)
      if (n < 2) return()
      th <- input$mp1_theme;  co <- input$mp1_color;  sz <- input$mp1_size
      pal <- input$mp1_palette; al <- input$mp1_alpha
      lp <- input$mp1_legendpos; gr <- input$mp1_grid
      for (i in 2:n) {
        if (!is.null(th)) updateSelectInput(session, paste0("mp", i, "_theme"), selected = th)
        if (!is.null(co)) colourpicker::updateColourInput(session, paste0("mp", i, "_color"), value = co)
        if (!is.null(sz)) updateSliderInput(session, paste0("mp", i, "_size"), value = sz)
        if (!is.null(pal)) updateSelectInput(session, paste0("mp", i, "_palette"), selected = pal)
        if (!is.null(al))  updateSliderInput(session, paste0("mp", i, "_alpha"), value = al)
        if (!is.null(lp))  updateSelectInput(session, paste0("mp", i, "_legendpos"), selected = lp)
        if (!is.null(gr))  updateCheckboxInput(session, paste0("mp", i, "_grid"), value = gr)
      }
      showNotification("Applied Plot 1's style to the other plots.", type = "message")
    })

    observeEvent(input$reset_plots, {
      for (k in names(sel)) sel[[k]] <- NULL   # forget remembered picks (cleared first)
      reset(reset() + 1L)
      updateRadioButtons(session, "n_plots", selected = 1)
      showNotification("Reset all plot settings to default.", type = "message")
    })

    # New data: forget remembered picks (a leftover pick is also guarded by the
    # validity check in keep(), but clear so it doesn't linger).
    observeEvent(data_in(), { for (k in names(sel)) sel[[k]] <- NULL })

    output$plots_area <- renderUI({
      req(is.data.frame(data_in()))
      n   <- as.integer(input$n_plots %||% 1)
      big <- nrow(data_in()) > BIG_ROWS
      cards <- lapply(seq_len(n), function(i) {
        # Faceted (small-multiple) charts need more vertical room to stay legible.
        fac     <- input[[paste0("mp", i, "_facetvar")]]
        faceted <- !is.null(fac) && !fac %in% c("", "__none__")
        base_h  <- if (n == 1) 470 else 330
        ph      <- paste0(if (faceted) base_h + 170 else base_h, "px")
        plot_ui <- if (big) {
          tagList(
            plotOutput(ns(paste0("mp_st", i)), height = ph),
            tags$div(class = "form-text",
                     sprintf("Static view (%s rows). Interactive zoom/hover is off above %s rows for speed; exports use all rows.",
                             format(nrow(data_in()), big.mark = ","),
                             format(BIG_ROWS, big.mark = ",")))
          )
        } else {
          tagList(
            conditionalPanel(
              sprintf("input['%s'] != 'pie' && input['%s'] != 'heatmap'",
                      ns(paste0("mp", i, "_type")), ns(paste0("mp", i, "_type"))),
              plotly::plotlyOutput(ns(paste0("mp_ly", i)), height = ph)),
            conditionalPanel(
              sprintf("input['%s'] == 'pie' || input['%s'] == 'heatmap'",
                      ns(paste0("mp", i, "_type")), ns(paste0("mp", i, "_type"))),
              plotOutput(ns(paste0("mp_st", i)), height = ph))
          )
        }
        card(
          full_screen = TRUE,
          card_header(paste("Plot", i)),
          plot_ui,
          accordion(
            open = FALSE,
            accordion_panel(
              tagList(icon("code"), " R code for this plot"),
              value = paste0("code_panel", i),
              tags$button("Copy code", class = "btn btn-sm btn-outline-primary mb-2",
                          onclick = sprintf("DEcopy('%s', this)",
                                            ns(paste0("code_plot", i)))),
              verbatimTextOutput(ns(paste0("code_plot", i)))
            )
          )
        )
      })
      do.call(layout_columns, c(list(col_widths = if (n == 1) 12 else 6), cards))
    })

    for (i in 1:4) {
      local({
        idx <- i
        output[[paste0("mp_ly", idx)]] <- plotly::renderPlotly({
          req(is.data.frame(data_in()))
          pr <- slot_params(idx)
          if (!identical(pr$type, "heatmap"))
            validate(need(nzchar(pr$x %||% ""),
                          "Choose your variables to draw this chart."))
          p <- build_full_plot(data_in(), pr)
          validate(need(!is.null(p),
                        "Finish choosing variables to draw this chart."))
          ply <- plotly::ggplotly(p) |>
            plotly::layout(margin = list(t = 55, b = 55))
          # ggplotly drops a couple of things ggplot got right -- clean the
          # grouped-trace legend names and honour the chosen legend position.
          ply <- clean_plotly_trace_names(ply)
          leg <- plotly_legend_layout(pr$legend_pos)
          if (!is.null(leg)) ply <- plotly::layout(ply, legend = leg)
          ply
        })
        output[[paste0("mp_st", idx)]] <- renderPlot({
          req(is.data.frame(data_in()))
          pr <- slot_params(idx)
          if (!identical(pr$type, "heatmap"))
            validate(need(nzchar(pr$x %||% ""),
                          "Choose your variables to draw this chart."))
          p <- build_full_plot(data_in(), pr)
          validate(need(!is.null(p),
                        "Finish choosing variables to draw this chart."))
          p
        }, bg = "white")
        output[[paste0("code_plot", idx)]] <- renderText({
          req(is.data.frame(data_in()))
          pr <- slot_params(idx)
          if (!identical(pr$type, "heatmap")) req(pr$x)
          generate_code(data_in(), pr)
        })
      })
    }

    # Build the currently-configured plots AND their ggplot2 code together, so
    # the two lists stay index-aligned (a slot without a usable X, or one that
    # fails to build, is skipped in both). Per-slot errors become NULL/NA rather
    # than breaking the whole list.
    current_outputs <- function() {
      df <- data_in()
      if (!is.data.frame(df)) return(list(plots = list(), code = list()))
      n <- as.integer(input$n_plots %||% 1)
      plots <- list(); codes <- list()
      for (i in seq_len(n)) {
        pr <- slot_params(i)
        if (!identical(pr$type, "heatmap") && (is.null(pr$x) || !nzchar(pr$x))) next
        pl <- tryCatch(build_full_plot(df, pr), error = function(e) NULL)
        if (is.null(pl)) next
        plots[[length(plots) + 1L]] <- pl
        codes[[length(codes) + 1L]] <- tryCatch(generate_code(df, pr),
                                                error = function(e) NA_character_)
      }
      list(plots = plots, code = codes)
    }

    # Return both the live ggplot list and the matching code, as
    # list(plots = <ggplots>, code = <strings>). Export reads $plots to render
    # images; Report reads both to embed charts with their code. Lazy: only
    # computed when actually read.
    reactive(current_outputs())
  })
}
