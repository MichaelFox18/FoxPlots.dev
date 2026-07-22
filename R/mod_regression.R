# ============================================================
# mod_regression.R -- fixed-effects regression (lm)
# ============================================================
# Thin wrapper over R/helpers_model.R. Pick a numeric response and one or more
# predictors (numeric OR categorical), optionally with pairwise interactions or
# a polynomial term, fit the model, and read a coefficient table (with CIs),
# fit statistics, a plain-English interpretation, the two diagnostic plots, and
# copy-ready code. Returns the fitted-model reactive. Requires ggplot2 + plotly
# attached by the app; uses copy_js / info_tip / label_or from components.R.

regressionUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$script(HTML(copy_js)),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h5("Model setup"),
        uiOutput(ns("ui_resp")),
        uiOutput(ns("ui_pred")),
        checkboxInput(ns("poly"),
          tagList("Fit a curve (polynomial)", info_tip(
            "Fit powers of a single numeric predictor (Y ~ X + X^2 + ...). ",
            "Uses only the first predictor.")), value = FALSE),
        conditionalPanel(
          sprintf("input['%s']", ns("poly")),
          sliderInput(ns("poly_deg"), "Polynomial degree", min = 2, max = 6,
                      value = 2)),
        conditionalPanel(
          sprintf("!input['%s']", ns("poly")),
          checkboxInput(ns("interactions"),
            tagList("Include interactions", info_tip(
              "Add all pairwise interaction terms (a * b): let each predictor's ",
              "effect depend on the others. Needs two or more predictors.")),
            value = FALSE)),
        uiOutput(ns("ui_ref")),
        hr(),
        actionButton(ns("fit"), "Fit model", class = "btn-primary w-100",
                     icon = icon("play")),
        br(), br(),
        downloadButton(ns("download"), "Export summary (.txt)",
                       class = "btn-success w-100")
      ),
      navset_card_tab(
        id = ns("tabs"),
        nav_panel("Coefficients",
          layout_columns(
            col_widths = c(8, 4),
            card(card_header(icon("table-list"), " Coefficients (95% CI)"),
                 DT::DTOutput(ns("coefs"))),
            card(card_header(icon("gauge-high"), " Fit statistics"),
                 DT::DTOutput(ns("fitstats")))
          ),
          card(card_header(icon("lightbulb"), " Statistical interpretation"),
               uiOutput(ns("interpretation")))
        ),
        nav_panel("Estimated means",
          card(
            card_body(
              uiOutput(ns("ui_emm_controls")),
              uiOutput(ns("emm_note"))
            )
          ),
          layout_columns(
            col_widths = c(6, 6),
            card(card_header(icon("layer-group"),
                             " Estimated means + letter groups"),
                 DT::DTOutput(ns("emm_cld"))),
            card(card_header(icon("chart-column"), " Means plot"),
                 plotOutput(ns("emm_plot"), height = "420px"))
          ),
          card(card_header(icon("code-compare"), " Pairwise comparisons"),
               DT::DTOutput(ns("emm_pairs"))),
          card(
            card_header(icon("code"), " EMMeans code"),
            tags$button("Copy code",
                        class = "btn btn-sm btn-outline-primary mb-2",
                        onclick = sprintf("DEcopy('%s', this)", ns("emm_code"))),
            verbatimTextOutput(ns("emm_code"))
          )
        ),
        nav_panel("Diagnostics",
          layout_columns(
            col_widths = c(6, 6),
            card(card_header(icon("chart-line"), " Fitted vs Actual"),
                 plotly::plotlyOutput(ns("plot_fitted"), height = "320px")),
            card(card_header(icon("chart-simple"), " Residuals vs Fitted"),
                 plotly::plotlyOutput(ns("plot_resid"), height = "320px"))
          )
        ),
        nav_panel("Model summary & code",
          card(card_header(icon("file-lines"), " Model summary"),
               verbatimTextOutput(ns("summary"))),
          card(
            card_header(icon("code"), " Reproducible R code"),
            tags$button("Copy code",
                        class = "btn btn-sm btn-outline-primary mb-2",
                        onclick = sprintf("DEcopy('%s', this)", ns("code"))),
            verbatimTextOutput(ns("code"))
          )
        )
      )
    )
  )
}

regressionServer <- function(id, data_in) {
  moduleServer(id, function(input, output, session) {
    ns    <- session$ns
    model <- reactiveVal(NULL)

    cols_num <- reactive({
      df <- data_in(); req(is.data.frame(df))
      names(df)[vapply(df, is.numeric, logical(1))]
    })
    cols_all <- reactive({
      df <- data_in(); req(is.data.frame(df)); names(df)
    })
    # Predictors the user picked that are categorical (need a reference level).
    cat_preds <- reactive({
      df <- data_in(); preds <- input$pred
      if (!is.data.frame(df) || !length(preds)) return(character(0))
      preds <- intersect(preds, names(df))
      preds[vapply(preds, function(v)
        is.character(df[[v]]) || is.factor(df[[v]]), logical(1))]
    })

    # New data invalidates any previously fitted model.
    observeEvent(data_in(), model(NULL), ignoreNULL = FALSE)

    output$ui_resp <- renderUI({
      nums <- cols_num()
      if (!length(nums))
        return(helpText("This dataset has no numeric columns to model."))
      selectInput(ns("resp"),
        tagList("Response variable (Y)", info_tip(
          "The numeric outcome you want to predict.")),
        choices = nums)
    })

    output$ui_pred <- renderUI({
      all_cols <- cols_all()
      resp     <- input$resp
      choices  <- setdiff(all_cols, resp)
      if (!length(choices)) return(NULL)
      selectInput(ns("pred"),
        tagList("Predictor variables (X)", info_tip(
          "One or more variables to predict the response. Numeric or ",
          "categorical (a categorical predictor enters as a factor). Hold ",
          "Ctrl/Cmd to select several.")),
        choices = choices, multiple = TRUE)
    })

    # One reference-level picker per categorical predictor (the baseline the
    # other levels are compared against).
    output$ui_ref <- renderUI({
      df   <- data_in(); cats <- cat_preds()
      if (!is.data.frame(df) || !length(cats) || isTRUE(input$poly)) return(NULL)
      tagList(
        hr(),
        tags$small(class = "text-muted",
                   tagList("Reference level", info_tip(
                     "The baseline category each factor is compared to."))),
        lapply(cats, function(v) {
          lv <- levels(factor(df[[v]]))
          selectInput(ns(paste0("ref_", v)), v, choices = lv, selected = lv[1])
        })
      )
    })

    # Assemble the spec from the current inputs.
    build_spec <- function() {
      preds <- input$pred
      refs  <- NULL
      cats  <- cat_preds()
      if (length(cats)) {
        refs <- stats::setNames(
          lapply(cats, function(v) input[[paste0("ref_", v)]]), cats)
        refs <- refs[!vapply(refs, is.null, logical(1))]
      }
      reg_spec(
        response     = input$resp,
        predictors   = preds,
        interactions = isTRUE(input$interactions) && !isTRUE(input$poly),
        poly_degree  = if (isTRUE(input$poly)) input$poly_deg %||% 2 else NULL,
        ref_levels   = refs)
    }

    observeEvent(input$fit, {
      df <- data_in()
      req(is.data.frame(df), input$resp, input$pred)
      spec     <- build_spec()
      problems <- reg_validate(df, spec)
      if (length(problems)) {
        showNotification(paste("Can't fit:", problems[1]), type = "warning",
                         duration = 8)
        return(invisible(NULL))
      }
      tryCatch({
        model(reg_fit(df, spec))
        showNotification("Model fitted successfully.", type = "message")
      }, error = function(e)
        showNotification(paste("Fitting error:", conditionMessage(e)),
                         type = "error", duration = 8))
    })

    # summary() is the expensive call; compute once, share.
    model_summary <- reactive({ req(model()); summary(model()) })

    # -- Estimated marginal means (factor predictors) --------------------------
    # The factors in the CURRENTLY fitted model (not the live picks), so the
    # EMMeans controls always match what was fit.
    model_factors <- reactive({
      m <- model(); if (is.null(m)) character(0) else names(m$xlevels)
    })

    output$ui_emm_controls <- renderUI({
      if (is.null(model()))
        return(tags$p(class = "text-muted fst-italic",
                      "Fit a model first to estimate marginal means."))
      facs <- model_factors()
      if (!length(facs))
        return(tags$p(class = "text-muted",
          "Estimated marginal means need a categorical predictor. Recast one to a factor on the Import tab (Change Type), then refit."))
      layout_columns(
        col_widths = c(4, 4, 4),
        selectInput(ns("emm_var"),
          tagList("Estimate means for", info_tip(
            "The categorical predictor to average the response over, holding ",
            "numeric predictors at their means.")),
          choices = facs, selected = facs[1]),
        if (length(facs) >= 2)
          selectInput(ns("emm_by"),
            tagList("Within (optional)", info_tip(
              "Compare the first factor's levels separately within each level ",
              "of this one (simple effects).")),
            choices = c("(none)" = "__none__", setdiff(facs, "")), selected = "__none__"),
        selectInput(ns("emm_adjust"),
          tagList("p-value adjustment", info_tip(
            "Multiplicity correction for the pairwise tests and the letters.")),
          choices = c("Tukey" = "tukey", "Sidak" = "sidak",
                      "Bonferroni" = "bonferroni", "None" = "none"),
          selected = "tukey")
      )
    })

    emm_res <- reactive({
      m <- model(); req(!is.null(m))
      req(input$emm_var)
      by <- input$emm_by %||% "__none__"
      by <- if (identical(by, "__none__")) NULL else by
      reg_emmeans(m, input$emm_var, by = by, adjust = input$emm_adjust %||% "tukey")
    })

    output$emm_note <- renderUI({
      if (is.null(model()) || !length(model_factors())) return(NULL)
      er <- emm_res()
      if (!isTRUE(er$ok)) return(NULL)
      if (is.null(er$held)) return(NULL)
      tags$p(class = "text-muted small mb-0",
             sprintf("Adjusted means \u2014 numeric predictors held at: %s.",
                     er$held))
    })

    output$emm_cld <- DT::renderDT({
      validate(need(!is.null(model()), "Fit a model first."))
      er <- emm_res()
      validate(need(isTRUE(er$ok), er$error %||% "EMMeans unavailable."))
      DT::datatable(round_df(er$cld), rownames = FALSE,
                    class = "compact stripe hover",
                    options = list(scrollX = TRUE, dom = "t", paging = FALSE))
    })

    output$emm_pairs <- DT::renderDT({
      validate(need(!is.null(model()), "Fit a model first."))
      er <- emm_res()
      validate(need(isTRUE(er$ok), er$error %||% "EMMeans unavailable."))
      DT::datatable(round_df(er$pairs), rownames = FALSE,
                    class = "compact stripe hover",
                    options = list(scrollX = TRUE, dom = "t", paging = FALSE))
    })

    # Static (not plotly): the geom_text letter labels position reliably in a
    # rendered ggplot, and the card header already names it, so the plot's own
    # long title is dropped to stop it overflowing the half-width card.
    output$emm_plot <- renderPlot({
      validate(need(!is.null(model()), "Fit a model first."))
      er <- emm_res()
      validate(need(isTRUE(er$ok), er$error %||% "EMMeans unavailable."))
      lmer_emm_plot(er, input$resp, "none") + labs(title = NULL)
    }, res = 96)

    output$emm_code <- renderText({
      if (is.null(model()) || !length(model_factors()) || is.null(input$emm_var))
        return("# Fit a model with a categorical predictor to see EMMeans code.")
      by <- input$emm_by %||% "__none__"
      by <- if (identical(by, "__none__")) NULL else by
      reg_emm_code(input$emm_var, by = by, adjust = input$emm_adjust %||% "tukey")
    })

    output$coefs <- DT::renderDT({
      validate(need(!is.null(model()),
                    "Fit a model to see its coefficients."))
      DT::datatable(reg_coef_table(model()), rownames = FALSE,
                    class = "compact stripe hover",
                    options = list(scrollX = TRUE, dom = "t", paging = FALSE))
    })

    output$fitstats <- DT::renderDT({
      validate(need(!is.null(model()), "Fit a model to see fit statistics."))
      DT::datatable(reg_fit_stats(model()), rownames = FALSE, colnames = c("", ""),
                    class = "compact stripe", options = list(dom = "t",
                                                             paging = FALSE))
    })

    output$summary <- renderPrint({
      if (is.null(model())) cat("Fit a model using the panel on the left.\n")
      else                  model_summary()
    })

    output$code <- renderText({
      if (is.null(model()))
        return("# Fit a model to see reproducible code.")
      reg_code(model())
    })

    output$interpretation <- renderUI({
      if (is.null(model()))
        return(tags$p(class = "text-muted fst-italic",
                      "Fit a model to see an interpretation of the results."))
      info <- model_interpretation(model())
      op   <- info$overall_p
      p_label <- if (!is.na(op)) {
        if (op < 0.001) "p < 0.001" else paste0("p = ", round(op, 4))
      } else "p = N/A"
      overall_tag <- if (!is.na(op) && op < 0.05)
        tags$p(tags$span(style = "color:#2e7d32; font-weight:600;",
          icon("circle-check"),
          sprintf(" The overall model is statistically significant (%s).", p_label)))
      else
        tags$p(tags$span(style = "color:#c62828; font-weight:600;",
          icon("circle-xmark"),
          sprintf(" The overall model is NOT statistically significant (%s).", p_label)))
      tagList(
        overall_tag,
        tags$p(tags$b(sprintf("R\u00b2 = %s", info$r2)), " \u2014 explains ",
               tags$b(paste0(round(info$r2 * 100, 1), "%")), " of the variance. ",
               tags$span(style = "color:#555;",
                         sprintf("(Adj. R\u00b2 = %s)", info$adj_r2))),
        if (length(info$significant))
          tags$p(tags$b(style = "color:#2e7d32;", "Significant (p < 0.05): "),
                 paste(info$significant, collapse = ", ")),
        if (length(info$nonsignificant))
          tags$p(tags$b(style = "color:#c62828;", "Not significant (p \u2265 0.05): "),
                 paste(info$nonsignificant, collapse = ", ")),
        tags$p(class = "text-muted small mt-2",
               "\u03b1 = 0.05. Statistical significance does not imply practical importance.")
      )
    })

    output$plot_fitted <- plotly::renderPlotly({
      validate(need(!is.null(model()),
                    "Fit a model to see the fitted-vs-actual diagnostic."))
      plotly::ggplotly(reg_fitted_gg(model())) |>
        plotly::layout(margin = list(t = 90, b = 40, l = 55, r = 20))
    })
    output$plot_resid <- plotly::renderPlotly({
      validate(need(!is.null(model()),
                    "Fit a model to see the residuals diagnostic."))
      plotly::ggplotly(reg_resid_gg(model())) |>
        plotly::layout(margin = list(t = 90, b = 40, l = 55, r = 20))
    })

    output$download <- downloadHandler(
      filename = function() paste0("model_summary_", Sys.Date(), ".txt"),
      content  = function(f) {
        validate(need(!is.null(model()), "Fit a model first."))
        utils::capture.output(summary(model()), file = f)
      }
    )

    model
  })
}
