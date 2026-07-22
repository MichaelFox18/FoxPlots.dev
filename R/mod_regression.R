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
        radioButtons(ns("family"),
          tagList("Outcome type", info_tip(
            "Continuous -> linear regression (lm). Binary (two categories, ",
            "e.g. yes/no) -> logistic regression (glm) with odds ratios.")),
          choices = c("Continuous (linear)" = "gaussian",
                      "Binary (logistic)"   = "binomial"),
          selected = "gaussian"),
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
          conditionalPanel(
            sprintf("input['%s'] == 'binomial'", ns("family")),
            card(card_header(icon("scale-unbalanced"),
                             " Odds ratios (95% CI)"),
                 DT::DTOutput(ns("odds")),
                 uiOutput(ns("odds_note")))
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
                 plotly::plotlyOutput(ns("plot_fitted"), height = "300px")),
            card(card_header(icon("chart-simple"), " Residuals vs Fitted"),
                 plotly::plotlyOutput(ns("plot_resid"), height = "300px"))
          ),
          layout_columns(
            col_widths = c(6, 6),
            card(card_header(icon("chart-area"), " Normal Q-Q"),
                 plotOutput(ns("plot_qq"), height = "300px")),
            card(card_header(icon("chart-simple"), " Scale-Location"),
                 plotOutput(ns("plot_scaleloc"), height = "300px"))
          ),
          card(card_header(icon("magnifying-glass-chart"), " Cook's distance"),
               plotOutput(ns("plot_cooks"), height = "260px")),
          layout_columns(
            col_widths = c(7, 5),
            card(card_header(icon("clipboard-check"), " Assumption checks"),
                 uiOutput(ns("assumptions"))),
            card(card_header(icon("diagram-project"),
                             " Multicollinearity (VIF)"),
                 DT::DTOutput(ns("vif")),
                 uiOutput(ns("vif_note")))
          )
        ),
        nav_panel("Model comparison",
          card(
            card_body(
              tags$p(class = "mb-2",
                "Save the current fit as ", tags$b("Model A"), ", then change ",
                "the setup and fit again to compare the two. Valid only when one ",
                "model is nested in the other and both use the same rows."),
              actionButton(ns("save_a"), "Save current fit as Model A",
                           class = "btn-outline-primary btn-sm",
                           icon = icon("bookmark")),
              uiOutput(ns("saved_a"))
            )
          ),
          card(card_header(icon("code-compare"),
                           " A (saved) vs B (current)"),
               DT::DTOutput(ns("compare")),
               uiOutput(ns("compare_notes")))
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
    # Columns eligible as a logistic response (binary).
    cols_binary <- reactive({
      df <- data_in(); req(is.data.frame(df))
      names(df)[vapply(df, reg_binary_ok, logical(1))]
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
      binomial <- identical(input$family, "binomial")
      choices  <- if (binomial) cols_binary() else cols_num()
      if (!length(choices))
        return(helpText(if (binomial)
          "This dataset has no binary columns (two-level factor / logical / 0-1) to model."
          else "This dataset has no numeric columns to model."))
      tip <- if (binomial)
        "The two-category outcome to predict. The model estimates the probability of the second level."
        else "The numeric outcome you want to predict."
      selectInput(ns("resp"),
        tagList("Response variable (Y)", info_tip(tip)),
        choices = choices)
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
        ref_levels   = refs,
        family       = input$family %||% "gaussian")
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

    is_logistic <- reactive(inherits(model(), "glm"))

    # -- Odds ratios (logistic only) -------------------------------------------
    output$odds <- DT::renderDT({
      validate(need(!is.null(model()), "Fit a model first."))
      or <- reg_odds_ratios(model())
      validate(need(!is.null(or), "Odds ratios apply to logistic models."))
      DT::datatable(or, rownames = FALSE, class = "compact stripe hover",
                    options = list(scrollX = TRUE, dom = "t", paging = FALSE))
    })
    output$odds_note <- renderUI({
      if (!isTRUE(is_logistic())) return(NULL)
      tags$p(class = "text-muted small mt-2 mb-0", sprintf(
        "Odds of '%s'. An odds ratio > 1 raises the odds, < 1 lowers them; a numeric predictor's OR is per one-unit increase.",
        attr(model(), "success") %||% "the modelled level"))
    })

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
      logit <- identical(info$family, "binomial")
      test_lab <- if (logit) "likelihood-ratio test" else "F-test"
      overall_tag <- if (!is.na(op) && op < 0.05)
        tags$p(tags$span(style = "color:#2e7d32; font-weight:600;",
          icon("circle-check"),
          sprintf(" The overall model is statistically significant (%s, %s).",
                  test_lab, p_label)))
      else
        tags$p(tags$span(style = "color:#c62828; font-weight:600;",
          icon("circle-xmark"),
          sprintf(" The overall model is NOT statistically significant (%s, %s).",
                  test_lab, p_label)))
      fit_line <- if (logit)
        tags$p(tags$b(sprintf("McFadden R\u00b2 = %s", info$r2)),
               " \u2014 a pseudo-R\u00b2 for logistic models (0.2\u20130.4 already ",
               "indicates a good fit; it does not mean % of variance).")
      else
        tags$p(tags$b(sprintf("R\u00b2 = %s", info$r2)), " \u2014 explains ",
               tags$b(paste0(round(info$r2 * 100, 1), "%")), " of the variance. ",
               tags$span(style = "color:#555;",
                         sprintf("(Adj. R\u00b2 = %s)", info$adj_r2)))
      tagList(
        overall_tag,
        fit_line,
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
    # Static (these carry a loess smooth / per-obs stems that ggplotly mangles).
    # Q-Q and Scale-Location assume normal residuals -- meaningless for logistic,
    # so they show a note there instead.
    linear_only <- function(gg) {
      validate(need(!is.null(model()), "Fit a model first."))
      validate(need(!isTRUE(is_logistic()),
                    "This diagnostic is for linear models; logistic uses deviance residuals (see Residuals vs Fitted and Cook's distance)."))
      gg(model())
    }
    output$plot_qq       <- renderPlot(linear_only(reg_qq_gg), res = 96)
    output$plot_scaleloc <- renderPlot(linear_only(reg_scale_loc_gg), res = 96)
    output$plot_cooks <- renderPlot({
      validate(need(!is.null(model()), "Fit a model first."))
      reg_cooks_gg(model())
    }, res = 96)

    # -- assumption checks + VIF -----------------------------------------------
    output$assumptions <- renderUI({
      if (is.null(model()))
        return(tags$p(class = "text-muted fst-italic",
                      "Fit a model to check its assumptions."))
      a <- reg_assumptions(model())
      if (is.null(a))
        return(tags$p(class = "text-muted",
          "These assumption checks are for linear regression. Logistic models are assessed by deviance and classification accuracy (Fit statistics) and the residual/Cook's plots above."))
      badge <- function(ok) {
        if (is.na(ok))
          tags$span(class = "badge bg-secondary", "n/a")
        else if (isTRUE(ok))
          tags$span(class = "badge bg-success", "OK")
        else
          tags$span(class = "badge bg-warning text-dark", "Check")
      }
      rows <- lapply(seq_len(nrow(a)), function(i) {
        p_txt <- if (is.na(a$p_value[i])) "" else
          sprintf(" (%s, p = %s)", a$Test[i],
                  if (a$p_value[i] < 0.001) "< 0.001" else round(a$p_value[i], 3))
        tags$li(class = "mb-1", badge(a$OK[i]), " ",
                tags$b(a$Assumption[i]), tags$span(class = "text-muted", p_txt))
      })
      tagList(
        tags$ul(class = "list-unstyled mb-2", rows),
        tags$p(class = "text-muted small mb-0", attr(a, "independence_note"))
      )
    })

    output$vif <- DT::renderDT({
      validate(need(!is.null(model()), "Fit a model first."))
      v <- reg_vif(model())
      validate(need(!is.null(v),
                    "VIF needs at least two predictor terms."))
      DT::datatable(v, rownames = FALSE, class = "compact stripe",
                    options = list(dom = "t", paging = FALSE, scrollX = TRUE))
    })
    output$vif_note <- renderUI({
      if (is.null(model()) || is.null(reg_vif(model()))) return(NULL)
      tags$p(class = "text-muted small mt-2 mb-0",
             "VIF > 5 is moderate, > 10 is high multicollinearity. Factor and ",
             "interaction terms are shown per level.")
    })

    # -- model comparison (A saved vs B current) -------------------------------
    saved_model <- reactiveVal(NULL)
    observeEvent(input$save_a, {
      req(!is.null(model()))
      saved_model(model())
      showNotification("Saved the current fit as Model A.", type = "message")
    })
    # A saved model no longer matches a new dataset.
    observeEvent(data_in(), saved_model(NULL), ignoreNULL = FALSE)

    output$saved_a <- renderUI({
      m <- saved_model()
      if (is.null(m))
        return(tags$p(class = "text-muted fst-italic mt-2 mb-0",
                      "No model saved yet."))
      f <- gsub("\\s+", " ", paste(deparse(stats::formula(m)), collapse = " "))
      tags$p(class = "mt-2 mb-0",
             tags$b("Model A: "), tags$code(f))
    })

    comparison <- reactive({
      req(!is.null(saved_model()), !is.null(model()))
      reg_compare(saved_model(), model())
    })

    output$compare <- DT::renderDT({
      validate(need(!is.null(saved_model()),
                    "Save a model as A, then fit another to compare."))
      validate(need(!is.null(model()), "Fit a current model (B)."))
      cmp <- comparison()
      validate(need(is.null(cmp$error), cmp$error %||% "Comparison failed."))
      validate(need(!is.null(cmp$table),
                    "These models can't be compared (see the notes below)."))
      DT::datatable(cmp$table, rownames = FALSE, class = "compact stripe",
                    options = list(dom = "t", paging = FALSE, scrollX = TRUE))
    })
    output$compare_notes <- renderUI({
      if (is.null(saved_model()) || is.null(model())) return(NULL)
      cmp <- comparison()
      deltas <- if (!is.null(cmp$table))
        tags$p(class = "small mt-2 mb-1", tags$b("B - A: "),
               sprintf("AIC %+.3g, BIC %+.3g (negative favours B).",
                       cmp$aic_delta, cmp$bic_delta))
      tagList(
        deltas,
        lapply(cmp$warnings, function(w)
          tags$p(class = "text-muted small mb-1", w))
      )
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
