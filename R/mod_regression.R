# ============================================================
# mod_regression.R -- linear / multiple / polynomial regression
# ============================================================
# Thin wrapper over R/helpers_model.R. Pick a numeric response and one or
# more numeric predictors, fit the model, and read the summary, a
# plain-English interpretation, and the two diagnostic plots. Returns the
# fitted-model reactive. Requires ggplot2 + plotly attached by the app.

regressionUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 290,
      h5("Model setup"),
      uiOutput(ns("ui_resp")),
      uiOutput(ns("ui_pred")),
      selectInput(ns("type"),
        tagList("Model type", info_tip(HTML(
          "<b>Simple Linear:</b> one predictor, straight line (Y ~ X).<br>
           <b>Multiple Linear:</b> two or more predictors (Y ~ X1 + X2 + \u2026).<br>
           <b>Polynomial:</b> curved fit using powers of one predictor."))),
        choices = c("Simple Linear" = "linear", "Multiple Linear" = "multiple",
                    "Polynomial" = "polynomial")),
      conditionalPanel(
        sprintf("input['%s'] == 'polynomial'", ns("type")),
        sliderInput(ns("poly_deg"), "Polynomial degree", min = 2, max = 6,
                    value = 2)
      ),
      hr(),
      actionButton(ns("fit"), "Fit model", class = "btn-primary w-100",
                   icon = icon("play")),
      br(), br(),
      downloadButton(ns("download"), "Export summary (.txt)",
                     class = "btn-outline-secondary w-100")
    ),
    layout_columns(
      col_widths = c(5, 7),
      tagList(
        card(card_header(icon("file-alt"), " Model summary"),
             verbatimTextOutput(ns("summary"))),
        card(card_header(icon("lightbulb"), " Statistical interpretation"),
             uiOutput(ns("interpretation")))
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Fitted vs Actual"),
             plotly::plotlyOutput(ns("plot_fitted"), height = "300px")),
        card(card_header("Residuals vs Fitted"),
             plotly::plotlyOutput(ns("plot_resid"), height = "300px"))
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
      nums     <- cols_num()
      if (!length(nums)) return(NULL)
      is_multi <- identical(input$type, "multiple")
      lbl <- if (is_multi) "Predictor variables (X)" else "Predictor variable (X)"
      tip <- if (is_multi)
        "Two or more numeric variables. Hold Ctrl/Cmd to select several."
      else "One numeric variable to predict the response."
      selectInput(ns("pred"), tagList(lbl, info_tip(tip)),
                  choices = nums, multiple = is_multi)
    })

    observeEvent(input$fit, {
      df <- data_in()
      req(is.data.frame(df), input$resp, input$pred)
      tryCatch({
        model(fit_model(df, input$resp, input$pred,
                        type = input$type %||% "linear",
                        degree = input$poly_deg %||% 2))
        showNotification("Model fitted successfully.", type = "message")
      }, error = function(e)
        showNotification(paste("Fitting error:", e$message), type = "error",
                         duration = 8))
    })

    # summary() is the expensive call; compute once, share.
    model_summary <- reactive({ req(model()); summary(model()) })

    output$summary <- renderPrint({
      if (is.null(model())) cat("Fit a model using the panel on the left.\n")
      else                  model_summary()
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
      req(model())
      plotly::ggplotly(reg_fitted_gg(model())) |>
        plotly::layout(margin = list(t = 90, b = 40, l = 55, r = 20))
    })
    output$plot_resid <- plotly::renderPlotly({
      req(model())
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
