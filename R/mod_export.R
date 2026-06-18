# ============================================================
# mod_export.R -- the Export stage
# ============================================================
# Download the data that flows in (CSV / Excel / RDS). Optionally also export
# charts (when a `plots` reactive is supplied, e.g. from mod_visualize) and
# regression results (when a `model` reactive is supplied, e.g. from
# mod_regression). Pattern A terminal stage: returns nothing.
#
# exportServer(id, data_in, plots = NULL, model = NULL, summary_tbl = NULL)
#   data_in     : reactive(data frame | NULL)
#   plots       : reactive(list of ggplots) | NULL -> shows an "Export charts" block
#   model       : reactive(lm | NULL)        | NULL -> shows an "Export regression" block
#   summary_tbl : reactive(data frame | NULL)| NULL -> shows an "Export summary" block
#
# The chart export needs render_plots_to_file() (helpers_plot.R) and ggplot2;
# the regression preview needs model_interpretation() (helpers_model.R). Only the
# app that passes those reactives (data_explorer) attaches/sources them.

exportUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      h5("Export data"),
      textInput(ns("filename"), "File name (no extension)",
                value = "data-export"),
      radioButtons(ns("fmt"), "Format",
                   choices = c("CSV (.csv)"    = "csv",
                               "Excel (.xlsx)" = "xlsx",
                               "R data (.rds)" = "rds"),
                   selected = "csv"),
      downloadButton(ns("download"), "Download data", class = "btn-primary w-100"),
      helpText("Downloads the data exactly as it stands at this point in the ",
               "pipeline."),
      uiOutput(ns("summary_ui")),  # filled only when a summary reactive is given
      uiOutput(ns("charts_ui")),   # filled only when a plots reactive is given
      uiOutput(ns("model_ui"))     # filled only when a model reactive is given
    ),
    card(
      card_header(icon("file-export"), " Data to export"),
      textOutput(ns("caption")),
      DT::DTOutput(ns("preview"))
    ),
    uiOutput(ns("summary_preview_ui")), # filled only when a summary reactive is given
    uiOutput(ns("charts_preview_ui")),  # filled only when a plots reactive is given
    uiOutput(ns("model_preview_ui"))    # filled only when a model reactive is given
  )
}

exportServer <- function(id, data_in, plots = NULL, model = NULL,
                         summary_tbl = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$caption <- renderText({
      d <- data_in()
      if (is.null(d) || !is.data.frame(d))
        return("Nothing to export yet \u2014 import (and optionally transform) first.")
      sprintf("%s rows \u00d7 %s columns will be exported.",
              format(nrow(d), big.mark = ","), ncol(d))
    })

    output$preview <- DT::renderDT({
      d <- data_in()
      req(is.data.frame(d))
      DT::datatable(utils::head(d, 200), rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE))
    })

    # Sanitise the user's file name to a safe stem; fall back if blank.
    safe_stem <- reactive({
      stem <- gsub("[^A-Za-z0-9._-]+", "_", input$filename %||% "")
      if (nzchar(stem)) stem else "data-export"
    })

    output$download <- downloadHandler(
      filename = function() paste0(safe_stem(), ".", input$fmt %||% "csv"),
      content  = function(file) {
        d <- data_in()
        validate(need(is.data.frame(d), "No data to export."))
        switch(input$fmt %||% "csv",
          csv  = utils::write.csv(d, file, row.names = FALSE),
          xlsx = writexl::write_xlsx(d, file),
          rds  = saveRDS(d, file))
      }
    )

    # -- Optional: export + preview the summary table (from mod_summarize) --
    if (!is.null(summary_tbl)) {
      summ_or_null <- function() tryCatch(summary_tbl(), error = function(e) NULL)
      output$summary_ui <- renderUI({
        tagList(
          hr(), h6("Export summary"),
          downloadButton(ns("dl_summary"), "Summary table (.csv)",
                         class = "btn-outline-secondary w-100")
        )
      })
      output$summary_preview_ui <- renderUI({
        card(card_header(icon("layer-group"), " Summary preview"),
             DT::DTOutput(ns("summary_preview")))
      })
      output$summary_preview <- DT::renderDT({
        d <- summ_or_null()
        validate(need(is.data.frame(d) && nrow(d) >= 1L,
                      "Build a summary on the Summarize tab to preview it here."))
        DT::datatable(d, rownames = FALSE, class = "compact stripe",
                      options = list(scrollX = TRUE, pageLength = 10, dom = "tip"))
      })
      output$dl_summary <- downloadHandler(
        filename = function() paste0(safe_stem(), "_summary.csv"),
        content  = function(f) {
          d <- summ_or_null()
          validate(need(is.data.frame(d), "No summary to export yet."))
          utils::write.csv(d, f, row.names = FALSE)
        }
      )
    }

    # -- Optional: export charts (from mod_visualize) ----------
    if (!is.null(plots)) {
      # Preview the exact grid that will be exported.
      output$charts_preview_ui <- renderUI({
        card(card_header(icon("chart-line"), " Chart preview"),
             plotOutput(ns("charts_preview"), height = "440px"))
      })
      output$charts_preview <- renderPlot({
        pl <- plots()
        validate(need(length(pl) >= 1L,
                      "Configure at least one chart on the Visualize tab."))
        draw_plot_grid(pl)
      }, bg = "white")

      output$charts_ui <- renderUI({
        tagList(
          hr(), h6("Export charts"),
          selectInput(ns("plot_fmt"), "Image format",
                      choices = c("PNG" = "png", "PDF" = "pdf")),
          sliderInput(ns("plot_w"), "Width per plot (in)",  3, 12, 6,   0.5),
          sliderInput(ns("plot_h"), "Height per plot (in)", 3, 12, 4.5, 0.5),
          conditionalPanel(
            sprintf("input['%s'] == 'png'", ns("plot_fmt")),
            sliderInput(ns("plot_dpi"), "Resolution (DPI)", 72, 300, 150, 1)),
          downloadButton(ns("dl_plots"), "Download chart(s)",
                         class = "btn-success w-100")
        )
      })
      output$dl_plots <- downloadHandler(
        filename = function()
          paste0(safe_stem(), "_charts.", input$plot_fmt %||% "png"),
        content = function(file) {
          pl <- plots()
          validate(need(length(pl) >= 1L,
                        "Configure at least one chart on the Visualize tab first."))
          render_plots_to_file(pl, file, input$plot_fmt %||% "png",
                               input$plot_w %||% 6, input$plot_h %||% 4.5,
                               input$plot_dpi %||% 150)
        }
      )
    }

    # -- Optional: export regression results (from mod_regression) -
    if (!is.null(model)) {
      output$model_ui <- renderUI({
        tagList(
          hr(), h6("Export regression"),
          downloadButton(ns("dl_summary"), "Model summary (.txt)",
                         class = "btn-outline-secondary w-100 mb-1"),
          downloadButton(ns("dl_coefs"), "Coefficients (.csv)",
                         class = "btn-outline-secondary w-100 mb-1"),
          downloadButton(ns("dl_fitted"), "Fitted & actual (.csv)",
                         class = "btn-outline-secondary w-100 mb-1"),
          downloadButton(ns("dl_resid"), "Residuals (.csv)",
                         class = "btn-outline-secondary w-100 mb-2"),
          selectInput(ns("reg_plot_fmt"), "Diagnostic plot format",
                      choices = c("PNG" = "png", "PDF" = "pdf")),
          downloadButton(ns("dl_reg_plots"), "Diagnostic plots",
                         class = "btn-success w-100")
        )
      })
      # Preview the fitted model so Export mirrors the Regression tab.
      output$model_preview_ui <- renderUI({
        card(card_header(icon("chart-simple"), " Regression preview"),
             verbatimTextOutput(ns("model_preview_text")),
             uiOutput(ns("model_preview_interp")),
             tags$h6(class = "mt-2", "Diagnostic plots"),
             plotOutput(ns("model_preview_plots"), height = "320px"))
      })
      # The two diagnostic ggplots, side by side (Fitted vs Actual, Residuals).
      reg_diag_plots <- function(m) list(reg_fitted_gg(m), reg_resid_gg(m))
      output$model_preview_plots <- renderPlot({
        m <- tryCatch(model(), error = function(e) NULL)
        validate(need(!is.null(m),
                      "Fit a model on the Regression tab to preview its plots."))
        draw_plot_grid(reg_diag_plots(m))
      }, bg = "white")
      output$model_preview_text <- renderPrint({
        m <- tryCatch(model(), error = function(e) NULL)
        if (is.null(m)) cat("Fit a model on the Regression tab to preview it here.\n")
        else            summary(m)
      })
      output$model_preview_interp <- renderUI({
        m <- tryCatch(model(), error = function(e) NULL)
        if (is.null(m)) return(NULL)
        info <- model_interpretation(m)
        op   <- info$overall_p
        p_label <- if (!is.na(op)) {
          if (op < 0.001) "p < 0.001" else paste0("p = ", round(op, 4))
        } else "p = N/A"
        sig <- !is.na(op) && op < 0.05
        tags$p(class = "mt-2 mb-1",
          tags$b(sprintf("R\u00b2 = %s", info$r2)),
          sprintf(" \u2014 explains %s%% of the variance. ", round(info$r2 * 100, 1)),
          tags$span(
            style = sprintf("color:%s; font-weight:600;",
                            if (sig) "#2e7d32" else "#c62828"),
            if (sig) sprintf("Overall model is significant (%s).", p_label)
            else     sprintf("Overall model is not significant (%s).", p_label)))
      })

      need_model <- function()
        validate(need(!is.null(model()), "Fit a model on the Regression tab first."))
      output$dl_summary <- downloadHandler(
        filename = function() paste0(safe_stem(), "_model_summary.txt"),
        content  = function(f) { need_model(); utils::capture.output(summary(model()), file = f) }
      )
      output$dl_coefs <- downloadHandler(
        filename = function() paste0(safe_stem(), "_coefficients.csv"),
        content  = function(f) {
          need_model()
          co <- as.data.frame(summary(model())$coefficients)
          co <- cbind(Term = rownames(co), co)
          utils::write.csv(co, f, row.names = FALSE)
        }
      )
      output$dl_fitted <- downloadHandler(
        filename = function() paste0(safe_stem(), "_fitted.csv"),
        content  = function(f) {
          need_model(); m <- model()
          utils::write.csv(data.frame(actual = m$model[[1]], fitted = stats::fitted(m)),
                           f, row.names = FALSE)
        }
      )
      output$dl_resid <- downloadHandler(
        filename = function() paste0(safe_stem(), "_residuals.csv"),
        content  = function(f) {
          need_model(); m <- model()
          utils::write.csv(data.frame(fitted = stats::fitted(m), residual = stats::residuals(m)),
                           f, row.names = FALSE)
        }
      )
      output$dl_reg_plots <- downloadHandler(
        filename = function()
          paste0(safe_stem(), "_regression_diagnostics.", input$reg_plot_fmt %||% "png"),
        content  = function(file) {
          need_model()
          render_plots_to_file(reg_diag_plots(model()), file,
                               input$reg_plot_fmt %||% "png", 6, 4.5, 150)
        }
      )
    }

    invisible(NULL)
  })
}
