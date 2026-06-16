# ============================================================
# mod_summarize.R — summary statistics by group
# ============================================================
# Two modes over R/helpers_stats.R, both "by group":
#   • Means       — count/mean/median/mode/min/max/SD/SE/IQR of numeric vars
#                   (grouped_summary).
#   • Proportions — percent of each level of a categorical outcome, with exact
#                   (Clopper–Pearson) binomial CIs (proportions_summary; needs
#                   the binom package).
# Takes the working data as a reactive; returns the current summary table.
#
# Requires R/helpers_stats.R (grouped_summary, proportions_summary,
# numeric_cols, groupable_cols) and, for Proportions, the binom package.

summarizeUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      radioButtons(ns("mode"), "Summary type",
                   choices = c("Means by group"       = "means",
                               "Proportions by group" = "props"),
                   selected = "means"),

      selectizeInput(
        ns("groups"),
        tagList("Grouped by", info_tip(
          "The variable(s) that define the groups — e.g. cyl. With several, ",
          "you get one row per combination.")),
        choices = NULL, multiple = TRUE,
        options = list(placeholder = "pick grouping variable(s)")
      ),

      conditionalPanel(
        sprintf("input['%s'] == 'means'", ns("mode")),
        selectizeInput(
          ns("vars"),
          tagList("Summarize (numeric)", info_tip(
            "The numeric variable(s) to summarize within each group — e.g. mpg.")),
          choices = NULL, multiple = TRUE,
          options = list(placeholder = "pick numeric variable(s)")),
        helpText("Count, mean, median, mode, min, max, SD, SE, and IQR within ",
                 "each group. Mode is blank where nothing repeats.")
      ),
      conditionalPanel(
        sprintf("input['%s'] == 'props'", ns("mode")),
        selectInput(
          ns("outcome"),
          tagList("Outcome (category)", info_tip(
            "The categorical variable whose level percentages you want within ",
            "each group — e.g. transmission type.")),
          choices = NULL),
        selectInput(ns("conf"), "Confidence level",
                    choices = c("90%" = "0.90", "95%" = "0.95", "99%" = "0.99"),
                    selected = "0.95"),
        helpText("Percent of each level within each group, with exact ",
                 "(Clopper–Pearson) binomial confidence intervals.")
      ),

      hr(),
      downloadButton(ns("download"), "Download table (.csv)",
                     class = "btn-success w-100")
    ),
    card(
      card_header(icon("layer-group"), textOutput(ns("card_title"), inline = TRUE)),
      textOutput(ns("caption")),
      DT::DTOutput(ns("table"))
    )
  )
}

summarizeServer <- function(id, data_in) {
  moduleServer(id, function(input, output, session) {

    observeEvent(data_in(), {
      df <- data_in()
      req(is.data.frame(df))
      nums <- numeric_cols(df)
      grps <- groupable_cols(df)
      updateSelectizeInput(session, "vars", choices = nums,
                           selected = utils::head(nums, 1))
      updateSelectizeInput(session, "groups", choices = grps,
                           selected = utils::head(grps, 1))
      # Outcome: a categorical, ideally distinct from the default grouping var.
      out_default <- setdiff(grps, utils::head(grps, 1))
      updateSelectInput(session, "outcome", choices = grps,
                        selected = utils::head(c(out_default, grps), 1))
    }, ignoreNULL = TRUE)

    table_df <- reactive({
      df <- data_in()
      validate(need(is.data.frame(df),
                    "Import data on the Import tab to begin."))
      validate(need(length(input$groups) >= 1L,
                    "Pick at least one grouping variable."))
      if (identical(input$mode, "props")) {
        validate(need(nzchar(input$outcome %||% ""), "Choose an outcome column."))
        out <- proportions_summary(df, input$outcome, input$groups,
                                   conf_level = as.numeric(input$conf %||% "0.95"))
        validate(need(!is.null(out), "Couldn't summarize with those selections."))
        out
      } else {
        validate(need(length(input$vars) >= 1L,
                      "Pick at least one numeric variable."))
        out <- grouped_summary(df, input$vars, input$groups)
        validate(need(!is.null(out), "Couldn't summarize with those selections."))
        out
      }
    })

    output$card_title <- renderText({
      if (identical(input$mode, "props")) " Proportions by group"
      else " Summary statistics by group"
    })

    output$caption <- renderText({
      if (!length(input$groups)) return("")
      g <- paste(input$groups, collapse = " × ")
      if (identical(input$mode, "props")) {
        if (!nzchar(input$outcome %||% "")) return("")
        sprintf("Percent of each level of %s within each %s, with exact %s%% binomial CIs.",
                input$outcome, g, sub("0[.]", "", input$conf %||% "0.95"))
      } else {
        if (!length(input$vars)) return("")
        sprintf("Count, mean, median, mode, min, max, SD, SE, and IQR of %s, grouped by %s.",
                paste(input$vars, collapse = ", "), g)
      }
    })

    output$table <- DT::renderDT({
      DT::datatable(table_df(), rownames = FALSE, class = "compact stripe hover",
                    options = list(scrollX = TRUE, pageLength = 15))
    })

    output$download <- downloadHandler(
      filename = function() {
        stem <- if (identical(input$mode, "props")) "group_proportions"
                else "group_summary"
        paste0(stem, "_", Sys.Date(), ".csv")
      },
      content  = function(f) {
        d <- tryCatch(table_df(), error = function(e) NULL)
        validate(need(!is.null(d), "Nothing to download yet."))
        utils::write.csv(d, f, row.names = FALSE)
      }
    )

    table_df
  })
}
