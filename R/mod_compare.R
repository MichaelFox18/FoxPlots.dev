# ============================================================
# mod_compare.R — Compare Groups (hypothesis testing)
# ============================================================
# Thin wrapper over R/helpers_compare.R. Two modes:
#   • Numeric outcome by group — t-test / ANOVA (or Wilcoxon / Kruskal–Wallis),
#     with assumption checks (normality, equal variance), effect sizes, Tukey
#     post-hoc, a boxplot, and a plain-English verdict.
#   • Two categorical variables — chi-square (with a Fisher's-exact fallback),
#     the contingency table, Cramér's V, and a grouped bar chart.
# Returns the current result reactive (a list, or NULL).
#
# Requires R/helpers_compare.R, R/helpers_stats.R (numeric_cols/groupable_cols),
# R/helpers_plot.R (build_full_plot) and ggplot2 attached by the app.

# Round a p-value for display.
fmt_p <- function(p) {
  if (is.null(p) || is.na(p)) return("N/A")
  if (p < 0.001) "p < 0.001" else paste0("p = ", round(p, 4))
}

compareUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      radioButtons(ns("mode"), "What do you want to compare?",
                   choices = c("A number across groups"   = "num",
                               "Two categorical variables" = "cat"),
                   selected = "num"),

      # ---- numeric-outcome mode ----
      conditionalPanel(
        sprintf("input['%s'] == 'num'", ns("mode")),
        selectInput(ns("outcome"),
          tagList("Outcome (numeric)", info_tip(
            "The numeric measurement to compare — e.g. mpg.")),
          choices = NULL),
        selectInput(ns("group"),
          tagList("Groups (category)", info_tip(
            "The categorical variable whose levels define the groups — e.g. ",
            "cyl or transmission. Two levels → t-test; three or more → ANOVA.")),
          choices = NULL),
        radioButtons(ns("method"), "Test family",
          choices = c("Parametric (t-test / ANOVA)"          = "param",
                      "Non-parametric (Wilcoxon / Kruskal)"  = "nonparam"),
          selected = "param"),
        conditionalPanel(
          sprintf("input['%s'] == 'param'", ns("method")),
          checkboxInput(ns("var_equal"),
            tagList("Assume equal variances", info_tip(
              "Off (default) uses Welch's t-test, which is safer when the two ",
              "groups' spreads differ. Only affects the two-group t-test.")),
            value = FALSE))
      ),

      # ---- categorical mode ----
      conditionalPanel(
        sprintf("input['%s'] == 'cat'", ns("mode")),
        selectInput(ns("cat1"),
          tagList("Variable 1 (category)", info_tip(
            "First categorical variable — its levels become the table rows.")),
          choices = NULL),
        selectInput(ns("cat2"),
          tagList("Variable 2 (category)", info_tip(
            "Second categorical variable — its levels become the table columns.")),
          choices = NULL)
      ),

      hr(),
      downloadButton(ns("download"), "Download results (.txt)",
                     class = "btn-success w-100")
    ),

    uiOutput(ns("results_ui"))
  )
}

compareServer <- function(id, data_in) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Populate the variable choices whenever the working data changes, but leave
    # them UNSELECTED ("Choose a variable…") so the test only runs once the user
    # picks — no auto-run on a default selection (slow on large data).
    observeEvent(data_in(), {
      df <- data_in(); req(is.data.frame(df))
      ph_n <- c("Choose a variable…" = "", numeric_cols(df))
      ph_c <- c("Choose a variable…" = "", groupable_cols(df))
      updateSelectInput(session, "outcome", choices = ph_n, selected = "")
      updateSelectInput(session, "group",   choices = ph_c, selected = "")
      updateSelectInput(session, "cat1",    choices = ph_c, selected = "")
      updateSelectInput(session, "cat2",    choices = ph_c, selected = "")
    }, ignoreNULL = TRUE)

    # The current test result (a structured list) or NULL. Warnings from the
    # rank tests / chi-square on small samples are expected; keep them quiet.
    result <- reactive({
      df <- data_in()
      validate(need(is.data.frame(df), "Import data on the Import tab to begin."))
      if (identical(input$mode, "cat")) {
        validate(need(nzchar(input$cat1 %||% "") && nzchar(input$cat2 %||% ""),
                      "Pick two categorical variables."))
        validate(need(!identical(input$cat1, input$cat2),
                      "Pick two different variables."))
        out <- suppressWarnings(compare_categorical(df, input$cat1, input$cat2))
        validate(need(!is.null(out),
          "Each variable needs at least two categories (and they must differ)."))
        out$mode <- "cat"; out
      } else {
        validate(need(nzchar(input$outcome %||% "") && nzchar(input$group %||% ""),
                      "Pick a numeric outcome and a grouping variable."))
        out <- suppressWarnings(compare_groups_numeric(
          df, input$outcome, input$group,
          parametric = identical(input$method, "param"),
          var_equal  = isTRUE(input$var_equal)))
        validate(need(!is.null(out),
          "Need a numeric outcome and a group with 2+ levels and 2+ rows each."))
        out$mode <- "num"; out
      }
    })

    # ---- headline result card -------------------------------------------------
    output$result_card <- renderUI({
      r <- result()
      if (identical(r$mode, "cat")) {
        stat_line <- sprintf("χ²(%d, N = %d) = %s,  %s",
                             r$df, r$n, round(r$statistic, 3), fmt_p(r$p_value))
        rows <- list(
          tags$li(tags$b("Test: "), "Pearson's chi-square test of independence"),
          tags$li(tags$b("Statistic: "), stat_line),
          tags$li(tags$b("Cramér's V: "), round(r$cramers_v, 3),
                  sprintf(" (%s association)", effect_magnitude("Cramér's V", r$cramers_v))))
        if (!is.na(r$fisher_p))
          rows <- c(rows, list(tags$li(tags$b("Fisher's exact (fallback): "),
                                       fmt_p(r$fisher_p))))
      } else {
        df_txt <- if (!is.na(r$df2)) sprintf("(%g, %g)", r$df, r$df2)
                  else if (!is.na(r$df)) sprintf("(%g)", r$df) else ""
        stat_line <- sprintf("%s = %s%s,  %s",
                             if (grepl("ANOVA", r$test)) "F" else
                             if (grepl("Kruskal", r$test)) "χ²" else
                             if (grepl("Wilcoxon", r$test)) "W" else "t",
                             round(r$statistic, 3), df_txt, fmt_p(r$p_value))
        rows <- list(
          tags$li(tags$b("Test: "), r$test, sprintf(" (%d groups, N = %d)", r$k, r$n)),
          tags$li(tags$b("Statistic: "), stat_line))
        if (!is.na(r$effect_value))
          rows <- c(rows, list(tags$li(tags$b(paste0(r$effect_name, ": ")),
            round(r$effect_value, 3),
            sprintf(" (%s effect)", effect_magnitude(r$effect_name, r$effect_value)))))
      }
      tags$ul(class = "list-unstyled mb-0", rows)
    })

    # ---- plain-English interpretation ----------------------------------------
    output$interp <- renderUI({
      r   <- result()
      sig <- !is.na(r$p_value) && r$p_value < 0.05
      verdict <- function(text_yes, text_no)
        tags$span(style = sprintf("color:%s; font-weight:600;",
                                  if (sig) "#2e7d32" else "#c62828"),
                  icon(if (sig) "circle-check" else "circle-xmark"),
                  if (sig) text_yes else text_no)
      if (identical(r$mode, "cat")) {
        body <- tagList(
          tags$p(verdict(
            sprintf(" %s and %s are significantly associated (%s).",
                    r$var1, r$var2, fmt_p(r$p_value)),
            sprintf(" No significant association between %s and %s (%s).",
                    r$var1, r$var2, fmt_p(r$p_value)))),
          tags$p("Strength of association (Cramér's V): ",
                 tags$b(round(r$cramers_v, 3)),
                 sprintf(" — %s.", effect_magnitude("Cramér's V", r$cramers_v))),
          if (r$low_expected > 0)
            tags$p(class = "text-warning small",
                   icon("triangle-exclamation"),
                   sprintf(" %d%% of cells have an expected count below 5, so the chi-square result is approximate — rely on the Fisher's exact p-value (%s) instead.",
                           round(100 * r$low_expected), fmt_p(r$fisher_p))))
      } else {
        eff <- if (!is.na(r$effect_value))
          tags$p("Effect size (", r$effect_name, "): ", tags$b(round(r$effect_value, 3)),
                 sprintf(" — a %s effect.", effect_magnitude(r$effect_name, r$effect_value)))
        body <- tagList(
          tags$p(verdict(
            sprintf(" There is a statistically significant difference in %s across %s (%s, %s).",
                    r$outcome, r$group, r$test, fmt_p(r$p_value)),
            sprintf(" No statistically significant difference in %s across %s (%s, %s).",
                    r$outcome, r$group, r$test, fmt_p(r$p_value)))),
          eff,
          if (sig && !is.null(r$posthoc))
            tags$p(class = "text-muted small",
                   "See the Tukey post-hoc table for which specific groups differ."))
      }
      tagList(body, tags$p(class = "text-muted small mt-2",
        "α = 0.05. Statistical significance does not imply practical importance."))
    })

    # ---- assumptions (numeric mode) ------------------------------------------
    output$assumptions <- renderUI({
      r <- result(); req(identical(r$mode, "num"))
      df <- data_in()
      nt <- normality_table(df, input$outcome, input$group)
      lv <- levene_test(df[[input$outcome]], df[[input$group]])
      non_normal <- sum(!is.na(nt$Normal) & !nt$Normal)
      norm_msg <- if (non_normal > 0)
        tags$span(class = "text-warning",
          sprintf("%d group(s) deviate from normality (Shapiro–Wilk p < 0.05). For small samples, prefer the non-parametric option.",
                  non_normal))
      else tags$span(class = "text-success",
                     "No strong departures from normality detected.")
      var_msg <- if (!is.null(lv) && !lv$equal)
        tags$span(class = "text-warning",
          sprintf("Group variances differ (Levene p = %s). Welch's t-test (the 2-group default) handles this; for 3+ groups consider the non-parametric option.",
                  round(lv$p_value, 4)))
      else tags$span(class = "text-success", "Group variances look comparable.")
      tagList(
        tags$p(tags$b("Normality: "), norm_msg),
        DT::DTOutput(ns("normality_tbl")),
        tags$p(class = "mt-2", tags$b("Equal variance: "), var_msg))
    })

    output$normality_tbl <- DT::renderDT({
      r <- result(); req(identical(r$mode, "num"))
      nt <- normality_table(data_in(), input$outcome, input$group)
      DT::datatable(nt, rownames = FALSE, class = "compact stripe",
                    options = list(dom = "t", pageLength = 20))
    })

    # ---- post-hoc table (ANOVA only) -----------------------------------------
    output$posthoc <- DT::renderDT({
      r <- result(); req(identical(r$mode, "num"), !is.null(r$posthoc))
      DT::datatable(r$posthoc, rownames = FALSE, class = "compact stripe",
                    options = list(scrollX = TRUE, pageLength = 10, dom = "tip"))
    })

    # ---- contingency table (categorical mode) --------------------------------
    output$contingency <- DT::renderDT({
      r <- result(); req(identical(r$mode, "cat"))
      tab <- as.data.frame.matrix(r$table)
      tab <- cbind(` ` = rownames(tab), tab)
      DT::datatable(tab, rownames = FALSE, class = "compact stripe hover",
                    options = list(scrollX = TRUE, dom = "t"))
    })

    # ---- plot -----------------------------------------------------------------
    output$plot <- renderPlot({
      r <- result()
      if (identical(r$mode, "cat"))
        build_full_plot(data_in(), list(type = "bar", x = r$var1, color = r$var2,
                                        palette = "auto", legend_pos = "right"))
      else
        build_full_plot(data_in(), list(type = "boxplot", x = r$group,
                                        y = r$outcome, color = r$group,
                                        palette = "auto", legend_pos = "none"))
    }, bg = "white")

    # ---- assembled results layout --------------------------------------------
    output$results_ui <- renderUI({
      common <- list(
        card(card_header(icon("flask-vial"), " Result"), uiOutput(ns("result_card"))),
        card(card_header(icon("lightbulb"), " Interpretation"), uiOutput(ns("interp"))),
        card(card_header(icon("chart-column"), " Chart"),
             plotOutput(ns("plot"), height = "330px")))
      extra <- if (identical(input$mode, "cat"))
        list(card(card_header(icon("table-cells"), " Contingency table"),
                  DT::DTOutput(ns("contingency"))))
      else
        list(card(card_header(icon("ruler-combined"), " Assumption checks"),
                  uiOutput(ns("assumptions"))),
             card(card_header(icon("layer-group"), " Pairwise comparisons (Tukey HSD)"),
                  helpText("Shown for ANOVA (3+ groups)."),
                  DT::DTOutput(ns("posthoc"))))
      do.call(layout_columns, c(list(col_widths = 6), common, extra))
    })

    # ---- text export ----------------------------------------------------------
    output$download <- downloadHandler(
      filename = function() paste0("group_comparison_", Sys.Date(), ".txt"),
      content  = function(f) {
        r <- tryCatch(result(), error = function(e) NULL)
        validate(need(!is.null(r), "Nothing to export yet."))
        con <- file(f, "w"); on.exit(close(con))
        wl <- function(...) writeLines(paste0(...), con)
        wl("UF/IFAS Data Explorer — Compare Groups")
        wl("Generated: ", as.character(Sys.time())); wl("")
        if (identical(r$mode, "cat")) {
          wl("Mode: association between two categorical variables")
          wl("Variables: ", r$var1, " × ", r$var2)
          wl("Chi-square = ", round(r$statistic, 4), ", df = ", r$df,
             ", ", fmt_p(r$p_value))
          wl("Cramér's V = ", round(r$cramers_v, 4),
             " (", effect_magnitude("Cramér's V", r$cramers_v), ")")
          if (!is.na(r$fisher_p)) wl("Fisher's exact (fallback): ", fmt_p(r$fisher_p))
          wl(""); wl("Contingency table:")
          utils::write.table(as.data.frame.matrix(r$table), con, quote = FALSE,
                             sep = "\t", col.names = NA)
        } else {
          wl("Mode: numeric outcome by group")
          wl("Outcome: ", r$outcome, "   Groups: ", r$group,
             " (", r$k, " levels, N = ", r$n, ")")
          wl("Test: ", r$test)
          wl("Statistic = ", round(r$statistic, 4), ", ", fmt_p(r$p_value))
          if (!is.na(r$effect_value))
            wl(r$effect_name, " = ", round(r$effect_value, 4),
               " (", effect_magnitude(r$effect_name, r$effect_value), ")")
          wl(""); wl("Group statistics:")
          utils::write.table(r$group_stats, con, quote = FALSE, sep = "\t",
                             row.names = FALSE)
          if (!is.null(r$posthoc)) {
            wl(""); wl("Tukey HSD pairwise comparisons:")
            utils::write.table(r$posthoc, con, quote = FALSE, sep = "\t",
                               row.names = FALSE)
          }
        }
      }
    )

    result
  })
}
