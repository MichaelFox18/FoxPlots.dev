# ============================================================
# mod_compare.R -- Compare Groups (hypothesis testing)
# ============================================================
# Thin wrapper over R/helpers_compare.R. Two modes:
#   * Numeric outcome(s) by group(s) -- t-test / ANOVA (or Wilcoxon / Kruskal),
#     with assumption checks, effect sizes, post-hoc (Tukey / Dunn / Steel-Dwass),
#     connecting letters, a means plot and a plain-English verdict. Several
#     outcomes and several grouping variables can be tested at once: every
#     outcome x group combination is run, summarised in one table, and given its
#     own collapsible full-detail section.
#   * Two categorical variables -- chi-square (with a Fisher's-exact fallback),
#     the contingency table (+ optional row / column / total percentages),
#     expected counts, standardized residuals, Cramer's V and a grouped bar chart.
# Returns the current result reactive (a list, or NULL). Its $mode is "num"
# (one combination), "num_multi" (a grid) or "cat".
#
# Requires R/helpers_compare.R, R/helpers_stats.R (numeric_cols/groupable_cols),
# R/helpers_plot.R (build_full_plot, build_means_letters_plot) and ggplot2.

# Round a p-value for display.
fmt_p <- function(p) {
  if (is.null(p) || is.na(p)) return("N/A")
  if (p < 0.001) "p < 0.001" else paste0("p = ", round(p, 4))
}

# Max items each multi-select accepts; the product is capped separately at
# COMPARE_MAX_COMBOS (helpers_compare.R).
COMPARE_MAX_OUTCOMES <- 6L
COMPARE_MAX_GROUPS   <- 4L

P_ADJUST_CHOICES <- c("Benjamini-Hochberg (FDR)" = "BH",
                      "Bonferroni"               = "bonferroni",
                      "Holm"                     = "holm",
                      "None"                     = "none")

# The full card stack for one numeric combination. `i` suffixes every output id
# so a grid can render many stacks side by side; `r` is that combination's
# compare_groups_numeric() result (used only to decide which cards apply).
compare_detail_cards <- function(ns, i, r) {
  nm <- function(x) ns(paste0(x, "_", i))
  ph <- r$posthoc_name %||% "post-hoc"
  cards <- list(
    card(card_header(icon("flask-vial"), " Result"), uiOutput(nm("result_card"))),
    card(card_header(icon("lightbulb"), " Interpretation"), uiOutput(nm("interp"))),
    card(card_header(icon("chart-column"), " Chart"),
         plotOutput(nm("plot"), height = "330px")),
    card(card_header(icon("table-list"), " Group means"),
         helpText("Mean with standard error and 95% confidence interval."),
         DT::DTOutput(nm("means_tbl"))),
    card(card_header(icon("chart-simple"), " Means plot"),
         plotOutput(nm("means_plot"), height = "330px")))
  if (!is.null(r$anova_table))
    cards <- c(cards, list(
      card(card_header(icon("table"), " ANOVA table"),
           uiOutput(nm("fit_line")), DT::DTOutput(nm("anova_tbl")))))
  cards <- c(cards, list(
    card(card_header(icon("ruler-combined"), " Assumption checks"),
         uiOutput(nm("assumptions")))))
  if (!is.null(r$posthoc))
    cards <- c(cards, list(
      card(card_header(icon("layer-group"),
                       sprintf(" Pairwise comparisons (%s)", ph)),
           DT::DTOutput(nm("posthoc")))))
  if (!is.null(r$cld))
    cards <- c(cards, list(
      card(card_header(icon("font"), " Connecting letters"),
           helpText(sprintf(paste("Groups sharing a letter are not significantly",
                                  "different (%s, alpha = 0.05)."), ph)),
           DT::DTOutput(nm("cld_tbl")))))
  cards
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
        selectizeInput(ns("outcome"),
          tagList("Outcome(s) (numeric)", info_tip(
            "The numeric measurement(s) to compare - e.g. mpg. Pick more than ",
            "one to test several outcomes at once.")),
          choices = NULL, multiple = TRUE,
          options = list(maxItems = COMPARE_MAX_OUTCOMES,
                         placeholder = "Choose one or more...")),
        selectizeInput(ns("group"),
          tagList("Group(s) (category)", info_tip(
            "The categorical variable(s) whose levels define the groups - e.g. ",
            "cyl or transmission. Two levels -> t-test; three or more -> ANOVA. ",
            "Pick more than one to test against each grouping in turn.")),
          choices = NULL, multiple = TRUE,
          options = list(maxItems = COMPARE_MAX_GROUPS,
                         placeholder = "Choose one or more...")),
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
            value = FALSE)),
        conditionalPanel(
          sprintf("input['%s'] == 'nonparam'", ns("method")),
          selectInput(ns("posthoc"),
            tagList("All-pairs method", info_tip(
              "For 3+ groups. Dunn's test ranks every observation jointly (the ",
              "same ranking Kruskal-Wallis uses) and adjusts the p-values. ",
              "Steel-Dwass ranks each pair on its own and controls the ",
              "family-wise error rate directly - the rank analogue of Tukey.")),
            choices = c("Dunn's test" = "dunn", "Steel-Dwass" = "steel"),
            selected = "dunn")),
        # Only meaningful once more than one combination is being tested.
        conditionalPanel(
          sprintf(paste("input['%s'] && input['%s'] &&",
                        "input['%s'].length * input['%s'].length > 1"),
                  ns("outcome"), ns("group"), ns("outcome"), ns("group")),
          selectInput(ns("p_adjust"),
            tagList("Correct p across combinations", info_tip(
              "You are running one test per outcome x group combination, so ",
              "some 'significant' results are expected by chance alone. This ",
              "adjusts the p-values across the whole set.")),
            choices = P_ADJUST_CHOICES, selected = "BH"))
      ),

      # ---- categorical mode ----
      conditionalPanel(
        sprintf("input['%s'] == 'cat'", ns("mode")),
        selectInput(ns("cat1"),
          tagList("Variable 1 (category)", info_tip(
            "First categorical variable - its levels become the table rows.")),
          choices = NULL),
        selectInput(ns("cat2"),
          tagList("Variable 2 (category)", info_tip(
            "Second categorical variable - its levels become the table columns.")),
          choices = NULL),
        checkboxGroupInput(ns("pcts"),
          tagList("Show percentages", info_tip(
            "Adds a table per selection. Row % sums to 100 across each row, ",
            "column % down each column, and total % over the whole table.")),
          choices = c("Row %" = "row", "Column %" = "col", "Total %" = "total"),
          selected = character(0))
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
    # them UNSELECTED so nothing runs until the user picks (slow on large data).
    observeEvent(data_in(), {
      df <- data_in(); req(is.data.frame(df))
      updateSelectizeInput(session, "outcome", choices = numeric_cols(df),
                           selected = character(0))
      updateSelectizeInput(session, "group", choices = groupable_cols(df),
                           selected = character(0))
      updateSelectInput(session, "cat1", choices = c("Choose a variable..." = "",
                                                     groupable_cols(df)),
                        selected = "")
      updateSelectInput(session, "cat2", choices = c("Choose a variable..." = "",
                                                     groupable_cols(df)),
                        selected = "")
    }, ignoreNULL = TRUE)

    # The current result: a single comparison ($mode "num"/"cat") or a grid
    # ($mode "num_multi"). Warnings from the rank tests / chi-square on small
    # samples are expected; keep them quiet.
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
        out$mode <- "cat"
        out$pcts <- input$pcts %||% character(0)
        out
      } else {
        outs <- (input$outcome %||% character(0))
        grps <- (input$group   %||% character(0))
        outs <- outs[nzchar(outs)]; grps <- grps[nzchar(grps)]
        validate(need(length(outs) >= 1L && length(grps) >= 1L,
                      "Pick at least one numeric outcome and one grouping variable."))
        n_combo <- length(outs) * length(grps)
        validate(need(n_combo <= COMPARE_MAX_COMBOS, sprintf(
          paste("That is %d combinations (%d outcomes x %d groups). Pick fewer --",
                "the limit is %d."),
          n_combo, length(outs), length(grps), COMPARE_MAX_COMBOS)))
        para <- identical(input$method, "param")
        ph   <- input$posthoc %||% "dunn"
        if (n_combo == 1L) {
          out <- suppressWarnings(compare_groups_numeric(
            df, outs[1], grps[1], parametric = para,
            var_equal = isTRUE(input$var_equal), posthoc = ph))
          validate(need(!is.null(out),
            "Need a numeric outcome and a group with 2+ levels and 2+ rows each."))
          out$mode <- "num"; out
        } else {
          gr <- suppressWarnings(compare_grid(
            df, outs, grps, parametric = para,
            var_equal = isTRUE(input$var_equal), posthoc = ph,
            p_adjust = input$p_adjust %||% "BH"))
          validate(need(!is.null(gr),
            "None of those combinations are testable. Each needs a numeric outcome and a group with 2+ levels and 2+ rows each."))
          list(mode = "num_multi", grid = gr, outcomes = outs, groups = grps,
               p_adjust = input$p_adjust %||% "BH")
        }
      }
    })

    # The i-th numeric combination (NULL when it does not exist). Lets one set of
    # renderers serve both the single view and every accordion panel.
    combo <- function(i) reactive({
      r <- result()
      if (identical(r$mode, "num")) {
        if (i == 1L) r else NULL
      } else if (identical(r$mode, "num_multi")) {
        if (i <= length(r$grid$keys)) r$grid$results[[i]] else NULL
      } else NULL
    })

    # ---- grid summary ---------------------------------------------------------
    output$grid_tbl <- DT::renderDT({
      r <- result(); req(identical(r$mode, "num_multi"))
      DT::datatable(round_df(r$grid$summary), rownames = FALSE,
                    class = "compact stripe hover",
                    options = list(scrollX = TRUE, dom = "t",
                                   pageLength = COMPARE_MAX_COMBOS))
    })

    # ---- numeric renderers, one set per possible combination -----------------
    for (i in seq_len(COMPARE_MAX_COMBOS)) local({
      ii <- i
      ri <- combo(ii)
      nm <- function(x) paste0(x, "_", ii)

      output[[nm("result_card")]] <- renderUI({
        r <- ri(); req(!is.null(r))
        df_txt <- if (!is.na(r$df2)) sprintf("(%g, %g)", r$df, r$df2)
                  else if (!is.na(r$df)) sprintf("(%g)", r$df) else ""
        sym <- if (grepl("ANOVA", r$test)) "F"
               else if (grepl("Kruskal", r$test)) SYM_CHI2
               else if (grepl("Wilcoxon", r$test)) "W" else "t"
        stat_line <- sprintf("%s = %s%s,  %s", sym, round(r$statistic, 3),
                             df_txt, fmt_p(r$p_value))
        rows <- list(
          tags$li(tags$b("Test: "), r$test,
                  sprintf(" (%d groups, N = %d)", r$k, r$n)),
          tags$li(tags$b("Statistic: "), stat_line))
        if (!is.na(r$effect_value))
          rows <- c(rows, list(tags$li(tags$b(paste0(r$effect_name, ": ")),
            round(r$effect_value, 3),
            sprintf(" (%s effect)",
                    effect_magnitude(r$effect_name, r$effect_value)))))
        tags$ul(class = "list-unstyled mb-0", rows)
      })

      output[[nm("interp")]] <- renderUI({
        r <- ri(); req(!is.null(r))
        sig <- !is.na(r$p_value) && r$p_value < 0.05
        eff <- if (!is.na(r$effect_value))
          tags$p("Effect size (", r$effect_name, "): ",
                 tags$b(round(r$effect_value, 3)),
                 sprintf(" - a %s effect.",
                         effect_magnitude(r$effect_name, r$effect_value)))
        tagList(
          tags$p(tags$span(
            style = sprintf("color:%s; font-weight:600;",
                            if (sig) "#2e7d32" else "#c62828"),
            icon(if (sig) "circle-check" else "circle-xmark"),
            if (sig)
              sprintf(" There is a statistically significant difference in %s across %s (%s, %s).",
                      r$outcome, r$group, r$test, fmt_p(r$p_value))
            else
              sprintf(" No statistically significant difference in %s across %s (%s, %s).",
                      r$outcome, r$group, r$test, fmt_p(r$p_value)))),
          eff,
          if (sig && !is.null(r$posthoc))
            tags$p(class = "text-muted small",
                   sprintf("See the %s table for which specific groups differ.",
                           r$posthoc_name %||% "post-hoc")),
          tags$p(class = "text-muted small mt-2",
                 paste0(SYM_ALPHA, " = 0.05. Statistical significance does not ",
                        "imply practical importance.")))
      })

      output[[nm("plot")]] <- renderPlot({
        r <- ri(); req(!is.null(r))
        build_full_plot(data_in(), list(type = "boxplot", x = r$group,
                                        y = r$outcome, color = r$group,
                                        palette = "auto", legend_pos = "none"))
      }, bg = "white")

      output[[nm("means_tbl")]] <- DT::renderDT({
        r <- ri(); req(!is.null(r), !is.null(r$group_stats))
        DT::datatable(r$group_stats, rownames = FALSE,
                      class = "compact stripe hover",
                      options = list(scrollX = TRUE, dom = "t", pageLength = 20))
      })

      output[[nm("means_plot")]] <- renderPlot({
        r <- ri(); req(!is.null(r))
        build_means_letters_plot(r$group_stats, r$cld, ylab = r$outcome)
      }, bg = "white")

      output[[nm("fit_line")]] <- renderUI({
        r <- ri(); req(!is.null(r), !is.null(r$fit_stats))
        fs <- r$fit_stats
        lv <- levene_test(data_in()[[r$outcome]], data_in()[[r$group]])
        unequal <- !is.null(lv) && !lv$equal
        welch_line <- if (!is.null(r$welch)) {
          tags$p(style = if (unequal) "color:#c62828; font-weight:600;" else NULL,
            sprintf("Welch's ANOVA (unequal variances): F(%s, %s) = %s, %s",
                    round(r$welch$df1), round(r$welch$df2, 1),
                    round(r$welch$statistic, 3), fmt_p(r$welch$p_value)),
            if (unequal) tags$span(class = "small",
              " (group variances differ per Levene; prefer this test)"))
        }
        tagList(
          tags$p(tags$b("Summary of fit: "),
            sprintf("R-squared = %s, adjusted R-squared = %s, RMSE = %s",
                    round(fs$r_squared, 3), round(fs$adj_r_squared, 3),
                    round(fs$rmse, 4))),
          welch_line)
      })

      output[[nm("anova_tbl")]] <- DT::renderDT({
        r <- ri(); req(!is.null(r), !is.null(r$anova_table))
        DT::datatable(round_df(r$anova_table), rownames = FALSE,
                      class = "compact stripe hover",
                      options = list(scrollX = TRUE, dom = "t"))
      })

      output[[nm("assumptions")]] <- renderUI({
        r <- ri(); req(!is.null(r))
        df <- data_in()
        nt <- normality_table(df, r$outcome, r$group)
        lv <- levene_test(df[[r$outcome]], df[[r$group]])
        non_normal <- sum(!is.na(nt$Normal) & !nt$Normal)
        norm_msg <- if (non_normal > 0)
          tags$span(class = "text-warning", sprintf(
            "%d group(s) deviate from normality (Shapiro-Wilk p < 0.05). For small samples, prefer the non-parametric option.",
            non_normal))
        else tags$span(class = "text-success",
                       "No strong departures from normality detected.")
        var_msg <- if (!is.null(lv) && !lv$equal)
          tags$span(class = "text-warning", sprintf(
            "Group variances differ (Levene p = %s). Welch's t-test (the 2-group default) handles this; for 3+ groups see Welch's ANOVA or the non-parametric option.",
            round(lv$p_value, 4)))
        else tags$span(class = "text-success", "Group variances look comparable.")
        tagList(
          tags$p(tags$b("Normality: "), norm_msg),
          DT::DTOutput(ns(nm("normality_tbl"))),
          tags$p(class = "mt-2", tags$b("Equal variance: "), var_msg))
      })

      output[[nm("normality_tbl")]] <- DT::renderDT({
        r <- ri(); req(!is.null(r))
        DT::datatable(normality_table(data_in(), r$outcome, r$group),
                      rownames = FALSE, class = "compact stripe hover",
                      options = list(dom = "t", pageLength = 20))
      })

      output[[nm("posthoc")]] <- DT::renderDT({
        r <- ri(); req(!is.null(r), !is.null(r$posthoc))
        DT::datatable(round_df(r$posthoc), rownames = FALSE,
                      class = "compact stripe hover",
                      options = list(scrollX = TRUE, pageLength = 10,
                                     dom = "tip"))
      })

      output[[nm("cld_tbl")]] <- DT::renderDT({
        r <- ri(); req(!is.null(r), !is.null(r$cld))
        DT::datatable(r$cld, rownames = FALSE, class = "compact stripe hover",
                      options = list(dom = "t", pageLength = 20))
      })
    })

    # ---- categorical renderers ------------------------------------------------
    output$cat_result_card <- renderUI({
      r <- result(); req(identical(r$mode, "cat"))
      stat_line <- sprintf("%s(%d, N = %d) = %s,  %s", SYM_CHI2,
                           r$df, r$n, round(r$statistic, 3), fmt_p(r$p_value))
      rows <- list(
        tags$li(tags$b("Test: "), "Pearson's chi-square test of independence"),
        tags$li(tags$b("Statistic: "), stat_line),
        tags$li(tags$b(paste0(CRAMERS_V, ": ")), round(r$cramers_v, 3),
                sprintf(" (%s association)",
                        effect_magnitude(CRAMERS_V, r$cramers_v))))
      if (!is.na(r$fisher_p))
        rows <- c(rows, list(tags$li(tags$b("Fisher's exact (fallback): "),
                                     fmt_p(r$fisher_p))))
      tags$ul(class = "list-unstyled mb-0", rows)
    })

    output$cat_interp <- renderUI({
      r <- result(); req(identical(r$mode, "cat"))
      sig <- !is.na(r$p_value) && r$p_value < 0.05
      tagList(
        tags$p(tags$span(
          style = sprintf("color:%s; font-weight:600;",
                          if (sig) "#2e7d32" else "#c62828"),
          icon(if (sig) "circle-check" else "circle-xmark"),
          if (sig)
            sprintf(" %s and %s are significantly associated (%s).",
                    r$var1, r$var2, fmt_p(r$p_value))
          else
            sprintf(" No significant association between %s and %s (%s).",
                    r$var1, r$var2, fmt_p(r$p_value)))),
        tags$p("Strength of association (", CRAMERS_V, "): ",
               tags$b(round(r$cramers_v, 3)),
               sprintf(" - %s.", effect_magnitude(CRAMERS_V, r$cramers_v))),
        if (r$low_expected > 0)
          tags$p(class = "text-warning small",
                 icon("triangle-exclamation"),
                 sprintf(" %d%% of cells have an expected count below 5, so the chi-square result is approximate - rely on the Fisher's exact p-value (%s) instead.",
                         round(100 * r$low_expected), fmt_p(r$fisher_p))),
        tags$p(class = "text-muted small mt-2",
               paste0(SYM_ALPHA, " = 0.05. Statistical significance does not ",
                      "imply practical importance.")))
    })

    output$cat_plot <- renderPlot({
      r <- result(); req(identical(r$mode, "cat"))
      build_full_plot(data_in(), list(type = "bar", x = r$var1, color = r$var2,
                                      palette = "auto", legend_pos = "right"))
    }, bg = "white")

    # A count/percentage matrix rendered with its row names promoted to a column.
    mat_dt <- function(m) {
      d <- as.data.frame.matrix(m)
      d <- cbind(` ` = rownames(d), d)
      DT::datatable(d, rownames = FALSE, class = "compact stripe hover",
                    options = list(scrollX = TRUE, dom = "t"))
    }

    output$contingency   <- DT::renderDT({
      r <- result(); req(identical(r$mode, "cat")); mat_dt(r$table) })
    output$expected_tbl  <- DT::renderDT({
      r <- result(); req(identical(r$mode, "cat")); mat_dt(r$expected) })
    output$residuals_tbl <- DT::renderDT({
      r <- result(); req(identical(r$mode, "cat")); mat_dt(r$stdres) })
    output$rowpct_tbl    <- DT::renderDT({
      r <- result(); req(identical(r$mode, "cat")); mat_dt(r$row_pct) })
    output$colpct_tbl    <- DT::renderDT({
      r <- result(); req(identical(r$mode, "cat")); mat_dt(r$col_pct) })
    output$totalpct_tbl  <- DT::renderDT({
      r <- result(); req(identical(r$mode, "cat")); mat_dt(r$total_pct) })

    # ---- assembled results layout --------------------------------------------
    output$results_ui <- renderUI({
      r <- tryCatch(result(), error = function(e) NULL)
      # Not ready yet: one shell whose output surfaces the validate() message.
      if (is.null(r))
        return(card(card_header(icon("flask-vial"), " Result"),
                    uiOutput(ns("result_card_1"))))

      if (identical(r$mode, "cat")) {
        cards <- list(
          card(card_header(icon("flask-vial"), " Result"),
               uiOutput(ns("cat_result_card"))),
          card(card_header(icon("lightbulb"), " Interpretation"),
               uiOutput(ns("cat_interp"))),
          card(card_header(icon("chart-column"), " Chart"),
               plotOutput(ns("cat_plot"), height = "330px")),
          card(card_header(icon("table-cells"), " Contingency table"),
               DT::DTOutput(ns("contingency"))),
          card(card_header(icon("border-all"), " Expected counts"),
               DT::DTOutput(ns("expected_tbl"))),
          card(card_header(icon("magnifying-glass-chart"),
                           " Standardized residuals"),
               helpText("Standardized (Pearson) residuals; cells with |z| > 2 ",
                        "stand out as the drivers of the association."),
               DT::DTOutput(ns("residuals_tbl"))))
        pcts <- r$pcts %||% character(0)
        if ("row" %in% pcts)
          cards <- c(cards, list(card(card_header(icon("percent"), " Row %"),
            helpText("Each row sums to 100%."), DT::DTOutput(ns("rowpct_tbl")))))
        if ("col" %in% pcts)
          cards <- c(cards, list(card(card_header(icon("percent"), " Column %"),
            helpText("Each column sums to 100%."),
            DT::DTOutput(ns("colpct_tbl")))))
        if ("total" %in% pcts)
          cards <- c(cards, list(card(card_header(icon("percent"), " Total %"),
            helpText("The whole table sums to 100%."),
            DT::DTOutput(ns("totalpct_tbl")))))
        return(do.call(layout_columns, c(list(col_widths = 6), cards)))
      }

      if (identical(r$mode, "num"))
        return(do.call(layout_columns,
                       c(list(col_widths = 6), compare_detail_cards(ns, 1, r))))

      # --- a grid: summary on top, one collapsible section per combination ---
      adj_lab <- names(P_ADJUST_CHOICES)[
        match(r$p_adjust, P_ADJUST_CHOICES)]
      smry <- card(
        card_header(icon("table-list"), " All combinations"),
        helpText(sprintf(paste("One test per outcome x group. p_adj corrects",
                               "across all %d combinations (%s)."),
                         nrow(r$grid$summary), adj_lab %||% r$p_adjust)),
        DT::DTOutput(ns("grid_tbl")))
      panels <- lapply(seq_along(r$grid$keys), function(i) {
        ri <- r$grid$results[[i]]
        accordion_panel(
          title = sprintf("%s  (%s)", r$grid$keys[i], fmt_p(ri$p_value)),
          value = paste0("combo_", i),
          do.call(layout_columns,
                  c(list(col_widths = 6), compare_detail_cards(ns, i, ri))))
      })
      # All panels start closed so every combination is visible at a glance and
      # the user opens the ones they want (closed panels also stay cheap --
      # Shiny suspends the outputs inside them until they are expanded).
      tagList(smry,
              do.call(accordion,
                      c(panels, list(open = FALSE, multiple = TRUE))))
    })

    # ---- text export ----------------------------------------------------------
    output$download <- downloadHandler(
      filename = function() paste0("group_comparison_", Sys.Date(), ".txt"),
      content  = function(f) {
        r <- tryCatch(result(), error = function(e) NULL)
        validate(need(!is.null(r), "Nothing to export yet."))
        con <- file(f, "w"); on.exit(close(con))
        wl <- function(...) writeLines(paste0(...), con)
        tbl <- function(x, rn = FALSE) utils::write.table(
          x, con, quote = FALSE, sep = "\t",
          row.names = FALSE, col.names = if (rn) NA else TRUE)
        mat <- function(m) utils::write.table(as.data.frame.matrix(m), con,
                                              quote = FALSE, sep = "\t",
                                              col.names = NA)
        # One numeric combination, written in full.
        num_block <- function(r) {
          wl("Outcome: ", r$outcome, "   Groups: ", r$group,
             " (", r$k, " levels, N = ", r$n, ")")
          wl("Test: ", r$test)
          wl("Statistic = ", round(r$statistic, 4), ", ", fmt_p(r$p_value))
          if (!is.na(r$effect_value))
            wl(r$effect_name, " = ", round(r$effect_value, 4),
               " (", effect_magnitude(r$effect_name, r$effect_value), ")")
          if (!is.null(r$welch))
            wl("Welch's ANOVA (unequal variances): F(", round(r$welch$df1), ", ",
               round(r$welch$df2, 1), ") = ", round(r$welch$statistic, 4), ", ",
               fmt_p(r$welch$p_value))
          if (!is.null(r$fit_stats))
            wl("Summary of fit: R-squared = ", round(r$fit_stats$r_squared, 4),
               ", adjusted R-squared = ", round(r$fit_stats$adj_r_squared, 4),
               ", RMSE = ", round(r$fit_stats$rmse, 4))
          wl(""); wl("Group means (N, Mean, SD, Median, SE, 95% CI):")
          tbl(r$group_stats)
          if (!is.null(r$anova_table)) {
            wl(""); wl("ANOVA table:"); tbl(r$anova_table)
          }
          if (!is.null(r$posthoc)) {
            wl(""); wl(r$posthoc_name %||% "Post-hoc", " pairwise comparisons:")
            tbl(r$posthoc)
          }
          if (!is.null(r$cld)) {
            wl(""); wl("Connecting letters (shared letter = not significantly different):")
            tbl(r$cld)
          }
        }

        wl("UF/IFAS Data Explorer ", SYM_TIMES, " Compare Groups")
        wl("Generated: ", as.character(Sys.time())); wl("")
        if (identical(r$mode, "cat")) {
          wl("Mode: association between two categorical variables")
          wl("Variables: ", r$var1, " ", SYM_TIMES, " ", r$var2)
          wl("Chi-square = ", round(r$statistic, 4), ", df = ", r$df,
             ", ", fmt_p(r$p_value))
          wl(CRAMERS_V, " = ", round(r$cramers_v, 4),
             " (", effect_magnitude(CRAMERS_V, r$cramers_v), ")")
          if (!is.na(r$fisher_p))
            wl("Fisher's exact (fallback): ", fmt_p(r$fisher_p))
          wl(""); wl("Contingency table:");                  mat(r$table)
          wl(""); wl("Expected counts:");                    mat(r$expected)
          wl(""); wl("Standardized residuals (|z| > 2 flags a driver cell):")
          mat(r$stdres)
          pcts <- r$pcts %||% character(0)
          if ("row" %in% pcts)   { wl(""); wl("Row % (each row sums to 100):");        mat(r$row_pct) }
          if ("col" %in% pcts)   { wl(""); wl("Column % (each column sums to 100):");  mat(r$col_pct) }
          if ("total" %in% pcts) { wl(""); wl("Total % (whole table sums to 100):");   mat(r$total_pct) }
        } else if (identical(r$mode, "num")) {
          wl("Mode: numeric outcome by group"); wl("")
          num_block(r)
        } else {
          wl("Mode: numeric outcomes by groups (grid of ",
             nrow(r$grid$summary), " combinations)")
          wl("p_adj method across combinations: ", r$p_adjust)
          wl(""); wl("All combinations:")
          tbl(r$grid$summary)
          for (i in seq_along(r$grid$keys)) {
            wl(""); wl("--------------------------------------------------")
            wl(r$grid$keys[i]); wl("")
            num_block(r$grid$results[[i]])
          }
        }
      }
    )

    result
  })
}
