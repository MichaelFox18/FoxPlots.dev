# ============================================================
# mod_lmer.R -- the Mixed Model Review stage
# ============================================================
# A drop-down mixed-model (lmerTest / emmeans) review tool, ported from a
# single-file app into the kit's module pattern. Data arrives via `data_in`
# (mod_import owns upload + Data Health + type recast + preview), so this module
# keeps only the model-spec sidebar, the one feature mod_import lacks (a combined
# / interaction variable builder), and the analysis result tabs. All statistical
# work lives in pure helpers (helpers_lmer.R); this module is the thin wrapper.
#
# lmerServer(id, data_in) returns the augmented dataset reactive (base import +
# any user-created combined variables) so an Export stage can download it.

# A collapsible, copy-able code block tied to a renderText output. The output id
# must be namespaced so DEcopy()'s getElementById finds the right element.
lmer_code_panel <- function(ns, out_id, label = "R code") {
  tags$details(class = "uf-codewrap",
    tags$summary(tags$b(sprintf("> %s (copy & run in R)", label))),
    tags$button("Copy code", class = "btn btn-outline-secondary btn-sm uf-copy",
                onclick = sprintf("DEcopy('%s', this)", ns(out_id))),
    verbatimTextOutput(ns(out_id))
  )
}

lmerUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 340,
      tags$details(open = NA,
        tags$summary(tags$b("Create combined variable (interaction)")),
        helpText("Paste 2+ columns into one new factor (e.g. Diet x Time), so an ",
                 "interaction can be used as a single grouping in the model and ",
                 "EMMeans. Works for categorical or discrete-numeric columns. ",
                 "Added to the dataset and included when you export."),
        uiOutput(ns("combo_vars_ui")),
        textInput(ns("combo_sep"), "Separator", value = "."),
        actionButton(ns("combo_add"), "Create variable", class = "btn-primary"),
        uiOutput(ns("combo_list_ui"))
      ),
      tags$hr(),
      uiOutput(ns("response_ui")),
      selectInput(ns("transform"),
                  tagList("Response transformation", info_tip(
                    "Transform the response before fitting (e.g. log for ",
                    "right-skewed or multiplicative data). EMMeans can be ",
                    "back-transformed to the original scale below.")),
                  choices = c("None"                       = "none",
                              "log()"                       = "log",
                              "log1p()  [log(1+y)]"         = "log1p",
                              "sqrt()"                      = "sqrt",
                              "inverse (1/y)"               = "inverse",
                              "arcsine-sqrt (proportions)"  = "asin"),
                  selected = "none"),
      uiOutput(ns("fixed_ui")),
      checkboxInput(ns("interactions"),
                    "Include interactions among fixed effects", FALSE),
      uiOutput(ns("random_ui")),
      tags$details(
        tags$summary(tags$b("Advanced model options")),
        uiOutput(ns("slope_ui")),
        uiOutput(ns("slope_group_ui")),
        radioButtons(ns("estimator"), "Estimator",
                     choices = c("REML (recommended)" = "reml", "ML" = "ml"),
                     selected = "reml", inline = TRUE),
        selectInput(ns("anova_type"), "ANOVA type (sum of squares)",
                    choices = c("Type II" = "2", "Type III" = "3"),
                    selected = "3"),
        selectInput(ns("ddf"), "ANOVA denominator df",
                    choices = c("Kenward-Roger" = "Kenward-Roger",
                                "Satterthwaite" = "Satterthwaite"),
                    selected = default_ddf()),
        helpText("Type III main-effect tests depend on the contrast coding when ",
                 "interactions are present; this app applies sum-to-zero ",
                 "contrasts so they are interpretable. Kenward-Roger df requires ",
                 "REML and is auto-switched to Satterthwaite under ML.")
      ),
      tags$hr(),
      uiOutput(ns("emm_ui")),
      uiOutput(ns("emm_by_ui")),
      checkboxInput(ns("backtransform"),
                    "Back-transform EMMeans to response scale", TRUE),
      selectInput(ns("adjust"),
                  tagList("Post-hoc p-value adjustment", info_tip(
                    "How pairwise p-values are corrected for multiple ",
                    "comparisons. Tukey is the usual choice for all-pairs ",
                    "comparisons of means.")),
                  choices = c("Tukey" = "tukey", "Sidak" = "sidak",
                              "Bonferroni" = "bonferroni", "Holm" = "holm",
                              "None" = "none"),
                  selected = "tukey"),
      sliderInput(ns("conf"),
                  tagList("Confidence level", info_tip(
                    "Confidence level for the EMMeans intervals and the ",
                    "pairwise comparisons.")),
                  min = 0.80, max = 0.99, value = 0.95, step = 0.01),
      tags$hr(),
      actionButton(ns("run"), "Run analysis", class = "btn-primary"),
      actionButton(ns("reset"), "Reset", class = "btn-outline-secondary"),
      actionButton(ns("save_model"), "Save model for comparison",
                   class = "btn-outline-primary", style = "margin-top:8px;")
    ),

    tags$head(
      tags$script(HTML(copy_js)),
      tags$style(HTML(paste0(
        ".uf-dl { margin: 6px 6px 12px 0; } ",
        ".uf-copy { margin: 4px 0 0 0; font-size: 12px; padding: 3px 10px; } ",
        ".uf-codewrap { margin: 6px 0 14px 0; } ",
        "summary { cursor: pointer; user-select: none; padding: 2px 0; }")))
    ),
    uiOutput(ns("message_box")),

    navset_card_tab(
      id = ns("tabs"),

      nav_panel("Explore (raw data)",
               h4("Design balance (cell counts)"),
               helpText("Counts per combination of the selected categorical fixed ",
                        "effects. Empty cells (shaded) are what break EMMeans / cld ",
                        "-- check for them before fitting."),
               DT::DTOutput(ns("balance_table")),
               tags$hr(),
               h4("Response vs fixed effect(s)"),
               helpText("Raw, pre-model view. Look for outliers and for a spread ",
                        "that grows with the mean (a hint to try a log/sqrt ",
                        "transform)."),
               plotOutput(ns("explore_plot"), height = "460px"),
               lmer_code_panel(ns, "code_explore", "Descriptive plot code"),
               h4("Interaction plot (raw means)"),
               helpText("Non-parallel lines suggest an interaction -- a reason to ",
                        "tick 'Include interactions'. Needs >= 2 fixed effects."),
               plotOutput(ns("interaction_plot"), height = "440px"),
               lmer_code_panel(ns, "code_interaction", "Interaction plot code")),

      nav_panel("Model & code",
               h4("R code for this model"),
               verbatimTextOutput(ns("code_block")),
               tags$button("Copy code", class = "btn btn-outline-secondary btn-sm uf-copy",
                           onclick = sprintf("DEcopy('%s', this)", ns("code_block"))),
               downloadButton(ns("dl_code"), "Download .R", class = "btn-outline-secondary uf-dl"),
               h4("Model summary"),
               verbatimTextOutput(ns("model_summary"))),

      nav_panel("Fit & variance",
               h4("Fit statistics"),
               DT::DTOutput(ns("fit_table")),
               htmlOutput(ns("fit_help")),
               h4("Variance components"),
               DT::DTOutput(ns("vc_table")),
               h4("Random-effects estimates (caterpillar plot)"),
               plotOutput(ns("ranef_plot"), height = "420px"),
               h4("Random-effects normality (Q-Q of BLUPs)"),
               helpText("Mixed models assume the random effects are normally ",
                        "distributed. Points should hug the line."),
               plotOutput(ns("ranef_qq"), height = "420px"),
               lmer_code_panel(ns, "code_fit", "Fit, variance & random-effects code")),

      nav_panel("ANOVA",
               h4(textOutput(ns("anova_title"), inline = TRUE)),
               verbatimTextOutput(ns("anova_table")),
               lmer_code_panel(ns, "code_anova", "ANOVA code")),

      nav_panel("Residuals",
               h4("Diagnostic plots"),
               plotOutput(ns("resid_plot"), height = "560px"),
               downloadButton(ns("dl_resid"), "Download PNG", class = "btn-outline-secondary uf-dl"),
               htmlOutput(ns("resid_help")),
               lmer_code_panel(ns, "code_resid", "Residual diagnostics code"),
               tags$hr(),
               h4("Influence by group (Cook's distance)"),
               uiOutput(ns("infl_ui")),
               plotOutput(ns("cook_plot"), height = "380px"),
               htmlOutput(ns("cook_help")),
               lmer_code_panel(ns, "code_cook", "Cook's distance code")),

      nav_panel("EMMeans & post-hoc",
               uiOutput(ns("emm_note")),
               h4("Estimated marginal means"),
               DT::DTOutput(ns("emm_table")),
               downloadButton(ns("dl_emm"), "Download CSV", class = "btn-outline-secondary uf-dl"),
               h4("Pairwise comparisons"),
               DT::DTOutput(ns("pairs_table")),
               downloadButton(ns("dl_pairs"), "Download CSV", class = "btn-outline-secondary uf-dl"),
               h4("Compact letter display (cld)"),
               DT::DTOutput(ns("cld_table")),
               downloadButton(ns("dl_cld"), "Download CSV", class = "btn-outline-secondary uf-dl"),
               helpText("Means sharing a letter are not significantly different at ",
                        "the chosen adjustment level. With a 'compare within' factor ",
                        "set, letters are computed separately within each level of ",
                        "that factor (simple effects). Comparisons are always tested ",
                        "on the model (link) scale; when back-transformed, the ",
                        "displayed means are estimates on the response scale (e.g. a ",
                        "geometric mean for log), not arithmetic means, and log-scale ",
                        "differences are shown as ratios."),
               lmer_code_panel(ns, "code_emm", "EMMeans + post-hoc + cld code")),

      nav_panel("Interaction test",
               helpText("Letters tell you which cells differ; they do not tell you ",
                        "whether an interaction is real. The omnibus F-tests below ",
                        "test each model term (including any interaction) directly. ",
                        "Workflow: confirm the interaction here, then use the ",
                        "'compare within' option on the EMMeans tab for the ",
                        "simple-effects letters."),
               h4("Joint (omnibus) F-tests"),
               verbatimTextOutput(ns("joint_table")),
               tags$hr(),
               h4("Interaction contrasts (differences of differences)"),
               helpText("Shown when two or more EMMeans factors are selected and no ",
                        "'compare within' factor is set."),
               verbatimTextOutput(ns("inter_contrast_table")),
               lmer_code_panel(ns, "code_joint", "Interaction test code")),

      nav_panel("EMMeans plot",
               h4("Estimated means with letter groupings"),
               plotOutput(ns("emm_plot"), height = "520px"),
               downloadButton(ns("dl_emmplot"), "Download PNG", class = "btn-outline-secondary uf-dl"),
               lmer_code_panel(ns, "code_emmplot", "EMMeans plot code")),

      nav_panel("Compare models",
               helpText("Use 'Save model for comparison' on a fitted model, then ",
                        "change the spec, Run again, and compare here."),
               htmlOutput(ns("compare_status")),
               h4("Likelihood-ratio test (saved vs current)"),
               verbatimTextOutput(ns("compare_lrt")),
               lmer_code_panel(ns, "code_compare", "Model comparison code"))
    )
  )
}

lmerServer <- function(id, data_in) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      fit = NULL, modelA = NULL, message = NULL, code = NULL,
      cook = NULL, cook_grp = NULL, cook_err = NULL,
      combos = list(), reset = 0
    )

    # -- Data -----------------------------------------------------------------
    # Base frame from import. mod_import already recasts types; we add a
    # defensive character->factor and ordered->plain factor pass so emmeans
    # never gets polynomial contrasts from an ordered factor.
    base_data <- reactive({
      df <- data_in(); req(is.data.frame(df))
      df[] <- lapply(df, function(x) {
        if (is.character(x)) factor(x)
        else if (is.ordered(x)) factor(x, ordered = FALSE)
        else x
      })
      df
    })

    # Working frame = base + any user-created combined (interaction) variables.
    dataset <- reactive({
      df <- base_data()
      for (cb in rv$combos)
        if (all(cb$vars %in% names(df)))
          df[[cb$name]] <- make_combined_factor(df, cb$vars, cb$sep)
      df
    })

    # New data invalidates a stale fit.
    observeEvent(data_in(), {
      rv$fit <- NULL; rv$code <- NULL; rv$cook <- NULL; rv$cook_err <- NULL
    }, ignoreNULL = FALSE)

    # -- Combined-variable (interaction) builder ------------------------------
    output$combo_vars_ui <- renderUI({
      df <- base_data(); req(df); rv$reset
      selectInput(ns("combo_vars"), "Columns to combine (pick 2 or more)",
                  choices = names(df), multiple = TRUE)
    })

    observeEvent(input$combo_add, {
      vars <- input$combo_vars
      if (length(vars) < 2) {
        rv$message <- list(type = "warning",
                           text = "Pick at least two columns to combine into a new variable.")
        return()
      }
      sep  <- if (is.null(input$combo_sep) || !nzchar(input$combo_sep)) "." else input$combo_sep
      name <- paste(vars, collapse = sep)
      df    <- base_data()
      n_lev <- nlevels(make_combined_factor(df, vars, sep))
      rv$combos <- c(rv$combos[vapply(rv$combos, function(c) c$name != name, logical(1))],
                     list(list(name = name, vars = vars, sep = sep)))
      rv$message <- list(type = "info", text = sprintf(
        "Created combined variable '%s' from %s (%d level%s). It is now available as a fixed/random effect and for EMMeans, and is included when you export the data.",
        name, paste(vars, collapse = " + "), n_lev, if (n_lev == 1) "" else "s"))
    })

    observeEvent(input$combo_clear, {
      rv$combos <- list()
      rv$message <- list(type = "info", text = "Removed all created combined variables.")
    })

    output$combo_list_ui <- renderUI({
      if (!length(rv$combos)) return(helpText("No combined variables yet."))
      nm <- vapply(rv$combos, function(c) c$name, character(1))
      tagList(
        tags$div(tags$b("Created: "), paste(nm, collapse = ", ")),
        actionButton(ns("combo_clear"), "Clear created variables",
                     class = "btn-outline-secondary btn-sm")
      )
    })

    # -- Dynamic variable selectors -------------------------------------------
    output$response_ui <- renderUI({
      df <- dataset(); req(df); rv$reset
      num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      validate(need(length(num_cols) > 0,
                    "No numeric columns available for a response variable."))
      selectInput(ns("response"), "Response variable (numeric)", choices = num_cols)
    })

    output$fixed_ui <- renderUI({
      df <- dataset(); req(df); rv$reset
      selectizeInput(ns("fixed"),
                     sprintf("Fixed effects  (choose up to %d)", MAX_FIXED),
                     choices = names(df), multiple = TRUE,
                     options = list(maxItems = MAX_FIXED,
                                    placeholder = "select 1-3 variables"))
    })

    output$random_ui <- renderUI({
      df <- dataset(); req(df); rv$reset
      selectInput(ns("random"), "Random effects  (grouping factors)",
                  choices = names(df), multiple = TRUE)
    })

    output$slope_ui <- renderUI({
      df <- dataset(); req(df)
      fx <- input$fixed
      selectInput(ns("ranslope"), "Random slope variable (optional)",
                  choices = c("(intercept only)" = "", fx), selected = "")
    })

    output$slope_group_ui <- renderUI({
      rnd <- input$random
      if (is.null(input$ranslope) || !nzchar(input$ranslope) || length(rnd) < 1)
        return(NULL)
      selectInput(ns("ranslope_group"),
                  "Apply random slope to which grouping factor(s)?",
                  choices = rnd, selected = rnd[1], multiple = TRUE)
    })

    output$emm_ui <- renderUI({
      df <- dataset(); req(df)
      fx <- input$fixed
      cat_fixed <- if (length(fx)) fx[is_categorical(df, fx)] else character(0)
      selectInput(ns("emmvars"),
                  "EMMeans over (categorical fixed effects only)",
                  choices = cat_fixed, multiple = TRUE)
    })

    output$emm_by_ui <- renderUI({
      selectInput(ns("emm_by"),
                  "Compare within levels of (simple effects; optional)",
                  choices = c("(none - compare all cells)" = "", input$emmvars),
                  selected = "")
    })

    # -- Reset ----------------------------------------------------------------
    observeEvent(input$reset, {
      rv$reset    <- rv$reset + 1
      rv$fit      <- NULL; rv$message <- NULL; rv$code <- NULL
      rv$cook     <- NULL; rv$cook_err <- NULL; rv$combos <- list()
      updateSelectInput(session, "transform", selected = "none")
      updateSelectInput(session, "adjust",    selected = "tukey")
      updateSelectInput(session, "ddf",       selected = default_ddf())
      updateSelectInput(session, "anova_type", selected = "3")
      updateRadioButtons(session, "estimator", selected = "reml")
      updateCheckboxInput(session, "interactions",  value = FALSE)
      updateCheckboxInput(session, "backtransform", value = TRUE)
      updateSliderInput(session, "conf", value = 0.95)
    })

    # -- Fit on demand --------------------------------------------------------
    observeEvent(input$run, {
      df <- dataset(); req(df)
      spec <- list(
        response     = input$response,
        transform    = input$transform %||% "none",
        fixed        = input$fixed,
        random       = input$random,
        interactions = isTRUE(input$interactions),
        slope        = input$ranslope,
        slope_group  = input$ranslope_group,
        reml         = identical(input$estimator, "reml"),
        anova_type   = input$anova_type %||% "3",
        ddf          = input$ddf %||% default_ddf()
      )

      probs <- lmer_validate(df, spec)
      if (length(probs)) {
        rv$message <- list(type = "warning", text = probs)
        rv$fit <- NULL; rv$code <- NULL; return()
      }
      res <- lmer_fit(df, spec)
      if (!isTRUE(res$ok)) {
        rv$message <- list(type = "error", text = res$error)
        rv$fit <- NULL; rv$code <- NULL; return()
      }
      rv$fit  <- res
      rv$message <- if (length(res$notes)) list(type = "info", text = res$notes) else NULL
      rv$code <- lmer_code(res, input$emmvars, input$emm_by,
                           isTRUE(input$backtransform), input$conf, input$adjust)
      rv$cook <- NULL; rv$cook_err <- NULL
      updateTabsetPanel(session, "tabs", selected = "ANOVA")
    })

    # -- Message box ----------------------------------------------------------
    output$message_box <- renderUI({
      m <- rv$message; if (is.null(m)) return(NULL)
      cls <- switch(m$type, error = "alert alert-danger",
                    info = "alert alert-info", "alert alert-warning")
      lbl <- switch(m$type, error = "Error: ", info = "Note: ", "Heads up: ")
      div(class = cls, tags$b(lbl), tags$ul(lapply(m$text, tags$li)))
    })

    # -- Explore (raw data, pre-model) ----------------------------------------
    output$balance_table <- DT::renderDT({
      df <- dataset(); req(df)
      fx <- input$fixed
      cat_fx <- if (length(fx)) fx[is_categorical(df, fx)] else character(0)
      validate(need(length(cat_fx) >= 1,
                    "Select one or more categorical fixed effects to see the design balance."))
      tab <- as.data.frame(table(df[cat_fx]), responseName = "n")
      names(tab)[seq_along(cat_fx)] <- cat_fx
      dt <- DT::datatable(tab, rownames = FALSE, class = "compact stripe hover",
                          options = list(dom = "tp", pageLength = 16))
      DT::formatStyle(dt, "n",
                      backgroundColor = DT::styleEqual(0, "#ffd6cc"),
                      fontWeight      = DT::styleEqual(0, "bold"))
    })

    output$explore_plot <- renderPlot({
      df <- dataset(); req(df)
      resp <- input$response; fx <- input$fixed
      validate(need(!is.null(resp), "Select a response variable."),
               need(length(fx) >= 1, "Select at least one fixed effect to explore."))
      x <- fx[1]
      cat_x <- is.factor(df[[x]]) || is.character(df[[x]])

      p <- ggplot(df, aes(x = .data[[x]], y = .data[[resp]]))
      if (cat_x) {
        p <- p +
          geom_boxplot(outlier.shape = NA, fill = UF_BLUE_LT, colour = UF_BLUE) +
          geom_jitter(width = 0.15, height = 0, alpha = 0.45, colour = UF_ORANGE)
      } else {
        p <- p + geom_point(alpha = 0.55, colour = UF_BLUE) +
          geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                      colour = UF_ORANGE)
      }
      facet_vars <- character(0)
      if (length(fx) >= 2 && (is.factor(df[[fx[2]]]) || is.character(df[[fx[2]]])))
        facet_vars <- c(facet_vars, fx[2])
      if (length(fx) >= 3 && (is.factor(df[[fx[3]]]) || is.character(df[[fx[3]]])))
        facet_vars <- c(facet_vars, fx[3])
      if (length(facet_vars) == 2)
        p <- p + facet_grid(stats::reformulate(facet_vars[2], facet_vars[1]))
      else if (length(facet_vars) == 1)
        p <- p + facet_wrap(stats::reformulate(facet_vars))
      p + labs(x = x, y = resp, title = sprintf("%s by %s", resp, x)) +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(face = "bold", colour = UF_BLUE))
    })

    output$interaction_plot <- renderPlot({
      df <- dataset(); req(df)
      resp <- input$response; fx <- input$fixed
      validate(need(!is.null(resp), "Select a response variable."),
               need(length(fx) >= 2,
                    "Select at least two fixed effects to see an interaction plot."))
      x <- fx[1]; trace <- fx[2]
      facet3 <- if (length(fx) >= 3) fx[3] else NULL

      cols <- c(resp, x, trace, facet3)
      cc   <- stats::complete.cases(df[cols])
      n_drop <- sum(!cc)
      df <- df[cc, , drop = FALSE]
      validate(need(nrow(df) > 0, "No complete rows for the selected variables."))

      grp_list <- list(X = factor(df[[x]]), G = factor(df[[trace]]))
      if (!is.null(facet3)) grp_list$F3 <- factor(df[[facet3]])
      ag <- stats::aggregate(df[[resp]], by = grp_list,
                             FUN = function(z) mean(z, na.rm = TRUE))
      names(ag)[names(ag) == "x"] <- "mean"
      n_lev <- nlevels(ag$G)
      uf_pal <- rep(UF_COLORS, length.out = max(n_lev, 1))
      subt <- if (n_drop > 0)
        sprintf("%d row(s) with missing values dropped", n_drop) else NULL
      p <- ggplot(ag, aes(x = .data[["X"]], y = .data[["mean"]],
                          colour = .data[["G"]], group = .data[["G"]])) +
        geom_line(linewidth = 1) + geom_point(size = 2.6) +
        scale_colour_manual(values = uf_pal)
      if (!is.null(facet3)) p <- p + facet_wrap("F3")
      p + labs(x = x, colour = trace, y = sprintf("mean %s", resp),
               subtitle = subt,
               title = sprintf("%s: %s x %s (raw means)", resp, x, trace)) +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(face = "bold", colour = UF_BLUE),
              legend.title = element_text(colour = UF_CHARCOAL))
    })

    # -- Per-section reproducible code ----------------------------------------
    HDR <- paste0("# Assumes: `dat` is your data frame, and `mod` is the fitted ",
                  "model\n# from the 'Model & code' tab (run that block first).\n")

    emm_spec_str <- function() {
      ev <- input$emmvars
      if (length(ev)) emm_spec_text(ev, input$emm_by) else "~ <factor>"
    }
    emm_call_str <- function() {
      bt <- rv$fit$transform != "none" && isTRUE(input$backtransform)
      tr <- tran_for_emmeans(rv$fit$transform)
      if (bt && !is.null(tr))
        sprintf('emm <- emmeans(ref_grid(mod, tran = "%s"), %s,\n               type = "response", level = %s)',
                tr, emm_spec_str(), format(input$conf))
      else
        sprintf('emm <- emmeans(mod, %s, type = "%s", level = %s)',
                emm_spec_str(), if (bt) "response" else "link", format(input$conf))
    }

    output$code_explore <- renderText({
      resp <- input$response; fx <- input$fixed
      if (is.null(resp) || length(fx) < 1)
        return("# Pick a response variable and at least one fixed effect.")
      df <- dataset(); x <- fx[1]
      cat_x <- is.factor(df[[x]]) || is.character(df[[x]])
      geom <- if (cat_x)
        "  geom_boxplot(outlier.shape = NA) +\n  geom_jitter(width = 0.15, height = 0, alpha = 0.45)"
      else
        "  geom_point(alpha = 0.55) +\n  geom_smooth(method = \"loess\", formula = y ~ x, se = FALSE)"
      facets <- fx[-1][vapply(fx[-1],
                              function(v) is.factor(df[[v]]) || is.character(df[[v]]), logical(1))]
      fl <- if (length(facets) >= 2)
        sprintf(" +\n  facet_grid(%s ~ %s)", bq(facets[1]), bq(facets[2]))
      else if (length(facets) == 1)
        sprintf(" +\n  facet_wrap(~ %s)", bq(facets[1]))
      else ""
      paste0("library(ggplot2)\n",
             sprintf("ggplot(dat, aes(x = %s, y = %s)) +\n", bq(x), bq(resp)),
             geom, " +\n  theme_minimal()", fl)
    })

    output$code_interaction <- renderText({
      resp <- input$response; fx <- input$fixed
      if (is.null(resp) || length(fx) < 2)
        return("# Select at least two fixed effects for an interaction plot.")
      x <- fx[1]; tr <- fx[2]; f3 <- if (length(fx) >= 3) fx[3] else NULL
      cols <- paste(c(bq(resp), bq(x), bq(tr), if (!is.null(f3)) bq(f3)),
                    collapse = ", ")
      grp  <- paste(c(sprintf("X = %s", bq(x)), sprintf("G = %s", bq(tr)),
                      if (!is.null(f3)) sprintf("F3 = %s", bq(f3))), collapse = ", ")
      facet <- if (!is.null(f3)) " +\n  facet_wrap(~ F3)" else ""
      paste0("library(ggplot2)\n",
             sprintf("d  <- dat[complete.cases(dat[, c(%s)]), ]\n", gsub("`", "\"", cols)),
             sprintf("ag <- aggregate(d[[%s]], by = list(%s),\n", paste0("\"", resp, "\""), grp),
             "                FUN = function(z) mean(z, na.rm = TRUE))\n",
             "names(ag)[ncol(ag)] <- \"mean\"\n",
             "ggplot(ag, aes(X, mean, colour = G, group = G)) +\n",
             "  geom_line(linewidth = 1) + geom_point(size = 2.6)", facet,
             " +\n  labs(x = \"", x, "\", colour = \"", tr, "\", y = \"mean ", resp, "\") +\n",
             "  theme_minimal()")
    })

    output$code_fit <- renderText({
      if (is.null(rv$fit)) return("# Run a model first.")
      g1 <- rv$fit$random[1]
      paste0(HDR, "\n",
             "# Fit statistics\n",
             "AIC(mod); BIC(mod); logLik(mod)\n",
             "if (lme4::isREML(mod)) lme4::REMLcrit(mod) else deviance(mod)  # REML-safe\n",
             "performance::r2(mod)    # marginal & conditional R^2\n",
             "performance::icc(mod)   # intraclass correlation\n\n",
             "# Variance components\n",
             "as.data.frame(VarCorr(mod))\n\n",
             "# Random-effects estimates (caterpillar plot)\n",
             "lattice::dotplot(ranef(mod, condVar = TRUE))\n\n",
             "# Random-effects normality (Q-Q of BLUPs)\n",
             sprintf("re <- ranef(mod)[[\"%s\"]][[1]]\n", g1),
             "qqnorm(re); qqline(re)")
    })

    output$code_anova <- renderText({
      if (is.null(rv$fit)) return("# Run a model first.")
      paste0(HDR, "\n", sprintf('anova(mod, type = "%s", ddf = "%s")',
                                if (rv$fit$atype == "3") "III" else "II", rv$fit$ddf))
    })

    output$code_resid <- renderText({
      if (is.null(rv$fit)) return("# Run a model first.")
      paste0(HDR, "\n",
             "r <- resid(mod); f <- fitted(mod)\n",
             "op <- par(mfrow = c(2, 2))\n",
             "plot(f, r, main = \"Residuals vs Fitted\"); abline(h = 0, lty = 2)\n",
             "lines(lowess(f, r), col = 2, lwd = 2)\n",
             "qqnorm(r); qqline(r, col = 2, lwd = 2)\n",
             "plot(f, sqrt(abs(scale(r))), main = \"Scale-Location\")\n",
             "lines(lowess(f, sqrt(abs(scale(r)))), col = 2, lwd = 2)\n",
             "hist(r, breaks = \"FD\", main = \"Histogram of residuals\")\n",
             "par(op)")
    })

    output$code_cook <- renderText({
      if (is.null(rv$fit)) return("# Run a model first.")
      g <- (input$infl_group %||% rv$fit$random[1])
      paste0(HDR, "\n",
             "library(influence.ME)\n",
             sprintf('infl <- influence(mod, group = "%s")  # leave-one-group-out (slow)\n', g),
             "cooks.distance(infl)\n",
             "plot(infl, which = \"cook\", sort = TRUE,\n",
             "     cutoff = 4 / length(cooks.distance(infl)))")
    })

    output$code_emm <- renderText({
      if (is.null(rv$fit)) return("# Run a model first.")
      if (length(input$emmvars) < 1)
        return("# Choose one or more categorical fixed effects for EMMeans.")
      paste0(HDR, "\nlibrary(emmeans); library(multcomp)\n\n",
             emm_call_str(), "\n",
             "summary(emm)\n",
             sprintf('pairs(emm, adjust = "%s")\n', input$adjust),
             sprintf('cld(emm, Letters = letters, adjust = "%s", reversed = TRUE)',
                     input$adjust))
    })

    output$code_joint <- renderText({
      if (is.null(rv$fit)) return("# Run a model first.")
      paste0(HDR, "\nlibrary(emmeans)\n\n",
             "# Omnibus F-test for each model term (including any interaction)\n",
             "joint_tests(mod)\n\n",
             "# Interaction contrasts (differences of differences), if you fit an\n",
             "# interaction among 2+ factors:\n",
             if (length(input$emmvars) >= 2)
               paste0(emm_call_str(), "\n",
                      "contrast(emm, interaction = \"pairwise\")")
             else
               "# emm <- emmeans(mod, ~ A * B)\n# contrast(emm, interaction = \"pairwise\")")
    })

    output$code_emmplot <- renderText({
      if (is.null(rv$fit)) return("# Run a model first.")
      ev <- input$emmvars
      if (length(ev) < 1)
        return("# Choose one or more categorical fixed effects for EMMeans.")
      roles <- emm_roles(ev, input$emm_by)
      bt <- rv$fit$transform != "none" && isTRUE(input$backtransform)
      yc <- if (bt) "response" else "emmean"
      base <- paste0(HDR, "\nlibrary(ggplot2); library(multcomp)\n\n",
                     emm_call_str(), "\n",
                     sprintf('cl <- as.data.frame(cld(emm, Letters = letters, adjust = "%s", reversed = TRUE))\n',
                             input$adjust),
                     "cl$.group <- trimws(cl$.group)\n",
                     "lcl <- intersect(c(\"lower.CL\",\"asymp.LCL\"), names(cl))[1]\n",
                     "ucl <- intersect(c(\"upper.CL\",\"asymp.UCL\"), names(cl))[1]\n\n")
      facet <- if (!is.na(roles$facet)) sprintf(" +\n  facet_wrap(~ %s)", bq(roles$facet)) else ""
      if (is.na(roles$colour)) {
        plt <- paste0(
          sprintf("ggplot(cl, aes(%s, %s)) +\n", bq(roles$x), yc),
          "  geom_point(size = 3) +\n",
          "  geom_errorbar(aes(ymin = cl[[lcl]], ymax = cl[[ucl]]), width = .15) +\n",
          "  geom_text(aes(label = .group, y = cl[[ucl]]), vjust = -.8, fontface = \"bold\")",
          facet, " +\n  theme_minimal()")
      } else {
        plt <- paste0(
          sprintf("ggplot(cl, aes(%s, %s, colour = %s, group = %s)) +\n",
                  bq(roles$x), yc, bq(roles$colour), bq(roles$colour)),
          "  geom_point(size = 3, position = position_dodge(.6)) +\n",
          "  geom_errorbar(aes(ymin = cl[[lcl]], ymax = cl[[ucl]]),\n",
          "                width = .2, position = position_dodge(.6)) +\n",
          "  geom_text(aes(label = .group, y = cl[[ucl]]), vjust = -.8,\n",
          "            position = position_dodge(.6), show.legend = FALSE)", facet, " +\n",
          "  theme_minimal()")
      }
      paste0(base, plt)
    })

    output$code_compare <- renderText({
      if (is.null(rv$modelA))
        return("# Save a model (button in the sidebar) to use as baseline 'mod_A'.")
      paste0(
        "# mod_A = your saved baseline; mod = your current model.\n",
        "# anova() refits REML models with ML; the LRT is valid only for\n",
        "# nested models fit on identical data.\n",
        "anova(mod_A, mod)\n",
        "AIC(mod_A, mod); BIC(mod_A, mod)")
    })

    # -- Model & code ---------------------------------------------------------
    output$code_block    <- renderText({ req(rv$code); rv$code })
    output$model_summary <- renderPrint({ req(rv$fit); summary(rv$fit$mod) })
    output$dl_code <- downloadHandler(
      filename = function() "mixed_model_code.R",
      content  = function(file) writeLines(rv$code %||% "# run an analysis first", file)
    )

    # -- Fit statistics & variance components ---------------------------------
    output$fit_table <- DT::renderDT({
      req(rv$fit)
      tab <- lmer_fit_stats(rv$fit$mod, nrow(rv$fit$data), rv$fit$reml)
      DT::datatable(tab, options = list(dom = "t", pageLength = 25), rownames = FALSE,
                    class = "compact stripe hover")
    })

    output$fit_help <- renderUI({
      req(rv$fit)
      has_r2 <- requireNamespace("performance", quietly = TRUE) ||
        requireNamespace("MuMIn", quietly = TRUE)
      extra <- if (!has_r2)
        "<br><i>Install the 'performance' package to see marginal/conditional R&sup2; and ICC.</i>" else ""
      HTML(paste0(
        "<div style='margin-top:6px'><b>Marginal R&sup2;</b> = variance explained by fixed effects; ",
        "<b>conditional R&sup2;</b> = variance explained by fixed + random effects. ",
        "<b>ICC</b> is the share of variance attributable to the grouping factor(s). ",
        "Lower AIC/BIC indicate better relative fit.", extra, "</div>"))
    })

    output$vc_table <- DT::renderDT({
      req(rv$fit)
      vc <- as.data.frame(lme4::VarCorr(rv$fit$mod))
      DT::datatable(round_df(vc), options = list(dom = "t"), rownames = FALSE,
                    class = "compact stripe hover")
    })

    output$ranef_plot <- renderPlot({
      req(rv$fit)
      re  <- lme4::ranef(rv$fit$mod, condVar = TRUE)
      dps <- lattice::dotplot(re, strip = TRUE,
                              scales = list(x = list(relation = "free")))
      n <- length(dps)
      if (n == 1) {
        print(dps[[1]])
      } else {
        for (i in seq_len(n))
          print(dps[[i]], split = c(1, i, 1, n), more = (i < n))
      }
    })

    output$ranef_qq <- renderPlot({
      req(rv$fit)
      re <- lme4::ranef(rv$fit$mod)
      dfl <- do.call(rbind, lapply(names(re), function(g) {
        d <- re[[g]]
        do.call(rbind, lapply(names(d), function(term)
          data.frame(panel = paste0(g, ": ", term), value = d[[term]])))
      }))
      validate(need(nrow(dfl) > 1,
                    "Not enough random-effect estimates to assess normality."))
      ggplot(dfl, aes(sample = .data[["value"]])) +
        stat_qq(colour = UF_BLUE) + stat_qq_line(colour = UF_ORANGE, linewidth = 1) +
        facet_wrap("panel", scales = "free") +
        labs(x = "Theoretical quantiles", y = "BLUP",
             title = "Normal Q-Q of random effects") +
        theme_minimal(base_size = 13) +
        theme(plot.title = element_text(face = "bold", colour = UF_BLUE))
    })

    # -- ANOVA ----------------------------------------------------------------
    output$anova_title <- renderText({
      req(rv$fit)
      sprintf("Type %s ANOVA (ddf = %s)",
              if (rv$fit$atype == "3") "III" else "II", rv$fit$ddf)
    })
    output$anova_table <- renderPrint({
      req(rv$fit)
      a <- lmer_anova(rv$fit$mod, rv$fit$atype, rv$fit$ddf)
      if (!is.null(a$error)) { cat(a$error); return() }
      print(a$table)
      if (nzchar(a$note)) cat("\n\n", a$note, sep = "")
    })

    # -- Residual diagnostics -------------------------------------------------
    output$resid_plot <- renderPlot({ req(rv$fit); draw_resid_plots(rv$fit$mod) })
    output$dl_resid <- downloadHandler(
      filename = function() "residual_diagnostics.png",
      content  = function(file) {
        grDevices::png(file, width = 1100, height = 900, res = 120)
        draw_resid_plots(rv$fit$mod); grDevices::dev.off()
      }
    )
    output$resid_help <- renderUI({
      req(rv$fit)
      HTML(paste0(
        "<div style='margin-top:10px'><b>How to read these:</b><ul>",
        "<li><b>Residuals vs Fitted</b> should be a shapeless cloud centred on 0. ",
        "A funnel shape suggests non-constant variance (try a log/sqrt transform); ",
        "a curved orange line suggests a missing term or nonlinearity.</li>",
        "<li><b>Normal Q-Q</b> points should hug the orange line. Heavy tails or an ",
        "S-shape indicate non-normal residuals.</li>",
        "<li><b>Scale-Location</b> should show a roughly flat orange line; an upward ",
        "trend again points to increasing variance with the mean.</li>",
        "<li><b>Histogram</b> should look roughly symmetric and bell-shaped.</li>",
        "</ul>For mixed models these check the residuals only; gross departures are ",
        "what matter for a quick review, not tiny wiggles.</div>"))
    })

    # -- Influence: leave-one-group-out Cook's distance (on demand) -----------
    output$infl_ui <- renderUI({
      req(rv$fit)
      tagList(
        selectInput(ns("infl_group"), "Grouping factor to assess",
                    choices = rv$fit$random),
        actionButton(ns("run_infl"), "Compute influence (can be slow)",
                     class = "btn-outline-secondary"),
        helpText("Refits the model dropping each level of the chosen factor in ",
                 "turn (leave-one-group-out) and reports each group's Cook's ",
                 "distance on the fixed effects -- this can take a while on large ",
                 "data.")
      )
    })

    observeEvent(input$run_infl, {
      req(rv$fit, input$infl_group)
      res <- withProgress(message = "Refitting (leave-one-group-out)...",
                          value = 0, {
        tryCatch(
          lmer_cook(rv$fit$mod, rv$fit$fml, rv$fit$data, input$infl_group,
                    isTRUE(rv$fit$reml),
                    progress = function(amount, detail)
                      incProgress(amount, detail = detail)),
          error = function(e) e)
      })
      if (inherits(res, "error")) {
        rv$cook <- NULL; rv$cook_err <- conditionMessage(res); return()
      }
      if (all(is.na(res$cook))) {
        rv$cook <- NULL
        rv$cook_err <- "Every leave-one-group-out refit failed (the reduced models would not fit)."
        return()
      }
      rv$cook     <- res
      rv$cook_grp <- input$infl_group
      rv$cook_err <- NULL
    })

    output$cook_plot <- renderPlot({
      req(rv$fit)
      validate(need(is.null(rv$cook_err), paste("Influence failed:", rv$cook_err)))
      d <- rv$cook
      validate(need(!is.null(d), "Click 'Compute influence' to estimate Cook's distance."))
      cutoff <- 4 / nrow(d)
      d <- d[is.finite(d$cook), , drop = FALSE]
      validate(need(nrow(d) > 0, "No Cook's distances could be computed."))
      ggplot(d, aes(x = stats::reorder(.data[["group"]], .data[["cook"]]),
                    y = .data[["cook"]])) +
        geom_col(fill = UF_BLUE) +
        geom_hline(yintercept = cutoff, linetype = 2, colour = UF_ORANGE,
                   linewidth = 1) +
        coord_flip() +
        labs(x = rv$cook_grp, y = "Cook's distance",
             title = sprintf("Group influence (cutoff 4/n = %.3f)", cutoff)) +
        theme_minimal(base_size = 13) +
        theme(plot.title = element_text(face = "bold", colour = UF_BLUE))
    })

    output$cook_help <- renderUI({
      req(rv$cook)
      HTML(paste0(
        "<div style='margin-top:6px'>Bars past the dashed line exceed the common ",
        "4/n rule of thumb and may be unduly influencing the fit. Inspect those ",
        "groups; the rule is a flag for a closer look, not an automatic ",
        "delete.</div>"))
    })

    # -- EMMeans --------------------------------------------------------------
    output$emm_note <- renderUI({
      req(rv$fit)
      emmvars <- input$emmvars
      if (length(emmvars) < 1) return(NULL)
      msgs <- character(0)

      if (length(emmvars) >= 2) {
        tl <- attr(stats::terms(rv$fit$mod), "term.labels")
        inter_in_model <- any(vapply(tl, function(t) {
          parts <- gsub("`", "", strsplit(t, ":", fixed = TRUE)[[1]])
          length(parts) >= 2 && all(emmvars %in% parts)
        }, logical(1)))
        if (!inter_in_model)
          msgs <- c(msgs, paste0(
            "These combination means come from an <b>additive model</b> (no ",
            paste(emmvars, collapse = " x "), " interaction term). Any ",
            "non-parallel pattern reflects only main effects, not an interaction. ",
            "Tick 'Include interactions' and re-run to model an interaction; see ",
            "the Interaction test tab for the omnibus F-test."))
      }

      avg_over <- setdiff(rv$fit$cat_fixed, emmvars)
      if (length(avg_over))
        msgs <- c(msgs, paste0(
          "Averaging over categorical fixed effect(s) <b>",
          paste(avg_over, collapse = ", "), "</b> with equal weights (emmeans ",
          "default), not weighted by cell count."))

      if (!length(msgs)) return(NULL)
      div(class = "alert alert-info", tags$b("EMMeans notes:"),
          tags$ul(lapply(msgs, function(m) tags$li(HTML(m)))))
    })

    emm_result <- reactive({
      req(rv$fit)
      er <- lmer_emmeans(rv$fit, input$emmvars, input$emm_by,
                         isTRUE(input$backtransform), input$adjust, input$conf)
      validate(need(isTRUE(er$ok), er$error %||% "EMMeans unavailable."))
      er
    })

    # add the per-cell n to an EMMeans/cld data frame by matching the factor cols
    add_counts <- function(d, cnt, vars) {
      if (is.null(cnt) || !all(vars %in% names(d))) return(d)
      keyf <- function(x) do.call(paste,
                                  c(lapply(vars, function(v) as.character(x[[v]])), sep = "\r"))
      d$n <- cnt$n[match(keyf(d), keyf(cnt))]
      d
    }

    output$emm_table <- DT::renderDT({
      er  <- emm_result()
      em  <- add_counts(as.data.frame(er$emm), er$counts, er$vars)
      cap <- if (!is.null(er$held))
        tags$caption(style = "caption-side:top;",
                     sprintf("Continuous covariate(s) held at their mean: %s", er$held)) else NULL
      DT::datatable(round_df(em), caption = cap, class = "compact stripe hover",
                    options = list(dom = "t", pageLength = 25), rownames = FALSE)
    })
    output$pairs_table <- DT::renderDT({
      DT::datatable(round_df(emm_result()$pairs), class = "compact stripe hover",
                    options = list(dom = "tp", pageLength = 15), rownames = FALSE)
    })
    output$cld_table <- DT::renderDT({
      er <- emm_result()
      cl <- add_counts(er$cld, er$counts, er$vars)
      DT::datatable(round_df(cl), class = "compact stripe hover",
                    options = list(dom = "t", pageLength = 25), rownames = FALSE)
    })
    output$dl_emm <- downloadHandler(
      filename = function() "emmeans.csv",
      content  = function(f) utils::write.csv(as.data.frame(emm_result()$emm), f, row.names = FALSE))
    output$dl_pairs <- downloadHandler(
      filename = function() "pairwise_comparisons.csv",
      content  = function(f) utils::write.csv(emm_result()$pairs, f, row.names = FALSE))
    output$dl_cld <- downloadHandler(
      filename = function() "cld_letters.csv",
      content  = function(f) utils::write.csv(emm_result()$cld, f, row.names = FALSE))

    # -- Interaction test -----------------------------------------------------
    output$joint_table <- renderPrint({
      req(rv$fit)
      jt <- lmer_joint_tests(rv$fit$mod)
      if (is.null(jt$table)) { cat(jt$note); return() }
      print(jt$table)
      if (nzchar(jt$note)) cat("\n\n", jt$note, sep = "")
    })

    output$inter_contrast_table <- renderPrint({
      req(rv$fit)
      emmvars <- input$emmvars
      if (length(emmvars) < 2) {
        cat("Select two or more EMMeans factors (and leave 'compare within' unset)\n",
            "to see interaction contrasts.", sep = ""); return()
      }
      if (length(intersect(input$emm_by, emmvars))) {
        cat("A 'compare within' factor is set, so EMMeans are conditioned (simple\n",
            "effects). Clear it to compute interaction contrasts across all factors.",
            sep = ""); return()
      }
      bt  <- rv$fit$transform != "none" && isTRUE(input$backtransform)
      tr  <- tran_for_emmeans(rv$fit$transform)
      spec <- stats::as.formula(paste("~", paste(bq_each(emmvars), collapse = " * ")))
      emm <- tryCatch({
        if (bt && !is.null(tr))
          emmeans::emmeans(emmeans::ref_grid(rv$fit$mod, tran = tr), spec,
                           type = "response", level = input$conf)
        else
          emmeans::emmeans(rv$fit$mod, spec,
                           type = if (bt) "response" else "link", level = input$conf)
      }, error = function(e) e)
      if (inherits(emm, "error")) { cat("EMMeans failed:", conditionMessage(emm)); return() }
      ic <- tryCatch(emmeans::contrast(emm, interaction = "pairwise",
                                       adjust = input$adjust),
                     error = function(e) e)
      if (inherits(ic, "error")) cat("Interaction contrasts failed:", conditionMessage(ic))
      else print(ic)
    })

    # -- EMMeans plot ---------------------------------------------------------
    output$emm_plot <- renderPlot({
      print(lmer_emm_plot(emm_result(), rv$fit$response, rv$fit$transform))
    })
    output$dl_emmplot <- downloadHandler(
      filename = function() "emmeans_plot.png",
      content  = function(file)
        ggsave(file, lmer_emm_plot(emm_result(), rv$fit$response, rv$fit$transform),
               width = 9, height = 6, dpi = 150)
    )

    # -- Model comparison -----------------------------------------------------
    observeEvent(input$save_model, {
      if (is.null(rv$fit)) {
        rv$message <- list(type = "warning",
                           text = "Run a model first, then save it as the comparison baseline.")
        return()
      }
      rv$modelA <- rv$fit
    })

    output$compare_status <- renderUI({
      a <- rv$modelA; b <- rv$fit
      if (is.null(a))
        return(div(class = "alert alert-info",
                   "No saved model yet. Fit a model and click ",
                   tags$b("Save model for comparison"), "."))
      cur <- if (is.null(b)) "(none - run a model)" else b$fml_str
      HTML(paste0(
        "<div style='margin-bottom:8px'>",
        "<b>Saved (A):</b> <code>", a$fml_str, "</code><br>",
        "<b>Current (B):</b> <code>", cur, "</code></div>"))
    })

    output$compare_lrt <- renderPrint({
      a <- rv$modelA; b <- rv$fit
      if (is.null(a) || is.null(b)) {
        cat("Need a saved model (A) and a current fitted model (B).\n"); return()
      }
      cmp <- lmer_compare(a, b)
      cat(paste(cmp$warnings, collapse = "\n\n"))
      cat("\n\n")
      if (!is.null(cmp$error)) cat("Comparison failed:", cmp$error)
      else if (!is.null(cmp$anova)) print(cmp$anova)
    })

    # Expose the augmented dataset (base import + combined variables) so an
    # Export stage can download exactly what the model used, plus a report
    # payload of the current fit so a Report stage can include the model.
    # `$data` keeps the old contract for callers that only want the frame.
    list(
      data   = dataset,
      report = reactive({
        f <- rv$fit
        if (is.null(f) || !isTRUE(f$ok)) return(NULL)
        em <- tryCatch(emm_result(), error = function(e) NULL)
        lmer_report_payload(f, if (isTRUE(em$ok)) em else NULL, rv$code)
      })
    )
  })
}
