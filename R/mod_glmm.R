# ============================================================
# mod_glmm.R -- the GLMM Review stage (glmmTMB / DHARMa / emmeans)
# ============================================================
# A drop-down generalized linear mixed-model tool, absorbed from a
# single-file app into the kit's module pattern. Data arrives via `data_in`
# (mod_import owns upload + Data Health + type recast + preview), so this
# module keeps only the model-spec sidebar, the combined / interaction
# variable builder (the one feature mod_import lacks; its combos feed the
# dispersion model), and the analysis result tabs. All statistical work
# lives in pure helpers (helpers_glmm.R); this module is the thin wrapper.
#
# The module is instantiated TWICE per app -- glmmUI(id, binary = FALSE) for
# the general tab (family picker + zero-inflation + dispersion models) and
# glmmUI(id, binary = TRUE) for the 0/1 tab (family fixed to Bernoulli, a
# link picker, no dispersion model: 0/1 data has no free dispersion
# parameter). Keeping the binary path separate avoids the "it silently did
# something odd with my 0/1 column" failure mode.
#
# glmmServer(id, data_in, binary) returns the augmented dataset reactive
# (base import + any user-created combined variables) so an Export stage can
# download it.

glmmUI <- function(id, binary = FALSE) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 340,
      if (binary) helpText(
        tags$b("Binary outcomes are handled separately."),
        " The response must be numeric 0/1 or a two-level factor; the family",
        " is fixed to a Bernoulli (binomial) distribution. No dispersion",
        " model is offered here because true 0/1 data has no free dispersion",
        " parameter."),
      tags$details(open = NA,
        tags$summary(tags$b("Create combined variable (interaction)")),
        helpText("Paste 2+ columns into one new factor (e.g. ",
                 "Treatment.Season) \u2014 usable as a predictor here",
                 if (!binary) " (including in the dispersion model)",
                 " and included when you export."),
        uiOutput(ns("combo_vars_ui")),
        textInput(ns("combo_sep"), "Separator", value = "."),
        actionButton(ns("combo_add"), "Create variable", class = "btn-primary"),
        uiOutput(ns("combo_list_ui"))
      ),
      tags$hr(),
      uiOutput(ns("response_ui")),
      if (binary) uiOutput(ns("level_note_ui")),
      if (binary) selectInput(ns("link"),
        tagList("Link function", info_tip(
          "logit: symmetric, most common, coefficients are log-odds. ",
          "probit: symmetric, coefficients on a normal-CDF scale. ",
          "cloglog: asymmetric, useful when 1s are rare. ",
          "cauchit: symmetric with heavier tails, robust to extreme ",
          "predictors.")),
        choices = GLMM_BINARY_LINKS, selected = "logit"),
      if (!binary) selectInput(ns("family"),
        tagList("Family (distribution + link)", info_tip(
          "Choose the distribution that matches your response's valid ",
          "range (e.g. counts, positive continuous, or a proportion). ",
          "The note below the box updates for whatever family is selected ",
          "and lists exactly what values are valid.")),
        choices = GLMM_FAMILY_CHOICES, selected = "nbinom2_log"),
      if (!binary) uiOutput(ns("family_note_ui")),
      uiOutput(ns("fixed_ui")),
      checkboxInput(ns("interactions"),
                    "Include interactions among fixed effects", FALSE),
      uiOutput(ns("random_ui")),
      tags$details(
        tags$summary(tags$b("Advanced model options")),
        uiOutput(ns("slope_ui")),
        uiOutput(ns("slope_group_ui")),
        tags$hr(),
        checkboxInput(ns("zi_on"), tagList(
          if (binary) "Zero-inflation term (rare for true 0/1 data)"
          else        "Zero-inflation model (ziformula)",
          info_tip(
            "Models the probability of a structural extra zero on top of ",
            "the count/response distribution. Leave the predictor picker ",
            "empty for an intercept-only zero-inflation model (~1).")),
          value = FALSE),
        uiOutput(ns("zi_vars_ui")),
        if (!binary) uiOutput(ns("disp_vars_ui")),
        if (!binary) helpText(
          "The dispersion model (dispformula) lets residual spread vary ",
          "with 0, 1, or 2 predictors (e.g. by Treatment). Created ",
          "combined variables can be used here too.")
      ),
      tags$hr(),
      uiOutput(ns("emm_ui")),
      uiOutput(ns("emm_by_ui")),
      selectInput(ns("adjust"),
        tagList("Post-hoc p-value adjustment", info_tip(
          "How pairwise p-values are corrected for multiple comparisons. ",
          "Tukey is the usual choice for all-pairs comparisons of means.")),
        choices = c("Tukey" = "tukey", "Sidak" = "sidak",
                    "Bonferroni" = "bonferroni", "Holm" = "holm",
                    "None" = "none"),
        selected = "tukey"),
      sliderInput(ns("conf"),
        tagList("Confidence level", info_tip(
          "Confidence level for the EMMeans intervals and the pairwise ",
          "comparisons.")),
        min = 0.80, max = 0.99, value = 0.95, step = 0.01),
      tags$hr(),
      actionButton(ns("run"),   "Fit model", class = "btn-primary"),
      actionButton(ns("reset"), "Reset",     class = "btn-outline-secondary")
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

      nav_panel("Model & code",
        h4("Formulas used"),
        verbatimTextOutput(ns("formula_txt")),
        h4("Model summary"),
        verbatimTextOutput(ns("model_summary")),
        h4("Fit statistics"),
        DT::DTOutput(ns("fit_table")),
        h4("R code for this model"),
        verbatimTextOutput(ns("code_block")),
        tags$button("Copy code",
                    class = "btn btn-outline-secondary btn-sm uf-copy",
                    onclick = sprintf("DEcopy('%s', this)", ns("code_block"))),
        downloadButton(ns("dl_code"), "Download .R",
                       class = "btn-outline-secondary uf-dl")),

      nav_panel("Wald ANOVA",
        helpText("Type III Wald chi-square tests (car::Anova) \u2014 the ",
                 "standard omnibus test for glmmTMB fits. Each row tests ",
                 "whether that term explains real variation, given the ",
                 "others."),
        uiOutput(ns("anova_note")),
        DT::DTOutput(ns("anova_table"))),

      nav_panel("DHARMa residuals",
        helpText(if (binary) paste0(
          "For 0/1 data the DHARMa QQ/residual plot is naturally grainy; ",
          "focus on the formal tests below rather than the visual pattern.")
        else paste0(
          "Simulation-based residuals \u2014 the right tool for glmmTMB ",
          "(ordinary Pearson/deviance residuals from a GLMM are not ",
          "reliably uniform, so this replaces a classic residual panel). ",
          "Check this tab before trusting the ANOVA or EMMeans results.")),
        plotOutput(ns("dharma_plot"), height = "420px"),
        downloadButton(ns("dl_dharma"), "Download PNG",
                       class = "btn-outline-secondary uf-dl"),
        h4("Overdispersion"),
        verbatimTextOutput(ns("disp_test")),
        if (!binary) h4("Zero inflation"),
        if (!binary) verbatimTextOutput(ns("zi_test")),
        h4("Outliers"),
        verbatimTextOutput(ns("outlier_test")),
        glmm_code_panel(ns, "code_dharma", "DHARMa code")),

      nav_panel("EMMeans & post-hoc",
        helpText(if (binary) paste0(
          "Estimated marginal means are shown on the probability scale; ",
          "pairwise comparisons come out as odds ratios (logit link).")
        else paste0(
          "Estimated marginal means are shown on the response scale ",
          "(counts, proportions, ...). For log/logit-link families the ",
          "pairwise comparisons come out as ratios, not differences.")),
        h4("Estimated marginal means (response scale)"),
        DT::DTOutput(ns("emm_table")),
        downloadButton(ns("dl_emm"), "Download CSV",
                       class = "btn-outline-secondary uf-dl"),
        h4("Pairwise comparisons"),
        DT::DTOutput(ns("pairs_table")),
        downloadButton(ns("dl_pairs"), "Download CSV",
                       class = "btn-outline-secondary uf-dl"),
        h4("Compact letter display (cld)"),
        DT::DTOutput(ns("cld_table")),
        downloadButton(ns("dl_cld"), "Download CSV",
                       class = "btn-outline-secondary uf-dl"),
        helpText("Means sharing a letter are not significantly different ",
                 "at the chosen adjustment level."),
        h4("EMMeans plot"),
        plotOutput(ns("emm_plot"), height = "480px"),
        downloadButton(ns("dl_emmplot"), "Download PNG",
                       class = "btn-outline-secondary uf-dl"),
        glmm_code_panel(ns, "code_emm", "EMMeans + post-hoc + cld code"))
    )
  )
}

# A collapsible, copy-able code block tied to a renderText output (same
# construction as lmer_code_panel; duplicated locally so the two modules
# stay independently readable).
glmm_code_panel <- function(ns, out_id, label = "R code") {
  tags$details(class = "uf-codewrap",
    tags$summary(tags$b(sprintf("> %s (copy & run in R)", label))),
    tags$button("Copy code", class = "btn btn-outline-secondary btn-sm uf-copy",
                onclick = sprintf("DEcopy('%s', this)", ns(out_id))),
    verbatimTextOutput(ns(out_id))
  )
}

glmmServer <- function(id, data_in, binary = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(fit = NULL, message = NULL, combos = list(),
                         reset = 0)

    # -- Data ---------------------------------------------------------------
    # Base frame from import; defensive character->factor and ordered->plain
    # pass so emmeans never gets polynomial contrasts from an ordered factor.
    base_data <- reactive({
      df <- data_in(); req(is.data.frame(df))
      df[] <- lapply(df, function(x) {
        if (is.character(x)) factor(x)
        else if (is.ordered(x)) factor(x, ordered = FALSE)
        else x
      })
      df
    })

    dataset <- reactive({
      df <- base_data()
      for (cb in rv$combos)
        if (all(cb$vars %in% names(df)))
          df[[cb$name]] <- make_combined_factor(df, cb$vars, cb$sep)
      df
    })

    observeEvent(data_in(), { rv$fit <- NULL }, ignoreNULL = FALSE)

    # -- Combined-variable builder ------------------------------------------
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
      sep  <- if (is.null(input$combo_sep) || !nzchar(input$combo_sep)) "."
              else input$combo_sep
      name <- paste(vars, collapse = sep)
      n_lev <- nlevels(make_combined_factor(base_data(), vars, sep))
      rv$combos <- c(rv$combos[vapply(rv$combos,
                                      function(c) c$name != name, logical(1))],
                     list(list(name = name, vars = vars, sep = sep)))
      rv$message <- list(type = "info", text = sprintf(
        "Created combined variable '%s' from %s (%d level%s). It is now available as a predictor and is included when you export the data.",
        name, paste(vars, collapse = " + "), n_lev, if (n_lev == 1) "" else "s"))
    })

    observeEvent(input$combo_clear, {
      rv$combos <- list()
      rv$message <- list(type = "info",
                         text = "Removed all created combined variables.")
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

    # -- Dynamic variable selectors -----------------------------------------
    output$response_ui <- renderUI({
      df <- dataset(); req(df); rv$reset
      if (binary) {
        cand <- names(df)[vapply(df, function(x)
          (is.numeric(x) && all(x[is.finite(x)] %in% c(0, 1))) ||
            (is.factor(x) && nlevels(x) == 2), logical(1))]
        validate(need(length(cand) > 0, paste0(
          "No 0/1 or two-level factor column found for a binary response. ",
          "If your outcome is coded differently, recode it on the Import ",
          "tab first.")))
        selectInput(ns("response"),
                    "Response variable (0/1 or two-level factor)",
                    choices = cand)
      } else {
        num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
        validate(need(length(num_cols) > 0,
                      "No numeric columns available for a response variable."))
        selectInput(ns("response"), "Response variable", choices = num_cols)
      }
    })

    output$level_note_ui <- renderUI({
      df <- dataset(); r <- input$response
      req(binary, df, r, r %in% names(df))
      if (is.factor(df[[r]]))
        helpText(sprintf(
          "Modeling P(%s = \"%s\") \u2014 the second factor level counts as \"success\".",
          r, levels(df[[r]])[2]))
    })

    # Live valid-domain note for the selected family, plus an automatic check
    # of the chosen response's actual values BEFORE the user hits Fit model.
    output$family_note_ui <- renderUI({
      req(!binary, input$family)
      note <- GLMM_FAMILY_NOTE[[input$family]] %||% ""
      df <- dataset(); resp <- input$response
      warn <- if (!is.null(resp) && resp %in% names(df))
        glmm_domain_check(df[[resp]], input$family)
      tagList(
        helpText(note),
        if (!is.null(warn))
          div(class = "alert alert-warning",
              style = "padding:6px 10px;", warn))
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
      fx <- input$fixed
      selectInput(ns("ranslope"), "Random slope variable (optional)",
                  choices = c("(intercept only)" = "", fx), selected = "")
    })

    output$slope_group_ui <- renderUI({
      rnd <- input$random
      if (is.null(input$ranslope) || !nzchar(input$ranslope) ||
          length(rnd) < 1) return(NULL)
      selectInput(ns("ranslope_group"),
                  "Apply random slope to which grouping factor(s)?",
                  choices = rnd, selected = rnd[1], multiple = TRUE)
    })

    output$zi_vars_ui <- renderUI({
      if (!isTRUE(input$zi_on)) return(NULL)
      df <- dataset(); req(df)
      selectizeInput(ns("zi_vars"),
                     "Zero-inflation predictors (blank = intercept only)",
                     choices = names(df), multiple = TRUE)
    })

    output$disp_vars_ui <- renderUI({
      df <- dataset(); req(df, !binary)
      selectizeInput(ns("disp_vars"),
                     "Dispersion predictors (0, 1, or 2)",
                     choices = names(df), multiple = TRUE,
                     options = list(maxItems = 2,
                                    placeholder = "none = constant dispersion"))
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
                  choices = c("(none - compare all cells)" = "",
                              input$emmvars),
                  selected = "")
    })

    # -- Reset --------------------------------------------------------------
    observeEvent(input$reset, {
      rv$reset <- rv$reset + 1
      rv$fit <- NULL; rv$message <- NULL; rv$combos <- list()
      if (!binary) updateSelectInput(session, "family",
                                     selected = "nbinom2_log")
      if (binary)  updateSelectInput(session, "link", selected = "logit")
      updateSelectInput(session, "adjust", selected = "tukey")
      updateCheckboxInput(session, "interactions", value = FALSE)
      updateCheckboxInput(session, "zi_on", value = FALSE)
      updateSliderInput(session, "conf", value = 0.95)
    })

    # -- Fit on demand ------------------------------------------------------
    observeEvent(input$run, {
      df <- dataset(); req(df)
      resp <- input$response
      fx   <- input$fixed  %||% character(0)
      rnd  <- input$random %||% character(0)
      if (is.null(resp) || !nzchar(resp)) {
        rv$message <- list(type = "warning",
                           text = "Choose a response variable.")
        return()
      }
      if (length(fx) == 0 && length(rnd) == 0) {
        rv$message <- list(type = "warning",
          text = "Choose at least one fixed or random effect.")
        return()
      }
      spec <- list(
        response = resp, fixed = fx, random = rnd,
        interactions = isTRUE(input$interactions),
        slope = input$ranslope, slope_group = input$ranslope_group,
        binary = binary,
        link = if (binary) input$link %||% "logit",
        family_key = if (!binary) input$family %||% "nbinom2_log",
        zi_on = isTRUE(input$zi_on),
        zi_vars = input$zi_vars %||% character(0),
        disp_vars = if (!binary) input$disp_vars %||% character(0)
                    else character(0))
      res <- glmm_fit(df, spec)
      if (!isTRUE(res$ok)) {
        rv$message <- list(type = "error", text = res$error)
        rv$fit <- NULL
        return()
      }
      rv$fit <- res
      rv$message <- if (length(res$notes))
        list(type = "info", text = res$notes)
      else list(type = "info", text = "Model fitted.")
    })

    # -- Message box --------------------------------------------------------
    output$message_box <- renderUI({
      m <- rv$message; if (is.null(m)) return(NULL)
      cls <- switch(m$type, error = "alert alert-danger",
                    info = "alert alert-info", "alert alert-warning")
      lbl <- switch(m$type, error = "Error: ", info = "Note: ", "Heads up: ")
      div(class = cls, tags$b(lbl), tags$ul(lapply(m$text, tags$li)))
    })

    # -- Model & code -------------------------------------------------------
    output$formula_txt <- renderPrint({
      f <- rv$fit
      validate(need(!is.null(f), "Fit a model to see the formulas."))
      cat("Conditional: ", f$fml_str, "\n")
      cat("ziformula:   ", f$zi_str, "\n")
      if (!binary) cat("dispformula: ", f$disp_str, "\n")
      cat("family:      ", glmm_family_text(f), "\n")
    })

    output$model_summary <- renderPrint({
      f <- rv$fit
      validate(need(!is.null(f), "Fit a model to see its summary."))
      print(summary(f$mod))
    })

    output$fit_table <- DT::renderDT({
      f <- rv$fit
      validate(need(!is.null(f), "Fit a model to see the fit statistics."))
      DT::datatable(glmm_fit_stats(f$mod, n_total = nrow(dataset())),
                    rownames = FALSE, class = "compact stripe",
                    options = list(dom = "t", scrollX = TRUE))
    })

    full_code <- reactive({
      f <- rv$fit; req(f)
      paste0(glmm_code(f), "\n",
             glmm_emm_code(input$emmvars,
                           if (nzchar(input$emm_by %||% "")) input$emm_by,
                           input$adjust %||% "tukey",
                           input$conf %||% 0.95))
    })

    output$code_block <- renderPrint({
      f <- rv$fit
      validate(need(!is.null(f), "Fit a model to see its R code."))
      cat(full_code())
    })

    output$dl_code <- downloadHandler(
      filename = function() "glmm_model_code.R",
      content  = function(file) writeLines(full_code(), file))

    # -- Wald ANOVA ---------------------------------------------------------
    anova_result <- reactive({
      f <- rv$fit; req(f)
      glmm_anova(f$mod)
    })

    output$anova_note <- renderUI({
      f <- rv$fit; if (is.null(f)) return(NULL)
      a <- anova_result()
      if (!a$ok) div(class = "alert alert-warning", a$note)
    })

    output$anova_table <- DT::renderDT({
      f <- rv$fit
      validate(need(!is.null(f), "Fit a model to see the Wald ANOVA."))
      a <- anova_result()
      validate(need(a$ok, "ANOVA unavailable \u2014 see the note above."))
      DT::datatable(a$table, rownames = FALSE, class = "compact stripe",
                    options = list(dom = "t", scrollX = TRUE))
    })

    # -- DHARMa -------------------------------------------------------------
    dharma_sim <- reactive({
      f <- rv$fit; req(f)
      glmm_dharma(f$mod)
    })

    output$dharma_plot <- renderPlot({
      f <- rv$fit
      validate(need(!is.null(f), "Fit a model to see the DHARMa residuals."))
      s <- dharma_sim()
      validate(need(s$ok, s$error))
      plot(s$sim)
    })

    output$dl_dharma <- downloadHandler(
      filename = function() "dharma_residuals.png",
      content  = function(file) {
        s <- dharma_sim(); req(s$ok)
        grDevices::png(file, width = 900, height = 600)
        on.exit(grDevices::dev.off(), add = TRUE)
        plot(s$sim)
      })

    dharma_tests <- reactive({
      s <- dharma_sim(); req(s$ok)
      glmm_dharma_tests(s$sim, binary = binary)
    })

    output$disp_test <- renderPrint({
      f <- rv$fit
      validate(need(!is.null(f), "Fit a model to run the dispersion tests."))
      ratio <- glmm_pearson_ratio(f$mod)
      cat("Pearson chi-sq / residual df ratio:",
          if (is.na(ratio)) "unavailable" else ratio, "\n")
      cat("  (~1 = fine; >>1 = overdispersion; <<1 = underdispersion)\n\n")
      s <- dharma_sim()
      validate(need(s$ok, s$error))
      cat("DHARMa simulation-based dispersion test:\n")
      print(dharma_tests()$dispersion)
    })

    output$zi_test <- renderPrint({
      f <- rv$fit
      validate(need(!is.null(f) && !binary, ""))
      s <- dharma_sim()
      validate(need(s$ok, s$error))
      print(dharma_tests()$zero_inflation)
    })

    output$outlier_test <- renderPrint({
      f <- rv$fit
      validate(need(!is.null(f), "Fit a model to run the outlier test."))
      s <- dharma_sim()
      validate(need(s$ok, s$error))
      print(dharma_tests()$outliers)
    })

    output$code_dharma <- renderPrint(cat(glmm_dharma_code(binary)))

    # -- EMMeans ------------------------------------------------------------
    emm_result <- reactive({
      f <- rv$fit; req(f)
      by <- if (nzchar(input$emm_by %||% "")) input$emm_by else NULL
      glmm_emmeans(f, input$emmvars %||% character(0), by = by,
                   adjust = input$adjust %||% "tukey",
                   level  = input$conf %||% 0.95)
    })

    emm_dt <- function(get_df, empty_msg) {
      DT::renderDT({
        f <- rv$fit
        validate(need(!is.null(f), "Fit a model first."))
        er <- emm_result()
        validate(need(er$ok, er$error %||% empty_msg))
        DT::datatable(round_df(get_df(er)), rownames = FALSE,
                      class = "compact stripe",
                      options = list(dom = "tp", scrollX = TRUE))
      })
    }

    output$emm_table   <- emm_dt(function(er) er$emm_df,
                                 "EMMeans unavailable.")
    output$pairs_table <- emm_dt(function(er) er$pairs,
                                 "Pairwise comparisons unavailable.")
    output$cld_table   <- emm_dt(function(er) er$cld,
                                 "Letter display unavailable.")

    emm_csv <- function(stem, get_df) {
      downloadHandler(
        filename = function() paste0(stem, ".csv"),
        content  = function(file) {
          er <- emm_result(); req(er$ok)
          utils::write.csv(get_df(er), file, row.names = FALSE)
        })
    }

    output$dl_emm   <- emm_csv("emmeans",  function(er) er$emm_df)
    output$dl_pairs <- emm_csv("pairwise", function(er) er$pairs)
    output$dl_cld   <- emm_csv("cld",      function(er) er$cld)

    emm_plot_obj <- reactive({
      f <- rv$fit; req(f)
      er <- emm_result(); req(er$ok)
      lmer_emm_plot(er, f$response, "none") +
        labs(y = paste0(f$response, " (response scale)"))
    })

    output$emm_plot <- renderPlot({
      f <- rv$fit
      validate(need(!is.null(f), "Fit a model to see the EMMeans plot."))
      er <- emm_result()
      validate(need(er$ok, er$error))
      print(emm_plot_obj())
    })

    output$dl_emmplot <- downloadHandler(
      filename = function() "emmeans_plot.png",
      content  = function(file)
        ggsave(file, emm_plot_obj(), width = 9, height = 6, dpi = 150))

    output$code_emm <- renderPrint({
      code <- glmm_emm_code(input$emmvars,
                            if (nzchar(input$emm_by %||% "")) input$emm_by,
                            input$adjust %||% "tukey",
                            input$conf %||% 0.95)
      if (!nzchar(code))
        cat("# Pick one or more EMMeans factors in the sidebar first.\n")
      else cat(code)
    })

    # Return the augmented dataset (base + created combined variables) so the
    # Export stage can download it.
    dataset
  })
}
