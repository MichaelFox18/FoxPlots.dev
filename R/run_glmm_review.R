# ============================================================
# run_glmm_review.R -- focused import -> GLMM -> export app
# ============================================================
# The GLMM Review module (mod_glmm, instantiated twice: General and Binary)
# bracketed by the shared mod_import (upload + Data Health + type recast +
# the single data preview) and mod_export (data-only download; its own
# preview is suppressed so there is one preview). Combined variables created
# on either analysis tab are merged into the exported data.

#' The GLMM Review app
#'
#' A focused generalized linear mixed-model (glmmTMB / DHARMa / emmeans)
#' review tool: import a table, pick a response, a distribution family,
#' fixed effects and grouping (random) factors from drop-downs, and get
#' Wald ANOVA, DHARMa simulation-based residual diagnostics, EMMeans
#' post-hoc comparisons with letter groupings, and copy-pasteable R code --
#' without writing any `glmmTMB()` calls. Count, proportion, and
#' zero-inflated responses live on the General GLMM tab (with optional
#' zero-inflation and dispersion models); 0/1 outcomes get their own
#' Binary tab with a link-function picker and Bernoulli-specific
#' guardrails. Import and type recasting are handled by the shared import
#' stage; the result data (with any created variables) downloads from the
#' export stage.
#'
#' `glmm_review_app()` builds and returns the Shiny app object;
#' `run_glmm_review()` launches it in a browser.
#'
#' @param ... Passed on to [shiny::runApp()].
#' @return `glmm_review_app()` returns a [shiny::shinyApp()] object;
#'   `run_glmm_review()` runs the app (called for its side effect).
#' @examples
#' if (interactive()) run_glmm_review()
#' @export
glmm_review_app <- function() {
  options(shiny.maxRequestSize = 250 * 1024^2)

  ui <- page_navbar(
    title        = uf_title("GLMM Review"),
    window_title = "UF/IFAS GLMM Review",
    theme        = uf_theme(),
    fillable     = FALSE,   # the model tabs are long scrolling stacks of results

    about_nav_panel(
      "GLMM Review",
      paste("Fit and review generalized linear mixed models (glmmTMB):",
            "counts, proportions, zero-inflated and binary outcomes, with",
            "DHARMa simulation-based residual checks, Wald ANOVA, and",
            "EMMeans post-hoc comparisons with letter groupings."),
      c("Import your data (or load the built-in field example) on the Import tab.",
        "Pick a response, family, fixed and random effects on the General GLMM tab -- or use the Binary (0/1) tab for presence/absence outcomes -- then fit the model.",
        "Check the DHARMa residuals tab before trusting the ANOVA or EMMeans results; export the augmented data on the Export tab.")),
    nav_panel(tagList(icon("file-arrow-up"), " Import"), value = "import",
              importUI("imp",
                       example_choices = c(
                         "Field example (counts + proportion + binary)" = "glmm"))),
    nav_panel(tagList(icon("chart-simple"), " General GLMM"), value = "general",
              glmmUI("g", binary = FALSE)),
    nav_panel(tagList(icon("toggle-on"), " Binary (0/1) GLMM"), value = "binary",
              glmmUI("b", binary = TRUE)),
    nav_panel(tagList(icon("file-export"), " Export"), value = "export",
              exportUI("ex", preview = FALSE))
  )

  server <- function(input, output, session) {
    imported <- importServer("imp", examples = list(glmm = make_glmm_example_data()))
    g_data   <- glmmServer("g", imported, binary = FALSE)
    b_data   <- glmmServer("b", imported, binary = TRUE)

    # Exported frame = imported data + combined variables created on EITHER
    # analysis tab (each module returns its own augmented copy of the same
    # rows; new columns are unioned here).
    merged <- shiny::reactive({
      base <- imported(); shiny::req(is.data.frame(base))
      for (aug in list(g_data, b_data)) {
        d <- tryCatch(aug(), error = function(e) NULL)
        if (is.data.frame(d) && nrow(d) == nrow(base))
          for (nm in setdiff(names(d), names(base))) base[[nm]] <- d[[nm]]
      }
      base
    })
    exportServer("ex", merged, preview = FALSE)  # data-only; one preview lives in Import
  }

  shiny::shinyApp(ui, server)
}

#' @rdname glmm_review_app
#' @export
run_glmm_review <- function(...) {
  shiny::runApp(glmm_review_app(), ...)
}
