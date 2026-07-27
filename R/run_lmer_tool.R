# ============================================================
# run_lmer_tool.R -- focused import -> mixed model -> export app
# ============================================================
# The Mixed Model Review module (mod_lmer) bracketed by the shared mod_import
# (upload + Data Health + type recast + the single data preview) and mod_export
# (data-only download; its own preview is suppressed so there is one preview).

#' The Mixed Model Review app
#'
#' A focused mixed-model (lmerTest / emmeans) review tool: import a table, pick a
#' response, fixed effects and grouping (random) factors from drop-downs, and get
#' an ANOVA, fit diagnostics, EMMeans post-hoc comparisons and copy-pasteable R
#' code -- without writing any `lmer()` calls. Import and type recasting are
#' handled by the shared import stage; the result data (with any created
#' variables) downloads from the export stage.
#'
#' `lmer_tool_app()` builds and returns the Shiny app object;
#' `run_lmer_tool()` launches it in a browser.
#'
#' @param ... Passed on to [shiny::runApp()].
#' @return `lmer_tool_app()` returns a [shiny::shinyApp()] object;
#'   `run_lmer_tool()` runs the app (called for its side effect).
#' @examples
#' if (interactive()) run_lmer_tool()
#' @export
lmer_tool_app <- function() {
  options(shiny.maxRequestSize = 250 * 1024^2)

  ui <- page_navbar(
    title        = uf_title("Mixed Model Review"),
    window_title = "UF/IFAS Mixed Model Review",
    theme        = uf_theme(),
    fillable     = FALSE,   # the model tab is a long scrolling stack of results

    about_nav_panel(
      "Mixed Model Review",
      paste("Fit and review linear mixed models (lmerTest / emmeans): ANOVA,",
            "variance components, residual diagnostics, estimated marginal means",
            "with letter groupings, interaction tests, and model comparison."),
      c("Import your data (or load the built-in RCBD example) on the Import tab.",
        "Specify the response, fixed effects, and random effects on the Mixed Model tab, then run the analysis.",
        "Review the ANOVA, EMMeans, and diagnostics; export the augmented data on the Export tab.")),
    nav_panel(tagList(icon("file-arrow-up"),   " Import"),      value = "import",
              importUI("imp",
                       example_choices = c("RCBD example (3-factor)" = "rcbd"))),
    nav_panel(tagList(icon("diagram-project"), " Mixed Model"), value = "model",
              lmerUI("lmer")),
    nav_panel(tagList(icon("file-export"),     " Export & Report"),
              value = "export",
              exportReportUI("ex", "rep", preview = FALSE,
                             default_title = "Mixed Model Report"))
  )

  server <- function(input, output, session) {
    imported <- importServer("imp", examples = list(rcbd = make_example_data()))
    lmer_out <- lmerServer("lmer", imported)   # -> list(data =, report =)
    exportServer("ex", lmer_out$data, preview = FALSE)  # one preview, in Import
    # NULL-safe: the module dataset req()s, and a silent error there would
    # suppress the report's own "import data first" empty state.
    rep_data <- shiny::reactive(
      tryCatch(lmer_out$data(), error = function(e) NULL))
    reportServer("rep", rep_data,
                 mixed = reactive(Filter(Negate(is.null),
                                         list(lmer_out$report()))),
                 default_title = "Mixed Model Report")
  }

  shiny::shinyApp(ui, server)
}

#' @rdname lmer_tool_app
#' @export
run_lmer_tool <- function(...) {
  shiny::runApp(lmer_tool_app(), ...)
}
