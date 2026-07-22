# ============================================================
# helpers_model.R -- fixed-effects regression (lm)
# ============================================================
# Pure model helpers for the Regression tab: build a model from a spec, fit it,
# read a coefficient table / fit statistics / plain-English interpretation out
# of the result, generate reproducible code, and build the diagnostic ggplots.
# mod_regression.R is a thin wrapper over these.
#
# The engine is spec-driven (reg_spec -> reg_validate -> reg_fit), mirroring
# helpers_lmer.R, and supports numeric AND categorical predictors, pairwise
# interactions, and a polynomial term. fit_model() is a back-compatible wrapper.
# Everything here uses only base/stats; the gg helpers REQUIRE ggplot2 attached
# (the apps do) and use UF_BLUE/UF_ORANGE (components.R) and BIG_ROWS
# (helpers_plot.R). bq()/bq_each()/qq() come from helpers_plot.R / helpers_lmer.R.

# --- spec-driven engine -----------------------------------------------------

#' Describe a regression to fit.
#'
#' A plain list consumed by reg_validate() / reg_fit(). Character/factor
#' predictors enter the model as factors; `ref_levels` optionally sets each
#' factor's reference level (the baseline the other levels are compared to).
#'
#' @param response Name of the numeric response (Y) column.
#' @param predictors Character vector of predictor (X) columns.
#' @param interactions Include all pairwise interactions (`a * b`)? Ignored with
#'   a single predictor or a polynomial term.
#' @param poly_degree If non-NULL, fit `poly(predictors[1], degree)` -- a curved
#'   fit on the first predictor (which must be numeric).
#' @param ref_levels Optional named list, column -> reference level.
#' @return A spec list.
#' @noRd
reg_spec <- function(response, predictors, interactions = FALSE,
                     poly_degree = NULL, ref_levels = NULL) {
  list(response = response, predictors = predictors,
       interactions = isTRUE(interactions),
       poly_degree = if (!is.null(poly_degree)) as.integer(poly_degree),
       ref_levels = ref_levels)
}

# Coerce character/factor predictors to factors, honouring any reference level.
# Numeric predictors pass through untouched.
reg_prep_data <- function(df, spec) {
  for (v in intersect(spec$predictors, names(df))) {
    x <- df[[v]]
    if (is.character(x) || is.factor(x)) {
      f   <- factor(x)
      ref <- spec$ref_levels[[v]]
      if (!is.null(ref) && ref %in% levels(f)) f <- stats::relevel(f, ref = ref)
      df[[v]] <- f
    }
  }
  df
}

# The model formula, backticked so Excel-style names with spaces parse.
reg_build_formula <- function(spec) {
  response   <- spec$response
  predictors <- spec$predictors
  rhs <- if (!is.null(spec$poly_degree))
           sprintf("poly(%s, %d, raw = TRUE)", bq(predictors[1]),
                   as.integer(spec$poly_degree))
         else if (isTRUE(spec$interactions) && length(predictors) > 1)
           paste(bq_each(predictors), collapse = " * ")
         else
           paste(bq_each(predictors), collapse = " + ")
  stats::as.formula(paste(bq(response), "~", rhs))
}

#' Validate a regression spec against the data.
#'
#' @param df A data frame.
#' @param spec A list from reg_spec().
#' @return Character vector of problems, empty when the spec is fittable.
#' @noRd
reg_validate <- function(df, spec) {
  response   <- spec$response
  predictors <- spec$predictors
  if (is.null(response) || !length(response) || !nzchar(response))
    return("Choose a response variable (Y).")
  if (is.null(predictors) || !length(predictors))
    return("Choose at least one predictor (X).")
  missing <- setdiff(c(response, predictors), names(df))
  if (length(missing))
    return(sprintf("Column(s) not found in data: %s",
                   paste(missing, collapse = ", ")))

  problems <- character(0)
  if (!is.numeric(df[[response]]))
    problems <- c(problems, sprintf(
      "The response '%s' must be numeric for linear regression.", response))
  if (response %in% predictors)
    problems <- c(problems, "The response can't also be a predictor.")
  if (!is.null(spec$poly_degree) &&
      !is.numeric(df[[predictors[1]]]))
    problems <- c(problems, sprintf(
      "A polynomial term needs a numeric predictor; '%s' isn't numeric.",
      predictors[1]))
  if (length(problems)) return(problems)

  # Degenerate-data guards on the rows the fit will actually use.
  model_df <- reg_prep_data(df, spec)
  vars <- intersect(c(response, predictors), names(model_df))
  cc   <- stats::complete.cases(model_df[, vars, drop = FALSE])
  used <- model_df[cc, , drop = FALSE]
  if (sum(cc) < length(predictors) + 2L)
    problems <- c(problems, paste(
      "Not enough complete rows to fit this model after dropping missing",
      "values. Add data or use fewer predictors."))
  for (v in predictors)
    if (is.factor(used[[v]]) && nlevels(droplevels(used[[v]])) < 2L)
      problems <- c(problems, sprintf(
        "Predictor '%s' has only one level among the usable rows, so it can't be estimated.",
        v))
  problems
}

#' Fit a regression from a spec. Assumes reg_validate() already passed.
#'
#' @param df A data frame.
#' @param spec A list from reg_spec().
#' @return An lm object.
#' @noRd
reg_fit <- function(df, spec) {
  stopifnot(is.data.frame(df))
  stats::lm(reg_build_formula(spec), data = reg_prep_data(df, spec))
}

#' Fit a linear / multiple / polynomial regression.
#'
#' Back-compatible wrapper over the reg_spec()/reg_fit() engine; kept because it
#' is exported and used by the report. `type` is retained for compatibility --
#' "linear" vs "multiple" differ only in the number of predictors.
#'
#' @param df A data frame.
#' @param response Name of the numeric response (Y) column.
#' @param predictors Character vector of predictor (X) columns. Polynomial uses
#'   only the first.
#' @param type One of "linear", "multiple", "polynomial".
#' @param degree Polynomial degree (used when type = "polynomial").
#' @return An lm object.
#' @export
fit_model <- function(df, response, predictors,
                      type = c("linear", "multiple", "polynomial"),
                      degree = 2) {
  type <- match.arg(type)
  stopifnot(is.data.frame(df), is.character(response), length(response) == 1L,
            is.character(predictors), length(predictors) >= 1L)
  missing <- setdiff(c(response, predictors), names(df))
  if (length(missing)) {
    stop("Columns not found in data: ", paste(missing, collapse = ", "))
  }
  spec <- reg_spec(response, predictors, interactions = FALSE,
                   poly_degree = if (type == "polynomial") degree else NULL)
  reg_fit(df, spec)
}

#' Pull the headline numbers and significance out of a fitted model.
#'
#' @param model An lm object.
#' @return A list: r2, adj_r2, overall_p (model F-test p), and the names of the
#'   significant / non-significant predictors at alpha = 0.05.
#' @export
model_interpretation <- function(model) {
  s     <- summary(model)
  fstat <- s$fstatistic
  overall_p <- if (!is.null(fstat))
    unname(stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE))
  else NA_real_
  coef_df   <- as.data.frame(s$coefficients)
  pred_rows <- coef_df[rownames(coef_df) != "(Intercept)", , drop = FALSE]
  pvals     <- pred_rows[, 4]
  list(
    r2             = round(s$r.squared, 3),
    adj_r2         = round(s$adj.r.squared, 3),
    overall_p      = overall_p,
    significant    = rownames(pred_rows)[pvals <  0.05],
    nonsignificant = rownames(pred_rows)[pvals >= 0.05]
  )
}

#' Tidy coefficient table with 95% confidence intervals.
#'
#' summary() gives estimate / SE / statistic / p; confint() adds the interval.
#' Hand-assembled (no broom) and rounded at source per the house convention.
#' The statistic column is named for the model family ("t value" for lm).
#'
#' @param model An lm (or glm) object.
#' @param digits Rounding for the numeric columns.
#' @return A data frame [Term, Estimate, Std. Error, <stat>, p, CI 2.5%,
#'   CI 97.5%], or NULL if `model` isn't a model.
#' @noRd
reg_coef_table <- function(model, digits = 4) {
  if (is.null(model) || !inherits(model, "lm")) return(NULL)
  co <- stats::coef(summary(model))
  stat_name <- colnames(co)[3]           # "t value" (lm) / "z value" (glm)
  out <- data.frame(
    Term         = rownames(co),
    Estimate     = round(co[, 1], digits),
    `Std. Error` = round(co[, 2], digits),
    check.names  = FALSE,
    row.names    = NULL,
    stringsAsFactors = FALSE)
  out[[stat_name]] <- round(co[, 3], digits)
  out[["p"]]       <- round(co[, 4], digits)
  ci <- tryCatch(suppressMessages(stats::confint(model)),
                 error = function(e) NULL, warning = function(w) NULL)
  if (!is.null(ci)) {
    m <- match(rownames(co), rownames(ci))
    out[["CI 2.5%"]]  <- round(ci[m, 1], digits)
    out[["CI 97.5%"]] <- round(ci[m, 2], digits)
  }
  out
}

#' Model fit statistics as a Statistic/Value table.
#'
#' @param model An lm object.
#' @param digits Rounding.
#' @return A data frame [Statistic, Value], or NULL.
#' @noRd
reg_fit_stats <- function(model, digits = 4) {
  if (is.null(model) || !inherits(model, "lm")) return(NULL)
  s    <- summary(model)
  res  <- stats::residuals(model)
  rmse <- sqrt(mean(res^2))
  fst  <- s$fstatistic
  fp   <- if (!is.null(fst))
    unname(stats::pf(fst[1], fst[2], fst[3], lower.tail = FALSE)) else NA_real_
  vals <- c(
    N                = length(res),
    `R-squared`      = round(s$r.squared, digits),
    `Adj. R-squared` = round(s$adj.r.squared, digits),
    RMSE             = round(rmse, digits),
    AIC              = round(stats::AIC(model), digits),
    BIC              = round(stats::BIC(model), digits),
    `F statistic`    = if (!is.null(fst)) round(unname(fst[1]), digits) else NA_real_,
    `F p-value`      = round(fp, digits))
  data.frame(Statistic = names(vals), Value = unname(vals),
             row.names = NULL, stringsAsFactors = FALSE)
}

#' Copy-ready code that reproduces the fitted model.
#'
#' Emits any factor coercions (with the reference level the model actually used,
#' read from `model$xlevels`) so the pasted script fits the identical model,
#' then the lm() call, summary, and confint. Derived from the fitted model, so
#' it is correct for factors, interactions and polynomial terms alike.
#'
#' @param model An lm object.
#' @return A character scalar, or NULL.
#' @noRd
reg_code <- function(model) {
  if (is.null(model) || !inherits(model, "lm")) return(NULL)
  f <- gsub("\\s+", " ", paste(deparse(stats::formula(model)), collapse = " "))
  lines <- c(
    "# Reproduce this regression on your own data frame `df`.",
    "# (Replace `df` with your data, e.g. df <- read.csv(\"your_data.csv\"))")
  xl <- model$xlevels          # named list: factor predictor -> level order
  if (length(xl)) {
    lines <- c(lines, "",
      "# Factor predictors, releveled to the reference the model used:")
    for (v in names(xl))
      lines <- c(lines, sprintf(
        "df[[%s]] <- relevel(factor(df[[%s]]), ref = %s)",
        qq(v), qq(v), qq(xl[[v]][1])))
  }
  lines <- c(lines, "",
    sprintf("model <- lm(%s, data = df)", f),
    "summary(model)",
    "confint(model)   # 95% confidence intervals")
  paste(lines, collapse = "\n")
}

# --- estimated marginal means (factor predictors) ---------------------------

#' Estimated marginal means + pairwise comparisons + connecting letters.
#'
#' The ANCOVA-style adjusted means for a categorical predictor, holding the
#' numeric predictors at their means. Reuses the same emmeans / multcomp engine
#' and the emm_* role helpers as the Mixed Model tool, so the result has the
#' identical shape (`$cld`, `$roles`, ...) and lmer_emm_plot() can draw it.
#' emmeans / multcomp are hard Imports, so no requireNamespace guard is needed.
#'
#' @param model An lm object.
#' @param emmvars Character vector of categorical (factor) predictors to
#'   estimate over. Must be factors in the model.
#' @param by Optional conditioning factor for simple effects (`~ A | B`).
#' @param adjust Multiplicity adjustment for pairwise tests and letters.
#' @param level Confidence level.
#' @return A list: ok / error, and on success emm, vars, roles, cld, pairs,
#'   held, plus main_vars / by_var / backtransformed for lmer_emm_plot().
#' @noRd
reg_emmeans <- function(model, emmvars, by = NULL, adjust = "tukey",
                        level = 0.95) {
  if (is.null(model) || !inherits(model, "lm"))
    return(list(ok = FALSE, error = "No fitted model."))
  factors <- names(model$xlevels)
  if (!length(factors))
    return(list(ok = FALSE, error = paste(
      "Estimated marginal means need a categorical predictor. Recast one to a",
      "factor (Import > Change Type) and refit.")))
  if (is.null(emmvars) || !length(emmvars))
    return(list(ok = FALSE,
                error = "Choose a categorical predictor to estimate means for."))
  if (!all(emmvars %in% factors))
    return(list(ok = FALSE, error = paste(
      "Your selection isn't a categorical predictor in this model. Refit, then",
      "pick a factor.")))

  roles <- emm_roles(emmvars, by)
  spec  <- emm_spec_formula(emmvars, by)
  emm <- tryCatch(emmeans::emmeans(model, spec, level = level),
                  error = function(e) e)
  if (inherits(emm, "error"))
    return(list(ok = FALSE,
                error = paste("EMMeans failed:", conditionMessage(emm))))

  cld_df <- tryCatch(
    as.data.frame(multcomp::cld(emm, Letters = letters, adjust = adjust,
                                reversed = TRUE)),
    error = function(e) e)
  if (inherits(cld_df, "error"))
    return(list(ok = FALSE,
                error = "Could not compute letter groupings for this specification."))
  if (".group" %in% names(cld_df)) cld_df$.group <- trimws(cld_df$.group)

  prs <- tryCatch(as.data.frame(graphics::pairs(emm, adjust = adjust)),
                  error = function(e)
                    data.frame(note = paste("Pairwise comparisons unavailable:",
                                            conditionMessage(e))))

  # Numeric predictors emmeans held at their mean, for an "adjusted means" note.
  mf   <- stats::model.frame(model)
  cand <- setdiff(names(mf), c(names(mf)[1], factors))
  held_vars <- cand[vapply(cand, function(v)
    is.numeric(mf[[v]]) && make.names(v) == v, logical(1))]
  held <- if (length(held_vars))
    paste(sprintf("%s = %.4g", held_vars,
                  vapply(held_vars, function(v) mean(mf[[v]], na.rm = TRUE),
                         numeric(1))), collapse = ", ") else NULL

  list(ok = TRUE, error = NULL, emm = emm, vars = emmvars, roles = roles,
       main_vars = roles$main, by_var = roles$by, cld = cld_df, pairs = prs,
       held = held, backtransformed = FALSE)
}

#' Copy-ready code for the EMMeans / post-hoc block.
#' @noRd
reg_emm_code <- function(emmvars, by = NULL, adjust = "tukey") {
  if (is.null(emmvars) || !length(emmvars)) return(NULL)
  paste(c(
    "library(emmeans)",
    "library(multcomp)",
    sprintf("emm <- emmeans(model, %s)", emm_spec_text(emmvars, by)),
    "summary(emm)                                  # estimated marginal means",
    sprintf("pairs(emm, adjust = %s)                       # pairwise comparisons",
            qq(adjust)),
    sprintf("cld(emm, Letters = letters, adjust = %s)      # connecting letters",
            qq(adjust))),
    collapse = "\n")
}

# Diagnostic-plot data is thinned to BIG_ROWS points (deterministically, so the
# view doesn't jump on re-render) to keep ggplotly snappy; the model is still
# fit on every row.
thin_rows <- function(d) {
  if (nrow(d) <= BIG_ROWS) return(d)
  d[unique(round(seq(1, nrow(d), length.out = BIG_ROWS))), , drop = FALSE]
}
thin_note <- function(n) {
  if (n > BIG_ROWS)
    sprintf("Showing ~%s of %s points for responsiveness",
            format(BIG_ROWS, big.mark = ","), format(n, big.mark = ","))
  else NULL
}

#' Fitted-vs-actual diagnostic ggplot. Requires ggplot2 attached.
#' @noRd
reg_fitted_gg <- function(model) {
  d <- data.frame(actual = model$model[[1]], fitted = stats::fitted(model))
  note <- thin_note(nrow(d)); d <- thin_rows(d)
  ggplot(d, aes(x = .data[["actual"]], y = .data[["fitted"]])) +
    geom_point(color = UF_BLUE, size = 2.5, alpha = 0.7) +
    geom_abline(color = UF_ORANGE, linetype = "dashed", linewidth = 1) +
    theme_minimal(base_size = 12) +
    labs(title = "Fitted vs Actual", subtitle = note, x = "Actual", y = "Fitted") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 9, color = "#777"))
}

#' Residuals-vs-fitted diagnostic ggplot. Requires ggplot2 attached.
#' @noRd
reg_resid_gg <- function(model) {
  d <- data.frame(fitted = stats::fitted(model), resid = stats::residuals(model))
  note <- thin_note(nrow(d)); d <- thin_rows(d)
  ggplot(d, aes(x = .data[["fitted"]], y = .data[["resid"]])) +
    geom_point(color = UF_BLUE, size = 2.5, alpha = 0.7) +
    geom_hline(yintercept = 0, color = UF_ORANGE, linetype = "dashed", linewidth = 1) +
    theme_minimal(base_size = 12) +
    labs(title = "Residuals vs Fitted", subtitle = note,
         x = "Fitted Values", y = "Residuals") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 9, color = "#777"))
}
