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
#' @param family "gaussian" (linear, lm) or "binomial" (logistic, glm).
#' @return A spec list.
#' @noRd
reg_spec <- function(response, predictors, interactions = FALSE,
                     poly_degree = NULL, ref_levels = NULL,
                     family = "gaussian") {
  list(response = response, predictors = predictors,
       interactions = isTRUE(interactions),
       poly_degree = if (!is.null(poly_degree)) as.integer(poly_degree),
       ref_levels = ref_levels,
       family = match.arg(family, c("gaussian", "binomial")))
}

# Is x a usable binary response for logistic regression? A 2-level factor, a
# logical, or a numeric taking only two distinct values (mapped to 0/1).
reg_binary_ok <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(FALSE)
  if (is.logical(x)) return(TRUE)
  if (is.factor(x) || is.character(x)) return(length(unique(as.character(x))) == 2L)
  if (is.numeric(x)) return(length(unique(x)) == 2L)
  FALSE
}

# Coerce a binary response to a 0/1 numeric (glm models P(the "high" level)).
# Factor/character -> second sorted level = 1; logical -> TRUE = 1; two-value
# numeric -> larger value = 1. The mapped-to-1 level is stored on the result.
reg_binary_response <- function(x) {
  if (is.logical(x)) {
    return(structure(as.integer(x), success = "TRUE"))
  }
  if (is.factor(x) || is.character(x)) {
    lv <- sort(unique(as.character(x[!is.na(x)])))
    return(structure(as.integer(as.character(x) == lv[2]), success = lv[2]))
  }
  u <- sort(unique(x[!is.na(x)]))
  structure(as.integer(x == u[2]), success = as.character(u[2]))
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
  binomial <- identical(spec$family, "binomial")
  if (binomial) {
    if (!reg_binary_ok(df[[response]]))
      problems <- c(problems, sprintf(paste(
        "Logistic regression needs a binary response; '%s' isn't (it must be a",
        "two-level factor, a logical, or a 0/1 numeric)."), response))
  } else if (!is.numeric(df[[response]])) {
    problems <- c(problems, sprintf(
      "The response '%s' must be numeric for linear regression.", response))
  }
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
#' Gaussian family -> lm(); binomial -> glm(family = binomial) after mapping the
#' binary response to 0/1. The "success" level modelled is recorded on the
#' returned object as attr(fit, "success") for display.
#'
#' @param df A data frame.
#' @param spec A list from reg_spec().
#' @return An lm object (gaussian) or a glm object (binomial).
#' @noRd
reg_fit <- function(df, spec) {
  stopifnot(is.data.frame(df))
  model_df <- reg_prep_data(df, spec)
  fml      <- reg_build_formula(spec)
  if (identical(spec$family, "binomial")) {
    y <- reg_binary_response(model_df[[spec$response]])
    model_df[[spec$response]] <- as.numeric(y)
    fit <- stats::glm(fml, data = model_df, family = stats::binomial())
    attr(fit, "success") <- attr(y, "success")
    fit
  } else {
    stats::lm(fml, data = model_df)
  }
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
#' Linear (lm): r2 / adj_r2 and the overall F-test p. Logistic (glm): r2 is
#' McFadden's pseudo-R2, adj_r2 is NA, and overall_p is the likelihood-ratio
#' test against the null model. `family` says which.
#'
#' @param model An lm or glm object.
#' @return A list: family, r2, adj_r2, overall_p, significant, nonsignificant.
#' @export
model_interpretation <- function(model) {
  s         <- summary(model)
  co        <- stats::coef(s)
  pred_rows <- co[rownames(co) != "(Intercept)", , drop = FALSE]
  pvals     <- pred_rows[, 4]
  sig    <- rownames(pred_rows)[pvals <  0.05]
  nonsig <- rownames(pred_rows)[pvals >= 0.05]

  if (inherits(model, "glm")) {
    mcf <- if (model$null.deviance == 0) NA_real_ else
      1 - model$deviance / model$null.deviance
    dev_diff <- model$null.deviance - model$deviance
    df_diff  <- model$df.null - model$df.residual
    overall_p <- if (df_diff > 0)
      stats::pchisq(dev_diff, df_diff, lower.tail = FALSE) else NA_real_
    return(list(family = "binomial", r2 = round(mcf, 3), adj_r2 = NA_real_,
                overall_p = overall_p, significant = sig,
                nonsignificant = nonsig))
  }

  fstat <- s$fstatistic
  overall_p <- if (!is.null(fstat))
    unname(stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE))
  else NA_real_
  list(family = "gaussian", r2 = round(s$r.squared, 3),
       adj_r2 = round(s$adj.r.squared, 3), overall_p = overall_p,
       significant = sig, nonsignificant = nonsig)
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
#' Linear (lm): n, R2, adj-R2, RMSE, AIC, BIC, overall F. Logistic (glm): n,
#' null/residual deviance, AIC, BIC, McFadden pseudo-R2, and classification
#' accuracy at a 0.5 cutoff.
#'
#' @param model An lm or glm object.
#' @param digits Rounding.
#' @return A data frame [Statistic, Value], or NULL.
#' @noRd
reg_fit_stats <- function(model, digits = 4) {
  if (is.null(model) || !inherits(model, "lm")) return(NULL)
  mk <- function(v) data.frame(Statistic = names(v), Value = unname(v),
                               row.names = NULL, stringsAsFactors = FALSE)
  if (inherits(model, "glm")) {
    y    <- model$y
    phat <- stats::fitted(model)
    acc  <- mean((phat >= 0.5) == (y == 1))
    # McFadden = 1 - logLik(model)/logLik(null). For ungrouped 0/1 data the
    # saturated logLik is 0, so logLik = -deviance/2 and this reduces to
    # 1 - deviance/null.deviance -- exact, and avoids update()'s data-scoping trap.
    mcf  <- if (model$null.deviance == 0) NA_real_ else
      1 - model$deviance / model$null.deviance
    return(mk(c(
      N                   = length(y),
      `Null deviance`     = round(model$null.deviance, digits),
      `Residual deviance` = round(model$deviance, digits),
      AIC                 = round(stats::AIC(model), digits),
      BIC                 = round(stats::BIC(model), digits),
      `McFadden R-sq`     = round(mcf, digits),
      `Accuracy (0.5)`    = round(acc, digits))))
  }
  s    <- summary(model)
  res  <- stats::residuals(model)
  rmse <- sqrt(mean(res^2))
  fst  <- s$fstatistic
  fp   <- if (!is.null(fst))
    unname(stats::pf(fst[1], fst[2], fst[3], lower.tail = FALSE)) else NA_real_
  mk(c(
    N                = length(res),
    `R-squared`      = round(s$r.squared, digits),
    `Adj. R-squared` = round(s$adj.r.squared, digits),
    RMSE             = round(rmse, digits),
    AIC              = round(stats::AIC(model), digits),
    BIC              = round(stats::BIC(model), digits),
    `F statistic`    = if (!is.null(fst)) round(unname(fst[1]), digits) else NA_real_,
    `F p-value`      = round(fp, digits)))
}

#' Odds ratios (exp of the coefficients) with 95% CI, for a logistic glm.
#'
#' @param model A binomial glm.
#' @return A data frame [Term, `Odds ratio`, CI 2.5%, CI 97.5%, p], or NULL if
#'   `model` isn't a logistic glm.
#' @noRd
reg_odds_ratios <- function(model, digits = 4) {
  if (is.null(model) || !inherits(model, "glm") ||
      !identical(model$family$family, "binomial")) return(NULL)
  co <- stats::coef(summary(model))
  or <- exp(co[, 1])
  ci <- tryCatch(suppressMessages(exp(stats::confint(model))),
                 error = function(e) NULL, warning = function(w) NULL)
  out <- data.frame(
    Term         = rownames(co),
    `Odds ratio` = round(or, digits),
    check.names  = FALSE, row.names = NULL, stringsAsFactors = FALSE)
  if (!is.null(ci)) {
    m <- match(rownames(co), rownames(ci))
    out[["CI 2.5%"]]  <- round(ci[m, 1], digits)
    out[["CI 97.5%"]] <- round(ci[m, 2], digits)
  }
  out[["p"]] <- round(co[, 4], digits)
  out
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
  is_logit <- inherits(model, "glm") &&
    identical(model$family$family, "binomial")
  if (is_logit) {
    lines <- c(lines, "",
      sprintf("model <- glm(%s, data = df, family = binomial)", f),
      "summary(model)",
      "exp(cbind(`odds ratio` = coef(model), confint(model)))  # ORs + 95% CI")
  } else {
    lines <- c(lines, "",
      sprintf("model <- lm(%s, data = df)", f),
      "summary(model)",
      "confint(model)   # 95% confidence intervals")
  }
  paste(lines, collapse = "\n")
}

#' Compare two nested regressions.
#'
#' A likelihood-ratio (glm) or nested F (lm) test plus an AIC/BIC delta,
#' mirroring lmer_compare(). Warns when the models aren't comparable (different
#' response, family, or number of rows) -- the test is only valid for nested
#' models fit on identical data. B is the current model, A the saved one.
#'
#' @param fitA,fitB Two lm or two glm objects (B current, A saved).
#' @return A list: warnings (character), table (data frame or NULL), error.
#' @noRd
reg_compare <- function(fitA, fitB) {
  if (is.null(fitA) || is.null(fitB))
    return(list(warnings = "Save a model (A), then fit another (B) to compare.",
                table = NULL, error = NULL))
  msgs <- character(0)
  glmA <- inherits(fitA, "glm"); glmB <- inherits(fitB, "glm")
  if (glmA != glmB)
    return(list(warnings = paste(
      "Model A and B are different kinds (one linear, one logistic) and can't",
      "be compared."), table = NULL, error = NULL))
  respA <- as.character(stats::formula(fitA))[2]
  respB <- as.character(stats::formula(fitB))[2]
  if (!identical(respA, respB))
    msgs <- c(msgs, paste(
      "WARNING: the two models use different responses, so their likelihoods",
      "and AIC/BIC are not comparable."))
  if (stats::nobs(fitA) != stats::nobs(fitB))
    msgs <- c(msgs, sprintf(paste(
      "WARNING: the models were fit on different numbers of rows (A: %d, B: %d),",
      "usually different missing-value patterns. A valid test needs identical rows."),
      stats::nobs(fitA), stats::nobs(fitB)))
  msgs <- c(msgs, paste(
    "Valid only if one model is nested within the other and both use the same rows."))

  an <- tryCatch(
    if (glmB) stats::anova(fitA, fitB, test = "LRT") else stats::anova(fitA, fitB),
    error = function(e) e)
  if (inherits(an, "error"))
    return(list(warnings = msgs, table = NULL, error = conditionMessage(an)))

  tab <- as.data.frame(an)
  tab <- cbind(Model = c("A", "B"), round_df(tab))
  aic <- round(c(stats::AIC(fitA), stats::AIC(fitB)), 3)
  bic <- round(c(stats::BIC(fitA), stats::BIC(fitB)), 3)
  tab$AIC <- aic; tab$BIC <- bic
  list(warnings = msgs, table = tab, error = NULL,
       aic_delta = round(aic[2] - aic[1], 3),
       bic_delta = round(bic[2] - bic[1], 3))
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

# Shared theme for the extra diagnostics.
.reg_diag_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 9, color = "#777"))
}

#' Normal Q-Q plot of the standardized residuals. Requires ggplot2 attached.
#' @noRd
reg_qq_gg <- function(model) {
  d <- data.frame(sample = stats::rstandard(model))
  note <- thin_note(nrow(d)); d <- thin_rows(d)
  ggplot(d, aes(sample = .data[["sample"]])) +
    stat_qq(color = UF_BLUE, size = 2, alpha = 0.7) +
    stat_qq_line(color = UF_ORANGE, linetype = "dashed", linewidth = 1) +
    labs(title = "Normal Q-Q", subtitle = note,
         x = "Theoretical quantiles", y = "Standardized residuals") +
    .reg_diag_theme()
}

#' Scale-location plot: sqrt(|standardized residuals|) vs fitted -- a flat cloud
#' means constant variance. Requires ggplot2 attached.
#' @noRd
reg_scale_loc_gg <- function(model) {
  d <- data.frame(fitted = stats::fitted(model),
                  sl = sqrt(abs(stats::rstandard(model))))
  note <- thin_note(nrow(d)); d <- thin_rows(d)
  ggplot(d, aes(x = .data[["fitted"]], y = .data[["sl"]])) +
    geom_point(color = UF_BLUE, size = 2, alpha = 0.7) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                color = UF_ORANGE, linewidth = 1) +
    labs(title = "Scale-Location", subtitle = note, x = "Fitted values",
         y = "sqrt(|Std. residuals|)") +
    .reg_diag_theme()
}

#' Cook's distance per observation, with the 4/n rule-of-thumb line. Not thinned
#' -- the whole point is the influential points. Requires ggplot2 attached.
#' @noRd
reg_cooks_gg <- function(model) {
  ck  <- stats::cooks.distance(model)
  d   <- data.frame(obs = seq_along(ck), cooks = unname(ck))
  thr <- 4 / length(ck)
  ggplot(d, aes(x = .data[["obs"]], y = .data[["cooks"]])) +
    geom_segment(aes(xend = .data[["obs"]], yend = 0), color = UF_BLUE) +
    geom_hline(yintercept = thr, color = UF_ORANGE, linetype = "dashed",
               linewidth = 1) +
    labs(title = "Cook's distance",
         subtitle = sprintf("Points above %.3g (4/n) are influential", thr),
         x = "Observation", y = "Cook's distance") +
    .reg_diag_theme()
}

# --- assumptions + multicollinearity ----------------------------------------

#' Assumption checks for a linear model.
#'
#' A verdict table like mod_compare's: normality of residuals (Shapiro-Wilk),
#' constant variance (Breusch-Pagan against the fitted values), linearity (a
#' RESET-style test for leftover curvature -- residuals carry a quadratic-of-
#' fitted term only if the mean structure is misspecified), and independence
#' (Durbin-Watson; ORDER-DEPENDENT, only meaningful if the rows are in time
#' order). All hand-rolled, base/stats only.
#'
#' @param model An lm object.
#' @return A data frame [Assumption, Test, Statistic, p_value, OK], or NULL.
#'   OK is TRUE (met), FALSE (violated) or NA (not assessable). Independence
#'   carries an order-dependent caveat in `attr(x, "independence_note")`.
#' @noRd
reg_assumptions <- function(model) {
  # Linear-model assumptions only; logistic residuals don't work this way.
  if (is.null(model) || !inherits(model, "lm") || inherits(model, "glm"))
    return(NULL)
  r <- stats::residuals(model); f <- stats::fitted(model); n <- length(r)
  row <- function(a, t, s, p, ok)
    data.frame(Assumption = a, Test = t, Statistic = s, p_value = p, OK = ok,
               stringsAsFactors = FALSE)
  na_row <- function(a, t) row(a, t, NA_real_, NA_real_, NA)

  # Normality of residuals
  norm_row <- if (n >= 3L && n <= 5000L && stats::sd(r) > 0) {
    sw <- stats::shapiro.test(r)
    row("Normality of residuals", "Shapiro-Wilk",
        round(unname(sw$statistic), 4), sw$p.value, sw$p.value >= 0.05)
  } else na_row("Normality of residuals", "Shapiro-Wilk")

  # Constant variance: Breusch-Pagan against fitted (LM = n * R^2 ~ chisq_1)
  var_row <- tryCatch({
    stat <- n * summary(stats::lm(I(r^2) ~ f))$r.squared
    p    <- stats::pchisq(stat, df = 1, lower.tail = FALSE)
    row("Constant variance", "Breusch-Pagan", round(stat, 4), p, p >= 0.05)
  }, error = function(e) na_row("Constant variance", "Breusch-Pagan"))

  # Linearity: a quadratic-of-fitted term in the residuals signals curvature.
  lin_row <- tryCatch({
    cf <- stats::coef(summary(stats::lm(r ~ f + I(f^2))))
    p  <- cf["I(f^2)", 4]
    row("Linearity", "Quadratic residual test", round(cf["I(f^2)", 3], 4),
        p, p >= 0.05)
  }, error = function(e) na_row("Linearity", "Quadratic residual test"))

  # Independence: Durbin-Watson (~2 = no serial correlation). No exact p here;
  # flag on distance from 2. Only meaningful if rows are ordered in time.
  dw <- sum(diff(r)^2) / sum(r^2)
  ind_row <- row("Independence (row order)", "Durbin-Watson",
                 round(dw, 4), NA_real_, abs(dw - 2) < 1)

  res <- rbind(norm_row, var_row, lin_row, ind_row)
  attr(res, "independence_note") <- paste(
    "Durbin-Watson only detects serial correlation if the rows are in time",
    "order; otherwise ignore it.")
  res
}

#' Variance inflation factors (multicollinearity).
#'
#' Hand-rolled 1/(1 - R^2_j) by regressing each model-matrix column on the
#' others -- identical to car::vif for numeric predictors, and no `car`
#' dependency. For factor / interaction terms these are per-dummy VIFs (car's
#' GVIF would pool them); still a useful collinearity signal. NULL with < 2
#' predictor columns (VIF is undefined).
#'
#' @param model An lm object.
#' @return A data frame [Term, VIF, Concern] (low/moderate/high at 5/10), or NULL.
#' @noRd
reg_vif <- function(model) {
  if (is.null(model) || !inherits(model, "lm")) return(NULL)
  X <- stats::model.matrix(model)
  X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  if (ncol(X) < 2L) return(NULL)
  vif <- vapply(seq_len(ncol(X)), function(j) {
    r2 <- tryCatch(summary(stats::lm(X[, j] ~ X[, -j, drop = FALSE]))$r.squared,
                   error = function(e) NA_real_)
    if (is.na(r2) || r2 >= 1) NA_real_ else 1 / (1 - r2)
  }, numeric(1))
  data.frame(Term = colnames(X), VIF = round(vif, 3),
             Concern = ifelse(is.na(vif), "n/a",
                       ifelse(vif > 10, "high",
                       ifelse(vif > 5, "moderate", "low"))),
             stringsAsFactors = FALSE)
}
