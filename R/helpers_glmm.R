# ===========================================================================
#  helpers_glmm.R -- pure helpers for the GLMM Review app
# ---------------------------------------------------------------------------
#  Generalized linear mixed-model engine (glmmTMB / DHARMa / emmeans),
#  absorbed from the standalone "Quick GLMM Review" app. Everything here is
#  free of Shiny reactivity: functions take data + a plain spec list and
#  return data / structured lists, so they are unit-testable on their own.
#
#  Reuses the lmer kit where the two engines overlap: bq()/bq_each(),
#  build_random_part(), emm_spec_formula()/emm_roles(), is_categorical(),
#  round_df(), make_combined_factor(), MAX_FIXED, snapshot_rng(). What is
#  genuinely GLMM-specific lives here: the family registry + domain check,
#  the ziformula/dispformula side-model builder, the glmmTMB fit wrapper,
#  DHARMa simulation-based diagnostics, and the code generators.
# ===========================================================================

# ---------------------------------------------------------------------------
#  Family registry: UI label -> key -> family object / valid-domain note
# ---------------------------------------------------------------------------

# Label shown in the UI -> internal key. Keys encode distribution + link.
GLMM_FAMILY_CHOICES <- c(
  "Gaussian (identity link)"                                        = "gaussian_identity",
  "Gaussian (log link)"                                             = "gaussian_log",
  "Gamma (log link) \u2013 right-skewed positive"                   = "Gamma_log",
  "Poisson (log link) \u2013 counts"                                = "poisson_log",
  "Negative binomial, nbinom2 (log link) \u2013 overdispersed counts" = "nbinom2_log",
  "Negative binomial, nbinom1 (log link)"                           = "nbinom1_log",
  "Tweedie (log link) \u2013 zero-heavy continuous"                 = "tweedie_log",
  "Beta (logit link) \u2013 proportions in (0,1)"                   = "beta_logit")

# Link choices for the binary (Bernoulli) tab, where the family is fixed.
GLMM_BINARY_LINKS <- c("logit" = "logit", "probit" = "probit",
                       "cloglog" = "cloglog", "cauchit" = "cauchit")

# Plain-language valid-domain note per family, shown live under the family
# picker so a mismatched response (e.g. negative numbers with Gamma, or
# proportions with Poisson) is caught before fitting rather than after.
GLMM_FAMILY_NOTE <- c(
  "gaussian_identity" = "Any real number \u2014 positive, negative, or zero.",
  "gaussian_log"      = paste0(
    "Response should be positive \u2014 the log link keeps the modelled mean ",
    "above 0, so data at or below 0 usually signals the wrong family."),
  "Gamma_log"         = paste0(
    "Response must be strictly greater than 0. Gamma is undefined at exactly ",
    "0 \u2014 if your data includes true zeros, this is the wrong family."),
  "poisson_log"       = paste0(
    "Non-negative integer counts: 0, 1, 2, 3, ... No negative values, no ",
    "decimals."),
  "nbinom2_log"       = paste0(
    "Non-negative integer counts (0, 1, 2, ...). Use this over Poisson when ",
    "counts are overdispersed (variance > mean)."),
  "nbinom1_log"       = paste0(
    "Non-negative integer counts (0, 1, 2, ...). An alternative ",
    "overdispersion structure to nbinom2 (variance scales linearly with the ",
    "mean)."),
  "tweedie_log"       = paste0(
    "Non-negative continuous values, and CAN include exact zeros (e.g. ",
    "rainfall, biomass with true absences)."),
  "beta_logit"        = paste0(
    "Strictly between 0 and 1 \u2014 EXCLUDES exact 0 and exact 1. Rescale ",
    "data that touches the boundaries, e.g. y' = (y*(n-1)+0.5)/n."))

# Family key -> the glmmTMB/stats family object used for the fit.
glmm_family <- function(key) {
  switch(key,
         "gaussian_identity" = stats::gaussian(link = "identity"),
         "gaussian_log"      = stats::gaussian(link = "log"),
         "Gamma_log"         = stats::Gamma(link = "log"),
         "poisson_log"       = stats::poisson(link = "log"),
         "nbinom2_log"       = glmmTMB::nbinom2(link = "log"),
         "nbinom1_log"       = glmmTMB::nbinom1(link = "log"),
         "tweedie_log"       = glmmTMB::tweedie(link = "log"),
         "beta_logit"        = glmmTMB::beta_family(link = "logit"),
         stats::gaussian())
}

# Family key -> the constructor text emitted in the reproducible code
# (plain names: the emitted script starts with library(glmmTMB)).
glmm_family_code <- function(key) {
  switch(key,
         "gaussian_identity" = "gaussian(link = \"identity\")",
         "gaussian_log"      = "gaussian(link = \"log\")",
         "Gamma_log"         = "Gamma(link = \"log\")",
         "poisson_log"       = "poisson(link = \"log\")",
         "nbinom2_log"       = "nbinom2(link = \"log\")",
         "nbinom1_log"       = "nbinom1(link = \"log\")",
         "tweedie_log"       = "tweedie(link = \"log\")",
         "beta_logit"        = "beta_family(link = \"logit\")",
         "gaussian()")
}

# Advisory check of a response vector against a family's valid domain.
# Returns NULL when the data fits the family, otherwise a plain-language
# warning string. Advisory only -- it never blocks the fit (the note is
# shown pre-fit in the UI and folded into the fit notes), because some
# violations still fit (log link constrains the mean, not the data) while
# others fail with a cryptic glmmTMB error this message then explains.
# `key` is a GLMM_FAMILY_CHOICES key, or "binary" for the 0/1 tab.
glmm_domain_check <- function(x, key) {
  if (!is.numeric(x)) return(NULL)
  x <- x[is.finite(x)]
  if (!length(x)) return(NULL)
  bad <- switch(key,
                "binary"       = any(!x %in% c(0, 1)),
                "gaussian_log" = ,
                "Gamma_log"    = any(x <= 0),
                "poisson_log"  = ,
                "nbinom2_log"  = ,
                "nbinom1_log"  = any(x < 0 | x != round(x)),
                "tweedie_log"  = any(x < 0),
                "beta_logit"   = any(x <= 0 | x >= 1),
                FALSE)
  if (!isTRUE(bad)) return(NULL)
  if (identical(key, "binary"))
    return(paste0("The response contains values other than 0 and 1 \u2014 ",
                  "a binary model needs exactly 0/1 (or a two-level factor)."))
  paste0("The response contains values outside this family's valid range \u2014 ",
         "check the note under the family picker before fitting.")
}

# ---------------------------------------------------------------------------
#  Formula construction
# ---------------------------------------------------------------------------

# Zero-inflation / dispersion side formulas: character vector of predictor
# names -> "~0" (off), "~1" (intercept only), or "~`v1` + `v2`".
build_side_formula <- function(vars, enabled = TRUE) {
  if (!isTRUE(enabled)) return("~0")
  vars <- vars[nzchar(vars %||% character(0))]
  if (length(vars) == 0) return("~1")
  paste("~", paste(bq_each(vars), collapse = " + "))
}

# Main conditional (fixed + random) formula, e.g.  y ~ A * B + (1 | Block).
# Unlike the lmer builder there is no response transform (the family/link
# plays that role) and the random part may be absent entirely -- glmmTMB
# then fits an ordinary GLM (the fit wrapper notes this).
glmm_formula_string <- function(response, fixed, random, interactions = FALSE,
                                slope = NULL, slope_group = NULL) {
  fixed_part <- if (length(fixed) == 0) {
    "1"
  } else if (isTRUE(interactions) && length(fixed) > 1) {
    paste(bq_each(fixed), collapse = " * ")
  } else {
    paste(bq_each(fixed), collapse = " + ")
  }
  random_part <- build_random_part(random, slope, slope_group)
  rhs <- paste(c(fixed_part, random_part), collapse = " + ")
  paste(bq(response), "~", rhs)
}

# ---------------------------------------------------------------------------
#  Fit
# ---------------------------------------------------------------------------

# Fit a glmmTMB model from a spec list. Returns a structured list; on failure
# `ok = FALSE` with an `error` message instead of throwing. Spec fields:
#   response      response column name (required)
#   fixed, random character vectors (either may be empty, not both -- the
#                 caller guards; an empty spec still fits an intercept model)
#   interactions  cross the fixed effects with `*`
#   slope, slope_group   optional random slope (see build_random_part)
#   binary        TRUE for the 0/1 tab: family is binomial(link), a two-level
#                 factor response is recoded 2nd level = 1 ("success")
#   link          binary tab link key (GLMM_BINARY_LINKS)
#   family_key    general tab family key (GLMM_FAMILY_CHOICES)
#   zi_on, zi_vars       zero-inflation model -> ziformula ~0 / ~1 / ~vars
#   disp_vars     dispersion model predictors -> dispformula ~1 / ~vars
#                 (ignored on the binary tab: Bernoulli data has no free
#                 dispersion parameter)
glmm_fit <- function(df, spec) {
  response <- spec$response
  if (is.null(response) || !response %in% names(df))
    return(list(ok = FALSE, mod = NULL, error = "Choose a response variable."))
  fixed  <- spec$fixed  %||% character(0)
  random <- spec$random %||% character(0)
  binary <- isTRUE(spec$binary)

  model_df <- df
  for (g in random) model_df[[g]] <- factor(model_df[[g]])

  notes <- character(0)
  success_level <- NULL
  if (binary) {
    y <- model_df[[response]]
    if (is.factor(y) || is.character(y)) {
      y <- factor(y)
      if (nlevels(y) != 2)
        return(list(ok = FALSE, mod = NULL, error = sprintf(
          "'%s' has %d levels \u2014 a binary response needs exactly two.",
          response, nlevels(y))))
      success_level <- levels(y)[2]
      notes <- c(notes, sprintf("Modeling P(%s = \"%s\").",
                                response, success_level))
      model_df[[response]] <- as.integer(y) - 1L
    }
    dom <- glmm_domain_check(model_df[[response]], "binary")
    if (!is.null(dom))
      return(list(ok = FALSE, mod = NULL, error = dom))
  } else {
    dom <- glmm_domain_check(model_df[[response]],
                             spec$family_key %||% "gaussian_identity")
    if (!is.null(dom)) notes <- c(notes, dom)
  }

  if (length(random) == 0)
    notes <- c(notes, paste0(
      "No random effect selected \u2014 glmmTMB fits an ordinary GLM ",
      "(no grouping structure)."))

  use_slope <- if (!is.null(spec$slope) && nzchar(spec$slope)) spec$slope else NULL
  use_slope_group <- if (!is.null(use_slope)) {
    g <- intersect(spec$slope_group %||% character(0), random)
    if (length(g)) g else random
  } else NULL

  fml_str  <- glmm_formula_string(response, fixed, random,
                                  isTRUE(spec$interactions),
                                  use_slope, use_slope_group)
  zi_str   <- build_side_formula(spec$zi_vars %||% character(0),
                                 isTRUE(spec$zi_on))
  disp_str <- if (binary) "~1"
              else build_side_formula(spec$disp_vars %||% character(0), TRUE)

  fam <- if (binary) stats::binomial(link = spec$link %||% "logit")
         else glmm_family(spec$family_key %||% "gaussian_identity")

  fit_warnings <- character(0)
  mod <- tryCatch(
    withCallingHandlers(
      glmmTMB::glmmTMB(formula     = stats::as.formula(fml_str),
                       ziformula   = stats::as.formula(zi_str),
                       dispformula = stats::as.formula(disp_str),
                       family      = fam, data = model_df,
                       na.action   = stats::na.omit),
      warning = function(w) {
        fit_warnings <<- c(fit_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }),
    error = function(e) e)
  if (inherits(mod, "error"))
    return(list(ok = FALSE, mod = NULL,
                error = paste("Model failed to fit:", conditionMessage(mod))))

  n_drop <- nrow(model_df) - stats::nobs(mod)
  if (n_drop > 0)
    notes <- c(notes, sprintf(
      "%d of %d row(s) were dropped for missing values in the model variables (listwise deletion). The model used %d row(s).",
      n_drop, nrow(model_df), stats::nobs(mod)))

  fit_warnings <- unique(fit_warnings[nzchar(fit_warnings)])
  if (length(fit_warnings))
    notes <- c(notes, paste("Fit warning:", fit_warnings))
  pd <- tryCatch(isTRUE(mod$sdr$pdHess), error = function(e) TRUE)
  if (!pd)
    notes <- c(notes, paste0(
      "Convergence problem: non-positive-definite Hessian. The model is ",
      "likely over-parameterized for the data \u2014 try dropping the random ",
      "slope, the zero-inflation/dispersion terms, or a sparse factor. ",
      "Treat all estimates with caution."))

  list(ok = TRUE, mod = mod, error = NULL, notes = notes,
       fml_str = fml_str, zi_str = zi_str, disp_str = disp_str,
       family_key = if (binary) "binary" else spec$family_key %||% "gaussian_identity",
       link = if (binary) spec$link %||% "logit" else NULL,
       binary = binary, success_level = success_level,
       response = response, fixed = fixed, random = random,
       slope = use_slope, slope_group = use_slope_group,
       cat_fixed = fixed[is_categorical(df, fixed)],
       spec = spec, data = model_df)
}

# Human-readable family text for display / emitted code, e.g.
# 'binomial(link = "probit")' or 'nbinom2(link = "log")'.
glmm_family_text <- function(fit) {
  if (isTRUE(fit$binary))
    sprintf("binomial(link = \"%s\")", fit$link %||% "logit")
  else
    glmm_family_code(fit$family_key)
}

# ---------------------------------------------------------------------------
#  Fit statistics / ANOVA / dispersion cross-check
# ---------------------------------------------------------------------------

# One-row AIC/BIC/logLik/df table for the fitted model.
glmm_fit_stats <- function(model, n_total = NULL) {
  n_used <- stats::nobs(model)
  out <- data.frame(
    AIC        = round(stats::AIC(model), 2),
    BIC        = round(stats::BIC(model), 2),
    logLik     = round(as.numeric(stats::logLik(model)), 2),
    df_resid   = stats::df.residual(model),
    n_obs_used = n_used)
  if (!is.null(n_total)) {
    out$n_obs_total  <- n_total
    out$n_dropped_NA <- n_total - n_used
  }
  out
}

# Type III Wald chi-square tests via car::Anova (Suggests). Returns
# list(ok, table, note): when car is missing, ok = FALSE with an
# instructive note instead of an error; likewise on Anova failure.
glmm_anova <- function(model) {
  if (!requireNamespace("car", quietly = TRUE))
    return(list(ok = FALSE, table = NULL, note = paste0(
      "Package 'car' is not installed \u2014 install it ",
      "(install.packages(\"car\")) for Type III Wald chi-square tests.")))
  res <- tryCatch(car::Anova(model, type = 3), error = function(e) e)
  if (inherits(res, "error"))
    return(list(ok = FALSE, table = NULL,
                note = paste("ANOVA failed:", conditionMessage(res))))
  tab <- as.data.frame(res)
  tab <- cbind(Term = rownames(tab), tab)
  rownames(tab) <- NULL
  list(ok = TRUE, table = round_df(tab), note = NULL)
}

# Classic Pearson chi-square / residual-df ratio -- a quick overdispersion
# cross-check alongside DHARMa's simulation-based test. ~1 is fine; >>1
# suggests overdispersion; <<1 underdispersion. NA when unavailable
# (e.g. families without Pearson residuals).
glmm_pearson_ratio <- function(model) {
  tryCatch({
    pr  <- stats::residuals(model, type = "pearson")
    rdf <- stats::df.residual(model)
    if (!is.finite(rdf) || rdf <= 0) return(NA_real_)
    round(sum(pr^2, na.rm = TRUE) / rdf, 3)
  }, error = function(e) NA_real_)
}

# ---------------------------------------------------------------------------
#  DHARMa simulation-based residual diagnostics
# ---------------------------------------------------------------------------

# Simulate DHARMa residuals for a fitted glmmTMB model. Returns the DHARMa
# object, or list(ok = FALSE, error =) on failure. Seeded internally by
# DHARMa (simulateResiduals sets its own seed default) -- we snapshot the
# session RNG so the app never hijacks the user's random state.
glmm_dharma <- function(model, n = 250) {
  if (!requireNamespace("DHARMa", quietly = TRUE))
    return(list(ok = FALSE, error = paste0(
      "Package 'DHARMa' is not installed \u2014 simulation-based residual ",
      "diagnostics are unavailable.")))
  restore_rng <- snapshot_rng()
  on.exit(restore_rng(), add = TRUE)
  sim <- tryCatch(DHARMa::simulateResiduals(model, n = n),
                  error = function(e) e)
  if (inherits(sim, "error"))
    return(list(ok = FALSE,
                error = paste("DHARMa simulation failed:",
                              conditionMessage(sim))))
  list(ok = TRUE, sim = sim, error = NULL)
}

# Run the standard DHARMa test battery on a simulation. Returns a list of
# htest-like objects (or error strings) for printing: dispersion, outliers,
# and -- for non-binary fits -- zero inflation. plot = FALSE throughout:
# DHARMa's tests draw by default, and a pure helper must not open a graphics
# device (the module plots the simulation separately). RNG-snapshotted like
# glmm_dharma because the tests simulate internally too.
glmm_dharma_tests <- function(sim, binary = FALSE) {
  restore_rng <- snapshot_rng()
  on.exit(restore_rng(), add = TRUE)
  run <- function(f) tryCatch(f(sim, plot = FALSE), error = function(e)
    paste("Test unavailable:", conditionMessage(e)))
  out <- list(dispersion = run(DHARMa::testDispersion),
              outliers   = run(DHARMa::testOutliers))
  if (!isTRUE(binary)) out$zero_inflation <- run(DHARMa::testZeroInflation)
  out
}

# ---------------------------------------------------------------------------
#  EMMeans / post-hoc (response scale)
# ---------------------------------------------------------------------------

# Estimated marginal means + compact-letter display + pairwise comparisons
# for a glmm_fit() result, always on the response scale (counts,
# probabilities, proportions -- the scale the user's data lives on; pairwise
# contrasts of a log/logit-link model then come out as ratios / odds
# ratios). Honours an optional `by` factor (~ A | B simple effects).
# Reuses the lmer kit's emm_spec_formula()/emm_roles().
glmm_emmeans <- function(fit, emmvars, by = NULL,
                         adjust = "tukey", level = 0.95) {
  if (is.null(fit) || !isTRUE(fit$ok))
    return(list(ok = FALSE, error = "No fitted model."))
  if (length(emmvars) < 1)
    return(list(ok = FALSE, error =
      "Select one or more categorical fixed effects for EMMeans."))
  if (!all(emmvars %in% fit$fixed))
    return(list(ok = FALSE, error = paste0(
      "Your EMMeans selection no longer matches the fitted model. ",
      "Fit the model again.")))

  roles <- emm_roles(emmvars, by)
  spec  <- emm_spec_formula(emmvars, by)

  emm <- tryCatch(
    emmeans::emmeans(fit$mod, spec, type = "response", level = level),
    error = function(e) e)
  if (inherits(emm, "error"))
    return(list(ok = FALSE,
                error = paste("EMMeans failed:", conditionMessage(emm))))

  cld_df <- tryCatch(
    as.data.frame(multcomp::cld(emm, Letters = letters,
                                adjust = adjust, reversed = TRUE)),
    error = function(e) e)
  if (inherits(cld_df, "error"))
    return(list(ok = FALSE, error =
      "Could not compute letter groupings for this specification."))
  if (".group" %in% names(cld_df)) cld_df$.group <- trimws(cld_df$.group)

  prs <- tryCatch(as.data.frame(graphics::pairs(emm, adjust = adjust)),
                  error = function(e)
                    data.frame(note = paste("Pairwise comparisons unavailable:",
                                            conditionMessage(e))))

  used <- fit$data
  mv   <- unique(c(fit$response, fit$fixed, fit$random,
                   if (!is.null(fit$slope)) fit$slope))
  mv   <- intersect(mv, names(used))
  used <- used[stats::complete.cases(used[, mv, drop = FALSE]), , drop = FALSE]
  cnt  <- tryCatch(as.data.frame(table(used[emmvars]), responseName = "n"),
                   error = function(e) NULL)
  if (!is.null(cnt)) names(cnt)[seq_along(emmvars)] <- emmvars

  list(ok = TRUE, error = NULL, emm = emm, emm_df = as.data.frame(emm),
       vars = emmvars, roles = roles, main_vars = roles$main,
       by_var = roles$by, cld = cld_df, pairs = prs, counts = cnt)
}

# ---------------------------------------------------------------------------
#  Reproducible-code generators
# ---------------------------------------------------------------------------

# Standalone script reproducing the fitted model + its diagnostics.
glmm_code <- function(fit) {
  if (is.null(fit) || !isTRUE(fit$ok)) return("")
  paste0(
    "library(glmmTMB); library(DHARMa); library(emmeans)\n\n",
    "mod <- glmmTMB(\n",
    "  ", fit$fml_str, ",\n",
    "  ziformula   = ", fit$zi_str, ",\n",
    "  dispformula = ", fit$disp_str, ",\n",
    "  family      = ", glmm_family_text(fit), ",\n",
    "  data = your_data)\n\n",
    "summary(mod)\n\n",
    glmm_dharma_code(fit$binary),
    "\n# Type III Wald chi-square tests\ncar::Anova(mod, type = 3)\n")
}

# The DHARMa diagnostic block of the emitted script.
glmm_dharma_code <- function(binary = FALSE) {
  paste0(
    "# quick manual overdispersion check (Pearson chi-sq / residual df; ~1 is fine)\n",
    "sum(residuals(mod, type = \"pearson\")^2) / df.residual(mod)\n\n",
    "sim <- simulateResiduals(mod, n = 250, plot = TRUE)\n",
    "testDispersion(sim)\n",
    if (!isTRUE(binary)) "testZeroInflation(sim)\n" else "",
    "testOutliers(sim)\n")
}

# The EMMeans block of the emitted script, parameterized on the actual
# selection so the pasted code reproduces what the app showed.
glmm_emm_code <- function(emmvars, by = NULL, adjust = "tukey",
                          level = 0.95) {
  if (!length(emmvars)) return("")
  spec <- emm_spec_text(emmvars, by)
  paste0(
    "emm <- emmeans(mod, ", spec, ", type = \"response\", level = ",
    format(level), ")\n",
    "emm\n",
    "pairs(emm, adjust = \"", adjust, "\")\n",
    "multcomp::cld(emm, adjust = \"", adjust,
    "\", Letters = letters, reversed = TRUE)\n")
}

# ---------------------------------------------------------------------------
#  Built-in example data
# ---------------------------------------------------------------------------

#' Generate the built-in GLMM example dataset
#'
#' A seeded field-style dataset used as the demo for the GLMM Review app.
#' Eight sites (the random / grouping factor) crossed with a three-level
#' Treatment and a two-level Season, three replicates per cell. Four
#' responses each exercise a different family: \code{insect_count}
#' (overdispersed counts; negative binomial), \code{seedling_count}
#' (zero-heavy counts; a zero-inflation candidate), \code{cover_prop}
#' (a proportion strictly inside (0,1); Beta), and \code{present}
#' (0/1 presence; the binary tab).
#'
#' @return A data frame with Site, Treatment, Season and the four response
#'   columns (144 rows).
#' @examples
#' d <- make_glmm_example_data()
#' nrow(d)
#' @export
make_glmm_example_data <- function() {
  restore_rng <- snapshot_rng()   # fixed seed must NOT hijack the session RNG
  on.exit(restore_rng(), add = TRUE)
  set.seed(2024)
  d <- expand.grid(Site = factor(1:8),
                   Treatment = c("Control", "Fertilized", "Grazed"),
                   Season = c("Spring", "Fall"),
                   KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  d <- d[rep(seq_len(nrow(d)), each = 3), ]
  rownames(d) <- NULL
  n <- nrow(d)
  site_eff <- stats::setNames(stats::rnorm(8, 0, 0.4), levels(d$Site))
  b    <- unname(site_eff[as.character(d$Site)])
  trtC <- unname(c(Control = 0, Fertilized = 0.6, Grazed = -0.4)[d$Treatment])
  seaC <- unname(c(Spring = 0, Fall = -0.3)[d$Season])

  # overdispersed counts -> nbinom2 target
  mu_count <- exp(1.2 + trtC + seaC + b)
  d$insect_count <- stats::rnbinom(n, mu = mu_count, size = 2)

  # zero-heavy counts -> good candidate for ziformula
  zi_p <- stats::plogis(-1 + 0.8 * (d$Treatment == "Grazed"))
  lam  <- exp(1.0 + trtC + b)
  d$seedling_count <- ifelse(stats::rbinom(n, 1, zi_p) == 1, 0,
                             stats::rpois(n, lam))

  # proportion in (0,1) -> beta family
  eta <- 0.5 + trtC * 0.5 + seaC * 0.3 + b
  d$cover_prop <- pmin(pmax(stats::plogis(eta) + stats::rnorm(n, 0, 0.03),
                            0.001), 0.999)

  # binary presence/absence -> binomial (own tab)
  peta <- -0.3 + 0.9 * (d$Treatment == "Fertilized") -
    0.6 * (d$Treatment == "Grazed") + b
  d$present <- stats::rbinom(n, 1, stats::plogis(peta))

  d
}
