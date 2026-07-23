# ===========================================================================
#  helpers_lmer.R -- pure helpers for the Mixed Model Review app
# ---------------------------------------------------------------------------
#  Mixed-model logic extracted from the original single-file LMER tool so the
#  module (mod_lmer.R) stays a thin wrapper. Everything here is free of Shiny
#  reactivity: functions take data + a plain spec list and return data /
#  structured lists, so they are unit-testable on their own.
#
#  Reuses the package's canonical scalar bq() (helpers_plot.R) via bq_each();
#  copy_js / UF colours come from components.R; %||% is base R (R >= 4.4).
# ===========================================================================

# Extra plot-fill shades the mixed-model plots need on top of the kit palette.
UF_CHARCOAL <- "#222222"   # body text in plot themes
UF_BLUE_LT  <- "#cdd8ea"   # tint of UF blue for fills / histograms
MAX_FIXED   <- 3L          # hard cap on fixed effects the tool compares

# Vectorized backtick-quoter for formula strings built from >1 name. Maps the
# canonical scalar bq() (R/helpers_plot.R) over a vector so there is exactly one
# bq in the package namespace. bq() only quotes non-syntactic names, so syntactic
# names pass through unquoted -- formulas parse identically either way.
bq_each <- function(x) vapply(x, bq, character(1), USE.NAMES = FALSE)

# Default denominator-df method: Kenward-Roger when pbkrtest is available
# (it is defined for REML), otherwise Satterthwaite.
default_ddf <- function() {
  if (requireNamespace("pbkrtest", quietly = TRUE)) "Kenward-Roger" else "Satterthwaite"
}

# ---------------------------------------------------------------------------
#  Formula construction
# ---------------------------------------------------------------------------

# Build the left-hand side, applying the chosen response transformation.
build_lhs <- function(response, transform) {
  r <- bq(response)
  switch(transform,
         "none"    = r,
         "log"     = sprintf("log(%s)", r),
         "log1p"   = sprintf("log1p(%s)", r),
         "sqrt"    = sprintf("sqrt(%s)", r),
         "inverse" = sprintf("I(1/%s)", r),
         "asin"    = sprintf("asin(sqrt(%s))", r),
         r)
}

# Build the random-effects part. A random slope is applied ONLY to the chosen
# grouping factor(s) (slope_group); every other grouping factor stays
# intercept-only. If slope_group is empty the slope falls back to all groups.
build_random_part <- function(random, slope = NULL, slope_group = NULL) {
  if (length(random) == 0) return(NULL)
  if (!is.null(slope) && nzchar(slope)) {
    if (is.null(slope_group) || !length(slope_group)) slope_group <- random
    terms <- vapply(random, function(g) {
      if (g %in% slope_group)
        sprintf("(1 + %s | %s)", bq(slope), bq(g))
      else
        sprintf("(1 | %s)", bq(g))
    }, character(1))
    paste(terms, collapse = " + ")
  } else {
    paste(sprintf("(1 | %s)", bq_each(random)), collapse = " + ")
  }
}

#' Assemble a complete lmer() formula string.
#'
#' Combines the (optionally transformed) response, the fixed-effects part
#' (additive or fully crossed when interactions = TRUE), and the random-effects
#' part (intercepts, plus a random slope on the chosen grouping factor(s)).
#'
#' @param response Name of the numeric response column.
#' @param transform One of "none", "log", "log1p", "sqrt", "inverse", "asin".
#' @param fixed Character vector of fixed-effect column names.
#' @param random Character vector of grouping (random-effect) column names.
#' @param interactions Cross the fixed effects with `*` instead of `+`.
#' @param slope Optional column name for a random slope.
#' @param slope_group Optional grouping factor(s) the slope attaches to.
#' @return A single formula string suitable for [stats::as.formula()].
#' @noRd
build_formula_string <- function(response, transform, fixed, random,
                                 interactions, slope = NULL,
                                 slope_group = NULL) {
  lhs <- build_lhs(response, transform)

  if (length(fixed) == 0) {
    fixed_part <- "1"
  } else if (isTRUE(interactions) && length(fixed) > 1) {
    fixed_part <- paste(bq_each(fixed), collapse = " * ")
  } else {
    fixed_part <- paste(bq_each(fixed), collapse = " + ")
  }

  random_part <- build_random_part(random, slope, slope_group)
  paste(lhs, "~", fixed_part, "+", random_part)
}

# Which of a set of variables are categorical (factor / character) in df.
is_categorical <- function(df, vars) {
  vapply(vars, function(v) is.factor(df[[v]]) || is.character(df[[v]]),
         logical(1))
}

# Map a UI transform to the emmeans `tran` spec for back-transformation.
# log / sqrt are auto-detected by emmeans; inverse / arcsine / log1p are not.
tran_for_emmeans <- function(transform) {
  switch(transform,
         "none"    = NULL,
         "log"     = "log",
         "log1p"   = "log1p",
         "sqrt"    = "sqrt",
         "inverse" = "inverse",
         "asin"    = "asin.sqrt",
         NULL)
}

# ---------------------------------------------------------------------------
#  EMMeans spec helpers: split selected factors into "main" vs an optional
#  conditioning ("by") factor so the spec can become ~ A | B (simple effects).
# ---------------------------------------------------------------------------
emm_split <- function(emmvars, by) {
  by_var    <- intersect(by, emmvars)
  main_vars <- setdiff(emmvars, by_var)
  if (!length(main_vars)) { main_vars <- emmvars; by_var <- character(0) }
  list(main = main_vars, by = by_var)
}

emm_spec_formula <- function(emmvars, by) {
  s <- emm_split(emmvars, by)
  if (length(s$by))
    stats::as.formula(paste("~", paste(bq_each(s$main), collapse = " * "),
                            "|", paste(bq_each(s$by), collapse = " * ")))
  else
    stats::as.formula(paste("~", paste(bq_each(emmvars), collapse = " * ")))
}

#' Build the EMMeans spec text (`~ A` or `~ A | B`) for an emmeans call.
#'
#' @param emmvars Character vector of categorical factors to estimate over.
#' @param by Optional conditioning factor for simple effects (`~ A | B`).
#' @return A character string of the emmeans spec.
#' @noRd
emm_spec_text <- function(emmvars, by) {
  s <- emm_split(emmvars, by)
  if (length(s$by))
    paste("~", paste(bq_each(s$main), collapse = " * "),
          "|", paste(bq_each(s$by), collapse = " * "))
  else
    paste("~", paste(bq_each(emmvars), collapse = " * "))
}

# Map selected factors to plot roles (x / colour / facet). A conditioning
# (by) factor always becomes the facet so its within-group letters are legible.
emm_roles <- function(emmvars, by) {
  s    <- emm_split(emmvars, by)
  main <- s$main; byv <- s$by
  list(
    x      = if (length(main) >= 1) main[1] else NA_character_,
    colour = if (length(main) >= 2) main[2] else NA_character_,
    facet  = if (length(byv)) byv[1] else if (length(main) >= 3) main[3] else NA_character_,
    main   = main,
    by     = byv
  )
}

# Round numeric columns for display; p-value columns use significant figures so
# a small p (e.g. 2e-6) is never rounded to a misleading 0.
round_df <- function(d, digits = 4) {
  num  <- vapply(d, is.numeric, logical(1))
  is_p <- grepl("p\\.value|p_value|Pr\\(|^p$", names(d), ignore.case = TRUE)
  for (j in which(num))
    d[[j]] <- if (is_p[j]) signif(d[[j]], 3) else round(d[[j]], digits)
  d
}

# Paste 2+ columns into a single factor (a clean interaction grouping). Used by
# the module's "Create combined variable" builder.
make_combined_factor <- function(df, cols, sep = ".") {
  stopifnot(is.data.frame(df), is.character(cols), length(cols) >= 2L)
  missing <- setdiff(cols, names(df))
  if (length(missing))
    stop("Columns not found in data: ", paste(missing, collapse = ", "))
  parts <- lapply(cols, function(v) as.character(df[[v]]))
  factor(do.call(paste, c(parts, sep = sep)))
}

# ---------------------------------------------------------------------------
#  Built-in example: a 3-factor randomized complete block design (RCBD)
# ---------------------------------------------------------------------------
#  A fully crossed design so every feature has something to show:
#    Block (1-6)        : the random / grouping factor
#    Variety (V1-V3)    : categorical treatment factor (read as text)
#    Nitrogen (0/100/200): a treatment factor stored as a NUMBER -- treat it as
#                          a factor (Import > Change variable types) to compare
#                          its levels in EMMeans
#    Irrigation         : categorical treatment factor (read as text)
#  Five responses, each chosen to exercise a different part of the app:
#    yield_kg   - real Variety x Nitrogen interaction (no transform)
#    height_cm  - additive main effects only
#    biomass_g  - right-skewed / multiplicative (log transform)
#    germination- a proportion in (0,1)        (arcsine-sqrt transform)
#    pest_count - counts                        (sqrt transform)

#' Generate the built-in 3-factor RCBD example dataset.
#'
#' A seeded randomized complete block design used as the demo dataset for the
#' Mixed Model Review app. Five responses each exercise a different feature
#' (an interaction, additive effects, and log / arcsine-sqrt / sqrt transforms).
#'
#' @return A data frame with Block, Variety, Nitrogen, Irrigation, PlotOrder and
#'   five response columns (108 rows).
#' @examples
#' d <- make_example_data()
#' nrow(d)
#' @export
make_example_data <- function() {
  restore_rng <- snapshot_rng()   # fixed seed must NOT hijack the session RNG
  on.exit(restore_rng(), add = TRUE)
  set.seed(2024)
  blocks <- 1:6
  d <- expand.grid(Block      = blocks,
                   Variety     = c("V1", "V2", "V3"),
                   Nitrogen    = c(0, 100, 200),
                   Irrigation  = c("Drip", "Flood"),
                   KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  d <- d[order(d$Block, sample(nrow(d))), ]
  d$PlotOrder <- stats::ave(d$Block, d$Block, FUN = seq_along)
  rownames(d) <- NULL
  n <- nrow(d)

  bz   <- stats::setNames(stats::rnorm(length(blocks)), as.character(blocks))
  b    <- unname(bz[as.character(d$Block)])
  varV <- unname(c(V1 = 0, V2 = 8, V3 = 4)[d$Variety])
  nitr <- unname(c(`0` = 0, `100` = 6, `200` = 8)[as.character(d$Nitrogen)])
  irr  <- unname(c(Drip = 0, Flood = -3)[d$Irrigation])

  # yield: Variety x Nitrogen interaction (V2 gains most at high N)
  inter <- ifelse(d$Variety == "V2" & d$Nitrogen == 200,  7,
           ifelse(d$Variety == "V3" & d$Nitrogen == 200, -3, 0))
  d$yield_kg <- round(50 + varV + nitr + irr + inter + 5 * b +
                        stats::rnorm(n, 0, 2.5), 2)

  # height: purely additive main effects
  varH <- unname(c(V1 = 0, V2 = 10, V3 = -5)[d$Variety])
  nitH <- unname(c(`0` = 0, `100` = 5, `200` = 9)[as.character(d$Nitrogen)])
  irrH <- unname(c(Drip = 0, Flood = 4)[d$Irrigation])
  d$height_cm <- round(120 + varH + nitH + irrH + 4 * b + stats::rnorm(n, 0, 3), 1)

  # biomass: multiplicative / right-skewed -> log transform
  log_mu <- 3.0 +
    unname(c(V1 = 0, V2 = 0.30, V3 = 0.15)[d$Variety]) +
    unname(c(`0` = 0, `100` = 0.18, `200` = 0.25)[as.character(d$Nitrogen)]) +
    unname(c(Drip = 0, Flood = -0.12)[d$Irrigation]) + 0.20 * b
  d$biomass_g <- round(exp(log_mu + stats::rnorm(n, 0, 0.18)), 1)

  # germination: a proportion in (0, 1) -> arcsine-sqrt transform
  linp <- 1.2 +
    unname(c(V1 = 0, V2 = 0.4, V3 = 0.1)[d$Variety]) +
    unname(c(`0` = 0, `100` = 0.35, `200` = 0.6)[as.character(d$Nitrogen)]) +
    unname(c(Drip = 0, Flood = -0.5)[d$Irrigation]) + 0.3 * b +
    stats::rnorm(n, 0, 0.30)
  d$germination <- round(stats::plogis(linp), 3)

  # pest_count: counts -> sqrt transform
  lam <- exp(1.0 +
             unname(c(Drip = 0, Flood = 0.5)[d$Irrigation]) +
             unname(c(V1 = 0, V2 = 0.1, V3 = 0.25)[d$Variety]) + 0.15 * b)
  d$pest_count <- stats::rpois(n, lam)

  d[, c("Block", "Variety", "Nitrogen", "Irrigation", "PlotOrder",
        "yield_kg", "height_cm", "biomass_g", "germination", "pest_count")]
}

# ---------------------------------------------------------------------------
#  Engine: validate -> fit -> emmeans / anova / joint / compare / code
#  Each returns a structured list (ok / error / notes), never calls Shiny.
#  A `spec` is a plain list with: response, transform, fixed, random,
#  interactions, slope, slope_group, reml, anova_type, ddf.
# ---------------------------------------------------------------------------

# Validate a spec against the data WITHOUT fitting. Returns a character vector
# of human-readable problems (empty = good to fit). Folds the structural,
# transform-sanity, and degenerate-factor guards from the original app.
lmer_validate <- function(df, spec) {
  response  <- spec$response
  transform <- spec$transform %||% "none"
  fixed     <- spec$fixed
  random    <- spec$random
  slope     <- spec$slope

  problems <- character(0)
  if (is.null(fixed) || length(fixed) == 0)
    problems <- c(problems,
                  "No fixed effects selected. A model needs at least one fixed effect for the ANOVA and EMMeans to be meaningful.")
  if (length(fixed) > MAX_FIXED)
    problems <- c(problems,
                  sprintf("More than %d fixed effects selected. This tool limits comparisons to %d.", MAX_FIXED, MAX_FIXED))
  if (is.null(random) || length(random) == 0)
    problems <- c(problems,
                  "No random effects selected. lmer() requires at least one random (grouping) effect \u2014 without one use a fixed-effects model such as lm() / aov().")
  if (!is.null(response) && response %in% fixed)
    problems <- c(problems, sprintf(
      "The response '%s' is also selected as a fixed effect \u2014 a variable can't predict itself. Remove it from the fixed effects.", response))
  if (!is.null(response) && response %in% random)
    problems <- c(problems, sprintf(
      "The response '%s' is also selected as a random effect. Remove it from the random effects.", response))
  if (length(intersect(fixed, random)))
    problems <- c(problems, sprintf(
      "Variable(s) %s are selected as both fixed and random effects, which confounds the fixed estimate with the grouping. Use each variable in only one role.",
      paste(intersect(fixed, random), collapse = ", ")))

  if (length(problems)) return(problems)

  # transformation sanity checks
  if (!is.null(response) && response %in% names(df)) {
    y <- df[[response]]
    bad <- switch(transform,
                  "log"     = if (any(y <= 0, na.rm = TRUE)) "log() requires all response values > 0.",
                  "log1p"   = if (any(y <= -1, na.rm = TRUE)) "log1p() requires all response values > -1 (\u2212 1 gives log(0) = \u2212Inf).",
                  "sqrt"    = if (any(y < 0,  na.rm = TRUE)) "sqrt() requires all response values >= 0.",
                  "inverse" = if (any(y == 0, na.rm = TRUE)) "inverse (1/y) requires all response values != 0.",
                  "asin"    = if (any(y < 0 | y > 1, na.rm = TRUE)) "arcsine-sqrt requires the response to be a proportion in [0, 1].",
                  NULL)
    if (!is.null(bad)) return(bad)
  }

  # degenerate-factor guards, evaluated on the rows lmer will actually use
  model_df <- df
  for (g in random) model_df[[g]] <- factor(model_df[[g]])
  model_vars <- unique(c(response, fixed, random,
                         if (!is.null(slope) && nzchar(slope)) slope))
  model_vars <- intersect(model_vars, names(model_df))
  cc_rows    <- stats::complete.cases(model_df[, model_vars, drop = FALSE])
  used_df    <- model_df[cc_rows, , drop = FALSE]
  n_levels_used <- function(v) nlevels(droplevels(factor(v)))
  deg <- character(0)
  cat_fixed <- fixed[is_categorical(df, fixed)]
  if (sum(cc_rows) < 2)
    deg <- c(deg, "Fewer than two complete rows remain after removing missing values \u2014 nothing to fit.")
  for (v in cat_fixed)
    if (n_levels_used(used_df[[v]]) < 2)
      deg <- c(deg, sprintf(
        "Fixed effect '%s' has only one level among the rows used (after dropping missing values) \u2014 it can't be estimated. Drop it or pick another variable.", v))
  for (g in random)
    if (n_levels_used(used_df[[g]]) < 2)
      deg <- c(deg, sprintf(
        "Random effect '%s' has only one level among the rows used \u2014 lmer() needs each grouping factor to have >1 level. A single-group factor carries no between-group variance.", g))
  deg
}

# Fit the mixed model from a spec. Returns a structured list; on failure
# `ok = FALSE` with an `error` message instead of throwing.
lmer_fit <- function(df, spec) {
  response  <- spec$response
  transform <- spec$transform %||% "none"
  fixed     <- spec$fixed
  random    <- spec$random
  slope     <- spec$slope
  use_reml  <- isTRUE(spec$reml)
  atype     <- spec$anova_type %||% "3"
  req_ddf   <- spec$ddf %||% "Satterthwaite"
  interactions <- isTRUE(spec$interactions)

  model_df <- df
  for (g in random) model_df[[g]] <- factor(model_df[[g]])

  cat_fixed <- fixed[is_categorical(df, fixed)]
  has_inter <- interactions && length(fixed) > 1

  use_slope <- if (!is.null(slope) && nzchar(slope)) slope else NULL
  use_slope_group <- if (!is.null(use_slope)) {
    g <- intersect(spec$slope_group, random)
    if (length(g)) g else random[1]
  } else NULL

  fml_str <- build_formula_string(response, transform, fixed, random,
                                  interactions, use_slope, use_slope_group)
  fml     <- stats::as.formula(fml_str)

  # Type III main-effect tests are contrast-dependent ONLY when interactions
  # are present; sum-to-zero coding keeps them interpretable.
  apply_sum <- (atype == "3" && has_inter && length(cat_fixed) > 0)
  contr_arg <- if (apply_sum)
    stats::setNames(as.list(rep("contr.sum", length(cat_fixed))), cat_fixed)
  else NULL

  # Kenward-Roger df is defined for REML; auto-switch under ML.
  eff_ddf <- req_ddf
  ddf_switched <- (!use_reml && eff_ddf == "Kenward-Roger")
  if (ddf_switched) eff_ddf <- "Satterthwaite"

  fit_warnings <- character(0)
  fit <- tryCatch(
    withCallingHandlers(
      lmerTest::lmer(fml, data = model_df, REML = use_reml,
                     na.action = stats::na.omit, contrasts = contr_arg),
      warning = function(w) {
        fit_warnings <<- c(fit_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }),
    error = function(e) e)
  if (inherits(fit, "error"))
    return(list(ok = FALSE, mod = NULL,
                error = paste("Model failed to fit:", conditionMessage(fit))))

  # soft diagnostics (singular / convergence / dropped rows / ddf)
  notes <- character(0)
  n_drop <- nrow(model_df) - stats::nobs(fit)
  if (n_drop > 0)
    notes <- c(notes, sprintf(
      "%d of %d row(s) were dropped for missing values in the model variables (listwise deletion). The model used %d row(s).",
      n_drop, nrow(model_df), stats::nobs(fit)))
  if (ddf_switched)
    notes <- c(notes,
               "Kenward-Roger requires REML; Satterthwaite denominator df was used because ML was selected.")
  if (apply_sum)
    notes <- c(notes,
               "Type III ANOVA with interactions: sum-to-zero contrasts were applied to categorical fixed effects so the main-effect tests are interpretable. (This also re-codes the summary() coefficients as deviations from the grand mean.)")

  ng <- tryCatch(lme4::ngrps(fit), error = function(e) NULL)
  if (!is.null(ng)) {
    few <- ng[ng < 5]
    for (g in names(few))
      notes <- c(notes, sprintf(
        "Random effect '%s' has only %d level(s). Variance components estimated from fewer than ~5 levels are unreliable \u2014 consider modelling it as a fixed effect instead.",
        g, few[[g]]))
  }

  conv_msg  <- fit@optinfo$conv$lme4$messages
  warn_all  <- unique(c(fit_warnings, conv_msg))
  warn_all  <- warn_all[nzchar(warn_all)]
  singular  <- isTRUE(lme4::isSingular(fit))
  if (length(warn_all))
    notes <- c(notes, paste("Fit warning:", warn_all))
  if (singular)
    notes <- c(notes, "Singular fit: at least one variance/correlation is on the boundary (often 0).")
  trouble <- singular ||
    any(grepl("Hessian|gradient|converge|singular|isSingular|not uniquely",
              warn_all, ignore.case = TRUE))
  if (trouble)
    notes <- c(notes, paste0(
      "What this means: the model is likely over-parameterized for the data, ",
      "so some parameters aren't uniquely identified. Things to try: ",
      "(1) drop the random slope and use an intercept-only random structure; ",
      "(2) remove a sparse or empty-cell factor \u2014 check the design-balance ",
      "table on the Explore tab; (3) center and scale continuous predictors ",
      "so they're on comparable scales; (4) simplify the fixed-effects part. ",
      "The estimates can still be reported, but treat the affected variance ",
      "components and their SEs with caution."))

  list(ok = TRUE, mod = fit, fml = fml, fml_str = fml_str, error = NULL,
       notes = notes, spec = spec,
       response = response, transform = transform,
       fixed = fixed, random = random, slope = use_slope,
       slope_group = use_slope_group, reml = use_reml, ddf = eff_ddf,
       atype = atype, sum_contrasts = apply_sum, ddf_switched = ddf_switched,
       cat_fixed = cat_fixed, data = model_df)
}

# Compute estimated marginal means, compact-letter display, pairwise
# comparisons, per-cell counts and held covariates from a fit list. Honours an
# optional `by` factor (~ A | B simple effects) and back-transformation.
lmer_emmeans <- function(fit, emmvars, by = NULL, backtransform = FALSE,
                         adjust = "tukey", level = 0.95) {
  if (is.null(fit) || !isTRUE(fit$ok))
    return(list(ok = FALSE, error = "No fitted model."))
  if (length(emmvars) < 1)
    return(list(ok = FALSE,
                error = "Select one or more categorical fixed effects for EMMeans."))
  mod <- fit$mod
  if (!all(emmvars %in% fit$fixed))
    return(list(ok = FALSE, error =
      "Your EMMeans selection no longer matches the fitted model. Click 'Run analysis' again."))

  roles <- emm_roles(emmvars, by)
  spec  <- emm_spec_formula(emmvars, by)
  bt    <- fit$transform != "none" && isTRUE(backtransform)
  type  <- if (bt) "response" else "link"
  tran_spec <- tran_for_emmeans(fit$transform)

  emm <- tryCatch({
    if (bt && !is.null(tran_spec)) {
      rg <- emmeans::ref_grid(mod, tran = tran_spec)
      emmeans::emmeans(rg, spec, type = "response", level = level)
    } else {
      emmeans::emmeans(mod, spec, type = type, level = level)
    }
  }, error = function(e) e)
  if (inherits(emm, "error"))
    return(list(ok = FALSE, error = paste("EMMeans failed:", conditionMessage(emm))))

  cld_df <- tryCatch(
    as.data.frame(multcomp::cld(emm, Letters = letters,
                                adjust = adjust, reversed = TRUE)),
    error = function(e) e)
  if (inherits(cld_df, "error"))
    return(list(ok = FALSE,
                error = "Could not compute letter groupings for this specification."))
  if (".group" %in% names(cld_df)) cld_df$.group <- trimws(cld_df$.group)

  prs <- tryCatch(as.data.frame(graphics::pairs(emm, adjust = adjust)),
                  error = function(e)
                    data.frame(note = paste("Pairwise comparisons unavailable:",
                                            conditionMessage(e))))

  md <- fit$data
  mv <- unique(c(fit$response, fit$fixed, fit$random,
                 if (!is.null(fit$slope)) fit$slope))
  mv <- intersect(mv, names(md))
  used <- md[stats::complete.cases(md[, mv, drop = FALSE]), , drop = FALSE]
  cnt  <- tryCatch(as.data.frame(table(used[emmvars]), responseName = "n"),
                   error = function(e) NULL)
  if (!is.null(cnt)) names(cnt)[seq_along(emmvars)] <- emmvars
  contfx <- setdiff(fit$fixed, emmvars)
  contfx <- contfx[vapply(contfx, function(v) is.numeric(md[[v]]), logical(1))]
  held <- if (length(contfx))
    paste(sprintf("%s = %.4g", contfx,
                  vapply(contfx, function(v) mean(used[[v]], na.rm = TRUE),
                         numeric(1))), collapse = ", ") else NULL

  list(ok = TRUE, error = NULL, emm = emm, vars = emmvars, roles = roles,
       main_vars = roles$main, by_var = roles$by, cld = cld_df, pairs = prs,
       counts = cnt, held = held, backtransformed = bt)
}

# Fit statistics table (AIC/BIC/logLik, REML criterion or deviance, observation
# counts, and optional marginal/conditional R^2 + ICC via performance / MuMIn).
lmer_fit_stats <- function(model, n_total, reml) {
  n_used  <- stats::nobs(model)
  is_reml <- isTRUE(reml)
  dev_name <- if (is_reml) "REML_criterion" else "deviance"
  dev_val  <- tryCatch(
    if (is_reml) as.numeric(lme4::REMLcrit(model)) else stats::deviance(model, REML = FALSE),
    error = function(e) NA_real_)
  stats_l <- list(AIC = stats::AIC(model), BIC = stats::BIC(model),
                  logLik = as.numeric(stats::logLik(model)))
  stats_l[[dev_name]] <- dev_val
  stats_l <- c(stats_l, list(n_obs_used = n_used, n_obs_total = n_total,
                             n_dropped_NA = n_total - n_used))
  if (requireNamespace("performance", quietly = TRUE)) {
    r2  <- tryCatch(performance::r2(model),  error = function(e) NULL)
    icc <- tryCatch(performance::icc(model), error = function(e) NULL)
    # is.list() guards, not just !is.null(): on a SINGULAR fit performance::icc()
    # returns a bare NA (atomic), and NA$ICC_adjusted errors with "$ operator is
    # invalid for atomic vectors", reddening the whole Fit Statistics table.
    if (is.list(r2) && !is.null(r2$R2_marginal)) {
      stats_l$R2_marginal    <- as.numeric(r2$R2_marginal)
      stats_l$R2_conditional <- as.numeric(r2$R2_conditional)
    }
    if (is.list(icc) && !is.null(icc$ICC_adjusted))
      stats_l$ICC <- as.numeric(icc$ICC_adjusted)
  } else if (requireNamespace("MuMIn", quietly = TRUE)) {
    r2m <- tryCatch(MuMIn::r.squaredGLMM(model), error = function(e) NULL)
    if (!is.null(r2m)) {
      stats_l$R2_marginal    <- as.numeric(r2m[1, "R2m"])
      stats_l$R2_conditional <- as.numeric(r2m[1, "R2c"])
    }
  }
  data.frame(Statistic = names(stats_l),
             Value = round(unlist(stats_l), 4), row.names = NULL)
}

# Type II / III ANOVA with a Satterthwaite fallback if the requested ddf fails,
# and a plain-language note when p-values / denominator df come back non-finite.
lmer_anova <- function(model, type = "3", ddf = "Kenward-Roger") {
  type_roman <- if (type == "3") "III" else "II"
  warns <- character(0)
  run_anova <- function(ddf_) withCallingHandlers(
    stats::anova(model, type = type_roman, ddf = ddf_),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning")
    })

  fell_back <- FALSE
  a <- tryCatch(run_anova(ddf), error = function(e) e)
  if (inherits(a, "error")) {
    fell_back <- TRUE
    a <- tryCatch(run_anova("Satterthwaite"), error = function(e) e)
  }
  if (inherits(a, "error"))
    return(list(table = NULL, note = "",
                error = paste("ANOVA failed:", conditionMessage(a))))

  note <- if (fell_back)
    "Requested ddf failed; fell back to Satterthwaite.\n" else ""
  pcol    <- intersect(c("Pr(>F)", "p.value"), names(a))
  bad_p   <- length(pcol) && any(!is.finite(a[[pcol[1]]]))
  bad_ddf <- "DenDF" %in% names(a) && any(!is.finite(a[["DenDF"]]))
  if (bad_p || bad_ddf || any(grepl("NaN", warns, ignore.case = TRUE)))
    note <- paste0(note,
      "Note: one or more p-values could not be computed (NaN). The denominator ",
      "degrees of freedom for those terms can't be estimated \u2014 this happens ",
      "when the fit is singular / over-parameterized (see the fit warnings on the ",
      "model run). Try an intercept-only random structure (drop the random slope), ",
      "remove a sparse factor (Explore > design balance), or center & scale ",
      "continuous predictors, then refit. The F statistics shown are still ",
      "informative; only the affected p-values are unreliable.")
  list(table = a, note = note, error = NULL)
}

# Omnibus joint F-tests for every model term (including any interaction), with a
# note when the model has no interaction term.
lmer_joint_tests <- function(model) {
  jt <- tryCatch(emmeans::joint_tests(model), error = function(e) e)
  if (inherits(jt, "error"))
    return(list(table = NULL,
                note = paste("joint_tests() failed:", conditionMessage(jt))))
  tl <- attr(stats::terms(model), "term.labels")
  note <- if (!any(grepl(":", tl, fixed = TRUE)))
    paste0("Note: this model has no interaction term, so only main effects are ",
           "tested above. Tick 'Include interactions' and re-run to fit one.") else ""
  list(table = jt, note = note)
}

# Likelihood-ratio comparison of a saved baseline (A) vs the current fit (B).
# Returns the validity advisories (as a character vector) plus the anova() LRT.
lmer_compare <- function(fitA, fitB) {
  if (is.null(fitA) || is.null(fitB))
    return(list(warnings = "Need a saved model (A) and a current fitted model (B).",
                anova = NULL, error = NULL))
  a <- fitA; b <- fitB
  msgs <- character(0)
  same_resp <- identical(a$response, b$response) &&
    identical(a$transform, b$transform)
  na_used <- stats::nobs(a$mod); nb_used <- stats::nobs(b$mod)
  fixed_differ  <- !identical(sort(a$fixed),  sort(b$fixed)) ||
    !identical(a$fml_str, b$fml_str)
  random_differ <- !identical(sort(a$random), sort(b$random)) ||
    !identical(a$slope %||% "", b$slope %||% "")

  if (!same_resp)
    msgs <- c(msgs, paste0(
      "WARNING: the models use a different response or transformation, so the ",
      "likelihood is not comparable. A likelihood-ratio test is NOT valid here; ",
      "AIC/BIC are also not comparable across different responses."))
  if (na_used != nb_used)
    msgs <- c(msgs, sprintf(paste0(
      "WARNING: the models were fit on different numbers of rows (A: %d, B: %d), ",
      "usually because they involve variables with different missing-value ",
      "patterns. A valid LRT requires both models fit on exactly the same rows."),
      na_used, nb_used))
  if (random_differ)
    msgs <- c(msgs, paste0(
      "NOTE: the random-effects structures differ. Testing whether a variance ",
      "component is zero places the null on the boundary of the parameter space, ",
      "so the ordinary chi-square LRT is conservative (the true p-value is ",
      "smaller; a common correction halves it). Interpret such tests with care."))
  if (fixed_differ && !random_differ)
    msgs <- c(msgs, paste0(
      "NOTE: the models differ in fixed effects. anova() refits both with ML ",
      "(REML likelihoods are not comparable across different fixed effects), ",
      "which is the correct basis for this test."))
  msgs <- c(msgs, paste0(
    "The LRT below is valid only if model A is nested within model B (or vice ",
    "versa) and both are fit on identical data."))

  res <- tryCatch(stats::anova(a$mod, b$mod), error = function(e) e)
  if (inherits(res, "error"))
    return(list(warnings = msgs, anova = NULL, error = conditionMessage(res)))
  list(warnings = msgs, anova = res, error = NULL)
}

# Generate the copy-pasteable R code for the whole analysis (fit -> anova ->
# joint test -> fit quality -> emmeans + post hoc). Pure string assembly.
lmer_code <- function(fit, emmvars = character(0), emm_by = NULL,
                      backtransform = FALSE, conf = 0.95, adjust = "tukey") {
  fml_str   <- fit$fml_str
  use_reml  <- isTRUE(fit$reml)
  transform <- fit$transform
  cat_fixed <- fit$cat_fixed
  apply_sum <- isTRUE(fit$sum_contrasts)
  eff_ddf   <- fit$ddf
  atype     <- fit$atype

  spec_str <- if (length(emmvars))
    emm_spec_text(emmvars, emm_by) else "~ <pick a factor>"
  bt        <- transform != "none" && isTRUE(backtransform)
  tran_spec <- tran_for_emmeans(transform)
  emm_call  <- if (bt && !is.null(tran_spec))
    sprintf('emm <- emmeans(ref_grid(mod, tran = "%s"), %s,\n               type = "response", level = %s)',
            tran_spec, spec_str, format(conf))
  else
    sprintf('emm <- emmeans(mod, %s, type = "%s", level = %s)',
            spec_str, if (bt) "response" else "link", format(conf))

  contr_code <- if (apply_sum)
    sprintf(",\n            contrasts = list(%s)",
            paste(sprintf('%s = "contr.sum"', bq_each(cat_fixed)), collapse = ", "))
  else ""
  type_roman <- if (atype == "3") "III" else "II"

  paste(
    "library(lmerTest)   # loads lme4, adds K-R / Satterthwaite ddf",
    "library(emmeans)",
    "library(multcomp)",
    "",
    "# --- Fit the mixed model -------------------------------------------",
    sprintf("mod <- lmer(%s,\n            data = dat, REML = %s, na.action = na.omit%s)",
            fml_str, if (use_reml) "TRUE" else "FALSE", contr_code),
    "summary(mod)",
    "",
    "# --- ANOVA -----------------------------------------------------------",
    sprintf('anova(mod, type = "%s", ddf = "%s")', type_roman, eff_ddf),
    "",
    "# --- Omnibus interaction test ---------------------------------------",
    "joint_tests(mod)   # F-test for each term, including any interaction",
    "",
    "# --- Fit quality -----------------------------------------------------",
    "performance::r2(mod)    # marginal & conditional R^2",
    "performance::icc(mod)   # intraclass correlation",
    "",
    "# --- Estimated marginal means + post hoc -----------------------------",
    emm_call,
    sprintf('pairs(emm, adjust = "%s")', adjust),
    sprintf('cld(emm, Letters = letters, adjust = "%s", reversed = TRUE)', adjust),
    sep = "\n"
  )
}

# Leave-one-group-out Cook's distance, computed directly (influence.ME refits by
# re-evaluating the stored call, which fails inside a Shiny reactive). For each
# level of `group` the model is refit on the remaining rows and the standardized
# change in the fixed effects is reported:
#   D_i = (b - b_(-i))' Sigma^{-1} (b - b_(-i)) / p
# with Sigma = vcov(full model) and p = number of fixed-effect parameters.
# `progress` is an optional function(amount, detail) for a Shiny progress bar.
lmer_cook <- function(model, fml, data, group, reml, progress = NULL) {
  environment(fml) <- environment()
  beta <- lme4::fixef(model)
  Sig  <- as.matrix(stats::vcov(model))
  gv   <- factor(data[[group]])
  levs <- levels(gv)
  nlev <- length(levs)
  if (nlev < 2)
    stop("Need at least two groups to assess leave-one-group-out influence.")
  ck <- rep(NA_real_, nlev)
  for (k in seq_len(nlev)) {
    if (is.function(progress))
      progress(1 / nlev, detail = sprintf("%d of %d", k, nlev))
    sub <- data[gv != levs[k], , drop = FALSE]
    m_k <- suppressWarnings(tryCatch(
      lme4::lmer(fml, data = sub, REML = reml, na.action = stats::na.omit),
      error = function(e) NULL))
    if (is.null(m_k)) next
    fk <- lme4::fixef(m_k)
    nm <- intersect(names(beta), names(fk))
    if (!length(nm)) next
    db <- beta[nm] - fk[nm]
    ck[k] <- tryCatch(
      as.numeric(crossprod(db, solve(Sig[nm, nm, drop = FALSE], db))) / length(nm),
      error = function(e) NA_real_)
  }
  data.frame(group = levs, cook = ck, stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
#  Plot helpers (base graphics / ggplot2)
# ---------------------------------------------------------------------------

# Base-graphics 2x2 residual diagnostic panel (reused by viewer and PNG export).
draw_resid_plots <- function(mod) {
  r    <- stats::resid(mod)
  fitv <- stats::fitted(mod)
  op <- graphics::par(mfrow = c(2, 2), mar = c(4.5, 4.5, 2.5, 1))
  on.exit(graphics::par(op))

  plot(fitv, r, xlab = "Fitted values", ylab = "Residuals",
       main = "Residuals vs Fitted", pch = 19, col = UF_BLUE)
  graphics::abline(h = 0, lty = 2, col = "grey40")
  graphics::lines(stats::lowess(fitv, r), col = UF_ORANGE, lwd = 2)

  stats::qqnorm(r, main = "Normal Q-Q", pch = 19, col = UF_BLUE)
  stats::qqline(r, col = UF_ORANGE, lwd = 2)

  plot(fitv, sqrt(abs(scale(r))), xlab = "Fitted values",
       ylab = expression(sqrt(abs("std. residuals"))),
       main = "Scale-Location", pch = 19, col = UF_BLUE)
  graphics::lines(stats::lowess(fitv, sqrt(abs(scale(r)))), col = UF_ORANGE, lwd = 2)

  graphics::hist(r, breaks = "FD", col = UF_BLUE_LT, border = "white",
                 xlab = "Residuals", main = "Histogram of residuals")
}

# EMMeans plot with letter groupings. A pure function of an lmer_emmeans() result
# plus the response name and transform (so it has no Shiny dependency).
lmer_emm_plot <- function(er, response, transform) {
  cldf  <- er$cld
  roles <- er$roles
  # emmeans names the response-scale estimate by family: "response" for most,
  # "prob" for binomial, "rate" for Poisson/negative-binomial; "emmean" on
  # the link/identity scale. The cld frame's LEADING columns are the EMMeans
  # factors carrying user variable names, so a factor literally named "rate"
  # or "prob" must not shadow the estimate: exclude the role columns and
  # require the pick to be numeric before taking the first candidate.
  est_names <- c("response", "prob", "rate", "emmean")
  num_cols  <- names(cldf)[vapply(cldf, is.numeric, logical(1))]
  valcol    <- intersect(setdiff(est_names, c(roles$main, roles$by)),
                         num_cols)[1]
  if (is.na(valcol)) valcol <- intersect(est_names, num_cols)[1]
  lcol <- intersect(c("lower.CL", "asymp.LCL", "LCL"), names(cldf))[1]
  ucol <- intersect(c("upper.CL", "asymp.UCL", "UCL"), names(cldf))[1]
  grpcol <- if (".group" %in% names(cldf)) ".group" else NA

  ylab <- if (isTRUE(er$backtransformed))
    sprintf("%s (back-transformed)", response) else
      build_lhs(response, transform)

  x_var      <- roles$x
  colour_var <- roles$colour
  facet_var  <- roles$facet

  cldf[[x_var]] <- factor(cldf[[x_var]])
  if (!is.na(colour_var)) cldf[[colour_var]] <- factor(cldf[[colour_var]])
  if (!is.na(facet_var))  cldf[[facet_var]]  <- factor(cldf[[facet_var]])

  ytext <- if (!is.na(ucol)) cldf[[ucol]] else cldf[[valcol]]

  if (is.na(colour_var)) {
    p <- ggplot(cldf, aes(x = .data[[x_var]], y = .data[[valcol]])) +
      geom_point(size = 3, colour = UF_BLUE)
    if (!is.na(lcol) && !is.na(ucol))
      p <- p + geom_errorbar(aes(ymin = .data[[lcol]], ymax = .data[[ucol]]),
                             width = 0.15, colour = UF_BLUE)
    if (!is.na(grpcol))
      p <- p + geom_text(aes(label = .data[[grpcol]], y = ytext),
                         vjust = -0.8, fontface = "bold")
  } else {
    dodge <- position_dodge(width = 0.6)
    p <- ggplot(cldf, aes(x = .data[[x_var]], y = .data[[valcol]],
                          colour = .data[[colour_var]], group = .data[[colour_var]])) +
      geom_point(size = 3, position = dodge)
    if (!is.na(lcol) && !is.na(ucol))
      p <- p + geom_errorbar(aes(ymin = .data[[lcol]], ymax = .data[[ucol]]),
                             width = 0.2, position = dodge)
    if (!is.na(grpcol))
      p <- p + geom_text(aes(label = .data[[grpcol]], y = ytext),
                         vjust = -0.8, position = dodge,
                         fontface = "bold", show.legend = FALSE)
    n_lev <- nlevels(cldf[[colour_var]])
    uf_pal <- rep(UF_COLORS, length.out = max(n_lev, 1))
    p <- p + scale_colour_manual(values = uf_pal)
  }

  if (!is.na(facet_var))
    p <- p + facet_wrap(stats::reformulate(facet_var))

  subt <- if (length(er$by_var))
    sprintf("Letters compare levels within each %s (simple effects)",
            er$by_var[1]) else NULL

  p + labs(y = ylab, x = x_var, subtitle = subt,
           title = "Estimated marginal means \u00b1 CI with letter groupings") +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(face = "bold", colour = UF_BLUE),
          axis.title = element_text(colour = UF_CHARCOAL),
          legend.title = element_text(colour = UF_CHARCOAL))
}
