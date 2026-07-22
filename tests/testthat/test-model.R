# Tests for the regression helpers in helpers_model.R (base/stats only — the
# diagnostic ggplots are exercised in the PowerShell smoke test).

test_that("fit_model fits a simple linear model", {
  m <- fit_model(mtcars, "mpg", "wt", type = "linear")
  expect_s3_class(m, "lm")
  expect_equal(length(coef(m)), 2L)            # intercept + wt
})

test_that("fit_model builds a polynomial model of the given degree", {
  m <- fit_model(mtcars, "mpg", "wt", type = "polynomial", degree = 3)
  expect_s3_class(m, "lm")
  expect_equal(length(coef(m)), 4L)            # intercept + 3 poly terms
})

test_that("fit_model errors on unknown columns", {
  expect_error(fit_model(mtcars, "mpg", "nope"), "not found")
})

test_that("model_interpretation extracts R^2 and significance", {
  m    <- fit_model(mtcars, "mpg", c("wt", "hp"), type = "multiple")
  info <- model_interpretation(m)
  expect_true(info$r2 > 0.8)
  expect_true("wt" %in% info$significant)
  expect_true(is.numeric(info$overall_p) && info$overall_p < 0.05)
})

# ---- non-syntactic column names ---------------------------------------------
# Excel headers keep their spaces (readxl preserves them verbatim, unlike
# read.csv's check.names), so an unbackticked formula killed Regression for the
# most common import path with an opaque "unexpected symbol".

test_that("fit_model handles column names with spaces and other odd characters", {
  df <- mtcars
  names(df)[names(df) == "wt"]  <- "Body Weight"
  names(df)[names(df) == "hp"]  <- "Horse-Power (max)"
  names(df)[names(df) == "mpg"] <- "Miles per Gallon"

  m <- fit_model(df, "Miles per Gallon", c("Body Weight", "Horse-Power (max)"),
                 type = "multiple")
  expect_s3_class(m, "lm")
  expect_equal(unname(round(stats::coef(m), 4)),
               unname(round(stats::coef(
                 fit_model(mtcars, "mpg", c("wt", "hp"), type = "multiple")), 4)))
})

test_that("fit_model backticks the polynomial predictor too", {
  df <- mtcars; names(df)[names(df) == "wt"] <- "Body Weight"
  m  <- fit_model(df, "mpg", "Body Weight", type = "polynomial", degree = 2)
  expect_s3_class(m, "lm")
  expect_length(stats::coef(m), 3L)   # intercept + 2 poly terms
})

# ---- spec-driven engine: categorical predictors, interactions (v0.6.0+) ------

test_that("reg_validate accepts a good spec and rejects bad ones", {
  d <- mtcars; d$am <- factor(d$am)
  expect_length(reg_validate(d, reg_spec("mpg", c("wt", "am"))), 0L)
  expect_gt(length(reg_validate(d, reg_spec("am", "wt"))), 0L)        # non-numeric Y
  expect_gt(length(reg_validate(d, reg_spec("mpg", c("mpg", "wt")))), 0L) # Y in X
  expect_gt(length(reg_validate(d, reg_spec("mpg", "nope"))), 0L)     # missing col
  expect_gt(length(reg_validate(d, reg_spec("mpg", "am", poly_degree = 2))), 0L) # poly on factor
})

test_that("reg_validate flags a single-level factor among usable rows", {
  d <- data.frame(y = 1:6, g = factor(rep(c("a", "b"), 3)))
  d$g[d$g == "b"] <- "a"; d$g <- droplevels(d$g)
  expect_gt(length(reg_validate(d, reg_spec("y", "g"))), 0L)
})

test_that("reg_fit builds interaction and factor terms", {
  d <- mtcars; d$am <- factor(d$am, labels = c("auto", "manual"))
  m <- reg_fit(d, reg_spec("mpg", c("wt", "am"), interactions = TRUE))
  expect_s3_class(m, "lm")
  expect_true(any(grepl(":", names(stats::coef(m)))))       # interaction present
  expect_true("ammanual" %in% names(stats::coef(m)))         # factor dummy present
})

test_that("reg_fit honours a chosen reference level", {
  d <- mtcars; d$cyl <- factor(d$cyl)
  m <- reg_fit(d, reg_spec("mpg", "cyl", ref_levels = list(cyl = "8")))
  expect_equal(m$xlevels$cyl[1], "8")                        # 8 is the baseline
  expect_true("cyl4" %in% names(stats::coef(m)))
})

test_that("fit_model stays a thin wrapper (identical to the old direct lm)", {
  expect_equal(unname(stats::coef(fit_model(mtcars, "mpg", c("wt", "hp"), "multiple"))),
               unname(stats::coef(stats::lm(mpg ~ wt + hp, mtcars))))
  expect_length(stats::coef(fit_model(mtcars, "mpg", "wt", "polynomial", 2)), 3L)
})

# ---- coefficient table + fit stats ------------------------------------------

test_that("reg_coef_table matches summary() and carries a CI", {
  m  <- reg_fit(mtcars, reg_spec("mpg", c("wt", "hp")))
  ct <- reg_coef_table(m)
  expect_named(ct, c("Term", "Estimate", "Std. Error", "t value", "p",
                     "CI 2.5%", "CI 97.5%"))
  expect_equal(ct$Estimate, round(unname(stats::coef(m)), 4))
  ci <- stats::confint(m)
  expect_equal(ct[["CI 2.5%"]], round(unname(ci[, 1]), 4))
  # the CI must bracket the estimate
  expect_true(all(ct[["CI 2.5%"]] <= ct$Estimate & ct$Estimate <= ct[["CI 97.5%"]]))
  expect_null(reg_coef_table(NULL))
})

test_that("reg_fit_stats reports the headline numbers", {
  m  <- reg_fit(mtcars, reg_spec("mpg", c("wt", "hp")))
  fs <- reg_fit_stats(m)
  expect_named(fs, c("Statistic", "Value"))
  expect_equal(fs$Value[fs$Statistic == "N"], nrow(mtcars))
  expect_equal(fs$Value[fs$Statistic == "R-squared"], round(summary(m)$r.squared, 4))
  expect_equal(fs$Value[fs$Statistic == "AIC"], round(stats::AIC(m), 4))
  expect_null(reg_fit_stats(lm))   # not a model
})

# ---- reproducible code ------------------------------------------------------

test_that("reg_code parses, evaluates, and reproduces the fit", {
  d <- mtcars; d$am <- factor(d$am, labels = c("auto", "manual"))
  for (spec in list(
        reg_spec("mpg", c("wt", "hp")),
        reg_spec("mpg", c("wt", "am"), interactions = TRUE),
        reg_spec("mpg", "am", ref_levels = list(am = "manual")),
        reg_spec("mpg", "wt", poly_degree = 3))) {
    m    <- reg_fit(d, spec)
    code <- reg_code(m)
    expect_error(parse(text = code), NA)
    env <- new.env(parent = globalenv()); assign("df", d, envir = env)
    eval(parse(text = code), envir = env)
    expect_equal(stats::coef(get("model", envir = env)), stats::coef(m))
  }
  expect_null(reg_code(NULL))
})

test_that("reg_code emits the reference level the model actually used", {
  d <- mtcars; d$cyl <- factor(d$cyl)
  code <- reg_code(reg_fit(d, reg_spec("mpg", "cyl", ref_levels = list(cyl = "6"))))
  expect_match(code, 'ref = "6"', fixed = TRUE)
})

# ---- estimated marginal means (Phase 3b) ------------------------------------

test_that("reg_emmeans gives adjusted means + letters for a factor predictor", {
  d  <- mtcars; d$cyl <- factor(d$cyl)
  er <- reg_emmeans(reg_fit(d, reg_spec("mpg", c("wt", "cyl"))), "cyl")
  expect_true(er$ok)
  expect_true(all(c("cyl", "emmean", ".group") %in% names(er$cld)))
  expect_equal(nrow(er$cld), 3L)              # three cylinder levels
  expect_true(nzchar(er$held))                 # wt held at its mean
  expect_equal(nrow(er$pairs), 3L)             # 3 pairwise contrasts
})

test_that("reg_emmeans connecting letters separate the groups sensibly", {
  d  <- mtcars; d$cyl <- factor(d$cyl)
  er <- reg_emmeans(reg_fit(d, reg_spec("mpg", "cyl")), "cyl")
  grp <- setNames(trimws(er$cld$.group), as.character(er$cld$cyl))
  # 4-cyl mpg differs from 8-cyl -> they must not share a letter.
  expect_false(any(nchar(intersect(strsplit(grp[["4"]], "")[[1]],
                                    strsplit(grp[["8"]], "")[[1]])) > 0))
})

test_that("reg_emmeans guards non-factor and mismatched selections", {
  d <- mtcars; d$cyl <- factor(d$cyl)
  expect_false(reg_emmeans(reg_fit(mtcars, reg_spec("mpg", "wt")), "wt")$ok) # no factor
  expect_false(reg_emmeans(reg_fit(d, reg_spec("mpg", c("wt", "cyl"))), "wt")$ok) # wt not a factor
  expect_false(reg_emmeans(NULL, "cyl")$ok)
})

test_that("reg_emmeans result plots via lmer_emm_plot", {
  d  <- mtcars; d$cyl <- factor(d$cyl)
  er <- reg_emmeans(reg_fit(d, reg_spec("mpg", "cyl")), "cyl")
  p  <- lmer_emm_plot(er, "mpg", "none")
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("reg_emm_code parses and calls the emmeans engine", {
  code <- reg_emm_code("cyl", adjust = "tukey")
  expect_error(parse(text = code), NA)
  expect_match(code, "emmeans(model, ~ cyl)", fixed = TRUE)
  expect_match(code, "cld(", fixed = TRUE)
  expect_null(reg_emm_code(NULL))
})

# ---- diagnostics, assumptions, VIF (Phase 4) --------------------------------

test_that("the extra diagnostic plots are buildable ggplots", {
  m <- reg_fit(mtcars, reg_spec("mpg", c("wt", "hp")))
  for (fn in c(reg_qq_gg, reg_scale_loc_gg, reg_cooks_gg)) {
    p <- fn(m)
    expect_s3_class(p, "ggplot")
    expect_silent(ggplot2::ggplot_build(p))
  }
})

test_that("reg_assumptions returns the four verdicts", {
  a <- reg_assumptions(reg_fit(mtcars, reg_spec("mpg", c("wt", "hp"))))
  expect_named(a, c("Assumption", "Test", "Statistic", "p_value", "OK"))
  expect_setequal(a$Assumption,
                  c("Normality of residuals", "Constant variance",
                    "Linearity", "Independence (row order)"))
  expect_match(attr(a, "independence_note"), "time order")
  expect_null(reg_assumptions(NULL))
})

test_that("reg_assumptions flags a heteroscedastic fit (Breusch-Pagan)", {
  withr::local_seed(1)
  x <- 1:200; y <- x + stats::rnorm(200, sd = x / 6)   # variance grows with x
  bp <- reg_assumptions(stats::lm(y ~ x))
  expect_false(bp$OK[bp$Assumption == "Constant variance"])
})

test_that("reg_assumptions flags a curved (non-linear) fit", {
  withr::local_seed(2)
  x <- seq(-3, 3, length.out = 100); y <- x^2 + stats::rnorm(100, sd = 0.3)
  a <- reg_assumptions(stats::lm(y ~ x))                # straight line on a parabola
  expect_false(a$OK[a$Assumption == "Linearity"])
})

test_that("reg_vif matches car::vif and guards the small case", {
  m  <- reg_fit(mtcars, reg_spec("mpg", c("wt", "hp", "disp")))
  v  <- reg_vif(m)
  expect_named(v, c("Term", "VIF", "Concern"))
  skip_if_not_installed("car")
  expect_equal(v$VIF, round(unname(car::vif(m)), 3))
})

test_that("reg_vif needs at least two predictors", {
  expect_null(reg_vif(reg_fit(mtcars, reg_spec("mpg", "wt"))))
  expect_null(reg_vif(NULL))
})

test_that("reg_vif buckets concern at the 5 / 10 thresholds", {
  v <- reg_vif(reg_fit(mtcars, reg_spec("mpg", c("wt", "hp", "disp"))))
  expect_true(all(v$Concern %in% c("low", "moderate", "high", "n/a")))
  # disp is the collinear one here (VIF ~7.3 -> moderate)
  expect_equal(v$Concern[v$Term == "disp"], "moderate")
})

# ---- logistic regression + model comparison (Phase 5) -----------------------

make_logit_data <- function() {
  withr::local_seed(42)
  n <- 300; x1 <- stats::rnorm(n)
  grp <- factor(sample(c("A", "B", "C"), n, replace = TRUE))
  lp <- -0.5 + 1.2 * x1 + ifelse(grp == "B", 0.8, ifelse(grp == "C", -0.6, 0))
  y  <- stats::rbinom(n, 1, stats::plogis(lp))
  data.frame(y = y, yn = factor(ifelse(y == 1, "yes", "no")), x1 = x1, grp = grp)
}

test_that("reg_binary_ok detects binary responses", {
  expect_true(reg_binary_ok(c(0L, 1L, 1L, 0L)))
  expect_true(reg_binary_ok(factor(c("a", "b", "a"))))
  expect_true(reg_binary_ok(c(TRUE, FALSE, TRUE)))
  expect_false(reg_binary_ok(1:5))
  expect_false(reg_binary_ok(factor(c("a", "b", "c"))))
})

test_that("reg_validate enforces a binary response for logistic", {
  d <- make_logit_data()
  expect_length(reg_validate(d, reg_spec("yn", c("x1", "grp"), family = "binomial")), 0L)
  expect_length(reg_validate(d, reg_spec("y",  c("x1", "grp"), family = "binomial")), 0L)
  expect_gt(length(reg_validate(d, reg_spec("x1", "grp", family = "binomial"))), 0L)
})

test_that("reg_fit dispatches to glm for binomial and records the success level", {
  d <- make_logit_data()
  m <- reg_fit(d, reg_spec("yn", c("x1", "grp"), family = "binomial"))
  expect_s3_class(m, "glm")
  expect_equal(m$family$family, "binomial")
  expect_equal(attr(m, "success"), "yes")     # second sorted level
})

test_that("reg_odds_ratios equal exp(coef) and only apply to logistic glm", {
  d  <- make_logit_data()
  m  <- reg_fit(d, reg_spec("yn", c("x1", "grp"), family = "binomial"))
  or <- reg_odds_ratios(m)
  expect_named(or, c("Term", "Odds ratio", "CI 2.5%", "CI 97.5%", "p"))
  expect_equal(or[["Odds ratio"]], round(unname(exp(stats::coef(m))), 4))
  expect_null(reg_odds_ratios(reg_fit(mtcars, reg_spec("mpg", "wt"))))  # lm
})

test_that("logistic fit stats report deviance / McFadden / accuracy", {
  d  <- make_logit_data()
  m  <- reg_fit(d, reg_spec("yn", c("x1", "grp"), family = "binomial"))
  fs <- reg_fit_stats(m)
  expect_true(all(c("Null deviance", "Residual deviance", "McFadden R-sq",
                    "Accuracy (0.5)") %in% fs$Statistic))
  # McFadden = 1 - deviance/null.deviance for ungrouped 0/1 data
  expect_equal(fs$Value[fs$Statistic == "McFadden R-sq"],
               round(1 - m$deviance / m$null.deviance, 4))
  expect_true(fs$Value[fs$Statistic == "Accuracy (0.5)"] >= 0 &&
              fs$Value[fs$Statistic == "Accuracy (0.5)"] <= 1)
})

test_that("model_interpretation is glm-aware and assumptions are linear-only", {
  d  <- make_logit_data()
  m  <- reg_fit(d, reg_spec("yn", "x1", family = "binomial"))
  info <- model_interpretation(m)
  expect_equal(info$family, "binomial")
  expect_true(is.na(info$adj_r2))
  expect_true(info$overall_p < 0.05)          # x1 clearly predicts y
  expect_null(reg_assumptions(m))              # no linear-model assumptions
})

test_that("reg_code emits a glm call for logistic", {
  d <- make_logit_data()
  m <- reg_fit(d, reg_spec("yn", c("x1", "grp"), family = "binomial"))
  code <- reg_code(m)
  expect_match(code, "family = binomial", fixed = TRUE)
  expect_match(code, "odds ratio", fixed = TRUE)
  expect_error(parse(text = code), NA)
})

test_that("reg_compare runs a nested F (lm) and LRT (glm)", {
  a <- reg_fit(mtcars, reg_spec("mpg", "wt"))
  b <- reg_fit(mtcars, reg_spec("mpg", c("wt", "hp")))
  cmp <- reg_compare(a, b)
  expect_true(is.data.frame(cmp$table))
  expect_true("Pr(>F)" %in% names(cmp$table))
  expect_true(!is.null(cmp$aic_delta))

  d  <- make_logit_data()
  ga <- reg_fit(d, reg_spec("yn", "x1", family = "binomial"))
  gb <- reg_fit(d, reg_spec("yn", c("x1", "grp"), family = "binomial"))
  cg <- reg_compare(ga, gb)
  expect_true("Pr(>Chi)" %in% names(cg$table))
})

test_that("reg_compare guards mixed families and missing models", {
  a  <- reg_fit(mtcars, reg_spec("mpg", "wt"))
  d  <- make_logit_data()
  ga <- reg_fit(d, reg_spec("yn", "x1", family = "binomial"))
  expect_match(reg_compare(a, ga)$warnings[1], "different kinds")
  expect_match(reg_compare(NULL, a)$warnings[1], "Save a model")
})
