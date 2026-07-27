# Tests for the mixed-model helpers in helpers_lmer.R. The formula / spec / code
# helpers are pure (base + stats) and run without lmerTest. The engine helpers
# (lmer_fit / lmer_emmeans / lmer_anova / lmer_compare / lmer_fit_stats) are
# checked against the same reference identities as validate_engine.R and are
# gated with skip_if_not_installed() so the suite still runs without the heavy
# mixed-model stack.

# ---- pure helpers ---------------------------------------------------------

test_that("build_formula_string assembles fixed + random parts (additive)", {
  f <- build_formula_string("y", "none", c("A", "B"), "G", interactions = FALSE)
  fb <- gsub("`", "", f)                          # bq only quotes non-syntactic
  expect_match(fb, "A \\+ B")
  expect_match(fb, "\\(1 \\| G\\)")
  expect_silent(stats::as.formula(f))
})

test_that("interactions = TRUE crosses fixed effects with *", {
  f <- gsub("`", "", build_formula_string("y", "none", c("A", "B"), "G", TRUE))
  expect_match(f, "A \\* B")
})

test_that("build_lhs applies the response transform", {
  expect_equal(gsub("`", "", build_lhs("y", "none")), "y")
  expect_match(build_lhs("y", "log"),  "^log\\(")
  expect_match(build_lhs("y", "asin"), "asin\\(sqrt")
  expect_match(build_lhs("y", "inverse"), "I\\(1/")
})

test_that("random slope only attaches to slope_group", {
  r <- gsub("`", "", build_random_part(c("G1", "G2"), slope = "x",
                                       slope_group = "G1"))
  expect_match(r, "1 \\+ x \\| G1")
  expect_match(r, "\\(1 \\| G2\\)")
})

test_that("emm_spec_text builds ~ A | B for a by-factor", {
  expect_match(gsub("`", "", emm_spec_text(c("A", "B"), "B")), "~ A \\| B")
  expect_no_match(emm_spec_text(c("A", "B"), character(0)), "\\|")
})

test_that("emm_roles maps x / colour / facet", {
  r <- emm_roles(c("A", "B", "C"), "C")
  expect_equal(r$x, "A")
  expect_equal(r$colour, "B")
  expect_equal(r$facet, "C")          # the by-factor becomes the facet
})

test_that("tran_for_emmeans maps the non-auto transforms", {
  expect_null(tran_for_emmeans("none"))
  expect_equal(tran_for_emmeans("asin"), "asin.sqrt")
  expect_equal(tran_for_emmeans("inverse"), "inverse")
  expect_equal(tran_for_emmeans("log"), "log")
})

test_that("round_df keeps small p-values via signif", {
  out <- round_df(data.frame(p.value = 2e-6, est = 1.23456))
  expect_gt(out$p.value, 0)
  expect_equal(out$est, 1.2346)
})

test_that("make_combined_factor pastes columns into a factor", {
  d <- data.frame(A = c("x", "y"), B = c("1", "2"), stringsAsFactors = FALSE)
  f <- make_combined_factor(d, c("A", "B"), sep = ".")
  expect_s3_class(f, "factor")
  expect_equal(as.character(f), c("x.1", "y.2"))
})

test_that("make_combined_factor errors on fewer than two columns", {
  expect_error(make_combined_factor(mtcars, "mpg"))
  expect_error(make_combined_factor(mtcars, c("mpg", "nope")), "not found")
})

test_that("lmer_validate flags missing fixed / random effects", {
  d <- make_example_data()
  p_fixed <- lmer_validate(d, list(response = "yield_kg", fixed = character(0),
                                   random = "Block", transform = "none"))
  expect_match(p_fixed[1], "fixed")
  p_rand <- lmer_validate(d, list(response = "yield_kg", fixed = "Variety",
                                  random = character(0), transform = "none"))
  expect_match(p_rand[1], "random")
})

test_that("lmer_validate flags an impossible log transform", {
  d <- data.frame(y = c(-1, 2, 3),
                  A = factor(c("a", "b", "a")),
                  G = factor(c("g1", "g2", "g1")))
  probs <- lmer_validate(d, list(response = "y", transform = "log",
                                 fixed = "A", random = "G"))
  expect_true(any(grepl("log", probs)))
})

test_that("lmer_validate passes a sound spec", {
  d <- make_example_data()
  d$Nitrogen <- factor(d$Nitrogen)
  expect_length(lmer_validate(d, list(response = "yield_kg", transform = "none",
                                      fixed = c("Variety", "Nitrogen"),
                                      random = "Block")), 0L)
})

test_that("lmer_code emits lmer / anova / emmeans with the right type", {
  fit <- list(fml_str = "y ~ A + (1 | G)", reml = TRUE, transform = "none",
              cat_fixed = "A", sum_contrasts = TRUE, ddf = "Kenward-Roger",
              atype = "3")
  code <- lmer_code(fit, emmvars = "A")
  expect_match(code, "lmer\\(")
  expect_match(code, 'type = "III"')
  expect_match(code, "contr.sum")
  expect_match(code, "emmeans\\(")
})

test_that("make_example_data is a valid 3-factor RCBD", {
  d <- make_example_data()
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 6L * 3L * 3L * 2L)
  expect_true(all(c("Block", "Variety", "Nitrogen", "Irrigation",
                    "yield_kg", "germination", "pest_count") %in% names(d)))
  expect_true(is.numeric(d$Nitrogen))      # stored numeric on purpose
})

# ---- engine helpers vs the reference oracle (skip-gated) -------------------

spec_chick <- function(...) utils::modifyList(
  list(response = "weight", transform = "none", fixed = "Diet",
       random = "Chick", interactions = FALSE, slope = NULL,
       slope_group = NULL, reml = TRUE, anova_type = "3",
       ddf = "Satterthwaite"), list(...))

test_that("lmer_fit reproduces sleepstudy fixed effects (oracle sec 1)", {
  skip_if_not_installed("lmerTest")
  skip_if_not_installed("lme4")
  utils::data(sleepstudy, package = "lme4")
  res <- lmer_fit(sleepstudy, list(
    response = "Reaction", transform = "none", fixed = "Days",
    random = "Subject", interactions = FALSE, slope = "Days",
    slope_group = "Subject", reml = TRUE, anova_type = "2",
    ddf = "Satterthwaite"))
  expect_true(res$ok)
  fe <- lme4::fixef(res$mod)
  expect_lt(abs(fe[["(Intercept)"]] - 251.41), 0.5)
  expect_lt(abs(fe[["Days"]] - 10.47), 0.1)
})

test_that("lmer_fit nobs equals the complete-case count (oracle sec 2)", {
  skip_if_not_installed("lmerTest")
  cw  <- as.data.frame(ChickWeight)
  res <- lmer_fit(cw, spec_chick())
  expect_true(res$ok)
  expect_equal(stats::nobs(res$mod),
               sum(stats::complete.cases(cw[, c("weight", "Diet", "Chick")])))
})

test_that("lmer_emmeans preserves the ChickWeight diet ordering (oracle sec 2)", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("multcomp")
  cw  <- as.data.frame(ChickWeight)
  res <- lmer_fit(cw, spec_chick())
  er  <- lmer_emmeans(res, "Diet")
  expect_true(er$ok)
  obs <- tapply(cw$weight, cw$Diet, mean)
  expect_identical(order(as.data.frame(er$emm)$emmean),
                   order(as.numeric(obs)))
})

test_that("log back-transform: response equals exp(link) (oracle sec 3)", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("multcomp")
  cw   <- as.data.frame(ChickWeight)
  res  <- lmer_fit(cw, spec_chick(transform = "log"))
  link <- summary(lmer_emmeans(res, "Diet", backtransform = FALSE)$emm)$emmean
  resp <- summary(lmer_emmeans(res, "Diet", backtransform = TRUE)$emm)$response
  expect_lt(max(abs(resp - exp(link))), 1e-6)
})

test_that("cld assigns a letter to every level (oracle sec 4)", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("multcomp")
  cw  <- as.data.frame(ChickWeight)
  er  <- lmer_emmeans(lmer_fit(cw, spec_chick()), "Diet")
  expect_true(all(nchar(trimws(er$cld$.group)) >= 1))
})

test_that("lmer_anova returns a finite F (oracle sec 2)", {
  skip_if_not_installed("lmerTest")
  cw <- as.data.frame(ChickWeight)
  a  <- lmer_anova(lmer_fit(cw, spec_chick())$mod, type = "3",
                   ddf = "Satterthwaite")
  expect_null(a$error)
  expect_true(is.finite(a$table[["F value"]][1]))
})

test_that("lmer_compare reproduces the ML LRT chi-square identity (oracle sec 5)", {
  skip_if_not_installed("lmerTest")
  cw   <- as.data.frame(ChickWeight)
  fitA <- lmer_fit(cw, spec_chick(fixed = character(0)))   # intercept only
  fitB <- lmer_fit(cw, spec_chick())                       # + Diet
  cmp  <- lmer_compare(fitA, fitB)
  expect_null(cmp$error)
  llA  <- as.numeric(stats::logLik(stats::update(fitA$mod, REML = FALSE)))
  llB  <- as.numeric(stats::logLik(stats::update(fitB$mod, REML = FALSE)))
  row  <- which(is.finite(cmp$anova$Chisq))[1]
  expect_lt(abs(cmp$anova$Chisq[row] - 2 * (llB - llA)), 1e-4)
})

test_that("lmer_fit_stats R2 identities hold (oracle sec 7)", {
  skip_if_not_installed("lmerTest")
  skip_if_not_installed("performance")
  cw <- as.data.frame(ChickWeight)
  s  <- lmer_fit_stats(lmer_fit(cw, spec_chick())$mod, nrow(cw), reml = TRUE)
  rm <- s$Value[s$Statistic == "R2_marginal"]
  rc <- s$Value[s$Statistic == "R2_conditional"]
  skip_if(length(rm) == 0 || length(rc) == 0)
  expect_lte(rm, rc + 1e-8)
})

test_that("validate messages carry real glyphs, not corrupted escape digits", {
  # Regression: ten unicode escapes in helpers_lmer.R once lost their backslash
  # and rendered literal '2014' / '2212' / '00b1' digits to users.
  d <- make_example_data()
  p_rand <- lmer_validate(d, list(response = "yield_kg", fixed = "Variety",
                                  random = character(0), transform = "none"))
  expect_match(p_rand[1], intToUtf8(0x2014L), fixed = TRUE)   # a real em dash
  expect_false(grepl("2014", p_rand[1], fixed = TRUE))        # not bare digits
  d2 <- data.frame(y = c(-2, 2, 3), A = factor(c("a", "b", "a")),
                   G = factor(c("g1", "g2", "g1")))
  probs <- lmer_validate(d2, list(response = "y", transform = "log1p",
                                  fixed = "A", random = "G"))
  expect_match(probs[1], intToUtf8(0x2212L), fixed = TRUE)    # a real minus
  expect_false(grepl("2212", probs[1], fixed = TRUE))
})

test_that("make_example_data leaves the session RNG stream untouched", {
  set.seed(999)
  before <- .Random.seed
  invisible(make_example_data())
  expect_identical(.Random.seed, before)
  expect_identical(make_example_data(), make_example_data())  # still deterministic
})

test_that("lmer_report_payload summarises a fit for the report", {
  skip_if_not_installed("lmerTest")
  d <- make_example_data()
  fit <- lmer_fit(d, list(response = "yield_kg", fixed = "Variety",
                          random = "Block", reml = TRUE, anova_type = "3",
                          ddf = "Satterthwaite"))
  skip_if_not(isTRUE(fit$ok))
  pl <- lmer_report_payload(fit, code = "lmer(...)")
  expect_equal(pl$title, "Mixed model (lmerTest)")
  expect_match(unname(pl$formulas[["Model"]]), "yield_kg")
  expect_true("Fit statistics" %in% names(pl$tables))
  expect_true(any(grepl("ANOVA", names(pl$tables))))
  expect_true("Variance components" %in% names(pl$tables))
  expect_equal(pl$code, "lmer(...)")
  expect_null(lmer_report_payload(NULL))
  expect_null(lmer_report_payload(list(ok = FALSE)))
})

test_that("report payload never prints a correlation as a variance", {
  skip_if_not_installed("lmerTest")
  d <- make_example_data()
  fit <- suppressWarnings(lmer_fit(d, list(
    response = "yield_kg", fixed = "Nitrogen", random = "Block",
    slope = "Nitrogen", slope_group = "Block", reml = TRUE,
    anova_type = "3", ddf = "Satterthwaite")))
  skip_if_not(isTRUE(fit$ok))
  pl <- suppressWarnings(lmer_report_payload(fit))
  vc <- pl$tables[["Variance components"]]
  expect_named(vc, c("Group", "Term", "Variance", "SD"))
  # variances and SDs are non-negative by definition: a correlation row
  # leaking in here is exactly what produced negative values before
  expect_true(all(vc$Variance >= 0, na.rm = TRUE))
  expect_true(all(vc$SD >= 0, na.rm = TRUE))
  expect_equal(anyDuplicated(paste(vc$Group, vc$Term)), 0L)
  # the intercept-slope pair is reported separately, labelled for what it is
  cors <- pl$tables[["Random-effect correlations"]]
  expect_named(cors, c("Group", "Term 1", "Term 2", "Covariance", "Correlation"))
  expect_true(all(abs(cors$Correlation) <= 1))
  # an intercept-only fit has no correlation table at all
  f2 <- lmer_fit(d, list(response = "yield_kg", fixed = "Variety",
                         random = "Block", reml = TRUE, anova_type = "3",
                         ddf = "Satterthwaite"))
  expect_false("Random-effect correlations" %in%
                 names(lmer_report_payload(f2)$tables))
})

test_that("report Fit statistics carries every statistic, not two stray rows", {
  skip_if_not_installed("lmerTest")
  d <- make_example_data()
  fit <- lmer_fit(d, list(response = "yield_kg", fixed = "Variety",
                          random = "Block", reml = TRUE, anova_type = "3",
                          ddf = "Satterthwaite"))
  skip_if_not(isTRUE(fit$ok))
  st <- pl <- NULL
  st <- lmer_fit_stats(fit$mod, nrow(fit$data), fit$reml)
  pl <- lmer_report_payload(fit)$tables[["Fit statistics"]]
  expect_identical(pl, st)                       # report == the module's tab
  expect_gt(nrow(pl), 5L)
  expect_true(all(c("AIC", "BIC", "logLik") %in% pl$Statistic))
  expect_false(any(pl$Statistic == "Statistic"))  # the old garbled header row
})
