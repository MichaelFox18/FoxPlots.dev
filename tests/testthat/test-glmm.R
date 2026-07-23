# Tests for the GLMM helpers in helpers_glmm.R. The formula / family-registry /
# code helpers are pure (base + stats) and run without glmmTMB. The engine
# helpers (glmm_fit / glmm_emmeans / glmm_anova / glmm_dharma*) are validated
# against direct glmmTMB calls on the seeded example data and gated with
# skip_if_not_installed() so the suite still runs without the GLMM stack.

# ---- family registry ------------------------------------------------------

test_that("family registry keys are consistent across choices/notes/code", {
  keys <- unname(GLMM_FAMILY_CHOICES)
  expect_setequal(names(GLMM_FAMILY_NOTE), keys)
  for (k in keys) expect_match(glmm_family_code(k), "\\(link = ")
})

test_that("glmm_family builds the base-R families without glmmTMB", {
  expect_equal(glmm_family("gaussian_log")$link, "log")
  expect_equal(glmm_family("poisson_log")$family, "poisson")
  expect_equal(glmm_family("Gamma_log")$family, "Gamma")
  expect_equal(glmm_family("unknown")$family, "gaussian")
})

test_that("glmm_family builds the glmmTMB-specific families", {
  skip_if_not_installed("glmmTMB")
  expect_equal(glmm_family("nbinom2_log")$family, "nbinom2")
  expect_equal(glmm_family("beta_logit")$link, "logit")
  expect_equal(glmm_family("tweedie_log")$family, "tweedie")
})

# ---- domain check ---------------------------------------------------------

test_that("glmm_domain_check flags out-of-range responses", {
  expect_null(glmm_domain_check(c(-1, 0, 2.5), "gaussian_identity"))
  expect_match(glmm_domain_check(c(0, 1, 2), "Gamma_log"), "valid range")
  expect_null(glmm_domain_check(c(0.5, 1, 2), "Gamma_log"))
  expect_match(glmm_domain_check(c(1, 2.5), "poisson_log"), "valid range")
  expect_null(glmm_domain_check(c(0, 1, 2), "nbinom2_log"))
  expect_match(glmm_domain_check(c(-0.1, 3), "tweedie_log"), "valid range")
  expect_match(glmm_domain_check(c(0, 0.5), "beta_logit"), "valid range")
  expect_null(glmm_domain_check(c(0.001, 0.999), "beta_logit"))
  expect_match(glmm_domain_check(c(0, 1, 2), "binary"), "0 and 1")
  expect_null(glmm_domain_check(c(0, 1, NA), "binary"))
  expect_null(glmm_domain_check(letters, "Gamma_log"))   # non-numeric: NULL
})

# ---- formula builders -----------------------------------------------------

test_that("build_side_formula covers off / intercept-only / predictors", {
  expect_equal(build_side_formula(character(0), enabled = FALSE), "~0")
  expect_equal(build_side_formula(c("a", "b"), enabled = FALSE), "~0")
  expect_equal(build_side_formula(character(0), enabled = TRUE), "~1")
  expect_equal(build_side_formula(NULL, enabled = TRUE), "~1")
  f <- gsub("`", "", build_side_formula(c("Treatment", "my var"), TRUE))
  expect_equal(f, "~ Treatment + my var")
  expect_silent(stats::as.formula(build_side_formula(c("Treatment", "my var"),
                                                     TRUE)))
})

test_that("glmm_formula_string handles fixed/random/interactions/slope", {
  f <- gsub("`", "", glmm_formula_string("y", c("A", "B"), "G"))
  expect_equal(f, "y ~ A + B + (1 | G)")
  f2 <- gsub("`", "", glmm_formula_string("y", c("A", "B"), "G",
                                          interactions = TRUE))
  expect_match(f2, "A \\* B")
  f3 <- gsub("`", "", glmm_formula_string("y", "A", character(0)))
  expect_equal(f3, "y ~ A")                       # no random part -> plain GLM
  f4 <- gsub("`", "", glmm_formula_string("y", character(0), "G"))
  expect_equal(f4, "y ~ 1 + (1 | G)")
  f5 <- gsub("`", "", glmm_formula_string("y", "A", c("G1", "G2"),
                                          slope = "A", slope_group = "G1"))
  expect_match(f5, "1 \\+ A \\| G1")
  expect_match(f5, "\\(1 \\| G2\\)")
  expect_silent(stats::as.formula(glmm_formula_string("my y", "my x", "my g")))
})

# ---- example data ---------------------------------------------------------

test_that("make_glmm_example_data is seeded, well-shaped, and in-domain", {
  d <- make_glmm_example_data()
  expect_equal(nrow(d), 144L)                     # 8 sites x 3 trt x 2 seasons x 3 reps
  expect_named(d, c("Site", "Treatment", "Season", "insect_count",
                    "seedling_count", "cover_prop", "present"))
  expect_s3_class(d$Site, "factor")
  expect_true(all(d$insect_count >= 0 & d$insect_count == round(d$insect_count)))
  expect_true(all(d$seedling_count >= 0))
  expect_gt(mean(d$seedling_count == 0), 0.05)    # genuinely zero-heavy
  expect_true(all(d$cover_prop > 0 & d$cover_prop < 1))
  expect_true(all(d$present %in% c(0, 1)))
  expect_identical(d, make_glmm_example_data())   # seeded -> reproducible
})

test_that("make_glmm_example_data does not disturb the session RNG", {
  set.seed(99); x1 <- runif(1)
  set.seed(99); invisible(make_glmm_example_data()); x2 <- runif(1)
  expect_identical(x1, x2)
})

# ---- fit engine (validated against direct glmmTMB calls) ------------------

test_that("glmm_fit matches a direct glmmTMB call (nbinom2)", {
  skip_if_not_installed("glmmTMB")
  d <- make_glmm_example_data()
  fit <- glmm_fit(d, list(response = "insect_count", fixed = "Treatment",
                          random = "Site", family_key = "nbinom2_log"))
  expect_true(fit$ok)
  ref <- suppressWarnings(glmmTMB::glmmTMB(
    insect_count ~ Treatment + (1 | Site),
    family = glmmTMB::nbinom2(link = "log"), data = d))
  expect_equal(stats::AIC(fit$mod), stats::AIC(ref), tolerance = 1e-6)
  expect_equal(unname(glmmTMB::fixef(fit$mod)$cond),
               unname(glmmTMB::fixef(ref)$cond), tolerance = 1e-6)
})

test_that("glmm_fit fits a zero-inflation model when asked", {
  skip_if_not_installed("glmmTMB")
  d <- make_glmm_example_data()
  fit <- glmm_fit(d, list(response = "seedling_count", fixed = "Treatment",
                          random = "Site", family_key = "poisson_log",
                          zi_on = TRUE, zi_vars = character(0)))
  expect_true(fit$ok)
  expect_equal(fit$zi_str, "~1")
  expect_true(length(glmmTMB::fixef(fit$mod)$zi) >= 1)   # zi intercept present
})

test_that("glmm_fit binary path recodes a two-level factor and notes it", {
  skip_if_not_installed("glmmTMB")
  d <- make_glmm_example_data()
  d$present_f <- factor(ifelse(d$present == 1, "yes", "no"))
  fit <- glmm_fit(d, list(response = "present_f", fixed = "Treatment",
                          random = "Site", binary = TRUE, link = "logit"))
  expect_true(fit$ok)
  expect_equal(fit$success_level, "yes")
  expect_true(any(grepl("yes", fit$notes)))
  expect_true(all(fit$data$present_f %in% c(0, 1)))
  ref <- suppressWarnings(glmmTMB::glmmTMB(
    present ~ Treatment + (1 | Site),
    family = stats::binomial(link = "logit"), data = d))
  expect_equal(stats::AIC(fit$mod), stats::AIC(ref), tolerance = 1e-6)
})

test_that("glmm_fit surfaces degenerate specs as errors, not crashes", {
  skip_if_not_installed("glmmTMB")
  d <- make_glmm_example_data()
  expect_false(glmm_fit(d, list(response = "nope"))$ok)
  d$three <- factor(rep(c("a", "b", "c"), length.out = nrow(d)))
  expect_false(glmm_fit(d, list(response = "three", binary = TRUE))$ok)
  bad <- glmm_fit(d, list(response = "insect_count", binary = TRUE))
  expect_false(bad$ok)                             # counts are not 0/1
})

test_that("glmm_fit notes the GLM fallback when no random effect is chosen", {
  skip_if_not_installed("glmmTMB")
  d <- make_glmm_example_data()
  fit <- glmm_fit(d, list(response = "insect_count", fixed = "Treatment",
                          random = character(0), family_key = "poisson_log"))
  expect_true(fit$ok)
  expect_true(any(grepl("ordinary GLM", fit$notes)))
})

test_that("glmm_fit_stats returns the one-row summary", {
  skip_if_not_installed("glmmTMB")
  d <- make_glmm_example_data()
  fit <- glmm_fit(d, list(response = "insect_count", fixed = "Treatment",
                          random = "Site", family_key = "nbinom2_log"))
  st <- glmm_fit_stats(fit$mod, n_total = nrow(d))
  expect_named(st, c("AIC", "BIC", "logLik", "df_resid", "n_obs_used",
                     "n_obs_total", "n_dropped_NA"))
  expect_equal(st$n_dropped_NA, 0L)
  expect_equal(st$AIC, round(stats::AIC(fit$mod), 2))
})

# ---- ANOVA / dispersion ---------------------------------------------------

test_that("glmm_anova returns a Type III Wald table via car", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("car")
  d <- make_glmm_example_data()
  fit <- glmm_fit(d, list(response = "insect_count", fixed = "Treatment",
                          random = "Site", family_key = "nbinom2_log"))
  a <- glmm_anova(fit$mod)
  expect_true(a$ok)
  expect_true("Treatment" %in% a$table$Term)
  expect_true(any(grepl("Chisq", names(a$table))))
})

test_that("glmm_pearson_ratio is a finite positive number for a count fit", {
  skip_if_not_installed("glmmTMB")
  d <- make_glmm_example_data()
  fit <- glmm_fit(d, list(response = "insect_count", fixed = "Treatment",
                          random = "Site", family_key = "poisson_log"))
  r <- glmm_pearson_ratio(fit$mod)
  expect_true(is.finite(r) && r > 0)
})

# ---- DHARMa ---------------------------------------------------------------

test_that("glmm_dharma simulates residuals and runs the test battery", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("DHARMa")
  d <- make_glmm_example_data()
  fit <- glmm_fit(d, list(response = "insect_count", fixed = "Treatment",
                          random = "Site", family_key = "nbinom2_log"))
  sim <- glmm_dharma(fit$mod, n = 50)
  expect_true(sim$ok)
  expect_s3_class(sim$sim, "DHARMa")
  tests <- glmm_dharma_tests(sim$sim, binary = FALSE)
  expect_named(tests, c("uniformity", "dispersion", "outliers",
                        "zero_inflation"))
  expect_s3_class(tests$dispersion, "htest")
  expect_s3_class(tests$uniformity, "htest")
  binry <- glmm_dharma_tests(sim$sim, binary = TRUE)
  expect_false("zero_inflation" %in% names(binry))
  # the two-panel plot draws cleanly and leaves no device open
  n_dev <- length(grDevices::dev.list())
  f <- withr::local_tempfile(fileext = ".png")
  grDevices::png(f)
  glmm_dharma_plot(sim$sim)
  grDevices::dev.off()
  expect_true(file.exists(f) && file.size(f) > 0)
  expect_equal(length(grDevices::dev.list()), n_dev)
})

test_that("glmm_dharma does not disturb the session RNG", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("DHARMa")
  d <- make_glmm_example_data()
  fit <- glmm_fit(d, list(response = "insect_count", fixed = "Treatment",
                          random = "Site", family_key = "nbinom2_log"))
  set.seed(7); x1 <- runif(1)
  set.seed(7); invisible(glmm_dharma(fit$mod, n = 20)); x2 <- runif(1)
  expect_identical(x1, x2)
})

# ---- EMMeans --------------------------------------------------------------

test_that("glmm_emmeans returns response-scale means, letters, and pairs", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("emmeans")
  skip_if_not_installed("multcomp")
  skip_if_not_installed("multcompView")
  d <- make_glmm_example_data()
  fit <- glmm_fit(d, list(response = "insect_count", fixed = "Treatment",
                          random = "Site", family_key = "nbinom2_log"))
  em <- glmm_emmeans(fit, "Treatment")
  expect_true(em$ok)
  expect_true("response" %in% names(em$cld))       # response scale (counts)
  expect_true(".group" %in% names(em$cld))
  expect_equal(nrow(em$cld), 3L)
  expect_false(any(grepl("^\\s|\\s$", em$cld$.group)))   # letters trimmed
  expect_equal(nrow(em$pairs), 3L)                 # 3 pairwise contrasts
  expect_equal(sum(em$counts$n), nrow(d))
})

test_that("glmm_emmeans honours a by-factor and validates its inputs", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("emmeans")
  skip_if_not_installed("multcomp")
  skip_if_not_installed("multcompView")
  d <- make_glmm_example_data()
  fit <- glmm_fit(d, list(response = "insect_count",
                          fixed = c("Treatment", "Season"),
                          random = "Site", family_key = "nbinom2_log"))
  em <- glmm_emmeans(fit, c("Treatment", "Season"), by = "Season")
  expect_true(em$ok)
  expect_equal(em$by_var, "Season")
  expect_equal(nrow(em$cld), 6L)                   # 3 trt x 2 seasons
  expect_false(glmm_emmeans(fit, character(0))$ok)
  expect_false(glmm_emmeans(fit, "NotInModel")$ok)
  expect_false(glmm_emmeans(NULL, "Treatment")$ok)
})

# ---- code generators ------------------------------------------------------

test_that("glmm_code emits a parseable script mirroring the fit", {
  skip_if_not_installed("glmmTMB")
  d <- make_glmm_example_data()
  fit <- glmm_fit(d, list(response = "insect_count", fixed = "Treatment",
                          random = "Site", family_key = "nbinom2_log",
                          zi_on = TRUE, zi_vars = "Treatment"))
  code <- glmm_code(fit)
  expect_match(code, "glmmTMB\\(")
  expect_match(code, "nbinom2\\(link = \"log\"\\)")
  expect_match(code, "ziformula   = ~ Treatment")
  expect_match(code, "testZeroInflation")
  expect_match(code, "car::Anova")
  expect_silent(parse(text = code))
  expect_equal(glmm_code(NULL), "")
  expect_equal(glmm_code(list(ok = FALSE)), "")
})

test_that("binary glmm_code drops the zero-inflation test and names the link", {
  skip_if_not_installed("glmmTMB")
  d <- make_glmm_example_data()
  fit <- glmm_fit(d, list(response = "present", fixed = "Treatment",
                          random = "Site", binary = TRUE, link = "probit"))
  code <- glmm_code(fit)
  expect_match(code, "binomial\\(link = \"probit\"\\)")
  expect_no_match(code, "testZeroInflation")
  expect_silent(parse(text = code))
})

test_that("glmm_emm_code parameterizes spec, adjust, and level", {
  code <- glmm_emm_code(c("Treatment", "Season"), by = "Season",
                        adjust = "sidak", level = 0.9)
  expect_match(code, "\\| ")
  expect_match(code, "adjust = \"sidak\"")
  expect_match(code, "level = 0.9")
  expect_silent(parse(text = code))
  expect_equal(glmm_emm_code(character(0)), "")
})

# ---- module smoke test (testServer works on the macOS dev box) ------------

test_that("glmmServer fits on example data and returns the augmented frame", {
  skip_if_not_installed("glmmTMB")
  d <- make_glmm_example_data()
  shiny::testServer(glmmServer, args = list(data_in = shiny::reactive(d),
                                            binary = FALSE), {
    session$setInputs(response = "insect_count", family = "nbinom2_log",
                      fixed = "Treatment", random = "Site",
                      interactions = FALSE, zi_on = FALSE,
                      adjust = "tukey", conf = 0.95, emm_by = "",
                      run = 1)
    expect_true(rv$fit$ok)
    expect_match(rv$fit$fml_str, "insect_count")
    expect_equal(nrow(session$returned()), nrow(d))
    # combined-variable builder augments the returned frame
    session$setInputs(combo_vars = c("Treatment", "Season"), combo_sep = ".",
                      combo_add = 1)
    expect_true("Treatment.Season" %in% names(session$returned()))
  })
})

test_that("glmmServer binary instance recodes and fits a 0/1 model", {
  skip_if_not_installed("glmmTMB")
  d <- make_glmm_example_data()
  shiny::testServer(glmmServer, args = list(data_in = shiny::reactive(d),
                                            binary = TRUE), {
    session$setInputs(response = "present", link = "logit",
                      fixed = "Treatment", random = "Site",
                      interactions = FALSE, zi_on = FALSE,
                      adjust = "tukey", conf = 0.95, emm_by = "",
                      run = 1)
    expect_true(rv$fit$ok)
    expect_true(rv$fit$binary)
  })
})

test_that("lmer_emm_plot handles binomial (prob) and count (response) scales", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("emmeans")
  skip_if_not_installed("multcomp")
  skip_if_not_installed("multcompView")
  d <- make_glmm_example_data()
  fit_b <- glmm_fit(d, list(response = "present", fixed = "Treatment",
                            random = "Site", binary = TRUE, link = "logit"))
  em_b <- glmm_emmeans(fit_b, "Treatment")
  expect_true("prob" %in% names(em_b$cld))         # binomial names it prob
  p <- lmer_emm_plot(em_b, "present", "none")
  expect_s3_class(p, "ggplot")
  fit_p <- glmm_fit(d, list(response = "insect_count", fixed = "Treatment",
                            random = "Site", family_key = "poisson_log"))
  em_p <- glmm_emmeans(fit_p, "Treatment")
  p2 <- lmer_emm_plot(em_p, "insect_count", "none")
  expect_s3_class(p2, "ggplot")
})
