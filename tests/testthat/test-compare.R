# Tests for the group-comparison helpers in helpers_compare.R. All base/stats,
# so they run without Shiny or ggplot2. Results are checked against the raw
# stats:: tests they wrap.

# ---- compare_groups_numeric --------------------------------------------------

test_that("two-group parametric matches Welch t.test", {
  set.seed(1)
  df  <- data.frame(y = c(rnorm(20, 10), rnorm(20, 12)),
                    g = rep(c("A", "B"), each = 20))
  res <- compare_groups_numeric(df, "y", "g", parametric = TRUE)
  expect_match(res$test, "Welch")
  expect_equal(res$k, 2L)
  expect_equal(res$p_value, t.test(y ~ g, data = df)$p.value)
  expect_equal(res$effect_name, "Cohen's d")
  expect_false(is.na(res$effect_value))
  expect_null(res$posthoc)
})

test_that("equal-variance flag switches to Student's t-test", {
  df  <- data.frame(y = c(1, 2, 3, 4, 8, 9, 10, 11), g = rep(c("A", "B"), each = 4))
  res <- compare_groups_numeric(df, "y", "g", parametric = TRUE, var_equal = TRUE)
  expect_match(res$test, "equal variance")
  expect_equal(res$p_value, t.test(y ~ g, data = df, var.equal = TRUE)$p.value)
})

test_that("3+ groups parametric gives ANOVA + Tukey + eta-squared", {
  set.seed(2)
  df  <- data.frame(y = c(rnorm(15, 5), rnorm(15, 7), rnorm(15, 9)),
                    g = rep(c("A", "B", "C"), each = 15))
  res <- compare_groups_numeric(df, "y", "g", parametric = TRUE)
  expect_equal(res$test, "One-way ANOVA")
  expect_equal(res$effect_name, "Eta-squared")
  expect_s3_class(res$posthoc, "data.frame")
  expect_equal(nrow(res$posthoc), 3L)            # A-B, A-C, B-C
  aov_p <- summary(aov(y ~ g, data = df))[[1]][["Pr(>F)"]][1]
  expect_equal(res$p_value, aov_p)
})

test_that("non-parametric paths pick Wilcoxon / Kruskal–Wallis", {
  df2 <- data.frame(y = c(1, 2, 3, 9, 10, 11), g = rep(c("A", "B"), each = 3))
  expect_match(compare_groups_numeric(df2, "y", "g", parametric = FALSE)$test,
               "Wilcoxon")
  df3 <- data.frame(y = c(1, 2, 5, 6, 9, 10), g = rep(c("A", "B", "C"), each = 2))
  expect_match(compare_groups_numeric(df3, "y", "g", parametric = FALSE)$test,
               "Kruskal")
})

test_that("compare_groups_numeric returns NULL on unusable input", {
  expect_null(compare_groups_numeric(data.frame(y = 1:5, g = letters[1:5]),
                                     "g", "y"))         # non-numeric outcome
  one <- data.frame(y = 1:5, g = rep("A", 5))
  expect_null(compare_groups_numeric(one, "y", "g"))    # only one group
})

# ---- assumption checks -------------------------------------------------------

test_that("normality_table reports one row per group", {
  df <- data.frame(y = c(rnorm(10), rnorm(10)), g = rep(c("A", "B"), each = 10))
  nt <- normality_table(df, "y", "g")
  expect_equal(nrow(nt), 2L)
  expect_named(nt, c("Group", "N", "W", "p_value", "Normal"))
})

test_that("levene_test flags unequal variances", {
  set.seed(3)
  y  <- c(rnorm(30, sd = 1), rnorm(30, sd = 5))
  g  <- rep(c("A", "B"), each = 30)
  lv <- levene_test(y, g)
  expect_false(is.null(lv))
  expect_true(lv$p_value < 0.05)
  expect_false(lv$equal)
})

# ---- effect-size labels ------------------------------------------------------

test_that("effect_magnitude buckets standardised effects", {
  expect_equal(effect_magnitude("Cohen's d", 0.1), "negligible")
  expect_equal(effect_magnitude("Cohen's d", 0.9), "large")
  expect_equal(effect_magnitude("Eta-squared", 0.2), "large")
  expect_true(is.na(effect_magnitude("unknown", 0.5)))
})

# ---- compare_categorical -----------------------------------------------------

test_that("compare_categorical matches chisq.test and bounds Cramér's V", {
  df  <- data.frame(s = rep(c("x", "y"), each = 50),
                    o = c(rep(c("a", "b"), c(40, 10)),
                          rep(c("a", "b"), c(15, 35))))
  res <- compare_categorical(df, "s", "o")
  tab <- table(df$s, df$o)
  expect_equal(res$statistic, unname(suppressWarnings(chisq.test(tab))$statistic))
  expect_true(res$cramers_v >= 0 && res$cramers_v <= 1)
  expect_equal(dim(res$table), c(2L, 2L))
})

test_that("compare_categorical needs a 2x2-or-larger table", {
  df <- data.frame(s = rep("x", 5), o = letters[1:5])
  expect_null(compare_categorical(df, "s", "o"))
})
