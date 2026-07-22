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

test_that("compare_categorical returns Pearson and standardized residuals", {
  set.seed(9)
  df  <- data.frame(s = sample(c("x", "y"), 200, TRUE),
                    o = sample(c("a", "b", "c"), 200, TRUE))
  res <- compare_categorical(df, "s", "o")
  chi <- suppressWarnings(chisq.test(table(df$s, df$o)))
  expect_equal(res$residuals, round(chi$residuals, 2))     # Pearson (O-E)/sqrt(E)
  expect_equal(res$stdres,    round(chi$stdres, 2))        # standardized ~N(0,1)
  expect_equal(dim(res$residuals), dim(res$table))
})

# ---- enriched means table (oneway_means) -------------------------------------

test_that("oneway_means individual SE/CI use each group's own spread", {
  df <- data.frame(y = c(1, 2, 3, 4, 10, 12, 14, 16), g = rep(c("A", "B"), each = 4))
  m  <- oneway_means(df, "y", "g")
  expect_named(m, c("Group", "N", "Mean", "SD", "Median", "SE", "CI_low", "CI_high"))
  yA <- df$y[df$g == "A"]; nA <- length(yA)
  expect_equal(m$SE[m$Group == "A"], round(stats::sd(yA) / sqrt(nA), 4))
  expect_equal(m$CI_high[m$Group == "A"],
               round(mean(yA) + stats::qt(0.975, nA - 1) * stats::sd(yA) / sqrt(nA), 4))
})

test_that("oneway_means pooled SE/CI match the ANOVA error term", {
  set.seed(5)
  df  <- data.frame(y = c(rnorm(8, 5), rnorm(8, 7), rnorm(8, 9)),
                    g = rep(c("A", "B", "C"), each = 8))
  sm  <- summary(aov(y ~ g, data = df))[[1]]
  mse <- sm[["Mean Sq"]][2]; dfw <- sm[["Df"]][2]
  m   <- oneway_means(df, "y", "g", mse = mse, df_error = dfw)
  nA  <- sum(df$g == "A"); mnA <- mean(df$y[df$g == "A"])
  expect_equal(m$SE[m$Group == "A"], round(sqrt(mse / nA), 4))
  expect_equal(m$CI_high[m$Group == "A"],
               round(mnA + stats::qt(0.975, dfw) * sqrt(mse / nA), 4))
})

test_that("oneway_means returns NULL when no usable rows", {
  expect_null(oneway_means(data.frame(y = c(NA_real_, NA_real_), g = c("A", "B")),
                           "y", "g"))
})

# ---- ANOVA table + fit + Welch + connecting letters --------------------------

test_that("ANOVA result carries a full table, fit stats, and Welch", {
  set.seed(6)
  df  <- data.frame(y = c(rnorm(12, 5), rnorm(12, 7), rnorm(12, 12)),
                    g = rep(c("A", "B", "C"), each = 12))
  res <- compare_groups_numeric(df, "y", "g")
  at  <- res$anova_table
  expect_equal(at$Source, c("Between", "Within", "Total"))
  expect_equal(at$Df, c(2L, 33L, 35L))
  expect_equal(at$Sum_Sq[3], round(at$Sum_Sq[1] + at$Sum_Sq[2], 4))
  fit <- lm(y ~ g, data = df)
  expect_equal(res$fit_stats$r_squared, summary(fit)$r.squared)
  expect_equal(res$fit_stats$r_squared, res$effect_value)   # == eta-squared
  expect_equal(res$fit_stats$rmse, sigma(fit))
  expect_equal(res$welch$p_value,
               oneway.test(y ~ g, data = df, var.equal = FALSE)$p.value)
  expect_equal(res$welch$df1, 2)
  expect_named(res$group_stats,
               c("Group", "N", "Mean", "SD", "Median", "SE", "CI_low", "CI_high"))
})

test_that("Welch / ANOVA table / letters are NULL off the ANOVA path", {
  df2 <- data.frame(y = c(rnorm(10), rnorm(10, 3)), g = rep(c("A", "B"), each = 10))
  r_t <- compare_groups_numeric(df2, "y", "g")                     # t-test
  expect_null(r_t$welch); expect_null(r_t$anova_table); expect_null(r_t$cld)
  r_w <- compare_groups_numeric(df2, "y", "g", parametric = FALSE) # Wilcoxon
  expect_null(r_w$welch)
})

test_that("cld_from_tukey assigns letters and survives hyphenated levels", {
  skip_if_not_installed("multcompView")
  # Deterministic: A and B are identical (must share a letter); C is far away.
  df <- data.frame(
    y = c(rep(c(9, 10, 11), 5), rep(c(9, 10, 11), 5), rep(c(19, 20, 21), 5)),
    g = factor(rep(c("A", "B", "C"), each = 15)))
  cl <- compare_groups_numeric(df, "y", "g")$cld
  expect_named(cl, c("Group", "Mean", "N", "Letters"))
  expect_equal(nrow(cl), 3L)
  share <- function(x, y) any(strsplit(x, "")[[1]] %in% strsplit(y, "")[[1]])
  letA <- cl$Letters[cl$Group == "A"]; letB <- cl$Letters[cl$Group == "B"]
  letC <- cl$Letters[cl$Group == "C"]
  expect_false(share(letC, letA))   # C separates from A/B
  expect_true(share(letA, letB))    # A and B (identical) share a letter
  # a level name containing "-" must not corrupt the comparison-name split
  dfh <- df; levels(dfh$g) <- c("a-1", "a-2", "a-3")
  clh <- compare_groups_numeric(dfh, "y", "g")$cld
  expect_equal(nrow(clh), 3L)
  expect_setequal(clh$Group, c("a-1", "a-2", "a-3"))
})

# ---- non-parametric parity: rank effect sizes + Dunn's test ------------------

test_that("non-parametric branches report rank effect sizes", {
  set.seed(7)
  df2 <- data.frame(y = c(rnorm(12), rnorm(12, 2)), g = rep(c("A", "B"), each = 12))
  rw  <- compare_groups_numeric(df2, "y", "g", parametric = FALSE)
  expect_equal(rw$effect_name, "Rank-biserial r")
  W   <- unname(wilcox.test(y ~ g, data = df2)$statistic)
  expect_equal(rw$effect_value, 1 - 2 * W / (12 * 12))
  df3 <- data.frame(y = c(rnorm(8), rnorm(8, 2), rnorm(8, 4)),
                    g = rep(c("A", "B", "C"), each = 8))
  rk  <- compare_groups_numeric(df3, "y", "g", parametric = FALSE)
  expect_equal(rk$effect_name, "Epsilon-squared")
  H   <- unname(kruskal.test(y ~ g, data = df3)$statistic)
  expect_equal(rk$effect_value, H / (nrow(df3) - 1))
  expect_s3_class(rk$posthoc, "data.frame")                 # Dunn's post-hoc
  expect_named(rk$posthoc, c("Comparison", "Z", "p_value", "p_adj"))
})

test_that("dunn_test matches a hand-computed z (no ties)", {
  y <- c(1, 2, 3, 4, 5, 6, 7, 8, 9); g <- factor(rep(c("A", "B", "C"), each = 3))
  d <- dunn_test(y, g, p_adjust = "none")
  expect_named(d, c("Comparison", "Z", "p_value", "p_adj"))
  expect_equal(nrow(d), 3L)
  # ranks 1..9 -> mean ranks 2, 5, 8; sigma^2 = 9*10/12 = 7.5
  z_CA <- 6 / sqrt(7.5 * (1 / 3 + 1 / 3))
  expect_equal(d$Z[d$Comparison == "C-A"], round(z_CA, 4))
  expect_equal(d$p_value[d$Comparison == "C-A"], 2 * pnorm(-abs(z_CA)))
})

test_that("dunn_test returns NULL with fewer than two groups", {
  expect_null(dunn_test(1:5, rep("A", 5)))
})

test_that("effect_magnitude buckets rank effect sizes", {
  expect_equal(effect_magnitude("Epsilon-squared", 0.2), "large")
  expect_equal(effect_magnitude("Rank-biserial r", 0.05), "negligible")
  expect_equal(effect_magnitude("Rank-biserial r", 0.4), "medium")
})

# ---- Steel-Dwass (DSCF) ------------------------------------------------------

test_that("steel_dwass_test matches a hand-computed pair (no ties)", {
  y <- c(1, 2, 3, 4, 5, 6, 7, 8, 9); g <- factor(rep(c("A", "B", "C"), each = 3))
  d <- steel_dwass_test(y, g)
  expect_named(d, c("Comparison", "W", "Z", "p_value", "p_adj"))
  expect_equal(nrow(d), 3L)                       # k(k-1)/2
  # C-A: the pair {1,2,3} vs {7,8,9} ranks 1..6, so R_C = 15, E = 3*7/2 = 10.5.
  # With no ties s2 reduces to n_i*n_j*(N+1)/12 = 3*3*7/12 = 5.25.
  z_CA <- (15 - 10.5) / sqrt(5.25)
  expect_equal(d$Z[d$Comparison == "C-A"], round(z_CA, 4))
  expect_equal(d$p_value[d$Comparison == "C-A"],
               stats::ptukey(sqrt(2) * abs(z_CA), nmeans = 3, df = Inf,
                             lower.tail = FALSE))
  # ptukey already controls the family-wise rate, so no extra adjustment
  expect_equal(d$p_adj, d$p_value)
})

test_that("steel_dwass_test reduces to the Wilcoxon normal approximation at k=2", {
  # For two groups the studentized range collapses to the two-sided normal:
  # ptukey(sqrt(2)|z|, 2, Inf) == 2*pnorm(-|z|), so Z must equal the standardized
  # Wilcoxon rank-sum statistic and p its normal-approximation p-value.
  set.seed(11)
  yi <- rnorm(12, 10); yj <- rnorm(12, 12)
  d  <- steel_dwass_test(c(yi, yj), factor(rep(c("A", "B"), each = 12)))
  ni <- 12; nj <- 12; N <- 24
  r    <- rank(c(yi, yj)); Rj <- sum(r[(ni + 1):N])
  zman <- (Rj - nj * (N + 1) / 2) / sqrt(ni * nj * (N + 1) / 12)
  expect_equal(d$Z, round(zman, 4))
  expect_equal(d$p_value, 2 * stats::pnorm(-abs(zman)))
})

test_that("steel_dwass_test returns NULL with fewer than two groups", {
  expect_null(steel_dwass_test(1:5, rep("A", 5)))
})

test_that("posthoc arg switches the non-parametric all-pairs method", {
  set.seed(12)
  df <- data.frame(y = c(rnorm(10, 10, 2), rnorm(10, 12, 2), rnorm(10, 20, 3)),
                   g = factor(rep(c("A", "B", "C"), each = 10)))
  rd <- compare_groups_numeric(df, "y", "g", parametric = FALSE)   # dunn default
  rs <- compare_groups_numeric(df, "y", "g", parametric = FALSE, posthoc = "steel")
  expect_equal(rd$posthoc_name, "Dunn's test")
  expect_equal(rs$posthoc_name, "Steel-Dwass")
  expect_named(rd$posthoc, c("Comparison", "Z", "p_value", "p_adj"))
  expect_named(rs$posthoc, c("Comparison", "W", "Z", "p_value", "p_adj"))
  # both post-hocs now carry connecting letters
  expect_s3_class(rd$cld, "data.frame")
  expect_s3_class(rs$cld, "data.frame")
})

test_that("posthoc_name identifies the method on every path", {
  set.seed(13)
  df <- data.frame(y = c(rnorm(10, 5), rnorm(10, 7), rnorm(10, 9)),
                   g = factor(rep(c("A", "B", "C"), each = 10)))
  expect_equal(compare_groups_numeric(df, "y", "g")$posthoc_name, "Tukey HSD")
  df2 <- df[df$g %in% c("A", "B"), ]; df2$g <- droplevels(df2$g)
  expect_null(compare_groups_numeric(df2, "y", "g")$posthoc_name)              # t-test
  expect_null(compare_groups_numeric(df2, "y", "g", parametric = FALSE)$posthoc_name)
})

test_that("connecting letters run a, b, c... down the descending-mean rows", {
  skip_if_not_installed("multcompView")
  # Three clearly separated groups -> three distinct letters, in mean order.
  df <- data.frame(
    y = c(rep(c(1, 2, 3), 5), rep(c(11, 12, 13), 5), rep(c(21, 22, 23), 5)),
    g = factor(rep(c("A", "B", "C"), each = 15)))
  cl <- compare_groups_numeric(df, "y", "g")$cld
  expect_equal(cl$Group, c("C", "B", "A"))        # descending mean
  expect_equal(cl$Letters, c("a", "b", "c"))      # relabelled in row order
})

# ---- compare_grid (outcomes x groups) ---------------------------------------

test_that("compare_grid tests every combination and adjusts across the grid", {
  set.seed(14)
  df <- data.frame(y1 = rnorm(30), y2 = rnorm(30),
                   g1 = factor(rep(c("a", "b", "c"), each = 10)),
                   g2 = factor(rep(c("x", "y"), 15)))
  gr <- compare_grid(df, c("y1", "y2"), c("g1", "g2"))
  expect_equal(nrow(gr$summary), 4L)              # 2 outcomes x 2 groups
  expect_length(gr$results, 4L)
  expect_named(gr$summary,
               c("Outcome", "Group", "N", "k", "Test", "Statistic", "df",
                 "p_value", "p_adj", "Effect", "Effect_size", "Magnitude"))
  expect_equal(gr$keys, names(gr$results))
  expect_setequal(gr$keys, c("y1 by g1", "y1 by g2", "y2 by g1", "y2 by g2"))
  # the grid IS the family: p_adj is p.adjust over all combinations at once
  expect_equal(gr$summary$p_adj, stats::p.adjust(gr$summary$p_value, "BH"))
  # each stored result is a full single-combo result
  expect_equal(gr$results[["y1 by g1"]]$test, "One-way ANOVA")
  expect_equal(gr$results[["y1 by g2"]]$k, 2L)
})

test_that("compare_grid honours p_adjust = 'none' and drops unusable combos", {
  set.seed(15)
  df <- data.frame(y1 = rnorm(20), txt = letters[1:20],
                   g1 = factor(rep(c("a", "b"), each = 10)))
  gr <- compare_grid(df, "y1", "g1", p_adjust = "none")
  expect_equal(gr$summary$p_adj, gr$summary$p_value)
  # a non-numeric "outcome" yields no testable combination
  expect_null(compare_grid(df, "txt", "g1"))
  # same column on both axes is skipped
  expect_null(compare_grid(df, "g1", "g1"))
})

test_that("compare_grid returns NULL on unusable input", {
  df <- data.frame(y = rnorm(10), g = rep(c("a", "b"), 5))
  expect_null(compare_grid(df, character(0), "g"))
  expect_null(compare_grid(df, "y", character(0)))
  expect_null(compare_grid(df, "nope", "g"))
  expect_null(compare_grid("not a data frame", "y", "g"))
})

# ---- contingency-table percentages ------------------------------------------

test_that("compare_categorical returns row / column / total percentages", {
  set.seed(16)
  df  <- data.frame(s = sample(c("x", "y"), 200, TRUE),
                    o = sample(c("a", "b", "c"), 200, TRUE))
  res <- compare_categorical(df, "s", "o")
  tab <- table(df$s, df$o)
  expect_equal(res$row_pct,   round(prop.table(tab, 1) * 100, 1))
  expect_equal(res$col_pct,   round(prop.table(tab, 2) * 100, 1))
  expect_equal(res$total_pct, round(prop.table(tab) * 100, 1))
  # margins add up (allowing for 1-dp rounding)
  expect_true(all(abs(rowSums(res$row_pct) - 100) < 0.2))
  expect_true(all(abs(colSums(res$col_pct) - 100) < 0.2))
  expect_lt(abs(sum(res$total_pct) - 100), 0.2)
  expect_equal(dim(res$row_pct), dim(res$table))
})

# ---- split-by (stratify the grid by a 3rd variable) -------------------------
# The boss's ask: "MPG vs cyl by am" -> run MPG-by-cyl separately for each am.

test_that("compare_grid without split_by is unchanged", {
  g <- compare_grid(mtcars, "mpg", "cyl")
  expect_false("Stratum" %in% names(g$summary))
  expect_null(g$split_by)
  expect_equal(g$keys, "mpg by cyl")
})

test_that("compare_grid stratifies by a 3rd variable", {
  d <- mtcars; d$am <- factor(d$am, labels = c("auto", "manual"))
  g <- compare_grid(d, "mpg", "cyl", split_by = "am")
  expect_true("Stratum" %in% names(g$summary))
  expect_setequal(unique(g$summary$Stratum), c("auto", "manual"))
  expect_equal(nrow(g$summary), 2L)                       # one row per stratum
  expect_match(g$keys[1], "mpg by cyl | am = ", fixed = TRUE)
  # each stratum is an independent subset; the ns partition the data
  expect_equal(sum(g$summary$N), nrow(d))
})

test_that("a split variable is never also an outcome or a group", {
  d <- mtcars; d$am <- factor(d$am)
  g <- compare_grid(d, "mpg", c("cyl", "am"), split_by = "am")
  expect_false(any(g$summary$Group == "am"))
})

test_that("compare_grid caps the number of strata and flags it", {
  withr::local_seed(1)
  d <- data.frame(y = stats::rnorm(300),
                  grp = factor(sample(c("a", "b"), 300, TRUE)),
                  s   = factor(sample(paste0("L", 1:10), 300, TRUE)))
  g <- compare_grid(d, "y", "grp", split_by = "s")
  expect_lte(length(unique(g$summary$Stratum)), COMPARE_SPLIT_MAX)
  expect_true(g$split_capped)
})

test_that("p-values are BH-adjusted across the whole stratified grid", {
  d <- mtcars; d$am <- factor(d$am)
  g <- compare_grid(d, c("mpg", "hp"), "cyl", split_by = "am")
  expect_equal(g$summary$p_adj,
               stats::p.adjust(g$summary$p_value, method = "BH"))
})
