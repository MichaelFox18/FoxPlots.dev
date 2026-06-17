# ============================================================
# helpers_compare.R — group-comparison hypothesis tests
# ============================================================
# Pure base/stats helpers for the Compare Groups tab:
#   • compare a NUMERIC outcome across the levels of a categorical group
#     (t-test / ANOVA, or the rank-based Wilcoxon / Kruskal–Wallis),
#   • check the two key assumptions (per-group normality, equal variance),
#   • test association between TWO CATEGORICAL variables (chi-square, with a
#     Fisher's-exact fallback when expected counts are small).
# mod_compare.R is a thin wrapper over these.
#
# Everything here uses only base + stats, so it unit-tests without Shiny or
# ggplot2 attached. Functions return structured lists / data frames (or NULL
# on unusable input); the module turns them into UI + plain English.

# --- descriptive + effect-size helpers --------------------------------------

#' Per-group descriptive stats for a numeric outcome (NA-safe).
#' @return data.frame(Group, N, Mean, SD, Median), one row per non-empty level.
#' @noRd
group_stats_table <- function(df, outcome, group) {
  y  <- df[[outcome]]; g <- as.factor(df[[group]])
  ok <- !is.na(y) & !is.na(g); y <- y[ok]; g <- droplevels(g[ok])
  if (!nlevels(g)) return(NULL)
  data.frame(
    Group  = levels(g),
    N      = as.integer(tapply(y, g, length)),
    Mean   = round(as.numeric(tapply(y, g, mean)), 4),
    SD     = round(as.numeric(tapply(y, g, stats::sd)), 4),
    Median = round(as.numeric(tapply(y, g, stats::median)), 4),
    row.names = NULL, stringsAsFactors = FALSE
  )
}

#' Cohen's d (pooled SD) for a two-level grouping. NA if not exactly two groups.
#' @noRd
cohens_d <- function(y, g) {
  g <- droplevels(as.factor(g))
  lv <- levels(g); if (length(lv) != 2L) return(NA_real_)
  y1 <- y[g == lv[1]]; y2 <- y[g == lv[2]]
  n1 <- length(y1); n2 <- length(y2)
  if (n1 < 2L || n2 < 2L) return(NA_real_)
  sp <- sqrt(((n1 - 1) * stats::var(y1) + (n2 - 1) * stats::var(y2)) / (n1 + n2 - 2))
  if (is.na(sp) || sp == 0) return(NA_real_)
  unname((mean(y1) - mean(y2)) / sp)
}

#' Rough magnitude label for a standardised effect size.
#' @param name "Cohen's d", "Eta-squared", or "Cramér's V".
#' @param value The effect-size value.
#' @return A character bucket: negligible / small / medium / large (or NA).
#' @export
effect_magnitude <- function(name, value) {
  if (is.null(value) || is.na(value)) return(NA_character_)
  v   <- abs(value)
  thr <- switch(name,
    "Cohen's d"   = c(0.2, 0.5, 0.8),
    "Eta-squared" = c(0.01, 0.06, 0.14),
    "Cramér's V"  = c(0.1, 0.3, 0.5),
    NULL)
  if (is.null(thr)) return(NA_character_)
  if (v < thr[1]) "negligible" else if (v < thr[2]) "small"
  else if (v < thr[3]) "medium"  else "large"
}

# --- assumption checks ------------------------------------------------------

#' Per-group Shapiro–Wilk normality test. W / p are NA where a group is too
#' small (n < 3), too large (n > 5000), or constant.
#' @return data.frame(Group, N, W, p_value, Normal).
#' @noRd
normality_table <- function(df, outcome, group) {
  y  <- df[[outcome]]; g <- as.factor(df[[group]])
  ok <- !is.na(y) & !is.na(g); y <- y[ok]; g <- droplevels(g[ok])
  if (!nlevels(g)) return(NULL)
  rows <- lapply(levels(g), function(l) {
    yi <- y[g == l]; n <- length(yi)
    if (n < 3L || n > 5000L || stats::sd(yi) == 0) {
      data.frame(Group = l, N = n, W = NA_real_, p_value = NA_real_,
                 Normal = NA, stringsAsFactors = FALSE)
    } else {
      s <- stats::shapiro.test(yi)
      data.frame(Group = l, N = n, W = round(unname(s$statistic), 4),
                 p_value = s$p.value, Normal = s$p.value >= 0.05,
                 stringsAsFactors = FALSE)
    }
  })
  do.call(rbind, rows)
}

#' Brown–Forsythe Levene test (median-centred) for equal variances — pure base
#' R, more robust to non-normality than Bartlett's. NULL if < 2 groups.
#' @noRd
levene_test <- function(y, g) {
  g <- droplevels(as.factor(g))
  if (nlevels(g) < 2L) return(NULL)
  ok <- !is.na(y) & !is.na(g); y <- y[ok]; g <- droplevels(g[ok])
  med <- tapply(y, g, stats::median)
  z   <- abs(y - med[as.character(g)])
  a   <- stats::anova(stats::lm(z ~ g))
  list(statistic = unname(a[["F value"]][1]),
       df1 = a[["Df"]][1], df2 = a[["Df"]][2],
       p_value = a[["Pr(>F)"]][1],
       equal = a[["Pr(>F)"]][1] >= 0.05)
}

# --- numeric outcome × group comparison -------------------------------------

#' Compare a numeric outcome across the levels of a categorical group.
#'
#' Picks the test from the number of groups and the `parametric` flag:
#'   parametric, 2 groups  -> Welch (or Student) two-sample t-test  (+ Cohen's d)
#'   parametric, 3+ groups -> one-way ANOVA + Tukey HSD             (+ eta-squared)
#'   non-parametric, 2     -> Wilcoxon rank-sum (Mann–Whitney U)
#'   non-parametric, 3+    -> Kruskal–Wallis
#'
#' @param df A data frame.
#' @param outcome Name of the numeric outcome column.
#' @param group Name of the categorical grouping column.
#' @param parametric Use a parametric test (t-test / ANOVA) vs. a rank test.
#' @param var_equal Assume equal variances (two-group t-test only).
#' @return A list (or NULL on unusable input) with: test, statistic, df/df2,
#'   p_value, effect_name/effect_value, group_stats, posthoc (ANOVA only),
#'   k, n, parametric.
#' @export
compare_groups_numeric <- function(df, outcome, group,
                                   parametric = TRUE, var_equal = FALSE) {
  if (!is.data.frame(df) || !all(c(outcome, group) %in% names(df))) return(NULL)
  y <- df[[outcome]]; g <- df[[group]]
  if (!is.numeric(y)) return(NULL)
  ok <- !is.na(y) & !is.na(g); y <- y[ok]; g <- droplevels(as.factor(g[ok]))
  k <- nlevels(g)
  if (k < 2L || length(y) < 3L) return(NULL)
  if (any(table(g) < 2L))       return(NULL)   # need >= 2 obs per group

  res <- list(outcome = outcome, group = group, k = k, n = length(y),
              parametric = parametric, df2 = NA_real_, posthoc = NULL,
              effect_name = NA_character_, effect_value = NA_real_,
              group_stats = group_stats_table(df, outcome, group))

  if (parametric && k == 2L) {
    tt <- stats::t.test(y ~ g, var.equal = var_equal)
    res$test         <- if (var_equal) "Two-sample t-test (equal variance)"
                        else            "Welch two-sample t-test"
    res$statistic    <- unname(tt$statistic)
    res$df           <- unname(tt$parameter)
    res$p_value      <- tt$p.value
    res$effect_name  <- "Cohen's d"
    res$effect_value <- cohens_d(y, g)

  } else if (parametric && k >= 3L) {
    fit <- stats::aov(y ~ g)
    sm  <- summary(fit)[[1]]
    res$test         <- "One-way ANOVA"
    res$statistic    <- sm[["F value"]][1]
    res$df           <- sm[["Df"]][1]
    res$df2          <- sm[["Df"]][2]
    res$p_value      <- sm[["Pr(>F)"]][1]
    ss               <- sm[["Sum Sq"]]
    res$effect_name  <- "Eta-squared"
    res$effect_value <- ss[1] / sum(ss)
    th <- stats::TukeyHSD(fit)$g
    res$posthoc <- data.frame(
      Comparison = rownames(th),
      Difference = round(th[, "diff"], 4),
      CI_low     = round(th[, "lwr"], 4),
      CI_high    = round(th[, "upr"], 4),
      p_adj      = th[, "p adj"],
      row.names = NULL, stringsAsFactors = FALSE)

  } else if (!parametric && k == 2L) {
    wt <- stats::wilcox.test(y ~ g)
    res$test      <- "Wilcoxon rank-sum test (Mann–Whitney U)"
    res$statistic <- unname(wt$statistic)
    res$df        <- NA_real_
    res$p_value   <- wt$p.value

  } else {                                   # non-parametric, 3+ groups
    kt <- stats::kruskal.test(y ~ g)
    res$test      <- "Kruskal–Wallis test"
    res$statistic <- unname(kt$statistic)
    res$df        <- unname(kt$parameter)
    res$p_value   <- kt$p.value
  }
  res
}

# --- two categorical variables ----------------------------------------------

#' Test association between two categorical variables.
#'
#' Runs Pearson's chi-square on the contingency table and reports Cramér's V.
#' When any expected cell count is < 5 the chi-square approximation is shaky, so
#' a Monte-Carlo Fisher's-exact p-value is added as a fallback.
#'
#' @param df A data frame.
#' @param var1,var2 Names of the two categorical columns.
#' @return A list (or NULL) with: table, expected, statistic, df, p_value,
#'   fisher_p, low_expected (proportion of cells expected < 5), cramers_v, n.
#' @export
compare_categorical <- function(df, var1, var2) {
  if (!is.data.frame(df) || !all(c(var1, var2) %in% names(df))) return(NULL)
  a <- df[[var1]]; b <- df[[var2]]
  ok <- !is.na(a) & !is.na(b)
  tab <- table(droplevels(as.factor(a[ok])), droplevels(as.factor(b[ok])))
  if (nrow(tab) < 2L || ncol(tab) < 2L) return(NULL)

  chi      <- suppressWarnings(stats::chisq.test(tab))
  expected <- chi$expected
  low_exp  <- mean(expected < 5)
  fisher_p <- if (low_exp > 0)
    tryCatch(suppressWarnings(
      stats::fisher.test(tab, simulate.p.value = TRUE, B = 2000)$p.value),
      error = function(e) NA_real_)
  else NA_real_

  n <- sum(tab)
  list(
    var1 = var1, var2 = var2,
    table = tab, expected = round(expected, 2),
    statistic = unname(chi$statistic), df = unname(chi$parameter),
    p_value = chi$p.value, fisher_p = fisher_p,
    low_expected = low_exp,
    cramers_v = sqrt(unname(chi$statistic) / (n * (min(dim(tab)) - 1))),
    n = n
  )
}
