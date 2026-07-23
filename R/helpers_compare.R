# ============================================================
# helpers_compare.R -- group-comparison hypothesis tests
# ============================================================
# Pure base/stats helpers for the Compare Groups tab:
#   * compare a NUMERIC outcome across the levels of a categorical group
#     (t-test / ANOVA, or the rank-based Wilcoxon / Kruskal-Wallis),
#   * check the two key assumptions (per-group normality, equal variance),
#   * test association between TWO CATEGORICAL variables (chi-square, with a
#     Fisher's-exact fallback when expected counts are small).
# mod_compare.R is a thin wrapper over these.
#
# Everything here uses only base + stats, so it unit-tests without Shiny or
# ggplot2 attached. Functions return structured lists / data frames (or NULL
# on unusable input); the module turns them into UI + plain English.

# --- descriptive + effect-size helpers --------------------------------------

# The most tests any one grid run may fire (outcomes x groups, times strata
# when split). 24 = the module's selectize maxima product (6 outcomes x 4
# groups), so the pickers themselves are the practical limit for unsplit
# grids; the module's validate() gates stay as defense-in-depth for split
# grids and any future picker changes. The per-combination renderers are
# pre-registered up to this cap, so raising it raises server-init cost.
COMPARE_MAX_COMBOS <- 24L

# Display glyphs, built from code points so every source file that needs them
# stays ASCII-clean (R CMD check portability -- see CLAUDE.md).
# CRAMERS_V must match effect_magnitude()'s switch key exactly.
CRAMERS_V <- paste0("Cram", intToUtf8(0x00E9), "r's V")
SYM_CHI2  <- intToUtf8(c(0x03C7, 0x00B2))   # chi-squared
SYM_ALPHA <- intToUtf8(0x03B1)              # alpha
SYM_TIMES <- intToUtf8(0x00D7)              # multiplication sign

#' Per-group means table with standard error and a confidence interval.
#'
#' One row per non-empty level: N, Mean, SD, Median, SE, CI_low, CI_high. With a
#' pooled error term (`mse`/`df_error` from the ANOVA fit) the SE is
#' sqrt(mse / n_i) on df_error degrees of freedom -- JMP's "Means for Oneway
#' Anova". Without one, each group uses its own spread: SE = SD_i / sqrt(n_i) on
#' n_i - 1 df. NA-safe; NULL when no non-empty level.
#'
#' @return data.frame(Group, N, Mean, SD, Median, SE, CI_low, CI_high) or NULL.
#' @noRd
oneway_means <- function(df, outcome, group, mse = NULL, df_error = NULL,
                         conf = 0.95) {
  y  <- df[[outcome]]; g <- as.factor(df[[group]])
  ok <- !is.na(y) & !is.na(g); y <- y[ok]; g <- droplevels(g[ok])
  if (!nlevels(g)) return(NULL)
  n    <- as.integer(tapply(y, g, length))
  mn   <- as.numeric(tapply(y, g, mean))
  sdv  <- as.numeric(tapply(y, g, stats::sd))
  med  <- as.numeric(tapply(y, g, stats::median))
  pooled <- !is.null(mse) && !is.null(df_error)
  se     <- if (pooled) sqrt(mse / n) else sdv / sqrt(n)
  tcrit  <- if (pooled) stats::qt(1 - (1 - conf) / 2, df_error)
            else        stats::qt(1 - (1 - conf) / 2, pmax(n - 1L, 1L))
  data.frame(
    Group   = levels(g),
    N       = n,
    Mean    = round(mn, 4),
    SD      = round(sdv, 4),
    Median  = round(med, 4),
    SE      = round(se, 4),
    CI_low  = round(mn - tcrit * se, 4),
    CI_high = round(mn + tcrit * se, 4),
    row.names = NULL, stringsAsFactors = FALSE
  )
}

#' Classic ANOVA table + summary-of-fit from an aov summary (pure reshaping).
#'
#' Takes the first element of summary(aov_fit) -- nothing is re-fit -- and
#' returns the Between/Within/Total decomposition plus the fit statistics
#' (R^2, adjusted R^2, RMSE, grand mean) and the pooled error (mse, df_error)
#' that feeds the pooled-SE means table.
#'
#' @param sm summary(aov_fit)[[1]] (a data frame of the model terms).
#' @param k Number of groups. @param n Total N used in the fit.
#' @param grand_mean Overall mean of the outcome (for the fit summary).
#' @return list(table = data.frame(Source, Df, Sum_Sq, Mean_Sq, F, p_value),
#'   fit = list(r_squared, adj_r_squared, rmse, mean_response, n, mse, df_error)).
#' @noRd
anova_summary <- function(sm, k, n, grand_mean = NA_real_) {
  ssb <- sm[["Sum Sq"]][1]; ssw <- sm[["Sum Sq"]][2]; sst <- ssb + ssw
  dfb <- k - 1L; dfw <- n - k; dft <- n - 1L
  msb <- ssb / dfb; msw <- ssw / dfw
  fval <- sm[["F value"]][1]; pval <- sm[["Pr(>F)"]][1]
  tbl <- data.frame(
    Source  = c("Between", "Within", "Total"),
    Df      = c(dfb, dfw, dft),
    Sum_Sq  = round(c(ssb, ssw, sst), 4),
    Mean_Sq = round(c(msb, msw, NA_real_), 4),
    F       = round(c(fval, NA_real_, NA_real_), 4),
    p_value = c(pval, NA_real_, NA_real_),
    row.names = NULL, stringsAsFactors = FALSE
  )
  fit <- list(
    r_squared     = ssb / sst,
    adj_r_squared = 1 - msw / (sst / dft),
    rmse          = sqrt(msw),
    mean_response = grand_mean,
    n             = n,
    mse           = msw,
    df_error      = dfw
  )
  list(table = tbl, fit = fit)
}

#' Dunn's test -- post-hoc pairwise comparisons after Kruskal-Wallis.
#'
#' Hand-rolled in base R: every observation is ranked jointly (the same ranking
#' the Kruskal-Wallis omnibus uses), with Dunn's tie correction, so the post-hoc
#' is consistent with the reported H statistic. Two-sided z per pair, adjusted
#' across the k(k-1)/2 comparisons. Comparison names use the "B-A" order that
#' TukeyHSD uses, so the result drops into the same renderers as res$posthoc.
#'
#' @param y Numeric outcome. @param g Grouping factor.
#' @param p_adjust A stats::p.adjust method (default "BH").
#' @return data.frame(Comparison, Z, p_value, p_adj) or NULL (< 2 groups).
#' @noRd
dunn_test <- function(y, g, p_adjust = "BH") {
  ok <- !is.na(y) & !is.na(g); y <- y[ok]; g <- droplevels(as.factor(g[ok]))
  if (nlevels(g) < 2L) return(NULL)
  N   <- length(y)
  r   <- rank(y)
  lev <- levels(g)
  Rbar <- tapply(r, g, mean)
  ni   <- tapply(r, g, length)
  tt   <- as.numeric(table(y))            # tie-group sizes
  ties <- sum(tt^3 - tt)
  sig2 <- (N * (N + 1) / 12) - (ties / (12 * (N - 1)))
  cmb  <- utils::combn(seq_along(lev), 2L)
  z <- p <- numeric(ncol(cmb)); cmp <- character(ncol(cmb))
  for (m in seq_len(ncol(cmb))) {
    i <- cmb[1, m]; j <- cmb[2, m]
    se     <- sqrt(sig2 * (1 / ni[i] + 1 / ni[j]))
    z[m]   <- unname((Rbar[j] - Rbar[i]) / se)
    p[m]   <- 2 * stats::pnorm(-abs(z[m]))
    cmp[m] <- paste0(lev[j], "-", lev[i])
  }
  data.frame(
    Comparison = cmp,
    Z          = round(z, 4),
    p_value    = p,
    p_adj      = stats::p.adjust(p, method = p_adjust),
    row.names = NULL, stringsAsFactors = FALSE
  )
}

#' Steel-Dwass -- non-parametric all-pairs comparisons (JMP's "All Pairs").
#'
#' Dwass-Steel-Critchlow-Fligner. The rank analogue of Tukey HSD: unlike Dunn's
#' single joint ranking, each PAIR is ranked on its own, and the studentized
#' range distribution controls the family-wise error rate directly -- so no
#' p-adjustment is applied afterwards (p_adj == p_value).
#'
#' For a pair with N = n_i + n_j, ranking the pooled pair gives rank sum R_j for
#' group j; E = n_j(N+1)/2 and, with a tie correction,
#'   s2 = (n_i*n_j / (N*(N-1))) * (sum(r^2) - N*(N+1)^2/4)
#' (which reduces to n_i*n_j*(N+1)/12 with no ties). Z = (R_j - E)/sqrt(s2) and
#' p = ptukey(sqrt(2)*|Z|, k, Inf) where k is the total number of groups.
#'
#' @param y Numeric outcome. @param g Grouping factor.
#' @return data.frame(Comparison, W, Z, p_value, p_adj) or NULL (< 2 groups).
#'   Comparison uses the same "B-A" order as TukeyHSD / dunn_test.
#' @noRd
steel_dwass_test <- function(y, g) {
  ok <- !is.na(y) & !is.na(g); y <- y[ok]; g <- droplevels(as.factor(g[ok]))
  if (nlevels(g) < 2L) return(NULL)
  lev <- levels(g)
  k   <- nlevels(g)
  cmb <- utils::combn(seq_along(lev), 2L)
  w <- z <- p <- numeric(ncol(cmb)); cmp <- character(ncol(cmb))
  for (m in seq_len(ncol(cmb))) {
    i <- cmb[1, m]; j <- cmb[2, m]
    yi <- y[g == lev[i]]; yj <- y[g == lev[j]]
    ni <- length(yi);     nj <- length(yj)
    N  <- ni + nj
    r  <- rank(c(yi, yj))                 # rank THIS PAIR only
    Rj <- sum(r[(ni + 1L):N])             # rank sum of group j
    E  <- nj * (N + 1) / 2
    s2 <- (ni * nj / (N * (N - 1))) * (sum(r^2) - N * (N + 1)^2 / 4)
    zz <- if (is.na(s2) || s2 <= 0) NA_real_ else (Rj - E) / sqrt(s2)
    w[m]   <- Rj
    z[m]   <- zz
    p[m]   <- if (is.na(zz)) NA_real_ else
      stats::ptukey(sqrt(2) * abs(zz), nmeans = k, df = Inf, lower.tail = FALSE)
    cmp[m] <- paste0(lev[j], "-", lev[i])
  }
  data.frame(
    Comparison = cmp,
    W          = w,
    Z          = round(z, 4),
    p_value    = p,
    p_adj      = p,          # ptukey already controls the family-wise rate
    row.names = NULL, stringsAsFactors = FALSE
  )
}

#' Compact-letter-display groups from a pairwise table (Tukey / Dunn / Steel-Dwass).
#'
#' Turns a pairwise table (Comparison like "B-A" + p_adj) into one row per group
#' with a Letters column: groups sharing a letter are not significantly
#' different. Levels are tokenised to g1..gk before calling multcompView so a
#' level name containing "-" cannot corrupt the comparison-name split; the tokens
#' are mapped back to the real group names afterwards.
#'
#' @param posthoc data.frame with columns Comparison and p_adj.
#' @param means data.frame with columns Group, Mean, SE, N (from oneway_means();
#'   SE is pooled sqrt(mse/n_i) on the ANOVA path, per-group SD/sqrt(n) on the
#'   rank path).
#' @param threshold Significance cutoff. @param reversed Passed to multcompView.
#' @return data.frame(Group, Mean, SE, N, Letters) ordered by descending Mean,
#'   or NULL.
#' @noRd
cld_from_tukey <- function(posthoc, means, threshold = 0.05, reversed = TRUE) {
  if (is.null(posthoc) || is.null(means) || !nrow(posthoc)) return(NULL)
  if (!requireNamespace("multcompView", quietly = TRUE)) return(NULL)
  grp   <- as.character(means$Group)
  token <- stats::setNames(paste0("g", seq_along(grp)), grp)
  # Parse each "B-A" comparison into its two known levels (robust to hyphens in
  # level names) and re-key with hyphen-free tokens.
  parse_cmp <- function(cmp) {
    for (a in grp) for (b in grp)
      if (!identical(a, b) && identical(cmp, paste0(a, "-", b))) return(c(a, b))
    NULL
  }
  keys <- character(nrow(posthoc)); ok <- logical(nrow(posthoc))
  for (i in seq_len(nrow(posthoc))) {
    pr <- parse_cmp(as.character(posthoc$Comparison[i]))
    if (!is.null(pr)) {
      keys[i] <- paste0(token[[pr[1]]], "-", token[[pr[2]]]); ok[i] <- TRUE
    }
  }
  if (!any(ok)) return(NULL)
  pv   <- stats::setNames(posthoc$p_adj[ok], keys[ok])
  lett <- tryCatch(
    multcompView::multcompLetters(pv, threshold = threshold, reversed = reversed),
    error = function(e) NULL)
  if (is.null(lett) || is.null(lett$Letters)) return(NULL)
  L    <- lett$Letters
  back <- stats::setNames(names(token), unname(token))   # token -> group
  out  <- data.frame(
    Group   = unname(back[names(L)]),
    Letters = trimws(unname(L)),
    row.names = NULL, stringsAsFactors = FALSE)
  out <- merge(out, means[, c("Group", "Mean", "SE", "N")],
               by = "Group", sort = FALSE)
  out <- out[order(out$Mean, decreasing = TRUE),
             c("Group", "Mean", "SE", "N", "Letters")]
  rownames(out) <- NULL
  # multcompView assigns symbols in the order it meets groups in the comparison
  # names, so the grouping is right but the labels can read scrambled (e.g.
  # b, c, a down the rows). Relabel so letters run a, b, c... down the
  # descending-mean rows -- purely cosmetic, the groupings are untouched.
  seen <- character(0)
  for (s in out$Letters)
    for (ch in strsplit(s, "")[[1]])
      if (!ch %in% seen) seen <- c(seen, ch)
  if (length(seen) && length(seen) <= length(letters)) {
    map <- stats::setNames(letters[seq_along(seen)], seen)
    out$Letters <- vapply(out$Letters, function(s) {
      chs <- strsplit(s, "")[[1]]
      paste(sort(unname(map[chs])), collapse = "")
    }, character(1), USE.NAMES = FALSE)
  }
  out
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
#' @param name "Cohen's d", "Eta-squared", or "Cramer's V".
#' @param value The effect-size value.
#' @return A character bucket: negligible / small / medium / large (or NA).
#' @export
effect_magnitude <- function(name, value) {
  if (is.null(value) || is.na(value)) return(NA_character_)
  v   <- abs(value)
  thr <- switch(name,
    "Cohen's d"        = c(0.2, 0.5, 0.8),
    "Eta-squared"      = c(0.01, 0.06, 0.14),
    "Epsilon-squared"  = c(0.01, 0.06, 0.14),
    "Rank-biserial r"  = c(0.1, 0.3, 0.5),
    "Cram\u00e9r's V"  = c(0.1, 0.3, 0.5),
    NULL)
  if (is.null(thr)) return(NA_character_)
  if (v < thr[1]) "negligible" else if (v < thr[2]) "small"
  else if (v < thr[3]) "medium"  else "large"
}

# --- assumption checks ------------------------------------------------------

#' Per-group Shapiro-Wilk normality test. W / p are NA where a group is too
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

#' Brown-Forsythe Levene test (median-centred) for equal variances -- pure base
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

# --- numeric outcome x group comparison -------------------------------------

#' Compare a numeric outcome across the levels of a categorical group.
#'
#' Picks the test from the number of groups and the `parametric` flag:
#'   parametric, 2 groups  -> Welch (or Student) two-sample t-test  (+ Cohen's d)
#'   parametric, 3+ groups -> one-way ANOVA + Tukey HSD             (+ eta-squared)
#'   non-parametric, 2     -> Wilcoxon rank-sum (Mann-Whitney U)
#'   non-parametric, 3+    -> Kruskal-Wallis
#'
#' @param df A data frame.
#' @param outcome Name of the numeric outcome column.
#' @param group Name of the categorical grouping column.
#' @param parametric Use a parametric test (t-test / ANOVA) vs. a rank test.
#' @param var_equal Assume equal variances (two-group t-test only).
#' @param posthoc Non-parametric all-pairs method for 3+ groups: "dunn"
#'   (default) or "steel" (Steel-Dwass). Ignored on the parametric paths, which
#'   always use Tukey HSD.
#' @return A list (or NULL on unusable input) with: test, statistic, df/df2,
#'   p_value, effect_name/effect_value, group_stats (with SE + 95% CI), posthoc
#'   and posthoc_name (Tukey HSD for ANOVA; Dunn's test / Steel-Dwass for
#'   Kruskal), cld (connecting letters, whenever a post-hoc ran), and, for ANOVA
#'   only, anova_table, fit_stats and welch; k, n, parametric.
#' @export
compare_groups_numeric <- function(df, outcome, group,
                                   parametric = TRUE, var_equal = FALSE,
                                   posthoc = c("dunn", "steel")) {
  posthoc <- match.arg(posthoc)
  if (!is.data.frame(df) || !all(c(outcome, group) %in% names(df))) return(NULL)
  y <- df[[outcome]]; g <- df[[group]]
  if (!is.numeric(y)) return(NULL)
  ok <- !is.na(y) & !is.na(g); y <- y[ok]; g <- droplevels(as.factor(g[ok]))
  k <- nlevels(g)
  if (k < 2L || length(y) < 3L) return(NULL)
  if (any(table(g) < 2L))       return(NULL)   # need >= 2 obs per group

  res <- list(outcome = outcome, group = group, k = k, n = length(y),
              parametric = parametric, df2 = NA_real_, posthoc = NULL,
              posthoc_name = NULL,
              effect_name = NA_character_, effect_value = NA_real_,
              anova_table = NULL, fit_stats = NULL, cld = NULL, welch = NULL,
              group_stats = oneway_means(df, outcome, group))

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
    res$posthoc_name <- "Tukey HSD"
    # Full ANOVA table + summary of fit; pooled-error means; connecting letters.
    at <- anova_summary(sm, k, res$n, grand_mean = mean(y))
    res$anova_table <- at$table
    res$fit_stats   <- at$fit
    res$group_stats <- oneway_means(df, outcome, group,
                                    mse = at$fit$mse, df_error = at$fit$df_error)
    res$cld <- cld_from_tukey(res$posthoc, res$group_stats)
    # Welch's one-way ANOVA (unequal variances) shown alongside the classic one.
    w <- stats::oneway.test(y ~ g, var.equal = FALSE)
    res$welch <- list(statistic = unname(w$statistic),
                      df1 = unname(w$parameter[1]), df2 = unname(w$parameter[2]),
                      p_value = w$p.value)

  } else if (!parametric && k == 2L) {
    wt <- stats::wilcox.test(y ~ g)
    res$test      <- "Wilcoxon rank-sum test (Mann\u2013Whitney U)"
    res$statistic <- unname(wt$statistic)
    res$df        <- NA_real_
    res$p_value   <- wt$p.value
    lv <- levels(g); n1 <- sum(g == lv[1]); n2 <- sum(g == lv[2])
    res$effect_name  <- "Rank-biserial r"
    res$effect_value <- 1 - 2 * unname(wt$statistic) / (n1 * n2)

  } else {                                   # non-parametric, 3+ groups
    kt <- stats::kruskal.test(y ~ g)
    res$test      <- "Kruskal\u2013Wallis test"
    res$statistic <- unname(kt$statistic)
    res$df        <- unname(kt$parameter)
    res$p_value   <- kt$p.value
    res$effect_name  <- "Epsilon-squared"
    res$effect_value <- unname(kt$statistic) / (res$n - 1)
    if (identical(posthoc, "steel")) {
      res$posthoc      <- steel_dwass_test(y, g)
      res$posthoc_name <- "Steel-Dwass"
    } else {
      res$posthoc      <- dunn_test(y, g)
      res$posthoc_name <- "Dunn's test"
    }
    # Both post-hocs yield Comparison + p_adj, so the letter engine works here
    # too (Steel-Dwass is family-wise controlled; Dunn's is BH-adjusted).
    res$cld <- cld_from_tukey(res$posthoc, res$group_stats)
  }
  res
}

# --- many outcomes x many groups --------------------------------------------

#' Run compare_groups_numeric() over a grid of outcomes x grouping variables.
#'
#' Every (outcome, group) pair is tested with the same settings. Unusable pairs
#' (a non-numeric outcome, a group with < 2 usable levels, ...) are dropped
#' rather than erroring. Because the grid IS the family of tests, the adjusted
#' p-value is computed across all surviving combinations at once.
#'
#' @param df A data frame.
#' @param outcomes Character vector of numeric outcome columns.
#' @param groups Character vector of grouping columns.
#' @param parametric,var_equal,posthoc Passed to compare_groups_numeric().
#' @param p_adjust A stats::p.adjust method applied ACROSS the grid ("BH" default;
#'   "none" leaves p_adj equal to p_value).
#' @param split_by Optional 3rd (stratifying) variable: the whole outcome x group
#'   grid is run separately within each of its levels (capped at
#'   COMPARE_SPLIT_MAX). NULL gives the unstratified grid unchanged.
#' @return NULL if nothing is testable, else a list with:
#'   summary (data.frame: Outcome, Group, [Stratum,] N, k, Test, Statistic, df,
#'   p_value, p_adj, Effect, Effect_size, Magnitude -- one row per combination),
#'   results (named list of the full compare_groups_numeric() results),
#'   keys (names of `results`, aligned to `summary` rows; carry "| split = level"
#'   when stratified), p_adjust, outcomes, groups, split_by, split_capped.
#' @noRd
# A "split by" (stratifying) variable can have at most this many levels: the
# grid is outcomes x groups x strata, so a high-cardinality By variable would
# explode it. Matches the spirit of the module's combination cap.
COMPARE_SPLIT_MAX <- 6L

# Pre-run preview of what a split variable will do: how many observed levels
# it has, how many will actually run under the cap, and how many rows fall in
# no stratum because the split value is missing. Mirrors compare_grid()'s
# stratification counting (observed non-NA uniques, capped) so the sidebar
# can warn BEFORE the grid computes.
split_preview <- function(x, cap = COMPARE_SPLIT_MAX) {
  n_na     <- sum(is.na(x))
  n_levels <- length(unique(x[!is.na(x)]))
  list(n_levels = n_levels,
       n_used   = min(n_levels, cap),
       capped   = n_levels > cap,
       n_na     = n_na)
}

# One plain-language line stating how many tests the current picks will fire:
# "3 outcomes x 2 groups = 6 tests will run.", with an "x N levels of am"
# factor when split, and an over-the-limit variant past `cap`. NULL until at
# least one outcome and one group are picked.
compare_plan_text <- function(n_outcomes, n_groups, n_strata = 1L,
                              split_var = NULL, cap = COMPARE_MAX_COMBOS) {
  n_outcomes <- as.integer(n_outcomes %||% 0L)
  n_groups   <- as.integer(n_groups   %||% 0L)
  if (is.na(n_outcomes) || is.na(n_groups) ||
      n_outcomes < 1L || n_groups < 1L) return(NULL)
  n_strata <- max(1L, as.integer(n_strata %||% 1L))
  total <- n_outcomes * n_groups * n_strata
  pl <- function(n, noun) paste0(n, " ", noun, if (n == 1L) "" else "s")
  parts <- paste(pl(n_outcomes, "outcome"), "x", pl(n_groups, "group"))
  if (!is.null(split_var) && nzchar(split_var) && n_strata > 1L)
    parts <- sprintf("%s x %d levels of %s", parts, n_strata, split_var)
  if (total > cap) {
    sprintf("%s = %d tests - over the limit of %d. Remove some picks.",
            parts, total, cap)
  } else {
    sprintf("%s = %s will run.", parts, pl(total, "test"))
  }
}

compare_grid <- function(df, outcomes, groups, parametric = TRUE,
                         var_equal = FALSE, posthoc = c("dunn", "steel"),
                         p_adjust = "BH", split_by = NULL) {
  posthoc <- match.arg(posthoc)
  if (!is.data.frame(df) || !length(outcomes) || !length(groups)) return(NULL)
  outcomes <- intersect(outcomes, names(df))
  groups   <- intersect(groups,   names(df))
  if (!length(outcomes) || !length(groups)) return(NULL)

  # Optional stratification: run the whole outcome x group grid separately
  # within each level of split_by. NULL -> one stratum over all rows, giving
  # byte-identical output to the unstratified grid.
  split_by <- if (!is.null(split_by) && length(split_by) && nzchar(split_by) &&
                  split_by %in% names(df)) split_by else NULL
  split_capped <- FALSE
  n_split_na  <- 0L
  strata <- if (is.null(split_by)) {
    list(list(label = NA_character_, rows = rep(TRUE, nrow(df))))
  } else {
    svr <- df[[split_by]]
    sv  <- as.character(svr)
    n_split_na <- sum(is.na(sv))
    # Level ORDER matters for which strata survive the cap: honour factor
    # levels, sort numerics numerically (character-sorting a coded-numeric
    # split gives 1,10,11,...,2), and only fall back to character sort.
    lv <- if (is.factor(svr)) {
      intersect(levels(svr), unique(sv[!is.na(sv)]))
    } else if (is.numeric(svr)) {
      as.character(sort(unique(svr[!is.na(svr)])))
    } else sort(unique(sv[!is.na(sv)]))
    if (length(lv) > COMPARE_SPLIT_MAX) { split_capped <- TRUE; lv <- lv[seq_len(COMPARE_SPLIT_MAX)] }
    lapply(lv, function(l) list(label = l, rows = !is.na(sv) & sv == l))
  }

  keys <- character(0); results <- list(); rows <- list()
  dropped_strata <- character(0)
  for (st in strata) {
    sdf <- df[st$rows, , drop = FALSE]
    stratum_hit <- FALSE
    for (o in outcomes) for (g in groups) {
      if (identical(o, g) || identical(o, split_by) || identical(g, split_by))
        next
      r <- suppressWarnings(
        compare_groups_numeric(sdf, o, g, parametric = parametric,
                               var_equal = var_equal, posthoc = posthoc))
      if (is.null(r)) next
      stratum_hit <- TRUE
      key <- if (is.na(st$label)) paste0(o, " by ", g)
             else sprintf("%s by %s | %s = %s", o, g, split_by, st$label)
      keys <- c(keys, key)
      results[[key]] <- r
      row <- data.frame(
        Outcome     = o,
        Group       = g,
        N           = r$n,
        k           = r$k,
        Test        = r$test,
        Statistic   = round(r$statistic, 4),
        df          = if (is.na(r$df)) NA_real_ else round(r$df, 4),
        p_value     = r$p_value,
        Effect      = r$effect_name,
        Effect_size = if (is.na(r$effect_value)) NA_real_ else round(r$effect_value, 4),
        Magnitude   = effect_magnitude(r$effect_name, r$effect_value),
        row.names = NULL, stringsAsFactors = FALSE)
      if (!is.null(split_by)) row$Stratum <- st$label
      rows[[key]] <- row
    }
    if (!is.null(split_by) && !stratum_hit)
      dropped_strata <- c(dropped_strata, st$label)
  }
  if (!length(keys)) return(NULL)

  smry <- do.call(rbind, rows); rownames(smry) <- NULL
  # p-values are adjusted across the WHOLE grid (all strata included) -- the
  # grid is the family of tests.
  smry$p_adj <- if (identical(p_adjust, "none")) smry$p_value
                else stats::p.adjust(smry$p_value, method = p_adjust)
  # Keep p_adj next to p_value; the Stratum column (if any) sits after Group.
  cols <- c("Outcome", "Group", if (!is.null(split_by)) "Stratum", "N", "k",
            "Test", "Statistic", "df", "p_value", "p_adj", "Effect",
            "Effect_size", "Magnitude")
  smry <- smry[, cols]
  list(summary = smry, results = results, keys = keys, p_adjust = p_adjust,
       outcomes = outcomes, groups = groups, split_by = split_by,
       split_capped = split_capped, dropped_strata = dropped_strata,
       n_split_na = n_split_na)
}

# --- two categorical variables ----------------------------------------------

#' Test association between two categorical variables.
#'
#' Runs Pearson's chi-square on the contingency table and reports Cramer's V.
#' When any expected cell count is < 5 the chi-square approximation is shaky, so
#' a Monte-Carlo Fisher's-exact p-value is added as a fallback.
#'
#' @param df A data frame.
#' @param var1,var2 Names of the two categorical columns.
#' @return A list (or NULL) with: table, expected, row_pct / col_pct / total_pct
#'   (percentages of each row, column and the grand total), residuals (Pearson),
#'   stdres (standardized, ~N(0,1)), statistic, df, p_value, fisher_p,
#'   low_expected (proportion of cells expected < 5), cramers_v, n.
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
    row_pct   = round(prop.table(tab, 1) * 100, 1),  # each row sums to 100
    col_pct   = round(prop.table(tab, 2) * 100, 1),  # each column sums to 100
    total_pct = round(prop.table(tab) * 100, 1),     # whole table sums to 100
    residuals = round(chi$residuals, 2),   # Pearson: (O - E) / sqrt(E)
    stdres    = round(chi$stdres, 2),      # standardized, ~N(0,1); flag |z| > 2
    statistic = unname(chi$statistic), df = unname(chi$parameter),
    p_value = chi$p.value, fisher_p = fisher_p,
    low_expected = low_exp,
    cramers_v = sqrt(unname(chi$statistic) / (n * (min(dim(tab)) - 1))),
    n = n
  )
}
