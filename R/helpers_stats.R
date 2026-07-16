# ============================================================
# helpers_stats.R -- grouped summaries & column classification
# ============================================================
# Pure stats helpers lifted from the original Data Explorer, plus the
# column classifiers the UI uses to populate variable pickers. No Shiny,
# no reactivity -- mod_summarize.R is a thin wrapper over these.

# Snapshot the session RNG stream; call the returned function to restore it.
# Lets a helper use a fixed set.seed() for reproducibility WITHOUT leaving the
# whole session deterministic afterwards. A bare set.seed() in an example-data
# builder once parked the session RNG at a state where chromote's "random"
# debugging-port draw hit the same blocked port on every PNG export -- the
# map's snapshot button failed deterministically. Never bare-set.seed() in
# package code; pair it with this.
snapshot_rng <- function() {
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = globalenv())
  function() {
    if (had) {
      assign(".Random.seed", old, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
    invisible(NULL)
  }
}

# NA-safe scalar reducers: return NA on an empty / too-small group instead of
# warning or returning Inf/NaN (e.g. min() of an all-NA group).
.s_mean <- function(x) { x <- x[!is.na(x)]; if (!length(x))   NA_real_ else mean(x) }
.s_med  <- function(x) { x <- x[!is.na(x)]; if (!length(x))   NA_real_ else stats::median(x) }
.s_min  <- function(x) { x <- x[!is.na(x)]; if (!length(x))   NA_real_ else min(x) }
.s_max  <- function(x) { x <- x[!is.na(x)]; if (!length(x))   NA_real_ else max(x) }
.s_sd   <- function(x) { x <- x[!is.na(x)]; if (length(x) < 2) NA_real_ else stats::sd(x) }
.s_se   <- function(x) { x <- x[!is.na(x)]; if (length(x) < 2) NA_real_ else stats::sd(x) / sqrt(length(x)) }
.s_iqr  <- function(x) { x <- x[!is.na(x)]; if (!length(x))    NA_real_ else stats::IQR(x) }

# Mode = the most frequent value; NA when nothing repeats (e.g. continuous data
# where every value is unique), which is the honest answer for such columns.
.s_mode <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  tb <- table(x)
  if (max(tb) == 1) return(NA_real_)
  as.numeric(names(tb)[which.max(tb)])
}

#' Grouped summary statistics -- "summary stats by ___".
#'
#' For each combination of the grouping column(s), report N, mean, median,
#' mode, min, max, SD, SE (= SD/sqrtN), and IQR of each chosen numeric variable
#' (one row per group, per variable when several are chosen). All stats ignore
#' missing values.
#'
#' @param df A data frame.
#' @param vars Character vector of numeric columns to summarize.
#' @param groups Character vector of columns to group by.
#' @param digits Rounding for the reported statistics.
#' @return A tidy data frame [groups..., Variable, N, Mean, Median, Mode, Min,
#'   Max, SD, SE, IQR], or NULL if the inputs aren't usable.
#' @export
grouped_summary <- function(df, vars, groups, digits = 3) {
  if (is.null(df) || !length(vars) || !length(groups)) return(NULL)
  vars   <- intersect(vars, names(df))
  groups <- intersect(groups, names(df))
  if (!length(vars) || !length(groups)) return(NULL)
  per_var <- lapply(vars, function(v) {
    df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::summarise(
        Variable = v,
        N        = sum(!is.na(.data[[v]])),
        Mean     = round(.s_mean(.data[[v]]), digits),
        Median   = round(.s_med(.data[[v]]),  digits),
        Mode     = round(.s_mode(.data[[v]]), digits),
        Min      = round(.s_min(.data[[v]]),  digits),
        Max      = round(.s_max(.data[[v]]),  digits),
        SD       = round(.s_sd(.data[[v]]),   digits),
        SE       = round(.s_se(.data[[v]]),   digits),
        IQR      = round(.s_iqr(.data[[v]]),  digits),
        .groups  = "drop"
      )
  })
  res <- as.data.frame(dplyr::bind_rows(per_var), check.names = FALSE)
  res[, c(groups, "Variable", "N", "Mean", "Median", "Mode", "Min", "Max",
          "SD", "SE", "IQR"), drop = FALSE]
}

#' Proportions of a categorical outcome within each group, with exact CIs.
#'
#' For each group, count how many rows fall in each level of `outcome`, then
#' report the percentage and an exact (Clopper-Pearson) binomial confidence
#' interval via binom::binom.confint(). The categorical analogue of
#' grouped_summary() -- "percent of counts by group".
#'
#' @param df A data frame.
#' @param outcome Name of the categorical outcome column (first is used).
#' @param groups Character vector of columns to group by.
#' @param conf_level Confidence level for the interval (default 0.95).
#' @param digits Rounding for the reported percentages.
#' @return A tidy data frame [groups..., Level, N, Total, Percent, CI_low,
#'   CI_high] (percentages 0-100), or NULL if the inputs aren't usable.
#' @export
proportions_summary <- function(df, outcome, groups, conf_level = 0.95,
                                digits = 1) {
  if (is.null(df) || !length(outcome) || !length(groups)) return(NULL)
  outcome <- outcome[[1]]
  if (!all(c(outcome, groups) %in% names(df))) return(NULL)
  keep <- !is.na(df[[outcome]])
  df   <- df[keep, , drop = FALSE]
  if (!nrow(df)) return(NULL)

  counts <- dplyr::count(df, dplyr::across(dplyr::all_of(c(groups, outcome))),
                         name = "N")
  totals <- counts |>
    dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
    dplyr::summarise(Total = sum(.data[["N"]]), .groups = "drop")
  out <- dplyr::left_join(counts, totals, by = groups)

  ci <- binom::binom.confint(out$N, out$Total, conf.level = conf_level,
                             methods = "exact")
  out$Percent <- round(100 * out$N / out$Total, digits)
  out$CI_low  <- round(100 * ci$lower, digits)
  out$CI_high <- round(100 * ci$upper, digits)
  names(out)[names(out) == outcome] <- "Level"
  as.data.frame(out)[, c(groups, "Level", "N", "Total", "Percent",
                         "CI_low", "CI_high"), drop = FALSE]
}

#' A plain-English column type for display ("numeric", "factor", "date", ...).
#' @param x A vector (a data-frame column).
#' @return A length-1 character label.
#' @export
friendly_type <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXt"))) "date"
  else if (is.logical(x)) "logical"
  else if (is.factor(x))  "factor"
  else if (is.integer(x)) "integer"
  else if (is.numeric(x)) "numeric"
  else "text"
}

#' One-row-per-column profile: type, % missing, distinct count, and either
#' numeric stats (mean/median/min/max) or the most common value. Needs
#' blank_cell() from helpers_clean.R.
#' @param df A data frame.
#' @return A data frame, one row per input column.
#' @export
column_profile <- function(df) {
  if (is.null(df) || !ncol(df)) return(data.frame())
  n <- nrow(df)
  do.call(rbind, lapply(names(df), function(nm) {
    x    <- df[[nm]]
    num  <- is.numeric(x)
    hasv <- any(!is.na(x))
    miss <- sum(blank_cell(x))
    top  <- if (!num) {
      tb <- sort(table(as.character(x)), decreasing = TRUE)
      if (length(tb)) sprintf("%s (%s)", substr(names(tb)[1], 1, 40),
                              format(tb[[1]], big.mark = ",")) else NA_character_
    } else NA_character_
    data.frame(
      Column   = nm,
      Type     = friendly_type(x),
      Missing  = sprintf("%d (%.0f%%)", miss, if (n) 100 * miss / n else 0),
      Distinct = dplyr::n_distinct(x),
      Mean     = if (num && hasv) round(mean(x, na.rm = TRUE), 3) else NA_real_,
      Median   = if (num && hasv) round(stats::median(x, na.rm = TRUE), 3) else NA_real_,
      Min      = if (num && hasv) round(min(x, na.rm = TRUE), 3) else NA_real_,
      Max      = if (num && hasv) round(max(x, na.rm = TRUE), 3) else NA_real_,
      Top      = top,
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }))
}

#' Headline counts for the "at a glance" summary.
#' @param df A data frame.
#' @return A list: n, m, num, cat, date, complete.
#' @export
data_glance <- function(df) {
  types <- vapply(df, friendly_type, character(1))
  list(
    n        = nrow(df), m = ncol(df),
    num      = sum(types %in% c("numeric", "integer")),
    cat      = sum(types %in% c("text", "factor", "logical")),
    date     = sum(types == "date"),
    complete = sum(stats::complete.cases(df))
  )
}

#' Numeric columns suitable for summarizing / plotting on a quantitative axis.
#'
#' @param df A data frame.
#' @return Character vector of column names (numeric, excluding dates).
#' @export
numeric_cols <- function(df) {
  if (is.null(df) || !ncol(df)) return(character(0))
  keep <- vapply(df, function(x)
    is.numeric(x) && !inherits(x, c("Date", "POSIXct", "POSIXt")),
    logical(1))
  names(df)[keep]
}

#' Low-cardinality columns suitable for grouping / faceting.
#'
#' Any column with at most `max_levels` distinct values -- so categorical and
#' coded-numeric columns (cyl, a 1-5 rating) qualify, but a high-cardinality ID
#' column can't blow up the grouping into thousands of rows.
#'
#' @param df A data frame.
#' @param max_levels Maximum distinct values to count as groupable.
#' @return Character vector of column names.
#' @export
groupable_cols <- function(df, max_levels = 30L) {
  if (is.null(df) || !ncol(df)) return(character(0))
  keep <- vapply(df, function(x)
    dplyr::n_distinct(x, na.rm = TRUE) <= max_levels, logical(1))
  names(df)[keep]
}
