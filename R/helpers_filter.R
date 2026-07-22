# ============================================================
# helpers_filter.R -- value-based row filtering
# ============================================================
# Pure helpers for the Import tab's "Filter rows" panel: keep only the rows
# that satisfy a set of conditions, combined with AND. A condition is a plain
# list -- list(col, op, value) -- so the module can build, store, and describe
# them without any Shiny in the logic.
#
#   numeric   op in between / >= / <= / > / < / == / !=   (value: scalar, or
#             c(lo, hi) for "between")
#   date      op = between                                (value: c(from, to))
#   category  op in in / not_in (value: character vector) | contains (value: text)
#
# Rows where the test is NA are dropped (matching dplyr::filter). Unknown
# columns or unusable values make a condition a no-op rather than an error, so
# a half-built or stale condition never breaks the pipeline.

# Logical keep-mask for ONE condition. Returns all-TRUE (a no-op) if the column
# is missing, so callers can apply conditions defensively.
filter_mask <- function(df, cond) {
  if (is.null(cond$col) || !cond$col %in% names(df)) return(rep(TRUE, nrow(df)))
  x <- df[[cond$col]]; v <- cond$value
  num <- function() suppressWarnings(as.numeric(v[1]))
  # The comparison operators are numeric-only (categories use in/not_in/
  # contains, dates use between). Dispatching on the operator alone used to
  # break when a column was recast AFTER its filter was built: comparing a
  # factor to a number warns, yields an all-NA mask, and line 43 turns that
  # into "keep zero rows" -- silently emptying every downstream tab while the
  # filter chip still reads "pts > 20". Fall back to the file's documented
  # no-op instead.
  cmp <- function(f) if (is_real_numeric(x)) f() else rep(TRUE, length(x))
  mask <- switch(cond$op %||% "",
    "between" = {
      if (inherits(x, c("Date", "POSIXct", "POSIXt"))) {
        vv <- as.Date(v); x >= min(vv) & x <= max(vv)
      } else if (is_real_numeric(x)) {
        vv <- suppressWarnings(as.numeric(v)); x >= min(vv) & x <= max(vv)
      } else {
        rep(TRUE, length(x))
      }
    },
    ">"  = cmp(function() x >  num()),
    ">=" = cmp(function() x >= num()),
    "<"  = cmp(function() x <  num()),
    "<=" = cmp(function() x <= num()),
    "==" = cmp(function() x == num()),
    "!=" = cmp(function() x != num()),
    "in"       = as.character(x) %in% as.character(v),
    "not_in"   = !(as.character(x) %in% as.character(v)),
    "contains" = grepl(tolower(as.character(v[1])), tolower(as.character(x)),
                       fixed = TRUE),
    rep(TRUE, length(x)))
  mask[is.na(mask)] <- FALSE
  mask
}

#' Keep the rows of `df` matching every condition (AND).
#'
#' @param df A data frame.
#' @param conditions A list of conditions, each list(col, op, value). An empty
#'   or NULL list returns `df` unchanged.
#' @return `df` with non-matching rows removed.
#' @examples
#' apply_filters(data.frame(team = c("LAL", "BOS"), pts = c(25, 18)),
#'               list(list(col = "pts", op = ">", value = 20)))
#' @export
apply_filters <- function(df, conditions) {
  if (!is.data.frame(df) || is.null(conditions) || !length(conditions)) return(df)
  keep <- rep(TRUE, nrow(df))
  for (cond in conditions) {
    m <- tryCatch(filter_mask(df, cond),
                  error = function(e) rep(TRUE, nrow(df)))
    if (length(m) == nrow(df)) keep <- keep & m
  }
  df[keep, , drop = FALSE]
}

#' Human-readable one-line description of a condition (for the filter chips).
#' @param cond A filter condition, `list(col, op, value)`.
#' @return A length-1 character label.
#' @export
describe_condition <- function(cond) {
  lab <- switch(cond$op %||% "",
    "between" = "is between", ">" = ">", ">=" = "\u2265", "<" = "<", "<=" = "\u2264",
    "==" = "=", "!=" = "\u2260", "in" = "is any of", "not_in" = "is none of",
    "contains" = "contains", cond$op %||% "?")
  v <- cond$value
  val <- if (identical(cond$op, "between")) paste(v[1], "and", v[2])
         else if (cond$op %in% c("in", "not_in")) paste(v, collapse = ", ")
         else if (identical(cond$op, "contains")) sprintf('"%s"', v[1])
         else as.character(v[1])
  sprintf("%s %s %s", cond$col, lab, val)
}
