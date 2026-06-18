# ============================================================
# helpers_reshape.R -- pure data-reshaping verbs (JMP Tables menu)
# ============================================================
# Single-table reshape operations as plain, validated, testable
# functions. No reactivity, no Shiny calls -- mod_reshape.R is a thin
# wrapper that calls these. House style is set by do_stack(): validate
# inputs up front with a clear error, then delegate to tidyr.
#
# See docs/JMP_Tables_Menu_to_R.md for the JMP -> R mapping.

#' Stack columns into a label/value pair (wide -> tall)
#'
#' JMP Tables > Stack. Wraps tidyr::pivot_longer: collapses several columns
#' into one value column plus a label column recording the source column.
#' The inverse of do_split().
#'
#' @param data A data frame.
#' @param cols Character vector of column names to stack.
#' @param label_to Name for the new column holding the source column names.
#' @param value_to Name for the new column holding the stacked values.
#' @return A tibble in long form.
#' @examples
#' do_stack(data.frame(id = 1:2, q1 = c(5, 2), q2 = c(3, 4)), c("q1", "q2"))
#' @export
do_stack <- function(data, cols, label_to = "Label", value_to = "Data") {
  stopifnot(is.data.frame(data), is.character(cols), length(cols) >= 1L)
  missing <- setdiff(cols, names(data))
  if (length(missing)) {
    stop("Columns not found in data: ", paste(missing, collapse = ", "))
  }
  tidyr::pivot_longer(
    data,
    cols      = tidyselect::all_of(cols),
    names_to  = label_to,
    values_to = value_to
  )
}

#' Split one column into many (tall -> wide)
#'
#' JMP Tables > Split. Wraps tidyr::pivot_wider: spreads a value column across
#' new columns keyed by a "split by" column, keeping the group columns as the
#' row identity. The inverse of do_stack().
#'
#' @param data A data frame.
#' @param value_col Name of the column holding the values to spread
#'   (JMP "Split Columns").
#' @param split_by Name of the column whose distinct values become the new
#'   column headers (JMP "Split By").
#' @param group_cols Character vector of columns identifying a row, kept as-is
#'   (JMP "Group"). If NULL (default), every column except value_col and
#'   split_by is used as the grouping identity.
#' @param values_fn Optional function to resolve duplicate group/split-by
#'   combinations (e.g. mean). The default (NULL) is pivot_wider's behaviour:
#'   duplicates produce list-columns and a warning, so expose this in the UI.
#' @return A tibble in wide form.
#' @examples
#' long <- data.frame(id = c(1, 1, 2, 2), k = c("a", "b", "a", "b"),
#'                    v = c(5, 3, 2, 4))
#' do_split(long, value_col = "v", split_by = "k", group_cols = "id")
#' @export
do_split <- function(data, value_col, split_by, group_cols = NULL,
                     values_fn = NULL) {
  stopifnot(
    is.data.frame(data),
    is.character(value_col), length(value_col) == 1L,
    is.character(split_by),  length(split_by)  == 1L,
    is.null(group_cols) || is.character(group_cols)
  )
  missing <- setdiff(c(value_col, split_by, group_cols), names(data))
  if (length(missing)) {
    stop("Columns not found in data: ", paste(missing, collapse = ", "))
  }
  if (is.null(group_cols)) {
    group_cols <- setdiff(names(data), c(value_col, split_by))
  }
  tidyr::pivot_wider(
    data,
    id_cols     = tidyselect::all_of(group_cols),
    names_from  = tidyselect::all_of(split_by),
    values_from = tidyselect::all_of(value_col),
    values_fn   = values_fn
  )
}

#' Sort rows by one or more columns (tall stays tall)
#'
#' JMP Tables > Sort. Reorders rows by the given columns in priority order,
#' each independently ascending or descending. Missing values sort last.
#'
#' @param data A data frame.
#' @param cols Character vector of columns to sort by, highest priority first.
#' @param desc Logical, recycled to length(cols): TRUE = descending for the
#'   column in the matching position. Default FALSE (all ascending).
#' @return A tibble with the rows reordered.
#' @examples
#' do_sort(data.frame(g = c("a", "a", "b"), v = c(1, 2, 1)),
#'         c("g", "v"), desc = c(FALSE, TRUE))
#' @export
do_sort <- function(data, cols, desc = FALSE) {
  stopifnot(is.data.frame(data), is.character(cols), length(cols) >= 1L,
            is.logical(desc), length(desc) >= 1L)
  missing <- setdiff(cols, names(data))
  if (length(missing)) {
    stop("Columns not found in data: ", paste(missing, collapse = ", "))
  }
  desc <- rep_len(desc, length(cols))
  # unname() so a column literally named "method"/"decreasing" can't collide
  # with order()'s own arguments. radix is the only method that accepts a
  # per-key `decreasing` vector.
  keys <- unname(as.list(data[, cols, drop = FALSE]))
  ord  <- do.call(order, c(keys, list(decreasing = desc, method = "radix",
                                      na.last = TRUE)))
  tibble::as_tibble(data[ord, , drop = FALSE])
}

#' Subset rows and/or columns, with optional (stratified) random sampling
#'
#' JMP Tables > Subset. Keeps a chosen set of columns and/or a sample of rows.
#' Sampling can be a fixed count or a proportion, and can be stratified so the
#' sample is drawn within each group of `stratify_by`.
#'
#' @param data A data frame.
#' @param cols Character vector of columns to keep, or NULL (default) for all.
#' @param sample One of "all" (no row sampling), "n" (a fixed number of rows),
#'   or "prop" (a fraction of rows).
#' @param size For sample = "n", the number of rows; for "prop", a fraction in
#'   (0, 1]. Ignored when sample = "all".
#' @param stratify_by Optional column(s) to sample within (stratified sampling).
#' @param seed Optional integer RNG seed for a reproducible sample.
#' @return A tibble.
#' @examples
#' do_subset(data.frame(g = rep(c("a", "b"), each = 5), x = 1:10),
#'           sample = "prop", size = 0.5, stratify_by = "g", seed = 1)
#' @export
do_subset <- function(data, cols = NULL, sample = c("all", "n", "prop"),
                      size = NULL, stratify_by = NULL, seed = NULL) {
  sample <- match.arg(sample)
  stopifnot(is.data.frame(data),
            is.null(cols)        || is.character(cols),
            is.null(stratify_by) || is.character(stratify_by))
  missing <- setdiff(c(cols, stratify_by), names(data))
  if (length(missing)) {
    stop("Columns not found in data: ", paste(missing, collapse = ", "))
  }
  if (!is.null(seed)) set.seed(as.integer(seed))

  out <- data
  if (sample != "all") {
    if (is.null(size)) stop("`size` is required when sampling rows.")
    if (!is.null(stratify_by)) {
      out <- dplyr::group_by(out, dplyr::across(tidyselect::all_of(stratify_by)))
    }
    out <- if (sample == "n") {
      dplyr::slice_sample(out, n = as.integer(size))
    } else {
      dplyr::slice_sample(out, prop = size)
    }
    out <- dplyr::ungroup(out)
  }
  if (!is.null(cols)) {
    out <- dplyr::select(out, tidyselect::all_of(cols))
  }
  tibble::as_tibble(out)
}

#' Transpose a table -- swap rows and columns
#'
#' JMP Tables > Transpose. Implemented as a pivot round-trip (keeps types saner
#' than t(), which forces a matrix). One column can supply the new column
#' headers; the remaining columns are transposed into rows. When the transposed
#' columns are of mixed type the values become character (the honest result of
#' a true transpose).
#'
#' @param data A data frame.
#' @param names_from Optional single column whose values become the new column
#'   headers. NULL (default) generates V1, V2, ... headers from the row order.
#' @param id_col Name for the new column holding the original column names.
#' @return A tibble, transposed.
#' @examples
#' do_transpose(data.frame(metric = c("h", "w"), a = c(1, 10), b = c(2, 20)),
#'              names_from = "metric")
#' @export
do_transpose <- function(data, names_from = NULL, id_col = "name") {
  stopifnot(
    is.data.frame(data),
    is.null(names_from) || (is.character(names_from) && length(names_from) == 1L),
    is.character(id_col), length(id_col) == 1L
  )
  if (!is.null(names_from) && !names_from %in% names(data)) {
    stop("Columns not found in data: ", names_from)
  }
  if (is.null(names_from)) {
    data <- tibble::add_column(data, .row = paste0("V", seq_len(nrow(data))),
                               .before = 1L)
    names_from <- ".row"
  }
  others <- setdiff(names(data), names_from)
  # Coerce to character only when the columns being transposed are of mixed
  # type, so an all-numeric transpose stays numeric.
  types <- unique(vapply(data[others], function(z) class(z)[1], character(1)))
  vt    <- if (length(types) > 1L) as.character else NULL
  long  <- tidyr::pivot_longer(
    data, cols = tidyselect::all_of(others),
    names_to = id_col, values_to = ".value", values_transform = vt
  )
  tidyr::pivot_wider(long, names_from = tidyselect::all_of(names_from),
                     values_from = ".value")
}
