# ============================================================
# helpers_combine.R -- two-table operations (JMP Tables menu)
# ============================================================
# Pure helpers for the operations that need a SECOND table -- the half of
# JMP's Tables menu that doesn't fit the single-table `reshape` module.
# mod_combine.R is a thin wrapper. dplyr-qualified; no Shiny.
#
# See docs/JMP_Tables_Menu_to_R.md: Concatenate -> bind_rows, Join -> *_join /
# cross_join, Update -> rows_update / rows_patch, Compare -> a diff summary.

#' Concatenate two tables (append rows).
#'
#' JMP Tables > Concatenate. Stacks `right` beneath `left`, matching columns by
#' name; columns present in only one table are filled with NA.
#'
#' @param left,right Data frames.
#' @param add_source Add a column recording which table each row came from?
#' @param source_col Name for that column.
#' @return A tibble.
#' @export
do_concatenate <- function(left, right, add_source = FALSE,
                           source_col = "source") {
  stopifnot(is.data.frame(left), is.data.frame(right))
  if (add_source)
    dplyr::bind_rows(left = left, right = right, .id = source_col)
  else
    dplyr::bind_rows(left, right)
}

#' Join two tables side by side by key (or as a Cartesian product).
#'
#' JMP Tables > Join. Wraps dplyr's joins. include-only-matches = inner;
#' keep-left = left; keep-both = full; plus right and cross (all-pairs).
#'
#' @param left,right Data frames.
#' @param by Character vector of key columns present in both tables. Ignored for
#'   type = "cross".
#' @param type One of "left", "inner", "full", "right", "cross".
#' @return A tibble.
#' @export
do_join <- function(left, right, by = NULL,
                    type = c("left", "inner", "full", "right", "cross")) {
  type <- match.arg(type)
  stopifnot(is.data.frame(left), is.data.frame(right))
  if (type == "cross") return(dplyr::cross_join(left, right))
  if (is.null(by) || !length(by)) stop("Choose at least one key column to join by.")
  miss <- c(setdiff(by, names(left)), setdiff(by, names(right)))
  if (length(miss))
    stop("Join key(s) not found in both tables: ",
         paste(unique(miss), collapse = ", "))
  fn <- switch(type,
    left  = dplyr::left_join,  inner = dplyr::inner_join,
    full  = dplyr::full_join,  right = dplyr::right_join)
  fn(left, right, by = by)
}

#' Update values in one table from another, matched by key.
#'
#' JMP Tables > Update. "overwrite" replaces matching values (rows_update);
#' "fill" only fills blanks in `main` (rows_patch) -- the JMP "don't replace
#' existing columns" behaviour. Updates columns not present in `main` are
#' ignored; rows in `updates` that don't match `main` are ignored.
#'
#' @param main The table to update.
#' @param updates The table supplying new values.
#' @param by Character vector of key columns present in both tables.
#' @param mode "overwrite" or "fill".
#' @return A tibble the shape of `main`.
#' @export
do_update <- function(main, updates, by = NULL,
                      mode = c("overwrite", "fill")) {
  mode <- match.arg(mode)
  stopifnot(is.data.frame(main), is.data.frame(updates))
  if (is.null(by) || !length(by)) stop("Choose at least one key column to match on.")
  miss <- c(setdiff(by, names(main)), setdiff(by, names(updates)))
  if (length(miss))
    stop("Match key(s) not found in both tables: ",
         paste(unique(miss), collapse = ", "))
  keep    <- union(by, intersect(names(updates), names(main)))
  updates <- updates[, keep, drop = FALSE]
  if (mode == "overwrite")
    dplyr::rows_update(main, updates, by = by, unmatched = "ignore")
  else
    dplyr::rows_patch(main, updates, by = by, unmatched = "ignore")
}

#' Compare two tables and summarise their differences.
#'
#' JMP Tables > Compare Data Tables. Returns one row per column (union of both
#' tables) noting which table it appears in and -- when the row counts match --
#' how many cells differ in each shared column.
#'
#' @param left,right Data frames.
#' @return A data frame [Column, In_left, In_right, Differing].
#' @export
compare_tables <- function(left, right) {
  stopifnot(is.data.frame(left), is.data.frame(right))
  cols   <- union(names(left), names(right))
  same_n <- nrow(left) == nrow(right)
  do.call(rbind, lapply(cols, function(c) {
    in_l <- c %in% names(left)
    in_r <- c %in% names(right)
    diff <- if (in_l && in_r && same_n) {
      a <- left[[c]]; b <- right[[c]]
      sum(!((is.na(a) & is.na(b)) |
            (!is.na(a) & !is.na(b) & as.character(a) == as.character(b))))
    } else NA_integer_
    data.frame(Column = c,
               In_left  = if (in_l) "yes" else "no",
               In_right = if (in_r) "yes" else "no",
               Differing = as.integer(diff),
               stringsAsFactors = FALSE)
  }))
}

# near_match_columns -- flag column names that ALMOST match across tables.
# Pairs like "Country" / "country" differ only by case or whitespace --
# almost certainly an intended join key -- so the combine UI can explain an
# empty key picker instead of leaving it silently blank. Exact shared names
# are excluded first; matching is case/whitespace only, NO fuzzy/edit-distance
# matching. Returns a zero-row data frame when there is nothing to flag.
# Internal (not exported).
near_match_columns <- function(left_names, right_names) {
  stopifnot(is.character(left_names), is.character(right_names))
  shared <- intersect(left_names, right_names)
  l <- setdiff(left_names, shared)
  r <- setdiff(right_names, shared)
  norm <- function(x) tolower(trimws(x))
  hit  <- match(norm(l), norm(r))
  keep <- !is.na(hit)
  data.frame(left = l[keep], right = r[hit[keep]], stringsAsFactors = FALSE)
}
