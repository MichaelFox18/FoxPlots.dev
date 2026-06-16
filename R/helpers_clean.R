# ============================================================
# helpers_clean.R — Data Health & variable type conversion
# ============================================================
# Pure functions for diagnosing common spreadsheet problems and applying
# opt-in, reversible fixes, plus recasting a single column's type. Detection
# and the fix transform live together in clean_specs() so they can't drift
# apart. No Shiny — mod_import.R wraps these. Base R only.

# Strings treated as "missing" placeholders. "-"/"." are deliberately excluded
# so legitimate category values aren't clobbered.
NA_TOKENS <- c("", "NA", "N/A", "n/a", "NULL", "null", "#N/A", "#n/a")

# Parse a character vector as numbers after stripping $, commas, %, spaces.
num_from_text <- function(x) suppressWarnings(as.numeric(gsub("[,$%[:space:]]", "", x)))

# Fraction of real values that parse cleanly as numbers, ignoring blanks and NA
# placeholders so a stray "N/A" doesn't hide a numeric column.
numeric_frac <- function(x) {
  v <- x[!is.na(x) & !(trimws(x) %in% NA_TOKENS)]
  if (!length(v)) return(0)
  mean(!is.na(num_from_text(v)))
}

# TRUE if any real value is a leading-zero code (e.g. "02134", "007"): a sign the
# column is an identifier (ZIP, phone) where converting to a number would
# silently drop the leading zero, so we leave such columns as text.
has_leading_zeros <- function(x) {
  v <- x[!is.na(x) & !(trimws(x) %in% NA_TOKENS)]
  length(v) > 0 && any(grepl("^0[0-9]", trimws(v)))
}

# TRUE if x is character, looks numeric (>= 90% parse), and is not an ID column.
is_numeric_text <- function(x) {
  is.character(x) && !has_leading_zeros(x) && numeric_frac(x) >= 0.9
}

# Returns a Date vector if >= 90% of real values match an unambiguous ISO
# format, else NULL (we avoid guessing m/d vs d/m).
dates_from_text <- function(x) {
  keep <- !is.na(x) & !(trimws(x) %in% NA_TOKENS)
  if (!any(keep)) return(NULL)
  for (fmt in c("%Y-%m-%d", "%Y/%m/%d")) {
    d <- suppressWarnings(as.Date(x, format = fmt))
    if (mean(!is.na(d[keep])) >= 0.9) return(d)
  }
  NULL
}

# Logical "is this cell blank" — short-circuits on type so it never coerces a
# numeric column to character (which dominates the cost on large data).
blank_cell <- function(v) {
  if (is.character(v)) is.na(v) | trimws(v) == "" else is.na(v)
}

# Each spec: default (pre-checked?), detect(df) -> HTML string or NULL,
# apply(df) -> df. Order here is the order fixes are applied.
clean_specs <- function() list(
  names = list(
    default = TRUE,
    detect = function(df) {
      nm  <- names(df)
      bad <- sum(is.na(nm) | trimws(nm) == "" | duplicated(nm))
      if (bad == 0) return(NULL)
      sprintf("<b>Column names:</b> make %d blank or duplicated name(s) unique.", bad)
    },
    apply = function(df) {
      nm <- trimws(names(df)); nm[is.na(nm) | nm == ""] <- "V"
      names(df) <- make.unique(nm, sep = "_"); df
    }
  ),
  trim = list(
    default = TRUE,
    detect = function(df) {
      ch    <- vapply(df, is.character, logical(1))
      cells <- if (any(ch)) sum(vapply(df[ch], function(v)
                 sum(!is.na(v) & v != trimws(v)), integer(1))) else 0L
      nmws  <- sum(names(df) != trimws(names(df)))
      if (cells == 0 && nmws == 0) return(NULL)
      sprintf("<b>Whitespace:</b> trim leading/trailing spaces from %d value(s)%s.",
              cells, if (nmws) sprintf(" and %d header(s)", nmws) else "")
    },
    apply = function(df) {
      names(df) <- trimws(names(df))
      for (c in names(df)) if (is.character(df[[c]])) df[[c]] <- trimws(df[[c]])
      df
    }
  ),
  na_tokens = list(
    default = TRUE,
    detect = function(df) {
      ch    <- vapply(df, is.character, logical(1))
      cells <- if (any(ch)) sum(vapply(df[ch], function(v)
                 sum(!is.na(v) & v %in% NA_TOKENS), integer(1))) else 0L
      if (cells == 0) return(NULL)
      sprintf("<b>Missing-value markers:</b> convert %d placeholder cell(s) (e.g. \"N/A\", blank) to true missing (NA).", cells)
    },
    apply = function(df) {
      for (c in names(df)) if (is.character(df[[c]])) {
        v <- df[[c]]; v[v %in% NA_TOKENS] <- NA; df[[c]] <- v
      }
      df
    }
  ),
  numeric = list(
    default = TRUE,
    detect = function(df) {
      cols <- names(df)[vapply(df, is_numeric_text, logical(1))]
      if (!length(cols)) return(NULL)
      sprintf("<b>Numbers stored as text:</b> convert %s to numeric (strips $, commas, %%; ID-style columns with leading zeros are left alone).",
              paste(sprintf("<code>%s</code>", cols), collapse = ", "))
    },
    apply = function(df) {
      for (c in names(df)) if (is_numeric_text(df[[c]])) df[[c]] <- num_from_text(df[[c]])
      df
    }
  ),
  dates = list(
    default = FALSE,
    detect = function(df) {
      cols <- names(df)[vapply(df, function(v)
        is.character(v) && !is.null(dates_from_text(v)), logical(1))]
      if (!length(cols)) return(NULL)
      sprintf("<b>Dates stored as text:</b> convert %s to Date (ISO yyyy-mm-dd).",
              paste(sprintf("<code>%s</code>", cols), collapse = ", "))
    },
    apply = function(df) {
      for (c in names(df)) if (is.character(df[[c]])) {
        d <- dates_from_text(df[[c]]); if (!is.null(d)) df[[c]] <- d
      }
      df
    }
  ),
  empty_cols = list(
    default = TRUE,
    detect = function(df) {
      if (!nrow(df)) return(NULL)
      n <- sum(vapply(df, function(v) all(blank_cell(v)), logical(1)))
      if (n == 0) return(NULL)
      sprintf("<b>Empty columns:</b> drop %d column(s) that are entirely blank.", n)
    },
    apply = function(df) {
      if (!nrow(df)) return(df)
      df[, !vapply(df, function(v) all(blank_cell(v)), logical(1)), drop = FALSE]
    }
  ),
  empty_rows = list(
    default = TRUE,
    detect = function(df) {
      if (!nrow(df) || !ncol(df)) return(NULL)
      m <- sapply(df, blank_cell)
      if (is.null(dim(m))) m <- matrix(m, nrow = nrow(df))
      n <- sum(rowSums(m) == ncol(df))
      if (n == 0) return(NULL)
      sprintf("<b>Empty rows:</b> drop %d row(s) that are entirely blank.", n)
    },
    apply = function(df) {
      if (!nrow(df) || !ncol(df)) return(df)
      m <- sapply(df, blank_cell)
      if (is.null(dim(m))) m <- matrix(m, nrow = nrow(df))
      df[rowSums(m) != ncol(df), , drop = FALSE]
    }
  ),
  dups = list(
    default = TRUE,
    detect = function(df) {
      n <- sum(duplicated(df))
      if (n == 0) return(NULL)
      sprintf("<b>Duplicate rows:</b> remove %d exact duplicate row(s).", n)
    },
    apply = function(df) df[!duplicated(df), , drop = FALSE]
  )
)

# Issues present in df, in spec order: named list of list(id, desc, default).
detect_issues <- function(df) {
  specs <- clean_specs()
  out   <- list()
  for (id in names(specs)) {
    d <- specs[[id]]$detect(df)
    if (!is.null(d)) out[[id]] <- list(id = id, desc = d, default = specs[[id]]$default)
  }
  out
}

# Apply the selected fix ids (always in canonical spec order).
clean_apply <- function(df, ids) {
  specs <- clean_specs()
  for (id in names(specs)) if (id %in% ids) df <- specs[[id]]$apply(df)
  df
}

# --- Variable type conversion -----------------------------------------------
# Target types offered in the UI (display label -> internal code).
CONVERT_TYPES <- c(
  "Factor (category)"      = "factor",
  "Text"                   = "character",
  "Number (decimal)"       = "numeric",
  "Whole number (integer)" = "integer",
  "TRUE / FALSE (logical)" = "logical",
  "Date (ISO yyyy-mm-dd)"  = "date"
)

# Recast a vector to `to`. To a number, character/factor input is run through
# num_from_text (strips $ , %) and a factor is routed via its LABELS, never the
# hidden integer codes (the classic as.numeric(factor) trap). Values that can't
# be parsed become NA; the caller reports how many.
convert_column <- function(x, to) {
  as_num <- function(z)
    if (is.character(z) || is.factor(z)) num_from_text(as.character(z)) else as.numeric(z)
  switch(to,
    factor    = as.factor(x),
    character = as.character(x),
    numeric   = as_num(x),
    integer   = as.integer(round(as_num(x))),
    logical   = as.logical(if (is.factor(x)) as.character(x) else x),
    date      = as.Date(as.character(x),
                        tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y", "%d/%m/%Y")),
    x)
}
