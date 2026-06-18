# ============================================================
# helpers_state.R -- save / restore a working session
# ============================================================
# A "session" captures the data-prep stage so a user can pick up later: the
# working data (after Data Health fixes + type recasts), the originally loaded
# data (so Revert still works), the row filters, and the reshape operation +
# its settings. Analysis-tab choices are intentionally NOT saved.
#
# Pure + versioned: build_session_state() normalizes the pieces into a tagged,
# versioned list; validate_session_state() checks a loaded object before it's
# trusted; session_state_summary() renders a one-line human description. The
# actual file I/O (save_session/load_session) is a thin saveRDS/readRDS pair.
# mod_import.R / mod_reshape.R are the thin wrappers that call these.

# Bump when the saved structure changes incompatibly. validate_session_state()
# refuses a file written by a NEWER version than the running app understands.
SESSION_STATE_VERSION <- 1L

#' Assemble a portable session-state object.
#'
#' @param data The working data frame (after fixes/recasts). Required.
#' @param data_raw The originally loaded data (defaults to `data`).
#' @param filters A list of filter conditions (each list(col, op, value)).
#' @param source A short label for where the data came from.
#' @param reshape A list of the reshape module's settings (op + inputs), or NULL.
#' @param created A POSIXct timestamp.
#' @return A list tagged with class "foxplots_session".
#' @export
build_session_state <- function(data, data_raw = NULL, filters = list(),
                                source = NULL, reshape = NULL,
                                created = Sys.time()) {
  stopifnot(is.data.frame(data))
  if (is.null(data_raw)) data_raw <- data
  if (is.null(filters))  filters  <- list()
  structure(
    list(
      version  = SESSION_STATE_VERSION,
      created  = created,
      source   = source %||% "session",
      data     = data,
      data_raw = data_raw,
      filters  = filters,
      reshape  = reshape,
      n        = nrow(data),
      m        = ncol(data)
    ),
    class = "foxplots_session"
  )
}

#' Validate a loaded object before restoring from it.
#'
#' @param x An object read back from a session file.
#' @return TRUE if usable, otherwise a one-line character reason (so the caller
#'   can show a friendly message).
#' @export
validate_session_state <- function(x) {
  if (is.null(x))                        return("the file is empty or unreadable")
  if (!inherits(x, "foxplots_session"))  return("this isn't a FoxPlots session file")
  ver <- x$version %||% NA_integer_
  if (is.na(ver))                        return("the file is missing its version tag")
  if (ver > SESSION_STATE_VERSION)
    return(sprintf("it was saved by a newer version (v%s) than this app supports (v%s)",
                   ver, SESSION_STATE_VERSION))
  if (!is.data.frame(x$data))            return("the saved data is missing or corrupt")
  if (!is.null(x$filters) && !is.list(x$filters))
    return("the saved filters are corrupt")
  if (!is.null(x$reshape) && !is.list(x$reshape))
    return("the saved reshape settings are corrupt")
  TRUE
}

#' One-line human summary of a session (for a restore notification / preview).
#' @param x A session object from [build_session_state()].
#' @return A one-line character description (or "" if `x` isn't a session).
#' @export
session_state_summary <- function(x) {
  if (!inherits(x, "foxplots_session")) return("")
  nf <- length(x$filters %||% list())
  op <- if (is.list(x$reshape) && !is.null(x$reshape$op)) x$reshape$op else "none"
  sprintf("%s \u00b7 %s \u00d7 %s \u00b7 %d filter%s \u00b7 reshape: %s \u00b7 saved %s",
          x$source %||% "session",
          format(x$n %||% nrow(x$data), big.mark = ","),
          x$m %||% ncol(x$data),
          nf, if (nf == 1L) "" else "s", op,
          format(x$created %||% Sys.time(), "%Y-%m-%d %H:%M"))
}

#' Write a session object to an `.rds` file (thin saveRDS wrapper).
#' @param state A session object from [build_session_state()].
#' @param file Output path.
#' @return `file`, invisibly.
#' @export
save_session <- function(state, file) { saveRDS(state, file); invisible(file) }
load_session <- function(file) tryCatch(readRDS(file), error = function(e) NULL)
