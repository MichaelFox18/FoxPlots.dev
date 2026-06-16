# ============================================================
# helpers_io.R — reading tabular files robustly
# ============================================================
# Pure file-reading helpers, lifted from the original Data Explorer.
# No Shiny, no reactivity — mod_import.R is a thin wrapper over these.
# Namespace-qualified (readxl::, utils::) so callers needn't attach the
# packages.

#' Find the real table inside a messy delimited file.
#'
#' Many real-world CSVs (e.g. BEA exports) start with a few title lines and end
#' with quoted footnotes. The real table is the span of lines whose field count
#' matches the table's column count (the most common field count among
#' delimited lines). Given the per-line field counts, return the [start, end]
#' physical-line span of that table. Only leading/trailing non-matching lines
#' are trimmed — everything between the first and last data-shaped line is kept,
#' so an interior blank or stray line can never drop valid rows.
#'
#' @param fields Integer vector of per-line field counts (from count.fields).
#' @return A list(start, end) of 1-based physical line numbers.
detect_table_bounds <- function(fields) {
  none <- list(start = 1L, end = length(fields))
  if (!length(fields)) return(list(start = 1L, end = 0L))
  fields[is.na(fields)] <- 0L
  if (max(fields) < 2) return(none)             # nothing delimited; leave as-is
  delim <- fields[fields >= 2]
  mfc   <- as.integer(names(which.max(table(delim))))  # table's column count
  at    <- which(fields == mfc)
  list(start = min(at), end = max(at))
}

#' Read a delimited file, auto-trimming title/footnote lines.
#'
#' Uses detect_table_bounds() to find the data span, then reads just that span.
#' Records how many leading/trailing lines were skipped as the attributes
#' "n_skip_head" / "n_skip_tail" so the caller can report it.
#'
#' @param path File path.
#' @param sep Field separator.
#' @param header Logical: is the first data row a header?
#' @param dec Decimal mark.
#' @param reader The base reader to delegate to (read.csv or read.table).
#' @return A data frame with n_skip_head / n_skip_tail attributes.
read_delim_smart <- function(path, sep, header, dec, reader) {
  # blank.lines.skip = FALSE keeps the field-count vector aligned 1:1 with the
  # physical lines, so the [start, end] span can be sliced exactly.
  fields <- tryCatch(
    utils::count.fields(path, sep = sep, quote = "\"",
                        comment.char = "", blank.lines.skip = FALSE),
    error = function(e) integer(0))
  total  <- length(fields)
  b      <- detect_table_bounds(fields)
  base   <- list(header = header, sep = sep, dec = dec,
                 stringsAsFactors = FALSE, fill = TRUE, blank.lines.skip = TRUE)
  if (b$start <= 1L && b$end >= total) {
    d <- do.call(reader, c(list(file = path), base))           # clean: read directly
  } else {
    lines <- readLines(path, warn = FALSE)
    d <- do.call(reader, c(list(text = lines[b$start:b$end]), base))
  }
  attr(d, "n_skip_head") <- max(0L, b$start - 1L)
  attr(d, "n_skip_tail") <- max(0L, total - b$end)
  d
}

#' Read a tabular file by extension (CSV / TSV / TXT / Excel / RDS).
#'
#' @param path File path.
#' @param ext File extension (without the dot), case-insensitive.
#' @param header Logical: first row is a header (delimited files).
#' @param sep Field separator (delimited files; ignored for TSV, which forces tab).
#' @param dec Decimal mark.
#' @param sheet Worksheet name or index for Excel files.
#' @return A data frame. Delimited reads carry n_skip_head / n_skip_tail attrs.
read_file_data <- function(path, ext, header = TRUE, sep = ",", dec = ".",
                           sheet = 1) {
  ext <- tolower(ext)
  if (ext %in% c("xlsx", "xls"))
    return(as.data.frame(readxl::read_excel(path, sheet = sheet)))
  if (ext == "rds")
    return(as.data.frame(readRDS(path)))
  if (ext == "csv")
    return(read_delim_smart(path, sep, header, dec, utils::read.csv))
  if (ext %in% c("tsv", "txt"))
    return(read_delim_smart(path, if (ext == "tsv") "\t" else sep, header, dec,
                            utils::read.table))
  stop("Unsupported file extension: .", ext)
}
