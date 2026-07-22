# Tests for the file-reading helpers in R/helpers_io.R.
# Helpers are sourced by setup.R before this file runs.

# ---- detect_table_bounds -----------------------------------------------------

test_that("detect_table_bounds keeps a clean table whole", {
  expect_equal(detect_table_bounds(c(3, 3, 3)), list(start = 1L, end = 3L))
})

test_that("detect_table_bounds trims title and footnote lines", {
  # two 1-field title lines, three 3-field data lines, one 1-field footnote
  b <- detect_table_bounds(c(1, 1, 3, 3, 3, 1))
  expect_equal(b$start, 3L)
  expect_equal(b$end, 5L)
})

test_that("detect_table_bounds leaves a non-delimited file alone", {
  expect_equal(detect_table_bounds(c(1, 1, 1)), list(start = 1L, end = 3L))
})

# ---- read_file_data (CSV round-trip) ----------------------------------------

test_that("read_file_data reads a CSV and trims title/footnote lines", {
  f <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("Some report title", "generated today",
               "a,b,c", "1,2,3", "4,5,6", "\"a footnote\""), f)
  d <- read_file_data(f, "csv")
  expect_named(d, c("a", "b", "c"))
  expect_equal(nrow(d), 2L)
  expect_equal(attr(d, "n_skip_head"), 2L)
  expect_equal(attr(d, "n_skip_tail"), 1L)
})

test_that("read_file_data errors on an unsupported extension", {
  expect_error(read_file_data("x.foo", "foo"), "Unsupported")
})

# ---- list-column flattening --------------------------------------------------
# Reshape > Split with duplicate keys yields list-columns. write.csv() dies on
# them ("unimplemented type 'list'", surfaced as an opaque 500) and writexl
# silently writes BLANK columns, so mod_export flattens first.

test_that("flatten_list_cols collapses list-columns and leaves others alone", {
  df <- data.frame(id = 1:2)
  df$vals <- list(c("a", "b"), "c")
  out <- flatten_list_cols(df)
  expect_false(any(vapply(out, is.list, logical(1))))
  expect_equal(out$vals, c("a; b", "c"))
  expect_equal(out$id, 1:2)
})

test_that("flatten_list_cols is a no-op on a plain data frame", {
  expect_equal(flatten_list_cols(mtcars), mtcars)
})

test_that("a flattened list-column can actually be written to CSV and XLSX", {
  df <- data.frame(id = 1:2); df$vals <- list(c("a", "b"), "c")
  f1 <- withr::local_tempfile(fileext = ".csv")
  expect_error(utils::write.csv(flatten_list_cols(df), f1, row.names = FALSE), NA)
  expect_equal(nrow(utils::read.csv(f1)), 2L)
  f2 <- withr::local_tempfile(fileext = ".xlsx")
  expect_error(writexl::write_xlsx(flatten_list_cols(df), f2), NA)
  expect_equal(as.character(readxl::read_excel(f2)$vals), c("a; b", "c"))
})
