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
