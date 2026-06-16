# Tests for the Data Health engine + type conversion in helpers_clean.R.

test_that("num_from_text strips currency, commas, and percent", {
  expect_equal(num_from_text(c("$1,000", "5%", "3")), c(1000, 5, 3))
})

test_that("is_numeric_text spots numbers-as-text but skips ID codes", {
  expect_true(is_numeric_text(c("1", "2", "$3")))
  expect_false(is_numeric_text(c("02134", "00501")))   # leading zeros => ID
})

test_that("detect_issues flags numbers-as-text and empty columns", {
  df  <- data.frame(b = c("1", "2", "3"), empty = c(NA, NA, NA),
                    stringsAsFactors = FALSE)
  ids <- names(detect_issues(df))
  expect_true("numeric" %in% ids)
  expect_true("empty_cols" %in% ids)
})

test_that("detect_issues is empty for clean data", {
  expect_length(detect_issues(data.frame(x = 1:3, y = c("a", "b", "c"))), 0)
})

test_that("clean_apply converts numbers-as-text and drops empty columns", {
  df  <- data.frame(b = c("1", "2", "3"), empty = c(NA, NA, NA),
                    stringsAsFactors = FALSE)
  out <- clean_apply(df, c("numeric", "empty_cols"))
  expect_true(is.numeric(out$b))
  expect_false("empty" %in% names(out))
})

test_that("clean_apply removes exact duplicate rows", {
  df  <- data.frame(a = c(1, 1, 2), b = c("x", "x", "y"))
  expect_equal(nrow(clean_apply(df, "dups")), 2L)
})

test_that("convert_column recasts a factor via its labels, not its codes", {
  f <- factor(c("10", "20", "30"))
  expect_equal(convert_column(f, "numeric"), c(10, 20, 30))   # not 1, 2, 3
  expect_s3_class(convert_column(1:3, "factor"), "factor")
})
