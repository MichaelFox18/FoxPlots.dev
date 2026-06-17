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

test_that("outlier_iqr flags extreme values, not ordinary spread", {
  x <- c(10, 11, 12, 13, 12, 11, 10, 1000)   # 1000 is a far outlier
  f <- outlier_iqr(x, k = 3)
  expect_true(f[8])
  expect_false(any(f[1:7]))
  expect_equal(outlier_iqr(rep(5, 6)), rep(FALSE, 6))   # constant -> none
  expect_equal(outlier_iqr(c(1, 2, NA, 3)), c(FALSE, FALSE, FALSE, FALSE))
})

test_that("outlier_rows marks rows extreme in any numeric column", {
  df <- data.frame(a = c(1, 2, 3, 4, 999), b = c(10, 9, 11, 10, 10),
                   g = letters[1:5])           # g (text) ignored
  r  <- outlier_rows(df, k = 3)
  expect_equal(which(r), 5L)
  expect_length(r, nrow(df))
})

test_that("the outliers Data Health fix flags rows with an is_outlier column", {
  df  <- data.frame(x = c(1, 2, 3, 2, 1, 500), y = c(5, 6, 5, 6, 5, 6))
  iss <- detect_issues(df)
  expect_true("outliers" %in% names(iss))
  expect_false(iss$outliers$default)            # opt-in, not pre-checked
  out <- clean_apply(df, "outliers")
  expect_true("is_outlier" %in% names(out))
  expect_equal(which(out$is_outlier), 6L)
  expect_equal(nrow(out), nrow(df))             # nothing deleted
})
