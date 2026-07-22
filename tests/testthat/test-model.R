# Tests for the regression helpers in helpers_model.R (base/stats only — the
# diagnostic ggplots are exercised in the PowerShell smoke test).

test_that("fit_model fits a simple linear model", {
  m <- fit_model(mtcars, "mpg", "wt", type = "linear")
  expect_s3_class(m, "lm")
  expect_equal(length(coef(m)), 2L)            # intercept + wt
})

test_that("fit_model builds a polynomial model of the given degree", {
  m <- fit_model(mtcars, "mpg", "wt", type = "polynomial", degree = 3)
  expect_s3_class(m, "lm")
  expect_equal(length(coef(m)), 4L)            # intercept + 3 poly terms
})

test_that("fit_model errors on unknown columns", {
  expect_error(fit_model(mtcars, "mpg", "nope"), "not found")
})

test_that("model_interpretation extracts R^2 and significance", {
  m    <- fit_model(mtcars, "mpg", c("wt", "hp"), type = "multiple")
  info <- model_interpretation(m)
  expect_true(info$r2 > 0.8)
  expect_true("wt" %in% info$significant)
  expect_true(is.numeric(info$overall_p) && info$overall_p < 0.05)
})

# ---- non-syntactic column names ---------------------------------------------
# Excel headers keep their spaces (readxl preserves them verbatim, unlike
# read.csv's check.names), so an unbackticked formula killed Regression for the
# most common import path with an opaque "unexpected symbol".

test_that("fit_model handles column names with spaces and other odd characters", {
  df <- mtcars
  names(df)[names(df) == "wt"]  <- "Body Weight"
  names(df)[names(df) == "hp"]  <- "Horse-Power (max)"
  names(df)[names(df) == "mpg"] <- "Miles per Gallon"

  m <- fit_model(df, "Miles per Gallon", c("Body Weight", "Horse-Power (max)"),
                 type = "multiple")
  expect_s3_class(m, "lm")
  expect_equal(unname(round(stats::coef(m), 4)),
               unname(round(stats::coef(
                 fit_model(mtcars, "mpg", c("wt", "hp"), type = "multiple")), 4)))
})

test_that("fit_model backticks the polynomial predictor too", {
  df <- mtcars; names(df)[names(df) == "wt"] <- "Body Weight"
  m  <- fit_model(df, "mpg", "Body Weight", type = "polynomial", degree = 2)
  expect_s3_class(m, "lm")
  expect_length(stats::coef(m), 3L)   # intercept + 2 poly terms
})
