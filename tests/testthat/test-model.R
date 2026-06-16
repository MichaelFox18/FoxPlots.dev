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
