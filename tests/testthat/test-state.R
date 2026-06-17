# Tests for helpers_state.R — the pure session save/restore pieces.

test_that("build_session_state captures the pieces and tags the object", {
  st <- build_session_state(
    data = head(mtcars, 5), data_raw = mtcars,
    filters = list(list(col = "cyl", op = ">=", value = 6)),
    source = "mtcars", reshape = list(op = "stack"))
  expect_s3_class(st, "foxplots_session")
  expect_equal(st$version, SESSION_STATE_VERSION)
  expect_equal(st$n, 5L)
  expect_equal(st$m, ncol(mtcars))
  expect_equal(nrow(st$data_raw), nrow(mtcars))
  expect_length(st$filters, 1L)
  expect_equal(st$reshape$op, "stack")
})

test_that("build_session_state defaults data_raw to data and filters to empty", {
  st <- build_session_state(mtcars)
  expect_identical(st$data_raw, st$data)
  expect_length(st$filters, 0L)
  expect_null(st$reshape)
  expect_error(build_session_state(1:10))   # not a data frame
})

test_that("validate_session_state accepts a good object", {
  expect_true(validate_session_state(build_session_state(mtcars)))
})

test_that("validate_session_state rejects bad inputs with a reason", {
  expect_match(validate_session_state(NULL), "empty|unreadable")
  expect_match(validate_session_state(list(a = 1)), "isn't a FoxPlots")
  # a future version
  st <- build_session_state(mtcars); st$version <- SESSION_STATE_VERSION + 1L
  expect_match(validate_session_state(st), "newer version")
  # corrupt data
  st2 <- build_session_state(mtcars); st2$data <- "oops"
  expect_match(validate_session_state(st2), "data is missing|corrupt")
  # corrupt filters
  st3 <- build_session_state(mtcars); st3$filters <- "nope"
  expect_match(validate_session_state(st3), "filters")
})

test_that("session_state_summary reports source, dims, filters, and reshape op", {
  st  <- build_session_state(
    head(mtcars, 8), source = "demo",
    filters = list(list(col = "cyl", op = ">=", value = 6)),
    reshape = list(op = "sort"))
  txt <- session_state_summary(st)
  expect_true(grepl("demo", txt))
  expect_true(grepl("8 .* ", txt))
  expect_true(grepl("1 filter", txt))
  expect_true(grepl("reshape: sort", txt))
  expect_equal(session_state_summary(list()), "")
})

test_that("save_session / load_session round-trip on disk", {
  f  <- withr::local_tempfile(fileext = ".rds")
  st <- build_session_state(mtcars, source = "rt",
                            filters = list(list(col = "mpg", op = ">", value = 20)))
  save_session(st, f)
  back <- load_session(f)
  expect_true(validate_session_state(back))
  expect_equal(back$source, "rt")
  expect_equal(nrow(back$data), nrow(mtcars))
  expect_equal(back$filters[[1]]$value, 20)
})

test_that("load_session returns NULL on a non-RDS file", {
  f <- withr::local_tempfile(fileext = ".rds")
  writeLines("not an rds", f)
  expect_null(load_session(f))
})
