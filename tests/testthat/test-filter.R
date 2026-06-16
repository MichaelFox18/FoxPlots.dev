# Tests for the row-filtering helpers in helpers_filter.R — pure base R.

df <- data.frame(
  team = c("LAL", "BOS", "LAL", "MIA", "BOS"),
  pos  = c("G",   "F",   "C",   "G",   "F"),
  pts  = c(25,    18,    30,    NA,    12),
  stringsAsFactors = FALSE
)

test_that("numeric comparison keeps matching rows and drops NA", {
  out <- apply_filters(df, list(list(col = "pts", op = ">", value = 20)))
  expect_equal(nrow(out), 2L)                 # 25, 30 (NA excluded)
  expect_setequal(out$pts, c(25, 30))
})

test_that("between is inclusive on both ends", {
  out <- apply_filters(df, list(list(col = "pts", op = "between", value = c(12, 18))))
  expect_setequal(out$pts, c(18, 12))
})

test_that("categorical in / not_in select levels", {
  expect_setequal(
    apply_filters(df, list(list(col = "team", op = "in", value = "LAL")))$team,
    c("LAL", "LAL"))
  expect_setequal(
    apply_filters(df, list(list(col = "team", op = "not_in", value = "LAL")))$team,
    c("BOS", "MIA", "BOS"))
})

test_that("contains does a case-insensitive substring match", {
  out <- apply_filters(df, list(list(col = "team", op = "contains", value = "la")))
  expect_setequal(out$team, c("LAL", "LAL"))
})

test_that("multiple conditions combine with AND", {
  out <- apply_filters(df, list(
    list(col = "team", op = "in",  value = "LAL"),
    list(col = "pts",  op = ">",   value = 26)))
  expect_equal(nrow(out), 1L)
  expect_equal(out$pts, 30)
})

test_that("empty conditions and unknown columns are no-ops (not errors)", {
  expect_equal(nrow(apply_filters(df, list())), 5L)
  expect_equal(nrow(apply_filters(df, NULL)), 5L)
  expect_equal(nrow(apply_filters(df, list(list(col = "nope", op = ">", value = 1)))), 5L)
})

test_that("describe_condition reads naturally", {
  expect_equal(describe_condition(list(col = "pts", op = ">", value = 20)),
               "pts > 20")
  expect_equal(describe_condition(list(col = "team", op = "in", value = c("LAL", "BOS"))),
               "team is any of LAL, BOS")
  expect_equal(describe_condition(list(col = "pts", op = "between", value = c(10, 20))),
               "pts is between 10 and 20")
})
