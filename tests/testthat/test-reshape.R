# Tests for the single-table reshape verbs in R/helpers_reshape.R.
# Helpers are sourced by setup.R before this file runs.

# ---- do_stack (wide -> tall) -------------------------------------------------

test_that("do_stack collapses selected columns into label/value", {
  df  <- data.frame(id = 1:2, q1 = c(5, 2), q2 = c(3, 4))
  out <- do_stack(df, c("q1", "q2"))
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("id", "Label", "Data"))
  expect_equal(nrow(out), 4L)
  expect_setequal(unique(out$Label), c("q1", "q2"))
})

test_that("do_stack honours custom label/value names", {
  df  <- data.frame(id = 1:2, q1 = c(5, 2), q2 = c(3, 4))
  out <- do_stack(df, c("q1", "q2"), label_to = "Question", value_to = "Score")
  expect_named(out, c("id", "Question", "Score"))
})

test_that("do_stack errors on unknown columns", {
  expect_error(do_stack(data.frame(a = 1), c("a", "nope")), "not found")
})

# ---- do_split (tall -> wide) -------------------------------------------------

test_that("do_split spreads values into columns keyed by split_by", {
  long <- data.frame(id = c(1, 1, 2, 2), k = c("a", "b", "a", "b"),
                     v = c(5, 3, 2, 4))
  out  <- do_split(long, value_col = "v", split_by = "k", group_cols = "id")
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("id", "a", "b"))
  expect_equal(nrow(out), 2L)
  expect_equal(out$a, c(5, 2))
  expect_equal(out$b, c(3, 4))
})

test_that("do_split defaults group_cols to everything else", {
  long <- data.frame(id = c(1, 1, 2, 2), k = c("a", "b", "a", "b"),
                     v = c(5, 3, 2, 4))
  out  <- do_split(long, value_col = "v", split_by = "k")
  expect_named(out, c("id", "a", "b"))
})

test_that("do_split resolves duplicate combinations with values_fn", {
  dup <- data.frame(id = c(1, 1), k = c("a", "a"), v = c(2, 4))
  out <- do_split(dup, value_col = "v", split_by = "k", group_cols = "id",
                  values_fn = mean)
  expect_equal(out$a, 3)
})

test_that("do_split errors on unknown columns", {
  long <- data.frame(id = 1, k = "a", v = 1)
  expect_error(do_split(long, value_col = "v", split_by = "nope"), "not found")
})

# ---- stack <-> split are inverses -------------------------------------------

test_that("stack then split round-trips back to the original", {
  wide <- data.frame(id = 1:2, q1 = c(5, 2), q2 = c(3, 4))
  long <- do_stack(wide, c("q1", "q2"))
  back <- do_split(long, value_col = "Data", split_by = "Label",
                   group_cols = "id")
  expect_equal(as.data.frame(back), wide)
})

# ---- do_sort -----------------------------------------------------------------

test_that("do_sort orders by a single key ascending", {
  out <- do_sort(data.frame(x = c(3, 1, 2), y = c("c", "a", "b")), "x")
  expect_equal(out$x, c(1, 2, 3))
})

test_that("do_sort supports per-column descending", {
  out <- do_sort(data.frame(g = c("a", "a", "b"), v = c(1, 2, 1)),
                 c("g", "v"), desc = c(FALSE, TRUE))
  expect_equal(out$g, c("a", "a", "b"))
  expect_equal(out$v, c(2, 1, 1))
})

test_that("do_sort errors on unknown columns", {
  expect_error(do_sort(data.frame(a = 1), "nope"), "not found")
})

# ---- do_subset ---------------------------------------------------------------

test_that("do_subset keeps the chosen columns", {
  out <- do_subset(data.frame(a = 1:3, b = 4:6, c = 7:9), cols = c("a", "c"))
  expect_named(out, c("a", "c"))
  expect_equal(nrow(out), 3L)
})

test_that("do_subset draws a reproducible random n with a seed", {
  df   <- data.frame(id = 1:100)
  out1 <- do_subset(df, sample = "n", size = 10, seed = 1)
  out2 <- do_subset(df, sample = "n", size = 10, seed = 1)
  expect_equal(nrow(out1), 10L)
  expect_equal(out1, out2)
})

test_that("do_subset samples a proportion within each stratum", {
  df  <- data.frame(g = rep(c("a", "b"), each = 50), x = 1:100)
  out <- do_subset(df, sample = "prop", size = 0.1, stratify_by = "g", seed = 42)
  expect_equal(nrow(out), 10L)             # 5 per group
  expect_setequal(unique(out$g), c("a", "b"))
})

test_that("do_subset errors on unknown columns", {
  expect_error(do_subset(data.frame(a = 1), cols = "nope"), "not found")
})

# ---- do_transpose ------------------------------------------------------------

test_that("do_transpose swaps rows and columns using a header column", {
  df  <- data.frame(metric = c("height", "weight"), a = c(10, 100),
                    b = c(20, 200))
  out <- do_transpose(df, names_from = "metric")
  expect_true(all(c("name", "height", "weight") %in% names(out)))
  expect_equal(nrow(out), 2L)              # one row per transposed column (a, b)
  expect_equal(out$height[out$name == "a"], 10)
})

test_that("do_transpose without a header column generates V1.. headers", {
  out <- do_transpose(data.frame(x = c(1, 2), y = c(3, 4)))
  expect_true("name" %in% names(out))
  expect_true(all(c("V1", "V2") %in% names(out)))
  expect_equal(nrow(out), 2L)
})

test_that("do_transpose errors on unknown header column", {
  expect_error(do_transpose(data.frame(a = 1), names_from = "nope"),
               "not found")
})

test_that("do_subset's seed is reproducible without touching the session RNG", {
  df <- data.frame(id = 1:100, g = rep(letters[1:4], 25))
  set.seed(999)
  before <- .Random.seed
  s1 <- do_subset(df, sample = "n", size = 10, seed = 7)
  expect_identical(.Random.seed, before)              # stream untouched
  s2 <- do_subset(df, sample = "n", size = 10, seed = 7)
  expect_identical(s1, s2)                            # still reproducible
})
