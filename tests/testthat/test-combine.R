# Tests for the two-table helpers in helpers_combine.R.

test_that("do_concatenate stacks rows and can tag the source", {
  a <- data.frame(id = 1:2, x = c("a", "b"))
  b <- data.frame(id = 3:4, x = c("c", "d"))
  expect_equal(nrow(do_concatenate(a, b)), 4L)
  out <- do_concatenate(a, b, add_source = TRUE, source_col = "src")
  expect_true("src" %in% names(out))
  expect_setequal(unique(out$src), c("left", "right"))
})

test_that("do_concatenate fills non-shared columns with NA", {
  a <- data.frame(id = 1, band = "x")
  b <- data.frame(id = 2, plays = "y")
  out <- do_concatenate(a, b)
  expect_true(all(c("id", "band", "plays") %in% names(out)))
  expect_true(is.na(out$plays[out$id == 1]))
})

test_that("do_join supports the join types", {
  l <- data.frame(id = 1:3, x = letters[1:3])
  r <- data.frame(id = 2:4, y = LETTERS[2:4])
  expect_equal(nrow(do_join(l, r, "id", "inner")), 2L)   # ids 2,3
  expect_equal(nrow(do_join(l, r, "id", "left")),  3L)
  expect_equal(nrow(do_join(l, r, "id", "full")),  4L)   # ids 1-4
  expect_equal(nrow(do_join(l, r, type = "cross")), 9L)  # 3 x 3
})

test_that("do_join errors on a missing key (non-cross)", {
  expect_error(do_join(data.frame(a = 1), data.frame(b = 1), by = "id"),
               "not found")
  expect_error(do_join(data.frame(a = 1), data.frame(a = 1), by = NULL,
                       type = "left"), "key")
})

test_that("do_update overwrites or fills by key", {
  main <- data.frame(id = 1:3, v = c(10, NA, 30))
  upd  <- data.frame(id = c(2, 3), v = c(99, 88))
  ov <- do_update(main, upd, by = "id", mode = "overwrite")
  expect_equal(ov$v, c(10, 99, 88))            # both matching rows replaced
  fl <- do_update(main, upd, by = "id", mode = "fill")
  expect_equal(fl$v, c(10, 99, 30))            # only the NA (row 2) filled
})

test_that("compare_tables reports membership and cell diffs", {
  l <- data.frame(id = 1:2, x = c(1, 2), only_l = c("a", "b"))
  r <- data.frame(id = 1:2, x = c(1, 9), only_r = c("p", "q"))
  cmp <- compare_tables(l, r)
  expect_s3_class(cmp, "data.frame")
  expect_equal(cmp$Differing[cmp$Column == "x"], 1L)        # one cell differs
  expect_equal(cmp$In_right[cmp$Column == "only_l"], "no")
})

test_that("near_match_columns pairs names differing only by case/whitespace", {
  out <- near_match_columns(c("Country", "pop"), c("country ", "gdp"))
  expect_equal(out$left,  "Country")
  expect_equal(out$right, "country ")
})

test_that("near_match_columns ignores exact shared names and true misses", {
  expect_equal(nrow(near_match_columns(c("id", "x"), c("id", "y"))), 0L)
  expect_equal(nrow(near_match_columns(character(0), c("a"))), 0L)
})
