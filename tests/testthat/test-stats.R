# Tests for the grouped-summary + column-classifier helpers in helpers_stats.R.
# Helpers are sourced by setup.R before this file runs.

test_that("grouped_summary returns one row per group with the right stats", {
  df  <- data.frame(g = c("a", "a", "b"), x = c(1, 3, 10))
  out <- grouped_summary(df, vars = "x", groups = "g")
  expect_equal(out$g, c("a", "b"))
  expect_equal(out$Variable, c("x", "x"))
  expect_equal(out$N, c(2L, 1L))
  expect_equal(out$Mean, c(2, 10))
  expect_true(all(c("g", "Variable", "N", "Mean", "Median", "Mode", "Min",
                    "Max", "SD", "SE", "IQR") %in% names(out)))
  expect_equal(out$SE[out$g == "a"], 1)        # sd(1,3)/sqrt(2) = 1
  expect_true(is.na(out$SE[out$g == "b"]))     # SE undefined for n = 1
})

test_that("grouped_summary returns NULL when inputs are unusable", {
  expect_null(grouped_summary(data.frame(x = 1), vars = character(0),
                              groups = "x"))
  expect_null(grouped_summary(NULL, "x", "g"))
})

test_that("numeric_cols and groupable_cols classify columns", {
  df <- data.frame(id = 1:40, grp = rep(letters[1:4], 10), val = (1:40) / 7)
  expect_setequal(numeric_cols(df), c("id", "val"))
  expect_true("grp" %in% groupable_cols(df))    # 4 distinct -> groupable
  expect_false("id" %in% groupable_cols(df))     # 40 distinct -> too many
})

test_that("the mode reducer is NA when nothing repeats", {
  expect_true(is.na(.s_mode(c(1, 2, 3))))
  expect_equal(.s_mode(c(1, 2, 2, 3)), 2)
})

test_that("friendly_type labels columns in plain English", {
  expect_equal(friendly_type(1L), "integer")
  expect_equal(friendly_type(1.5), "numeric")
  expect_equal(friendly_type(factor("a")), "factor")
  expect_equal(friendly_type("a"), "text")
  expect_equal(friendly_type(as.Date("2026-01-01")), "date")
})

test_that("column_profile reports type, missingness, and distinct counts", {
  df  <- data.frame(x = c(1, 2, NA), g = c("a", "a", "b"),
                    stringsAsFactors = FALSE)
  prof <- column_profile(df)
  expect_setequal(prof$Column, c("x", "g"))
  expect_equal(prof$Distinct[prof$Column == "g"], 2L)
  expect_match(prof$Missing[prof$Column == "x"], "1 \\(33%\\)")
})

test_that("data_glance counts rows, columns, and complete cases", {
  g <- data_glance(data.frame(x = c(1, NA), y = c("a", "b")))
  expect_equal(g$n, 2L)
  expect_equal(g$m, 2L)
  expect_equal(g$complete, 1L)        # one row has an NA
})

test_that("proportions_summary gives percents and exact CIs by group", {
  skip_if_not_installed("binom")
  df <- data.frame(
    g       = rep(c("A", "B"), each = 10),
    outcome = c(rep("yes", 6), rep("no", 4),     # A: 6/10 yes
                rep("yes", 5), rep("no", 5))      # B: 5/10 yes
  )
  out <- proportions_summary(df, "outcome", "g")
  expect_true(all(c("g", "Level", "N", "Total", "Percent", "CI_low",
                    "CI_high") %in% names(out)))
  ay <- out[out$g == "A" & out$Level == "yes", ]
  expect_equal(ay$N, 6L)
  expect_equal(ay$Total, 10L)
  expect_equal(ay$Percent, 60)
  expect_true(ay$CI_low < 60 && ay$CI_high > 60)   # exact CI brackets 60%
})

test_that("proportions_summary returns NULL on unusable input", {
  expect_null(proportions_summary(data.frame(x = 1), "x", character(0)))
})

# ---- reserved-name collisions ------------------------------------------------
# summarise() used to overwrite a GROUPING column that happened to be named
# "N"/"Mean"/... with the statistic of the same name, so rows grouped correctly
# but became unidentifiable (and the name-based select then duplicated a stat).

test_that("grouped_summary keeps group labels when a group column is named N", {
  df  <- data.frame(N = rep(c("a", "b"), each = 5), score = c(1:5, 11:15))
  out <- grouped_summary(df, vars = "score", groups = "N")
  expect_equal(as.character(out$N), c("a", "b"))   # group labels, not counts
  expect_equal(out$Mean, c(3, 13))
  expect_false(anyDuplicated(names(out)) > 0)      # our stat yielded, not theirs
})

test_that("grouped_summary survives group columns named after every stat", {
  for (nm in c("Mean", "Median", "Min", "Max", "SD", "SE", "IQR", "Variable")) {
    df <- data.frame(g = rep(c("a", "b"), each = 4), score = c(1:4, 11:14))
    names(df)[1] <- nm
    out <- grouped_summary(df, vars = "score", groups = nm)
    expect_equal(as.character(out[[nm]]), c("a", "b"), info = nm)
  }
})

test_that("proportions_summary handles an outcome column named Total", {
  df  <- data.frame(grp = rep(c("x", "y"), each = 4),
                    Total = rep(c("yes", "no"), 4))
  out <- proportions_summary(df, outcome = "Total", groups = "grp")
  expect_s3_class(out, "data.frame")
  expect_equal(unique(out$Percent), 50)
})

test_that("proportions_summary keeps group labels when a group is named N", {
  df  <- data.frame(N = rep(c("x", "y"), each = 4), val = rep(c("yes", "no"), 4))
  out <- proportions_summary(df, outcome = "val", groups = "N")
  expect_equal(unique(as.character(out$N)), c("x", "y"))
})

test_that("normal grouped_summary output is unchanged by the rename", {
  out <- grouped_summary(mtcars, vars = "mpg", groups = "cyl")
  expect_named(out, c("cyl", "Variable", "N", "Mean", "Median", "Mode",
                      "Min", "Max", "SD", "SE", "IQR"))
})
