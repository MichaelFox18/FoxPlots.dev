# Tests for the ggplot2-free plotting logic in helpers_plot.R: chart-suitability
# hints, the code generator, and the small helpers. Building actual ggplot
# objects is exercised in the PowerShell smoke test (needs ggplot2 attached).

# ---- chart_hint --------------------------------------------------------------

test_that("chart_hint flags a categorical X on a scatter", {
  df  <- data.frame(g = letters[1:5], y = 1:5)
  msg <- chart_hint(df, list(type = "scatter", x = "g"))
  expect_true(grepl("categorical", msg))
})

test_that("chart_hint is silent for a numeric scatter X", {
  df <- data.frame(x = 1:20, y = 1:20)
  expect_null(chart_hint(df, list(type = "scatter", x = "x")))
})

test_that("chart_hint needs >= 2 numeric columns for a heatmap", {
  df  <- data.frame(x = 1:5, g = letters[1:5])
  msg <- chart_hint(df, list(type = "heatmap"))
  expect_true(grepl("2 numeric", msg))
})

# ---- generate_code -----------------------------------------------------------

test_that("generate_code emits parseable ggplot2 for a scatter", {
  df   <- data.frame(x = 1:10, y = (1:10) * 2)
  code <- generate_code(df, list(type = "scatter", x = "x", y = "y"))
  expect_true(grepl("ggplot", code))
  expect_true(grepl("geom_point", code))
  expect_silent(parse(text = code))          # it's runnable R
})

test_that("generate_code prompts for a chart type up front", {
  expect_match(generate_code(data.frame(x = 1), list()), "Choose a chart type")
})

test_that("generate_code backticks non-syntactic column names", {
  df   <- data.frame(`my x` = 1:3, y = 1:3, check.names = FALSE)
  code <- generate_code(df, list(type = "scatter", x = "my x", y = "y"))
  expect_true(grepl("`my x`", code, fixed = TRUE))
})

test_that("generate_code emits a sqrt scale", {
  df   <- data.frame(x = 1:10, y = 1:10)
  code <- generate_code(df, list(type = "scatter", x = "x", y = "y",
                                 logscale = "sqrty"))
  expect_true(grepl("scale_y_sqrt()", code, fixed = TRUE))
  expect_false(grepl("scale_x", code))
  expect_silent(parse(text = code))
})

test_that("generate_code fits the regression overlay per group", {
  df   <- data.frame(x = 1:10, y = 1:10, g = rep(c("a", "b"), 5))
  code <- generate_code(df, list(type = "scatter", x = "x", y = "y", color = "g",
                                 reg_overlay = TRUE, reg_type = "lm"))
  expect_true(grepl("geom_smooth(aes(color = g)", code, fixed = TRUE))
  expect_silent(parse(text = code))
})

test_that("generate_code aggregates the line chart by X", {
  df   <- data.frame(g = rep(c("a", "b"), each = 3), y = 1:6)
  code <- generate_code(df, list(type = "line", x = "g", y = "y",
                                 line_agg = "mean"))
  expect_true(grepl("dplyr::summarise", code))
  expect_true(grepl("geom_line", code))
  expect_silent(parse(text = code))
})

test_that("generate_code coerces a numeric boxplot X to a factor", {
  code <- generate_code(mtcars, list(type = "boxplot", x = "cyl", y = "qsec"))
  expect_true(grepl('as.factor(df[["cyl"]])', code, fixed = TRUE))
  expect_silent(parse(text = code))
})

test_that("generate_code adds the bar connecting-line overlay", {
  df   <- data.frame(g = c("a", "b", "c"), y = c(1, 2, 3))
  code <- generate_code(df, list(type = "bar", x = "g", y = "y",
                                 bar_agg = "sum", bar_line = TRUE))
  expect_true(grepl("geom_col", code))
  expect_true(grepl("geom_line", code))
  expect_silent(parse(text = code))
})

# ---- small helpers -----------------------------------------------------------

test_that("needs_x_rotation triggers for many/long discrete labels", {
  df <- data.frame(g = paste0("category_", 1:12), y = 1:12)
  expect_true(needs_x_rotation(df, "bar", "g"))
  expect_false(needs_x_rotation(df, "scatter", "y"))
})

test_that("lump_bar_x keeps the top categories and lumps the rest", {
  df  <- data.frame(g = c("a", "a", "a", "b", "b", "c", "d"))
  out <- lump_bar_x(df, "g", NULL, 2)          # keep a(3), b(2); c,d -> Other
  expect_true("Other" %in% levels(out$g))
  expect_setequal(as.character(unique(out$g)), c("a", "b", "Other"))
})

test_that("bq backticks only non-syntactic names; qq quotes", {
  expect_equal(bq("mpg"), "mpg")
  expect_equal(bq("my var"), "`my var`")
  expect_equal(qq("hi"), '"hi"')
})

# ---- plotly post-processing helpers -----------------------------------------

test_that("plotly_legend_layout repositions only top/bottom", {
  expect_null(plotly_legend_layout("right"))   # default needs no override
  expect_null(plotly_legend_layout("none"))    # hidden handled by ggplotly
  expect_null(plotly_legend_layout(NULL))
  expect_equal(plotly_legend_layout("bottom")$orientation, "h")
  expect_equal(plotly_legend_layout("top")$orientation, "h")
})

test_that("clean_plotly_trace_names strips the leaked panel index", {
  ply <- list(x = list(data = list(
    list(name = "(4,1)"), list(name = "(6,1)"),
    list(name = "8"),     list(name = NULL))))
  out <- clean_plotly_trace_names(ply)
  expect_equal(out$x$data[[1]]$name, "4")
  expect_equal(out$x$data[[2]]$name, "6")
  expect_equal(out$x$data[[3]]$name, "8")      # already clean -> untouched
  expect_null(out$x$data[[4]]$name)            # NULL name -> left alone
})

test_that("clean_plotly_trace_names tolerates a plot with no traces", {
  expect_equal(clean_plotly_trace_names(list(x = list())), list(x = list()))
})
