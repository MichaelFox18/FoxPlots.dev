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

test_that("chart_hint prompts to pick variables for a heatmap (no show-all)", {
  df <- data.frame(a = 1:5, b = 2:6, c = 3:7)   # 3 numeric columns available
  # nothing selected -> prompt to pick, even though the data has enough columns
  expect_true(grepl("Pick at least", chart_hint(df, list(type = "heatmap"))))
  # only one selected -> still prompt
  expect_true(grepl("Pick at least",
                    chart_hint(df, list(type = "heatmap", corr_vars = "a"))))
  # two selected -> good to go
  expect_null(chart_hint(df, list(type = "heatmap", corr_vars = c("a", "b"))))
})

test_that("generate_corr_code honours the selection and never falls back to all", {
  df <- data.frame(a = 1:5, b = 2:6, c = 3:7, d = 4:8)
  # no selection -> a friendly comment, not code over every column
  expect_true(grepl("Pick at least two",
                    generate_corr_code(df, list(type = "heatmap"))))
  # two chosen -> code subsets to exactly those and parses
  code <- generate_corr_code(df, list(type = "heatmap", corr_vars = c("a", "c")))
  expect_true(grepl('num[, c("a", "c")]', code, fixed = TRUE))
  expect_false(grepl('"b"', code, fixed = TRUE))   # unchosen column absent
  expect_error(parse(text = code), NA)             # syntactically valid
})

# ---- new chart types: violin / density / mean±error / hexbin -----------------

test_that("generate_code emits parseable code for the new chart types", {
  df <- data.frame(g = rep(letters[1:3], 4), x = 1:12, y = (1:12) * 1.5)
  specs <- list(
    violin     = list(type = "violin",    x = "g", y = "y"),
    density    = list(type = "density",   x = "x"),
    density_g  = list(type = "density",   x = "x", color = "g"),
    meanerr_se = list(type = "meanerror", x = "g", y = "y", err_type = "se"),
    meanerr_sd = list(type = "meanerror", x = "g", y = "y", err_type = "sd"),
    hexbin     = list(type = "hexbin",    x = "x", y = "y", bins = 20))
  for (nm in names(specs)) {
    code <- generate_code(df, specs[[nm]])
    expect_true(grepl("ggplot", code), info = nm)
    expect_error(parse(text = code), NA)          # runnable R
  }
})

test_that("generate_code picks the right geom / summary per new type", {
  df <- data.frame(g = rep(letters[1:3], 4), x = 1:12, y = (1:12) * 1.5)
  expect_true(grepl("geom_violin",  generate_code(df, list(type = "violin",  x = "g", y = "y"))))
  expect_true(grepl("geom_density", generate_code(df, list(type = "density", x = "x"))))
  expect_true(grepl("geom_hex",     generate_code(df, list(type = "hexbin",  x = "x", y = "y"))))
  se <- generate_code(df, list(type = "meanerror", x = "g", y = "y", err_type = "se"))
  expect_true(grepl("mean_se", se, fixed = TRUE))
  sd <- generate_code(df, list(type = "meanerror", x = "g", y = "y", err_type = "sd"))
  expect_false(grepl("mean_sdl", sd, fixed = TRUE))   # base-R SD, no Hmisc
  expect_true(grepl("ymin = m - s", sd, fixed = TRUE))
})

test_that("chart_hint steers numeric/categorical X for the new types", {
  df <- data.frame(num = 1:12, cat = rep(letters[1:2], 6))   # num: 12 distinct (>10)
  # density / hexbin want a numeric X
  expect_true(grepl("numeric", chart_hint(df, list(type = "density", x = "cat"))))
  expect_true(grepl("numeric", chart_hint(df, list(type = "hexbin",  x = "cat", y = "num"))))
  expect_null(chart_hint(df, list(type = "density", x = "num")))
  # violin / mean±error want a categorical X (a continuous one triggers a hint)
  expect_true(grepl("continuous", chart_hint(df, list(type = "violin",    x = "num", y = "num"))))
  expect_true(grepl("continuous", chart_hint(df, list(type = "meanerror", x = "num", y = "num"))))
})

# ---- generate_code -----------------------------------------------------------

test_that("generate_code emits parseable ggplot2 for a scatter", {
  df   <- data.frame(x = 1:10, y = (1:10) * 2)
  code <- generate_code(df, list(type = "scatter", x = "x", y = "y"))
  expect_true(grepl("ggplot", code))
  expect_true(grepl("geom_point", code))
  expect_silent(parse(text = code))          # it's runnable R
})

test_that("scatter 'size by' makes a bubble chart (size aesthetic + scale)", {
  df <- data.frame(x = 1:10, y = (1:10) * 2, z = c(1, 5, 2, 8, 3, 7, 4, 9, 6, 10))
  code <- generate_code(df, list(type = "scatter", x = "x", y = "y", size_by = "z"))
  expect_true(grepl("size = z", code, fixed = TRUE))            # mapped, not fixed
  expect_true(grepl("scale_size_continuous", code, fixed = TRUE))
  expect_false(grepl("geom_point(size =", code, fixed = TRUE))  # size not also fixed
  expect_error(parse(text = code), NA)
  # a non-numeric size_by is ignored (still a plain scatter)
  df2 <- data.frame(x = 1:4, y = 1:4, g = letters[1:4])
  plain <- generate_code(df2, list(type = "scatter", x = "x", y = "y", size_by = "g"))
  expect_false(grepl("scale_size_continuous", plain, fixed = TRUE))
  # size_by only applies to scatter, not e.g. a line
  ln <- generate_code(df, list(type = "line", x = "x", y = "y", size_by = "z"))
  expect_false(grepl("scale_size_continuous", ln, fixed = TRUE))
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
