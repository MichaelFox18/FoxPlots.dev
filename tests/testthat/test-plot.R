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

# ---- faceting an aggregating chart (line / bar-with-Y) -----------------------
# Regression: the line and bar-with-Y branches aggregate df before plotting, so
# the facet column must survive that aggregation or facet_wrap() errors at
# render time (the plot built fine but crashed at print/ggplotly).

test_that("build_full_plot keeps the facet column through line/bar aggregation", {
  df <- data.frame(g = rep(letters[1:3], each = 8),
                   f = rep(c("u", "v"), 12),
                   x = rep(1:4, 6),
                   y = (1:24) * 1.5)
  specs <- list(
    line       = list(type = "line", x = "x", y = "y", facet = "f"),
    line_col   = list(type = "line", x = "x", y = "y", color = "g", facet = "f"),
    bar_y      = list(type = "bar",  x = "g", y = "y", bar_agg = "mean", facet = "f"))
  for (nm in names(specs)) {
    p_obj <- build_full_plot(df, specs[[nm]])
    expect_true(inherits(p_obj, "ggplot"), info = nm)
    expect_error(ggplot2::ggplot_build(p_obj), NA)   # errored before the fix
  }
})

test_that("generate_code groups the aggregated data by the facet column too", {
  df <- data.frame(f = rep(c("u", "v"), 6), x = rep(1:3, 4), y = 1:12)
  for (ty in c("line", "bar")) {
    code <- generate_code(df, list(type = ty, x = "x", y = "y", facet = "f"))
    expect_true(grepl("group_by(df, x, f)", code, fixed = TRUE), info = ty)
    expect_true(grepl("facet_wrap(vars(f))", code, fixed = TRUE), info = ty)
    expect_error(parse(text = code), NA)
  }
})

test_that("build_full_plot still drops a facet with more than 30 levels", {
  df <- data.frame(x = rep(1:2, 31), y = 1:62, f = factor(rep(1:31, each = 2)))
  p_obj <- build_full_plot(df, list(type = "line", x = "x", y = "y", facet = "f"))
  expect_true(inherits(p_obj, "ggplot"))
  expect_error(ggplot2::ggplot_build(p_obj), NA)
  # aggregation must NOT have grouped by the dropped facet: one point per x
  expect_equal(nrow(p_obj$data), 2L)
})

test_that("generate_code applies the same >30-level facet cap as the builder", {
  df <- data.frame(x = rep(1:2, 31), y = 1:62, f = factor(rep(1:31, each = 2)))
  code <- generate_code(df, list(type = "line", x = "x", y = "y", facet = "f"))
  expect_false(grepl("facet_wrap", code, fixed = TRUE))   # facet dropped on screen
  expect_true(grepl("group_by(df, x)", code, fixed = TRUE))
  expect_error(parse(text = code), NA)
})

# ---- render_plots_to_file: real round-trip per format ------------------------
# Regression guard: PDF export used cairo_pdf(), which on a machine without
# cairo (macOS sans XQuartz) warns, opens NO device, writes NOTHING, and raises
# no condition -- so the download handler silently served an empty file. These
# assert real bytes reach disk, which no previous test did.

test_that("render_plots_to_file writes a real, non-empty PNG and PDF", {
  p <- list(ggplot(mtcars, aes(wt, mpg)) + geom_point())
  for (fmt in c("png", "pdf")) {
    f <- withr::local_tempfile(fileext = paste0(".", fmt))
    expect_silent(render_plots_to_file(p, f, fmt, 7, 4.5, 110))
    expect_true(file.exists(f))
    expect_gt(file.size(f), 1000)
  }
})

test_that("render_plots_to_file leaves no device open and no stray Rplots.pdf", {
  p   <- list(ggplot(mtcars, aes(wt, mpg)) + geom_point())
  before <- grDevices::dev.cur()
  f <- withr::local_tempfile(fileext = ".pdf")
  render_plots_to_file(p, f, "pdf", 7, 4.5, 110)
  expect_identical(grDevices::dev.cur(), before)
  expect_false(file.exists(file.path(getwd(), "Rplots.pdf")))
})

test_that("render_plots_to_file reports an unavailable device instead of failing silently", {
  # SVG needs cairo; where it is missing we must raise, never write nothing quietly.
  p <- list(ggplot(mtcars, aes(wt, mpg)) + geom_point())
  f <- withr::local_tempfile(fileext = ".svg")
  res <- tryCatch({ render_plots_to_file(p, f, "svg", 7, 4.5, 110); "ok" },
                  error = function(e) conditionMessage(e))
  if (identical(res, "ok")) {
    expect_gt(file.size(f), 100)          # cairo present: real file
  } else {
    expect_match(res, "cairo|device")     # cairo absent: a loud, useful error
    expect_equal(unname(grDevices::dev.cur()), 1L)   # and no device left open
  }
})

# ---- GROUP_MAX: the high-cardinality colour cap -------------------------------
# Regression guard for the 0.10.0 freeze: colouring by an 11,000-level ID column
# took 111.7s to render (linear in level count), locking the single-threaded app.

# Grab whatever build_full_plot mapped to colour, or NULL if it dropped it.
# Base R only -- rlang is not a declared dependency, and an undeclared ::  in
# the tests is an R CMD check --as-cran warning.
colour_var <- function(p_obj) {
  v <- p_obj$mapping$colour %||% p_obj$mapping$fill
  if (is.null(v)) return(NULL)
  sub('^.*\\[\\["(.*)"\\]\\].*$', "\\1", paste(deparse(v), collapse = ""))
}

test_that("build_full_plot keeps a discrete colour at the cap and drops it past", {
  n  <- 400
  df <- data.frame(x = rnorm(n), y = rnorm(n))
  df$at  <- sprintf("g%03d", rep_len(seq_len(GROUP_MAX),     n))   # exactly 50
  df$ovr <- sprintf("g%03d", rep_len(seq_len(GROUP_MAX + 1L), n))  # 51
  expect_equal(dplyr::n_distinct(df$at),  GROUP_MAX)
  expect_equal(dplyr::n_distinct(df$ovr), GROUP_MAX + 1L)

  for (ty in c("density", "boxplot", "violin", "histogram", "meanerror", "scatter", "line")) {
    p <- list(type = ty, x = if (ty %in% c("density", "histogram")) "x" else "y",
              y = "y", color = "at")
    if (ty %in% c("scatter", "line")) p$x <- "x"
    expect_false(is.null(colour_var(build_full_plot(df, p))),
                 info = paste(ty, "should keep a 50-level colour"))
    p$color <- "ovr"
    expect_null(colour_var(build_full_plot(df, p)),
                info = paste(ty, "should drop a 51-level colour"))
  }
})

test_that("the colour cap does not touch a continuous numeric colour", {
  # A numeric colour uses ONE gradient scale, so it is fast at any cardinality
  # and must survive -- this is what makes the cap safe to set as low as 50.
  n  <- 2000
  df <- data.frame(x = rnorm(n), y = rnorm(n), depth = runif(n, 0, 700))
  expect_gt(dplyr::n_distinct(df$depth), GROUP_MAX)
  p <- build_full_plot(df, list(type = "scatter", x = "x", y = "y", color = "depth"))
  expect_identical(colour_var(p), "depth")
})

test_that("a factor and a Date colour are capped like any other discrete column", {
  n  <- 300
  df <- data.frame(x = rnorm(n), y = rnorm(n))
  df$f <- factor(sprintf("l%03d", seq_len(n)))                  # 300 levels
  df$d <- as.Date("2020-01-01") + seq_len(n)                    # 300 dates
  expect_null(colour_var(build_full_plot(df, list(type = "scatter", x = "x", y = "y", color = "f"))))
  expect_null(colour_var(build_full_plot(df, list(type = "scatter", x = "x", y = "y", color = "d"))))
})

test_that("generate_code mirrors the colour cap so the snippet matches the chart", {
  n  <- 300
  df <- data.frame(x = rnorm(n), y = rnorm(n),
                   few  = rep_len(letters[1:5], n),
                   many = sprintf("id%03d", seq_len(n)))
  keep <- generate_code(df, list(type = "density", x = "x", color = "few"))
  drop <- generate_code(df, list(type = "density", x = "x", color = "many"))
  expect_match(keep, "few")
  expect_false(grepl("many", drop, fixed = TRUE))
  # and it must still be runnable code, not a broken aes()
  expect_silent(parse(text = drop))
})

test_that("chart_hint explains the dropped colour and names the limit", {
  n  <- 300
  df <- data.frame(x = rnorm(n), y = rnorm(n),
                   id  = sprintf("EQ%03d", seq_len(n)),
                   few = rep_len(letters[1:4], n))
  msg <- chart_hint(df, list(type = "density", x = "x", color = "id"))
  expect_true(grepl("id", msg))
  expect_true(grepl("300", msg))                 # the actual count
  expect_true(grepl(as.character(GROUP_MAX), msg))
  expect_true(grepl("facet", msg))               # the way out
  # silent for an ordinary grouping and for a continuous numeric colour
  expect_null(chart_hint(df, list(type = "density", x = "x", color = "few")))
  expect_null(chart_hint(df, list(type = "scatter", x = "x", y = "y", color = "y")))
})

test_that("the hint and the cap agree on how NA counts (release-review finding)", {
  # ggplot draws NA as its own level with its own legend key, so it costs one
  # of the GROUP_MAX slots. build_full_plot and generate_code always counted it;
  # chart_hint used na.rm = TRUE, so a column of exactly 50 real levels plus any
  # NA was dropped with NO on-screen explanation -- the exact failure the hint
  # exists to prevent.
  lv <- sprintf("L%02d", seq_len(GROUP_MAX))         # 50 real levels
  g  <- c(lv, lv, NA, NA)                            # + NA  -> 51 counted
  df <- data.frame(x = seq_along(g), y = seq_along(g), g = g,
                   stringsAsFactors = FALSE)
  p  <- list(type = "scatter", x = "x", y = "y", color = "g")
  expect_equal(dplyr::n_distinct(df$g), GROUP_MAX + 1L)
  expect_null(colour_var(build_full_plot(df, p)))      # builder drops it ...
  expect_false(is.null(chart_hint(df, p)))             # ... and now says so
  expect_true(grepl(as.character(GROUP_MAX + 1L), chart_hint(df, p)))
  # and 50 real levels with NO missing values is still under the cap
  df2 <- data.frame(x = 1:100, y = 1:100, g = rep_len(lv, 100),
                    stringsAsFactors = FALSE)
  expect_false(is.null(colour_var(build_full_plot(df2, p))))
  expect_null(chart_hint(df2, p))
})
