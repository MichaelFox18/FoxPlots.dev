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
  # A small categorical X for the box family: since X_LEVELS_MAX (0.12.0) a
  # 400-distinct numeric X blocks those charts outright, and this test is
  # about the COLOUR cap, not the x cap.
  df$g <- rep_len(c("a", "b", "c", "d"), n)
  expect_equal(dplyr::n_distinct(df$at),  GROUP_MAX)
  expect_equal(dplyr::n_distinct(df$ovr), GROUP_MAX + 1L)

  for (ty in c("density", "boxplot", "violin", "histogram", "meanerror", "scatter", "line")) {
    p <- list(type = ty,
              x = if (ty %in% c("density", "histogram")) "x"
                  else if (ty %in% c("boxplot", "violin", "meanerror")) "g"
                  else "y",
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
  expect_true(grepl("Filter rows", msg))         # the way out (must be followable)
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

# ---- crowded x axes: automatic label thinning + the manual override ----------
# The report: a line chart over a date column that arrived as TEXT drew one
# tick label per distinct value (300+) and smeared into an unreadable band.
# A real Date or numeric x already gets ~5 pretty default breaks (measured),
# so thinning is a DISCRETE-axis feature; dates/numerics get manual control.

# The x scale build_full_plot ended up with, or NULL if it added none.
x_scale_of <- function(p_obj) {
  s <- Filter(function(z) "x" %in% z$aesthetics, p_obj$scales$scales)
  if (!length(s)) NULL else s[[1]]
}
text_dates <- function() sprintf("2024-%02d-%02d", rep(1:12, each = 25),
                                 rep(1:25, 12))

test_that("auto leaves numeric and real-date axes to ggplot's own breaks", {
  expect_null(x_break_spec(seq(0, 2e5, length.out = 2000)))
  expect_null(x_break_spec(as.Date("2020-01-01") + 0:500))
  expect_null(x_break_spec(as.POSIXct("2020-01-01", tz = "UTC") + 0:2000 * 3600))
  expect_null(x_break_spec(letters[1:8]))          # discrete, under the cap
})

test_that("x_break_spec thins a crowded discrete axis and keeps both ends", {
  lv <- text_dates()
  s  <- x_break_spec(lv)
  expect_identical(s$kind, "discrete")
  expect_lte(length(s$breaks), AXIS_LABEL_MAX)
  expect_true(all(s$breaks %in% lv))
  expect_identical(s$breaks[1], min(lv))
  expect_identical(s$breaks[length(s$breaks)], max(lv))
})

test_that("a flipped chart keeps far more labels (that is why users flip)", {
  lv <- sprintf("cat%02d", 1:25)
  expect_null(x_break_spec(lv, flip = TRUE))              # 25 <= the flip cap
  expect_false(is.null(x_break_spec(lv, flip = FALSE)))   # thinned upright
})

test_that("the manual override wins on every axis kind", {
  many <- sprintf("c%03d", 1:300)
  expect_null(x_break_spec(many, want = "all"))
  expect_equal(length(x_break_spec(many, want = "20")$breaks), 20L)
  n <- x_break_spec(1:2000, want = "10")
  expect_identical(n$kind, "continuous")
  expect_equal(n$n_breaks, 10L)
  d <- x_break_spec(as.Date("2020-01-01") + 0:1000, want = "8")
  expect_identical(d$kind, "date")
  expect_identical(d$date_breaks, "6 months")
})

test_that("the date ladder scales step AND label format with the span", {
  ten <- x_break_spec(as.Date("2024-01-01") + 0:9,    want = "6")
  yr  <- x_break_spec(as.Date("2010-01-01") + 0:5000, want = "6")
  hr  <- x_break_spec(as.POSIXct("2024-01-01", tz = "UTC") + 0:200 * 3600,
                      want = "8")
  expect_identical(c(ten$date_breaks, ten$date_labels), c("2 days", "%b %d"))
  expect_match(yr$date_breaks, "year")
  expect_identical(yr$date_labels, "%Y")     # never more precise than the step
  expect_identical(hr$kind, "datetime")
})

test_that("a hand-picked date format is honoured, and long ones get angled", {
  d <- as.Date("2020-01-01") + 0:500
  expect_identical(x_break_spec(d, date_format = "%Y-%m-%d")$date_labels,
                   "%Y-%m-%d")
  expect_equal(x_break_spec(d, date_format = "%Y-%m-%d")$angle, 40L) # 10 chars
  expect_equal(x_break_spec(d, date_format = "%b %Y")$angle,     0L) # 8 chars
  # never angled when flipped -- the labels are stacked vertically there
  expect_equal(x_break_spec(d, date_format = "%Y-%m-%d", flip = TRUE)$angle, 0L)
})

test_that("build_full_plot thins a text-date line chart and 'all' undoes it", {
  d <- data.frame(day = text_dates(), y = rnorm(300), stringsAsFactors = FALSE)
  p <- build_full_plot(d, list(type = "line", x = "day", y = "y"))
  expect_error(ggplot2::ggplot_build(p), NA)
  expect_lte(length(x_scale_of(p)$breaks), AXIS_LABEL_MAX)
  p2 <- build_full_plot(d, list(type = "line", x = "day", y = "y",
                                x_labels = "all"))
  expect_null(x_scale_of(p2))
  expect_error(ggplot2::ggplot_build(p2), NA)
})

test_that("a real Date x gets no scale on auto, a date scale on request", {
  d <- data.frame(day = as.Date("2020-01-01") + 0:500, y = rnorm(501))
  p <- build_full_plot(d, list(type = "line", x = "day", y = "y"))
  expect_null(x_scale_of(p))
  expect_error(ggplot2::ggplot_build(p), NA)
  p2 <- build_full_plot(d, list(type = "line", x = "day", y = "y",
                                x_date_format = "%Y-%m-%d"))
  expect_false(is.null(x_scale_of(p2)))
  expect_error(ggplot2::ggplot_build(p2), NA)
})

test_that("the log/sqrt x transform still owns the x scale (no double scale)", {
  # A second x scale prints "Scale for x is already present" at + time.
  d <- data.frame(x = seq(1, 1000, length.out = 300), y = rnorm(300))
  pr <- list(type = "scatter", x = "x", y = "y", logscale = "logx",
             x_labels = "20")
  expect_silent(p <- build_full_plot(d, pr))
  expect_equal(length(Filter(function(z) "x" %in% z$aesthetics,
                             p$scales$scales)), 1L)
  expect_error(ggplot2::ggplot_build(p), NA)
})

test_that("bar lumping and label thinning compose, and 'Other' survives", {
  # lump_bar_x puts "Other" last; thin_levels always keeps the last level, so
  # the user can still see where the tail went.
  d <- data.frame(g = sprintf("c%03d", 1:300), v = runif(300),
                  stringsAsFactors = FALSE)
  p <- build_full_plot(d, list(type = "bar", x = "g", y = "v",
                               bar_agg = "sum"))
  expect_error(ggplot2::ggplot_build(p), NA)
  br <- x_scale_of(p)$breaks
  expect_lte(length(br), AXIS_LABEL_MAX)
  expect_true("Other" %in% br)
})

test_that("boxplot's numeric->factor coercion yields a thinned discrete axis", {
  d <- data.frame(x = rep(1:40, each = 3), y = rnorm(120))
  s <- x_scale_of(build_full_plot(d, list(type = "boxplot", x = "x",
                                          y = "y")))
  expect_false(is.null(s))
  expect_lte(length(s$breaks), AXIS_LABEL_MAX)
  expect_identical(s$breaks[1], "1")     # factor(1:40) sorts numerically
})

test_that("a minimal p list (the mod_compare contract) still builds", {
  # mod_compare passes lists with no x_labels/x_date_format at all; absent
  # fields must mean "auto".
  d <- data.frame(g = rep(sprintf("grp%02d", 1:60), each = 3),
                  y = rnorm(180), stringsAsFactors = FALSE)
  p <- build_full_plot(d, list(type = "boxplot", x = "g", y = "y",
                               color = "g", palette = "auto",
                               legend_pos = "none"))
  expect_error(ggplot2::ggplot_build(p), NA)
  expect_lte(length(x_scale_of(p)$breaks), AXIS_LABEL_MAX)
})

test_that("generate_code mirrors the thinned axis and needs no new library", {
  d <- data.frame(day = text_dates(), y = rnorm(300), stringsAsFactors = FALSE)
  code <- generate_code(d, list(type = "line", x = "day", y = "y"))
  expect_true(grepl("scale_x_discrete(breaks = c(", code, fixed = TRUE))
  expect_error(parse(text = code), NA)
  expect_false(grepl("scales::",        code, fixed = TRUE))
  expect_false(grepl("library(scales)", code, fixed = TRUE))
  # the snippet's breaks are EXACTLY the ones the chart drew
  for (b in x_break_spec(d$day)$breaks)
    expect_true(grepl(b, code, fixed = TRUE))
  expect_false(grepl("scale_x_discrete",
    generate_code(d, list(type = "line", x = "day", y = "y",
                          x_labels = "all")),
    fixed = TRUE))
})

test_that("generate_code emits a date scale only when the user asks for one", {
  d <- data.frame(day = as.Date("2020-01-01") + 0:500, y = rnorm(501))
  expect_false(grepl("scale_x_date",
    generate_code(d, list(type = "line", x = "day", y = "y")), fixed = TRUE))
  code <- generate_code(d, list(type = "line", x = "day", y = "y",
                                x_date_format = "%Y-%m-%d", x_labels = "8"))
  expect_true(grepl('date_labels = "%Y-%m-%d"', code, fixed = TRUE))
  expect_true(grepl("guide_axis(angle = 40)", code, fixed = TRUE))
  expect_error(parse(text = code), NA)
})

test_that("generate_code adds no x scale when log/sqrt already owns it", {
  d <- data.frame(x = seq(1, 1000, length.out = 300), y = rnorm(300))
  code <- generate_code(d, list(type = "scatter", x = "x", y = "y",
                                logscale = "logx", x_labels = "20"))
  expect_true(grepl("scale_x_log10()",     code, fixed = TRUE))
  expect_false(grepl("scale_x_continuous", code, fixed = TRUE))
  expect_error(parse(text = code), NA)
})

test_that("chart_hint names the real cause when dates arrive as text", {
  d <- data.frame(day = text_dates(), y = rnorm(300), stringsAsFactors = FALSE)
  msg <- chart_hint(d, list(type = "line", x = "day", y = "y"))
  expect_true(grepl("text", msg))
  expect_true(grepl("Change variable types", msg))   # followable, like the
  expect_true(grepl("Import", msg))                  # Filter rows precedent
  expect_false(grepl("box plot", msg))               # NOT the generic advice
  # a real Date column says nothing at all
  d2 <- data.frame(day = as.Date("2024-01-01") + 0:299, y = rnorm(300))
  expect_null(chart_hint(d2, list(type = "line", x = "day", y = "y")))
  # an ordinary category column still gets the ordinary hint
  d3 <- data.frame(g = rep(letters[1:5], 60), y = rnorm(300))
  expect_true(grepl("categorical",
                    chart_hint(d3, list(type = "line", x = "g", y = "y"))))
})

test_that("looks_like_text_date agrees with the Import tab's own detector", {
  expect_false(looks_like_text_date(letters))
  expect_false(looks_like_text_date(c("12345", "67890")))          # zip-like
  expect_false(looks_like_text_date(1:10))
  expect_false(looks_like_text_date(as.Date("2024-01-01") + 0:5))  # real Date
  expect_true(looks_like_text_date(c("2024-01-01", "2024-02-15", "2024-03-09")))
  expect_true(looks_like_text_date(c("2024/01/01", "2024/02/15", "2024/03/09")))
  # whatever it claims, the Data Health dates fix must be able to deliver
  v <- c("2024-01-01", "2024-02-15", "2024-03-09")
  expect_false(is.null(dates_from_text(v)))
})

# ---- X_LEVELS_MAX: the per-value-chart x cap ----------------------------------
# Regression guard for the 0.12.0 block: a box plot over a ~1,500-distinct
# numeric X used to draw one box per value (measured: a 1,500-level violin cost
# 22.4s to build plus 40.2s in ggplotly -- a one-minute freeze). Past the cap
# the builder refuses, the hint explains, and the emitted code is a comment.

test_that("per-value charts draw at X_LEVELS_MAX x levels and block one past", {
  n  <- 3L * (X_LEVELS_MAX + 1L)
  df <- data.frame(y = rnorm(n))
  df$at  <- rep_len(seq_len(X_LEVELS_MAX),      n)
  df$ovr <- rep_len(seq_len(X_LEVELS_MAX + 1L), n)
  expect_equal(dplyr::n_distinct(df$at),  as.integer(X_LEVELS_MAX))
  expect_equal(dplyr::n_distinct(df$ovr), X_LEVELS_MAX + 1L)
  for (ty in c("boxplot", "violin", "meanerror", "bar")) {
    expect_s3_class(build_full_plot(df, list(type = ty, x = "at", y = "y")),
                    "ggplot")
    expect_null(build_full_plot(df, list(type = ty, x = "ovr", y = "y")))
  }
  # A DISCRETE x past the cap blocks the box family too (an ID column is the
  # same one-glyph-per-value pathology) -- but a discrete BAR still lumps to
  # "Other" via BAR_MAX instead of blocking.
  df$id <- sprintf("id%04d", df$ovr)
  expect_null(build_full_plot(df, list(type = "boxplot", x = "id", y = "y")))
  expect_s3_class(build_full_plot(df, list(type = "bar", x = "id")), "ggplot")
})

test_that("the x cap leaves per-row charts, dates, and histograms alone", {
  df <- data.frame(x = rep(seq_len(2000), each = 2))
  df$y <- rnorm(nrow(df))
  # one mark per ROW, not per level -- must stay available at any cardinality
  expect_s3_class(build_full_plot(df, list(type = "scatter", x = "x", y = "y")),
                  "ggplot")
  expect_s3_class(build_full_plot(df, list(type = "line", x = "x", y = "y")),
                  "ggplot")
  # histogram is the escape hatch the bar message advises -- it must work
  expect_s3_class(build_full_plot(df, list(type = "histogram", x = "x")),
                  "ggplot")
  # a real Date axis picks its own pretty breaks; never blocked
  dd <- data.frame(day = as.Date("2020-01-01") + seq_len(1500), y = rnorm(1500))
  expect_s3_class(build_full_plot(dd, list(type = "line", x = "day", y = "y")),
                  "ggplot")
  expect_s3_class(build_full_plot(dd, list(type = "boxplot", x = "day", y = "y")),
                  "ggplot")
  expect_null(plot_block_reason(dd, list(type = "boxplot", x = "day", y = "y")))
})

test_that("plot_block_reason names the column, the count, and the way out", {
  df <- data.frame(x = rep(seq_len(1500), 2), y = rnorm(3000))
  r  <- plot_block_reason(df, list(type = "boxplot", x = "x", y = "y"))
  expect_true(grepl("x has", r, fixed = TRUE))
  expect_true(grepl("1,500", r, fixed = TRUE))
  expect_true(grepl(as.character(X_LEVELS_MAX), r))
  expect_true(grepl("Filter rows", r, fixed = TRUE))
  rb <- plot_block_reason(df, list(type = "bar", x = "x"))
  expect_true(grepl("istogram", rb))
  # benign / short-list (mod_compare) shapes never error and never block
  expect_null(plot_block_reason(df, list(type = "scatter", x = "x", y = "y")))
  expect_null(plot_block_reason(df, list(type = "boxplot", x = "nope")))
  expect_null(plot_block_reason(df, list(type = "boxplot")))
  expect_null(plot_block_reason(df, list(x = "x")))
})

test_that("the x cap counts NA as a level, like GROUP_MAX does", {
  n  <- 2L * X_LEVELS_MAX
  df <- data.frame(x = rep_len(seq_len(X_LEVELS_MAX), n), y = rnorm(n))
  expect_s3_class(build_full_plot(df, list(type = "boxplot", x = "x", y = "y")),
                  "ggplot")
  df$x[1] <- NA  # X_LEVELS_MAX real levels + NA = one past the cap
  expect_null(build_full_plot(df, list(type = "boxplot", x = "x", y = "y")))
  expect_false(is.null(plot_block_reason(df, list(type = "boxplot", x = "x",
                                                  y = "y"))))
})

test_that("generate_code refuses a blocked chart with a parseable comment", {
  df <- data.frame(x = rep(seq_len(1500), 2), y = rnorm(3000))
  code <- generate_code(df, list(type = "boxplot", x = "x", y = "y"))
  expect_false(grepl("geom_boxplot", code, fixed = TRUE))
  expect_true(grepl(as.character(X_LEVELS_MAX), code))
  expect_silent(parse(text = code))
  # an at-cap config still emits real chart code
  d2 <- data.frame(x = rep(seq_len(X_LEVELS_MAX), 2),
                   y = rnorm(2L * X_LEVELS_MAX))
  expect_true(grepl("geom_boxplot",
                    generate_code(d2, list(type = "boxplot", x = "x", y = "y")),
                    fixed = TRUE))
})

test_that("chart_hint explains the block and keeps its advisory layering", {
  df <- data.frame(x = rep(seq_len(1500), 2), y = rnorm(3000))
  h  <- chart_hint(df, list(type = "boxplot", x = "x", y = "y"))
  expect_true(grepl("<b>x</b>", h, fixed = TRUE))
  expect_true(grepl("1,500", h, fixed = TRUE))
  expect_true(grepl(as.character(X_LEVELS_MAX), h))
  expect_true(grepl("Filter rows", h, fixed = TRUE))
  expect_true(grepl("istogram", chart_hint(df, list(type = "bar", x = "x"))))
  # the 11-100 band keeps the OLD advisory wording (warn, still draw)
  d2 <- data.frame(x = rep(seq_len(40), 2), y = rnorm(80))
  h2 <- chart_hint(d2, list(type = "boxplot", x = "x", y = "y"))
  expect_true(grepl("one group per value", h2, fixed = TRUE))
  expect_false(grepl("not rendered", h2, fixed = TRUE))
  # a text-date column past the cap still gets the CONVERSION advice first --
  # converting to a real Date un-blocks, so that clause must win
  days <- format(as.Date("2018-01-01") + seq_len(200), "%Y-%m-%d")
  d3 <- data.frame(x = rep(days, 2), y = rnorm(400))
  expect_true(grepl("Change variable types",
                    chart_hint(d3, list(type = "boxplot", x = "x", y = "y")),
                    fixed = TRUE))
})
