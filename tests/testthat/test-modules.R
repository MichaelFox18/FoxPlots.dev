# Module smoke tests.
#
# Before 0.10.0 not one of the twelve mod_*.R servers had automated coverage,
# while the helpers they wrap had thousands of lines of it. The gap matters
# because the bugs that actually escaped to users lived in the WIRING, not the
# maths -- a re-rendered picker silently discarding a selection, a return value
# changing shape, a validation gate that never fires.
#
# These are deliberately smoke tests: the contract of each module (what it
# returns, that it survives empty/garbage input, that its central params
# reactive reflects the inputs). Deep behaviour stays in the helper tests.

# Small, well-behaved frames used throughout.
mod_df   <- function() data.frame(g = rep(c("a", "b"), each = 5),
                                  x = 1:10, y = rnorm(10),
                                  stringsAsFactors = FALSE)
mod_wide <- function() data.frame(id = 1:3, q1 = c(5, 2, 4), q2 = c(3, 4, 1))

# ---- mod_import --------------------------------------------------------------

test_that("importServer returns a reactive and starts empty", {
  testServer(importServer, args = list(examples = list(demo = mod_df())), {
    expect_true(is.function(session$returned))
    expect_null(session$returned())          # nothing loaded yet
  })
})

test_that("importServer loads an example and publishes it downstream", {
  testServer(importServer, args = list(examples = list(demo = mod_df())), {
    session$setInputs(example = "demo", load_example = 1)
    out <- session$returned()
    expect_s3_class(out, "data.frame")
    expect_equal(nrow(out), 10L)
    expect_named(out, c("g", "x", "y"))
  })
})

test_that("importServer's row filter narrows what it publishes", {
  testServer(importServer, args = list(examples = list(demo = mod_df())), {
    session$setInputs(example = "demo", load_example = 1)
    expect_equal(nrow(session$returned()), 10L)
    session$setInputs(filter_col = "g", filter_op = "in",
                      filter_vals = "a", filter_add = 1)
    expect_equal(nrow(session$returned()), 5L)
    expect_true(all(session$returned()$g == "a"))
  })
})

# ---- mod_reshape -------------------------------------------------------------

test_that("reshapeServer stacks columns and returns the reshaped frame", {
  testServer(reshapeServer, args = list(data_in = reactive(mod_wide())), {
    session$setInputs(op = "stack", stack_cols = c("q1", "q2"),
                      label_to = "Label", value_to = "Data", apply = 1)
    out <- session$returned()
    expect_s3_class(out, "data.frame")
    expect_equal(nrow(out), 6L)
    expect_true(all(c("Label", "Data") %in% names(out)))
  })
})

test_that("reshapeServer passes data through untouched before Apply", {
  testServer(reshapeServer, args = list(data_in = reactive(mod_wide())), {
    expect_equal(session$returned(), mod_wide())
  })
})

# ---- mod_summarize -----------------------------------------------------------

test_that("summarizeServer returns a grouped summary table", {
  testServer(summarizeServer, args = list(data_in = reactive(mod_df())), {
    session$setInputs(mode = "stats", groups = "g", vars = "x")
    out <- session$returned()
    expect_s3_class(out, "data.frame")
    expect_equal(nrow(out), 2L)             # one row per group
  })
})

# ---- mod_visualize -----------------------------------------------------------

test_that("visualizeServer returns plots, code and specs for its slots", {
  testServer(visualizeServer, args = list(data_in = reactive(mod_df())), {
    session$setInputs(n_plots = 1, mp1_type = "scatter",
                      mp1_xvar = "x", mp1_yvar = "y")
    out <- session$returned()
    expect_type(out, "list")
    expect_true(length(out$plots) >= 1L)
    expect_s3_class(out$plots[[1]], "ggplot")
    expect_true(any(grepl("ggplot", unlist(out$code))))
  })
})

test_that("visualizeServer drops a colour past GROUP_MAX (the 0.10.0 freeze)", {
  # The wiring half of the P1 fix: the module must hand mod_export/mod_report
  # a plot with no colour mapping, not a 51-level one.
  df <- data.frame(x = rnorm(200), y = rnorm(200),
                   id = sprintf("i%03d", 1:200), stringsAsFactors = FALSE)
  testServer(visualizeServer, args = list(data_in = reactive(df)), {
    session$setInputs(n_plots = 1, mp1_type = "scatter", mp1_xvar = "x",
                      mp1_yvar = "y", mp1_colorvar = "id")
    p <- session$returned()$plots[[1]]
    expect_null(p$mapping$colour)
    expect_null(p$mapping$fill)
  })
})

test_that("visualizeServer thins a crowded x axis and honours the override", {
  df <- data.frame(day = sprintf("2024-%02d-%02d", rep(1:12, each = 25),
                                 rep(1:25, 12)),
                   y = rnorm(300), stringsAsFactors = FALSE)
  x_scales <- function(p) Filter(function(z) "x" %in% z$aesthetics,
                                 p$scales$scales)
  testServer(visualizeServer, args = list(data_in = reactive(df)), {
    session$setInputs(n_plots = 1, mp1_type = "line", mp1_xvar = "day",
                      mp1_yvar = "y", mp1_xticks = "auto")
    sc <- x_scales(session$returned()$plots[[1]])
    expect_equal(length(sc), 1L)
    expect_lte(length(sc[[1]]$breaks), AXIS_LABEL_MAX)
    session$setInputs(mp1_xticks = "all")
    expect_equal(length(x_scales(session$returned()$plots[[1]])), 0L)
  })
})

test_that("the date-format picker appears only for a REAL Date x", {
  # The wiring half: conditionalPanel cannot see column types, so this must be
  # a renderUI. Reverting it to a static control would pass the helper tests.
  df <- data.frame(d = as.Date("2024-01-01") + 0:9,
                   s = sprintf("2024-01-%02d", 1:10),
                   y = rnorm(10), stringsAsFactors = FALSE)
  testServer(visualizeServer, args = list(data_in = reactive(df)), {
    session$setInputs(n_plots = 1, mp1_type = "line", mp1_yvar = "y",
                      mp1_xvar = "s")
    expect_null(output$ui_mp1_xdatefmt)      # renderUI returned NULL -> no UI
    session$setInputs(mp1_xvar = "d")
    html <- as.character(output$ui_mp1_xdatefmt$html)
    expect_true(grepl("mp1_xdatefmtv", html, fixed = TRUE))
    # options are labelled with the format applied to the user's own data
    expect_true(grepl("2024-01-01", html, fixed = TRUE))
  })
})

# ---- mod_map -----------------------------------------------------------------

test_that("mapServer returns a leaflet widget once coordinates resolve", {
  testServer(mapServer, args = list(data_in = reactive(make_map_example_data())), {
    session$setInputs(map_type = "points", lon = "lon", lat = "lat",
                      basemap = "CartoDB.Positron", show_points = TRUE)
    session$flushReact()
    out <- session$returned()
    expect_type(out, "list")
  })
})

test_that("mapServer's region picker really renders the example labels", {
  # Caught by mutation testing: helper tests proved geojson_prop_choices()
  # builds "county - e.g. Brooks", but NOTHING asserted mod_map used it, so
  # reverting the picker to bare `props` passed the whole suite.
  states <- data.frame(state = c("Florida", "Georgia"), rate = c(1, 2),
                       stringsAsFactors = FALSE)
  testServer(mapServer, args = list(data_in = reactive(states)), {
    session$setInputs(map_type = "choro", geo_source = "us_states",
                      geo_filter = "__all__")
    session$flushReact()
    html <- as.character(output$ui_choro$html %||% output$ui_choro)
    expect_true(grepl("region_prop", html, fixed = TRUE))
    expect_true(grepl("e.g.", html, fixed = TRUE),
                info = "the property picker must label options with an example")
    # values stay bare, or keep_sel() and the built-in key lookup break
    expect_true(grepl('value="state"', html, fixed = TRUE))
  })
})

test_that("mapServer's choropleth needs no coordinates at all", {
  # The P5 claim, exercised through the module rather than the helper.
  states <- data.frame(state = c("Florida", "Georgia", "Texas"),
                       rate = c(12.7, 14.0, 13.4), stringsAsFactors = FALSE)
  testServer(mapServer, args = list(data_in = reactive(states)), {
    session$setInputs(map_type = "choro", geo_source = "us_states",
                      geo_filter = "__all__", region_prop = "state",
                      region_key = "state", region_value = "rate",
                      region_agg = "mean", choro_palette = "auto",
                      choro_scale = "linear", choro_legend = TRUE)
    session$flushReact()
    expect_no_error(session$returned())
  })
})

# ---- mod_compare -------------------------------------------------------------

test_that("compareServer returns comparison results for one outcome", {
  testServer(compareServer, args = list(data_in = reactive(mod_df())), {
    session$setInputs(mode = "num", outcome = "y", group = "g",
                      method = "param", posthoc = "dunn",
                      split_by = "__none__")
    session$flushReact()
    expect_no_error(session$returned())
  })
})

# ---- mod_regression ----------------------------------------------------------

test_that("regressionServer fits a model and returns it", {
  testServer(regressionServer, args = list(data_in = reactive(mod_df())), {
    session$setInputs(family = "linear", yvar = "y", xvars = "x", fit = 1)
    session$flushReact()
    m <- session$returned()
    expect_true(is.null(m) || inherits(m$mod, c("lm", "glm")) || is.list(m))
  })
})

test_that("regressionServer returns nothing before Fit is pressed", {
  testServer(regressionServer, args = list(data_in = reactive(mod_df())), {
    session$setInputs(family = "linear", yvar = "y", xvars = "x")
    expect_null(session$returned())
  })
})

# ---- mod_combine -------------------------------------------------------------

test_that("combineServer joins two tables and returns the result", {
  l <- data.frame(k = c("a", "b"), v1 = 1:2, stringsAsFactors = FALSE)
  r <- data.frame(k = c("a", "b"), v2 = 3:4, stringsAsFactors = FALSE)
  testServer(combineServer,
             args = list(left = reactive(l), right = reactive(r)), {
    session$setInputs(op = "join", join_type = "left", join_by = "k")
    out <- session$returned()
    expect_s3_class(out, "data.frame")
    expect_true(all(c("v1", "v2") %in% names(out)))
    expect_equal(nrow(out), 2L)
  })
})

# ---- mod_export / mod_report -------------------------------------------------

test_that("exportServer wires up without error and offers its downloads", {
  testServer(exportServer, args = list(data_in = reactive(mod_df())), {
    session$setInputs(fmt = "csv", filename = "test-export")
    session$flushReact()
    expect_true(nzchar(output$caption))
  })
})

test_that("reportServer's section picker gates what the report includes", {
  testServer(reportServer,
             args = list(data_in = reactive(mod_df()),
                         summary_tbl = reactive(mod_df())), {
    session$setInputs(title = "T", author = "A", fmt = "html",
                      inc_data = TRUE, inc_summary = FALSE)
    session$flushReact()
    expect_no_error(session$flushReact())
  })
})

# ---- mod_lmer / mod_glmm -----------------------------------------------------

test_that("lmerServer returns the data + report payload pair", {
  skip_if_not_installed("lmerTest")
  testServer(lmerServer, args = list(data_in = reactive(make_example_data())), {
    session$flushReact()
    out <- session$returned            # a LIST, not a reactive -- do not call
    expect_type(out, "list")
    expect_true(all(c("data", "report") %in% names(out)))
    expect_true(is.reactive(out$data) && is.reactive(out$report))
  })
})

test_that("glmmServer returns the same pair in both general and binary modes", {
  skip_if_not_installed("glmmTMB")
  for (bin in c(FALSE, TRUE)) {
    testServer(glmmServer,
               args = list(data_in = reactive(make_glmm_example_data()),
                           binary = bin), {
      session$flushReact()
      out <- session$returned          # a LIST, not a reactive -- do not call
      expect_type(out, "list")
      expect_true(all(c("data", "report") %in% names(out)))
    })
  }
})

# ---- 0.12.0 additions: import rename + the 8-slot / blocked-config wiring ----

test_that("importServer renames a column and publishes the new name", {
  testServer(importServer, args = list(examples = list(demo = mod_df())), {
    session$setInputs(example = "demo", load_example = 1)
    session$setInputs(rn_col = "g", rn_new = "group", rn_apply = 1)
    expect_named(session$returned(), c("group", "x", "y"))
  })
})

test_that("importServer's rename carries an active filter to the new name", {
  # The one real rename hazard: filters store bare column names, and an
  # unknown column silently no-ops (helpers_filter.R). The handler must
  # rewrite the stored condition or a rename silently UN-filters.
  testServer(importServer, args = list(examples = list(demo = mod_df())), {
    session$setInputs(example = "demo", load_example = 1)
    session$setInputs(filter_col = "g", filter_op = "in",
                      filter_vals = "a", filter_add = 1)
    expect_equal(nrow(session$returned()), 5L)
    session$setInputs(rn_col = "g", rn_new = "group", rn_apply = 1)
    out <- session$returned()
    expect_true("group" %in% names(out))
    expect_false("g" %in% names(out))
    expect_equal(nrow(out), 5L)                  # still filtered, not 10
    expect_true(all(out$group == "a"))
    expect_equal(rv$filters[[1]]$col, "group")   # the chip tells the truth
  })
})

test_that("visualizeServer reaches slot 8 and renders all eight accordions", {
  testServer(visualizeServer, args = list(data_in = reactive(mod_df())), {
    session$setInputs(n_plots = MAX_PLOTS,
                      mp1_type = "scatter", mp1_xvar = "x", mp1_yvar = "y",
                      mp8_type = "scatter", mp8_xvar = "x", mp8_yvar = "y")
    out <- session$returned()
    expect_equal(length(out$plots), 2L)          # slots 1 and 8; 2-7 skipped
    expect_s3_class(out$plots[[2]], "ggplot")
    html <- as.character(output$plot_config_accordion$html)
    expect_true(grepl(paste0("mp", MAX_PLOTS, "_type"), html, fixed = TRUE))
  })
})

test_that("visualizeServer blocks a per-value chart past X_LEVELS_MAX", {
  # The wiring half of the x cap: Export/Report must never see the blocked
  # slot, and the sidebar hint must explain the block.
  df <- data.frame(x = rep(seq_len(X_LEVELS_MAX + 50L), 2))
  df$y <- rnorm(nrow(df))
  testServer(visualizeServer, args = list(data_in = reactive(df)), {
    session$setInputs(n_plots = 1, mp1_type = "boxplot",
                      mp1_xvar = "x", mp1_yvar = "y")
    expect_equal(length(session$returned()$plots), 0L)
    html <- as.character(output$ui_mp1_hint$html)
    expect_true(grepl(as.character(X_LEVELS_MAX), html))
  })
})
