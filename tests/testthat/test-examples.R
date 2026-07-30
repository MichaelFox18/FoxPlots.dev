# Tests for the example-dataset registry (helpers_examples.R): one source of
# truth for menu labels AND data, so a launcher can no longer advertise a key
# it cannot load. Plus the two generated sets (daily time series + the messy
# Data Health demo), which follow the house generator pattern: a shape/domain
# test and an RNG-hygiene test each.

test_that("every registry entry has a label, tags and a lazy builder", {
  for (k in names(EXAMPLE_SETS)) {
    e <- EXAMPLE_SETS[[k]]
    expect_true(all(c("label", "tags", "build") %in% names(e)), info = k)
    expect_true(is.character(e$label) && nzchar(e$label), info = k)
    expect_true(is.function(e$build), info = k)          # lazy, not data
    expect_equal(length(formals(e$build)), 0L, info = k)
  }
  # Labels are the menu text: duplicates would render identical options.
  labs <- vapply(EXAMPLE_SETS, `[[`, character(1), "label")
  expect_equal(anyDuplicated(labs), 0L)
  # ASCII only -- these strings land in man/ via foxplots_examples().
  expect_false(any(grepl("[^ -~]", labs)))
})

test_that("every advertised key builds a usable data frame", {
  for (k in names(EXAMPLE_SETS)) {
    d <- foxplots_example(k)
    expect_s3_class(d, "data.frame")
    expect_gt(nrow(d), 0L)
    expect_gt(ncol(d), 0L)
    # DT / writexl / column_profile all assume atomic columns; a slice must
    # not leak its source rownames into previews and exports; ordered factors
    # contrast-code differently in every model downstream.
    expect_false(any(vapply(d, is.list, logical(1))), info = k)
    expect_identical(rownames(d), as.character(seq_len(nrow(d))), info = k)
    expect_false(any(vapply(d, is.ordered, logical(1))), info = k)
  }
})

test_that("as_example_spec rejects an unknown key loudly", {
  expect_error(as_example_spec(c("mtcars", "nope")), "Unknown example key")
  expect_error(as_example_spec(42), "must be example keys")
  expect_named(as_example_spec(NULL), EXAMPLES_DEFAULT)
})

test_that("as_example_spec still accepts a named list (the test-fixture path)", {
  spec <- as_example_spec(list(demo = data.frame(a = 1:3)))
  expect_named(spec, "demo")
  expect_identical(example_build(spec, "demo"), data.frame(a = 1:3))
  # a zero-arg builder function works too
  spec2 <- as_example_spec(list(gen = function() data.frame(b = 1)))
  expect_identical(example_build(spec2, "gen"), data.frame(b = 1))
})

test_that("example_menu produces label -> key in menu order", {
  m <- example_menu(as_example_spec(c("iris", "mtcars")))
  expect_equal(unname(m), c("iris", "mtcars"))
  expect_equal(names(m)[1], EXAMPLE_SETS$iris$label)
})

test_that("example_build refuses to succeed on a broken entry", {
  spec <- list(bad = list(label = "bad", tags = character(0),
                          build = function() NULL))
  expect_error(example_build(spec, "bad"), "did not produce a usable table")
  expect_error(example_build(spec, "missing"), "No example named")
})

test_that("every launcher menu references only registry keys, none orphaned", {
  menus <- list(EXAMPLES_DEFAULT, EXAMPLES_EXPLORER, EXAMPLES_RESHAPE,
                EXAMPLES_COMBINE, EXAMPLES_COMPARE, EXAMPLES_REGRESSION,
                EXAMPLES_LMER, EXAMPLES_GLMM, EXAMPLES_MAP)
  used <- unique(unlist(menus))
  expect_equal(setdiff(used, names(EXAMPLE_SETS)), character(0))
  # no dead registry entries either -- an unadvertised set is untested code
  expect_equal(setdiff(names(EXAMPLE_SETS), used), character(0))
})

test_that("app menus carry what their features need", {
  # Compare Groups: the chi-square path needs 2+ groupable columns somewhere.
  ok_cat <- any(vapply(EXAMPLES_COMPARE, function(k)
    length(groupable_cols(foxplots_example(k))) >= 2, logical(1)))
  expect_true(ok_cat)
  # Map: at least one point set (coords resolve) and one choropleth set
  # (no coords -- joins by name instead).
  has_coords <- vapply(EXAMPLES_MAP, function(k) {
    d <- foxplots_example(k)
    cc <- detect_coord_cols(d)
    !is.null(cc$lat) && !is.null(cc$lon)
  }, logical(1))
  expect_true(any(has_coords))
  expect_true(any(!has_coords))
  # Mixed models: at least one set with a 5+-level grouping column.
  ok_grp <- any(vapply(EXAMPLES_LMER, function(k) {
    d <- foxplots_example(k)
    any(vapply(d, function(x) !is.numeric(x) &&
                 dplyr::n_distinct(x) >= 5, logical(1)))
  }, logical(1)))
  expect_true(ok_grp)
})

# ---- make_timeseries_example_data --------------------------------------------

test_that("make_timeseries_example_data is seeded and date-axis ready", {
  d <- make_timeseries_example_data()
  expect_equal(nrow(d), 1095L)
  expect_named(d, c("date", "date_text", "station", "month",
                    "temp_c", "rain_mm", "ndvi"))
  expect_s3_class(d$date, "Date")
  expect_type(d$date_text, "character")
  expect_equal(length(unique(d$date)), 365L)   # enough to crowd an axis
  expect_equal(levels(d$month), month.abb)     # correct chart order
  expect_false(is.ordered(d$month))
  expect_length(unique(d$station), 3L)
  expect_gt(mean(d$temp_c[d$month == "Jul"]),
            mean(d$temp_c[d$month == "Jan"]) + 5)   # a real seasonal signal
  expect_true(all(d$ndvi > 0 & d$ndvi < 1))
  expect_gt(mean(d$rain_mm == 0), 0.2)              # dry days
  expect_true(all(d$rain_mm >= 0))
  # deliberately just over BIG_ROWS: Visualize renders static ggplot, which is
  # where axis-label crowding is visible (plotly thins its own ticks).
  expect_gt(nrow(d), BIG_ROWS)
  # the text date is exactly the Date, ISO-formatted, and the Data Health fix
  # can convert it back
  expect_identical(d$date_text, format(d$date, "%Y-%m-%d"))
  expect_false(is.null(dates_from_text(d$date_text)))
  expect_identical(d, make_timeseries_example_data())  # seeded -> reproducible
})

test_that("make_timeseries_example_data leaves the session RNG untouched", {
  set.seed(999); x1 <- runif(1)
  set.seed(999); invisible(make_timeseries_example_data()); x2 <- runif(1)
  expect_identical(x1, x2)
})

# ---- make_messy_example_data -------------------------------------------------

test_that("make_messy_example_data triggers every Data Health detector", {
  d <- make_messy_example_data()
  expect_equal(nrow(d), 123L)
  expect_setequal(names(detect_issues(d)), names(clean_specs()))  # all 9
  f <- clean_apply(d, names(clean_specs()))
  expect_length(detect_issues(f), 0L)          # every fix really fixes
  expect_equal(nrow(f), 120L)                  # blank row + 2 dups gone
  expect_false("" %in% names(f))               # blank header renamed/dropped
  expect_type(f$price_usd, "double")           # "$4,271" -> 4271
  expect_s3_class(f$sample_date, "Date")
  expect_type(f$plot_id, "character")          # leading zeros survive
  expect_true(all(grepl("^0", f$plot_id)))
  expect_setequal(unique(f$treatment), c("Control", "Low N", "High N"))
  expect_equal(sum(f$is_outlier), 3L)
  expect_equal(sum(is.na(f$moisture_pct)), 6L) # real NAs are NOT "fixed"
  # case drift in crop is deliberately untouched by the fixes
  expect_gt(dplyr::n_distinct(f$crop), 3L)
  expect_identical(d, make_messy_example_data())
})

test_that("make_messy_example_data leaves the session RNG untouched", {
  set.seed(999); x1 <- runif(1)
  set.seed(999); invisible(make_messy_example_data()); x2 <- runif(1)
  expect_identical(x1, x2)
})
