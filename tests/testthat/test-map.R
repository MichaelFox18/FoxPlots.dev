# Tests for the pure map logic in helpers_map.R: coordinate detection and
# cleaning, radius / palette scaling, popups, hints, the leaflet widget
# builder (structure assertions on the htmlwidget), and the code generator.
# leaflet is in Imports, so widget-building tests run un-gated. The HTML
# exporter is pure text-processing and fully tested; only the PNG path (needs
# a headless browser) gets a light probe check.

# Small helpers to inspect a leaflet htmlwidget's structure.
map_call_methods <- function(w) vapply(w$x$calls, function(cl) cl$method, character(1))
map_get_call <- function(w, m) w$x$calls[[match(m, map_call_methods(w))]]
map_has_dep <- function(w, name) {
  any(grepl(name, vapply(w$dependencies %||% list(),
                         function(d) d$name %||% "", character(1)),
            fixed = TRUE))
}
arg_is <- function(cl, val)
  any(vapply(cl$args, function(a) identical(a, val), logical(1)))

# ---- coordinate detection ----------------------------------------------------

test_that("detect_coord_cols finds name-matched, range-valid columns", {
  df <- data.frame(Longitude = c(-82.3, -81.7), LAT = c(29.6, 27.9), yield = 1:2)
  got <- detect_coord_cols(df)
  expect_equal(got$lon, "Longitude")   # case-insensitive match
  expect_equal(got$lat, "LAT")
})

test_that("detect_coord_cols handles datasets::quakes (lat + long, 0-360)", {
  got <- detect_coord_cols(datasets::quakes)
  expect_equal(got$lon, "long")
  expect_equal(got$lat, "lat")
})

test_that("detect_coord_cols rejects a name match whose values are out of range", {
  df <- data.frame(lat = c(250, 300), lon = c(10, 20))
  got <- detect_coord_cols(df)
  expect_null(got$lat)                 # named 'lat' but not latitudes
  expect_equal(got$lon, "lon")
  expect_null(detect_coord_cols(data.frame(a = 1:3, b = letters[1:3]))$lon)
})

test_that("detect_coord_cols never returns the same column twice", {
  got <- detect_coord_cols(data.frame(x = c(10, 20)))   # valid for either role
  expect_false(identical(got$lon, got$lat))
})

# ---- clean_coords --------------------------------------------------------------

test_that("clean_coords drops NA and out-of-range rows and counts them", {
  df <- data.frame(lon = c(-82, NA, 200.5, -81, 400),
                   lat = c(29, 28, 27, 999, 26))
  out <- clean_coords(df, "lon", "lat")
  expect_equal(nrow(out$data), 2L)   # rows 1 and 3 (200.5 is a valid 0-360 lon)
  expect_equal(out$n_dropped, 3L)    # NA lon, 999 lat, 400 lon
  expect_equal(out$n_total, 5L)
})

test_that("clean_coords keeps 0-360 longitudes (quakes convention)", {
  out <- clean_coords(datasets::quakes, "long", "lat")
  expect_equal(out$n_dropped, 0L)
  expect_equal(nrow(out$data), nrow(datasets::quakes))
})

test_that("clean_coords errors on unknown or non-numeric columns", {
  df <- data.frame(lon = 1:2, lat = c("a", "b"))
  expect_error(clean_coords(df, "nope", "lat"), "not found")
  expect_error(clean_coords(df, "lon", "lat"), "numeric")
})

# ---- radii ---------------------------------------------------------------------

test_that("scale_radius is area-proportional (sqrt) and bounded", {
  r <- scale_radius(c(0, 1, 4), range = c(4, 18))
  expect_equal(r, c(4, 4 + 14 * 0.5, 18))      # sqrt(0), sqrt(1/4), sqrt(4/4)
  expect_true(all(scale_radius(rnorm(50), c(4, 18)) >= 4))
  expect_true(all(scale_radius(rnorm(50), c(4, 18)) <= 18))
})

test_that("scale_radius handles constants, NAs and non-numerics", {
  expect_equal(scale_radius(c(5, 5, 5), c(4, 18)), rep(11, 3))   # midpoint
  r <- scale_radius(c(0, NA, 4), c(4, 18))
  expect_equal(r[2], 4)                          # NA -> smallest
  expect_null(scale_radius(letters))
  expect_null(scale_radius(NULL))
})

# ---- palettes ------------------------------------------------------------------

test_that("map_palette_colors mirrors the house palette decision tree", {
  expect_equal(map_palette_colors("auto", TRUE, 20), "viridis")
  expect_equal(map_palette_colors("auto", FALSE, 3), "Set1")
  expect_equal(map_palette_colors("auto", FALSE, 20), "viridis")   # > BREWER_MAX
  expect_equal(map_palette_colors("set1", FALSE, 20), "viridis")   # brewer overflow
  expect_equal(map_palette_colors("uf", TRUE, 5), c(UF_BLUE, UF_ORANGE))
  expect_equal(map_palette_colors("uf", FALSE, 3)[1:2], c(UF_BLUE, UF_ORANGE))
  expect_equal(map_palette_colors("cb", FALSE, 4), okabe_ito(4))
  expect_length(map_palette_colors("greys", FALSE, 5), 5)
})

test_that("map_palette builds working leaflet pal functions", {
  df <- data.frame(v = seq(0, 100, length.out = 20),
                   g = rep(c("a", "b"), 10),
                   few = rep(1:2, 10))
  cont <- map_palette(df, "v", "auto")
  expect_equal(cont$kind, "numeric")
  expect_match(cont$pal(50), "^#[0-9A-Fa-f]{6}")
  disc <- map_palette(df, "g", "auto")
  expect_equal(disc$kind, "factor")
  expect_match(disc$pal(factor("a", levels = c("a", "b"))), "^#[0-9A-Fa-f]{6}")
  expect_equal(map_palette(df, "few", "auto")$kind, "factor")  # numeric, <= 10 distinct
})

# ---- popups --------------------------------------------------------------------

test_that("popup_html escapes user content and rounds numerics", {
  df <- data.frame(site = c("<script>alert('x')</script>", "B"),
                   yield = c(1.23456, NA))
  html <- popup_html(df, c("site", "yield"))
  expect_length(html, 2L)
  expect_false(any(grepl("<script>", html, fixed = TRUE)))    # escaped
  expect_true(any(grepl("&lt;script&gt;", html, fixed = TRUE)))
  expect_true(grepl("1.235", html[1], fixed = TRUE))          # rounded
  expect_true(grepl("<b>site:</b>", html[1], fixed = TRUE))
  expect_null(popup_html(df, character(0)))
  expect_null(popup_html(df, "nope"))
})

# ---- hints ---------------------------------------------------------------------

test_that("map_hint flags each problem and stays quiet when all is well", {
  df <- data.frame(lon = c(-82, -81, NA), lat = c(29, 28, 27), g = letters[1:3])
  expect_match(map_hint(df, list(lon = "lat", lat = "lat")), "same column")
  expect_match(map_hint(df, list(lon = "lon", lat = "lat")), "1 of 3 rows")
  swapped <- data.frame(mylat = c(120, 150), mylon = c(10, 20))
  expect_match(map_hint(swapped, list(lon = "mylon", lat = "mylat")), "swapped")
  many <- data.frame(lon = rep(-82, 40), lat = rep(29, 40),
                     cat = as.character(seq_len(40)))
  expect_match(map_hint(many, list(lon = "lon", lat = "lat", color = "cat")),
               "legend is hidden")
  expect_match(map_hint(df, list(lon = "lon", lat = "lat", size_by = "g")),
               "ignored")
  big <- data.frame(lon = runif(5100, -82, -80), lat = runif(5100, 26, 30))
  expect_match(map_hint(big, list(lon = "lon", lat = "lat", cluster = "off")),
               "clustering")
  clean <- data.frame(lon = c(-82, -81), lat = c(29, 28))
  expect_null(map_hint(clean, list(lon = "lon", lat = "lat")))
})

# ---- the widget builder --------------------------------------------------------

test_that("build_leaflet_map returns NULL until the spec is usable", {
  df <- data.frame(lon = c(-82, -81), lat = c(29, 28), txt = c("a", "b"))
  expect_null(build_leaflet_map(NULL, list(lon = "lon", lat = "lat")))
  expect_null(build_leaflet_map(df, list(lon = NULL, lat = "lat")))
  expect_null(build_leaflet_map(df, list(lon = "__none__", lat = "lat")))
  expect_null(build_leaflet_map(df, list(lon = "nope", lat = "lat")))
  expect_null(build_leaflet_map(df, list(lon = "txt", lat = "lat")))
  expect_null(build_leaflet_map(df, list(lon = "lat", lat = "lat")))  # same col
  none <- data.frame(lon = c(NA, 500), lat = c(NA, 999))
  expect_null(build_leaflet_map(none, list(lon = "lon", lat = "lat")))
})

test_that("build_leaflet_map assembles the expected layers", {
  q <- datasets::quakes
  w <- build_leaflet_map(q, list(lon = "long", lat = "lat", color = "mag",
                                 basemap = "Esri.WorldImagery",
                                 popup_cols = c("depth", "stations")))
  expect_s3_class(w, "leaflet")
  m <- map_call_methods(w)
  expect_true(all(c("addProviderTiles", "addCircleMarkers", "addLegend") %in% m))
  expect_true(arg_is(map_get_call(w, "addProviderTiles"), "Esri.WorldImagery"))
  # 1000 points > MAP_CLUSTER_AUTO -> "auto" clustering kicks in
  expect_true(map_has_dep(w, "markercluster"))
  # popups made it in: a character arg of length nrow with bold field names
  cl <- map_get_call(w, "addCircleMarkers")
  expect_true(any(vapply(cl$args, function(a)
    is.character(a) && length(a) == nrow(q) && any(grepl("<b>depth:</b>", a, fixed = TRUE)),
    logical(1))))
  expect_false(is.null(w$x$fitBounds))          # no saved view -> fit to data
  expect_null(w$x$setView)
})

test_that("cluster / legend / view switches behave", {
  df <- data.frame(lon = runif(20, -82, -80), lat = runif(20, 27, 29),
                   g = rep(letters[1:4], 5), v = rnorm(20))
  # 20 rows < auto threshold and cluster off -> no cluster dependency
  w0 <- build_leaflet_map(df, list(lon = "lon", lat = "lat", cluster = "off"))
  expect_false(map_has_dep(w0, "markercluster"))
  w1 <- build_leaflet_map(df, list(lon = "lon", lat = "lat", cluster = "on"))
  expect_true(map_has_dep(w1, "markercluster"))
  # legend suppressed on demand and when categories overflow MAP_LEGEND_MAX
  w2 <- build_leaflet_map(df, list(lon = "lon", lat = "lat", color = "g",
                                   legend = FALSE))
  expect_false("addLegend" %in% map_call_methods(w2))
  many <- data.frame(lon = rep(-82, 40), lat = rep(29, 40),
                     cat = as.character(seq_len(40)))
  w3 <- build_leaflet_map(many, list(lon = "lon", lat = "lat", color = "cat"))
  expect_false("addLegend" %in% map_call_methods(w3))
  # a saved view wins over fitBounds
  w4 <- build_leaflet_map(df, list(lon = "lon", lat = "lat",
                                   view = list(lng = -81, lat = 28, zoom = 7)))
  expect_false(is.null(w4$x$setView))
  expect_null(w4$x$fitBounds)
  # graduated symbols: radius becomes a full-length numeric vector
  w5 <- build_leaflet_map(df, list(lon = "lon", lat = "lat", size_by = "v"))
  cl5 <- map_get_call(w5, "addCircleMarkers")
  n_vec_args <- sum(vapply(cl5$args, function(a)
    is.numeric(a) && length(a) == nrow(df), logical(1)))
  expect_gte(n_vec_args, 3L)                    # lng + lat + radius
  w6 <- build_leaflet_map(df, list(lon = "lon", lat = "lat"))
  cl6 <- map_get_call(w6, "addCircleMarkers")
  n_vec_args6 <- sum(vapply(cl6$args, function(a)
    is.numeric(a) && length(a) == nrow(df), logical(1)))
  expect_equal(n_vec_args6, 2L)                 # lng + lat only
})

# ---- code generation -----------------------------------------------------------

test_that("generate_map_code emits parseable leaflet code across specs", {
  df <- data.frame(lon = runif(30, -82, -80), lat = runif(30, 27, 29),
                   crop = rep(c("Citrus", "Tomato", "Peanut"), 10),
                   yield = rnorm(30, 30, 5),
                   "site name" = sprintf("s%02d", 1:30), check.names = FALSE)
  specs <- list(
    plain    = list(lon = "lon", lat = "lat"),
    colored  = list(lon = "lon", lat = "lat", color = "crop"),
    numeric  = list(lon = "lon", lat = "lat", color = "yield"),
    bubbles  = list(lon = "lon", lat = "lat", size_by = "yield"),
    loaded   = list(lon = "lon", lat = "lat", color = "crop", size_by = "yield",
                    popup_cols = c("crop", "site name"), label_col = "site name",
                    cluster = "on", title = "My map",
                    basemap = "Esri.WorldTopoMap"))
  for (nm in names(specs)) {
    code <- generate_map_code(df, specs[[nm]])
    expect_true(grepl("leaflet(df)", code, fixed = TRUE), info = nm)
    expect_error(parse(text = code), NA)                 # runnable R
    expect_true(all(charToRaw(code) <= as.raw(127L)), info = nm)  # ASCII only
  }
  expect_true(grepl("colorFactor",  generate_map_code(df, specs$colored)))
  expect_true(grepl("colorNumeric", generate_map_code(df, specs$numeric)))
  expect_true(grepl("radius = ~.map_radius", generate_map_code(df, specs$bubbles),
                    fixed = TRUE))
  loaded <- generate_map_code(df, specs$loaded)
  expect_true(grepl("`site name`", loaded, fixed = TRUE))  # non-syntactic backticked
  expect_true(grepl("Esri.WorldTopoMap", loaded, fixed = TRUE))
  expect_true(grepl("markerClusterOptions()", loaded, fixed = TRUE))
  expect_true(grepl("addControl", loaded, fixed = TRUE))
})

test_that("generate_map_code guides instead of erroring when unset", {
  df <- data.frame(lon = 1, lat = 2)
  expect_match(generate_map_code(NULL, list()), "^# Import data")
  expect_match(generate_map_code(df, list(lon = "", lat = "lat")),
               "Choose the longitude")
  expect_match(generate_map_code(df, list(lon = "lat", lat = "lat")),
               "Choose the longitude")
})

# ---- example data ---------------------------------------------------------------

test_that("make_map_example_data is deterministic, clean, and map-ready", {
  d1 <- make_map_example_data()
  d2 <- make_map_example_data()
  expect_identical(d1, d2)
  expect_named(d1, c("site", "county", "crop", "yield", "acres", "lat", "lon"))
  expect_equal(nrow(d1), 120L)
  expect_equal(clean_coords(d1, "lon", "lat")$n_dropped, 0L)
  got <- detect_coord_cols(d1)
  expect_equal(got$lon, "lon")
  expect_equal(got$lat, "lat")
})

# ---- color scales ----------------------------------------------------------------

test_that("map_color_scale falls back to linear when the data can't support it", {
  expect_equal(map_color_scale(1:100, "log"), "log")
  expect_equal(map_color_scale(c(0, 1:99), "log"), "linear")    # zero present
  expect_equal(map_color_scale(c(-5, 1:99), "log"), "linear")   # negative
  expect_equal(map_color_scale(1:100, "quantile"), "quantile")
  expect_equal(map_color_scale(rep(c(1, 2), 50), "quantile"), "linear") # 2 distinct
  expect_equal(map_color_scale(1:100, "nonsense"), "linear")
  expect_equal(map_color_scale(1:100, NULL), "linear")
})

test_that("map_palette honours the color scale for continuous columns", {
  df <- data.frame(v = c(1, 10, 100, 1000, seq(2, 900, length.out = 20)))
  lg <- map_palette(df, "v", "auto", "log")
  expect_equal(lg$scale, "log")
  expect_equal(lg$values, log10(df$v))          # pal expects log10 space
  expect_match(lg$pal(log10(50)), "^#[0-9A-Fa-f]{6}")
  qt <- map_palette(df, "v", "auto", "quantile")
  expect_equal(qt$scale, "quantile")
  expect_match(qt$pal(50), "^#[0-9A-Fa-f]{6}")
  ln <- map_palette(df, "v", "auto", "linear")
  expect_equal(ln$scale, "linear")
  # categorical color ignores the scale entirely
  dfg <- data.frame(g = rep(letters[1:3], 10))
  expect_equal(map_palette(dfg, "g", "auto", "log")$scale, "linear")
})

test_that("map_hint flags an unsupported color scale and oversized grouping", {
  df <- data.frame(lon = runif(30, -82, -80), lat = runif(30, 27, 29),
                   v = c(0, seq_len(29)) * 10,   # includes 0 -> log invalid
                   id = as.character(1:30))
  df$v[1] <- 0
  expect_match(map_hint(df, list(lon = "lon", lat = "lat", color = "v",
                                 color_scale = "log")),
               "log color scale")
  expect_match(map_hint(df, list(lon = "lon", lat = "lat", group_by = "id")),
               "too many for a layer list")
})

# ---- layer groups ----------------------------------------------------------------

test_that("group_by builds one toggleable layer per level", {
  df <- data.frame(lon = runif(30, -82, -80), lat = runif(30, 27, 29),
                   county = rep(c("Alachua", "Marion", "Polk"), 10),
                   yield = rnorm(30, 30, 4))
  w <- build_leaflet_map(df, list(lon = "lon", lat = "lat",
                                  group_by = "county", cluster = "on"))
  m <- map_call_methods(w)
  expect_equal(sum(m == "addCircleMarkers"), 3L)       # one layer per county
  expect_true("addLayersControl" %in% m)
  expect_true(map_has_dep(w, "markercluster"))         # per-group clustering
  # auto-color rule: no color picked -> colored (and legend'd) by the group
  expect_true("addLegend" %in% m)
  # group levels ride along as the marker layers' group names
  expect_true(arg_is(map_get_call(w, "addLayersControl"),
                     c("Alachua", "Marion", "Polk")))
})

test_that("NA group values become a '(missing)' layer in widget AND code", {
  df <- data.frame(lon = runif(12, -82, -80), lat = runif(12, 27, 29),
                   county = c(rep(c("A", "B"), 5), NA, NA))
  w <- build_leaflet_map(df, list(lon = "lon", lat = "lat", group_by = "county"))
  expect_true(arg_is(map_get_call(w, "addLayersControl"),
                     c("(missing)", "A", "B")))
  code <- generate_map_code(df, list(lon = "lon", lat = "lat",
                                     group_by = "county"))
  expect_true(grepl('"(missing)"', code, fixed = TRUE))
  expect_true(grepl("df$.map_group", code, fixed = TRUE))
  expect_error(parse(text = code), NA)
})

test_that("a 12-level column with NAs is gated consistently everywhere", {
  df <- data.frame(lon = runif(26, -82, -80), lat = runif(26, 27, 29),
                   g = c(rep(letters[1:12], 2), NA, NA))
  # 12 levels + "(missing)" = 13 > MAP_GROUP_MAX -> grouping ignored, hint says so
  w <- build_leaflet_map(df, list(lon = "lon", lat = "lat", group_by = "g"))
  expect_false("addLayersControl" %in% map_call_methods(w))
  expect_match(map_hint(df, list(lon = "lon", lat = "lat", group_by = "g")),
               "13 groups")
})

test_that(".splice replaces literally, including a target at the text's end", {
  expect_equal(.splice("abcXYZ", "XYZ", "-"), "abc-")     # end-of-string
  expect_equal(.splice("XYZabc", "XYZ", "-"), "-abc")     # start
  expect_equal(.splice("aXbXc", "X", "\\1"), "a\\1b\\1c") # verbatim backslashes
  expect_equal(.splice("abc", "X", "-"), "abc")           # absent -> unchanged
})

test_that("group_by is ignored beyond MAP_GROUP_MAX levels and for bad columns", {
  df <- data.frame(lon = rep(-82, 40), lat = rep(29, 40),
                   id = as.character(seq_len(40)))
  w <- build_leaflet_map(df, list(lon = "lon", lat = "lat", group_by = "id"))
  m <- map_call_methods(w)
  expect_equal(sum(m == "addCircleMarkers"), 1L)       # single flat layer
  expect_false("addLayersControl" %in% m)
  w2 <- build_leaflet_map(df, list(lon = "lon", lat = "lat", group_by = "nope"))
  expect_false("addLayersControl" %in% map_call_methods(w2))
})

test_that("generate_map_code mirrors scales and grouping and stays parseable", {
  df <- data.frame(lon = runif(30, -82, -80), lat = runif(30, 27, 29),
                   county = rep(c("A", "B", "C"), 10),
                   v = seq(1, 3000, length.out = 30))
  lg <- generate_map_code(df, list(lon = "lon", lat = "lat", color = "v",
                                   color_scale = "log"))
  expect_true(grepl("colorNumeric", lg))
  expect_true(grepl("log10", lg, fixed = TRUE))
  expect_true(grepl("labelFormat(transform", lg, fixed = TRUE))
  expect_error(parse(text = lg), NA)
  qt <- generate_map_code(df, list(lon = "lon", lat = "lat", color = "v",
                                   color_scale = "quantile"))
  expect_true(grepl("colorQuantile", qt))
  expect_error(parse(text = qt), NA)
  gp <- generate_map_code(df, list(lon = "lon", lat = "lat",
                                   group_by = "county",
                                   popup_cols = "v", size_by = "v"))
  expect_true(grepl("addLayersControl", gp, fixed = TRUE))
  expect_true(grepl("for (g in groups)", gp, fixed = TRUE))
  expect_true(grepl("group = g", gp, fixed = TRUE))
  expect_error(parse(text = gp), NA)
  expect_true(all(charToRaw(gp) <= as.raw(127L)))      # ASCII only
})

# ---- exporters -------------------------------------------------------------------

test_that("export capability probe returns a logical scalar", {
  expect_type(map_snapshot_ok(), "logical")
  expect_length(map_snapshot_ok(), 1L)
})

test_that("save_map_html writes ONE self-contained file (no pandoc, no sidecar)", {
  w <- build_leaflet_map(data.frame(lon = c(-82, -81), lat = c(29, 28)),
                         list(lon = "lon", lat = "lat"))
  out <- withr::local_tempfile(fileext = ".html")
  save_map_html(w, out)
  expect_true(file.exists(out))
  expect_gt(file.size(out), 100000)   # the leaflet js/css must be inside
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_false(grepl('src="map_files/', html, fixed = TRUE))
  expect_false(grepl('href="map_files/', html, fixed = TRUE))
  expect_true(grepl("data:application/javascript;base64,", html, fixed = TRUE))
  expect_true(grepl("<style>", html, fixed = TRUE))
})

# ---- app objects (smoke; no testServer -- it segfaults on this machine) ---------

test_that("the Map Tool and Data Explorer app objects build", {
  expect_s3_class(map_tool_app(), "shiny.appobj")
  expect_s3_class(data_explorer_app(), "shiny.appobj")
  ui <- mapUI("m")
  expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list"))
})

test_that("make_map_example_data leaves the session RNG stream untouched", {
  # Regression: a bare set.seed() here parked the session RNG at a state where
  # chromote drew the same blocked "random" port on every PNG export.
  set.seed(999)
  before <- .Random.seed
  invisible(make_map_example_data())
  expect_identical(.Random.seed, before)
})

# ---- size scales + size legend (v0.6.0) --------------------------------------

test_that("map_size_scale falls back exactly like map_color_scale", {
  expect_equal(map_size_scale(c(1, 10, 100), "log"), "log")
  expect_equal(map_size_scale(c(0, 10, 100), "log"), "linear")   # log needs > 0
  expect_equal(map_size_scale(c(-5, 10), "log"), "linear")
  expect_equal(map_size_scale(c(1, 1, 2), "quantile"), "linear") # too few distinct
  expect_equal(map_size_scale(1:5, "quantile"), "quantile")
  expect_equal(map_size_scale(1:5, "nonsense"), "linear")
  expect_equal(map_size_scale(c(NA_real_, NA_real_), "log"), "linear")
})

test_that("radii stay inside the range and increase with the value", {
  v <- c(1, 2, 5, 10, 50, 1000)
  for (s in c("linear", "log", "quantile")) {
    r <- scale_radius(v, MAP_RADIUS_RANGE, s)
    expect_true(all(r >= MAP_RADIUS_RANGE[1] & r <= MAP_RADIUS_RANGE[2]), info = s)
    expect_false(is.unsorted(r), info = s)                 # monotone in the value
  }
})

test_that("log sizing spreads skewed values more than linear does", {
  v   <- c(1, 2, 5, 10, 50, 1000)
  lin <- scale_radius(v, MAP_RADIUS_RANGE, "linear")
  lg  <- scale_radius(v, MAP_RADIUS_RANGE, "log")
  # The whole point: under linear the small end collapses together.
  expect_lt(diff(range(lin[1:5])), diff(range(lg[1:5])))
})

test_that("a constant column gives the midpoint radius on every scale", {
  for (s in c("linear", "log", "quantile"))
    expect_equal(unique(scale_radius(rep(7, 4), MAP_RADIUS_RANGE, s)),
                 mean(MAP_RADIUS_RANGE), info = s)
})

test_that("non-finite values fall to the smallest radius", {
  r <- scale_radius(c(1, NA, 10, Inf), MAP_RADIUS_RANGE, "linear")
  expect_equal(r[c(2, 4)], rep(MAP_RADIUS_RANGE[1], 2))
})

test_that("size_legend_breaks radii are computed by the same code path as markers", {
  v <- c(1, 2, 5, 10, 50, 1000)
  for (s in c("linear", "log", "quantile")) {
    b <- size_legend_breaks(v, s)
    expect_s3_class(b, "data.frame")
    expect_true(all(b$radius >= MAP_RADIUS_RANGE[1] &
                    b$radius <= MAP_RADIUS_RANGE[2]), info = s)
    # The legend must not be able to drift from the markers.
    expect_equal(b$radius,
                 scale_radius(b$value, MAP_RADIUS_RANGE, s, domain = v), info = s)
  }
})

test_that("size_legend_breaks declines to draw a key it can't justify", {
  expect_null(size_legend_breaks(rep(5, 10), "linear"))   # constant
  expect_null(size_legend_breaks(numeric(0), "linear"))
  expect_null(size_legend_breaks(c("a", "b"), "linear"))  # not numeric
})

test_that("size_legend_html renders one circle per break and escapes its title", {
  h <- size_legend_html(size_legend_breaks(c(1, 10, 100, 1000), "log"),
                        title = "acres <b>")
  expect_type(h, "character")
  expect_equal(lengths(regmatches(h, gregexpr("border-radius:50%", h))), 4L)
  expect_false(grepl("<b>", h, fixed = TRUE))            # escaped, not injected
  expect_null(size_legend_html(NULL))
})

test_that("the map builder honours size_scale and the legend toggle", {
  d  <- make_map_example_data()
  cc <- detect_coord_cols(d)
  base <- list(lon = cc$lon, lat = cc$lat, size_by = "acres")
  for (s in c("linear", "log", "quantile")) {
    m <- build_leaflet_map(d, c(base, list(size_scale = s)))
    expect_s3_class(m, "leaflet")
  }
  with_leg <- build_leaflet_map(d, c(base, list(size_legend = TRUE)))
  no_leg   <- build_leaflet_map(d, c(base, list(size_legend = FALSE)))
  expect_gt(length(with_leg$x$calls), length(no_leg$x$calls))
})

test_that("generated code reproduces the builder's radii exactly", {
  d  <- make_map_example_data()
  cc <- detect_coord_cols(d)
  skewed   <- within(d, acres <- c(rep(1, 60), rep(5, 40), rep(9000, 20)))
  constant <- within(d, acres <- rep(42, nrow(d)))
  for (df in list(d, skewed, constant)) {
    for (s in c("linear", "log", "quantile")) {
      p   <- list(lon = cc$lon, lat = cc$lat, size_by = "acres", size_scale = s)
      cd  <- clean_coords(df, cc$lon, cc$lat)$data
      app <- scale_radius(cd$acres, MAP_RADIUS_RANGE,
                          map_size_scale(cd$acres, s))
      env <- new.env(parent = globalenv()); assign("df", df, envir = env)
      expect_error(eval(parse(text = generate_map_code(df, p)), envir = env), NA)
      expect_equal(unname(get("df", envir = env)$.map_radius), unname(app),
                   info = s)
    }
  }
})

test_that("map_hint explains a size-scale fallback", {
  d  <- make_map_example_data(); cc <- detect_coord_cols(d)
  d0 <- d; d0$acres[1] <- 0
  h  <- map_hint(d0, list(lon = cc$lon, lat = cc$lat, size_by = "acres",
                          size_scale = "log"))
  expect_match(h, "log scale")
  # No hint when the scale is honoured.
  expect_false(grepl("log scale", map_hint(
    d, list(lon = cc$lon, lat = cc$lat, size_by = "acres",
            size_scale = "log")) %||% ""))
})

# ---- export framing ----------------------------------------------------------
# A snapshot is rendered on a fixed 1200x800 canvas, not the on-screen pane.
# centre+zoom means "this scale", so replaying it on a differently-sized canvas
# shows a different AREA -- the report map came back surrounded by empty ocean.
# view_bounds says "cover this area", which survives the canvas change.

test_that("view_bounds takes precedence over view", {
  d  <- make_map_example_data(); cc <- detect_coord_cols(d)
  bb <- list(north = 27.5, south = 25.2, east = -79.8, west = -82.6)
  m  <- build_leaflet_map(d, list(lon = cc$lon, lat = cc$lat,
                                  view = list(lng = 0, lat = 0, zoom = 2),
                                  view_bounds = bb))
  # leaflet stores these on the widget itself, not as entries in $calls.
  expect_null(m$x$setView)
  expect_equal(unname(unlist(m$x$fitBounds)),
               c(bb$south, bb$west, bb$north, bb$east))
})

test_that("view is still honoured for the live pane when no bounds are given", {
  d  <- make_map_example_data(); cc <- detect_coord_cols(d)
  m  <- build_leaflet_map(d, list(lon = cc$lon, lat = cc$lat,
                                  view = list(lng = -81, lat = 28, zoom = 7)))
  expect_equal(unname(unlist(m$x$setView)), c(28, -81, 7))
  expect_null(m$x$fitBounds)
})

test_that("an incomplete bounds list falls through instead of erroring", {
  d  <- make_map_example_data(); cc <- detect_coord_cols(d)
  m  <- build_leaflet_map(d, list(lon = cc$lon, lat = cc$lat,
                                  view_bounds = list(north = 27, south = 25)))
  expect_s3_class(m, "leaflet")
  expect_false(is.null(m$x$fitBounds))    # fell back to the data bbox
  # ...and it is the DATA's bbox, not the half-specified one.
  expect_false(identical(unname(unlist(m$x$fitBounds))[c(1, 3)], c(25, 27)))
})

test_that("view_bounds is never emitted into the generated code", {
  d  <- make_map_example_data(); cc <- detect_coord_cols(d)
  code <- generate_map_code(d, list(lon = cc$lon, lat = cc$lat,
                                    view_bounds = list(north = 27, south = 25,
                                                       east = -80, west = -82)))
  expect_false(grepl("view_bounds|fitBounds", code))
})

# ---- Phase 7: scale bar + density heatmap ------------------------------------

test_that("the scale bar is on by default and can be switched off", {
  d  <- make_map_example_data(); cc <- detect_coord_cols(d)
  methods_of <- function(m) vapply(m$x$calls, function(c) c$method, character(1))
  expect_true("addScaleBar" %in% methods_of(
    build_leaflet_map(d, list(lon = cc$lon, lat = cc$lat))))
  expect_false("addScaleBar" %in% methods_of(
    build_leaflet_map(d, list(lon = cc$lon, lat = cc$lat, scalebar = FALSE))))
})

test_that("the heatmap layer is added when asked and leaflet.extras exists", {
  skip_if_not_installed("leaflet.extras")
  d  <- make_map_example_data(); cc <- detect_coord_cols(d)
  m  <- build_leaflet_map(d, list(lon = cc$lon, lat = cc$lat,
                                  heatmap = TRUE, heat_by = "acres"))
  expect_true("addHeatmap" %in%
                vapply(m$x$calls, function(c) c$method, character(1)))
  # and not when unasked
  m0 <- build_leaflet_map(d, list(lon = cc$lon, lat = cc$lat))
  expect_false("addHeatmap" %in%
                 vapply(m0$x$calls, function(c) c$method, character(1)))
})

test_that("generated code emits the heatmap and scale bar to match the builder", {
  d  <- make_map_example_data(); cc <- detect_coord_cols(d)
  code <- generate_map_code(d, list(lon = cc$lon, lat = cc$lat,
                                    heatmap = TRUE, heat_by = "acres"))
  expect_match(code, "addHeatmap", fixed = TRUE)
  expect_match(code, "intensity = ~pmax(0, ifelse(is.finite(acres), acres, 0))",
               fixed = TRUE)
  expect_match(code, "addScaleBar", fixed = TRUE)
  expect_error(parse(text = code), NA)
  # scalebar off -> not emitted
  expect_false(grepl("addScaleBar", generate_map_code(
    d, list(lon = cc$lon, lat = cc$lat, scalebar = FALSE))))
})

# ---- Phase 8: choropleth -----------------------------------------------------

choro_gj <- function() {
  paste0('{"type":"FeatureCollection","features":[',
    '{"type":"Feature","properties":{"NAME":"Alpha"},"geometry":{"type":"Polygon",',
    '"coordinates":[[[-82,29],[-81,29],[-81,30],[-82,30],[-82,29]]]}},',
    '{"type":"Feature","properties":{"NAME":"Beta"},"geometry":{"type":"Polygon",',
    '"coordinates":[[[-81,29],[-80,29],[-80,30],[-81,30],[-81,29]]]}},',
    '{"type":"Feature","properties":{"NAME":"Gamma"},"geometry":{"type":"Polygon",',
    '"coordinates":[[[-82,30],[-81,30],[-81,31],[-82,31],[-82,30]]]}}]}')
}

test_that("parse_geojson accepts a FeatureCollection and rejects junk", {
  gj <- parse_geojson(choro_gj())
  expect_false(is.null(gj))
  expect_equal(geojson_props(gj), "NAME")
  expect_null(parse_geojson("not json"))
  expect_null(parse_geojson('{"type":"Feature"}'))
  expect_null(parse_geojson(NULL))
})

test_that("geojson_bounds covers all features", {
  b <- geojson_bounds(parse_geojson(choro_gj()))
  expect_equal(c(b$lng1, b$lat1, b$lng2, b$lat2), c(-82, 29, -80, 31))
})

test_that("build_leaflet_map routes to a choropleth and reports the join", {
  dat <- data.frame(county = c("Alpha", "Alpha", "Beta", "Delta"),
                    yield  = c(10, 20, 50, 99))
  m <- build_leaflet_map(dat, list(geojson = choro_gj(), region_key = "county",
                                   region_prop = "NAME", region_value = "yield"))
  expect_s3_class(m, "leaflet")
  methods <- vapply(m$x$calls, function(c) c$method, character(1))
  expect_true(all(c("addGeoJSON", "addLegend", "addScaleBar") %in% methods))
  diag <- attr(m, "choro_diag")
  expect_equal(diag$n_matched, 2L)                    # Alpha + Beta
  expect_equal(diag$unmatched_geo, "Gamma")           # region with no data
  expect_equal(diag$unmatched_data, "Delta")          # data with no region
})

test_that("the choropleth aggregates with the chosen summary", {
  dat <- data.frame(county = c("Alpha", "Alpha"), yield = c(10, 20))
  for (agg in c("mean", "sum", "median")) {
    m <- build_leaflet_map(dat, list(geojson = choro_gj(),
                                     region_key = "county", region_prop = "NAME",
                                     region_value = "yield", region_agg = agg))
    expect_s3_class(m, "leaflet")
  }
})

test_that("choropleth guards bad inputs", {
  dat <- data.frame(county = "Alpha", yield = 1)
  # non-numeric value column
  expect_null(build_leaflet_map(
    data.frame(county = "Alpha", yield = "x"),
    list(geojson = choro_gj(), region_key = "county", region_prop = "NAME",
         region_value = "yield")))
  # missing key column
  expect_null(build_leaflet_map(
    dat, list(geojson = choro_gj(), region_key = "nope", region_prop = "NAME",
              region_value = "yield")))
  # junk geojson
  expect_null(build_leaflet_map(
    dat, list(geojson = "junk", region_key = "county", region_prop = "NAME",
              region_value = "yield")))
})

test_that("choropleth code-gen parses and reads like the builder", {
  code <- generate_map_code(
    data.frame(county = "Alpha", yield = 1),
    list(geojson = choro_gj(), region_key = "county", region_prop = "NAME",
         region_value = "yield", region_agg = "sum"))
  expect_match(code, "addGeoJSON", fixed = TRUE)
  expect_match(code, "tapply(df$yield, as.character(df$county), sum", fixed = TRUE)
  expect_error(parse(text = code), NA)
})

# ---- adversarial-review fixes (v0.6.0 release hardening) --------------------

test_that("choro_palette falls back to linear on tied quantile breaks", {
  vals <- c(rep(0, 8), 1, 2, 3, 4)               # 5 distinct but tied quartiles
  cp <- choro_palette(vals, "auto", "quantile")
  expect_equal(cp$scale, "linear")
  expect_match(cp$pal(1), "^#")                   # and the pal actually works
  expect_equal(choro_palette(rep(5, 4))$scale, "constant")
})

test_that("hostile GeoJSON property shapes degrade to unmatched, never error", {
  gj_mk <- function(pl) sprintf(
    '{"type":"FeatureCollection","features":[{"type":"Feature","properties":%s,"geometry":{"type":"Polygon","coordinates":[[[-82,29],[-81,29],[-81,30],[-82,30],[-82,29]]]}}]}', pl)
  for (pl in c('{"NAME":["A","Alpha"]}', '{"NAME":[]}', '{"OTHER":"x"}'))
    expect_s3_class(build_leaflet_map(
      data.frame(county = "A", yield = 1),
      list(geojson = gj_mk(pl), region_key = "county", region_prop = "NAME",
           region_value = "yield")), "leaflet")
  expect_equal(geojson_prop_chr(list(NAME = list("A", "B")), "NAME"), "A")
  expect_equal(geojson_prop_chr(list(NAME = list()), "NAME"), "")
})

test_that("geojson_props unions keys across heterogeneous features", {
  gj <- parse_geojson(paste0(
    '{"type":"FeatureCollection","features":[',
    '{"type":"Feature","properties":{"A":1},"geometry":null},',
    '{"type":"Feature","properties":{"B":2},"geometry":null}]}'))
  expect_setequal(geojson_props(gj), c("A", "B"))
})

test_that("map_heat_weights sanitises hostile weight columns", {
  expect_false(map_heat_weights(c(NA_real_, NA_real_))$weighted)  # all-NA
  expect_false(map_heat_weights(c(-5, -1))$weighted)              # all-negative
  hw <- map_heat_weights(c(1, NA, 3, Inf))
  expect_true(hw$weighted)
  expect_equal(hw$intensity, c(1, 0, 3, 0))                        # NA/Inf -> 0
  expect_equal(hw$max, 3)
})

test_that("emitted heatmap code carries max= and matches the builder's weights", {
  d <- make_map_example_data(); cc <- detect_coord_cols(d)
  code <- generate_map_code(d, list(lon = cc$lon, lat = cc$lat,
                                    heatmap = TRUE, heat_by = "acres"))
  expect_match(code, "max = ", fixed = TRUE)
  expect_match(code, "pmax(0", fixed = TRUE)
})

test_that("choropleth code-gen honours scalebar/legend flags and quantile fallback", {
  dat <- data.frame(county = LETTERS[1:8], yield = c(0,0,0,0,0,1,2,3))
  gjm <- paste0('{"type":"FeatureCollection","features":[',
    paste(sprintf('{"type":"Feature","properties":{"NAME":"%s"},"geometry":{"type":"Polygon","coordinates":[[[%d,29],[%d,29],[%d,30],[%d,30],[%d,29]]]}}',
      LETTERS[1:8], -90:-83, -89:-82, -89:-82, -90:-83, -90:-83), collapse=","), "]}")
  base <- list(geojson = gjm, region_key = "county", region_prop = "NAME",
               region_value = "yield")
  c1 <- generate_map_code(dat, c(base, list(scalebar = FALSE, legend = FALSE)))
  expect_false(grepl("addScaleBar", c1)); expect_false(grepl("addLegend", c1))
  # tied values + quantile request -> emitted code must use the LINEAR pal too
  c2 <- generate_map_code(dat, c(base, list(color_scale = "quantile")))
  expect_match(c2, "colorNumeric", fixed = TRUE)
  expect_false(grepl("colorQuantile", c2))
  expect_error(parse(text = c2), NA)
})

# ---- show_points: heatmap-only view (0.8.0) -------------------------------

test_that("show_points = FALSE drops markers, layers control, and legends", {
  d <- make_map_example_data()
  p <- list(lon = "lon", lat = "lat", color = "yield", size_by = "acres",
            group_by = "county", show_points = FALSE, heatmap = FALSE)
  m <- build_leaflet_map(d, p)
  calls <- vapply(m$x$calls, function(cl) cl$method, character(1))
  expect_false("addCircleMarkers" %in% calls)
  expect_false("addLayersControl" %in% calls)
  expect_false("addLegend" %in% calls)
  # default / TRUE keeps markers exactly as before
  p$show_points <- TRUE
  calls2 <- vapply(build_leaflet_map(d, p)$x$calls,
                   function(cl) cl$method, character(1))
  expect_true("addCircleMarkers" %in% calls2)
  p$show_points <- NULL
  calls3 <- vapply(build_leaflet_map(d, p)$x$calls,
                   function(cl) cl$method, character(1))
  expect_true("addCircleMarkers" %in% calls3)
})

test_that("show_points = FALSE keeps the heatmap layer (heatmap-only view)", {
  skip_if_not_installed("leaflet.extras")
  d <- make_map_example_data()
  p <- list(lon = "lon", lat = "lat", show_points = FALSE, heatmap = TRUE)
  calls <- vapply(build_leaflet_map(d, p)$x$calls,
                  function(cl) cl$method, character(1))
  expect_true("addHeatmap" %in% calls)
  expect_false("addCircleMarkers" %in% calls)
})

test_that("generated code mirrors the hidden-markers map and stays parseable", {
  d <- make_map_example_data()
  p <- list(lon = "lon", lat = "lat", color = "yield", group_by = "county",
            show_points = FALSE, heatmap = TRUE, heat_radius = 18)
  code <- generate_map_code(d, p)
  expect_no_match(code, "addCircleMarkers", fixed = TRUE)
  expect_match(code, "addHeatmap", fixed = TRUE)
  expect_no_match(code, "addLegend", fixed = TRUE)
  expect_silent(parse(text = code))
})

test_that("map_hint warns when points are hidden and no heatmap is on", {
  d <- make_map_example_data()
  p <- list(lon = "lon", lat = "lat", show_points = FALSE, heatmap = FALSE)
  expect_match(map_hint(d, p), "only the basemap")
  p$heatmap <- TRUE
  hint <- map_hint(d, p)
  expect_true(is.null(hint) || !grepl("only the basemap", hint))
})

# ---- combine points by area (admin clustering, 0.8.0) ---------------------

test_that("aggregate_by_admin: centroids, counts, NA area, na.rm means", {
  d <- data.frame(county = c("B", "A", "A", NA),
                  lat = c(30, 28, 26, 25), lon = c(-82, -81, -83, -80),
                  yield = c(10, 4, NA, 7),
                  crop = c("corn", "soy", "soy", "corn"))
  out <- aggregate_by_admin(d, "county", "lon", "lat",
                            agg_cols = c("yield", "crop"))
  expect_equal(out$county, c("(missing)", "A", "B"))    # sorted, NA has a row
  expect_equal(out$.n_points, c(1L, 2L, 1L))
  expect_equal(out$lat[out$county == "A"], 27)          # centroid = mean
  expect_equal(out$yield[out$county == "A"], 4)         # na.rm mean
  expect_false("crop" %in% names(out))                  # non-numeric dropped
  allna <- aggregate_by_admin(
    data.frame(g = c("x", "x"), lat = c(1, 2), lon = c(3, 4),
               v = c(NA_real_, NA_real_)),
    "g", "lon", "lat", agg_cols = "v")
  expect_true(is.na(allna$v))                           # NA, never NaN
})

test_that("admin_cluster_params gates and rewrites like the design says", {
  d <- make_map_example_data()
  base <- list(lon = "lon", lat = "lat", cluster_by = "county",
               color = "yield", group_by = "crop", cluster = "auto",
               heatmap = TRUE, size_by = "acres")
  ac <- admin_cluster_params(d, base, "lon", "lat")
  expect_true(ac$active)
  expect_equal(nrow(ac$d), 12L)                         # one row per county
  expect_equal(ac$p$size_by, ".n_points")
  expect_equal(ac$p$group_by, "__none__")
  expect_equal(ac$p$cluster, "off")
  expect_false(ac$p$heatmap)
  expect_equal(ac$p$color, "yield")                     # numeric -> area mean
  expect_equal(ac$p$label_col, "county")
  expect_true("Points" %in% names(ac$d))
  # categorical color pick is ignored
  ac2 <- admin_cluster_params(d, utils::modifyList(base, list(color = "crop")),
                              "lon", "lat")
  expect_equal(ac2$p$color, "__none__")
  # gates: none / missing col / lon-lat / too many areas
  expect_false(admin_cluster_params(d, list(cluster_by = "__none__"),
                                    "lon", "lat")$active)
  expect_false(admin_cluster_params(d, list(cluster_by = "nope"),
                                    "lon", "lat")$active)
  expect_false(admin_cluster_params(d, list(cluster_by = "lat"),
                                    "lon", "lat")$active)
  d2 <- d; d2$id <- as.character(seq_len(nrow(d2)))     # 120 areas > 100
  expect_false(admin_cluster_params(d2, list(cluster_by = "id"),
                                    "lon", "lat")$active)
})

test_that("builder in admin mode: one marker per area, no cluster/heat/groups", {
  d <- make_map_example_data()
  p <- list(lon = "lon", lat = "lat", cluster_by = "county", color = "yield",
            group_by = "crop", cluster = "on", heatmap = TRUE)
  m <- build_leaflet_map(d, p)
  calls <- vapply(m$x$calls, function(cl) cl$method, character(1))
  expect_equal(sum(calls == "addCircleMarkers"), 1L)
  expect_false("addLayersControl" %in% calls)
  expect_false("addHeatmap" %in% calls)
  mk <- m$x$calls[[which(calls == "addCircleMarkers")]]$args
  expect_equal(length(mk[[1]]), 12L)                     # 12 county bubbles
  radii <- mk[[3]]
  expect_true(all(radii >= MAP_RADIUS_RANGE[1] & radii <= MAP_RADIUS_RANGE[2]))
  expect_true("addLegend" %in% calls)                    # area-mean shading
})

test_that("admin-mode generated code aggregates and stays parseable", {
  d <- make_map_example_data()
  p <- list(lon = "lon", lat = "lat", cluster_by = "county", color = "yield")
  code <- generate_map_code(d, p)
  expect_match(code, "tapply", fixed = TRUE)
  expect_match(code, ".n_points", fixed = TRUE)
  expect_match(code, "mean(x, na.rm = TRUE)", fixed = TRUE)
  expect_silent(parse(text = code))
  # and it RUNS: eval the script against the raw data, then compare the
  # aggregated frame it builds with aggregate_by_admin's
  env <- new.env()
  env$df <- d
  code_noleaflet <- sub("leaflet\\(df\\)[\\s\\S]*$", "df", code, perl = TRUE)
  got <- eval(parse(text = code_noleaflet), envir = env)
  ref <- aggregate_by_admin(clean_coords(d, "lon", "lat")$data,
                            "county", "lon", "lat", agg_cols = "yield")
  expect_equal(got$.n_points, ref$.n_points)
  expect_equal(got$county, ref$county)
  expect_equal(got$yield, ref$yield, tolerance = 1e-12)
})

test_that("map_hint explains an unusable combine column", {
  d <- make_map_example_data()
  d$id <- as.character(seq_len(nrow(d)))
  expect_match(map_hint(d, list(lon = "lon", lat = "lat", cluster_by = "id")),
               "too many to combine")
  d$one <- "same"
  expect_match(map_hint(d, list(lon = "lon", lat = "lat", cluster_by = "one")),
               "only one area")
})
