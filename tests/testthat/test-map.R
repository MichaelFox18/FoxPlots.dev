# Tests for the pure map logic in helpers_map.R: coordinate detection and
# cleaning, radius / palette scaling, popups, hints, the leaflet widget
# builder (structure assertions on the htmlwidget), and the code generator.
# leaflet is in Imports, so widget-building tests run un-gated; the impure
# exporters get light checks only (pandoc / Chrome are machine-dependent).

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
  expect_true(grepl("radius = radius", generate_map_code(df, specs$bubbles),
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

# ---- exporters (light: external tools are machine-dependent) --------------------

test_that("export capability probes return logical scalars", {
  expect_type(pandoc_ok(), "logical")
  expect_length(pandoc_ok(), 1L)
  expect_type(map_snapshot_ok(), "logical")
  expect_length(map_snapshot_ok(), 1L)
})

test_that("save_map_html writes a non-empty file (html or zip fallback)", {
  skip_if_not(pandoc_ok() || requireNamespace("zip", quietly = TRUE))
  w <- build_leaflet_map(data.frame(lon = c(-82, -81), lat = c(29, 28)),
                         list(lon = "lon", lat = "lat"))
  out <- withr::local_tempfile(fileext = if (pandoc_ok()) ".html" else ".zip")
  save_map_html(w, out)
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0)
})

# ---- app objects (smoke; no testServer -- it segfaults on this machine) ---------

test_that("the Map Tool and Data Explorer app objects build", {
  expect_s3_class(map_tool_app(), "shiny.appobj")
  expect_s3_class(data_explorer_app(), "shiny.appobj")
  ui <- mapUI("m")
  expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list"))
})
