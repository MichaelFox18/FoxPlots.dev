# ============================================================
# helpers_map.R -- point-map building, coordinate handling & code gen
# ============================================================
# Pure logic for the Map stage: coordinate detection / validation, the leaflet
# widget builder, its code-generation twin, and the (impure) HTML / PNG export
# helpers at the bottom. Everything above the export section is Shiny-free and
# unit-tested (tests/testthat/test-map.R); mod_map.R is a thin (namespaced)
# wrapper over these.
#
# leaflet / htmlwidgets / htmltools are namespace-qualified throughout (no
# @import); UF_BLUE / UF_ORANGE come from components.R, and PALETTES,
# BREWER_MAX, uf_discrete(), okabe_ito(), bq(), qq() from helpers_plot.R
# (bq_each() from helpers_lmer.R).

SYM_MDASH <- intToUtf8(0x2014)   # em dash for user-facing hints (source stays ASCII)

MAP_LAT_RANGE <- c(-90, 90)      # valid latitudes, decimal degrees
# Longitudes allow the 0-360 convention too (e.g. datasets::quakes crosses the
# antimeridian near 180); leaflet renders >180 fine on its wrapped world.
MAP_LON_RANGE <- c(-180, 360)

# Key-free basemaps from leaflet's bundled provider list. The values ARE the
# provider strings (the builder and the generated code emit them verbatim);
# light-first because a neutral basemap keeps the data legible.
MAP_BASEMAPS <- c(
  "Light (CartoDB Positron)"      = "CartoDB.Positron",
  "Streets (OpenStreetMap)"       = "OpenStreetMap",
  "Satellite (Esri WorldImagery)" = "Esri.WorldImagery",
  "Terrain (Esri WorldTopoMap)"   = "Esri.WorldTopoMap")

MAP_CLUSTER_AUTO <- 500    # "auto" clustering kicks in above this many points
MAP_BIG_ROWS     <- 5000   # unclustered circles get sluggish past this -> hint
MAP_LEGEND_MAX   <- 30     # a discrete legend beyond this is unreadable (facet-cap analog)
MAP_RADIUS_RANGE <- c(4, 18)  # graduated-symbol radius bounds in px (sqrt scale)
MAP_POPUP_MAX    <- 5      # the module preselects at most this many popup columns
MAP_PNG_W        <- 1200   # fixed PNG snapshot size -- no extra inputs in v1
MAP_PNG_H        <- 800

# Lowercase name hints for coordinate auto-detection, most specific first
# ("long" covers datasets::quakes; "x"/"y" are last resorts, still range-checked).
COORD_LON_NAMES <- c("lon", "lng", "long", "longitude", "lon_dd",
                     "decimallongitude", "x")
COORD_LAT_NAMES <- c("lat", "latitude", "lat_dd", "decimallatitude", "y")

# "__none__" / "" / NULL -> NULL (the pickers' "unset" sentinels).
map_col <- function(x) {
  if (!is.null(x) && length(x) == 1L && nzchar(x) && !identical(x, "__none__")) x
  else NULL
}

# Vectorized validity predicates: finite and inside the plausible range.
is_valid_lat <- function(x) {
  if (!is.numeric(x)) return(rep(FALSE, length(x)))
  is.finite(x) & x >= MAP_LAT_RANGE[1] & x <= MAP_LAT_RANGE[2]
}
is_valid_lon <- function(x) {
  if (!is.numeric(x)) return(rep(FALSE, length(x)))
  is.finite(x) & x >= MAP_LON_RANGE[1] & x <= MAP_LON_RANGE[2]
}

#' Guess the coordinate columns of a data frame.
#'
#' Case-insensitive exact name match against the hint lists, confirmed by a
#' range check (every non-NA value must be a plausible coordinate). No
#' range-only guessing: any 0-90 numeric pair would false-positive.
#'
#' @param df A data frame.
#' @return list(lon =, lat =) of column names; either may be NULL, never equal.
#' @noRd
detect_coord_cols <- function(df) {
  stopifnot(is.data.frame(df))
  find <- function(hints, valid_fn, exclude = character(0)) {
    for (h in hints) {
      for (cand in setdiff(names(df)[tolower(names(df)) == h], exclude)) {
        v <- df[[cand]]
        if (is.numeric(v) && any(!is.na(v)) && all(valid_fn(v) | is.na(v)))
          return(cand)
      }
    }
    NULL
  }
  lat <- find(COORD_LAT_NAMES, is_valid_lat)
  lon <- find(COORD_LON_NAMES, is_valid_lon,
              exclude = if (is.null(lat)) character(0) else lat)
  list(lon = lon, lat = lat)
}

#' Drop rows whose coordinates can't be placed on a map.
#'
#' @param df A data frame.
#' @param lon,lat Names of the numeric coordinate columns.
#' @return list(data =, n_dropped =, n_total =).
#' @noRd
clean_coords <- function(df, lon, lat) {
  stopifnot(is.data.frame(df), is.character(lon), is.character(lat))
  missing <- setdiff(c(lon, lat), names(df))
  if (length(missing)) {
    stop("Columns not found in data: ", paste(missing, collapse = ", "))
  }
  if (!is.numeric(df[[lon]]) || !is.numeric(df[[lat]])) {
    stop("Coordinate columns must be numeric.")
  }
  keep <- is_valid_lon(df[[lon]]) & is_valid_lat(df[[lat]])
  list(data = df[keep, , drop = FALSE],
       n_dropped = sum(!keep), n_total = nrow(df))
}

# Padded bounding box for fitBounds(); a single point gets a fixed pad so the
# map doesn't zoom to street level (pad is a fraction of the extent).
map_bbox <- function(df, lon, lat, pad = 0.05) {
  ex <- function(r) {
    d <- diff(r)
    if (!is.finite(d) || d == 0) d <- 1
    r + c(-1, 1) * pad * d
  }
  rl <- ex(range(df[[lon]], na.rm = TRUE))
  rt <- ex(range(df[[lat]], na.rm = TRUE))
  list(lng1 = rl[1], lat1 = rt[1], lng2 = rl[2], lat2 = rt[2])
}

# Graduated-symbol radii: sqrt so marker AREA (not radius) tracks the value --
# linear radii visually exaggerate the large end. Constant column -> midpoint;
# NA / non-finite -> smallest.
scale_radius <- function(v, range = MAP_RADIUS_RANGE) {
  if (is.null(v) || !is.numeric(v)) return(NULL)
  fin <- is.finite(v)
  if (!any(fin)) return(rep(mean(range), length(v)))
  v0  <- v - min(v[fin])
  top <- max(v0[fin])
  if (top == 0) return(rep(mean(range), length(v)))
  r <- range[1] + (range[2] - range[1]) * sqrt(v0 / top)
  r[!fin] <- range[1]
  r
}

# The palette decision tree of group_scales()/palette_code() (helpers_plot.R),
# translated to leaflet inputs: returns either a palette NAME ("viridis",
# "Set1", ...) or a vector of hex colors -- both are valid for colorNumeric /
# colorFactor, and the code generator emits whichever form comes back.
map_palette_colors <- function(palette, is_cont, n) {
  palette <- palette %||% "auto"
  if (palette == "auto") {
    if (is_cont)        return("viridis")
    if (n > BREWER_MAX) return("viridis")
    return("Set1")
  }
  if (is_cont) {
    if (palette == "uf") return(c(UF_BLUE, UF_ORANGE))
    return("viridis")
  }
  if (palette %in% c("set1", "set2") && n > BREWER_MAX) return("viridis")
  switch(palette,
    uf      = uf_discrete(n),
    viridis = "viridis",
    cb      = okabe_ito(n),
    set1    = "Set1",
    set2    = "Set2",
    greys   = grDevices::gray(seq(0.2, 0.75, length.out = max(n, 2))),
    "Set1")
}

# Wrap map_palette_colors() in a leaflet pal function. A numeric column with
# <= 10 distinct values is treated as categorical (build_full_plot's coercion
# rule), keeping numeric level order.
map_palette <- function(df, cv, palette = "auto") {
  v       <- df[[cv]]
  n       <- dplyr::n_distinct(v)   # one pass; callers reuse it via $n
  is_cont <- is.numeric(v) && n > 10
  cols    <- map_palette_colors(palette, is_cont, n)
  if (is_cont) {
    pal <- leaflet::colorNumeric(cols, domain = range(v, na.rm = TRUE),
                                 na.color = "#bbbbbb")
    list(pal = pal, kind = "numeric", values = v, n = n)
  } else {
    vf <- if (is.numeric(v)) factor(v, levels = sort(unique(v[!is.na(v)])))
          else as.factor(v)
    pal <- leaflet::colorFactor(cols, domain = levels(vf),
                                na.color = "#bbbbbb")
    list(pal = pal, kind = "factor", values = vf, n = n)
  }
}

# One HTML popup string per row: "<b>col:</b> value" lines. Everything is
# escaped -- popups render raw HTML, and column names / values come from user
# files. NULL when no columns are chosen.
popup_html <- function(df, cols) {
  cols <- intersect(cols %||% character(0), names(df))
  if (!length(cols)) return(NULL)
  fmt <- function(x) {
    out <- if (is.numeric(x)) format(round(x, 3), trim = TRUE, big.mark = ",")
           else as.character(x)
    out[is.na(x)] <- ""
    htmltools::htmlEscape(out)
  }
  parts <- lapply(cols, function(cn)
    sprintf("<b>%s:</b> %s", htmltools::htmlEscape(cn), fmt(df[[cn]])))
  do.call(paste, c(parts, sep = "<br/>"))
}

# Advisory shown under the pickers (chart_hint analog). NULL when all is well.
map_hint <- function(df, p) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(NULL)
  lon <- map_col(p$lon); lat <- map_col(p$lat)
  if (is.null(lon) || is.null(lat)) return(NULL)
  if (identical(lon, lat))
    return(paste0("Longitude and latitude are the same column ", SYM_MDASH,
                  " pick two different columns."))
  if (!lon %in% names(df) || !lat %in% names(df)) return(NULL)
  if (!is.numeric(df[[lon]]) || !is.numeric(df[[lat]]))
    return(paste0("Coordinate columns must be numeric ", SYM_MDASH,
                  " check the chosen longitude / latitude columns."))

  # Swapped pickers: the "latitude" fails its range but would pass as
  # longitude, while the "longitude" would pass as latitude.
  lat_v <- df[[lat]]; lon_v <- df[[lon]]
  lat_bad <- !all(is_valid_lat(lat_v) | is.na(lat_v))
  if (lat_bad && all(is_valid_lon(lat_v) | is.na(lat_v)) &&
      all(is_valid_lat(lon_v) | is.na(lon_v)))
    return(paste0("These columns look swapped (latitude values outside -90..90) ",
                  SYM_MDASH, " try the Swap link."))

  cc <- clean_coords(df, lon, lat)
  if (!nrow(cc$data))
    return(paste0("No rows have usable coordinates ", SYM_MDASH,
                  " values must be decimal degrees (e.g. 29.65, -82.32)."))

  hints <- character(0)
  if (cc$n_dropped > 0)
    hints <- c(hints, sprintf(
      "%s of %s rows have missing or out-of-range coordinates and are not shown.",
      format(cc$n_dropped, big.mark = ","), format(cc$n_total, big.mark = ",")))
  cv <- map_col(p$color)
  if (!is.null(cv) && cv %in% names(df)) {
    n_lv <- dplyr::n_distinct(df[[cv]])
    if (!(is.numeric(df[[cv]]) && n_lv > 10) && n_lv > MAP_LEGEND_MAX)
      hints <- c(hints, sprintf(
        "'%s' has %d categories %s too many for a readable legend, so the legend is hidden.",
        cv, n_lv, SYM_MDASH))
  }
  sv <- map_col(p$size_by)
  if (!is.null(sv) && sv %in% names(df) && !is.numeric(df[[sv]]))
    hints <- c(hints, sprintf("'%s' isn't numeric %s sizing by it is ignored.",
                              sv, SYM_MDASH))
  if (identical(p$cluster %||% "auto", "off") && nrow(cc$data) > MAP_BIG_ROWS)
    hints <- c(hints, sprintf(
      "%s points without clustering can be slow %s consider setting Cluster to Auto.",
      format(nrow(cc$data), big.mark = ","), SYM_MDASH))
  if (length(hints)) paste(hints, collapse = " ") else NULL
}

# --- Map builder --------------------------------------------------------------
# p is a plain list of settings (the build_full_plot contract, map edition):
#   lon, lat   : coordinate column names (required)
#   basemap    : a MAP_BASEMAPS value                  (default CartoDB.Positron)
#   color      : column to color markers by; "__none__"/NULL -> single color
#   palette    : a PALETTES value                      (default "auto")
#   color_hex  : marker color when `color` is unset    (default UF_BLUE)
#   size_by    : numeric column -> graduated radii; "__none__"/NULL -> fixed
#   size       : fixed circle radius in px             (default 6)
#   alpha      : fill opacity                          (default 0.8)
#   popup_cols : character vector of click-popup columns (default none)
#   label_col  : hover-label column                    (default none)
#   cluster    : "auto" / "on" / "off"                 (default "auto")
#   legend     : draw the legend when `color` is set   (default TRUE)
#   title      : optional map title                    (default none)
#   view       : module-only: list(lng, lat, zoom) restoring the user's view;
#                NULL -> fitBounds to the data. Never emitted in code.
build_leaflet_map <- function(df, p) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(NULL)
  lon <- map_col(p$lon); lat <- map_col(p$lat)
  if (is.null(lon) || is.null(lat) || identical(lon, lat)) return(NULL)
  if (!lon %in% names(df) || !lat %in% names(df)) return(NULL)
  if (!is.numeric(df[[lon]]) || !is.numeric(df[[lat]])) return(NULL)

  cc <- clean_coords(df, lon, lat)
  d  <- cc$data
  if (!nrow(d)) return(NULL)

  cv <- map_col(p$color)
  if (!is.null(cv) && (!cv %in% names(d) || all(is.na(d[[cv]])))) cv <- NULL
  sizev <- map_col(p$size_by)
  if (!is.null(sizev) && (!sizev %in% names(d) || !is.numeric(d[[sizev]])))
    sizev <- NULL
  labv <- map_col(p$label_col)
  if (!is.null(labv) && !labv %in% names(d)) labv <- NULL
  popc  <- intersect(p$popup_cols %||% character(0), names(d))
  size  <- p[["size"]] %||% 6   # [[ ]] not $ -- $ would partial-match p$size_by
  alpha <- p$alpha %||% 0.8
  basemap <- p$basemap %||% "CartoDB.Positron"
  cluster_on <- switch(p$cluster %||% "auto",
                       on = TRUE, off = FALSE, nrow(d) > MAP_CLUSTER_AUTO)

  pal_info <- if (!is.null(cv)) map_palette(d, cv, p$palette %||% "auto")

  m <- leaflet::leaflet(d)
  m <- leaflet::addProviderTiles(m, basemap)
  m <- leaflet::addCircleMarkers(
    m, lng = d[[lon]], lat = d[[lat]],
    radius      = if (!is.null(sizev)) scale_radius(d[[sizev]]) else size,
    stroke      = TRUE, weight = 1, color = "#FFFFFF", opacity = 0.9,
    fillColor   = if (!is.null(pal_info)) pal_info$pal(pal_info$values)
                  else (p$color_hex %||% UF_BLUE),
    fillOpacity = alpha,
    popup       = popup_html(d, popc),
    label       = if (!is.null(labv)) as.character(d[[labv]]),
    clusterOptions = if (cluster_on) leaflet::markerClusterOptions())

  show_legend <- !is.null(pal_info) && isTRUE(p$legend %||% TRUE) &&
    (pal_info$kind == "numeric" || pal_info$n <= MAP_LEGEND_MAX)
  if (show_legend)
    m <- leaflet::addLegend(m, position = "bottomright", pal = pal_info$pal,
                            values = pal_info$values, title = cv, opacity = 0.9)

  ttl <- if (!is.null(p$title) && nzchar(trimws(p$title))) trimws(p$title)
  if (!is.null(ttl))
    m <- leaflet::addControl(
      m, position = "topright",
      html = sprintf(
        "<div style='font-weight:600; font-size:1.1em; padding:2px 6px;'>%s</div>",
        htmltools::htmlEscape(ttl)))

  if (!is.null(p$view) && all(c("lng", "lat", "zoom") %in% names(p$view))) {
    m <- leaflet::setView(m, lng = p$view$lng, lat = p$view$lat,
                          zoom = p$view$zoom)
  } else {
    bb <- map_bbox(d, lon, lat)
    m <- leaflet::fitBounds(m, bb$lng1, bb$lat1, bb$lng2, bb$lat2)
  }
  m
}

# --- Code generation ----------------------------------------------------------
# Twin of build_leaflet_map(): emits standalone, parse()-clean leaflet code that
# reproduces the current map (minus the saved view -- leaflet auto-fits).

# c("#..." hex vector) -> code literal; palette names stay quoted strings.
map_palette_code <- function(cols) {
  if (all(startsWith(cols, "#")))
    sprintf("c(%s)", paste(vapply(cols, qq, character(1)), collapse = ", "))
  else qq(cols[1])
}

assemble_map_code <- function(pre, code) {
  head_lines <- c(
    "library(leaflet)",
    "",
    "# Replace `df` with your own data frame, e.g.:",
    '# df <- read.csv("your_data.csv")',
    ""
  )
  if (length(pre)) pre <- c(pre, "")
  paste(c(head_lines, pre, code), collapse = "\n")
}

generate_map_code <- function(df, p) {
  if (is.null(df) || !is.data.frame(df))
    return("# Import data to generate map code.")
  lon <- map_col(p$lon); lat <- map_col(p$lat)
  if (is.null(lon) || is.null(lat) || identical(lon, lat) ||
      !lon %in% names(df) || !lat %in% names(df) ||
      !is.numeric(df[[lon]]) || !is.numeric(df[[lat]]))
    return("# Choose the longitude and latitude columns to generate map code.")

  cc <- clean_coords(df, lon, lat)
  d0 <- cc$data   # guards evaluate on the CLEANED rows, matching the builder
  cv <- map_col(p$color)
  if (!is.null(cv) && (!cv %in% names(d0) || all(is.na(d0[[cv]])))) cv <- NULL
  sizev <- map_col(p$size_by)
  if (!is.null(sizev) && (!sizev %in% names(d0) || !is.numeric(d0[[sizev]])))
    sizev <- NULL
  labv <- map_col(p$label_col)
  if (!is.null(labv) && !labv %in% names(d0)) labv <- NULL
  popc  <- intersect(p$popup_cols %||% character(0), names(d0))
  size  <- p[["size"]] %||% 6
  alpha <- round(p$alpha %||% 0.8, 2)
  basemap <- p$basemap %||% "CartoDB.Positron"
  cluster_on <- switch(p$cluster %||% "auto",
                       on = TRUE, off = FALSE, nrow(cc$data) > MAP_CLUSTER_AUTO)

  dollar <- function(cn) paste0("df$", bq(cn))
  pre <- c(
    "# Keep rows with usable coordinates (decimal degrees)",
    sprintf("ok <- is.finite(%s) & is.finite(%s) &", dollar(lon), dollar(lat)),
    sprintf("      %s >= -90 & %s <= 90 & %s >= -180 & %s <= 360",
            dollar(lat), dollar(lat), dollar(lon), dollar(lon)),
    "df <- df[ok, ]")

  show_legend <- FALSE
  if (!is.null(cv)) {
    n_lv    <- dplyr::n_distinct(d0[[cv]])
    is_cont <- is.numeric(d0[[cv]]) && n_lv > 10
    cols    <- map_palette_colors(p$palette %||% "auto", is_cont, n_lv)
    pre <- c(pre, "", sprintf(
      "pal <- %s(%s, domain = %s)",
      if (is_cont) "colorNumeric" else "colorFactor",
      map_palette_code(cols), dollar(cv)))
    show_legend <- isTRUE(p$legend %||% TRUE) &&
      (is_cont || n_lv <= MAP_LEGEND_MAX)
  }
  if (length(popc)) {
    esc_name <- function(cn) htmltools::htmlEscape(cn)
    parts <- vapply(popc, function(cn)
      sprintf('"<b>%s:</b> ", %s', esc_name(cn), dollar(cn)), character(1))
    pre <- c(pre, "", paste0(
      "popup <- paste0(",
      paste(parts, collapse = ', "<br/>",\n                '), ")"))
  }
  if (!is.null(sizev)) {
    pre <- c(pre, "",
      "# Bubble radii: sqrt scaling so marker AREA tracks the value",
      sprintf("v <- %s - min(%s, na.rm = TRUE)", dollar(sizev), dollar(sizev)),
      sprintf("radius <- %s + %s * sqrt(v / max(v, na.rm = TRUE))",
              MAP_RADIUS_RANGE[1], MAP_RADIUS_RANGE[2] - MAP_RADIUS_RANGE[1]),
      sprintf("radius[!is.finite(radius)] <- %s", MAP_RADIUS_RANGE[1]))
  }

  marker_args <- c(
    sprintf("lng = ~%s, lat = ~%s", bq(lon), bq(lat)),
    if (!is.null(sizev)) "radius = radius" else sprintf("radius = %s", size),
    'stroke = TRUE, weight = 1, color = "#FFFFFF"',
    if (!is.null(cv)) sprintf("fillColor = ~pal(%s)", bq(cv))
    else               sprintf("fillColor = %s", qq(p$color_hex %||% UF_BLUE)),
    sprintf("fillOpacity = %s", alpha),
    if (length(popc)) "popup = popup",
    if (!is.null(labv)) sprintf("label = ~as.character(%s)", bq(labv)),
    if (cluster_on) "clusterOptions = markerClusterOptions()")

  lines <- c(
    "leaflet(df)",
    sprintf('addProviderTiles("%s")', basemap),
    sprintf("addCircleMarkers(%s)",
            paste(marker_args, collapse = ",\n                   ")))
  if (show_legend)
    lines <- c(lines, sprintf(
      'addLegend(position = "bottomright", pal = pal, values = ~%s, title = %s)',
      bq(cv), qq(cv)))
  ttl <- if (!is.null(p$title) && nzchar(trimws(p$title))) trimws(p$title)
  if (!is.null(ttl))
    lines <- c(lines, sprintf(
      'addControl(%s, position = "topright")',
      qq(sprintf("<b>%s</b>", htmltools::htmlEscape(ttl)))))

  code <- paste0(lines[1], " |>\n  ",
                 paste(lines[-1], collapse = " |>\n  "))
  assemble_map_code(pre, code)
}

# --- Example data ---------------------------------------------------------

#' Example point data for the Map Tool
#'
#' A small synthetic dataset of Florida field-research sites for trying the
#' Map Tool without importing anything: 120 sites spread across 12 Florida
#' counties, each with a crop, a season yield, and the site's acreage, plus
#' latitude / longitude columns the map auto-detects. Deterministic (fixed
#' seed), so examples and screenshots are reproducible.
#'
#' @return A data frame with columns `site`, `county`, `crop`, `yield`,
#'   `acres`, `lat`, and `lon`.
#' @examples
#' head(make_map_example_data())
#' @export
make_map_example_data <- function() {
  centroids <- data.frame(
    county = c("Alachua", "Marion", "Polk", "Hillsborough", "Manatee",
               "Hendry", "Palm Beach", "Miami-Dade", "Suwannee", "Jackson",
               "St. Johns", "Highlands"),
    lat = c(29.68, 29.21, 27.95, 27.91, 27.48, 26.55, 26.65, 25.61,
            30.19, 30.80, 29.91, 27.34),
    lon = c(-82.36, -82.06, -81.69, -82.35, -82.37, -81.17, -80.44, -80.50,
            -82.99, -85.21, -81.41, -81.34))
  set.seed(42)
  idx   <- rep(seq_len(nrow(centroids)), each = 10L)
  crops <- c("Citrus", "Tomato", "Strawberry", "Peanut", "Sweet corn")
  base  <- c(Citrus = 38, Tomato = 32, Strawberry = 24, Peanut = 20,
             "Sweet corn" = 16)
  crop  <- sample(crops, length(idx), replace = TRUE)
  data.frame(
    site   = sprintf("Site %03d", seq_along(idx)),
    county = centroids$county[idx],
    crop   = crop,
    yield  = round(base[crop] + stats::rnorm(length(idx), 0, 4), 1),
    acres  = round(stats::runif(length(idx), 2, 120), 1),
    lat    = round(centroids$lat[idx] + stats::rnorm(length(idx), 0, 0.12), 5),
    lon    = round(centroids$lon[idx] + stats::rnorm(length(idx), 0, 0.12), 5),
    row.names = NULL)
}

# --- Map export (impure: filesystem / external tools) -----------------------
# Smoke-tested, not unit-tested: they need pandoc or a headless Chrome. The
# module turns their stop() messages into notifications.

# saveWidget(selfcontained = TRUE) shells out to pandoc; mirror htmlwidgets'
# lookup (PATH, then RStudio's bundled copy via RSTUDIO_PANDOC).
pandoc_ok <- function() {
  if (nzchar(Sys.which("pandoc"))) return(TRUE)
  rs <- Sys.getenv("RSTUDIO_PANDOC")
  nzchar(rs) && (file.exists(file.path(rs, "pandoc.exe")) ||
                 file.exists(file.path(rs, "pandoc")))
}

# Save an interactive map: one self-contained .html when pandoc is available,
# otherwise a .zip of the page plus its dependency folder (CRAN 'zip' package,
# no external zip binary needed).
save_map_html <- function(widget, file) {
  if (pandoc_ok()) {
    htmlwidgets::saveWidget(widget, file, selfcontained = TRUE, title = "Map")
    return(invisible(file))
  }
  if (!requireNamespace("zip", quietly = TRUE)) {
    stop("Saving a single-file map needs pandoc (bundled with RStudio) or ",
         "the 'zip' package. install.packages(\"zip\")")
  }
  tmp <- file.path(tempfile("map"), "map.html")
  dir.create(dirname(tmp), recursive = TRUE)
  on.exit(unlink(dirname(tmp), recursive = TRUE), add = TRUE)
  htmlwidgets::saveWidget(widget, tmp, selfcontained = FALSE, title = "Map")
  zip::zipr(file, files = list.files(dirname(tmp), full.names = TRUE))
  invisible(file)
}

# Can we take PNG snapshots? Needs webshot2 + chromote AND a real Chromium
# browser. find_chrome() doesn't know about Microsoft Edge (Chromium-based,
# present on nearly every Windows machine), so fall back to pointing
# CHROMOTE_CHROME at it -- find_chrome() honours that variable from then on.
map_snapshot_ok <- function() {
  if (!requireNamespace("webshot2", quietly = TRUE) ||
      !requireNamespace("chromote", quietly = TRUE)) return(FALSE)
  found <- !is.null(tryCatch(suppressMessages(chromote::find_chrome()),
                             error = function(e) NULL,
                             warning = function(w) NULL))
  if (!found && !nzchar(Sys.getenv("CHROMOTE_CHROME"))) {
    edge <- c(file.path(Sys.getenv("ProgramFiles"),
                        "Microsoft", "Edge", "Application", "msedge.exe"),
              file.path(Sys.getenv("ProgramFiles(x86)"),
                        "Microsoft", "Edge", "Application", "msedge.exe"))
    edge <- edge[file.exists(edge)]
    if (length(edge)) {
      Sys.setenv(CHROMOTE_CHROME = edge[[1]])
      found <- TRUE
    }
  }
  found
}

# Rasterize the map via headless Chrome; `delay` gives basemap tiles a moment
# to load before the shot.
save_map_png <- function(widget, file, width = MAP_PNG_W, height = MAP_PNG_H,
                         delay = 1) {
  if (!map_snapshot_ok()) {
    stop("PNG snapshots need the optional 'webshot2' package and a ",
         "Chrome or Edge browser on this computer.")
  }
  tmp <- file.path(tempfile("mapshot"), "map.html")
  dir.create(dirname(tmp), recursive = TRUE)
  on.exit(unlink(dirname(tmp), recursive = TRUE), add = TRUE)
  htmlwidgets::saveWidget(widget, tmp, selfcontained = FALSE, title = "Map")
  webshot2::webshot(tmp, file = file, vwidth = width, vheight = height,
                    delay = delay)
  invisible(file)
}
