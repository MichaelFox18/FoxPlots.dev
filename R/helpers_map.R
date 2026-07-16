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
MAP_GROUP_MAX    <- 12     # layer-control checkboxes beyond this are unusable
MAP_QUANT_BINS   <- 5      # quantile color scale bin count

# Color scales for a CONTINUOUS color column. Log spreads out skewed values
# (regional totals, populations); quantile guarantees every bin gets ink.
MAP_SCALES <- c("Linear" = "linear", "Log" = "log",
                "Quantile (5 bins)" = "quantile")
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

# Layer-group labels: character values with NA as its own "(missing)" group.
# The SINGLE definition of what counts as a group -- builder, code-gen, hint,
# module picker and focus-zoom must all agree or grouping silently vanishes.
map_group_vals <- function(x) {
  g <- as.character(x)
  g[is.na(g)] <- "(missing)"
  g
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

# Effective color scale for a continuous color column: the requested one
# unless the data can't support it (log needs all-positive values; quantile
# needs enough distinct values for unique bin breaks). The single source of
# truth for the fallback -- builder, code-gen and map_hint all consult it.
map_color_scale <- function(v, requested) {
  requested <- requested %||% "linear"
  if (!requested %in% c("log", "quantile")) return("linear")
  if (requested == "log" && any(v <= 0, na.rm = TRUE)) return("linear")
  if (requested == "quantile") {
    qs <- stats::quantile(v, probs = seq(0, 1, 1 / MAP_QUANT_BINS),
                          na.rm = TRUE)
    if (length(unique(qs)) < MAP_QUANT_BINS + 1) return("linear")
  }
  requested
}

# Wrap map_palette_colors() in a leaflet pal function. A numeric column with
# <= 10 distinct values is treated as categorical (build_full_plot's coercion
# rule), keeping numeric level order. For a continuous column, `color_scale`
# picks linear / log / quantile coloring; $values are what the pal function
# expects (log10-transformed under "log" -- the builder's legend converts the
# labels back).
map_palette <- function(df, cv, palette = "auto", color_scale = "linear") {
  v       <- df[[cv]]
  n       <- dplyr::n_distinct(v)   # one pass; callers reuse it via $n
  is_cont <- is.numeric(v) && n > 10
  cols    <- map_palette_colors(palette, is_cont, n)
  if (is_cont) {
    scale <- map_color_scale(v, color_scale)
    if (scale == "log") {
      lv  <- log10(v)
      pal <- leaflet::colorNumeric(cols, domain = range(lv, na.rm = TRUE),
                                   na.color = "#bbbbbb")
      list(pal = pal, kind = "numeric", values = lv, n = n, scale = "log")
    } else if (scale == "quantile") {
      pal <- leaflet::colorQuantile(cols, domain = v, n = MAP_QUANT_BINS,
                                    na.color = "#bbbbbb")
      list(pal = pal, kind = "numeric", values = v, n = n, scale = "quantile")
    } else {
      pal <- leaflet::colorNumeric(cols, domain = range(v, na.rm = TRUE),
                                   na.color = "#bbbbbb")
      list(pal = pal, kind = "numeric", values = v, n = n, scale = "linear")
    }
  } else {
    vf <- if (is.numeric(v)) factor(v, levels = sort(unique(v[!is.na(v)])))
          else as.factor(v)
    pal <- leaflet::colorFactor(cols, domain = levels(vf),
                                na.color = "#bbbbbb")
    list(pal = pal, kind = "factor", values = vf, n = n, scale = "linear")
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
    n_lv    <- dplyr::n_distinct(df[[cv]])   # computed once for both checks
    is_cont <- is.numeric(df[[cv]]) && n_lv > 10
    if (!is_cont && n_lv > MAP_LEGEND_MAX)
      hints <- c(hints, sprintf(
        "'%s' has %d categories %s too many for a readable legend, so the legend is hidden.",
        cv, n_lv, SYM_MDASH))
    # Requested color scale the data can't support -> silent linear fallback,
    # loud hint (same map_color_scale rule the builder uses).
    req_scale <- p$color_scale %||% "linear"
    if (is_cont && req_scale %in% c("log", "quantile") &&
        map_color_scale(df[[cv]], req_scale) == "linear")
      hints <- c(hints, if (req_scale == "log") sprintf(
        "A log color scale needs values above 0; '%s' has zeros or negatives %s using a linear scale.",
        cv, SYM_MDASH) else sprintf(
        "'%s' doesn't have enough distinct values for %d quantile bins %s using a linear scale.",
        cv, MAP_QUANT_BINS, SYM_MDASH))
  }
  sv <- map_col(p$size_by)
  if (!is.null(sv) && sv %in% names(df) && !is.numeric(df[[sv]]))
    hints <- c(hints, sprintf("'%s' isn't numeric %s sizing by it is ignored.",
                              sv, SYM_MDASH))
  if (identical(p$cluster %||% "auto", "off") && nrow(cc$data) > MAP_BIG_ROWS)
    hints <- c(hints, sprintf(
      "%s points without clustering can be slow %s consider setting Cluster to Auto.",
      format(nrow(cc$data), big.mark = ","), SYM_MDASH))
  gv <- map_col(p$group_by)
  if (!is.null(gv) && gv %in% names(df)) {
    # Same rows AND same "(missing)"-inclusive count the builder gates on.
    n_grp <- dplyr::n_distinct(map_group_vals(cc$data[[gv]]))
    if (n_grp > MAP_GROUP_MAX)
      hints <- c(hints, sprintf(
        "'%s' has %d groups %s too many for a layer list (max %d), so grouping is ignored.",
        gv, n_grp, SYM_MDASH, MAP_GROUP_MAX))
  }
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
#   color_scale: "linear" / "log" / "quantile" for a CONTINUOUS color
#                (default "linear"; silent fallback per map_color_scale)
#   group_by   : column -> one toggleable layer per level (max MAP_GROUP_MAX
#                levels), clustered per group; auto-colors by it when `color`
#                is unset ("__none__"/NULL -> no grouping)
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

  gv <- map_col(p$group_by)
  if (!is.null(gv) && (!gv %in% names(d) || all(is.na(d[[gv]])) ||
                       dplyr::n_distinct(map_group_vals(d[[gv]])) > MAP_GROUP_MAX))
    gv <- NULL
  cv <- map_col(p$color)
  if (!is.null(cv) && (!cv %in% names(d) || all(is.na(d[[cv]])))) cv <- NULL
  # Grouped layers with no color pick would all look alike -- color by the
  # group so the layers (and legend) are visually distinct, ArcGIS-style.
  if (!is.null(gv) && is.null(cv)) cv <- gv
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

  pal_info <- if (!is.null(cv))
    map_palette(d, cv, p$palette %||% "auto", p$color_scale %||% "linear")

  # Per-row aesthetics computed ONCE on the full cleaned data, then subset per
  # group -- radii and colors must be comparable across layers. Scalars stay
  # scalar (fixed size / single color) and are passed through unsubset.
  fill   <- if (!is.null(pal_info)) pal_info$pal(pal_info$values)
            else (p$color_hex %||% UF_BLUE)
  radius <- if (!is.null(sizev)) scale_radius(d[[sizev]]) else size
  popup  <- popup_html(d, popc)
  labs   <- if (!is.null(labv)) as.character(d[[labv]])
  sub_i  <- function(x, i) if (length(x) > 1) x[i] else x

  m <- leaflet::leaflet(d)
  m <- leaflet::addProviderTiles(m, basemap)
  if (is.null(gv)) {
    m <- leaflet::addCircleMarkers(
      m, lng = d[[lon]], lat = d[[lat]],
      radius      = radius,
      stroke      = TRUE, weight = 1, color = "#FFFFFF", opacity = 0.9,
      fillColor   = fill,
      fillOpacity = alpha,
      popup       = popup,
      label       = labs,
      clusterOptions = if (cluster_on) leaflet::markerClusterOptions())
  } else {
    # One overlay layer per group level, clustered WITHIN the group (so a
    # cluster never mixes groups), plus a checkbox layer list to toggle them.
    gvals  <- map_group_vals(d[[gv]])
    groups <- sort(unique(gvals))
    for (g in groups) {
      i <- which(gvals == g)
      m <- leaflet::addCircleMarkers(
        m, lng = d[[lon]][i], lat = d[[lat]][i],
        radius      = sub_i(radius, i),
        stroke      = TRUE, weight = 1, color = "#FFFFFF", opacity = 0.9,
        fillColor   = sub_i(fill, i),
        fillOpacity = alpha,
        popup       = if (!is.null(popup)) popup[i],
        label       = if (!is.null(labs)) labs[i],
        group       = g,
        clusterOptions = if (cluster_on) leaflet::markerClusterOptions())
    }
    m <- leaflet::addLayersControl(
      m, overlayGroups = groups,
      options  = leaflet::layersControlOptions(collapsed = FALSE),
      position = "topleft")
  }

  show_legend <- !is.null(pal_info) && isTRUE(p$legend %||% TRUE) &&
    (pal_info$kind == "numeric" || pal_info$n <= MAP_LEGEND_MAX)
  if (show_legend) {
    # Log scale colors run over log10(values); relabel the legend back to the
    # original units so it reads like the data.
    lab_fmt <- if (identical(pal_info$scale, "log"))
      leaflet::labelFormat(transform = function(x) signif(10^x, 3))
    else leaflet::labelFormat()
    m <- leaflet::addLegend(m, position = "bottomright", pal = pal_info$pal,
                            values = pal_info$values, title = cv,
                            opacity = 0.9, labFormat = lab_fmt)
  }

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
  gv <- map_col(p$group_by)
  if (!is.null(gv) && (!gv %in% names(d0) || all(is.na(d0[[gv]])) ||
                       dplyr::n_distinct(map_group_vals(d0[[gv]])) > MAP_GROUP_MAX))
    gv <- NULL
  cv <- map_col(p$color)
  if (!is.null(cv) && (!cv %in% names(d0) || all(is.na(d0[[cv]])))) cv <- NULL
  if (!is.null(gv) && is.null(cv)) cv <- gv   # the builder's auto-color rule
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
  fill_expr <- sprintf("fillColor = %s", qq(p$color_hex %||% UF_BLUE))
  legend_args <- NULL
  if (!is.null(cv)) {
    n_lv    <- dplyr::n_distinct(d0[[cv]])
    is_cont <- is.numeric(d0[[cv]]) && n_lv > 10
    cols    <- map_palette_colors(p$palette %||% "auto", is_cont, n_lv)
    scale   <- if (is_cont) map_color_scale(d0[[cv]], p$color_scale %||% "linear")
               else "linear"
    pal_line <- if (!is_cont) sprintf(
        'pal <- colorFactor(%s, domain = %s, na.color = "#bbbbbb")',
        map_palette_code(cols), dollar(cv))
      else switch(scale,
        log      = sprintf(
          'pal <- colorNumeric(%s, domain = log10(%s), na.color = "#bbbbbb")  # log color scale',
          map_palette_code(cols), dollar(cv)),
        quantile = sprintf(
          'pal <- colorQuantile(%s, domain = %s, n = %d, na.color = "#bbbbbb")',
          map_palette_code(cols), dollar(cv), MAP_QUANT_BINS),
        sprintf('pal <- colorNumeric(%s, domain = %s, na.color = "#bbbbbb")',
                map_palette_code(cols), dollar(cv)))
    pre <- c(pre, "", pal_line)
    fill_expr <- if (identical(scale, "log"))
      sprintf("fillColor = ~pal(log10(%s))", bq(cv))
    else sprintf("fillColor = ~pal(%s)", bq(cv))
    show_legend <- isTRUE(p$legend %||% TRUE) &&
      (is_cont || n_lv <= MAP_LEGEND_MAX)
    if (show_legend) {
      leg_vals <- if (identical(scale, "log")) sprintf("~log10(%s)", bq(cv))
                  else sprintf("~%s", bq(cv))
      leg_fmt  <- if (identical(scale, "log"))
        ",\n            labFormat = labelFormat(transform = function(x) signif(10^x, 3))"
      else ""
      legend_args <- sprintf(
        'position = "bottomright", pal = pal, values = %s, title = %s%s',
        leg_vals, qq(cv), leg_fmt)
    }
  }
  # Emitted helper columns are dot-prefixed so they can't silently clobber the
  # user's own columns when the script is pasted over their data.
  if (length(popc)) {
    parts <- vapply(popc, function(cn)
      sprintf('"<b>%s:</b> ", %s', htmltools::htmlEscape(cn), dollar(cn)),
      character(1))
    pre <- c(pre, "", paste0(
      "df$.map_popup <- paste0(",
      paste(parts, collapse = ', "<br/>",\n                     '), ")"))
  }
  if (!is.null(sizev)) {
    pre <- c(pre, "",
      "# Bubble radii: sqrt scaling so marker AREA tracks the value",
      sprintf("v <- %s - min(%s, na.rm = TRUE)", dollar(sizev), dollar(sizev)),
      sprintf("df$.map_radius <- %s + %s * sqrt(v / max(v, na.rm = TRUE))",
              MAP_RADIUS_RANGE[1], MAP_RADIUS_RANGE[2] - MAP_RADIUS_RANGE[1]),
      sprintf("df$.map_radius[!is.finite(df$.map_radius)] <- %s",
              MAP_RADIUS_RANGE[1]))
  }

  marker_args <- c(
    sprintf("lng = ~%s, lat = ~%s", bq(lon), bq(lat)),
    if (!is.null(sizev)) "radius = ~.map_radius" else sprintf("radius = %s", size),
    'stroke = TRUE, weight = 1, color = "#FFFFFF"',
    fill_expr,
    sprintf("fillOpacity = %s", alpha),
    if (length(popc)) "popup = ~.map_popup",
    if (!is.null(labv)) sprintf("label = ~as.character(%s)", bq(labv)),
    if (cluster_on) "clusterOptions = markerClusterOptions()")
  ttl <- if (!is.null(p$title) && nzchar(trimws(p$title))) trimws(p$title)
  title_args <- if (!is.null(ttl)) sprintf(
    '%s, position = "topright"',
    qq(sprintf("<b>%s</b>", htmltools::htmlEscape(ttl))))

  if (is.null(gv)) {
    lines <- c(
      "leaflet(df)",
      sprintf('addProviderTiles("%s")', basemap),
      sprintf("addCircleMarkers(%s)",
              paste(marker_args, collapse = ",\n                   ")))
    if (!is.null(legend_args)) lines <- c(lines, sprintf("addLegend(%s)", legend_args))
    if (!is.null(title_args))  lines <- c(lines, sprintf("addControl(%s)", title_args))
    code <- paste0(lines[1], " |>\n  ",
                   paste(lines[-1], collapse = " |>\n  "))
    return(assemble_map_code(pre, code))
  }

  # Grouped form: one toggleable layer per level, clustered within the group.
  # NA group values become their own "(missing)" layer, exactly as rendered.
  code <- c(
    sprintf("df$.map_group <- as.character(%s)", dollar(gv)),
    'df$.map_group[is.na(df$.map_group)] <- "(missing)"',
    "groups <- sort(unique(df$.map_group))",
    sprintf('m <- leaflet(df) |>\n  addProviderTiles("%s")', basemap),
    "for (g in groups) {",
    "  m <- addCircleMarkers(m, data = df[df$.map_group == g, ],",
    paste0("                   ",
           paste(c(marker_args, "group = g"),
                 collapse = ",\n                   "), ")"),
    "}",
    "m <- addLayersControl(m, overlayGroups = groups,",
    '                      options = layersControlOptions(collapsed = FALSE),',
    '                      position = "topleft")')
  if (!is.null(legend_args))
    code <- c(code, sprintf("m <- addLegend(m, %s)", legend_args))
  if (!is.null(title_args))
    code <- c(code, sprintf("m <- addControl(m, %s)", title_args))
  code <- c(code, "m")
  assemble_map_code(pre, paste(code, collapse = "\n"))
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
  restore_rng <- snapshot_rng()   # fixed seed must NOT hijack the session RNG
  on.exit(restore_rng(), add = TRUE)
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

# --- Map export (filesystem / external tools) --------------------------------
# inline_html_deps is pure text-processing over files and IS unit-tested; the
# PNG path needs a headless browser and is smoke-tested. The module turns
# stop() messages into notifications.

# MIME types for the assets a widget's stylesheets can reference.
.map_mime <- function(path) {
  switch(tolower(tools::file_ext(path)),
         png  = "image/png",   gif  = "image/gif",
         jpg  = "image/jpeg",  jpeg = "image/jpeg",
         svg  = "image/svg+xml",
         woff = "font/woff",   woff2 = "font/woff2", ttf = "font/ttf",
         "application/octet-stream")
}

# Replace one literal chunk of `text` with `replacement`, inserting the
# replacement VERBATIM (gsub would interpret backslashes in JS/CSS payloads).
# A sentinel byte keeps strsplit from dropping the trailing field when the
# target sits at the very end of the text (it would be deleted, not replaced).
.splice <- function(text, target, replacement) {
  parts <- strsplit(paste0(text, "\x01"), target, fixed = TRUE)[[1]]
  out <- paste(parts, collapse = replacement)
  substr(out, 1L, nchar(out) - 1L)
}

# Make saveWidget(selfcontained = FALSE) output truly self-contained WITHOUT
# pandoc: scripts become data-URI src, stylesheets are inlined as <style>
# blocks with their url(...) assets (icons, images) converted to data URIs.
# This is what pandoc's --self-contained does, minus the pandoc dependency
# (the user's machine usually has none outside RStudio).
inline_html_deps <- function(html_file) {
  base <- dirname(html_file)
  html <- paste(readLines(html_file, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
  local_path <- function(ref, dir) {
    if (grepl("^(https?:|data:|//)", ref)) return(NULL)
    fp <- file.path(dir, utils::URLdecode(ref))
    if (file.exists(fp)) fp else NULL
  }

  inline_css_assets <- function(css, css_dir) {
    urls <- unique(unlist(regmatches(
      css, gregexpr("url\\((['\"]?)[^)'\"]+\\1\\)", css))))
    for (u in urls) {
      ref <- sub("['\"]?\\)$", "", sub("^url\\(['\"]?", "", u))
      fp  <- local_path(ref, css_dir)
      if (is.null(fp)) next
      css <- .splice(css, u, sprintf(
        "url(%s)", base64enc::dataURI(file = fp, mime = .map_mime(fp))))
    }
    css
  }

  # <link rel="stylesheet" href="local.css"> -> <style>...inlined css...</style>
  links <- unique(unlist(regmatches(
    html, gregexpr('<link[^>]*href="[^"]+"[^>]*/?>', html))))
  for (tag in links) {
    if (!grepl("stylesheet", tag, fixed = TRUE)) next
    ref <- sub('.*href="([^"]+)".*', "\\1", tag)
    fp  <- local_path(ref, base)
    if (is.null(fp)) next
    css <- paste(readLines(fp, warn = FALSE, encoding = "UTF-8"),
                 collapse = "\n")
    css <- inline_css_assets(css, dirname(fp))
    html <- .splice(html, tag, paste0("<style>\n", css, "\n</style>"))
  }

  # <script src="local.js"></script> -> data-URI src (sidesteps any need to
  # escape "</script" sequences inside the payload).
  scripts <- unique(unlist(regmatches(
    html, gregexpr('<script[^>]*src="[^"]+"[^>]*></script>', html))))
  for (tag in scripts) {
    ref <- sub('.*src="([^"]+)".*', "\\1", tag)
    fp  <- local_path(ref, base)
    if (is.null(fp)) next
    html <- .splice(html, tag, sprintf(
      '<script src="%s"></script>',
      base64enc::dataURI(file = fp, mime = "application/javascript")))
  }
  html
}

# Save an interactive map as ONE self-contained .html that opens anywhere --
# no pandoc, no zip, no sidecar folder.
save_map_html <- function(widget, file) {
  tmp <- file.path(tempfile("map"), "map.html")
  dir.create(dirname(tmp), recursive = TRUE)
  on.exit(unlink(dirname(tmp), recursive = TRUE), add = TRUE)
  htmlwidgets::saveWidget(widget, tmp, selfcontained = FALSE, title = "Map")
  con <- file(file, open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(enc2utf8(inline_html_deps(tmp)), con, useBytes = TRUE)
  invisible(file)
}

# Prefer the modern headless mode: current Chrome/Edge builds have dropped
# the old one, so chromote's default dies with "debugging port not open".
# An explicit user setting is respected.
.chromote_env <- function() {
  if (!nzchar(Sys.getenv("CHROMOTE_HEADLESS")))
    Sys.setenv(CHROMOTE_HEADLESS = "new")
  invisible(NULL)
}

# Can we take PNG snapshots? Needs webshot2 + chromote AND a real Chromium
# browser. find_chrome() doesn't know about Microsoft Edge (Chromium-based,
# present on nearly every Windows machine), so fall back to pointing
# CHROMOTE_CHROME at it -- find_chrome() honours that variable from then on.
map_snapshot_ok <- function() {
  if (!requireNamespace("webshot2", quietly = TRUE) ||
      !requireNamespace("chromote", quietly = TRUE)) return(FALSE)
  .chromote_env()
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

# Close chromote's cached browser so the next webshot starts a fresh one --
# a crashed/stale session (or one holding a dead DevTools port) otherwise
# poisons every later attempt in the same R session.
.reset_chromote <- function() {
  tryCatch({
    if (chromote::has_default_chromote_object())
      chromote::default_chromote_object()$get_browser()$close()
  }, error = function(e) NULL)
  invisible(NULL)
}

# Rasterize the map via headless Chrome/Edge; `delay` gives basemap tiles a
# moment to load. Browser startup on managed Windows machines is flaky
# ("Cannot find an available port", "Failed to start chrome"), so a failed
# first attempt retries ONCE with a fresh browser in the modern headless mode
# (newer Chrome/Edge builds dropped the old one).
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
  .chromote_env()
  shot <- function() webshot2::webshot(tmp, file = file, vwidth = width,
                                       vheight = height, delay = delay)
  out <- tryCatch(shot(), error = function(e) e)
  if (inherits(out, "error")) {
    .reset_chromote()   # a stale/crashed browser poisons every later attempt
    out <- tryCatch(shot(), error = function(e) e)
  }
  if (inherits(out, "error")) {
    stop("The headless browser failed to start twice (",
         conditionMessage(out), "). Run chromote::chromote_info() in the R ",
         "console to diagnose, or use the interactive HTML download instead.")
  }
  invisible(file)
}
