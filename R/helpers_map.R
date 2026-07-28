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
MAP_ADMIN_MAX    <- 100    # "combine by area" bubbles beyond this are unreadable
                           # (67 FL counties fit comfortably; raw ZIPs do not)
MAP_QUANT_BINS   <- 5      # quantile color scale bin count

# Color scales for a CONTINUOUS color column. Log spreads out skewed values
# (regional totals, populations); quantile guarantees every bin gets ink.
MAP_SCALES <- c("Linear" = "linear", "Log" = "log",
                "Quantile (5 bins)" = "quantile")
# The same three scales drive "size by". Reused verbatim so the two pickers
# offer identical wording; the quantile label reads as bins for color and as
# ranks for size, which is the same idea either way.
MAP_SIZE_SCALES <- c("Linear (area-proportional)" = "linear",
                     "Log (spread out skewed values)" = "log",
                     "Quantile (even spread of sizes)" = "quantile")
MAP_RADIUS_RANGE <- c(4, 18)  # graduated-symbol radius bounds in px (sqrt scale)
MAP_SIZE_LEG_N   <- 4      # graduated circles drawn in the size legend
MAP_HEAT_RADIUS  <- 20     # default density-heatmap point radius (px)
MAP_CHORO_BINS   <- 5      # quantile bins for a choropleth colour scale
# Cap on GeoJSON features rendered (perf guard). 4000 clears the built-in US
# counties set (3,222 features incl. DC and Puerto Rico) with headroom -- at
# 3000 it silently clipped the last ~200 counties off the map. When the cap
# DOES bite, build_choropleth reports it in choro_diag rather than quietly
# dropping regions.
MAP_CHORO_MAX    <- 4000
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

# Collapse cleaned point rows to ONE row per admin area (county / region /
# zip): the area's centroid (mean of the clean coordinates), a .n_points
# count (dot-prefixed like every emitted helper column, so it can't clobber
# a user column), and na.rm means of any requested numeric columns (an
# all-NA area yields NA, not NaN). NA admin values form their own
# "(missing)" area via map_group_vals(), exactly like layer grouping. Rows
# come back sorted by area label so marker order, palette domains, and the
# emitted tapply() script all agree.
# Longitude mean that survives the antimeridian: an area whose points span
# +179/-179 has an arithmetic mean near 0 (the wrong side of the planet), so
# when the spread exceeds 180 degrees the negative half is shifted +360, the
# mean taken, and the result wrapped back. Ordinary areas hit the plain-mean
# branch, so builder results stay byte-identical to the emitted script's
# lonmean() twin.
map_lon_mean <- function(x) {
  if (diff(range(x)) > 180) {
    x <- ifelse(x < 0, x + 360, x)
    m <- mean(x)
    if (m > 180) m - 360 else m
  } else mean(x)
}

aggregate_by_admin <- function(d, admin_col, lon, lat,
                               agg_cols = character(0)) {
  av <- map_group_vals(d[[admin_col]])
  n  <- tapply(rep(1L, length(av)), av, length)   # names sorted by label
  out <- data.frame(names(n), stringsAsFactors = FALSE, check.names = FALSE)
  names(out) <- admin_col
  out[[lat]] <- as.vector(tapply(d[[lat]], av, mean))
  out[[lon]] <- as.vector(tapply(d[[lon]], av, map_lon_mean))
  out$.n_points <- as.integer(n)
  for (col in setdiff(agg_cols, c(admin_col, lon, lat))) {
    if (!col %in% names(d) || !is.numeric(d[[col]])) next
    v <- as.vector(tapply(d[[col]], av, function(x) mean(x, na.rm = TRUE)))
    v[is.nan(v)] <- NA_real_
    out[[col]] <- v
  }
  rownames(out) <- NULL
  out
}

# The shared gate + params rewrite for "combine points by area", consumed by
# BOTH build_leaflet_map and generate_map_code so the pane, the downloads,
# and the emitted script can never disagree (the same quadruple-gate rule as
# layer groups). Inactive -- d and p returned unchanged -- unless a usable
# admin column with 2..MAP_ADMIN_MAX areas is chosen. When active, the
# point layer is REPLACED by one bubble per area: sized by the .n_points
# count through the existing radius machinery, colored by the area MEAN of
# the user's color column when that column is numeric (a categorical pick
# has no meaningful area aggregate and is ignored -> single marker color),
# and labelled/popup'd with the area name + point count (via the friendly
# "Points" alias). Proximity clustering and layer groups are point-level
# ideas, so they switch off while combining. The density HEATMAP does not:
# build_leaflet_map keeps the un-aggregated rows and draws it from those, so
# it still describes the underlying points (see the note at the modifyList).
admin_cluster_params <- function(d, p, lon, lat) {
  av <- map_col(p$cluster_by)
  if (is.null(av) || !av %in% names(d) || av %in% c(lon, lat))
    return(list(d = d, p = p, active = FALSE))
  n_area <- dplyr::n_distinct(map_group_vals(d[[av]]))
  if (n_area < 2L || n_area > MAP_ADMIN_MAX)
    return(list(d = d, p = p, active = FALSE))
  cv <- map_col(p$color)
  keep_cv <- !is.null(cv) && cv %in% names(d) && is.numeric(d[[cv]]) &&
    !cv %in% c(av, lon, lat)
  agg <- aggregate_by_admin(d, av, lon, lat,
                            agg_cols = if (keep_cv) cv else character(0))
  # Popup-friendly alias for the count -- but never clobber a real user
  # column named "Points" (it may be the aggregated color column); the
  # popup then shows the dot-prefixed internal name instead.
  count_col <- if ("Points" %in% names(agg)) ".n_points" else "Points"
  if (identical(count_col, "Points")) agg$Points <- agg$.n_points
  # `heatmap` is deliberately NOT forced off here. The heat surface is built
  # from the RAW points (see build_leaflet_map), so it composes with the
  # per-area bubbles: density underneath, counts on top. Everything else in
  # this list genuinely cannot apply once the rows are aggregated.
  p2 <- utils::modifyList(p, list(
    color      = if (keep_cv) cv else "__none__",
    size_by    = ".n_points",
    size_scale = "linear",
    group_by   = "__none__",
    cluster    = "off",
    label_col  = av,
    popup_cols = c(av, count_col, if (keep_cv) cv)))
  list(d = agg, p = p2, active = TRUE)
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

# Sanitise a heatmap weight column. leaflet.heat needs finite, positive
# intensities and a positive max; an all-NA column would make max() return
# -Inf (serialised to null -> invisible layer + a warning per rebuild), and
# negative weights produce NaN cell math. Unusable weights -> plain density.
# The single source of truth: builder AND code generator both call this.
map_heat_weights <- function(v) {
  if (is.null(v) || !is.numeric(v))
    return(list(intensity = NULL, max = 1, weighted = FALSE))
  fin <- is.finite(v) & v > 0
  if (!any(fin)) return(list(intensity = NULL, max = 1, weighted = FALSE))
  w <- ifelse(fin, v, 0)          # NA/Inf/negative rows count as zero weight
  list(intensity = w, max = max(w), weighted = TRUE)
}

# Effective size scale for a numeric size-by column: the requested one unless
# the data can't support it. Twin of map_color_scale() and the single source of
# truth for the fallback -- builder, code-gen and map_hint all consult it.
map_size_scale <- function(v, requested) {
  requested <- requested %||% "linear"
  if (!requested %in% c("log", "quantile")) return("linear")
  vv <- v[is.finite(v)]
  if (!length(vv)) return("linear")
  if (requested == "log" && any(vv <= 0)) return("linear")
  # With fewer than three distinct values the ranks carry no more information
  # than the raw values, so quantile sizing just adds a confusing legend.
  if (requested == "quantile" && length(unique(vv)) < 3) return("linear")
  requested
}

# Position of each value along the chosen size scale, as a 0-1 fraction
# measured against `domain`. Splitting this out is what lets the markers and
# the size legend share ONE code path: the legend passes its break values as
# `v` and the full column as `domain`, so a drawn circle is guaranteed to be
# the size a marker of that value would be.
map_size_frac <- function(v, domain, scale) {
  scale <- scale %||% "linear"
  if (identical(scale, "quantile")) {
    d <- domain[is.finite(domain)]
    if (!length(d)) return(rep(NA_real_, length(v)))
    return(stats::ecdf(d)(v))
  }
  tf  <- if (identical(scale, "log")) log10 else identity
  vt  <- suppressWarnings(tf(v))
  dt  <- suppressWarnings(tf(domain))
  fin <- is.finite(dt)
  if (!any(fin)) return(rep(NA_real_, length(v)))
  lo <- min(dt[fin]); hi <- max(dt[fin])
  if (hi == lo) return(rep(0.5, length(v)))
  pmin(pmax((vt - lo) / (hi - lo), 0), 1)
}

# Graduated-symbol radii. Under the LINEAR scale the fraction is sqrt-ed so
# marker AREA (not radius) tracks the value -- linear radii visually exaggerate
# the large end. Log and quantile are explicitly "amplify the spread" modes, so
# they map straight to radius: a second sqrt would undo exactly the spreading
# they were chosen for. Constant column -> midpoint; NA / non-finite -> smallest.
scale_radius <- function(v, range = MAP_RADIUS_RANGE, scale = "linear",
                         domain = v) {
  if (is.null(v) || !is.numeric(v)) return(NULL)
  scale <- scale %||% "linear"
  dfin  <- domain[is.finite(domain)]
  if (!length(dfin) || max(dfin) == min(dfin))
    return(rep(mean(range), length(v)))
  frac <- map_size_frac(v, domain, scale)
  if (identical(scale, "linear")) frac <- sqrt(frac)
  r <- range[1] + (range[2] - range[1]) * frac
  # Keyed off the INPUT, not the computed radius: an Inf clamps to frac = 1 and
  # would otherwise be drawn as the largest bubble on the map rather than
  # flagged as unusable. The emitted code keys off `v` for the same reason.
  r[!is.finite(v)] <- range[1]
  r
}

# Representative value/radius pairs for the size legend: data quantiles, so the
# breaks always exist in the data and span it even when it's badly skewed.
# Radii come from scale_radius() against the full column -- same math as the
# markers, by construction.
size_legend_breaks <- function(v, scale = "linear", range = MAP_RADIUS_RANGE,
                               n = MAP_SIZE_LEG_N) {
  if (is.null(v) || !is.numeric(v)) return(NULL)
  vv <- v[is.finite(v)]
  if (!length(vv) || max(vv) == min(vv)) return(NULL)
  brks <- unique(signif(stats::quantile(vv, probs = seq(0, 1, length.out = n),
                                        na.rm = TRUE, names = FALSE), 3))
  if (length(brks) < 2) return(NULL)
  data.frame(value = brks,
             radius = scale_radius(brks, range, scale, domain = v))
}

# Graduated-circle legend markup for addControl(). leaflet's addLegend() can
# only draw color swatches, so sized symbols have to be hand-built. Circles are
# right-aligned in a fixed-width cell (the widest circle) so the labels line up.
size_legend_html <- function(brks, title = NULL, fill = UF_BLUE) {
  if (is.null(brks) || !nrow(brks)) return(NULL)
  cell <- 2 * max(brks$radius) + 2
  rows <- vapply(rev(seq_len(nrow(brks))), function(i) {
    dia <- 2 * brks$radius[i]
    sprintf(paste0(
      "<div style='display:flex;align-items:center;margin:1px 0;'>",
      "<span style='width:%.0fpx;display:flex;justify-content:center;'>",
      "<span style='display:inline-block;width:%.0fpx;height:%.0fpx;",
      "border-radius:50%%;background:%s;opacity:0.75;border:1px solid #FFF;'>",
      "</span></span>",
      "<span style='font-size:11px;margin-left:6px;'>%s</span></div>"),
      cell, dia, dia, fill,
      htmltools::htmlEscape(format(brks$value[i], big.mark = ",",
                                   trim = TRUE, scientific = FALSE)))
  }, character(1))
  head_html <- if (!is.null(title) && nzchar(title))
    sprintf("<div style='font-weight:600;font-size:11px;margin-bottom:3px;'>%s</div>",
            htmltools::htmlEscape(title))
  else ""
  sprintf(paste0("<div style='background:rgba(255,255,255,0.85);padding:6px 8px;",
                 "border-radius:4px;line-height:1;'>%s%s</div>"),
          head_html, paste(rows, collapse = ""))
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

# Data with no coordinates can still be mapped -- as shaded regions, keyed on
# a name column. Nothing said so, so users with (say) state-level poverty
# rates assumed the map tool was not for them. Returns the nudge, or NULL when
# the data has usable coordinates or nothing that looks like region names.
# `n_regions` bounds what is plausible: 1 value is not a map, and thousands of
# distinct strings is an ID column, not a set of areas.
map_no_coord_advice <- function(df, n_regions = c(2L, 4000L)) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(NULL)
  det <- detect_coord_cols(df)
  if (!is.null(det$lon) && !is.null(det$lat)) return(NULL)
  # A plausible region column: text (or factor), with a sane number of levels.
  cand <- names(df)[vapply(df, function(x) {
    if (!(is.character(x) || is.factor(x))) return(FALSE)
    n <- dplyr::n_distinct(x, na.rm = TRUE)
    n >= n_regions[1] && n <= n_regions[2]
  }, logical(1))]
  if (!length(cand)) return(NULL)
  # "or" takes singular agreement, so one "holds" serves any candidate count.
  sprintf(paste0("No latitude/longitude columns found. If %s holds state, ",
                 "county or country names, switch Map type to <b>Shaded ",
                 "regions</b> %s it maps data by name, with boundaries built ",
                 "in, and needs no coordinates."),
          paste(sprintf("<b>%s</b>", htmltools::htmlEscape(utils::head(cand, 3))),
                collapse = " or "),
          SYM_MDASH)
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
  # Same loud fallback the color scale gets (map_size_scale is the shared rule).
  if (!is.null(sv) && sv %in% names(df) && is.numeric(df[[sv]])) {
    req_ssc <- p$size_scale %||% "linear"
    if (req_ssc %in% c("log", "quantile") &&
        map_size_scale(cc$data[[sv]], req_ssc) == "linear")
      hints <- c(hints, if (req_ssc == "log") sprintf(
        "'%s' has zero or negative values %s size can't use a log scale, so it's linear.",
        sv, SYM_MDASH) else sprintf(
        "'%s' doesn't have enough distinct values for quantile sizing %s using a linear scale.",
        sv, SYM_MDASH))
  }
  # The heatmap now survives "combine by area" (it describes the raw points),
  # so the tick is always the effective state -- no admin-mode gate needed.
  if (!isTRUE(p$show_points %||% TRUE) && !isTRUE(p$heatmap))
    hints <- c(hints, paste0(
      "Point markers are hidden and the density heatmap is off ", SYM_MDASH,
      " the map shows only the basemap. Turn one of them back on."))
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
  avh <- map_col(p$cluster_by)
  if (!is.null(avh) && avh %in% names(df)) {
    # Same gate admin_cluster_params applies, explained instead of silent.
    n_area <- dplyr::n_distinct(map_group_vals(cc$data[[avh]]))
    if (n_area > MAP_ADMIN_MAX)
      hints <- c(hints, sprintf(
        "'%s' has %d areas %s too many to combine into one bubble each (max %d), so it is ignored.",
        avh, n_area, SYM_MDASH, MAP_ADMIN_MAX))
    else if (n_area < 2L)
      hints <- c(hints, sprintf(
        "'%s' has only one area %s nothing to combine, so it is ignored.",
        avh, SYM_MDASH))
  }
  if (length(hints)) paste(hints, collapse = " ") else NULL
}

# --- Built-in boundaries ------------------------------------------------------
# Finding and formatting a boundary file is the hard part of a choropleth, so
# the package ships three ready-made sets. They enter the SAME engine as an
# upload (parse_geojson -> build_choropleth), just from disk instead of a
# fileInput, so every downstream feature -- property picker, join diagnostics,
# HTML/PNG export, generated code -- works unchanged.
#
# Both sources are public domain: the US sets are Census Bureau cartographic
# boundary files at 1:20,000,000 (a US Government work), the world set is
# Natural Earth 1:110m. Built at dev time by dev/build_boundaries.R (sf is used
# there to read shapefiles; the package itself never depends on it), stripped
# to a few join-useful properties, coordinates rounded to 4 dp (~11 m) and
# gzipped -- 611 KB for all three.
#
# `key` is the property pre-selected to join on; `filter_by` names an optional
# property the module offers as a subset ("just Florida's counties"), which
# also keeps the browser from drawing 3,222 polygons when 67 will do.
MAP_BUILTIN_BOUNDARIES <- list(
  us_states = list(
    label     = "US states",
    file      = "us_states.geojson.gz",
    key       = "state",
    filter_by = NULL,
    source    = "US Census Bureau cartographic boundaries, 1:20m (public domain)"),
  us_counties = list(
    label     = "US counties",
    file      = "us_counties.geojson.gz",
    key       = "county_state",
    filter_by = "state",
    source    = "US Census Bureau cartographic boundaries, 1:20m (public domain)"),
  world_countries = list(
    label     = "World countries",
    file      = "world_countries.geojson.gz",
    key       = "country",
    filter_by = "continent",
    source    = "Natural Earth 1:110m admin-0 (public domain)"))

# Named vector for a selectInput: label -> key, with the upload sentinel first.
builtin_boundary_choices <- function() {
  c(stats::setNames(names(MAP_BUILTIN_BOUNDARIES),
                    vapply(MAP_BUILTIN_BOUNDARIES, `[[`, character(1), "label")),
    "Upload my own file..." = "__upload__")
}

# Read one built-in set as GeoJSON TEXT (the same thing readLines() of an
# upload yields), optionally keeping only features whose `filter_by` property
# equals `filter_value`. Returns NULL for an unknown key or a missing file so
# a stale bookmark degrades to "no boundaries" rather than an error.
builtin_boundary_text <- function(key, filter_value = NULL) {
  spec <- MAP_BUILTIN_BOUNDARIES[[key %||% ""]]
  if (is.null(spec)) return(NULL)
  path <- system.file("geo", spec$file, package = "foxplots")
  if (!nzchar(path) || !file.exists(path)) return(NULL)
  con <- gzfile(path, "rt")
  on.exit(close(con), add = TRUE)
  txt <- paste(readLines(con, warn = FALSE), collapse = "\n")
  if (is.null(spec$filter_by) || is.null(filter_value) ||
      !nzchar(filter_value) || identical(filter_value, "__all__"))
    return(txt)
  gj <- parse_geojson(txt)
  if (is.null(gj)) return(NULL)
  keep <- vapply(gj$features, function(f)
    identical(geojson_prop_chr(f$properties, spec$filter_by), filter_value),
    logical(1))
  if (!any(keep)) return(txt)   # unknown filter value: show everything
  gj$features <- gj$features[keep]
  jsonlite::toJSON(gj, auto_unbox = TRUE, digits = NA)
}

# The distinct values of a built-in set's filter property (e.g. every state
# name), for the module's "limit to" picker. character(0) when not filterable.
builtin_boundary_filter_values <- function(key) {
  spec <- MAP_BUILTIN_BOUNDARIES[[key %||% ""]]
  if (is.null(spec) || is.null(spec$filter_by)) return(character(0))
  gj <- parse_geojson(builtin_boundary_text(key))
  if (is.null(gj)) return(character(0))
  vals <- vapply(gj$features,
                 function(f) geojson_prop_chr(f$properties, spec$filter_by),
                 character(1))
  sort(unique(vals[nzchar(vals)]))
}

# --- Choropleth (shaded regions from uploaded GeoJSON or a built-in set) ------

# Parse GeoJSON text (or an already-parsed list) into a FeatureCollection list.
# jsonlite is a hard leaflet dependency, so no extra guard is needed. Returns
# NULL when the text isn't a usable FeatureCollection.
parse_geojson <- function(x) {
  if (is.null(x)) return(NULL)
  gj <- if (is.character(x)) {
    tryCatch(jsonlite::fromJSON(x, simplifyVector = FALSE),
             error = function(e) NULL)
  } else if (is.list(x)) x else NULL
  if (is.null(gj) || !identical(gj$type, "FeatureCollection") ||
      !length(gj$features)) return(NULL)
  gj
}

# The distinct feature-property names available to join on. Real-world files
# are NOT always homogeneous, so union the keys across (up to) the first 100
# features rather than trusting feature 1's schema.
geojson_props <- function(gj) {
  if (is.null(gj) || !length(gj$features)) return(character(0))
  n <- min(length(gj$features), 100L)
  unique(unlist(lapply(gj$features[seq_len(n)],
                       function(f) names(f$properties))))
}

# A feature property as a length-1 character, whatever shape the JSON gave it:
# scalars pass through, arrays take their first element, empty/NULL -> "".
geojson_prop_chr <- function(props, name) {
  v <- props[[name]]
  v <- unlist(v, use.names = FALSE)
  if (is.null(v) || !length(v)) "" else as.character(v[[1]])
}

# One sample value per property, for labelling the "Region name property"
# picker. Raw property names ("county_state", "fips") say nothing about what
# they hold, so the picker shows the name AND an example -- the difference
# between "county_state" and "Brooks County, Georgia" is the whole point.
# Scans several features because the first one can be missing a property.
geojson_prop_examples <- function(gj, props = NULL, n_scan = 20L,
                                  max_chars = 30L) {
  if (is.null(gj) || !length(gj$features)) return(character(0))
  if (is.null(props)) props <- geojson_props(gj)
  if (!length(props)) return(character(0))
  feats <- gj$features[seq_len(min(length(gj$features), n_scan))]
  out <- vapply(props, function(p) {
    for (f in feats) {
      v <- geojson_prop_chr(f$properties, p)
      if (nzchar(v) && !identical(v, "NA")) return(v)
    }
    ""
  }, character(1))
  trunc_at <- function(x) if (nchar(x) > max_chars)
    paste0(substr(x, 1L, max_chars - 1L), "\u2026") else x
  vapply(out, trunc_at, character(1))
}

# Choices for the region-property picker: values stay the bare property names
# (so a saved selection still matches), only the LABELS gain the example.
geojson_prop_choices <- function(gj, props = NULL) {
  if (is.null(props)) props <- geojson_props(gj)
  if (!length(props)) return(character(0))
  ex  <- geojson_prop_examples(gj, props)
  lab <- ifelse(nzchar(ex), sprintf("%s \u2014 e.g. %s", props, ex), props)
  stats::setNames(props, lab)
}

# Bounding box of every coordinate in the collection. GeoJSON nests
# positions arbitrarily deep (Polygon -> rings -> [lng,lat]; MultiPolygon
# adds another level), so flatten each feature's coordinates in one pass and
# read the interleaved lng/lat off the result.
#
# This used to walk the nesting recursively, growing `lng`/`lat` with
# `<<- c()` one position at a time -- quadratic, and on the built-in
# 3,222-feature counties set it took ~13 s PER CALL, freezing the whole
# Shiny process on every debounced settings change. unlist() does the same
# work in C: same bbox, ~1000x faster.
geojson_bounds <- function(gj) {
  n <- length(gj$features)
  lngs <- vector("list", n); lats <- vector("list", n)
  for (i in seq_len(n)) {
    v <- suppressWarnings(as.numeric(unlist(gj$features[[i]]$geometry$coordinates,
                                            use.names = FALSE)))
    if (!length(v) || length(v) %% 2L != 0L) next   # 3D/ragged: skip, not guess
    odd <- seq.int(1L, length(v) - 1L, by = 2L)
    lngs[[i]] <- v[odd]
    lats[[i]] <- v[odd + 1L]
  }
  # Fill a preallocated list and flatten ONCE: growing the vectors per
  # feature would reintroduce the quadratic cost at the feature level.
  lng <- unlist(lngs, use.names = FALSE)
  lat <- unlist(lats, use.names = FALSE)
  if (!length(lng)) return(NULL)
  ok <- is.finite(lng) & is.finite(lat)
  if (!any(ok)) return(NULL)
  list(lng1 = min(lng[ok]), lat1 = min(lat[ok]),
       lng2 = max(lng[ok]), lat2 = max(lat[ok]))
}

# Palette for choropleth values (linear or quantile only -- log's transform
# bookkeeping isn't worth it for shaded regions). Reuses the shared colour ramp.
choro_palette <- function(vals, palette = "auto", color_scale = "linear") {
  vv   <- vals[is.finite(vals)]
  nuni <- length(unique(vv))
  cols <- map_palette_colors(palette, TRUE, max(nuni, 3L))
  # Quantile needs UNIQUE breaks, not just enough distinct values -- tied
  # quantiles (e.g. many regions sharing one value) make colorQuantile()'s cut()
  # error at CALL time with "'breaks' are not unique". Same fallback rule as
  # map_color_scale(); a constant column (nuni < 2) can't even do linear.
  scale <- "linear"
  if (identical(color_scale, "quantile") && nuni >= MAP_CHORO_BINS) {
    qs <- stats::quantile(vv, probs = seq(0, 1, 1 / MAP_CHORO_BINS),
                          na.rm = TRUE)
    if (length(unique(qs)) == MAP_CHORO_BINS + 1L) scale <- "quantile"
  }
  pal <- if (identical(scale, "quantile"))
    leaflet::colorQuantile(cols, domain = vv, n = MAP_CHORO_BINS,
                           na.color = "#dddddd")
  else if (nuni >= 2L)
    leaflet::colorNumeric(cols, domain = range(vv), na.color = "#dddddd")
  else {
    scale <- "constant"    # single value: one colour, and no legend to draw
    function(x) rep(UF_BLUE, length(x))
  }
  list(pal = pal, scale = scale)
}

#' Build a choropleth (shaded regions) from uploaded GeoJSON joined to the data.
#'
#' p needs: geojson (text/list), region_key (data column), region_prop (GeoJSON
#' property matching region_key), region_value (numeric column to shade),
#' region_agg ("mean"/"sum"/"median", default mean). Optional: palette,
#' color_scale, basemap, legend, title, scalebar, view/view_bounds. The map's
#' match diagnostics are attached as attr(., "choro_diag").
#' @noRd
build_choropleth <- function(df, p) {
  gj <- parse_geojson(p$geojson)
  if (is.null(gj)) return(NULL)
  key  <- map_col(p$region_key)
  prop <- if (!is.null(p$region_prop) && nzchar(p$region_prop)) p$region_prop else NULL
  val  <- map_col(p$region_value)
  if (is.null(key) || is.null(prop) || is.null(val)) return(NULL)
  if (!is.data.frame(df) || !all(c(key, val) %in% names(df))) return(NULL)
  if (!is.numeric(df[[val]])) return(NULL)

  fn <- switch(p$region_agg %||% "mean",
               sum = function(x) sum(x, na.rm = TRUE),
               median = function(x) stats::median(x, na.rm = TRUE),
               function(x) mean(x, na.rm = TRUE))
  vals_by <- tapply(df[[val]], as.character(df[[key]]), fn)
  vals_by <- vals_by[is.finite(vals_by)]
  if (!length(vals_by)) return(NULL)

  cp <- choro_palette(as.numeric(vals_by), p$palette %||% "auto",
                      p$color_scale %||% "linear")

  n_total <- length(gj$features)          # before the perf cap truncates
  n_feat  <- min(n_total, MAP_CHORO_MAX)
  gj$features <- gj$features[seq_len(n_feat)]
  geo_keys <- character(0); matched <- 0L
  for (i in seq_len(n_feat)) {
    # geojson_prop_chr: arrays take their first element, NULL/empty -> "" --
    # a hostile property shape must degrade to "unmatched", never error.
    rk <- geojson_prop_chr(gj$features[[i]]$properties, prop)
    geo_keys <- c(geo_keys, rk)
    v   <- if (nzchar(rk) && rk %in% names(vals_by)) unname(vals_by[[rk]]) else NA_real_
    col <- if (is.finite(v)) cp$pal(v) else "#dddddd"
    if (is.finite(v)) matched <- matched + 1L
    # leaflet's default style function reads feature.properties.style.
    gj$features[[i]]$properties$style <- list(
      fillColor = col, weight = 1, color = "#ffffff",
      fillOpacity = if (is.finite(v)) (p$alpha %||% 0.75) else 0.35, opacity = 1)
  }

  m <- leaflet::leaflet()
  m <- leaflet::addProviderTiles(m, p$basemap %||% "CartoDB.Positron")
  m <- leaflet::addGeoJSON(m, gj, weight = 1, color = "#ffffff",
                           fillOpacity = p$alpha %||% 0.75)
  if (isTRUE(p$legend %||% TRUE) && !identical(cp$scale, "constant"))
    m <- leaflet::addLegend(m, position = "bottomright", pal = cp$pal,
                            values = as.numeric(vals_by), title = val,
                            opacity = 0.9)
  if (isTRUE(p$scalebar %||% TRUE))
    m <- leaflet::addScaleBar(m, position = "bottomleft",
                              options = leaflet::scaleBarOptions(imperial = TRUE))
  ttl <- if (!is.null(p$title) && nzchar(trimws(p$title))) trimws(p$title)
  if (!is.null(ttl))
    m <- leaflet::addControl(m, position = "topright", html = sprintf(
      "<div style='font-weight:600; font-size:1.1em; padding:2px 6px;'>%s</div>",
      htmltools::htmlEscape(ttl)))

  vb <- p$view_bounds
  if (!is.null(vb) && all(c("north", "south", "east", "west") %in% names(vb))) {
    m <- leaflet::fitBounds(m, vb$west, vb$south, vb$east, vb$north)
  } else if (!is.null(p$view) &&
             all(c("lng", "lat", "zoom") %in% names(p$view))) {
    m <- leaflet::setView(m, lng = p$view$lng, lat = p$view$lat, zoom = p$view$zoom)
  } else {
    bb <- geojson_bounds(gj)
    if (!is.null(bb)) m <- leaflet::fitBounds(m, bb$lng1, bb$lat1, bb$lng2, bb$lat2)
  }

  attr(m, "choro_diag") <- list(
    n_features   = n_feat,
    n_total      = n_total,
    n_dropped    = max(0L, n_total - n_feat),
    n_matched    = matched,
    # A built-in set covers a whole country, so "most regions have no data" is
    # the normal case there, not a join failure -- the module words the
    # diagnostic differently when this is TRUE. p$geo_builtin is the set's KEY
    # (a string), so test for presence, not truth.
    builtin      = !is.null(p$geo_builtin) && nzchar(p$geo_builtin),
    unmatched_geo  = setdiff(unique(geo_keys[nzchar(geo_keys)]), names(vals_by)),
    unmatched_data = setdiff(names(vals_by), geo_keys))
  m
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
#   size_scale : "linear" / "log" / "quantile" for `size_by`
#                (default "linear"; silent fallback per map_size_scale)
#   size_legend: draw the graduated-circle key when `size_by` is set
#                (default TRUE)
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
#   heatmap    : add a density heatmap overlay (needs leaflet.extras)  (FALSE)
#   heat_by    : numeric column weighting the heatmap ("__none__"/NULL -> density)
#   heat_radius: heatmap point radius in px             (default MAP_HEAT_RADIUS)
#   scalebar   : draw a distance scale bar              (default TRUE)
#   title      : optional map title                    (default none)
#   view       : module-only: list(lng, lat, zoom) restoring the user's view;
#                NULL -> fitBounds to the data. Never emitted in code.
#   view_bounds: module-only: list(north, south, east, west) -- the area to
#                cover. Takes precedence over `view`; used by the PNG/report
#                snapshots, whose canvas differs in size from the live pane.
#                Never emitted in code.
build_leaflet_map <- function(df, p) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(NULL)
  # Choropleth mode: shaded regions from GeoJSON, no lon/lat needed. The
  # presence of p$geojson IS the mode flag (only the choro branch of
  # map_params sets it), so a half-configured choropleth returns NULL and the
  # caller shows "pick the region property..." -- it must NOT fall through and
  # quietly draw a points map, which looked like the mode switch had failed.
  if (!is.null(p$geojson)) {
    if (is.null(map_col(p$region_value))) return(NULL)
    return(build_choropleth(df, p))
  }
  lon <- map_col(p$lon); lat <- map_col(p$lat)
  if (is.null(lon) || is.null(lat) || identical(lon, lat)) return(NULL)
  if (!lon %in% names(df) || !lat %in% names(df)) return(NULL)
  if (!is.numeric(df[[lon]]) || !is.numeric(df[[lat]])) return(NULL)

  cc <- clean_coords(df, lon, lat)
  d  <- cc$data
  if (!nrow(d)) return(NULL)

  # "Combine points by area": one bubble per admin value, then fall through
  # to the ordinary single-layer path on the aggregated frame. Keep the
  # un-aggregated rows: the density heatmap must describe where the POINTS
  # are, not where a dozen area centroids sit, so it stays on d_pts.
  d_pts <- d
  ac <- admin_cluster_params(d, p, lon, lat)
  d  <- ac$d
  p  <- ac$p

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
  size_scale <- if (!is.null(sizev))
    map_size_scale(d[[sizev]], p$size_scale %||% "linear") else "linear"
  radius <- if (!is.null(sizev))
    scale_radius(d[[sizev]], MAP_RADIUS_RANGE, size_scale) else size
  popup  <- popup_html(d, popc)
  labs   <- if (!is.null(labv)) as.character(d[[labv]])
  sub_i  <- function(x, i) if (length(x) > 1) x[i] else x

  m <- leaflet::leaflet(d)
  m <- leaflet::addProviderTiles(m, basemap)

  # Density heatmap overlay (leaflet.extras, optional). Added BEFORE the
  # markers so it draws under them (leaflet stacks layers in add order).
  # Weighting uses map_heat_weights() -- the shared sanitiser the code
  # generator also consults -- and quietly falls back to plain density when
  # the weight column is unusable (all-NA/Inf, or a non-positive max).
  # d_pts, not d: while combining by area, d holds one row per area, and a
  # heat surface over a dozen centroids would say nothing about the data.
  if (isTRUE(p$heatmap) && requireNamespace("leaflet.extras", quietly = TRUE)) {
    hw <- map_heat_weights(if (!is.null(map_col(p$heat_by)) &&
                               map_col(p$heat_by) %in% names(d_pts))
      d_pts[[map_col(p$heat_by)]] else NULL)
    m <- leaflet.extras::addHeatmap(
      m, lng = d_pts[[lon]], lat = d_pts[[lat]], intensity = hw$intensity,
      radius = p$heat_radius %||% MAP_HEAT_RADIUS, blur = 15, max = hw$max)
  }

  # Markers can be hidden outright (e.g. heatmap-only view); the legends
  # below are marker keys, so they hide with them.
  show_pts <- isTRUE(p$show_points %||% TRUE)
  if (!show_pts) {
    # no marker layers, no layers control
  } else if (is.null(gv)) {
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

  show_legend <- show_pts && !is.null(pal_info) &&
    isTRUE(p$legend %||% TRUE) &&
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

  # Size key. Without one, graduated circles are decoration -- there is no way
  # to read a value off a bubble. Sits bottom-LEFT so it never collides with
  # the color legend above.
  if (show_pts && !is.null(sizev) && isTRUE(p$size_legend %||% TRUE)) {
    leg_html <- size_legend_html(
      size_legend_breaks(d[[sizev]], size_scale, MAP_RADIUS_RANGE),
      title = if (isTRUE(ac$active)) "Points" else sizev,
      fill  = if (is.null(pal_info)) (p$color_hex %||% UF_BLUE) else UF_BLUE)
    if (!is.null(leg_html))
      m <- leaflet::addControl(m, position = "bottomleft", html = leg_html)
  }

  # Scale bar (native leaflet). On by default -- it makes an exported PNG read
  # as a real map. Bottom-left, where it can share the corner with the size key.
  if (isTRUE(p$scalebar %||% TRUE))
    m <- leaflet::addScaleBar(m, position = "bottomleft",
                              options = leaflet::scaleBarOptions(imperial = TRUE))

  ttl <- if (!is.null(p$title) && nzchar(trimws(p$title))) trimws(p$title)
  if (!is.null(ttl))
    m <- leaflet::addControl(
      m, position = "topright",
      html = sprintf(
        "<div style='font-weight:600; font-size:1.1em; padding:2px 6px;'>%s</div>",
        htmltools::htmlEscape(ttl)))

  # view_bounds wins over view: "cover this area" survives being re-rendered at
  # the snapshot canvas's size, whereas "this centre at this zoom" does not.
  # The exports pass bounds; the live pane passes centre+zoom (exact there).
  vb <- p$view_bounds
  if (!is.null(vb) && all(c("north", "south", "east", "west") %in% names(vb))) {
    m <- leaflet::fitBounds(m, vb$west, vb$south, vb$east, vb$north)
  } else if (!is.null(p$view) &&
             all(c("lng", "lat", "zoom") %in% names(p$view))) {
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

# Standalone script for the choropleth. The GeoJSON itself is the user's
# uploaded file, so the script reads it from disk rather than embedding it.
# Mirrors the builder: same aggregated values, same palette decision
# (choro_palette) computed here on the actual data, scale bar honoured.
generate_choropleth_code <- function(p, df = NULL) {
  key  <- map_col(p$region_key);  prop <- p$region_prop %||% "NAME"
  val  <- map_col(p$region_value)
  agg  <- p$region_agg %||% "mean"
  if (is.null(key) || is.null(val)) return("# Choose the join key and value.")

  # Decide the palette exactly as the builder will, using the real values.
  cols  <- map_palette_colors(p$palette %||% "auto", TRUE, MAP_CHORO_BINS)
  scale <- "linear"
  if (!is.null(df) && all(c(key, val) %in% names(df)) &&
      is.numeric(df[[val]])) {
    fn <- switch(agg, sum = function(x) sum(x, na.rm = TRUE),
                 median = function(x) stats::median(x, na.rm = TRUE),
                 function(x) mean(x, na.rm = TRUE))
    vb <- tapply(df[[val]], as.character(df[[key]]), fn)
    scale <- choro_palette(as.numeric(vb[is.finite(vb)]),
                           p$palette %||% "auto",
                           p$color_scale %||% "linear")$scale
  }
  pal_line <- if (identical(scale, "quantile")) sprintf(
    'pal  <- colorQuantile(%s, domain = vals[is.finite(vals)], n = %d, na.color = "#dddddd")',
    map_palette_code(cols), MAP_CHORO_BINS)
  else if (identical(scale, "constant")) sprintf(
    "pal  <- function(x) rep(%s, length(x))   # single value: one colour",
    qq(UF_BLUE))
  else sprintf(
    'pal  <- colorNumeric(%s, domain = range(vals, finite = TRUE), na.color = "#dddddd")',
    map_palette_code(cols))

  pipe <- c(
    "leaflet()",
    sprintf('addProviderTiles("%s")', p$basemap %||% "CartoDB.Positron"),
    "addGeoJSON(gj, weight = 1)",
    if (isTRUE(p$legend %||% TRUE) && !identical(scale, "constant")) sprintf(
      'addLegend(position = "bottomright", pal = pal, values = vals[is.finite(vals)], title = %s)',
      qq(val)),
    if (isTRUE(p$scalebar %||% TRUE)) 'addScaleBar(position = "bottomleft")')
  pipe_txt <- paste0(pipe[1], " |>\n  ",
                     paste(pipe[-1], collapse = " |>\n  "))

  # A built-in set is reproducible from the installed package; an upload is
  # only on the user's disk, so the script names a placeholder they replace.
  bkey <- p$geo_builtin
  geo_lines <- if (!is.null(bkey) && !is.null(MAP_BUILTIN_BOUNDARIES[[bkey]])) {
    spec <- MAP_BUILTIN_BOUNDARIES[[bkey]]
    filt <- p$geo_filter %||% "__all__"
    c(sprintf("# Boundaries: %s, shipped with foxplots (%s)",
              spec$label, spec$source),
      sprintf('gj <- fromJSON(gzfile(system.file("geo", "%s", package = "foxplots")),',
              spec$file),
      "               simplifyVector = FALSE)",
      if (!is.null(spec$filter_by) && nzchar(filt) &&
          !identical(filt, "__all__"))
        c(sprintf('# keep only %s == "%s"', spec$filter_by, filt),
          sprintf('keep <- vapply(gj$features, function(f) identical(as.character(f$properties$%s), "%s"), logical(1))',
                  spec$filter_by, filt),
          "gj$features <- gj$features[keep]"))
  } else {
    c("# Your data frame `df` and the boundary file you uploaded:",
      'gj <- fromJSON("your_boundaries.geojson", simplifyVector = FALSE)')
  }

  paste(c(
    "library(leaflet)",
    "library(jsonlite)",
    "",
    geo_lines,
    "",
    sprintf("# %s of %s within each %s", agg, bq(val), bq(key)),
    sprintf("vals <- tapply(df$%s, as.character(df$%s), %s, na.rm = TRUE)",
            bq(val), bq(key), agg),
    pal_line,
    "",
    "# Colour each region by writing its style into the feature properties",
    "# (leaflet's default style function reads feature.properties.style):",
    "for (i in seq_along(gj$features)) {",
    sprintf('  k <- unlist(gj$features[[i]]$properties[["%s"]])[1]', prop),
    "  v <- if (!is.null(k) && k %in% names(vals)) vals[[k]] else NA_real_",
    "  gj$features[[i]]$properties$style <- list(",
    '    fillColor = if (is.finite(v)) pal(v) else "#dddddd",',
    '    weight = 1, color = "#ffffff",',
    "    fillOpacity = if (is.finite(v)) 0.75 else 0.35)",
    "}",
    "",
    pipe_txt),
    collapse = "\n")
}

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
  # Choropleth mode has its own (much shorter) script. Mirror
  # build_leaflet_map exactly: p$geojson alone is the mode flag, so a
  # half-configured choropleth must NOT fall through and emit a points script
  # while the pane refuses to draw one.
  if (!is.null(p$geojson)) {
    if (is.null(map_col(p$region_value)))
      return("# Choose the numeric column to shade the regions by.")
    return(generate_choropleth_code(p, df))
  }
  lon <- map_col(p$lon); lat <- map_col(p$lat)
  if (is.null(lon) || is.null(lat) || identical(lon, lat) ||
      !lon %in% names(df) || !lat %in% names(df) ||
      !is.numeric(df[[lon]]) || !is.numeric(df[[lat]]))
    return("# Choose the longitude and latitude columns to generate map code.")

  cc <- clean_coords(df, lon, lat)
  d0 <- cc$data   # guards evaluate on the CLEANED rows, matching the builder
  # Hidden markers: the emitted script drops the marker layer and everything
  # that only describes markers (grouping, color/size palettes, legends,
  # popups/labels) but keeps the heatmap, scale bar, and title.
  show_pts <- isTRUE(p$show_points %||% TRUE)
  if (!show_pts) {
    p$group_by <- "__none__"; p$color <- "__none__"; p$size_by <- "__none__"
    p$legend <- FALSE; p$size_legend <- FALSE
    p$label_col <- "__none__"; p$popup_cols <- character(0)
  }
  # "Combine points by area": rewrite params + aggregate d0 exactly like the
  # builder does, so every palette/radius/guard decision below sees the
  # aggregated frame; the matching aggregation code is emitted into `pre`
  # once the emit helpers exist.
  ac <- admin_cluster_params(d0, p, lon, lat)
  if (ac$active) {
    d0 <- ac$d
    p  <- ac$p
  }
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
  if (ac$active) {
    av <- map_col(p$cluster_by)
    vc <- map_col(p$color)   # post-rewrite: the numeric color column or NULL
    pre <- c(pre, "",
      # The aggregation overwrites df, so stash the points first when the
      # heatmap needs them -- it describes where the POINTS are, not the
      # handful of area centroids.
      if (isTRUE(p$heatmap)) c(
        "# Keep the un-aggregated points for the density heatmap",
        "pts <- df", ""),
      sprintf("# One bubble per %s: centroid of its points + how many it combines",
              av),
      "# (lonmean handles the rare area whose points span the antimeridian)",
      "lonmean <- function(x) {",
      "  if (diff(range(x)) > 180) {",
      "    x <- ifelse(x < 0, x + 360, x)",
      "    m <- mean(x); if (m > 180) m - 360 else m",
      "  } else mean(x)",
      "}",
      sprintf("%s <- as.character(%s)", dollar(av), dollar(av)),
      sprintf('%s[is.na(%s)] <- "(missing)"', dollar(av), dollar(av)),
      sprintf("n <- tapply(%s, %s, length)", dollar(lat), dollar(av)),
      "df <- data.frame(check.names = FALSE,",
      sprintf("  %s = names(n),", bq(av)),
      sprintf("  %s = as.vector(tapply(%s, %s, mean)),",
              bq(lat), dollar(lat), dollar(av)),
      sprintf("  %s = as.vector(tapply(%s, %s, lonmean)),",
              bq(lon), dollar(lon), dollar(av)),
      paste0("  `.n_points` = as.integer(n)",
             if (is.null(vc)) ")" else ","),
      if (!is.null(vc)) sprintf(
        "  %s = as.vector(tapply(%s, %s, function(x) mean(x, na.rm = TRUE))))",
        bq(vc), dollar(vc), dollar(av)),
      if ("Points" %in% (p$popup_cols %||% character(0)))
        "df$Points <- df$`.n_points`")
  }

  show_legend <- FALSE
  fill_expr <- sprintf("fillColor = %s", qq(p$color_hex %||% UF_BLUE))
  legend_args <- NULL
  size_leg_args <- NULL
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
    r0   <- MAP_RADIUS_RANGE[1]
    span <- MAP_RADIUS_RANGE[2] - MAP_RADIUS_RANGE[1]
    mid  <- mean(MAP_RADIUS_RANGE)
    ssc  <- map_size_scale(d0[[sizev]], p$size_scale %||% "linear")
    # The `else` arms below are the constant-column midpoint guard the builder
    # has always applied. The old emitted code had no guard and computed 0/0
    # there, so a constant column drew 4px markers in the script and 11px ones
    # in the app.
    head_cmt <- switch(ssc,
      log      = "# Bubble radii: log scale -- spreads out skewed values",
      quantile = "# Bubble radii: quantile scale -- every size gets used",
      "# Bubble radii: sqrt scaling so marker AREA tracks the value")
    body <- switch(ssc,
      quantile = c(
        sprintf("v  <- %s", dollar(sizev)),
        "ok <- is.finite(v)",
        "df$.map_radius <- if (any(ok) && length(unique(v[ok])) > 1) {",
        sprintf("  %s + %s * ecdf(v[ok])(v)", r0, span),
        sprintf("} else %s", mid)),
      c(
        sprintf("v  <- %s%s", if (identical(ssc, "log")) "log10" else "",
                if (identical(ssc, "log")) sprintf("(%s)", dollar(sizev))
                else dollar(sizev)),
        "ok <- is.finite(v)",
        "df$.map_radius <- if (any(ok) && max(v[ok]) > min(v[ok])) {",
        "  lo <- min(v[ok]); hi <- max(v[ok])",
        sprintf("  %s + %s * %s((v - lo) / (hi - lo))", r0, span,
                if (identical(ssc, "linear")) "sqrt" else ""),
        sprintf("} else %s", mid)))
    pre <- c(pre, "", head_cmt, body,
             sprintf("df$.map_radius[!is.finite(v)] <- %s", r0))

    # Size key. Emitted as literal markup rather than code that rebuilds it:
    # leaflet has no graduated-symbol legend to call, and open-coding the
    # rebuild would add ~15 lines of string assembly to a script whose job is
    # to be readable. Regenerate from the app if the data changes.
    if (isTRUE(p$size_legend %||% TRUE)) {
      lh <- size_legend_html(
        size_legend_breaks(d0[[sizev]], ssc, MAP_RADIUS_RANGE),
        # same title the builder uses: never leak ".n_points" into a script
        title = if (isTRUE(ac$active)) "Points" else sizev,
        fill = if (is.null(cv)) (p$color_hex %||% UF_BLUE) else UF_BLUE)
      if (!is.null(lh)) size_leg_args <- sprintf(
        'position = "bottomleft", html = %s', qq(lh))
    }
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

  # Heatmap overlay + scale bar (emitted to match the builder). The same
  # map_heat_weights() rule decides weighting, and max= is emitted -- without
  # it leaflet.heat's default max=1 saturates every weighted cell into one
  # uniform blob, nothing like the app's map.
  heat_args <- if (isTRUE(p$heatmap)) {
    hb <- map_col(p$heat_by)
    # Weights come from the CLEANED, un-aggregated rows -- d0 is the aggregated
    # frame while combining by area, and its per-area means are not the point
    # weights the builder uses.
    hsrc <- if (ac$active) cc$data else d0
    hw <- map_heat_weights(if (!is.null(hb) && hb %in% names(hsrc)) hsrc[[hb]]
                           else NULL)
    # Formulas (~col) resolve against leaflet's data, which is the aggregated
    # df in combine mode -- so reference the stashed `pts` frame explicitly
    # there. Note the tilde prefixes the whole intensity EXPRESSION, not each
    # column, so the two modes differ by more than the column reference.
    ref  <- if (ac$active) function(cn) paste0("pts$", bq(cn)) else
                           function(cn) paste0("~", bq(cn))
    icol <- if (ac$active) function(cn) paste0("pts$", bq(cn)) else bq
    tld  <- if (ac$active) "" else "~"
    sprintf("lng = %s, lat = %s%s, radius = %s, blur = 15, max = %s",
            ref(lon), ref(lat),
            if (hw$weighted) sprintf(
              ", intensity = %spmax(0, ifelse(is.finite(%s), %s, 0))",
              tld, icol(hb), icol(hb)) else "",
            p$heat_radius %||% MAP_HEAT_RADIUS,
            if (hw$weighted) format(hw$max) else "1")
  }
  scalebar_on <- isTRUE(p$scalebar %||% TRUE)

  if (is.null(gv)) {
    lines <- c(
      "leaflet(df)",
      sprintf('addProviderTiles("%s")', basemap),
      if (show_pts) sprintf("addCircleMarkers(%s)",
              paste(marker_args, collapse = ",\n                   ")))
    if (!is.null(heat_args))
      lines <- c(lines, sprintf("leaflet.extras::addHeatmap(%s)", heat_args))
    if (!is.null(legend_args)) lines <- c(lines, sprintf("addLegend(%s)", legend_args))
    if (!is.null(size_leg_args))
      lines <- c(lines, sprintf("addControl(%s)", size_leg_args))
    if (scalebar_on) lines <- c(lines, 'addScaleBar(position = "bottomleft")')
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
  if (!is.null(size_leg_args))
    code <- c(code, sprintf("m <- addControl(m, %s)", size_leg_args))
  if (!is.null(heat_args))
    code <- c(code, sprintf("m <- leaflet.extras::addHeatmap(m, %s)", heat_args))
  if (scalebar_on)
    code <- c(code, 'm <- addScaleBar(m, position = "bottomleft")')
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
                         delay = 1, zoom = 1) {
  if (!map_snapshot_ok()) {
    stop("PNG snapshots need the optional 'webshot2' package and a ",
         "Chrome or Edge browser on this computer.")
  }
  tmp <- file.path(tempfile("mapshot"), "map.html")
  dir.create(dirname(tmp), recursive = TRUE)
  on.exit(unlink(dirname(tmp), recursive = TRUE), add = TRUE)
  # Pin the widget's own CSS size to the snapshot canvas. Without this the saved
  # page sizes the map to a default (~960px), so leaflet builds its tile grid
  # and fits its view for THAT size; webshot then loads the page at the larger
  # vwidth x vheight and the extra area is left as blank grey tiles (a hard
  # rectangle of un-loaded map). Matching the two makes leaflet request tiles
  # for the whole canvas. CSS px, not the zoom-scaled output px -- webshot's
  # `zoom` handles the device pixel ratio on top.
  widget$width  <- width
  widget$height <- height
  htmlwidgets::saveWidget(widget, tmp, selfcontained = FALSE, title = "Map")
  .chromote_env()
  # `zoom` raises the device pixel ratio: it multiplies the OUTPUT resolution
  # without touching the CSS layout, so the map still covers exactly the same
  # ground. That is what lets a snapshot taken at the on-screen pane size come
  # out crisp instead of small.
  shot <- function() webshot2::webshot(tmp, file = file, vwidth = width,
                                       vheight = height, delay = delay,
                                       zoom = zoom)
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
