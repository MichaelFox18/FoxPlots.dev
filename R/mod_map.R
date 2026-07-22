# ============================================================
# mod_map.R -- the Map stage (interactive leaflet point maps)
# ============================================================
# Thin (namespaced) wrapper over helpers_map.R: pick the coordinate columns
# (auto-detected), style the markers (color / size / popups / clustering),
# and download the map as interactive HTML or a PNG snapshot. The downloads
# live HERE, not in mod_export -- exportServer's chart slot is ggplot-only.
#
# mapServer() RETURNS reactive(list(maps = <0-1 leaflet widgets>,
# code = <0-1 strings>)), mirroring mod_visualize's contract so a future
# Report section can consume it. Pattern A: data flows in as a reactive.
#
# Rendering strategy: the whole widget is rebuilt from the pure builder on
# (debounced) setting changes -- but the user's pan/zoom is captured from
# leaflet's map_center/map_zoom events and re-applied via p$view, so tweaking
# a slider never snaps the map back to fitBounds. leafletProxy is used only
# by the "Zoom to data" button (view-only, never layers).

mapUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    tags$script(HTML(copy_js)),
    sidebar = sidebar(
      width = 320,
      h5("Map settings"),
      radioButtons(ns("map_type"),
        tagList("Map type", info_tip(
          "Points puts each row on the map by its coordinates. Shaded regions ",
          "(choropleth) colours uploaded boundaries (a GeoJSON file) by a ",
          "summary of your data - e.g. counties shaded by average yield.")),
        choices = c("Points" = "points", "Shaded regions" = "choro"),
        selected = "points", inline = TRUE),

      # ---- choropleth controls ----
      conditionalPanel(
        sprintf("input['%s'] == 'choro'", ns("map_type")),
        fileInput(ns("geojson_file"),
          tagList("Boundaries (.geojson / .json)", info_tip(
            "A GeoJSON FeatureCollection of the regions to shade - e.g. ",
            "county boundaries. Each feature needs a property (like NAME) ",
            "that matches a column in your data.")),
          accept = c(".geojson", ".json")),
        uiOutput(ns("ui_choro"))),

      # ---- point controls ----
      conditionalPanel(
        sprintf("input['%s'] == 'points'", ns("map_type")),
        uiOutput(ns("ui_lon")),
        uiOutput(ns("ui_lat")),
        actionLink(ns("swap"), tagList(icon("right-left"), " Swap lat / lon"),
                   class = "small mb-2")),
      uiOutput(ns("ui_hint")),
      hr(),
      h6("Style"),
      selectInput(ns("basemap"),
                  tagList("Basemap", info_tip(
                    "The background map. Tiles load from the internet, so an ",
                    "offline machine shows a grey background.")),
                  choices = MAP_BASEMAPS),
      conditionalPanel(
        sprintf("input['%s'] == 'points'", ns("map_type")),
        uiOutput(ns("ui_color")),
        conditionalPanel(
          sprintf("!input['%s'] || input['%s'] == '__none__'",
                  ns("color"), ns("color")),
          colourpicker::colourInput(ns("color_hex"), "Marker color",
                                    value = UF_BLUE)),
        conditionalPanel(
          sprintf("input['%s'] && input['%s'] != '__none__'",
                  ns("color"), ns("color")),
          selectInput(ns("palette"), "Color palette", choices = PALETTES),
          uiOutput(ns("ui_scale")),
          checkboxInput(ns("legend"), "Show legend", TRUE)),
        uiOutput(ns("ui_size")),
        conditionalPanel(
          sprintf("input['%s'] && input['%s'] != '__none__'",
                  ns("size_by"), ns("size_by")),
          selectInput(ns("size_scale"),
            tagList("Size scale", info_tip(
              "How values map to bubble area. Linear keeps twice the value at ",
              "twice the ink. Log spreads out skewed data so the small end ",
              "stops looking identical. Quantile gives every size an equal ",
              "share of points. Log and quantile fall back to linear when the ",
              "data can't support them.")),
            choices = MAP_SIZE_SCALES),
          checkboxInput(ns("size_legend"), "Show size legend", TRUE)),
        conditionalPanel(
          sprintf("!input['%s'] || input['%s'] == '__none__'",
                  ns("size_by"), ns("size_by")),
          sliderInput(ns("size"), "Point size", min = 2, max = 12, value = 6,
                      step = 1))),
      sliderInput(ns("alpha"), "Opacity", min = 0.1, max = 1, value = 0.8,
                  step = 0.05),
      conditionalPanel(
        sprintf("input['%s'] == 'points'", ns("map_type")),
        radioButtons(ns("cluster"),
                     tagList("Cluster nearby points", info_tip(
                       "Groups close-together markers into expandable bubbles. ",
                       "Auto turns clustering on above ", MAP_CLUSTER_AUTO,
                       " points. With layer groups, clustering happens within ",
                       "each group.")),
                     choices = c("Auto" = "auto", "On" = "on", "Off" = "off"),
                     selected = "auto", inline = TRUE),
        checkboxInput(ns("heatmap"),
          tagList("Density heatmap layer", info_tip(
            "Overlay a continuous density surface - useful when many points ",
            "overlap into an unreadable blob. Optionally weighted by a ",
            "numeric column. Needs the optional leaflet.extras package.")),
          value = FALSE),
        conditionalPanel(
          sprintf("input['%s']", ns("heatmap")),
          uiOutput(ns("ui_heat"))),
        hr(),
        h6("Layer groups"),
        uiOutput(ns("ui_group")),
        uiOutput(ns("ui_focus")),
        hr(),
        h6("Popups & labels"),
        uiOutput(ns("ui_popup")),
        uiOutput(ns("ui_label"))),
      checkboxInput(ns("scalebar"), "Show distance scale bar", TRUE),
      textInput(ns("title"), "Map title", placeholder = "(optional)"),
      hr(),
      h6("Download map"),
      textInput(ns("fname"), "File name (no extension)", value = "map-export"),
      downloadButton(ns("dl_html"), "Interactive map (.html)",
                     class = "btn-success w-100 mb-1"),
      uiOutput(ns("png_ui"))
    ),
    card(
      full_screen = TRUE,
      card_header(
        class = "d-flex justify-content-between align-items-center",
        span(icon("map-location-dot"), " Map"),
        actionButton(ns("zoom_data"), tagList(icon("expand"), " Zoom to data"),
                     class = "btn-sm btn-outline-secondary")),
      # Fixed height on purpose: a leafletOutput inside a flex fill can
      # collapse to zero height; full_screen covers the immersive case.
      leaflet::leafletOutput(ns("map"), height = "600px"),
      card_footer(uiOutput(ns("rowcount")))
    ),
    accordion(
      open = FALSE,
      accordion_panel(
        tagList(icon("code"), " R code for this map"), value = "code",
        tags$button("Copy code", class = "btn btn-sm btn-outline-secondary mb-2",
                    onclick = sprintf("DEcopy('%s', this)", ns("code"))),
        verbatimTextOutput(ns("code"))
      )
    )
  )
}

mapServer <- function(id, data_in) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    detected <- reactive({
      df <- data_in()
      req(is.data.frame(df))
      detect_coord_cols(df)
    })

    # --- pickers (re-rendered only when the data changes, which is exactly
    # when the picks should reset to a fresh auto-detect) ---------------------
    output$ui_lon <- renderUI({
      df <- data_in(); req(is.data.frame(df))
      num <- numeric_cols(df)   # the house classifier (excludes date columns)
      selectInput(ns("lon"),
                  tagList("Longitude column", info_tip(
                    "East-west position in decimal degrees (e.g. -82.32). ",
                    "Detected automatically from common column names.")),
                  choices = c("Choose..." = "", num),
                  selected = detected()$lon %||% "")
    })
    output$ui_lat <- renderUI({
      df <- data_in(); req(is.data.frame(df))
      num <- numeric_cols(df)
      selectInput(ns("lat"), "Latitude column",
                  choices = c("Choose..." = "", num),
                  selected = detected()$lat %||% "")
    })
    output$ui_color <- renderUI({
      df <- data_in(); req(is.data.frame(df))
      selectInput(ns("color"),
                  tagList("Color by (optional)", info_tip(
                    "Color markers by a column: categories get distinct ",
                    "colors, numbers get a gradient, both get a legend.")),
                  choices = c("None" = "__none__", names(df)))
    })
    output$ui_size <- renderUI({
      df <- data_in(); req(is.data.frame(df))
      num <- numeric_cols(df)
      selectInput(ns("size_by"),
                  tagList("Size by (optional)", info_tip(
                    "Scale marker size by a numeric column. Bubbles are ",
                    "area-proportional, so twice the value reads as twice ",
                    "the ink.")),
                  choices = c("None" = "__none__", num))
    })
    # ---- heatmap controls ----------------------------------------------------
    output$ui_heat <- renderUI({
      df <- data_in(); req(is.data.frame(df))
      if (!requireNamespace("leaflet.extras", quietly = TRUE))
        return(helpText(paste(
          "The density layer needs the optional leaflet.extras package:",
          'install.packages("leaflet.extras")')))
      tagList(
        selectInput(ns("heat_by"),
          tagList("Weight by (optional)", info_tip(
            "A numeric column weighting the density - hot spots become where ",
            "the VALUES concentrate, not just the points. None = plain point ",
            "density.")),
          choices = c("None" = "__none__", numeric_cols(df))),
        sliderInput(ns("heat_radius"), "Heat radius (px)", min = 5, max = 50,
                    value = MAP_HEAT_RADIUS, step = 5))
    })

    # ---- choropleth state + controls ----------------------------------------
    # The uploaded GeoJSON, parsed once per upload.
    geojson_state <- reactiveVal(NULL)
    observeEvent(input$geojson_file, {
      req(input$geojson_file$datapath)
      txt <- paste(readLines(input$geojson_file$datapath, warn = FALSE),
                   collapse = "\n")
      gj <- parse_geojson(txt)
      if (is.null(gj)) {
        geojson_state(NULL)
        showNotification(paste(
          "That file isn't a usable GeoJSON FeatureCollection. Export your",
          "boundaries as GeoJSON and try again."), type = "error", duration = 8)
      } else {
        geojson_state(gj)
        showNotification(sprintf("Loaded %d regions.", length(gj$features)),
                         type = "message")
      }
    })

    output$ui_choro <- renderUI({
      df <- data_in(); req(is.data.frame(df))
      gj <- geojson_state()
      if (is.null(gj))
        return(helpText("Upload a GeoJSON boundary file to begin."))
      props <- geojson_props(gj)
      tagList(
        selectInput(ns("region_prop"),
          tagList("Region name property", info_tip(
            "The GeoJSON property naming each region (e.g. NAME). It must ",
            "match the values in your data's key column.")),
          choices = props),
        selectInput(ns("region_key"),
          tagList("Match to data column", info_tip(
            "The column in your data holding the region names, matched ",
            "against the property above (exact text match).")),
          choices = c("Choose..." = "__none__", names(df))),
        selectInput(ns("region_value"),
          tagList("Shade by (numeric)", info_tip(
            "The numeric column summarised within each region to set its ",
            "colour.")),
          choices = c("Choose..." = "__none__", numeric_cols(df))),
        selectInput(ns("region_agg"), "Summary",
          choices = c("Mean" = "mean", "Sum" = "sum", "Median" = "median")),
        # Choro styling gets its OWN inputs: the points palette/scale/legend
        # controls are hidden in this mode, and driving the map from hidden
        # inputs' stale values is exactly the trap we're avoiding.
        selectInput(ns("choro_palette"), "Color palette", choices = PALETTES),
        selectInput(ns("choro_scale"),
          tagList("Color scale", info_tip(
            "Quantile gives every colour an equal share of the regions - ",
            "useful when a few regions dwarf the rest. Falls back to linear ",
            "when values are too tied for unique bins.")),
          choices = c("Linear" = "linear", "Quantile (5 bins)" = "quantile")),
        checkboxInput(ns("choro_legend"), "Show legend", TRUE),
        uiOutput(ns("choro_diag"))
      )
    })

    # Join diagnostics: a silent mismatch renders a blank grey map, so say
    # loudly how many regions matched and name the strays.
    output$choro_diag <- renderUI({
      w <- current_widget()
      diag <- attr(w, "choro_diag")
      if (is.null(diag)) return(NULL)
      msgs <- list(tags$p(class = "small mb-1", sprintf(
        "%d of %d regions matched your data.", diag$n_matched, diag$n_features)))
      if (length(diag$unmatched_geo))
        msgs <- c(msgs, list(tags$p(class = "small text-warning mb-1", paste0(
          "No data for: ", paste(utils::head(diag$unmatched_geo, 8),
                                 collapse = ", "),
          if (length(diag$unmatched_geo) > 8) ", ..." else ""))))
      if (length(diag$unmatched_data))
        msgs <- c(msgs, list(tags$p(class = "small text-warning mb-0", paste0(
          "No region for: ", paste(utils::head(diag$unmatched_data, 8),
                                   collapse = ", "),
          if (length(diag$unmatched_data) > 8) ", ..." else ""))))
      tagList(msgs)
    })

    output$ui_popup <- renderUI({
      df <- data_in(); req(is.data.frame(df))
      # Don't preselect coordinate-ish columns (detected OR merely coord-named,
      # so a failed detection doesn't put raw lat/lon in every popup).
      coordish <- names(df)[tolower(names(df)) %in%
                              c(COORD_LON_NAMES, COORD_LAT_NAMES)]
      pre <- utils::head(setdiff(names(df), c(unlist(detected()), coordish)),
                         MAP_POPUP_MAX)
      selectizeInput(ns("popup_cols"),
                     tagList("Popup columns (click a point)", info_tip(
                       "Shown when a marker is clicked, one line per column.")),
                     choices = names(df), selected = pre, multiple = TRUE)
    })
    output$ui_label <- renderUI({
      df <- data_in(); req(is.data.frame(df))
      selectInput(ns("label_col"),
                  tagList("Hover label (optional)", info_tip(
                    "Shown when the mouse rests on a marker (a popup needs ",
                    "a click; a label just a hover).")),
                  choices = c("None" = "__none__", names(df)))
    })
    # Color scale only makes sense for a CONTINUOUS color column (numeric,
    # > 10 distinct); categorical colors ignore it, so hide the control.
    output$ui_scale <- renderUI({
      df <- data_in(); req(is.data.frame(df))
      cv <- map_col(input$color)
      if (is.null(cv) || !cv %in% names(df)) return(NULL)
      v <- df[[cv]]
      if (!is.numeric(v) || dplyr::n_distinct(v) <= 10) return(NULL)
      selectInput(ns("color_scale"),
                  tagList("Color scale", info_tip(
                    "Log spreads out skewed values so regional differences ",
                    "show; quantile bins guarantee every color gets used.")),
                  choices = MAP_SCALES,
                  # isolate: seeding from our own input must not make this
                  # renderUI re-fire (and flicker) on every selection
                  selected = isolate(input$color_scale) %||% "linear")
    })
    output$ui_group <- renderUI({
      df <- data_in(); req(is.data.frame(df))
      ok <- names(df)[vapply(df, function(x) {
        # "(missing)" counts as a group -- same rule as the builder's gate,
        # or a 12-level column with NAs would be offered then rendered flat.
        n <- dplyr::n_distinct(x, na.rm = TRUE) + anyNA(x)
        n >= 2 && n <= MAP_GROUP_MAX
      }, logical(1))]
      selectInput(ns("group_by"),
                  tagList("Group layers by (optional)", info_tip(
                    "Each level (e.g. each county) becomes a layer you can ",
                    "toggle on the map, with its own clustering. Columns ",
                    "with up to ", MAP_GROUP_MAX, " groups are offered.")),
                  choices = c("None" = "__none__", ok))
    })
    output$ui_focus <- renderUI({
      df <- data_in(); req(is.data.frame(df))
      gv <- map_col(input$group_by)
      if (is.null(gv) || !gv %in% names(df)) return(NULL)
      lv <- unique(as.character(df[[gv]]))
      has_na <- anyNA(lv)
      lv <- sort(lv[!is.na(lv)])
      if (has_na) lv <- c(lv, "(missing)")
      selectInput(ns("focus"),
                  tagList("Focus on group", info_tip(
                    "Zooms the map to just that group's points.")),
                  choices = c("(all data)" = "__all__", lv))
    })

    observeEvent(input$swap, {
      lo <- input$lon; la <- input$lat
      updateSelectInput(session, "lon", selected = la %||% "")
      updateSelectInput(session, "lat", selected = lo %||% "")
    })

    # --- settings list (slot_params analog) ----------------------------------
    map_params <- function() {
      choro <- identical(input$map_type %||% "points", "choro")
      base <- list(
        lon        = input$lon %||% "",
        lat        = input$lat %||% "",
        basemap    = input$basemap %||% "CartoDB.Positron",
        color      = input$color %||% "__none__",
        palette    = input$palette %||% "auto",
        color_hex  = input$color_hex %||% UF_BLUE,
        size_by    = input$size_by %||% "__none__",
        size       = input[["size"]] %||% 6,
        size_scale = input$size_scale %||% "linear",
        size_legend = isTRUE(input$size_legend %||% TRUE),
        alpha      = input$alpha %||% 0.8,
        popup_cols = input$popup_cols,
        label_col  = input$label_col %||% "__none__",
        cluster    = input$cluster %||% "auto",
        legend     = isTRUE(input$legend %||% TRUE),
        color_scale = input$color_scale %||% "linear",
        group_by   = input$group_by %||% "__none__",
        heatmap    = isTRUE(input$heatmap) && !choro,
        heat_by    = input$heat_by %||% "__none__",
        heat_radius = input$heat_radius %||% MAP_HEAT_RADIUS,
        scalebar   = isTRUE(input$scalebar %||% TRUE),
        title      = input$title
      )
      if (!choro) return(base)
      # Choropleth mode: the GeoJSON's presence is what routes the builder.
      # modifyList (not c()) so the choro-mode styling controls OVERRIDE the
      # hidden points-mode palette/scale/legend inputs -- c() would keep the
      # stale points values (first duplicate name wins).
      utils::modifyList(base, list(
        geojson      = geojson_state(),
        region_key   = input$region_key %||% "__none__",
        region_prop  = input$region_prop %||% "",
        region_value = input$region_value %||% "__none__",
        region_agg   = input$region_agg %||% "mean",
        palette      = input$choro_palette %||% "auto",
        color_scale  = input$choro_scale %||% "linear",
        legend       = isTRUE(input$choro_legend %||% TRUE),
        heatmap      = FALSE))
    }
    # Debounced so slider drags rebuild the widget once, not per tick.
    params_d <- debounce(reactive(map_params()), 350)

    # --- view preservation ----------------------------------------------------
    # leaflet publishes input$<id>_center / _zoom on every pan/zoom; remember
    # them and re-apply via p$view so a settings change never re-fits the map.
    view_state <- reactiveVal(NULL)
    observeEvent({ input$map_center; input$map_zoom }, {
      ctr <- input$map_center
      if (!is.null(ctr) && !is.null(input$map_zoom))
        view_state(list(lng = ctr$lng, lat = ctr$lat, zoom = input$map_zoom))
    })
    # The VISIBLE bounds, tracked separately for the exports. A snapshot is
    # rendered on a fixed 1200x800 canvas, which is a different size and shape
    # from the on-screen pane -- and center+zoom means "this scale", not "this
    # area", so replaying it on a bigger canvas silently shows far more ground
    # (an ocean of empty map around the points). Bounds say "cover this area",
    # The map pane's real pixel size, published by Shiny for every output.
    # Snapshotting at exactly this size and replaying centre+zoom reproduces
    # the user's view EXACTLY -- zoom fixes the scale, so identical pixels give
    # identical ground. This is the ONLY thing that makes the export match the
    # screen; a fixed export canvas necessarily frames a different area.
    #
    # Do NOT try to do this from the visible bounds instead: leaflet does not
    # publish input$<id>_bounds here (verified NULL even after zoom events), and
    # fitBounds would anyway snap to whole zoom levels and pad whichever axis
    # doesn't match the canvas.
    pane_px <- reactive({
      cd <- session$clientData
      w  <- cd[[paste0("output_", ns("map"), "_width")]]
      h  <- cd[[paste0("output_", ns("map"), "_height")]]
      if (is.numeric(w) && is.numeric(h) && w > 50 && h > 50)
        list(width = round(w), height = round(h)) else NULL
    })

    # How the exports should be framed. With a known pane size: that size, the
    # user's centre+zoom, and a 2x pixel ratio so the image is crisp rather than
    # merely screen-sized. Without one (the Map tab was never opened, so the
    # browser never reported a size) there is nothing to match, so fall back to
    # the default canvas fitted to the data.
    # zoom stays 1 (device pixel ratio 1): at DPR 2 the CartoDB basemap returns
    # roughly half its tiles blank, leaving grey rectangles in the snapshot. The
    # pane's own pixel size is the right resolution anyway -- it is literally
    # what the user is looking at.
    snapshot_spec <- function(p) {
      px <- pane_px()
      if (is.null(px)) {
        attr(p, "snap") <- list(width = MAP_PNG_W, height = MAP_PNG_H, zoom = 1)
      } else {
        attr(p, "snap") <- list(width = px$width, height = px$height, zoom = 1)
      }
      p
    }
    # priority = 10: the resets must run BEFORE output$map's re-render in the
    # same flush, or a dataset swap that keeps the lon/lat column names could
    # re-render with the previous dataset's stale view.
    observeEvent(data_in(), view_state(NULL), ignoreNULL = FALSE,
                 priority = 10)
    observeEvent({ input$lon; input$lat }, view_state(NULL),
                 ignoreInit = TRUE, priority = 10)
    # Switching Points <-> Shaded regions is a different geography: without
    # this reset the choropleth inherits the points view (its own fitBounds
    # would be unreachable), and vice versa.
    observeEvent(input$map_type, view_state(NULL),
                 ignoreInit = TRUE, priority = 10)

    # The built widget, shared between the pane and the choropleth diagnostics.
    current_widget <- reactive({
      df <- data_in()
      req(is.data.frame(df))
      p <- params_d()
      p$view <- isolate(view_state())   # isolate: panning must not re-render
      build_leaflet_map(df, p)
    })

    output$map <- leaflet::renderLeaflet({
      w <- current_widget()
      validate(need(!is.null(w), if (identical(input$map_type, "choro"))
        "Upload boundaries, then pick the region property, key column, and value."
        else "Choose the longitude and latitude columns to draw the map."))
      w
    })

    # View-only proxy moves: never rebuild layers, just fly the camera.
    fly_to <- function(rows_of = NULL) {
      df <- data_in()
      req(is.data.frame(df))
      lon <- map_col(input$lon); lat <- map_col(input$lat)
      req(!is.null(lon), !is.null(lat), !identical(lon, lat),
          all(c(lon, lat) %in% names(df)),
          is.numeric(df[[lon]]), is.numeric(df[[lat]]))
      cc <- clean_coords(df, lon, lat)
      d  <- cc$data
      if (!is.null(rows_of)) d <- d[rows_of(d), , drop = FALSE]
      req(nrow(d) > 0)
      bb <- map_bbox(d, lon, lat)
      view_state(NULL)
      leaflet::leafletProxy("map", session) |>
        leaflet::flyToBounds(bb$lng1, bb$lat1, bb$lng2, bb$lat2)
    }
    observeEvent(input$zoom_data, fly_to())
    # Focus-on-group: zoom to one layer group's points. "(all data)" is a
    # deliberate no-op -- it's also the value the picker RESETS to whenever it
    # re-renders (data/group change), and flying on it would yank the user's
    # preserved view; the "Zoom to data" button covers zooming out.
    observeEvent(input$focus, {
      f <- input$focus
      if (is.null(f) || identical(f, "__all__")) return(invisible(NULL))
      gv <- map_col(input$group_by)
      req(!is.null(gv))
      fly_to(function(d) map_group_vals(d[[gv]]) == f)
    }, ignoreInit = TRUE)

    output$ui_hint <- renderUI({
      df <- data_in(); req(is.data.frame(df))
      if (identical(input$map_type, "choro")) {
        # Points hints (coords, clustering...) don't apply to a choropleth.
        if (isTRUE(input$heatmap) &&
            !requireNamespace("leaflet.extras", quietly = TRUE)) return(NULL)
        return(NULL)
      }
      msgs <- map_hint(df, params_d())   # debounced: the hint scans all rows
      if (isTRUE(input$heatmap) &&
          !requireNamespace("leaflet.extras", quietly = TRUE))
        msgs <- paste(c(msgs, paste0(
          "The density layer needs the optional leaflet.extras package, so ",
          "it is not drawn (and the generated code needs it installed).")),
          collapse = " ")
      if (is.null(msgs)) return(NULL)
      div(class = "alert alert-warning py-1 px-2 small mb-2", role = "alert",
          icon("triangle-exclamation"),
          HTML(paste0(" ", htmltools::htmlEscape(msgs))))
    })

    output$rowcount <- renderUI({
      df <- data_in(); req(is.data.frame(df))
      if (identical(input$map_type, "choro")) {
        w <- current_widget(); diag <- attr(w, "choro_diag")
        if (is.null(diag)) return(NULL)
        return(span(class = "text-muted small",
                    sprintf("%d of %d regions matched your data.",
                            diag$n_matched, diag$n_features)))
      }
      lon <- map_col(input$lon); lat <- map_col(input$lat)
      if (is.null(lon) || is.null(lat) || identical(lon, lat) ||
          !all(c(lon, lat) %in% names(df)) ||
          !is.numeric(df[[lon]]) || !is.numeric(df[[lat]])) return(NULL)
      cc <- clean_coords(df, lon, lat)
      span(class = "text-muted small",
           sprintf("Showing %s of %s rows with usable coordinates.",
                   format(nrow(cc$data), big.mark = ","),
                   format(cc$n_total, big.mark = ",")))
    })

    output$code <- renderText({
      df <- data_in()
      req(is.data.frame(df))
      generate_map_code(df, params_d())   # debounced: code-gen rescans rows
    })

    # --- downloads -------------------------------------------------------------
    safe_stem <- reactive({
      stem <- gsub("[^A-Za-z0-9._-]+", "_", input$fname %||% "")
      if (nzchar(stem)) stem else "map-export"
    })
    # Rebuild the widget fresh at click time with the CURRENT view: what you
    # see is what you save, at the on-screen pane size -- see snapshot_spec().
    export_widget <- function() {
      df <- data_in()
      validate(need(is.data.frame(df), "No data to map yet."))
      p <- map_params()
      p$view <- view_state()
      p <- snapshot_spec(p)
      w <- build_leaflet_map(df, p)
      if (!is.null(w)) attr(w, "fox_snapshot") <- attr(p, "snap")
      validate(need(!is.null(w), if (identical(input$map_type, "choro"))
        "Upload boundaries and pick the region property, key column, and value first."
        else "Choose the longitude and latitude columns first."))
      w
    }

    output$dl_html <- downloadHandler(
      filename = function() paste0(safe_stem(), ".html"),
      content = function(file) {
        w <- export_widget()
        tryCatch(save_map_html(w, file), error = function(e) {
          showNotification(paste("Map export failed:", conditionMessage(e)),
                           type = "error", duration = 8)
          validate(need(FALSE, conditionMessage(e)))
        })
      }
    )

    output$png_ui <- renderUI({
      if (map_snapshot_ok()) {
        downloadButton(ns("dl_png"), "Static image (.png)",
                       class = "btn-outline-secondary w-100")
      } else {
        helpText("PNG snapshots need the optional webshot2 package plus a ",
                 "Chrome or Edge browser.")
      }
    })
    output$dl_png <- downloadHandler(
      filename = function() paste0(safe_stem(), ".png"),
      content = function(file) {
        w <- export_widget()
        withProgress(message = "Rendering PNG (headless browser)...",
                     value = 0.4, {
          sp <- attr(w, "fox_snapshot") %||%
            list(width = MAP_PNG_W, height = MAP_PNG_H, zoom = 1)
          tryCatch(save_map_png(w, file, width = sp$width, height = sp$height,
                                zoom = sp$zoom %||% 1), error = function(e) {
            showNotification(paste("PNG export failed:", conditionMessage(e)),
                             type = "error", duration = 8)
            validate(need(FALSE, conditionMessage(e)))
          })
          incProgress(0.6)
        })
      }
    )

    # Return the live widget and its matching code as
    # list(maps = <widgets>, code = <strings>) -- mod_visualize's contract,
    # ready for a future Report section. Lazy: only computed when read.
    reactive({
      df <- data_in()
      if (!is.data.frame(df)) return(list(maps = list(), code = list()))
      p <- map_params()
      p$view <- view_state()
      p <- snapshot_spec(p)             # report snapshot: match the on-screen view
      w <- tryCatch(build_leaflet_map(df, p), error = function(e) NULL)
      if (is.null(w)) return(list(maps = list(), code = list()))
      attr(w, "fox_snapshot") <- attr(p, "snap")
      list(maps = list(w),
           code = list(tryCatch(generate_map_code(df, p),
                                error = function(e) NA_character_)))
    })
  })
}
