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
      uiOutput(ns("ui_lon")),
      uiOutput(ns("ui_lat")),
      actionLink(ns("swap"), tagList(icon("right-left"), " Swap lat / lon"),
                 class = "small mb-2"),
      uiOutput(ns("ui_hint")),
      hr(),
      h6("Style"),
      selectInput(ns("basemap"),
                  tagList("Basemap", info_tip(
                    "The background map. Tiles load from the internet, so an ",
                    "offline machine shows a grey background.")),
                  choices = MAP_BASEMAPS),
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
        sprintf("!input['%s'] || input['%s'] == '__none__'",
                ns("size_by"), ns("size_by")),
        sliderInput(ns("size"), "Point size", min = 2, max = 12, value = 6,
                    step = 1)),
      sliderInput(ns("alpha"), "Opacity", min = 0.1, max = 1, value = 0.8,
                  step = 0.05),
      radioButtons(ns("cluster"),
                   tagList("Cluster nearby points", info_tip(
                     "Groups close-together markers into expandable bubbles. ",
                     "Auto turns clustering on above ", MAP_CLUSTER_AUTO,
                     " points. With layer groups, clustering happens within ",
                     "each group.")),
                   choices = c("Auto" = "auto", "On" = "on", "Off" = "off"),
                   selected = "auto", inline = TRUE),
      hr(),
      h6("Layer groups"),
      uiOutput(ns("ui_group")),
      uiOutput(ns("ui_focus")),
      hr(),
      h6("Popups & labels"),
      uiOutput(ns("ui_popup")),
      uiOutput(ns("ui_label")),
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
      list(
        lon        = input$lon %||% "",
        lat        = input$lat %||% "",
        basemap    = input$basemap %||% "CartoDB.Positron",
        color      = input$color %||% "__none__",
        palette    = input$palette %||% "auto",
        color_hex  = input$color_hex %||% UF_BLUE,
        size_by    = input$size_by %||% "__none__",
        size       = input[["size"]] %||% 6,
        alpha      = input$alpha %||% 0.8,
        popup_cols = input$popup_cols,
        label_col  = input$label_col %||% "__none__",
        cluster    = input$cluster %||% "auto",
        legend     = isTRUE(input$legend %||% TRUE),
        color_scale = input$color_scale %||% "linear",
        group_by   = input$group_by %||% "__none__",
        title      = input$title
      )
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
    # priority = 10: the resets must run BEFORE output$map's re-render in the
    # same flush, or a dataset swap that keeps the lon/lat column names could
    # re-render with the previous dataset's stale view.
    observeEvent(data_in(), view_state(NULL), ignoreNULL = FALSE,
                 priority = 10)
    observeEvent({ input$lon; input$lat }, view_state(NULL),
                 ignoreInit = TRUE, priority = 10)

    output$map <- leaflet::renderLeaflet({
      df <- data_in()
      req(is.data.frame(df))
      p <- params_d()
      p$view <- isolate(view_state())   # isolate: panning must not re-render
      w <- build_leaflet_map(df, p)
      validate(need(!is.null(w),
                    "Choose the longitude and latitude columns to draw the map."))
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
      msg <- map_hint(df, params_d())   # debounced: the hint scans all rows
      if (is.null(msg)) return(NULL)
      div(class = "alert alert-warning py-1 px-2 small mb-2", role = "alert",
          icon("triangle-exclamation"),
          HTML(paste0(" ", htmltools::htmlEscape(msg))))
    })

    output$rowcount <- renderUI({
      df <- data_in(); req(is.data.frame(df))
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
    # see is what you save.
    export_widget <- function() {
      df <- data_in()
      validate(need(is.data.frame(df), "No data to map yet."))
      p <- map_params()
      p$view <- view_state()
      w <- build_leaflet_map(df, p)
      validate(need(!is.null(w),
                    "Choose the longitude and latitude columns first."))
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
          tryCatch(save_map_png(w, file), error = function(e) {
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
      w <- tryCatch(build_leaflet_map(df, p), error = function(e) NULL)
      if (is.null(w)) return(list(maps = list(), code = list()))
      list(maps = list(w),
           code = list(tryCatch(generate_map_code(df, p),
                                error = function(e) NA_character_)))
    })
  })
}
