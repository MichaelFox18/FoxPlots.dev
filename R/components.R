# ============================================================
# components.R -- shared UF/IFAS look-and-feel
# ============================================================
# The ONE source of truth for the kit's branding: UF colours, the bslib
# theme, the logo, and small shared UI atoms. Every app pulls its theme
# from here so the Data Explorer and the standalone tools look identical.
#
# Function definitions and plain constants only -- nothing here runs until
# an app calls it, so it is safe to source in tests. Requires `shiny` and
# `bslib` to be attached by the caller (apps already do).

# --- Brand palette ----------------------------------------------------------
# UF brand blue. The original DataExplorerApp used #003087; the modular plan
# floated #0021A5. We match the existing app here so the look is identical --
# flip the whole kit by changing this one line.
UF_BLUE   <- "#003087"
UF_ORANGE <- "#FA4616"
# Categorical accent palette for charts (blue, orange, green, purple).
UF_COLORS <- c(UF_BLUE, UF_ORANGE, "#2ca25f", "#8856a7")

#' The canonical UF/IFAS bslib theme.
#'
#' flatly base (Lato font) with a UF-blue navbar and orange secondary --
#' identical to the existing Data Explorer. Pass to a page_*()'s `theme` arg.
#' @return a bslib::bs_theme object.
#' @export
uf_theme <- function() {
  bslib::bs_theme(
    bootswatch  = "flatly",
    primary     = UF_BLUE,
    secondary   = UF_ORANGE,
    font_scale  = 0.95,
    "navbar-bg" = UF_BLUE
  ) |>
    bslib::bs_add_rules(
      # Keep Shiny's "no data yet" validation messages on one line. Otherwise,
      # while a tab's width is still settling during a switch, the message
      # briefly wraps one letter per row before reflowing.
      ".shiny-output-error-validation { white-space: nowrap; }"
    )
}

#' Locate the white IFAS logo and return it as an inlined base64 data URI.
#'
#' Inlining (rather than a www/ `src`) means the logo renders no matter how the
#' app is launched. Resolves the installed package's `inst/www/` via
#' system.file(), with dev-mode fallbacks; returns NULL (text-only title) if the
#' file genuinely can't be found, so an app never errors on startup. base64enc
#' ships with Shiny, so it is always available.
#' @return a data: URI string, or NULL.
#' @export
uf_logo_uri <- function() {
  cands <- c(
    system.file("www", "IFAS-White.png", package = "foxplots"),
    if (requireNamespace("here", quietly = TRUE))
      tryCatch(here::here("inst", "www", "IFAS-White.png"),
               error = function(e) NA_character_)
    else NA_character_,
    "inst/www/IFAS-White.png"
  )
  cands <- cands[!is.na(cands) & nzchar(cands)]
  f <- cands[file.exists(cands)]
  if (length(f)) base64enc::dataURI(file = f[[1]], mime = "image/png") else NULL
}

#' A navbar title: white IFAS logo + app name, matching the Data Explorer.
#'
#' @param text The app title shown next to the logo.
#' @param logo Optional pre-resolved data URI; defaults to uf_logo_uri().
#' @return a shiny tag suitable for a page_navbar(title = ).
#' @export
uf_title <- function(text, logo = uf_logo_uri()) {
  shiny::tags$span(
    if (!is.null(logo))
      shiny::tags$img(src = logo, height = "51px", alt = "UF/IFAS",
                      style = "margin-right: 12px; vertical-align: middle;"),
    shiny::tags$span(text, style = "vertical-align: middle;")
  )
}

#' Custom label if the user typed one, else a default -- used wherever an
#' optional text input overrides an auto-generated label (axis titles, etc.).
#' @param custom The user-entered string (may be NULL/blank).
#' @param default The fallback label.
#' @return A single string: `custom` when it is non-NULL and non-blank after
#'   trimming, otherwise `default`.
#' @export
label_or <- function(custom, default) {
  if (!is.null(custom) && nzchar(trimws(custom))) custom else default
}

# Clipboard helper for "copy the R code" buttons. A button calls
# DEcopy('<output-id>', this); the id is the (namespaced) verbatim output's DOM
# id. Inject once via tags$script(HTML(copy_js)) -- mod_visualize does this.
copy_js <- "
function DEcopy(id, btn){
  var el = document.getElementById(id);
  if(!el) return;
  var txt = el.innerText || el.textContent || '';
  navigator.clipboard.writeText(txt).then(function(){
    if(btn){ var o = btn.innerHTML; btn.innerHTML = 'Copied!'; setTimeout(function(){ btn.innerHTML = o; }, 1200); }
  });
}
"

#' A small grey "?" tooltip icon for inline help.
#'
#' @param ... Tooltip content (passed to bslib::tooltip).
#' @param placement Tooltip placement ("right", "top", ...).
#' @return a bslib tooltip tag.
#' @export
info_tip <- function(..., placement = "right") {
  bslib::tooltip(
    shiny::tags$span(
      shiny::icon("circle-question"),
      style = "color:#aaa; cursor:help; margin-left:5px; font-size:0.82em;"
    ),
    ...,
    placement = placement
  )
}

# A compact "About" landing panel for the standalone mini-apps, mirroring the
# Data Explorer's About tab. `steps` is a character vector rendered as a
# numbered list. Internal (not exported).
about_nav_panel <- function(title, lead, steps) {
  bslib::nav_panel(
    shiny::tagList(shiny::icon("circle-info"), " About"), value = "about",
    bslib::card(
      bslib::card_header(shiny::icon("circle-info"), " ", title),
      shiny::tags$p(lead),
      shiny::tags$ol(lapply(steps, shiny::tags$li)),
      shiny::tags$p(class = "text-muted small",
        "Hover the grey ? icons for inline help on any control.")))
}
