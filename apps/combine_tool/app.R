# ============================================================
# Combine Tool — import two tables, combine them, export
# ============================================================
# The two-table counterpart to reshape_tool: a full mod_import for each of
# the Left and Right tables (upload/clean/recast either), then mod_combine
# (concatenate / join / update / compare), then mod_export. Pattern A: each
# importer returns a data reactive; both feed combineServer; its result feeds
# Export.
#
# Run from the projects/ root (launch via PowerShell, not git-bash):
#   setwd("C:/Users/michaelfox1/Desktop/FoxPlots")
#   shiny::runApp("apps/combine_tool")

library(shiny)
library(bslib)
library(here)

options(shiny.maxRequestSize = 250 * 1024^2)

source(here::here("R", "components.R"))
source(here::here("R", "helpers_io.R"))
source(here::here("R", "helpers_clean.R"))
source(here::here("R", "helpers_filter.R"))
source(here::here("R", "helpers_stats.R"))
source(here::here("R", "helpers_combine.R"))
source(here::here("modules", "mod_import.R"))
source(here::here("modules", "mod_combine.R"))
source(here::here("modules", "mod_export.R"))

# Join-friendly built-in examples: band_members + band_instruments share `name`
# (the classic join demo); the two mtcars halves demo Concatenate.
mt <- as.data.frame(mtcars); mt$car <- rownames(mt); rownames(mt) <- NULL
combine_examples <- list(
  band_members     = as.data.frame(dplyr::band_members),
  band_instruments = as.data.frame(dplyr::band_instruments),
  mtcars_top       = utils::head(mt, 16),
  mtcars_bottom    = utils::tail(mt, 16)
)
ex_choices <- c("band_members (name, band)"      = "band_members",
                "band_instruments (name, plays)" = "band_instruments",
                "mtcars (rows 1–16)"             = "mtcars_top",
                "mtcars (rows 17–32)"            = "mtcars_bottom")

ui <- page_navbar(
  title        = uf_title("Combine Tool"),
  window_title = "UF/IFAS Combine Tool",
  theme        = uf_theme(),
  fillable     = "combine",

  nav_panel(tagList(icon("table-columns"), " Left table"),  value = "left",
            importUI("left", ex_choices)),
  nav_panel(tagList(icon("table-columns"), " Right table"), value = "right",
            importUI("right", ex_choices)),
  nav_panel(tagList(icon("object-group"),  " Combine"),     value = "combine",
            combineUI("cmb")),
  nav_panel(tagList(icon("file-export"),   " Export"),      value = "export",
            exportUI("ex"))
)

server <- function(input, output, session) {
  left_data  <- importServer("left",  examples = combine_examples)
  right_data <- importServer("right", examples = combine_examples)
  combined   <- combineServer("cmb", left_data, right_data)
  exportServer("ex", combined)
}

shinyApp(ui, server)
