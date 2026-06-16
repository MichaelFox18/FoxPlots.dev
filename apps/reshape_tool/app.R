# ============================================================
# Reshape Tool — a focused import → reshape → export mini-app
# ============================================================
# Assembled from the shared kit: the same mod_reshape that lives inside the
# full Data Explorer, bracketed by mod_import and mod_export. Wiring is
# Pattern A — each stage returns a reactive that feeds the next.
#
# Run from the projects/ root (launch via PowerShell, not git-bash):
#   setwd("C:/Users/michaelfox1/Desktop/FoxPlots")
#   shiny::runApp("apps/reshape_tool")

library(shiny)
library(bslib)
library(here)

# Lift the 5 MB default upload cap so larger CSVs go through.
options(shiny.maxRequestSize = 250 * 1024^2)

source(here::here("R", "components.R"))
source(here::here("R", "helpers_io.R"))
source(here::here("R", "helpers_clean.R"))
source(here::here("R", "helpers_filter.R"))
source(here::here("R", "helpers_stats.R"))
source(here::here("R", "helpers_reshape.R"))
source(here::here("modules", "mod_import.R"))
source(here::here("modules", "mod_reshape.R"))
source(here::here("modules", "mod_export.R"))

ui <- page_navbar(
  title        = uf_title("Reshape Tool"),
  window_title = "UF/IFAS Reshape Tool",
  theme        = uf_theme(),
  fillable     = "reshape",   # the reshape preview fills; import/export scroll

  nav_panel(tagList(icon("file-arrow-up"), " Import"),  value = "import",
            importUI("imp")),
  nav_panel(tagList(icon("table-cells"),   " Reshape"), value = "reshape",
            reshapeUI("rs")),
  nav_panel(tagList(icon("file-export"),   " Export"),  value = "export",
            exportUI("ex"))
)

server <- function(input, output, session) {
  imported <- importServer("imp")             # -> reactive(data | NULL)
  reshaped <- reshapeServer("rs", imported)   # -> reactive(reshaped data)
  exportServer("ex", reshaped)                # terminal stage
}

shinyApp(ui, server)
