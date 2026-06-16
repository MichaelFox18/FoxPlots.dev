# ============================================================
# dev/run_reshape.R — boot mod_reshape on its own
# ============================================================
# Exercises the reshape module in isolation with built-in sample data,
# wearing the shared UF/IFAS theme so it looks like the real app. Run
# from the projects/ root (launch via PowerShell, not git-bash):
#
#   setwd("C:/Users/michaelfox1/Desktop/FoxPlots")
#   shiny::runApp("dev/run_reshape.R")
#
# Two samples are provided so both directions work out of the box:
#   relig_income    is WIDE  -> try Stack
#   fish_encounters is LONG  -> try Split

library(shiny)
library(bslib)
library(here)

source(here::here("R", "components.R"))
source(here::here("R", "helpers_reshape.R"))
source(here::here("modules", "mod_reshape.R"))

samples <- list(
  "relig_income  (wide — try Stack)"    = as.data.frame(tidyr::relig_income),
  "fish_encounters  (long — try Split)" = as.data.frame(tidyr::fish_encounters)
)

ui <- page_navbar(
  title        = uf_title("Reshape"),
  window_title = "Reshape — dev harness",
  theme        = uf_theme(),
  fillable     = TRUE,

  nav_panel(
    title = tagList(icon("table-cells"), " Reshape"),
    reshapeUI("rs")
  ),

  # Sample-data picker pushed to the right of the navbar. In the real app this
  # is the upstream Import stage; here it just feeds the module its `data_in`.
  nav_spacer(),
  nav_item(
    tags$div(
      class = "d-flex align-items-center gap-2",
      tags$span("Sample data", class = "navbar-text text-white-50 small"),
      selectInput("dataset", NULL, choices = names(samples), width = "260px")
    )
  )
)

server <- function(input, output, session) {
  data_in <- reactive({
    req(input$dataset)
    samples[[input$dataset]]
  })

  # Pattern A: the module returns its result; a real app would hand this
  # `reshaped` reactive to the next stage. Here it just proves the contract.
  reshaped <- reshapeServer("rs", data_in)
}

shinyApp(ui, server)
