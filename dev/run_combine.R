# ============================================================
# dev/run_combine.R — boot mod_combine on its own
# ============================================================
# Exercises the two-table combine module with built-in sample tables.
# Pick a Left and a Right table, then Concatenate / Join / Update / Compare.
# Run from the projects/ root (launch via PowerShell, not git-bash):
#   setwd("C:/Users/michaelfox1/Desktop/FoxPlots")
#   shiny::runApp("dev/run_combine.R")
#
# band_members (name, band) + band_instruments (name, plays) are the classic
# join example (key = name); the two mtcars halves demo Concatenate.

library(shiny)
library(bslib)
library(here)

source(here::here("R", "components.R"))
source(here::here("R", "helpers_combine.R"))
source(here::here("modules", "mod_combine.R"))

mt <- as.data.frame(mtcars); mt$car <- rownames(mt); rownames(mt) <- NULL
samples <- list(
  "band_members  (name, band)"      = as.data.frame(dplyr::band_members),
  "band_instruments  (name, plays)" = as.data.frame(dplyr::band_instruments),
  "mtcars  (rows 1–16)"             = utils::head(mt, 16),
  "mtcars  (rows 17–32)"            = utils::tail(mt, 16)
)

ui <- page_navbar(
  title        = uf_title("Combine"),
  window_title = "mod_combine — dev harness",
  theme        = uf_theme(),
  fillable     = TRUE,

  nav_panel(tagList(icon("object-group"), " Combine"), combineUI("cmb")),
  nav_spacer(),
  nav_item(
    tags$div(
      class = "d-flex align-items-center gap-2",
      tags$span("Left", class = "navbar-text text-white-50 small"),
      selectInput("left", NULL, choices = names(samples),
                  selected = names(samples)[1], width = "210px"),
      tags$span("Right", class = "navbar-text text-white-50 small"),
      selectInput("right", NULL, choices = names(samples),
                  selected = names(samples)[2], width = "210px")
    )
  )
)

server <- function(input, output, session) {
  left  <- reactive({ req(input$left);  samples[[input$left]] })
  right <- reactive({ req(input$right); samples[[input$right]] })
  combined <- combineServer("cmb", left, right)
}

shinyApp(ui, server)
