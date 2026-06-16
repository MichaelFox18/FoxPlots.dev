# ============================================================
# dev/run_compare.R — boot mod_compare on its own
# ============================================================
# Exercises the Compare Groups module in isolation with built-in sample data,
# wearing the shared UF/IFAS theme. Run from the projects/ root (launch via
# PowerShell, not git-bash):
#
#   setwd("C:/Users/michaelfox1/Desktop/FoxPlots")
#   shiny::runApp("dev/run_compare.R")
#
# Two samples cover both modes:
#   iris    — numeric outcome by group (Sepal.Length by Species -> ANOVA)
#   mtcars  — two low-cardinality columns for the categorical mode (cyl × gear)

library(shiny)
library(bslib)
library(here)
library(ggplot2)

source(here::here("R", "components.R"))
source(here::here("R", "helpers_stats.R"))
source(here::here("R", "helpers_plot.R"))
source(here::here("R", "helpers_compare.R"))
source(here::here("modules", "mod_compare.R"))

samples <- list(
  "iris  (number by group — try ANOVA)"     = as.data.frame(iris),
  "mtcars  (two categories — try chi-square)" = local({
    d <- as.data.frame(mtcars); d$car <- rownames(d); rownames(d) <- NULL; d
  })
)

ui <- page_navbar(
  title        = uf_title("Compare Groups"),
  window_title = "Compare Groups — dev harness",
  theme        = uf_theme(),
  fillable     = TRUE,

  nav_panel(
    title = tagList(icon("flask-vial"), " Compare Groups"),
    compareUI("cmp")
  ),

  nav_spacer(),
  nav_item(
    tags$div(
      class = "d-flex align-items-center gap-2",
      tags$span("Sample data", class = "navbar-text text-white-50 small"),
      selectInput("dataset", NULL, choices = names(samples), width = "300px")
    )
  )
)

server <- function(input, output, session) {
  data_in <- reactive({ req(input$dataset); samples[[input$dataset]] })
  compareServer("cmp", data_in)
}

shinyApp(ui, server)
