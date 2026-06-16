# ============================================================
# Data Explorer — the full pipeline, assembled from the kit
# ============================================================
# Import → Reshape → Summarize → Visualize → Compare → Regression → Export, each
# a shared module wired Pattern A (every stage returns a reactive feeding the next).
#
# Run from the projects/ root (launch via PowerShell, not git-bash):
#   setwd("C:/Users/michaelfox1/Desktop/FoxPlots")
#   shiny::runApp("apps/data_explorer")

library(shiny)
library(bslib)
library(here)
library(ggplot2)
library(plotly)
library(colourpicker)

options(shiny.maxRequestSize = 250 * 1024^2)   # lift the 5 MB upload cap

source(here::here("R", "components.R"))
source(here::here("R", "helpers_io.R"))
source(here::here("R", "helpers_clean.R"))
source(here::here("R", "helpers_filter.R"))
source(here::here("R", "helpers_stats.R"))
source(here::here("R", "helpers_plot.R"))
source(here::here("R", "helpers_model.R"))
source(here::here("R", "helpers_reshape.R"))
source(here::here("R", "helpers_compare.R"))
source(here::here("modules", "mod_import.R"))
source(here::here("modules", "mod_reshape.R"))
source(here::here("modules", "mod_summarize.R"))
source(here::here("modules", "mod_visualize.R"))
source(here::here("modules", "mod_compare.R"))
source(here::here("modules", "mod_regression.R"))
source(here::here("modules", "mod_export.R"))

# --- About tab (static orientation) -----------------------------------------
about_panel <- nav_panel(
  title = tagList(icon("circle-info"), " About"),
  value = "about",
  layout_columns(
    col_widths = c(6, 6),
    card(
      card_header(icon("compass"), " What is Data Explorer?"),
      tags$div(
        class = "px-2",
        tags$p("A point-and-click tool for UF/IFAS students to import, clean, ",
               "reshape, summarize, visualize, model, and export tabular data — ",
               "no R code required."),
        tags$p(class = "mb-1", tags$b("The workflow runs left to right:")),
        tags$ol(
          class = "px-3",
          tags$li(tags$b("Import"), " — upload CSV/Excel/TSV/RDS or load an example, ",
                  "then run a Data Health check, recast column types, and filter ",
                  "to just the rows you want."),
          tags$li(tags$b("Reshape"), " — stack, split, transpose, sort, or subset ",
                  "(optional)."),
          tags$li(tags$b("Summarize"), " — count, mean, median, mode, min, max, SD, ",
                  "SE, and IQR by group, or category proportions with confidence ",
                  "intervals."),
          tags$li(tags$b("Visualize"), " — up to four charts at once (scatter, line, ",
                  "bar, histogram, box, pie, correlation heatmap) with copy-ready ",
                  "ggplot2 code."),
          tags$li(tags$b("Compare Groups"), " — t-test / ANOVA (or non-parametric) ",
                  "across groups, or chi-square between two categories, with ",
                  "assumption checks and effect sizes."),
          tags$li(tags$b("Regression"), " — fit linear, multiple, or polynomial ",
                  "models with diagnostics and a plain-English interpretation."),
          tags$li(tags$b("Export"), " — download the data, the charts, the summary, ",
                  "and the model results.")
        )
      )
    ),
    card(
      card_header(icon("lightbulb"), " Tips"),
      tags$ul(
        class = "px-3",
        tags$li("Each tab feeds the next: what you import and reshape is what gets ",
                "summarized, visualized, modeled, and exported."),
        tags$li("Leave Reshape on “None” to pass data straight through unchanged."),
        tags$li("Hover the ", icon("circle-question"), " icons for plain-English help."),
        tags$li("On the Visualize tab, use the ", icon("expand"),
                " full-screen button on any chart to see it in detail — handy for ",
                "faceted (small-multiple) charts."),
        tags$li("Numbers stored as text, missing-value markers, and duplicate rows ",
                "are caught by Data Health on the Import tab — fixes are reversible.")
      )
    )
  )
)

ui <- page_navbar(
  title        = uf_title("Data Explorer"),
  window_title = "UF/IFAS Data Explorer",
  theme        = uf_theme(),
  fillable     = c("reshape", "visualize", "compare", "regression"),

  about_panel,
  nav_panel(tagList(icon("file-arrow-up"), " Import"),    value = "import",
            importUI("imp")),
  nav_panel(tagList(icon("table-cells"),   " Reshape"),   value = "reshape",
            reshapeUI("rs")),
  nav_panel(tagList(icon("layer-group"),   " Summarize"), value = "summarize",
            summarizeUI("sm")),
  nav_panel(tagList(icon("chart-line"),    " Visualize"),  value = "visualize",
            visualizeUI("viz")),
  nav_panel(tagList(icon("flask-vial"),    " Compare Groups"), value = "compare",
            compareUI("cmp")),
  nav_panel(tagList(icon("chart-simple"),  " Regression"), value = "regression",
            regressionUI("reg")),
  nav_panel(tagList(icon("file-export"),   " Export"),     value = "export",
            exportUI("ex"))
)

server <- function(input, output, session) {
  imported  <- importServer("imp")              # Import    -> reactive(data | NULL)
  working   <- reshapeServer("rs", imported)    # Reshape   -> reactive(working data)
  summary_t <- summarizeServer("sm", working)   # Summarize -> reactive(summary table)
  plots     <- visualizeServer("viz", working)  # Visualize -> reactive(list of ggplots)
  compareServer("cmp", working)                 # Compare    reads the working data
  model     <- regressionServer("reg", working) # Regression -> reactive(fitted lm)
  exportServer("ex", working, plots = plots, model = model,  # data + charts +
               summary_tbl = summary_t)                      # model + summary
}

shinyApp(ui, server)
