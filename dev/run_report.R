# ============================================================
# run_report.R — boot mod_report alone with a pre-built session
# ============================================================
# Exercises the report module in isolation: feeds it static, pre-made artifacts
# (a summary, two charts + their code, a group comparison, and a model) so you
# can click "Download report" and inspect the HTML without the full pipeline.
#
# Launch via PowerShell (not git-bash):
#   shiny::runApp("dev/run_report.R")

library(shiny)
library(bslib)
library(here)
library(ggplot2)

for (f in c("components", "helpers_clean", "helpers_stats", "helpers_plot",
            "helpers_model", "helpers_compare", "helpers_report"))
  source(here::here("R", paste0(f, ".R")))
source(here::here("modules", "mod_report.R"))

# --- a pre-built "session" over mtcars --------------------------------------
df       <- mtcars
plot1    <- build_full_plot(df, list(type = "scatter", x = "wt", y = "mpg",
              color = "__none__", palette = "auto", legend_pos = "right",
              reg_overlay = TRUE, reg_type = "lm", reg_ci = TRUE))
plot2    <- build_full_plot(df, list(type = "boxplot", x = "cyl", y = "mpg",
              color = "cyl", palette = "auto", legend_pos = "none"))
code1    <- generate_code(df, list(type = "scatter", x = "wt", y = "mpg",
              reg_overlay = TRUE, reg_type = "lm"))
code2    <- generate_code(df, list(type = "boxplot", x = "cyl", y = "mpg"))
summ     <- grouped_summary(df, c("mpg", "hp"), "cyl")
cmp      <- compare_groups_numeric(df, "mpg", "cyl"); cmp$mode <- "num"
mod      <- fit_model(df, "mpg", c("wt", "hp"), "multiple")

ui <- page_navbar(
  title = uf_title("Report — dev harness"),
  theme = uf_theme(),
  nav_panel(tagList(icon("file-lines"), " Report"), reportUI("rep"))
)

server <- function(input, output, session) {
  reportServer(
    "rep",
    data_in     = reactive(df),
    summary_tbl = reactive(summ),
    plots       = reactive(list(plot1, plot2)),
    plot_code   = reactive(list(code1, code2)),
    comparison  = reactive(cmp),
    model       = reactive(mod)
  )
}

shinyApp(ui, server)
