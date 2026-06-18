# ============================================================
# mod_combine.R -- two-table operations (JMP combine half)
# ============================================================
# Concatenate / Join / Update / Compare two tables. Pattern A with TWO
# inputs: combineServer(id, left, right) takes two data reactives and
# returns the combined result as a reactive. Thin wrapper over
# R/helpers_combine.R. Requires R/components.R (info_tip).

combineUI <- function(id) {
  ns <- NS(id)
  cond <- function(val) sprintf("input['%s'] == '%s'", ns("op"), val)
  layout_sidebar(
    sidebar = sidebar(
      width = 330,
      radioButtons(ns("op"),
        tagList("Operation", info_tip(
          "Two-table operations: stack rows (Concatenate), match columns side ",
          "by side (Join), patch values (Update), or diff two tables (Compare).")),
        choices = c("Concatenate  (stack rows)"  = "concat",
                    "Join  (match by key)"       = "join",
                    "Update  (patch values)"     = "update",
                    "Compare  (diff tables)"      = "compare"),
        selected = "concat"),

      conditionalPanel(
        cond("concat"),
        checkboxInput(ns("concat_src"),
                      "Tag each row with its source table", FALSE),
        conditionalPanel(
          sprintf("input['%s'] == true", ns("concat_src")),
          textInput(ns("concat_srccol"), "Source column name", "source")),
        helpText("Stacks the right table beneath the left, matching columns by ",
                 "name; columns in only one table are filled with NA.")
      ),

      conditionalPanel(
        cond("join"),
        selectInput(ns("join_type"), "Join type",
          choices = c("Keep all left rows (left join)" = "left",
                      "Only matching rows (inner)"     = "inner",
                      "Keep all rows from both (full)"  = "full",
                      "Keep all right rows (right)"     = "right",
                      "All pairs (cross / Cartesian)"   = "cross")),
        conditionalPanel(
          sprintf("input['%s'] != 'cross'", ns("join_type")),
          selectizeInput(ns("join_by"),
            tagList("Join by (key columns)", info_tip(
              "Columns present in both tables, used to match rows.")),
            choices = NULL, multiple = TRUE,
            options = list(placeholder = "shared column(s)"))),
        helpText("Adds the right table's columns to the left, matching rows by ",
                 "the chosen key(s).")
      ),

      conditionalPanel(
        cond("update"),
        selectizeInput(ns("update_by"),
          tagList("Match by (key columns)", info_tip(
            "Rows of the left table are matched to the right by these key(s); ",
            "matched values are then updated.")),
          choices = NULL, multiple = TRUE,
          options = list(placeholder = "shared column(s)")),
        radioButtons(ns("update_mode"), "How to update",
          choices = c("Overwrite matching values" = "overwrite",
                      "Only fill blanks (NA)"      = "fill")),
        helpText("Replaces values in the left table with the right's, matched ",
                 "by key. Overwrite replaces everything; Fill only fills blanks.")
      ),

      conditionalPanel(
        cond("compare"),
        helpText("Reports which columns are unique to each table and, when the ",
                 "row counts match, how many cells differ in each shared column.")
      )
    ),

    navset_card_tab(
      title = textOutput(ns("shape_summary"), inline = TRUE),
      nav_panel(tagList(icon("wand-magic-sparkles"), " Result"),
                DT::DTOutput(ns("preview"))),
      nav_panel(tagList(icon("table-columns"), " Left"),
                DT::DTOutput(ns("preview_left"))),
      nav_panel(tagList(icon("table-columns"), " Right"),
                DT::DTOutput(ns("preview_right")))
    )
  )
}

combineServer <- function(id, left, right) {
  moduleServer(id, function(input, output, session) {

    shared <- reactive({
      l <- left(); r <- right()
      if (!is.data.frame(l) || !is.data.frame(r)) return(character(0))
      intersect(names(l), names(r))
    })

    observeEvent(shared(), {
      sh <- shared()
      sel <- if (length(sh)) sh[[1]] else character(0)
      updateSelectizeInput(session, "join_by",   choices = sh, selected = sel)
      updateSelectizeInput(session, "update_by", choices = sh, selected = sel)
    }, ignoreNULL = FALSE)

    result <- reactive({
      l <- left(); r <- right()
      validate(need(is.data.frame(l) && is.data.frame(r),
                    "Provide two tables to combine."))
      switch(
        input$op %||% "concat",
        concat = do_concatenate(l, r, add_source = isTRUE(input$concat_src),
                                source_col = label_or(input$concat_srccol, "source")),
        join = {
          jt <- input$join_type %||% "left"
          if (jt != "cross")
            validate(need(length(input$join_by) >= 1L,
                          "Choose at least one key column to join by."))
          do_join(l, r, by = if (jt == "cross") NULL else input$join_by,
                  type = jt)
        },
        update = {
          validate(need(length(input$update_by) >= 1L,
                        "Choose at least one key column to match on."))
          do_update(l, r, by = input$update_by,
                    mode = input$update_mode %||% "overwrite")
        },
        compare = compare_tables(l, r)
      )
    })

    output$preview <- DT::renderDT({
      DT::datatable(result(), rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE))
    })
    tbl_out <- function(get) DT::renderDT({
      d <- get()
      validate(need(is.data.frame(d), "No table provided."))
      DT::datatable(d, rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE))
    })
    output$preview_left  <- tbl_out(left)
    output$preview_right <- tbl_out(right)

    output$shape_summary <- renderText({
      l <- left(); r <- right()
      if (!is.data.frame(l) || !is.data.frame(r))
        return("Provide two tables to combine.")
      bn   <- function(x) format(x, big.mark = ",")
      base <- sprintf("Left %s \u00d7 %s  +  Right %s \u00d7 %s",
                      bn(nrow(l)), ncol(l), bn(nrow(r)), ncol(r))
      out <- tryCatch(result(), error = function(e) NULL)
      if (is.null(out)) base
      else sprintf("%s  \u2192  %s \u00d7 %s", base, bn(nrow(out)), ncol(out))
    })

    # NULL-safe return for any downstream stage (e.g. Export).
    returned <- reactive(tryCatch(result(), error = function(e) NULL))
    returned
  })
}
