# ============================================================
# mod_reshape.R — single-table reshape stage (JMP Tables menu)
# ============================================================
# A thin Shiny module over the pure verbs in R/helpers_reshape.R.
# Pattern A wiring: the server takes the incoming data as a reactive
# (`data_in`) and RETURNS the reshaped data as a reactive, so a parent
# app can pass it straight into the next stage (Summarize / Visualize /
# Export). Stack and Split ship first; transpose/sort/subset/summary
# slot in here later behind the same operation selector.
#
# Requires R/helpers_reshape.R to be sourced first (do_stack, do_split).

reshapeUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      radioButtons(
        ns("op"),
        tagList("Operation", info_tip(
          "Reshaping changes the shape of your table without changing the ",
          "underlying data — e.g. turning many columns into one (Stack) or ",
          "one column into many (Split).")),
        choices = c("None  (pass through)"        = "none",
                    "Stack  (wide → tall)"        = "stack",
                    "Split  (tall → wide)"        = "split",
                    "Transpose  (swap rows/cols)" = "transpose",
                    "Sort  (reorder rows)"        = "sort",
                    "Subset  (rows / columns)"    = "subset",
                    "Summary  (stats by group)"   = "summary"),
        selected = "none"
      ),

      # ---- Stack controls ----
      conditionalPanel(
        condition = sprintf("input['%s'] == 'stack'", ns("op")),
        selectizeInput(
          ns("stack_cols"), "Columns to stack", choices = NULL, multiple = TRUE,
          options = list(placeholder = "pick one or more columns")
        ),
        textInput(ns("label_to"), "New label column", value = "Label"),
        textInput(ns("value_to"), "New value column", value = "Data"),
        helpText("Collapses the chosen columns into one value column plus a ",
                 "label column naming where each value came from.")
      ),

      # ---- Split controls ----
      conditionalPanel(
        condition = sprintf("input['%s'] == 'split'", ns("op")),
        selectInput(ns("value_col"), "Values to spread (Split Columns)",
                    choices = NULL),
        selectInput(ns("split_by"), "Split by (becomes new headers)",
                    choices = NULL),
        selectizeInput(
          ns("group_cols"), "Group / row identity", choices = NULL,
          multiple = TRUE,
          options = list(placeholder = "leave blank = everything else")
        ),
        selectInput(
          ns("values_fn"),
          tagList("If a group repeats, combine values with", info_tip(
            "Two or more rows want the same output cell. Choose how to merge ",
            "them: mean/median/sum combine numbers; first/last keep one value. ",
            "Leave on “keep separate” to be warned instead.")),
          choices = c("(keep separate / warn)" = "none", "first" = "first",
                      "last" = "last", "mean" = "mean", "sum" = "sum",
                      "median" = "median"),
          selected = "none"
        ),
        helpText("Spreads one column's values into several columns, one per ",
                 "category in the split-by column.")
      ),

      # ---- Transpose controls ----
      conditionalPanel(
        condition = sprintf("input['%s'] == 'transpose'", ns("op")),
        selectInput(ns("transpose_names_from"),
                    tagList("Column to use as new headers", info_tip(
                      "This column's values become the new column names. ",
                      "Pick “(none)” to get generic V1, V2, … headers instead.")),
                    choices = NULL),
        textInput(ns("transpose_id_col"), "Name for the labels column",
                  value = "name"),
        helpText("Swaps rows and columns. The chosen column's values become ",
                 "the new headers; pick “(none)” to get V1, V2, … instead. ",
                 "Mixed-type columns become text.")
      ),

      # ---- Sort controls ----
      conditionalPanel(
        condition = sprintf("input['%s'] == 'sort'", ns("op")),
        selectizeInput(
          ns("sort_cols"), "Sort by (in priority order)", choices = NULL,
          multiple = TRUE,
          options = list(placeholder = "pick one or more columns")
        ),
        checkboxGroupInput(ns("sort_desc"), "Descending for", choices = NULL),
        helpText("Rows are ordered by the first column, ties broken by the ",
                 "next, and so on. Check a column to sort it high → low.")
      ),

      # ---- Subset controls ----
      conditionalPanel(
        condition = sprintf("input['%s'] == 'subset'", ns("op")),
        selectizeInput(
          ns("subset_cols"), "Columns to keep", choices = NULL, multiple = TRUE,
          options = list(placeholder = "leave blank = keep all")
        ),
        radioButtons(ns("subset_sample"), "Rows",
                     choices = c("All rows"  = "all",
                                 "Random N"  = "n",
                                 "Random %"  = "prop"),
                     selected = "all"),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'n'", ns("subset_sample")),
          numericInput(ns("subset_n"), "Number of rows", value = 100,
                       min = 1, step = 1)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'prop'", ns("subset_sample")),
          numericInput(ns("subset_prop"), "Fraction of rows", value = 0.10,
                       min = 0.01, max = 1, step = 0.05)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] != 'all'", ns("subset_sample")),
          selectizeInput(
            ns("subset_stratify"),
            tagList("Sample within (optional)", info_tip(
              "Draw the sample separately inside each group of the chosen ",
              "column(s), so every group keeps its share of the rows ",
              "(stratified sampling).")),
            choices = NULL, multiple = TRUE,
            options = list(placeholder = "stratify by group(s)")
          ),
          numericInput(ns("subset_seed"), "Random seed (optional)",
                       value = NA, min = 0, step = 1)
        ),
        helpText("Keep a subset of columns and/or a sample of rows. ",
                 "Stratifying samples the same fraction within each group.")
      ),

      # ---- Summary controls ----
      conditionalPanel(
        condition = sprintf("input['%s'] == 'summary'", ns("op")),
        selectizeInput(
          ns("summary_groups"),
          tagList("Group by", info_tip(
            "Category column(s) that define the groups — one summary row per ",
            "combination.")),
          choices = NULL, multiple = TRUE,
          options = list(placeholder = "pick grouping column(s)")
        ),
        selectizeInput(
          ns("summary_vars"),
          tagList("Summarize (numeric)", info_tip(
            "Numeric column(s) to summarize within each group.")),
          choices = NULL, multiple = TRUE,
          options = list(placeholder = "pick numeric column(s)")
        ),
        helpText("Builds a new table — count, mean, median, mode, min, max, SD, ",
                 "SE, and IQR of each numeric column within each group — that ",
                 "then flows downstream like any reshaped table.")
      )
    ),

    navset_card_tab(
      title = textOutput(ns("shape_summary"), inline = TRUE),
      nav_panel(tagList(icon("wand-magic-sparkles"), " Result"),
                DT::DTOutput(ns("preview"))),
      nav_panel(tagList(icon("table"), " Original"),
                DT::DTOutput(ns("preview_original")))
    )
  )
}

reshapeServer <- function(id, data_in, store = NULL) {
  moduleServer(id, function(input, output, session) {

    # Keep the column pickers in sync with whatever data flows in. On a session
    # restore, the app stages the saved settings in store$pending_reshape; this
    # same observer re-applies them (column pickers get choices + the saved
    # selection together, so there's no repopulate-vs-restore race), then clears
    # the pending payload so it only applies once.
    observeEvent(data_in(), {
      df <- data_in()
      req(is.data.frame(df))
      cols <- names(df)
      pend <- if (!is.null(store)) store$pending_reshape else NULL
      # saved selection for a column picker, intersected with current columns;
      # falls back to the supplied default when nothing was saved.
      gcol <- function(field, default) {
        v <- if (is.list(pend)) pend[[field]] else NULL
        if (is.null(v)) default else intersect(v, cols)
      }
      updateSelectizeInput(session, "stack_cols", choices = cols,
                           selected = gcol("stack_cols", character(0)))
      updateSelectInput(session, "value_col", choices = cols,
                        selected = gcol("value_col", utils::tail(cols, 1)))
      updateSelectInput(session, "split_by", choices = cols,
                        selected = gcol("split_by", cols[[1]]))
      updateSelectizeInput(session, "group_cols", choices = cols,
                           selected = gcol("group_cols", character(0)))
      # "(none)" first so transpose defaults to generated V1.. headers.
      updateSelectInput(session, "transpose_names_from",
                        choices = c("(none — use V1, V2…)" = "", cols),
                        selected = gcol("transpose_names_from", ""))
      updateSelectizeInput(session, "sort_cols", choices = cols,
                           selected = gcol("sort_cols", character(0)))
      updateSelectizeInput(session, "subset_cols", choices = cols,
                           selected = gcol("subset_cols", character(0)))
      updateSelectizeInput(session, "subset_stratify", choices = cols,
                           selected = gcol("subset_stratify", character(0)))
      grps <- groupable_cols(df); nums <- numeric_cols(df)
      updateSelectizeInput(session, "summary_groups", choices = grps,
                           selected = gcol("summary_groups", utils::head(grps, 1)))
      updateSelectizeInput(session, "summary_vars", choices = nums,
                           selected = gcol("summary_vars", utils::head(nums, 1)))

      # Non-column settings (no competing observers) — apply the saved values.
      if (is.list(pend)) {
        updateRadioButtons(session, "op", selected = pend$op %||% "none")
        if (!is.null(pend$label_to))         updateTextInput(session, "label_to", value = pend$label_to)
        if (!is.null(pend$value_to))         updateTextInput(session, "value_to", value = pend$value_to)
        if (!is.null(pend$values_fn))        updateSelectInput(session, "values_fn", selected = pend$values_fn)
        if (!is.null(pend$transpose_id_col)) updateTextInput(session, "transpose_id_col", value = pend$transpose_id_col)
        if (!is.null(pend$subset_sample))    updateRadioButtons(session, "subset_sample", selected = pend$subset_sample)
        if (!is.null(pend$subset_n))         updateNumericInput(session, "subset_n", value = pend$subset_n)
        if (!is.null(pend$subset_prop))      updateNumericInput(session, "subset_prop", value = pend$subset_prop)
        if (!is.null(pend$subset_seed) && !is.na(pend$subset_seed))
          updateNumericInput(session, "subset_seed", value = pend$subset_seed)
        if (!is.null(pend$sort_desc))
          updateCheckboxGroupInput(session, "sort_desc",
                                   choices = gcol("sort_cols", character(0)),
                                   selected = pend$sort_desc)
        store$pending_reshape <- NULL   # consume once
      }
    }, ignoreNULL = TRUE)

    # Publish the current reshape settings so the app's Save can include them.
    if (!is.null(store)) {
      observe({
        store$reshape_state <- list(
          op                   = input$op %||% "none",
          stack_cols           = input$stack_cols,
          label_to             = input$label_to,
          value_to             = input$value_to,
          value_col            = input$value_col,
          split_by             = input$split_by,
          group_cols           = input$group_cols,
          values_fn            = input$values_fn,
          transpose_names_from = input$transpose_names_from,
          transpose_id_col     = input$transpose_id_col,
          sort_cols            = input$sort_cols,
          sort_desc            = input$sort_desc,
          subset_cols          = input$subset_cols,
          subset_sample        = input$subset_sample,
          subset_n             = input$subset_n,
          subset_prop          = input$subset_prop,
          subset_stratify      = input$subset_stratify,
          subset_seed          = input$subset_seed,
          summary_groups       = input$summary_groups,
          summary_vars         = input$summary_vars
        )
      })
    }

    # The "descending for" checkboxes track whatever sort keys are chosen,
    # preserving any boxes already ticked.
    observeEvent(input$sort_cols, {
      updateCheckboxGroupInput(
        session, "sort_desc", choices = input$sort_cols,
        selected = intersect(input$sort_desc, input$sort_cols)
      )
    }, ignoreNULL = FALSE)

    # Map the duplicate-handling choice to an aggregation function (or NULL).
    fn_for <- function(key) switch(
      key %||% "none",
      first  = dplyr::first,
      last   = dplyr::last,
      mean   = function(z) mean(z, na.rm = TRUE),
      sum    = function(z) sum(z, na.rm = TRUE),
      median = function(z) stats::median(z, na.rm = TRUE),
      NULL
    )

    # The user-facing computation: uses validate() so the preview shows a
    # friendly message for an empty or half-specified operation.
    result <- reactive({
      df <- data_in()
      validate(need(is.data.frame(df),
                    "Import data on the Import tab to begin reshaping."))

      switch(
        input$op %||% "none",

        # Pass-through: the data flows on unchanged (lets the reshape stage sit
        # inertly in a pipeline until the user opts into an operation).
        none = df,

        stack = {
          cols <- input$stack_cols
          validate(need(length(cols) >= 1L,
                        "Pick at least one column to stack."))
          do_stack(df, cols,
                   label_to = label_or(input$label_to, "Label"),
                   value_to = label_or(input$value_to, "Data"))
        },

        split = {
          validate(
            need(nzchar(input$value_col %||% ""), "Choose a values column."),
            need(nzchar(input$split_by  %||% ""), "Choose a split-by column."),
            need(!identical(input$value_col, input$split_by),
                 "Values and split-by must be different columns.")
          )
          groups <- input$group_cols
          if (!length(groups)) groups <- NULL
          do_split(df, value_col = input$value_col, split_by = input$split_by,
                   group_cols = groups, values_fn = fn_for(input$values_fn))
        },

        transpose = {
          nf <- input$transpose_names_from
          if (is.null(nf) || !nzchar(nf)) nf <- NULL
          do_transpose(df, names_from = nf,
                       id_col = label_or(input$transpose_id_col, "name"))
        },

        sort = {
          cols <- input$sort_cols
          validate(need(length(cols) >= 1L,
                        "Pick at least one column to sort by."))
          desc <- cols %in% (input$sort_desc %||% character(0))
          do_sort(df, cols, desc = desc)
        },

        subset = {
          smp  <- input$subset_sample %||% "all"
          cols <- input$subset_cols
          if (!length(cols)) cols <- NULL
          strat <- input$subset_stratify
          if (!length(strat)) strat <- NULL
          size <- switch(smp, n = input$subset_n, prop = input$subset_prop,
                         NULL)
          if (smp != "all") {
            validate(need(!is.null(size) && !is.na(size) && size > 0,
                          "Enter how many (or what fraction of) rows to keep."))
          }
          seed <- input$subset_seed
          if (is.null(seed) || is.na(seed)) seed <- NULL
          do_subset(df, cols = cols, sample = smp, size = size,
                    stratify_by = strat, seed = seed)
        },

        summary = {
          groups <- input$summary_groups
          vars   <- input$summary_vars
          validate(need(length(groups) >= 1L,
                        "Pick at least one grouping column."))
          validate(need(length(vars) >= 1L,
                        "Pick at least one numeric column to summarize."))
          out <- grouped_summary(df, vars, groups)
          validate(need(!is.null(out),
                        "Couldn't summarize with those selections."))
          out
        }
      )
    })

    output$preview <- DT::renderDT({
      DT::datatable(result(), rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE))
    })

    output$preview_original <- DT::renderDT({
      df <- data_in()
      validate(need(is.data.frame(df),
                    "Import data on the Import tab to begin."))
      DT::datatable(df, rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE))
    })

    # "before → after" shape line shown on the preview card's tab strip.
    output$shape_summary <- renderText({
      din <- data_in()
      if (!is.data.frame(din))
        return("Import data on the Import tab to begin reshaping.")
      out <- tryCatch(result(), error = function(e) NULL)
      bn  <- function(x) format(x, big.mark = ",")
      if (is.null(out))
        sprintf("Loaded %s × %s — finish choosing the operation.",
                bn(nrow(din)), ncol(din))
      else if (identical(input$op %||% "none", "none"))
        sprintf("Pass-through — %s rows × %s columns", bn(nrow(out)), ncol(out))
      else
        sprintf("Reshaped: %s × %s  →  %s × %s",
                bn(nrow(din)), ncol(din), bn(nrow(out)), ncol(out))
    })

    # Returned to the parent: NULL-safe (validation/errors become NULL) so a
    # downstream stage like Export degrades gracefully instead of erroring.
    returned <- reactive(tryCatch(result(), error = function(e) NULL))
    returned
  })
}
