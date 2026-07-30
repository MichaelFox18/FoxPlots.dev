# ============================================================
# helpers_report.R -- assemble a session into a self-contained HTML report
# ============================================================
# Builds a single, portable .html report from whatever the user produced in a
# session: a data overview, the grouped summary, the charts, a group comparison,
# and a regression. Deliberately PANDOC-FREE -- the HTML is assembled as a plain
# string with inlined CSS and base64-embedded plot images, so it renders the
# same from RStudio, VS Code, a bare Rscript, or a deployed app, with no extra
# system software. mod_report.R is a thin wrapper over these.
#
# Split for testability:
#   * report_spec()        -- pure: normalize the session artifacts into a spec.
#   * summary_code() / compare_code() / regression_code() -- pure: reproducible R.
#   * df_to_html() / html_escape() / build_report_html() -- pure: string output.
#   * plot_to_data_uri() / render_report() -- the only impure bits (rasterize a
#     ggplot to a PNG data URI, write the file). Smoke-tested, not unit-tested.
#
# Reuses data_glance()/column_profile() (helpers_stats), model_interpretation()
# (helpers_model), and effect_magnitude() (helpers_compare). base64enc ships
# with Shiny, so it is always available.

# --- small string utilities (pure) ------------------------------------------

#' HTML-escape a character vector (&, <, >, ").
#' @noRd
html_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;",  x, fixed = TRUE)
  x <- gsub("<", "&lt;",   x, fixed = TRUE)
  x <- gsub(">", "&gt;",   x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

#' Format one display cell as plain text: round numbers, blank out NA. No HTML
#' escaping (so it's reusable for Word) -- the HTML path escapes on top.
#' @noRd
.fmt_cell <- function(x) {
  if (length(x) != 1L) x <- x[1]
  if (is.na(x)) return("")
  if (is.numeric(x)) {
    if (is.finite(x) && x == round(x)) format(x, big.mark = ",")
    else format(round(as.numeric(x), 4), big.mark = ",")
  } else as.character(x)
}

#' HTML display cell = the plain cell, escaped.
#' @noRd
.report_cell <- function(x) html_escape(.fmt_cell(x))

#' Plain-text p-value (no HTML entity) -- for the Word path.
#' @noRd
.p_txt <- function(p) {
  if (is.null(p) || is.na(p)) return("N/A")
  if (p < 0.001) "< 0.001" else as.character(round(p, 4))
}

# Tables wider than this many columns get the "wide" treatment: a smaller
# font + nowrap cells in HTML, and column-chunking in Word. 8 keeps the
# regression/means tables (<= 8 cols) full-size while catching the compare
# grid summary (~13), chi-square matrices (a column per level), the column
# profile, and EMMeans/pairwise frames.
REPORT_WIDE_COLS <- 8L

#' Render a data frame as an HTML <table>. Pure (base R string building).
#' @param df A data frame.
#' @param caption Optional caption shown above the table.
#' @param max_rows Truncate to this many rows (a note is appended if cut).
#' @noRd
df_to_html <- function(df, caption = NULL, max_rows = 500L) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df))
    return("<p class=\"note\">(nothing to show)</p>")
  n_all <- nrow(df)
  if (n_all > max_rows) df <- df[seq_len(max_rows), , drop = FALSE]
  head_html <- paste0("<th>", html_escape(names(df)), "</th>", collapse = "")
  rows_html <- vapply(seq_len(nrow(df)), function(i) {
    cells <- vapply(df[i, , drop = FALSE], .report_cell, character(1))
    paste0("<tr>", paste0("<td>", cells, "</td>", collapse = ""), "</tr>")
  }, character(1))
  cap <- if (!is.null(caption))
    paste0("<p class=\"note\">", html_escape(caption), "</p>") else ""
  more <- if (n_all > max_rows)
    sprintf("<p class=\"note\">Showing first %s of %s rows.</p>",
            format(max_rows, big.mark = ","), format(n_all, big.mark = ",")) else ""
  # Every table sits in a horizontal-scroll wrapper so a wide one scrolls in
  # place instead of overflowing the fixed-width page; past REPORT_WIDE_COLS
  # it also drops to a smaller nowrap face (see .report_css).
  tab_open <- if (ncol(df) > REPORT_WIDE_COLS) "<table class=\"wide\">"
              else "<table>"
  paste0(cap, "<div class=\"tbl-wrap\">", tab_open, "<thead><tr>", head_html,
         "</tr></thead><tbody>", paste0(rows_html, collapse = ""),
         "</tbody></table></div>", more)
}

#' A static, non-executed R code block.
#' @noRd
code_block_html <- function(code) {
  if (is.null(code) || !nzchar(code)) return("")
  paste0("<pre class=\"code\"><code>", html_escape(code), "</code></pre>")
}

# --- reproducible-code generators (pure) ------------------------------------

# Render a character vector as `c("a", "b")` source.
.vec_code <- function(x) {
  paste0("c(", paste(sprintf("\"%s\"", x), collapse = ", "), ")")
}

#' Reproduce the grouped-summary / proportions call from the result table's
#' column layout (the outcome name is lost in proportions mode, so it's a stub).
#' @noRd
summary_code <- function(summary_tbl) {
  if (is.null(summary_tbl) || !is.data.frame(summary_tbl) || !nrow(summary_tbl))
    return(NULL)
  cn <- names(summary_tbl)
  if ("Variable" %in% cn) {
    groups <- cn[seq_len(match("Variable", cn) - 1L)]
    vars   <- unique(as.character(summary_tbl$Variable))
    sprintf("grouped_summary(data, vars = %s, groups = %s)",
            .vec_code(vars), .vec_code(groups))
  } else if ("Level" %in% cn) {
    groups <- cn[seq_len(match("Level", cn) - 1L)]
    sprintf("proportions_summary(data, outcome = <outcome>, groups = %s)",
            .vec_code(groups))
  } else NULL
}

#' Reproduce the hypothesis test from a compare_* result list. Infers
#' categorical vs numeric from the result's own fields (compare_categorical
#' carries var1/var2; compare_groups_numeric carries outcome/group), except for a
#' grid, which is tagged $mode = "num_multi" by the module.
#' @noRd
compare_code <- function(res) {
  if (is.null(res) || !is.list(res)) return(NULL)
  # A grid: loop the same test over every outcome x group (and, when the
  # analysis was split by a third variable, over each of its levels -- the
  # emitted code must reproduce the STRATIFIED analysis, not an unsplit one),
  # then adjust across all.
  if (identical(res$mode, "num_multi")) {
    para  <- isTRUE(res$grid$results[[1]]$parametric)
    split <- res$grid$split_by
    call <- if (para)
      "      fit <- aov(d[[o]] ~ factor(d[[g]]))\n      p <- summary(fit)[[1]][[\"Pr(>F)\"]][1]"
    else
      "      kt <- kruskal.test(d[[o]] ~ factor(d[[g]]))\n      p <- kt$p.value"
    if (is.null(split)) {
      return(sprintf(paste0(
        "outcomes <- c(%s)\ngroups   <- c(%s)\nd <- data\n\n",
        "res <- data.frame()\nfor (o in outcomes) {\n  for (g in groups) {\n",
        "%s\n",
        "      res <- rbind(res, data.frame(outcome = o, group = g, p_value = p))\n",
        "  }\n}\n",
        "res$p_adj <- p.adjust(res$p_value, method = \"%s\")\nres"),
        paste(sprintf("\"%s\"", res$outcomes), collapse = ", "),
        paste(sprintf("\"%s\"", res$groups), collapse = ", "),
        call, res$p_adjust %||% "BH"))
    }
    return(sprintf(paste0(
      "outcomes <- c(%s)\ngroups   <- c(%s)\n",
      "strata   <- sort(unique(na.omit(data[[\"%s\"]])))  # analysis is split by %s\n\n",
      "res <- data.frame()\nfor (s in strata) {\n",
      "  d <- data[!is.na(data[[\"%s\"]]) & data[[\"%s\"]] == s, ]\n",
      "  for (o in outcomes) {\n    for (g in groups) {\n",
      "  %s\n",
      "      res <- rbind(res, data.frame(outcome = o, group = g, stratum = s, p_value = p))\n",
      "    }\n  }\n}\n",
      "res$p_adj <- p.adjust(res$p_value, method = \"%s\")  # across the whole stratified family\nres"),
      paste(sprintf("\"%s\"", res$outcomes), collapse = ", "),
      paste(sprintf("\"%s\"", res$groups), collapse = ", "),
      split, split, split, split, call, res$p_adjust %||% "BH"))
  }
  is_cat <- !is.null(res$var1) && !is.null(res$var2) && is.null(res$outcome)
  if (is_cat)
    return(sprintf("chisq.test(table(data[[\"%s\"]], data[[\"%s\"]]))",
                   res$var1, res$var2))
  if (is.null(res$outcome) || is.null(res$group)) return(NULL)
  o <- res$outcome; g <- res$group
  test <- if (is.null(res$test)) "" else res$test
  if (grepl("ANOVA", test))
    sprintf("fit <- aov(%s ~ %s, data = data)\nsummary(fit)\nTukeyHSD(fit)", o, g)
  else if (grepl("Kruskal", test)) {
    ph <- if (identical(res$posthoc_name, "Steel-Dwass"))
      "\n# all-pairs post-hoc: Steel-Dwass (see PMCMRplus::dscfAllPairsTest)"
      else "\n# all-pairs post-hoc: Dunn's test (see PMCMRplus::kwAllPairsDunnTest)"
    paste0(sprintf("kruskal.test(%s ~ %s, data = data)", o, g), ph)
  }
  else if (grepl("Wilcoxon", test))
    sprintf("wilcox.test(%s ~ %s, data = data)", o, g)
  else {
    ve <- if (grepl("equal variance", test)) "TRUE" else "FALSE"
    sprintf("t.test(%s ~ %s, data = data, var.equal = %s)", o, g, ve)
  }
}

#' Reproduce the lm() call from a fitted model.
#' @noRd
regression_code <- function(model) {
  if (is.null(model) || !inherits(model, "lm")) return(NULL)
  f <- paste(deparse(stats::formula(model)), collapse = " ")
  f <- gsub("\\s+", " ", f)
  if (inherits(model, "glm") &&
      identical(model$family$family, "binomial")) {
    # The app mapped the binary response to 0/1 with attr "success" as the
    # modelled level; without the same mapping the pasted code can flip signs
    # (glm on a factor models P(second FACTOR level), not P(success)).
    resp <- as.character(stats::formula(model))[2]
    succ <- attr(model, "success")
    map_line <- if (!is.null(succ)) sprintf(
      "data[[%s]] <- as.integer(as.character(data[[%s]]) == %s)  # 1 = %s\n",
      qq(resp), qq(resp), qq(succ), succ) else ""
    return(paste0(map_line,
                  "model <- glm(", f, ", data = data, family = binomial)\n",
                  "summary(model)\n",
                  "exp(cbind(`odds ratio` = coef(model), confint(model)))"))
  }
  paste0("model <- lm(", f, ", data = data)\nsummary(model)")
}

# --- the spec (pure) --------------------------------------------------------

#' Normalize a session's artifacts into a report spec.
#'
#' Each section is included only if its artifact is present and usable, so a
#' report mirrors exactly what the user did. `data` is summarized into glance +
#' profile and then NOT retained (keeps the spec light and avoids carrying the
#' raw rows around).
#'
#' @param data The working data frame (required).
#' @param summary_tbl Result of mod_summarize (data frame) or NULL.
#' @param plots List of ggplots from mod_visualize, or NULL.
#' @param plot_code Character list aligned to `plots` (the ggplot2 code), or NULL.
#' @param maps List of leaflet widgets from mod_map, or NULL.
#' @param map_code Character list aligned to `maps` (the leaflet code), or NULL.
#' @param comparison Result list from mod_compare, or NULL.
#' @param model Fitted lm from mod_regression, or NULL.
#' @param mixed List of model-report payloads (see model_payload()) from the
#'   mixed-model tools, or NULL. Several may be supplied: the GLMM app
#'   contributes its general and binary fits as separate entries.
#' @param title Report title.
#' @param show_code Whether the report should reveal reproducible R code.
#' @param logo Optional data-URI string for the masthead logo.
#' @param generated A POSIXct timestamp.
#' @return A named list describing the report (see `sections`).
#' @export
report_spec <- function(data, summary_tbl = NULL, plots = NULL, plot_code = NULL,
                        maps = NULL, map_code = NULL,
                        comparison = NULL, model = NULL, mixed = NULL,
                        title = "Data Explorer Report", show_code = FALSE,
                        logo = NULL, generated = Sys.time()) {
  stopifnot(is.data.frame(data))
  has_summary <- is.data.frame(summary_tbl) && nrow(summary_tbl) > 0
  # plots and plot_code are assumed aligned (mod_visualize guarantees this).
  has_charts  <- !is.null(plots) && length(plots) > 0
  # maps/map_code likewise (mod_map returns the same list(widgets, code) shape).
  has_maps    <- !is.null(maps) && length(maps) > 0
  has_compare <- !is.null(comparison) && is.list(comparison) && !is.null(comparison$mode)
  has_model   <- !is.null(model) && inherits(model, "lm")
  # `mixed` is a LIST of model-report payloads (see model_payload()), so one
  # app can contribute several -- the GLMM tool runs a general and a binary
  # model side by side and reports both.
  mixed     <- Filter(function(x) is.list(x) && length(x$tables) > 0, mixed %||% list())
  has_mixed <- length(mixed) > 0

  list(
    title     = title,
    generated = generated,
    show_code = isTRUE(show_code),
    logo      = logo,
    glance    = data_glance(data),
    profile   = column_profile(data),
    summary       = if (has_summary) summary_tbl else NULL,
    summary_code  = if (has_summary) summary_code(summary_tbl) else NULL,
    plots         = if (has_charts) plots else NULL,
    plot_code     = if (has_charts) plot_code else NULL,
    maps          = if (has_maps) maps else NULL,
    map_code      = if (has_maps) map_code else NULL,
    comparison    = if (has_compare) comparison else NULL,
    compare_code  = if (has_compare) compare_code(comparison) else NULL,
    model         = if (has_model) model else NULL,
    model_interp  = if (has_model) model_interpretation(model) else NULL,
    regression_code = if (has_model) regression_code(model) else NULL,
    mixed         = if (has_mixed) mixed else NULL,
    sections = c(
      overview   = TRUE,
      summary    = has_summary,
      charts     = has_charts,
      maps       = has_maps,
      comparison = has_compare,
      regression = has_model,
      mixed      = has_mixed
    )
  )
}

# --- model-report payloads ---------------------------------------------------
# The mixed-model tools (lmer, GLMM) produce richer output than a single lm:
# several formulas, several tables, verbatim diagnostic blocks. Rather than
# teach both renderers about each engine, the modules hand the report a plain
# payload in this shape and ONE renderer per format walks it:
#
#   title    chr(1)  section heading, e.g. "Mixed model" / "GLMM (binary 0/1)"
#   formulas named chr, printed as a label: value block ("Response" = "y ~ x")
#   tables   named list of data frames, printed in order under their names
#   texts    named list of chr, printed verbatim (test output, model summary)
#   notes    chr, the engine's caveats (singular fit, contrast recoding, ...)
#   code     chr(1), the reproducible script (shown only when show_code)
#
# Empty slots are dropped, so a payload degrades cleanly when the user never
# opened, say, the EMMeans tab.
model_payload <- function(title, formulas = character(0), tables = list(),
                          texts = list(), notes = character(0), code = NULL) {
  drop_empty <- function(l) Filter(function(x) !is.null(x) && length(x) > 0, l)
  list(title    = title,
       formulas = formulas[nzchar(formulas %||% character(0))],
       tables   = drop_empty(tables),
       texts    = drop_empty(texts),
       notes    = notes[nzchar(notes %||% character(0))],
       code     = if (!is.null(code) && nzchar(code)) code else NULL)
}

# --- the HTML document (pure) -----------------------------------------------

.report_css <- "
body{font-family:'Segoe UI',Helvetica,Arial,sans-serif;color:#23252b;margin:0;background:#f4f5f8;line-height:1.5;}
.report{max-width:920px;margin:26px auto;background:#fff;padding:30px 42px 48px;box-shadow:0 1px 8px rgba(0,0,0,.10);}
header.masthead{border-bottom:3px solid #FA4616;padding-bottom:16px;margin-bottom:6px;display:flex;align-items:center;gap:18px;}
header.masthead img{height:56px;}
h1{color:#003087;font-size:1.72em;margin:.1em 0;}
h2{color:#003087;border-bottom:1px solid #e4e4ea;padding-bottom:6px;margin-top:2em;font-size:1.28em;}
h3{color:#3a3a3a;font-size:1.05em;margin-bottom:.3em;}
a{color:#003087;}
table{border-collapse:collapse;width:100%;margin:12px 0;font-size:.91em;}
th{background:#003087;color:#fff;text-align:left;padding:6px 9px;font-weight:600;}
td{border-bottom:1px solid #eee;padding:5px 9px;}
tr:nth-child(even) td{background:#f5f7fb;}
.tbl-wrap{overflow-x:auto;margin:12px 0;}
.tbl-wrap table{margin:0;}
table.wide{font-size:.8em;}
table.wide th,table.wide td{padding:4px 6px;white-space:nowrap;}
.meta{color:#7a7a7a;font-size:.9em;margin:.2em 0;}
.kpis{display:flex;gap:14px;flex-wrap:wrap;margin:14px 0;}
.kpi{background:#eef2fa;border-left:4px solid #003087;padding:9px 16px;border-radius:3px;min-width:96px;}
.kpi .n{font-size:1.4em;font-weight:700;color:#003087;display:block;line-height:1.1;}
.kpi .l{font-size:.74em;color:#566;text-transform:uppercase;letter-spacing:.03em;}
figure{margin:18px 0;text-align:center;}
figure img{max-width:100%;height:auto;border:1px solid #ececec;border-radius:3px;}
pre.code{background:#1e2330;color:#eaeaea;padding:12px 15px;border-radius:5px;overflow:auto;font-size:.84em;line-height:1.45;margin:8px 0 18px;}
.verdict-sig{color:#2e7d32;font-weight:600;}
.verdict-ns{color:#c62828;font-weight:600;}
.note{color:#6b6b6b;font-size:.88em;}
.toc{background:#f7f8fb;border:1px solid #e8e8ee;border-radius:4px;padding:10px 18px;margin:14px 0;}
.toc ul{margin:.3em 0;padding-left:1.2em;} .toc li{margin:.15em 0;}
footer{margin-top:2.4em;border-top:1px solid #e4e4ea;padding-top:12px;color:#9a9a9a;font-size:.82em;}
"

# Format a p-value for the report.
.report_p <- function(p) {
  if (is.null(p) || is.na(p)) return("p = N/A")
  if (p < 0.001) "p &lt; 0.001" else paste0("p = ", round(p, 4))
}

# Overview section: KPI cards + per-column profile.
.section_overview <- function(spec) {
  g <- spec$glance
  kpi <- function(n, l)
    sprintf("<div class=\"kpi\"><span class=\"n\">%s</span><span class=\"l\">%s</span></div>",
            format(n, big.mark = ","), l)
  cards <- paste0(
    kpi(g$n, "rows"), kpi(g$m, "columns"), kpi(g$num, "numeric"),
    kpi(g$cat, "categorical"), kpi(g$date, "date"),
    kpi(g$complete, "complete rows"))
  paste0("<h2 id=\"overview\">Data overview</h2>",
         "<div class=\"kpis\">", cards, "</div>",
         "<h3>Columns</h3>", df_to_html(spec$profile))
}

.section_summary <- function(spec) {
  code <- if (isTRUE(spec$show_code)) code_block_html(spec$summary_code) else ""
  paste0("<h2 id=\"summary\">Summary</h2>", df_to_html(spec$summary), code)
}

.section_charts <- function(spec, plot_uris) {
  figs <- vapply(seq_along(spec$plots), function(i) {
    uri  <- if (i <= length(plot_uris)) plot_uris[[i]] else NA_character_
    img  <- if (!is.na(uri))
      sprintf("<figure><img src=\"%s\" alt=\"Chart %d\"></figure>", uri, i)
    else "<p class=\"note\">(chart could not be rendered)</p>"
    code <- if (isTRUE(spec$show_code) && length(spec$plot_code) >= i)
      code_block_html(spec$plot_code[[i]]) else ""
    paste0("<h3>Chart ", i, "</h3>", img, code)
  }, character(1))
  paste0("<h2 id=\"charts\">Charts</h2>", paste0(figs, collapse = ""))
}

# Maps are static snapshots here, not live widgets: a report has to survive
# being emailed around and opened offline. When no snapshot could be taken
# (no headless Chrome) the section still appears, saying so, rather than
# vanishing without explanation.
.section_maps <- function(spec, map_uris) {
  figs <- vapply(seq_along(spec$maps), function(i) {
    uri <- if (i <= length(map_uris)) map_uris[[i]] else NA_character_
    img <- if (!is.na(uri))
      sprintf("<figure><img src=\"%s\" alt=\"Map %d\"></figure>", uri, i)
    else paste0("<p class=\"note\">(map snapshot unavailable &mdash; it needs ",
                "the optional webshot2 and chromote packages plus Chrome)</p>")
    code <- if (isTRUE(spec$show_code) && length(spec$map_code) >= i)
      code_block_html(spec$map_code[[i]]) else ""
    paste0("<h3>Map ", i, "</h3>", img, code)
  }, character(1))
  paste0("<h2 id=\"maps\">Maps</h2>", paste0(figs, collapse = ""))
}

# A matrix (counts / percentages / residuals) as an HTML table with its row
# names promoted to a leading column. `row_var` names that column (the ROW
# variable) -- the matrix itself has no dimnames, so without it the header
# cell is blank and the reader cannot tell which variable went where.
.mat_to_html <- function(m, caption, row_var = " ") {
  d <- cbind(stats::setNames(data.frame(rownames(m), stringsAsFactors = FALSE),
                             row_var),
             as.data.frame.matrix(m))
  df_to_html(d, caption)
}

# The body for ONE numeric comparison. Shared by the single-comparison report
# and every combination of a grid.
.section_comparison_num <- function(r) {
  sig <- !is.na(r$p_value) && r$p_value < 0.05
  vcl <- if (sig) "verdict-sig" else "verdict-ns"
  eff <- if (!is.na(r$effect_value))
    sprintf(" Effect size (%s) = %s (%s).", r$effect_name,
            round(r$effect_value, 3),
            effect_magnitude(r$effect_name, r$effect_value)) else ""
  verdict <- sprintf(
    "<span class=\"%s\">%s difference in %s across %s</span> (%s, %s).%s",
    vcl, if (sig) "Significant" else "No significant", html_escape(r$outcome),
    html_escape(r$group), html_escape(r$test), .report_p(r$p_value), eff)
  fit_txt <- if (!is.null(r$fit_stats))
    sprintf(paste0("<p class=\"note\">Summary of fit: R-squared = %s, ",
                   "adjusted R-squared = %s, RMSE = %s.</p>"),
            round(r$fit_stats$r_squared, 3), round(r$fit_stats$adj_r_squared, 3),
            round(r$fit_stats$rmse, 3)) else ""
  welch_txt <- if (!is.null(r$welch))
    sprintf(paste0("<p class=\"note\">Welch's ANOVA (unequal variances): ",
                   "F(%s, %s) = %s, %s.</p>"),
            round(r$welch$df1), round(r$welch$df2, 1),
            round(r$welch$statistic, 3), .report_p(r$welch$p_value)) else ""
  body <- paste0("<p>", verdict, "</p>", df_to_html(r$group_stats, "Group means"))
  if (!is.null(r$anova_table))
    body <- paste0(body, df_to_html(r$anova_table, "ANOVA table"), fit_txt, welch_txt)
  if (!is.null(r$posthoc))
    body <- paste0(body, df_to_html(r$posthoc, sprintf(
      "Pairwise comparisons (%s)", r$posthoc_name %||% "post-hoc")))
  if (!is.null(r$cld))
    body <- paste0(body, df_to_html(r$cld, "Connecting letters"))
  body
}

.section_comparison <- function(spec) {
  r <- spec$comparison
  if (identical(r$mode, "cat")) {
    sig <- !is.na(r$p_value) && r$p_value < 0.05
    vcl <- if (sig) "verdict-sig" else "verdict-ns"
    verdict <- sprintf(
      "<span class=\"%s\">%s association between %s and %s</span> (%s).",
      vcl, if (sig) "Significant" else "No significant", html_escape(r$var1),
      html_escape(r$var2), .report_p(r$p_value))
    stats_tbl <- data.frame(
      Statistic = c("Chi-square", "df", CRAMERS_V, "N"),
      Value = c(round(r$statistic, 3), r$df, round(r$cramers_v, 3), r$n),
      check.names = FALSE)
    rc <- sprintf(" (rows: %s, columns: %s)",
                  html_escape(r$var1), html_escape(r$var2))
    body <- paste0(
      "<p>", verdict, "</p>",
      "<p class=\"note\">Association strength (", CRAMERS_V, "): ",
      effect_magnitude(CRAMERS_V, r$cramers_v), ".</p>",
      df_to_html(stats_tbl, "Test statistics"),
      .mat_to_html(r$table, paste0("Contingency table", rc), r$var1),
      .mat_to_html(r$expected, paste0("Expected counts", rc), r$var1),
      .mat_to_html(r$stdres,
                   paste0("Standardized residuals", rc,
                          " (|z| &gt; 2 flags a driver cell)"), r$var1))
    pcts <- r$pcts %||% character(0)
    if ("row" %in% pcts)
      body <- paste0(body, .mat_to_html(
        r$row_pct, paste0("Row %", rc, " (each row sums to 100)"), r$var1))
    if ("col" %in% pcts)
      body <- paste0(body, .mat_to_html(
        r$col_pct, paste0("Column %", rc, " (each column sums to 100)"),
        r$var1))
    if ("total" %in% pcts)
      body <- paste0(body, .mat_to_html(
        r$total_pct, paste0("Total %", rc, " (whole table sums to 100)"),
        r$var1))
  } else if (identical(r$mode, "num_multi")) {
    body <- paste0(
      sprintf(paste0("<p class=\"note\">One test per outcome x group. p_adj ",
                     "corrects across all %d combinations (method: %s).</p>"),
              nrow(r$grid$summary), r$p_adjust %||% "BH"),
      df_to_html(round_df(r$grid$summary), "All combinations"))
    for (i in seq_along(r$grid$keys))
      body <- paste0(body, "<h3>", html_escape(r$grid$keys[i]), "</h3>",
                     .section_comparison_num(r$grid$results[[i]]))
  } else {
    body <- .section_comparison_num(r)
  }
  code <- if (isTRUE(spec$show_code)) code_block_html(spec$compare_code) else ""
  paste0("<h2 id=\"comparison\">Group comparison</h2>", body, code)
}

.section_regression <- function(spec, reg_uris) {
  m  <- spec$model; info <- spec$model_interp
  f  <- paste(deparse(stats::formula(m)), collapse = " ")
  logit <- identical(info$family, "binomial")
  sig <- !is.na(info$overall_p) && info$overall_p < 0.05
  vcl <- if (sig) "verdict-sig" else "verdict-ns"
  # Logistic models get a McFadden line (a linear-R2 reading would be wrong)
  # and a likelihood-ratio label for the overall test.
  headline <- if (logit) sprintf(
    "<p><b>Model:</b> <code>%s</code> (logistic)</p><p><b>McFadden pseudo-R&sup2; = %s</b> \u2014 0.2\u20130.4 already indicates a good fit for a logistic model (this is not %% of variance). <span class=\"%s\">%s (likelihood-ratio test, %s).</span></p>",
    html_escape(f), info$r2, vcl,
    if (sig) "Overall model is significant" else "Overall model is not significant",
    .report_p(info$overall_p))
  else sprintf(
    "<p><b>Model:</b> <code>%s</code></p><p><b>R&sup2; = %s</b> (adjusted %s) \u2014 explains %s%% of the variance. <span class=\"%s\">%s (%s).</span></p>",
    html_escape(f), info$r2, info$adj_r2, round(info$r2 * 100, 1), vcl,
    if (sig) "Overall model is significant" else "Overall model is not significant",
    .report_p(info$overall_p))
  co <- as.data.frame(summary(m)$coefficients, check.names = FALSE)
  co <- cbind(Term = rownames(co), co); rownames(co) <- NULL
  sig_txt <- if (length(info$significant))
    paste(info$significant, collapse = ", ") else "none"
  diag_note <- if (logit) paste0(
    "<p class=\"note\">For a logistic model these plots show deviance ",
    "residuals against fitted probabilities (the response is 0/1).</p>") else ""
  diag <- if (length(reg_uris) >= 1)
    paste0("<h3>Diagnostics</h3>", diag_note,
           paste0(sprintf("<figure><img src=\"%s\" alt=\"Diagnostic\"></figure>", reg_uris),
                  collapse = "")) else ""
  code <- if (isTRUE(spec$show_code)) code_block_html(spec$regression_code) else ""
  paste0("<h2 id=\"regression\">Regression</h2>", headline,
         df_to_html(co, "Coefficients"),
         sprintf("<p class=\"note\">Significant predictor(s): %s.</p>", html_escape(sig_txt)),
         diag, code)
}

# One model-report payload as HTML: heading, formula block, each table, each
# verbatim block, the engine's notes, and (optionally) its code.
.model_payload_html <- function(pl, show_code) {
  fml <- if (length(pl$formulas))
    sprintf("<pre class=\"code\"><code>%s</code></pre>",
            html_escape(paste(sprintf("%-13s %s", paste0(names(pl$formulas), ":"),
                                      unname(pl$formulas)), collapse = "\n")))
  tabs <- paste0(vapply(names(pl$tables), function(nm)
    df_to_html(pl$tables[[nm]], nm), character(1)), collapse = "")
  txts <- paste0(vapply(names(pl$texts), function(nm)
    paste0("<h4>", html_escape(nm), "</h4>",
           "<pre class=\"code\"><code>", html_escape(
             paste(pl$texts[[nm]], collapse = "\n")), "</code></pre>"),
    character(1)), collapse = "")
  notes <- if (length(pl$notes))
    paste0("<p class=\"note\">", paste(html_escape(pl$notes), collapse = "<br>"),
           "</p>")
  code <- if (isTRUE(show_code)) code_block_html(pl$code) else ""
  # The payload title sits a level above its own sub-blocks (h4), or two
  # models in one report run together as a flat wall of same-size
  # headings. The Word twin already nests heading 2 / heading 3.
  paste0("<h3>", html_escape(pl$title), "</h3>", fml, tabs, txts, notes, code)
}

# The Mixed models section: every payload the app contributed, in order.
.section_mixed <- function(spec) {
  paste0("<h2 id=\"mixed\">Mixed models</h2>",
         paste0(vapply(spec$mixed, .model_payload_html, character(1),
                       show_code = spec$show_code), collapse = ""))
}

#' Assemble the full self-contained HTML report as a single string. Pure: takes
#' pre-rasterized plot data URIs so it can be unit-tested with fakes.
#'
#' @param spec A list from report_spec().
#' @param plot_uris Character vector of data URIs aligned to spec$plots.
#' @param reg_uris Character vector of data URIs for regression diagnostics (0-2).
#' @param map_uris Character vector of data URIs aligned to spec$maps. An NA
#'   entry renders an explanatory note instead of an image.
#' @return A complete HTML document (character scalar).
#' @export
build_report_html <- function(spec, plot_uris = character(0),
                              reg_uris = character(0),
                              map_uris = character(0)) {
  sec <- spec$sections
  parts <- character(0)
  if (isTRUE(sec[["overview"]]))   parts <- c(parts, .section_overview(spec))
  if (isTRUE(sec[["summary"]]))    parts <- c(parts, .section_summary(spec))
  if (isTRUE(sec[["charts"]]))     parts <- c(parts, .section_charts(spec, plot_uris))
  if (isTRUE(sec[["maps"]]))       parts <- c(parts, .section_maps(spec, map_uris))
  if (isTRUE(sec[["comparison"]])) parts <- c(parts, .section_comparison(spec))
  if (isTRUE(sec[["regression"]])) parts <- c(parts, .section_regression(spec, reg_uris))
  if (isTRUE(sec[["mixed"]]))      parts <- c(parts, .section_mixed(spec))

  toc_items <- c(
    overview   = "Data overview", summary = "Summary", charts = "Charts",
    maps       = "Maps",
    comparison = "Group comparison", regression = "Regression",
    mixed      = "Mixed models")
  toc <- paste0(vapply(names(toc_items), function(k)
    if (isTRUE(sec[[k]]))
      sprintf("<li><a href=\"#%s\">%s</a></li>", k, toc_items[[k]]) else "",
    character(1)), collapse = "")

  logo_html <- if (!is.null(spec$logo))
    sprintf("<img src=\"%s\" alt=\"UF/IFAS\">", spec$logo) else ""
  when <- format(spec$generated, "%B %d, %Y at %H:%M")

  paste0(
    "<!DOCTYPE html>\n<html lang=\"en\"><head><meta charset=\"utf-8\">",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    "<title>", html_escape(spec$title), "</title><style>", .report_css,
    "</style></head><body><div class=\"report\">",
    "<header class=\"masthead\">", logo_html,
    "<div><h1>", html_escape(spec$title), "</h1>",
    "<p class=\"meta\">Generated ", when, " \u00b7 UF/IFAS Data Explorer</p></div></header>",
    "<div class=\"toc\"><b>Contents</b><ul>", toc, "</ul></div>",
    paste0(parts, collapse = ""),
    "<footer>Generated by the UF/IFAS Data Explorer. ",
    "Statistical significance does not imply practical importance.</footer>",
    "</div></body></html>")
}

# --- Word (.docx) document (officer) ----------------------------------------
# Same report_spec, a second renderer. Tables carry the exact data the app
# produced (re-rendered as native, editable Word tables); charts embed as PNGs.
# Needs the `officer` package; the module/app only calls this when the user
# picks the Word format.

# A data frame with every cell formatted to display text (rounds numbers, blanks
# NA) -- handles factor columns without leaking their integer codes.
.display_df <- function(df) {
  as.data.frame(
    lapply(df, function(col) {
      if (is.numeric(col)) vapply(col, .fmt_cell, character(1))
      else vapply(as.character(col), .fmt_cell, character(1))
    }),
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

# Split a wide frame into readable chunks: each keeps the first `keep`
# label column(s) plus up to (max_cols - keep) further columns, so every
# chunk stands alone. Word can't scroll sideways, and its autofit crams a
# 13-column table into unreadable slivers -- chunking beats transposing
# because the widest offenders (chi-square matrices) are level x level, so
# a transpose just moves the problem to the other axis. Returns a list of
# frames; each carries attr "col_range" (first/last original column index)
# when a split actually happened.
split_wide_df <- function(df, max_cols = 10L, keep = 1L) {
  nc <- ncol(df)
  if (nc <= max_cols) return(list(df))
  keep <- max(0L, min(keep, nc - 1L))
  lead <- seq_len(keep)
  rest <- setdiff(seq_len(nc), lead)
  per  <- max(1L, max_cols - keep)
  starts <- seq(1L, length(rest), by = per)
  lapply(starts, function(s) {
    idx <- rest[s:min(s + per - 1L, length(rest))]
    chunk <- df[, c(lead, idx), drop = FALSE]
    attr(chunk, "col_range") <- c(idx[1], idx[length(idx)])
    chunk
  })
}

.docx_add_table <- function(doc, df, caption = NULL) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df))
    return(officer::body_add_par(doc, "(nothing to show)", style = "Normal"))
  if (!is.null(caption))
    doc <- officer::body_add_par(doc, caption, style = "heading 3")
  # Autofit at full page width beats Word's default fixed layout for the
  # kit's mixed-width tables; genuinely wide frames are chunked so no chunk
  # exceeds 10 columns, with the row-identifying columns repeated in EVERY
  # chunk. Identity spans the LEADING RUN of columns up to the last
  # non-numeric one among the first four (grouped summaries are keyed by
  # group(s) + Variable, the stratified compare grid by Outcome + Group +
  # Stratum, matrices by their promoted rowname) -- keep = 1 alone left
  # second chunks whose rows could not be told apart.
  props <- officer::prop_table(
    style  = "table_template",
    layout = officer::table_layout(type = "autofit"),
    width  = officer::table_width(width = 1, unit = "pct"),
    tcf    = officer::table_conditional_formatting(first_row = TRUE))
  lead    <- seq_len(min(4L, ncol(df)))
  nonnum  <- lead[!vapply(df[lead], is.numeric, logical(1))]
  keep_n  <- if (length(nonnum)) max(nonnum) else 1L
  chunks <- split_wide_df(df, keep = keep_n)
  for (chunk in chunks) {
    rng <- attr(chunk, "col_range")
    if (!is.null(rng))
      doc <- officer::body_add_par(
        doc, sprintf("(columns %d-%d of %d)", rng[1], rng[2], ncol(df)),
        style = "Normal")
    doc <- officer::body_add_blocks(doc, officer::block_list(
      officer::block_table(.display_df(chunk), header = TRUE,
                           properties = props)))
  }
  doc
}

# Add a (possibly multi-line) R code block as monospace paragraphs.
.docx_add_code <- function(doc, code) {
  if (is.null(code) || !nzchar(code)) return(doc)
  mono <- officer::fp_text(font.family = "Consolas", font.size = 9)
  for (ln in strsplit(code, "\n", fixed = TRUE)[[1]])
    doc <- officer::body_add_fpar(
      doc, officer::fpar(officer::ftext(if (nzchar(ln)) ln else " ", mono)))
  doc
}

# A matrix as a Word table with its row names promoted to a leading column,
# headed by the ROW variable's name (the matrix itself has no dimnames).
.docx_add_mat <- function(doc, m, caption, row_var = " ") {
  d <- cbind(stats::setNames(data.frame(rownames(m), stringsAsFactors = FALSE),
                             row_var),
             as.data.frame.matrix(m))
  .docx_add_table(doc, d, caption)
}

# The body for ONE numeric comparison; shared by the single report and the grid.
.docx_section_comparison_num <- function(doc, r) {
  sig <- !is.na(r$p_value) && r$p_value < 0.05
  eff <- if (!is.na(r$effect_value))
    sprintf(" Effect size (%s) = %s (%s).", r$effect_name,
            round(r$effect_value, 3),
            effect_magnitude(r$effect_name, r$effect_value)) else ""
  doc <- officer::body_add_par(doc, sprintf(
    "%s difference in %s across %s (%s, p = %s).%s",
    if (sig) "Significant" else "No significant", r$outcome, r$group,
    r$test, .p_txt(r$p_value), eff), style = "Normal")
  if (!is.null(r$fit_stats))
    doc <- officer::body_add_par(doc, sprintf(
      "Summary of fit: R-squared = %s, adjusted R-squared = %s, RMSE = %s.",
      round(r$fit_stats$r_squared, 3), round(r$fit_stats$adj_r_squared, 3),
      round(r$fit_stats$rmse, 3)), style = "Normal")
  if (!is.null(r$welch))
    doc <- officer::body_add_par(doc, sprintf(
      "Welch's ANOVA (unequal variances): F(%s, %s) = %s, p = %s.",
      round(r$welch$df1), round(r$welch$df2, 1),
      round(r$welch$statistic, 3), .p_txt(r$welch$p_value)), style = "Normal")
  doc <- .docx_add_table(doc, r$group_stats, "Group means")
  if (!is.null(r$anova_table))
    doc <- .docx_add_table(doc, r$anova_table, "ANOVA table")
  if (!is.null(r$posthoc))
    doc <- .docx_add_table(doc, r$posthoc, sprintf(
      "Pairwise comparisons (%s)", r$posthoc_name %||% "post-hoc"))
  if (!is.null(r$cld))
    doc <- .docx_add_table(doc, r$cld, "Connecting letters")
  doc
}

.docx_section_comparison <- function(doc, spec) {
  r   <- spec$comparison
  doc <- officer::body_add_par(doc, "Group comparison", style = "heading 1")
  if (identical(r$mode, "cat")) {
    sig <- !is.na(r$p_value) && r$p_value < 0.05
    doc <- officer::body_add_par(doc, sprintf(
      "%s association between %s and %s (p = %s). %s = %s (%s).",
      if (sig) "Significant" else "No significant", r$var1, r$var2,
      .p_txt(r$p_value), CRAMERS_V, round(r$cramers_v, 3),
      effect_magnitude(CRAMERS_V, r$cramers_v)), style = "Normal")
    stats_tbl <- data.frame(
      Statistic = c("Chi-square", "df", CRAMERS_V, "N"),
      Value = c(round(r$statistic, 3), r$df, round(r$cramers_v, 3), r$n),
      check.names = FALSE)
    doc <- .docx_add_table(doc, stats_tbl, "Test statistics")
    rc <- sprintf(" (rows: %s, columns: %s)", r$var1, r$var2)
    doc <- .docx_add_mat(doc, r$table,
                         paste0("Contingency table", rc), r$var1)
    doc <- .docx_add_mat(doc, r$expected,
                         paste0("Expected counts", rc), r$var1)
    doc <- .docx_add_mat(doc, r$stdres,
                         paste0("Standardized residuals", rc,
                                " (|z| > 2 flags a driver cell)"), r$var1)
    pcts <- r$pcts %||% character(0)
    if ("row" %in% pcts)
      doc <- .docx_add_mat(doc, r$row_pct,
                           paste0("Row %", rc, " (each row sums to 100)"),
                           r$var1)
    if ("col" %in% pcts)
      doc <- .docx_add_mat(doc, r$col_pct,
                           paste0("Column %", rc, " (each column sums to 100)"),
                           r$var1)
    if ("total" %in% pcts)
      doc <- .docx_add_mat(doc, r$total_pct,
                           paste0("Total %", rc, " (whole table sums to 100)"),
                           r$var1)
  } else if (identical(r$mode, "num_multi")) {
    doc <- officer::body_add_par(doc, sprintf(
      "One test per outcome x group. p_adj corrects across all %d combinations (method: %s).",
      nrow(r$grid$summary), r$p_adjust %||% "BH"), style = "Normal")
    doc <- .docx_add_table(doc, round_df(r$grid$summary), "All combinations")
    for (i in seq_along(r$grid$keys)) {
      doc <- officer::body_add_par(doc, r$grid$keys[i], style = "heading 2")
      doc <- .docx_section_comparison_num(doc, r$grid$results[[i]])
    }
  } else {
    doc <- .docx_section_comparison_num(doc, r)
  }
  if (isTRUE(spec$show_code)) doc <- .docx_add_code(doc, spec$compare_code)
  doc
}

# The Mixed models section for Word: the same payloads the HTML path walks.
.docx_section_mixed <- function(doc, spec) {
  doc <- officer::body_add_par(doc, "Mixed models", style = "heading 1")
  for (pl in spec$mixed) {
    doc <- officer::body_add_par(doc, pl$title, style = "heading 2")
    for (nm in names(pl$formulas))
      doc <- officer::body_add_par(doc, sprintf("%s: %s", nm, pl$formulas[[nm]]),
                                   style = "Normal")
    for (nm in names(pl$tables))
      doc <- .docx_add_table(doc, pl$tables[[nm]], nm)
    for (nm in names(pl$texts)) {
      doc <- officer::body_add_par(doc, nm, style = "heading 3")
      doc <- .docx_add_code(doc, paste(pl$texts[[nm]], collapse = "\n"))
    }
    for (n in pl$notes)
      doc <- officer::body_add_par(doc, n, style = "Normal")
    if (isTRUE(spec$show_code) && !is.null(pl$code))
      doc <- .docx_add_code(doc, pl$code)
  }
  doc
}

.docx_section_regression <- function(doc, spec, reg_paths) {
  m <- spec$model; info <- spec$model_interp
  f <- gsub("\\s+", " ", paste(deparse(stats::formula(m)), collapse = " "))
  sig <- !is.na(info$overall_p) && info$overall_p < 0.05
  doc <- officer::body_add_par(doc, "Regression", style = "heading 1")
  doc <- officer::body_add_par(doc, sprintf("Model: %s", f), style = "Normal")
  doc <- officer::body_add_par(doc, sprintf(
    "R-squared = %s (adjusted %s) \u2014 explains %s%% of the variance. %s (p = %s).",
    info$r2, info$adj_r2, round(info$r2 * 100, 1),
    if (sig) "Overall model is significant" else "Overall model is not significant",
    .p_txt(info$overall_p)), style = "Normal")
  co <- as.data.frame(summary(m)$coefficients, check.names = FALSE)
  co <- cbind(Term = rownames(co), co); rownames(co) <- NULL
  doc <- .docx_add_table(doc, co, "Coefficients")
  sig_txt <- if (length(info$significant)) paste(info$significant, collapse = ", ") else "none"
  doc <- officer::body_add_par(doc, sprintf("Significant predictor(s): %s.", sig_txt),
                               style = "Normal")
  if (length(reg_paths)) {
    doc <- officer::body_add_par(doc, "Diagnostics", style = "heading 2")
    for (p in reg_paths)
      if (!is.na(p) && file.exists(p))
        doc <- officer::body_add_img(doc, p, width = 5, height = 5 * 4 / 5.5)
  }
  if (isTRUE(spec$show_code)) doc <- .docx_add_code(doc, spec$regression_code)
  doc
}

#' Build the report as an editable Word document (an officer `rdocx`).
#'
#' @param spec A list from report_spec().
#' @param plot_paths Character vector of PNG file paths aligned to spec$plots.
#' @param reg_paths PNG file paths for the regression diagnostics (0-2).
#' @param map_paths PNG file paths aligned to spec$maps. An NA entry writes an
#'   explanatory line instead of an image.
#' @return An `rdocx` object (print() it to a .docx file).
#' @export
build_report_docx <- function(spec, plot_paths = character(0),
                              reg_paths = character(0),
                              map_paths = character(0)) {
  if (!requireNamespace("officer", quietly = TRUE))
    stop("The 'officer' package is required for Word reports. install.packages('officer')")
  sec <- spec$sections
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, spec$title, style = "heading 1")
  doc <- officer::body_add_par(doc, sprintf(
    "Generated %s \u2014 UF/IFAS Data Explorer",
    format(spec$generated, "%B %d, %Y at %H:%M")), style = "Normal")

  if (isTRUE(sec[["overview"]])) {
    g <- spec$glance
    doc <- officer::body_add_par(doc, "Data overview", style = "heading 1")
    doc <- officer::body_add_par(doc, sprintf(
      "%s rows x %s columns \u2014 %d numeric, %d categorical, %d date; %s complete rows.",
      format(g$n, big.mark = ","), g$m, g$num, g$cat, g$date,
      format(g$complete, big.mark = ",")), style = "Normal")
    doc <- .docx_add_table(doc, spec$profile, "Columns")
  }
  if (isTRUE(sec[["summary"]])) {
    doc <- officer::body_add_par(doc, "Summary", style = "heading 1")
    doc <- .docx_add_table(doc, spec$summary)
    if (isTRUE(spec$show_code)) doc <- .docx_add_code(doc, spec$summary_code)
  }
  if (isTRUE(sec[["charts"]])) {
    doc <- officer::body_add_par(doc, "Charts", style = "heading 1")
    for (i in seq_along(spec$plots)) {
      doc <- officer::body_add_par(doc, paste("Chart", i), style = "heading 2")
      pp <- if (i <= length(plot_paths)) plot_paths[[i]] else NA_character_
      if (!is.na(pp) && file.exists(pp))
        doc <- officer::body_add_img(doc, pp, width = 6, height = 6 * 4.5 / 7)
      if (isTRUE(spec$show_code) && length(spec$plot_code) >= i)
        doc <- .docx_add_code(doc, spec$plot_code[[i]])
    }
  }
  if (isTRUE(sec[["maps"]])) {
    doc <- officer::body_add_par(doc, "Maps", style = "heading 1")
    for (i in seq_along(spec$maps)) {
      doc <- officer::body_add_par(doc, paste("Map", i), style = "heading 2")
      mp <- if (i <= length(map_paths)) map_paths[[i]] else NA_character_
      if (!is.na(mp) && file.exists(mp)) {
        # Snapshots come back at MAP_PNG_W x MAP_PNG_H; keep that aspect ratio.
        doc <- officer::body_add_img(doc, mp, width = 6,
                                     height = 6 * MAP_PNG_H / MAP_PNG_W)
      } else {
        doc <- officer::body_add_par(doc, paste(
          "(map snapshot unavailable - it needs the optional webshot2 and",
          "chromote packages plus Chrome)"), style = "Normal")
      }
      if (isTRUE(spec$show_code) && length(spec$map_code) >= i)
        doc <- .docx_add_code(doc, spec$map_code[[i]])
    }
  }
  if (isTRUE(sec[["comparison"]])) doc <- .docx_section_comparison(doc, spec)
  if (isTRUE(sec[["regression"]])) doc <- .docx_section_regression(doc, spec, reg_paths)
  if (isTRUE(sec[["mixed"]]))      doc <- .docx_section_mixed(doc, spec)
  doc
}

# --- rasterization + file output (impure) -----------------------------------

#' Rasterize one ggplot to a temp PNG file; returns the path (or NA). For the
#' Word path, which embeds images from files rather than data URIs.
#' @noRd
plot_to_png_file <- function(plot, width = 7, height = 4.5, dpi = 110) {
  if (is.null(plot)) return(NA_character_)
  tmp <- tempfile(fileext = ".png")
  ok <- tryCatch({
    ggplot2::ggsave(tmp, plot = plot, width = width, height = height,
                    dpi = dpi, units = "in", device = "png", bg = "white")
    TRUE
  }, error = function(e) FALSE)
  if (!ok || !file.exists(tmp)) return(NA_character_)
  tmp
}

#' Rasterize one ggplot to a base64 PNG data URI. Requires ggplot2 attached.
#' @noRd
plot_to_data_uri <- function(plot, width = 7, height = 4.5, dpi = 110) {
  if (is.null(plot)) return(NA_character_)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  ok <- tryCatch({
    ggplot2::ggsave(tmp, plot = plot, width = width, height = height,
                    dpi = dpi, units = "in", device = "png", bg = "white")
    TRUE
  }, error = function(e) FALSE)
  if (!ok || !file.exists(tmp)) return(NA_character_)
  base64enc::dataURI(file = tmp, mime = "image/png")
}

# A leaflet map is an htmlwidget, not a ggplot, so it can't go through
# ggsave() -- it needs a headless-browser snapshot (save_map_png, helpers_map.R).
# That depends on Suggests-only webshot2/chromote plus a real Chrome, so BOTH
# helpers below return NA rather than erroring: for the report, a missing
# snapshot means "skip this figure", never "fail the whole download".
#' Snapshot one leaflet widget to a PNG file. NA when unavailable.
#' @noRd
map_to_png_file <- function(widget, width = MAP_PNG_W, height = MAP_PNG_H) {
  if (is.null(widget) || !map_snapshot_ok()) return(NA_character_)
  # mod_map stamps the on-screen pane size onto the widget so the report can
  # reproduce the user's exact view (see snapshot_spec()). Without it we fall
  # back to the fixed canvas, which frames a different area.
  sp   <- attr(widget, "fox_snapshot")
  zm   <- 1
  if (is.list(sp) && is.numeric(sp$width) && is.numeric(sp$height)) {
    width <- sp$width; height <- sp$height; zm <- sp$zoom %||% 1
  }
  tmp <- tempfile(fileext = ".png")
  ok  <- tryCatch({ save_map_png(widget, tmp, width = width, height = height,
                                 zoom = zm)
                    TRUE }, error = function(e) FALSE)
  if (!isTRUE(ok) || !file.exists(tmp) || file.size(tmp) == 0)
    return(NA_character_)
  tmp
}

#' Snapshot one leaflet widget to a base64 PNG data URI. NA when unavailable.
#' @noRd
map_to_data_uri <- function(widget, width = MAP_PNG_W, height = MAP_PNG_H) {
  f <- map_to_png_file(widget, width = width, height = height)
  if (is.na(f)) return(NA_character_)
  on.exit(unlink(f), add = TRUE)
  base64enc::dataURI(file = f, mime = "image/png")
}

#' Render a report spec to a file. HTML = a self-contained .html (charts as
#' base64 data URIs); Word = an editable .docx via officer (charts as embedded
#' PNGs). Both rasterize the charts and, if a model is present, the regression
#' diagnostics.
#'
#' @param spec A list from report_spec().
#' @param file Output path.
#' @param format "html" or "docx".
#' @param width,height,dpi Per-chart image size.
#' @return `file`, invisibly.
#' @export
render_report <- function(spec, file, format = c("html", "docx"),
                          width = 7, height = 4.5, dpi = 110) {
  stopifnot(is.list(spec))
  format <- match.arg(format)

  if (identical(format, "docx")) {
    plot_paths <- if (!is.null(spec$plots))
      vapply(spec$plots, plot_to_png_file, character(1),
             width = width, height = height, dpi = dpi)
    else character(0)
    reg_paths <- character(0)
    if (!is.null(spec$model)) {
      diag <- list(reg_fitted_gg(spec$model), reg_resid_gg(spec$model))
      reg_paths <- vapply(diag, plot_to_png_file, character(1),
                          width = 5.5, height = 4, dpi = dpi)
      reg_paths <- reg_paths[!is.na(reg_paths)]
    }
    # Snapshots are the slow part of a report (a headless browser launch each,
    # ~7s), so they are only taken when the spec actually carries maps.
    map_paths <- if (!is.null(spec$maps))
      vapply(spec$maps, map_to_png_file, character(1))
    else character(0)
    doc <- build_report_docx(spec, plot_paths = plot_paths,
                             reg_paths = reg_paths, map_paths = map_paths)
    print(doc, target = file)
    return(invisible(file))
  }

  plot_uris <- if (!is.null(spec$plots))
    vapply(spec$plots, plot_to_data_uri, character(1),
           width = width, height = height, dpi = dpi)
  else character(0)
  reg_uris <- character(0)
  if (!is.null(spec$model)) {
    diag <- list(reg_fitted_gg(spec$model), reg_resid_gg(spec$model))
    reg_uris <- vapply(diag, plot_to_data_uri, character(1),
                       width = 5.5, height = 4, dpi = dpi)
    reg_uris <- reg_uris[!is.na(reg_uris)]
  }
  map_uris <- if (!is.null(spec$maps))
    vapply(spec$maps, map_to_data_uri, character(1))
  else character(0)
  html <- build_report_html(spec, plot_uris = plot_uris, reg_uris = reg_uris,
                            map_uris = map_uris)
  writeLines(html, file, useBytes = TRUE)
  invisible(file)
}
