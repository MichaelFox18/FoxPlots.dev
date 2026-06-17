# ============================================================
# helpers_report.R — assemble a session into a self-contained HTML report
# ============================================================
# Builds a single, portable .html report from whatever the user produced in a
# session: a data overview, the grouped summary, the charts, a group comparison,
# and a regression. Deliberately PANDOC-FREE — the HTML is assembled as a plain
# string with inlined CSS and base64-embedded plot images, so it renders the
# same from RStudio, VS Code, a bare Rscript, or a deployed app, with no extra
# system software. mod_report.R is a thin wrapper over these.
#
# Split for testability:
#   • report_spec()        — pure: normalize the session artifacts into a spec.
#   • summary_code() / compare_code() / regression_code() — pure: reproducible R.
#   • df_to_html() / html_escape() / build_report_html() — pure: string output.
#   • plot_to_data_uri() / render_report() — the only impure bits (rasterize a
#     ggplot to a PNG data URI, write the file). Smoke-tested, not unit-tested.
#
# Reuses data_glance()/column_profile() (helpers_stats), model_interpretation()
# (helpers_model), and effect_magnitude() (helpers_compare). base64enc ships
# with Shiny, so it is always available.

# --- small string utilities (pure) ------------------------------------------

#' HTML-escape a character vector (&, <, >, ").
html_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;",  x, fixed = TRUE)
  x <- gsub("<", "&lt;",   x, fixed = TRUE)
  x <- gsub(">", "&gt;",   x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

#' Format one display cell as plain text: round numbers, blank out NA. No HTML
#' escaping (so it's reusable for Word) — the HTML path escapes on top.
.fmt_cell <- function(x) {
  if (length(x) != 1L) x <- x[1]
  if (is.na(x)) return("")
  if (is.numeric(x)) {
    if (is.finite(x) && x == round(x)) format(x, big.mark = ",")
    else format(round(as.numeric(x), 4), big.mark = ",")
  } else as.character(x)
}

#' HTML display cell = the plain cell, escaped.
.report_cell <- function(x) html_escape(.fmt_cell(x))

#' Plain-text p-value (no HTML entity) — for the Word path.
.p_txt <- function(p) {
  if (is.null(p) || is.na(p)) return("N/A")
  if (p < 0.001) "< 0.001" else as.character(round(p, 4))
}

#' Render a data frame as an HTML <table>. Pure (base R string building).
#' @param df A data frame.
#' @param caption Optional caption shown above the table.
#' @param max_rows Truncate to this many rows (a note is appended if cut).
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
  paste0(cap, "<table><thead><tr>", head_html, "</tr></thead><tbody>",
         paste0(rows_html, collapse = ""), "</tbody></table>", more)
}

#' A static, non-executed R code block.
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
#' carries var1/var2; compare_groups_numeric carries outcome/group), so it does
#' not depend on the $mode tag the module adds.
compare_code <- function(res) {
  if (is.null(res) || !is.list(res)) return(NULL)
  is_cat <- !is.null(res$var1) && !is.null(res$var2) && is.null(res$outcome)
  if (is_cat)
    return(sprintf("chisq.test(table(data[[\"%s\"]], data[[\"%s\"]]))",
                   res$var1, res$var2))
  if (is.null(res$outcome) || is.null(res$group)) return(NULL)
  o <- res$outcome; g <- res$group
  test <- if (is.null(res$test)) "" else res$test
  if (grepl("ANOVA", test))
    sprintf("fit <- aov(%s ~ %s, data = data)\nsummary(fit)\nTukeyHSD(fit)", o, g)
  else if (grepl("Kruskal", test))
    sprintf("kruskal.test(%s ~ %s, data = data)", o, g)
  else if (grepl("Wilcoxon", test))
    sprintf("wilcox.test(%s ~ %s, data = data)", o, g)
  else {
    ve <- if (grepl("equal variance", test)) "TRUE" else "FALSE"
    sprintf("t.test(%s ~ %s, data = data, var.equal = %s)", o, g, ve)
  }
}

#' Reproduce the lm() call from a fitted model.
regression_code <- function(model) {
  if (is.null(model) || !inherits(model, "lm")) return(NULL)
  f <- paste(deparse(stats::formula(model)), collapse = " ")
  f <- gsub("\\s+", " ", f)
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
#' @param comparison Result list from mod_compare, or NULL.
#' @param model Fitted lm from mod_regression, or NULL.
#' @param title Report title.
#' @param show_code Whether the report should reveal reproducible R code.
#' @param logo Optional data-URI string for the masthead logo.
#' @param generated A POSIXct timestamp.
#' @return A named list describing the report (see `sections`).
report_spec <- function(data, summary_tbl = NULL, plots = NULL, plot_code = NULL,
                        comparison = NULL, model = NULL,
                        title = "Data Explorer Report", show_code = FALSE,
                        logo = NULL, generated = Sys.time()) {
  stopifnot(is.data.frame(data))
  has_summary <- is.data.frame(summary_tbl) && nrow(summary_tbl) > 0
  # plots and plot_code are assumed aligned (mod_visualize guarantees this).
  has_charts  <- !is.null(plots) && length(plots) > 0
  has_compare <- !is.null(comparison) && is.list(comparison) && !is.null(comparison$mode)
  has_model   <- !is.null(model) && inherits(model, "lm")

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
    comparison    = if (has_compare) comparison else NULL,
    compare_code  = if (has_compare) compare_code(comparison) else NULL,
    model         = if (has_model) model else NULL,
    model_interp  = if (has_model) model_interpretation(model) else NULL,
    regression_code = if (has_model) regression_code(model) else NULL,
    sections = c(
      overview   = TRUE,
      summary    = has_summary,
      charts     = has_charts,
      comparison = has_compare,
      regression = has_model
    )
  )
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

.section_comparison <- function(spec) {
  r <- spec$comparison
  sig <- !is.na(r$p_value) && r$p_value < 0.05
  vcl <- if (sig) "verdict-sig" else "verdict-ns"
  if (identical(r$mode, "cat")) {
    verdict <- sprintf(
      "<span class=\"%s\">%s association between %s and %s</span> (%s).",
      vcl, if (sig) "Significant" else "No significant", html_escape(r$var1),
      html_escape(r$var2), .report_p(r$p_value))
    stats_tbl <- data.frame(
      Statistic = c("Chi-square", "df", "Cramér's V", "N"),
      Value = c(round(r$statistic, 3), r$df, round(r$cramers_v, 3), r$n),
      check.names = FALSE)
    body <- paste0(
      "<p>", verdict, "</p>",
      "<p class=\"note\">Association strength (Cramér's V): ",
      effect_magnitude("Cramér's V", r$cramers_v), ".</p>",
      df_to_html(stats_tbl, "Test statistics"),
      df_to_html(cbind(` ` = rownames(r$table), as.data.frame.matrix(r$table)),
                 "Contingency table"))
  } else {
    eff <- if (!is.na(r$effect_value))
      sprintf(" Effect size (%s) = %s (%s).", r$effect_name,
              round(r$effect_value, 3),
              effect_magnitude(r$effect_name, r$effect_value)) else ""
    verdict <- sprintf(
      "<span class=\"%s\">%s difference in %s across %s</span> (%s, %s).%s",
      vcl, if (sig) "Significant" else "No significant", html_escape(r$outcome),
      html_escape(r$group), html_escape(r$test), .report_p(r$p_value), eff)
    body <- paste0("<p>", verdict, "</p>", df_to_html(r$group_stats, "Group statistics"))
    if (!is.null(r$posthoc))
      body <- paste0(body, df_to_html(r$posthoc, "Pairwise comparisons (Tukey HSD)"))
  }
  code <- if (isTRUE(spec$show_code)) code_block_html(spec$compare_code) else ""
  paste0("<h2 id=\"comparison\">Group comparison</h2>", body, code)
}

.section_regression <- function(spec, reg_uris) {
  m  <- spec$model; info <- spec$model_interp
  f  <- paste(deparse(stats::formula(m)), collapse = " ")
  sig <- !is.na(info$overall_p) && info$overall_p < 0.05
  vcl <- if (sig) "verdict-sig" else "verdict-ns"
  headline <- sprintf(
    "<p><b>Model:</b> <code>%s</code></p><p><b>R&sup2; = %s</b> (adjusted %s) — explains %s%% of the variance. <span class=\"%s\">%s (%s).</span></p>",
    html_escape(f), info$r2, info$adj_r2, round(info$r2 * 100, 1), vcl,
    if (sig) "Overall model is significant" else "Overall model is not significant",
    .report_p(info$overall_p))
  co <- as.data.frame(summary(m)$coefficients, check.names = FALSE)
  co <- cbind(Term = rownames(co), co); rownames(co) <- NULL
  sig_txt <- if (length(info$significant))
    paste(info$significant, collapse = ", ") else "none"
  diag <- if (length(reg_uris) >= 1)
    paste0("<h3>Diagnostics</h3>",
           paste0(sprintf("<figure><img src=\"%s\" alt=\"Diagnostic\"></figure>", reg_uris),
                  collapse = "")) else ""
  code <- if (isTRUE(spec$show_code)) code_block_html(spec$regression_code) else ""
  paste0("<h2 id=\"regression\">Regression</h2>", headline,
         df_to_html(co, "Coefficients"),
         sprintf("<p class=\"note\">Significant predictor(s): %s.</p>", html_escape(sig_txt)),
         diag, code)
}

#' Assemble the full self-contained HTML report as a single string. Pure: takes
#' pre-rasterized plot data URIs so it can be unit-tested with fakes.
#'
#' @param spec A list from report_spec().
#' @param plot_uris Character vector of data URIs aligned to spec$plots.
#' @param reg_uris Character vector of data URIs for regression diagnostics (0–2).
#' @return A complete HTML document (character scalar).
build_report_html <- function(spec, plot_uris = character(0),
                              reg_uris = character(0)) {
  sec <- spec$sections
  parts <- character(0)
  if (isTRUE(sec[["overview"]]))   parts <- c(parts, .section_overview(spec))
  if (isTRUE(sec[["summary"]]))    parts <- c(parts, .section_summary(spec))
  if (isTRUE(sec[["charts"]]))     parts <- c(parts, .section_charts(spec, plot_uris))
  if (isTRUE(sec[["comparison"]])) parts <- c(parts, .section_comparison(spec))
  if (isTRUE(sec[["regression"]])) parts <- c(parts, .section_regression(spec, reg_uris))

  toc_items <- c(
    overview   = "Data overview", summary = "Summary", charts = "Charts",
    comparison = "Group comparison", regression = "Regression")
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
    "<p class=\"meta\">Generated ", when, " · UF/IFAS Data Explorer</p></div></header>",
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
# NA) — handles factor columns without leaking their integer codes.
.display_df <- function(df) {
  as.data.frame(
    lapply(df, function(col) {
      if (is.numeric(col)) vapply(col, .fmt_cell, character(1))
      else vapply(as.character(col), .fmt_cell, character(1))
    }),
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

.docx_add_table <- function(doc, df, caption = NULL) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df))
    return(officer::body_add_par(doc, "(nothing to show)", style = "Normal"))
  if (!is.null(caption))
    doc <- officer::body_add_par(doc, caption, style = "heading 3")
  officer::body_add_table(doc, .display_df(df), style = "table_template",
                          first_row = TRUE)
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

.docx_section_comparison <- function(doc, spec) {
  r   <- spec$comparison
  sig <- !is.na(r$p_value) && r$p_value < 0.05
  doc <- officer::body_add_par(doc, "Group comparison", style = "heading 1")
  if (identical(r$mode, "cat")) {
    doc <- officer::body_add_par(doc, sprintf(
      "%s association between %s and %s (p = %s). Cramér's V = %s (%s).",
      if (sig) "Significant" else "No significant", r$var1, r$var2,
      .p_txt(r$p_value), round(r$cramers_v, 3),
      effect_magnitude("Cramér's V", r$cramers_v)), style = "Normal")
    stats_tbl <- data.frame(
      Statistic = c("Chi-square", "df", "Cramér's V", "N"),
      Value = c(round(r$statistic, 3), r$df, round(r$cramers_v, 3), r$n),
      check.names = FALSE)
    doc <- .docx_add_table(doc, stats_tbl, "Test statistics")
    doc <- .docx_add_table(
      doc, cbind(` ` = rownames(r$table), as.data.frame.matrix(r$table)),
      "Contingency table")
  } else {
    eff <- if (!is.na(r$effect_value))
      sprintf(" Effect size (%s) = %s (%s).", r$effect_name,
              round(r$effect_value, 3),
              effect_magnitude(r$effect_name, r$effect_value)) else ""
    doc <- officer::body_add_par(doc, sprintf(
      "%s difference in %s across %s (%s, p = %s).%s",
      if (sig) "Significant" else "No significant", r$outcome, r$group,
      r$test, .p_txt(r$p_value), eff), style = "Normal")
    doc <- .docx_add_table(doc, r$group_stats, "Group statistics")
    if (!is.null(r$posthoc))
      doc <- .docx_add_table(doc, r$posthoc, "Pairwise comparisons (Tukey HSD)")
  }
  if (isTRUE(spec$show_code)) doc <- .docx_add_code(doc, spec$compare_code)
  doc
}

.docx_section_regression <- function(doc, spec, reg_paths) {
  m <- spec$model; info <- spec$model_interp
  f <- gsub("\\s+", " ", paste(deparse(stats::formula(m)), collapse = " "))
  sig <- !is.na(info$overall_p) && info$overall_p < 0.05
  doc <- officer::body_add_par(doc, "Regression", style = "heading 1")
  doc <- officer::body_add_par(doc, sprintf("Model: %s", f), style = "Normal")
  doc <- officer::body_add_par(doc, sprintf(
    "R-squared = %s (adjusted %s) — explains %s%% of the variance. %s (p = %s).",
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
#' @param reg_paths PNG file paths for the regression diagnostics (0–2).
#' @return An `rdocx` object (print() it to a .docx file).
build_report_docx <- function(spec, plot_paths = character(0),
                              reg_paths = character(0)) {
  if (!requireNamespace("officer", quietly = TRUE))
    stop("The 'officer' package is required for Word reports. install.packages('officer')")
  sec <- spec$sections
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, spec$title, style = "heading 1")
  doc <- officer::body_add_par(doc, sprintf(
    "Generated %s — UF/IFAS Data Explorer",
    format(spec$generated, "%B %d, %Y at %H:%M")), style = "Normal")

  if (isTRUE(sec[["overview"]])) {
    g <- spec$glance
    doc <- officer::body_add_par(doc, "Data overview", style = "heading 1")
    doc <- officer::body_add_par(doc, sprintf(
      "%s rows x %s columns — %d numeric, %d categorical, %d date; %s complete rows.",
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
  if (isTRUE(sec[["comparison"]])) doc <- .docx_section_comparison(doc, spec)
  if (isTRUE(sec[["regression"]])) doc <- .docx_section_regression(doc, spec, reg_paths)
  doc
}

# --- rasterization + file output (impure) -----------------------------------

#' Rasterize one ggplot to a temp PNG file; returns the path (or NA). For the
#' Word path, which embeds images from files rather than data URIs.
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
    doc <- build_report_docx(spec, plot_paths = plot_paths, reg_paths = reg_paths)
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
  html <- build_report_html(spec, plot_uris = plot_uris, reg_uris = reg_uris)
  writeLines(html, file, useBytes = TRUE)
  invisible(file)
}
