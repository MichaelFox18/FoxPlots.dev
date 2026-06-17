# Tests for helpers_report.R — the pure report-assembly pieces (no pandoc, no
# graphics device needed). Plot rasterization / file output are smoke-tested
# separately, not here.

# --- string utilities -------------------------------------------------------

test_that("html_escape neutralises markup characters", {
  expect_equal(html_escape("a < b & \"c\" > d"),
               "a &lt; b &amp; &quot;c&quot; &gt; d")
})

test_that("df_to_html renders a table with headers and rounds/blanks cells", {
  df  <- data.frame(g = c("a", "b"), Mean = c(1.23456, NA), N = c(10L, 20L))
  out <- df_to_html(df)
  expect_true(grepl("<table>", out, fixed = TRUE))
  expect_true(grepl("<th>Mean</th>", out, fixed = TRUE))
  expect_true(grepl("1.2346", out, fixed = TRUE))   # rounded to 4 dp
  expect_true(grepl("<td></td>", out, fixed = TRUE)) # NA -> blank cell
})

test_that("df_to_html handles an empty/NULL frame gracefully", {
  expect_true(grepl("nothing to show", df_to_html(NULL)))
  expect_true(grepl("nothing to show", df_to_html(data.frame())))
})

test_that("code_block_html wraps code and escapes, empty on blank", {
  expect_equal(code_block_html(NULL), "")
  expect_true(grepl("<pre class=\"code\"><code>x &lt;- 1</code></pre>",
                    code_block_html("x <- 1"), fixed = TRUE))
})

# --- reproducible-code generators -------------------------------------------

test_that("summary_code reconstructs a grouped_summary call (means mode)", {
  tbl  <- grouped_summary(mtcars, c("mpg", "hp"), "cyl")
  code <- summary_code(tbl)
  expect_true(grepl("grouped_summary(data", code, fixed = TRUE))
  expect_true(grepl("groups = c(\"cyl\")", code, fixed = TRUE))
  expect_true(grepl("\"mpg\"", code, fixed = TRUE) && grepl("\"hp\"", code, fixed = TRUE))
})

test_that("summary_code recognises proportions mode via the Level column", {
  props <- data.frame(cyl = c(4, 6), Level = c("a", "b"), N = 1:2, Total = 3:4,
                      Percent = c(50, 50), CI_low = 0, CI_high = 100)
  code  <- summary_code(props)
  expect_true(grepl("proportions_summary(data", code, fixed = TRUE))
  expect_true(grepl("groups = c(\"cyl\")", code, fixed = TRUE))
})

test_that("summary_code returns NULL on empty input", {
  expect_null(summary_code(NULL))
  expect_null(summary_code(data.frame()))
})

test_that("compare_code emits the right call per test branch", {
  # 2-group parametric -> Welch t-test
  r2 <- compare_groups_numeric(mtcars, "mpg", "am")
  expect_true(grepl("t.test(mpg ~ am", compare_code(r2), fixed = TRUE))
  expect_true(grepl("var.equal = FALSE", compare_code(r2), fixed = TRUE))
  # 3-group parametric -> ANOVA + Tukey
  r3 <- compare_groups_numeric(mtcars, "mpg", "cyl")
  expect_true(grepl("aov(mpg ~ cyl", compare_code(r3), fixed = TRUE))
  expect_true(grepl("TukeyHSD", compare_code(r3), fixed = TRUE))
  # non-parametric routing
  rk <- compare_groups_numeric(mtcars, "mpg", "cyl", parametric = FALSE)
  expect_true(grepl("kruskal.test", compare_code(rk), fixed = TRUE))
  rw <- compare_groups_numeric(mtcars, "mpg", "am", parametric = FALSE)
  expect_true(grepl("wilcox.test", compare_code(rw), fixed = TRUE))
  # categorical
  rc <- compare_categorical(mtcars, "cyl", "gear"); rc$mode <- "cat"
  expect_true(grepl("chisq.test(table(", compare_code(rc), fixed = TRUE))
})

test_that("compare_code carries the mode set by the module", {
  r <- compare_groups_numeric(mtcars, "mpg", "am"); r$mode <- "num"
  expect_true(grepl("t.test", compare_code(r), fixed = TRUE))
  expect_null(compare_code(NULL))
})

test_that("regression_code reproduces the lm() call", {
  m    <- fit_model(mtcars, "mpg", c("wt", "hp"), "multiple")
  code <- regression_code(m)
  expect_true(grepl("lm(mpg ~ wt + hp, data = data)", code, fixed = TRUE))
  expect_true(grepl("summary(model)", code, fixed = TRUE))
  expect_null(regression_code(NULL))
  expect_null(regression_code(lm)) # not an lm object
})

# --- report_spec section logic ----------------------------------------------

test_that("report_spec includes a section only when its artifact is present", {
  spec <- report_spec(mtcars)
  expect_true(spec$sections[["overview"]])
  expect_false(spec$sections[["summary"]])
  expect_false(spec$sections[["charts"]])
  expect_false(spec$sections[["comparison"]])
  expect_false(spec$sections[["regression"]])
  expect_false(spec$show_code)

  full <- report_spec(
    mtcars,
    summary_tbl = grouped_summary(mtcars, "mpg", "cyl"),
    plots       = list("p1", "p2"),            # placeholders; spec only counts them
    plot_code   = list("code1", "code2"),
    comparison  = { r <- compare_groups_numeric(mtcars, "mpg", "cyl"); r$mode <- "num"; r },
    model       = fit_model(mtcars, "mpg", "wt", "linear"),
    show_code   = TRUE)
  expect_true(all(full$sections))
  expect_true(full$show_code)
  expect_equal(length(full$plots), 2L)
  expect_false(is.null(full$summary_code))
  expect_false(is.null(full$compare_code))
  expect_false(is.null(full$regression_code))
  expect_false(is.null(full$model_interp))
})

test_that("report_spec requires a data frame", {
  expect_error(report_spec(1:10))
})

# --- build_report_html assembly ---------------------------------------------

test_that("build_report_html emits a self-contained doc with only used sections", {
  spec <- report_spec(
    mtcars,
    summary_tbl = grouped_summary(mtcars, "mpg", "cyl"),
    plots       = list("fake-plot"),
    plot_code   = list("ggplot(...)"),
    show_code   = TRUE,
    logo        = "data:image/png;base64,AAAA")
  html <- build_report_html(spec, plot_uris = "data:image/png;base64,BBBB")

  expect_true(startsWith(html, "<!DOCTYPE html>"))
  expect_true(grepl("</html>", html, fixed = TRUE))
  expect_true(grepl("<style>", html, fixed = TRUE))          # inlined CSS
  expect_true(grepl("id=\"overview\"", html, fixed = TRUE))
  expect_true(grepl("id=\"summary\"", html, fixed = TRUE))
  expect_true(grepl("id=\"charts\"", html, fixed = TRUE))
  expect_true(grepl("data:image/png;base64,BBBB", html, fixed = TRUE)) # embedded chart
  expect_true(grepl("ggplot(...)", html, fixed = TRUE))      # show_code on
  # unused sections absent
  expect_false(grepl("id=\"regression\"", html, fixed = TRUE))
  expect_false(grepl("id=\"comparison\"", html, fixed = TRUE))
})

test_that("build_report_html hides code blocks when show_code is FALSE", {
  spec <- report_spec(mtcars, summary_tbl = grouped_summary(mtcars, "mpg", "cyl"),
                      show_code = FALSE)
  html <- build_report_html(spec)
  expect_false(grepl("grouped_summary(data", html, fixed = TRUE))
})

# --- Word (.docx) renderer (officer) ----------------------------------------

test_that("build_report_docx assembles a Word doc with only the used sections", {
  skip_if_not_installed("officer")
  spec <- report_spec(
    mtcars,
    summary_tbl = grouped_summary(mtcars, "mpg", "cyl"),
    comparison  = { r <- compare_groups_numeric(mtcars, "mpg", "cyl"); r$mode <- "num"; r },
    model       = fit_model(mtcars, "mpg", "wt", "linear"),
    show_code   = TRUE)
  doc <- build_report_docx(spec)            # no charts -> no image files needed
  expect_s3_class(doc, "rdocx")
  txt <- paste(officer::docx_summary(doc)$text, collapse = " | ")
  expect_true(grepl("Data overview", txt))
  expect_true(grepl("Summary", txt))
  expect_true(grepl("Group comparison", txt))
  expect_true(grepl("Regression", txt))
  # show_code on -> the reproducible call appears as text
  expect_true(grepl("lm(mpg ~ wt", txt, fixed = TRUE))
  # a real Word table was added (not just paragraphs)
  expect_true(any(officer::docx_summary(doc)$content_type == "table cell"))
})

test_that("render_report writes a .docx and an .html file", {
  skip_if_not_installed("officer")
  spec <- report_spec(mtcars, summary_tbl = grouped_summary(mtcars, "mpg", "cyl"))
  fd <- withr::local_tempfile(fileext = ".docx")
  render_report(spec, fd, format = "docx")
  expect_true(file.exists(fd) && file.info(fd)$size > 1000)

  fh <- withr::local_tempfile(fileext = ".html")
  render_report(spec, fh, format = "html")
  expect_true(file.exists(fh) && file.info(fh)$size > 1000)
})

test_that(".fmt_cell rounds numbers and blanks NA without HTML escaping", {
  expect_equal(.fmt_cell(1.23456), "1.2346")
  expect_equal(.fmt_cell(NA), "")
  expect_equal(.fmt_cell("a < b"), "a < b")   # no escaping in the plain path
})
