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
  rw <- suppressWarnings(   # mtcars mpg has ties -> expected exact-p warning
    compare_groups_numeric(mtcars, "mpg", "am", parametric = FALSE))
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
  expect_false(spec$sections[["maps"]])
  expect_false(spec$show_code)

  full <- report_spec(
    mtcars,
    summary_tbl = grouped_summary(mtcars, "mpg", "cyl"),
    plots       = list("p1", "p2"),            # placeholders; spec only counts them
    plot_code   = list("code1", "code2"),
    maps        = list("m1"),                  # ditto for maps
    map_code    = list("mcode1"),
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

# ---- maps in the report (v0.6.0) --------------------------------------------
# A leaflet map is an htmlwidget, not a ggplot, so it can't ride the chart path;
# it is snapshotted to a PNG. The snapshot needs Suggests-only webshot2/chromote
# plus Chrome, so the report must degrade to a note rather than fail.

fake_map <- function() structure(list(x = list()), class = c("leaflet", "htmlwidget"))

test_that("report_spec carries maps and flags the section", {
  spec <- report_spec(mtcars, maps = list(fake_map()), map_code = list("# code"))
  expect_true(spec$sections[["maps"]])
  expect_length(spec$maps, 1L)
  expect_equal(spec$map_code[[1]], "# code")
})

test_that("report_spec has no map section when no maps are supplied", {
  spec <- report_spec(mtcars)
  expect_false(spec$sections[["maps"]])
  expect_null(spec$maps)
  # An empty list must not switch the section on either.
  expect_false(report_spec(mtcars, maps = list())$sections[["maps"]])
})

test_that("the HTML report embeds a map image when a snapshot exists", {
  spec <- report_spec(mtcars, maps = list(fake_map()), map_code = list("# code"))
  html <- build_report_html(spec, map_uris = "data:image/png;base64,AAAA")
  expect_match(html, 'id="maps"')
  expect_match(html, 'alt="Map 1"')
  expect_match(html, "data:image/png;base64,AAAA", fixed = TRUE)
})

test_that("the HTML report explains a missing snapshot instead of dropping the section", {
  spec <- report_spec(mtcars, maps = list(fake_map()))
  html <- build_report_html(spec, map_uris = NA_character_)
  expect_match(html, 'id="maps"')             # section still present
  expect_match(html, "snapshot unavailable")  # and says why
  expect_false(grepl('alt="Map 1"', html))    # but no broken <img>
  # Same when the uris vector is simply absent.
  expect_match(build_report_html(spec), "snapshot unavailable")
})

test_that("map code is only shown when show_code is on", {
  spec_on  <- report_spec(mtcars, maps = list(fake_map()),
                          map_code = list("leaflet(df)"), show_code = TRUE)
  spec_off <- report_spec(mtcars, maps = list(fake_map()),
                          map_code = list("leaflet(df)"), show_code = FALSE)
  expect_match(build_report_html(spec_on, map_uris = "x"), "leaflet(df)", fixed = TRUE)
  expect_false(grepl("leaflet(df)", build_report_html(spec_off, map_uris = "x"),
                     fixed = TRUE))
})

test_that("the Word report gains a Maps section", {
  skip_if_not_installed("officer")
  spec <- report_spec(mtcars, maps = list(fake_map()))
  doc  <- build_report_docx(spec, map_paths = NA_character_)
  txt  <- paste(officer::docx_summary(doc)$text, collapse = " ")
  expect_match(txt, "Maps")
  expect_match(txt, "snapshot unavailable")
})

test_that("map_to_png_file and map_to_data_uri return NA rather than erroring", {
  # NULL widget short-circuits regardless of what's installed.
  expect_true(is.na(map_to_png_file(NULL)))
  expect_true(is.na(map_to_data_uri(NULL)))
})

test_that("render_report embeds a real map snapshot end to end", {
  skip_if_not_installed("webshot2")
  skip_if_not_installed("chromote")
  skip_if_not(map_snapshot_ok(), "no headless Chrome available")
  d  <- make_map_example_data()
  cc <- detect_coord_cols(d)
  m  <- build_leaflet_map(d, list(lon = cc$lon, lat = cc$lat, size_by = "acres"))
  spec <- report_spec(d, maps = list(m), map_code = list("# code"))
  f <- withr::local_tempfile(fileext = ".html")
  expect_error(render_report(spec, f, format = "html"), NA)
  html <- paste(readLines(f, warn = FALSE), collapse = "")
  expect_match(html, 'alt="Map 1"')
  expect_false(grepl("snapshot unavailable", html))
})

test_that("a snapshot honours the pane size stamped on the widget", {
  skip_if_not_installed("webshot2")
  skip_if_not_installed("png")
  skip_if_not(map_snapshot_ok(), "no headless Chrome available")
  d  <- make_map_example_data(); cc <- detect_coord_cols(d)
  w  <- build_leaflet_map(d, list(lon = cc$lon, lat = cc$lat))
  # mod_map stamps the pane's real pixel size so the snapshot reproduces the
  # on-screen framing at the on-screen resolution (zoom 1: DPR 2 makes the
  # CartoDB basemap drop ~half its tiles).
  attr(w, "fox_snapshot") <- list(width = 640, height = 400, zoom = 1)
  f <- map_to_png_file(w)
  skip_if(is.na(f), "snapshot failed in this environment")
  on.exit(unlink(f), add = TRUE)
  dims <- dim(png::readPNG(f))
  expect_equal(dims[2], 640L)
  expect_equal(dims[1], 400L)
})

test_that("a widget with no stamp falls back to the default canvas", {
  skip_if_not_installed("webshot2")
  skip_if_not_installed("png")
  skip_if_not(map_snapshot_ok(), "no headless Chrome available")
  d  <- make_map_example_data(); cc <- detect_coord_cols(d)
  w  <- build_leaflet_map(d, list(lon = cc$lon, lat = cc$lat))
  f  <- map_to_png_file(w)
  skip_if(is.na(f), "snapshot failed in this environment")
  on.exit(unlink(f), add = TRUE)
  dims <- dim(png::readPNG(f))
  expect_equal(dims[2], MAP_PNG_W)
  expect_equal(dims[1], MAP_PNG_H)
})

test_that("regression_code is glm-aware (mapping line + glm call + ORs)", {
  d <- mtcars; d$am <- factor(d$am, labels = c("auto", "manual"))
  m <- reg_fit(d, reg_spec("am", c("wt", "hp"), family = "binomial"))
  code <- regression_code(m)
  expect_match(code, "family = binomial", fixed = TRUE)
  expect_match(code, '== "manual"', fixed = TRUE)   # the success mapping
  expect_match(code, "odds ratio", fixed = TRUE)
  expect_error(parse(text = code), NA)
  # lm path unchanged
  expect_match(regression_code(stats::lm(mpg ~ wt, mtcars)), "^model <- lm")
})

test_that("the report headline is logistic-aware and the TOC lists Maps", {
  d <- mtcars; d$vs <- factor(d$vs)
  m <- reg_fit(d, reg_spec("vs", "wt", family = "binomial"))
  spec <- report_spec(d, model = m, maps = list(structure(list(x = list()),
                        class = c("leaflet", "htmlwidget"))))
  html <- build_report_html(spec, map_uris = "data:image/png;base64,AAAA")
  expect_match(html, "McFadden pseudo-R", fixed = TRUE)
  expect_false(grepl("adjusted NA", html, fixed = TRUE))
  expect_match(html, '<li><a href="#maps">Maps</a></li>', fixed = TRUE)
})

test_that("compare_code reproduces the stratified analysis", {
  d <- mtcars
  gr <- compare_grid(d, "mpg", "cyl", split_by = "am")
  res <- list(mode = "num_multi", grid = gr, outcomes = "mpg", groups = "cyl",
              p_adjust = "BH")
  code <- compare_code(res)
  expect_match(code, "strata", fixed = TRUE)
  expect_match(code, "stratum = s", fixed = TRUE)
  expect_error(parse(text = code), NA)
  # and eval reproduces the same number of tests
  env <- new.env(parent = globalenv()); assign("data", d, envir = env)
  out <- eval(parse(text = code), envir = env)
  expect_equal(nrow(out), nrow(gr$summary))
})

# ---- wide-table treatment (0.8.0 report formatting) -----------------------

test_that("df_to_html wraps every table and marks wide ones", {
  narrow <- df_to_html(data.frame(a = 1, b = 2, c = 3))
  expect_match(narrow, "<div class=\"tbl-wrap\">", fixed = TRUE)
  expect_match(narrow, "<table><thead>", fixed = TRUE)     # classless
  expect_no_match(narrow, "class=\"wide\"", fixed = TRUE)
  wide13 <- as.data.frame(setNames(as.list(1:13), paste0("c", 1:13)))
  wide <- df_to_html(wide13)
  expect_match(wide, "<table class=\"wide\">", fixed = TRUE)
  expect_match(wide, "</table></div>", fixed = TRUE)
  expect_match(.report_css, "overflow-x:auto", fixed = TRUE)
  expect_match(.report_css, "table.wide", fixed = TRUE)
})

test_that("split_wide_df chunks with a repeated label column and no loss", {
  wide <- as.data.frame(setNames(as.list(seq_len(13)), paste0("c", 1:13)))
  chunks <- split_wide_df(wide, max_cols = 10L, keep = 1L)
  expect_length(chunks, 2L)
  expect_true(all(vapply(chunks, ncol, integer(1)) <= 10L))
  expect_true(all(vapply(chunks, function(ch) names(ch)[1] == "c1",
                         logical(1))))                    # label repeats
  spread <- unlist(lapply(chunks, function(ch) setdiff(names(ch), "c1")))
  expect_setequal(spread, paste0("c", 2:13))              # partition exact
  expect_equal(anyDuplicated(spread), 0L)
  expect_equal(attr(chunks[[2]], "col_range"), c(11L, 13L))
  expect_length(split_wide_df(data.frame(a = 1, b = 2)), 1L)  # narrow: as-is
})

test_that("docx tables chunk wide frames with the label column repeated", {
  skip_if_not_installed("officer")
  wide <- as.data.frame(setNames(as.list(seq_len(13)), paste0("c", 1:13)))
  doc <- officer::read_docx()
  doc <- .docx_add_table(doc, wide, caption = "wide test")
  f <- withr::local_tempfile(fileext = ".docx")
  print(doc, target = f)
  smry <- officer::docx_summary(officer::read_docx(f))
  cells <- smry[smry$content_type == "table cell", ]
  expect_equal(sum(cells$text == "c1"), 2L)     # label header in BOTH chunks
  expect_true(any(grepl("columns 2-10 of 13", smry$text)))
  expect_true(any(grepl("columns 11-13 of 13", smry$text)))
})

# ---- Export & Report merged tab (0.8.0, Option A) -------------------------

test_that("exportReportUI composes both sub-tabs; export-only stays plain", {
  both <- as.character(htmltools::renderTags(exportReportUI("ex", "rep"))$html)
  expect_match(both, "Data &amp; downloads", fixed = TRUE)
  expect_match(both, "Full report", fixed = TRUE)
  solo <- as.character(htmltools::renderTags(exportReportUI("ex"))$html)
  expect_no_match(solo, "Full report", fixed = TRUE)
})

test_that("every app object still builds with the merged tab", {
  for (app in list(data_explorer_app(), compare_groups_app(),
                   regression_tool_app(), reshape_tool_app()))
    expect_s3_class(app, "shiny.appobj")
})

test_that("docx chunking keeps ALL leading identifier columns", {
  skip_if_not_installed("officer")
  gs <- grouped_summary(mtcars, c("mpg", "hp"), "cyl")   # 11 cols, keys cyl+Variable
  doc <- officer::read_docx()
  doc <- .docx_add_table(doc, gs, caption = "keyed")
  f <- withr::local_tempfile(fileext = ".docx")
  print(doc, target = f)
  cells <- officer::docx_summary(officer::read_docx(f))
  cells <- cells[cells$content_type == "table cell", "text"]
  expect_equal(sum(cells == "Variable"), 2L)   # key header in BOTH chunks
  expect_equal(sum(cells == "cyl"), 2L)
})

# ---- selectable report sections (0.9.0) -----------------------------------

test_that("reportServer includes every wired section by default", {
  shiny::testServer(reportServer, args = list(
    data_in     = shiny::reactive(mtcars),
    summary_tbl = shiny::reactive(grouped_summary(mtcars, "mpg", "cyl")),
    model       = shiny::reactive(fit_model(mtcars, "mpg", "wt", "linear"))), {
    expect_setequal(chosen(), c("summary", "regression"))
    a <- avail()
    expect_true(a$overview && a$summary && a$regression)
    expect_false(a$charts)          # not wired -> never available
  })
})

test_that("unticking a section drops it from the built spec", {
  shiny::testServer(reportServer, args = list(
    data_in     = shiny::reactive(mtcars),
    summary_tbl = shiny::reactive(grouped_summary(mtcars, "mpg", "cyl")),
    model       = shiny::reactive(fit_model(mtcars, "mpg", "wt", "linear"))), {
    # keep only the summary
    session$setInputs(inc_summary = TRUE, inc_regression = FALSE)
    expect_equal(chosen(), "summary")
    sel <- chosen()
    spec <- report_spec(
      data        = mtcars,
      summary_tbl = if ("summary" %in% sel) grouped_summary(mtcars, "mpg", "cyl"),
      model       = if ("regression" %in% sel) fit_model(mtcars, "mpg", "wt", "linear"))
    expect_true(spec$sections[["summary"]])
    expect_false(spec$sections[["regression"]])
    html <- build_report_html(spec)
    expect_match(html, "id=\"summary\"", fixed = TRUE)
    expect_no_match(html, "id=\"regression\"", fixed = TRUE)
    # unticking everything leaves the always-on overview
    session$setInputs(inc_summary = FALSE, inc_regression = FALSE)
    expect_equal(length(chosen()), 0L)
  })
})

test_that("a report with no optional stages still builds the overview", {
  spec <- report_spec(data = mtcars)
  expect_true(spec$sections[["overview"]])
  expect_false(any(spec$sections[-1]))
  html <- build_report_html(spec)
  expect_match(html, "id=\"overview\"", fixed = TRUE)
})
