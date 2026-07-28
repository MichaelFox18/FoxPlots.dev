# Launcher smoke tests.
#
# Until 0.10.0 nothing under R/run_*.R had any automated coverage: a typo in a
# module id, a renamed argument, or a nav_panel pointing at a UI function that
# no longer exists would only surface when someone opened the app. Three
# passes here -- build the object, render the real page, and run the whole
# server -- exercise every UI call and every wiring line at least once.

APP_BUILDERS <- c("data_explorer_app", "reshape_tool_app", "combine_tool_app",
                  "compare_groups_app", "lmer_tool_app", "glmm_review_app",
                  "map_tool_app", "regression_tool_app")

# The page as Shiny actually serves it (the app object exposes an http handler,
# not a $ui field).
app_page <- function(app) {
  req <- list(REQUEST_METHOD = "GET", PATH_INFO = "/", QUERY_STRING = "",
              HTTP_HOST = "127.0.0.1",
              rook.input = list(read_lines = function(...) character(0)))
  res <- app$httpHandler(req)
  list(status = res$status, html = paste(res$content, collapse = ""))
}

test_that("every launcher builds a shiny app object", {
  for (nm in APP_BUILDERS) {
    app <- do.call(nm, list())
    expect_s3_class(app, "shiny.appobj")
    expect_true(is.function(app$serverFuncSource()), info = nm)
  }
})

test_that("every launcher serves a full page carrying the UF theme", {
  for (nm in APP_BUILDERS) {
    pg <- app_page(do.call(nm, list()))
    expect_equal(pg$status, 200L, info = nm)
    expect_gt(nchar(pg$html), 10000)
    expect_true(grepl("navbar", pg$html, fixed = TRUE), info = nm)
    # the logo is inlined as a data URI by uf_title()
    expect_true(grepl("data:image/png;base64", pg$html, fixed = TRUE), info = nm)
  }
})

test_that("every app exposes About, Import and one Export & Report tab", {
  # 0.9.0 promised a report in all eight apps; this is the tripwire.
  for (nm in APP_BUILDERS) {
    html <- app_page(do.call(nm, list()))$html
    expect_true(grepl("Export &amp; Report", html, fixed = TRUE), info = nm)
    expect_true(grepl('data-value="about"', html, fixed = TRUE), info = nm)
    # the Combine tool imports two tables, so its panels are named differently
    expect_true(grepl("Import", html, fixed = TRUE) ||
                grepl("table", html, fixed = TRUE), info = nm)
    # Exactly ONE Export & Report NAV LINK, never the pre-0.9.0 split pair.
    # Count anchors only: data-value also lands on the tab pane, and the About
    # tab mentions the section by name in its feature list.
    n_tabs <- lengths(regmatches(
      html, gregexpr('<a[^>]*data-value="export"', html)))
    expect_equal(n_tabs, 1L, info = paste(nm, "should have one export tab"))
  }
})

test_that("every app's full server wiring runs end to end", {
  # This is the real prize: testServer on the app object instantiates EVERY
  # module server with the launcher's own arguments, so a wrong id or a
  # renamed parameter fails here instead of in front of a user.
  skip_if_not_installed("lmerTest")
  skip_if_not_installed("glmmTMB")
  for (nm in APP_BUILDERS) {
    expect_no_error(shiny::testServer(do.call(nm, list()), {
      session$flushReact()
    }))
  }
})

test_that("every module server id has a matching UI id in the same app", {
  # Caught by mutation testing: changing mapServer("map") to mapServer("mapp")
  # broke nothing detectably -- the server happily runs in a namespace whose
  # UI does not exist, so its outputs never bind and the tab is simply blank.
  # That is the classic Shiny wiring typo, and no runtime test sees it.
  ids_in <- function(txt, suffix) {
    calls <- regmatches(txt, gregexpr(
      sprintf("\\b[A-Za-z_.]+%s\\(([^)]*)", suffix), txt))[[1]]
    unique(unlist(lapply(calls, function(cl)
      regmatches(cl, gregexpr('"[^"]+"', cl))[[1]])))
  }
  for (nm in APP_BUILDERS) {
    src <- paste(deparse(get(nm, envir = asNamespace("foxplots"))),
                 collapse = "\n")
    ui_ids  <- ids_in(src, "UI")
    srv_ids <- ids_in(src, "Server")
    # Server calls take the id first; UI calls may take two (exportReportUI).
    orphans <- setdiff(srv_ids, ui_ids)
    expect_equal(orphans, character(0),
                 info = paste(nm, "server id(s) with no matching UI:",
                              paste(orphans, collapse = ", ")))
  }
})

test_that("run_* launchers exist for every app and are exported", {
  ns <- asNamespace("foxplots")
  exported <- getNamespaceExports(ns)
  for (nm in sub("_app$", "", APP_BUILDERS)) {
    runner <- paste0("run_", nm)
    expect_true(runner %in% exported, info = runner)
    expect_true(is.function(get(runner, envir = ns)), info = runner)
  }
})
