# Tests for the shared UI atoms in components.R. label_or() carries real
# branching logic; the rest are smoke checks that the tag builders return valid
# shiny/htmltools tags without erroring.

test_that("brand constants are valid hex colours", {
  expect_match(UF_BLUE, "^#[0-9A-Fa-f]{6}$")
  expect_match(UF_ORANGE, "^#[0-9A-Fa-f]{6}$")
  expect_length(UF_COLORS, 4L)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", UF_COLORS)))
})

test_that("label_or returns the custom label only when non-blank", {
  expect_equal(label_or("My axis", "default"), "My axis")
  expect_equal(label_or("", "default"), "default")      # empty
  expect_equal(label_or("   ", "default"), "default")   # whitespace only
  expect_equal(label_or(NULL, "default"), "default")    # NULL
  expect_equal(label_or("  keep me  ", "default"), "  keep me  ")  # kept verbatim
})

test_that("uf_theme builds a bslib theme", {
  th <- uf_theme()
  expect_s3_class(th, "bs_theme")
})

test_that("uf_title builds a tag with and without a logo", {
  expect_s3_class(uf_title("Data Explorer", logo = NULL), "shiny.tag")
  # a fake data URI should be embedded as an <img>
  html <- as.character(uf_title("Tool", logo = "data:image/png;base64,AAAA"))
  expect_true(grepl("<img", html))
  expect_true(grepl("Tool", html))
})

test_that("info_tip builds a tooltip tag", {
  tip <- info_tip("Helpful text")
  expect_true(inherits(tip, "shiny.tag") || inherits(tip, "shiny.tag.list"))
})

test_that("uf_logo_uri returns NULL or a data URI, never errors", {
  uri <- uf_logo_uri()
  expect_true(is.null(uri) || grepl("^data:image/png;base64,", uri))
})
