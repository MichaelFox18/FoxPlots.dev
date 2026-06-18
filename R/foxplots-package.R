#' @keywords internal
"_PACKAGE"

#' Package-wide imports
#'
#' The Shiny modules and plot builders call `shiny`, `bslib`, and `ggplot2`
#' functions unqualified -- exactly as the apps did when they attached those
#' packages with `library()` -- so the whole of each is imported here. Every
#' other dependency is called explicitly with `pkg::fun()`.
#'
#' @import shiny
#' @import bslib
#' @import ggplot2
#' @name foxplots-imports
#' @keywords internal
NULL
