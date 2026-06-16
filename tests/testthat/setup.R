# Sourced automatically by testthat before the test files run.
# This kit is loose files (not yet a package), so we source the pure
# helpers under test. here::here() resolves to projects/ via the .here
# anchor regardless of the working directory tests are launched from.
library(here)
for (f in list.files(here::here("R"), pattern = "[.][Rr]$", full.names = TRUE)) {
  source(f)
}
