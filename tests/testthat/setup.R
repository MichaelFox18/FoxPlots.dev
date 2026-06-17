# foxplots is now a package, so its functions (exported and internal) are
# available to the tests through the package namespace — no need to source the
# R/ files here. testthat::test_check()/test_local() loads the package for us.
