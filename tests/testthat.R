library(testthat)
library(familiar)

on_cran <- function() {
  !interactive() && identical(Sys.getenv("NOT_CRAN"), "")
}

# Prevent thread overuse (through data.table?) when running tests on CRAN.
if (on_cran()) {
  Sys.setenv("OMP_THREAD_LIMIT" = 2L)
}

suppressWarnings(
  testthat::test_check("familiar"),
  classes = c("deprecation_warning")
)
