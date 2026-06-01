library(testthat)
library(familiar)

# Prevent thread overuse (through data.table?) when running tests on CRAN.
Sys.setenv("OMP_THREAD_LIMIT" = 2L)

suppressWarnings(
  testthat::test_check("familiar"),
  classes = c("deprecation_warning")
)
