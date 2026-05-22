testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE

# Leave-one-out-cross-validation
familiar:::integrated_test(
  experimental_design = "lv(fs+mb)",
  vimp_method = "none",
  parallel = FALSE,
  estimation_type = "point",
  outcome_type_available = "binomial",
  debug = debug_flag
)
