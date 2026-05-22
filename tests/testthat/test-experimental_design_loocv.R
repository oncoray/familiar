testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE

# Leave-one-out-cross-validation
familiar:::integrated_test(
  experimental_design = "lv(fs+mb)",
  vimp_method = "none",
  parallel = FALSE,
  skip_evaluation_elements = "all",
  outcome_type_available = "binomial",
  debug = debug_flag
)
