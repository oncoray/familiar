# Don't perform any further tests on CRAN due to running time.
testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- TRUE

# Default
results <- familiar:::test_export_specific(
  export_function = familiar:::export_shap,
  outcome_type_available = "multinomial",
  data_element = "shap",
  create_novelty_detector = FALSE,
  debug = debug_flag
)
