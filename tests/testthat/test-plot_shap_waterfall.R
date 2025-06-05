# Don't perform any further tests on CRAN due to time of running the complete
# test.
testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE

# Test with single instance
familiar:::test_plots(
  plot_function = familiar::plot_shap_waterfall,
  data_element = "shap",
  evaluation_time = c(1.0, 2.0, 3.5),
  test_config = "single instance",
  debug = debug_flag
)
