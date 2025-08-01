# Don't perform any further tests on CRAN due to time of running the complete
# test.
testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE

familiar:::test_plots(
  plot_function = familiar::plot_shap_force,
  data_element = "shap",
  shap_max_iterations = 10L,
  outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
  debug = debug_flag,
  not_available_no_samples = FALSE
)

# Test only bog-standard data: no edge cases.
familiar:::test_plots(
  plot_function = familiar::plot_shap_force,
  data_element = "shap",
  shap_max_iterations = 10L,
  evaluation_time = c(1.0, 2.0, 3.5),
  test_config = "normal",
  debug = debug_flag
)

# Test with single instance
familiar:::test_plots(
  plot_function = familiar::plot_shap_force,
  data_element = "shap",
  shap_max_iterations = 10L,
  evaluation_time = c(1.0, 2.0, 3.5),
  test_config = "single instance",
  debug = debug_flag
)
