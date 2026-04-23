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

# The default limit for the number of important features exceeds the number of
# features in the data, which means that no actual feature selection has 
# to take place. To test all path ways, we set the number of important features
# to one here. Because the test routine relies on "none" as variable importance
# method, this triggers ad-hoc feature selection when computing the dataset.
# This will fail if outcome data are absent, mostly absent, or singular.
familiar:::test_plots(
  plot_function = familiar::plot_shap_force,
  not_available_all_prospective = TRUE,
  not_available_mostly_prospective = TRUE,
  not_available_single_sample = TRUE,
  data_element = "shap",
  shap_max_iterations = 10L,
  n_important_features = 1L,
  debug = debug_flag
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

# Test with highlight feature.
familiar:::test_plots(
  plot_function = familiar::plot_shap_force,
  data_element = "shap",
  shap_max_iterations = 10L,
  evaluation_time = c(1.0, 2.0, 3.5),
  plot_args = list("highlight_feature" = c("feature_1", "feature_4")),
  test_config = "normal",
  debug = debug_flag
)

# Test with original sample order.
familiar:::test_plots(
  plot_function = familiar::plot_shap_force,
  data_element = "shap",
  shap_max_iterations = 10L,
  evaluation_time = c(1.0, 2.0, 3.5),
  plot_args = list("sample_order" = "original"),
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
