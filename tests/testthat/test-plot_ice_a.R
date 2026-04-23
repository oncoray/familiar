# Don't perform any further tests on CRAN due to time of running the complete
# test.
testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE

familiar:::test_plots(
  plot_function = familiar:::plot_ice,
  not_available_some_predictions_fail = FALSE,
  data_element = "ice_data",
  debug = debug_flag
)

# The default limit for the number of important features exceeds the number of
# features in the data, which means that no actual feature selection has 
# to take place. To test all path ways, we set the number of important features
# to one here. Because the test routine relies on "none" as variable importance
# method, this triggers ad-hoc feature selection when computing the dataset.
# This will fail if outcome data are absent, mostly absent, or singular.
familiar:::test_plots(
  plot_function = familiar:::plot_ice,
  not_available_some_predictions_fail = FALSE,
  not_available_all_prospective = TRUE,
  not_available_mostly_prospective = TRUE,
  not_available_single_sample = TRUE,
  data_element = "ice_data",
  n_important_features = 1L,
  debug = debug_flag
)
