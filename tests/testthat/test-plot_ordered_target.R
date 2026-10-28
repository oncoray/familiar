# Don't perform any further tests on CRAN due to time of running the complete
# test.
testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE

# Test plotting from dataObject with selection of features.
data <- familiar:::test_create_good_data(outcome_type = "binomial")
p <- familiar::plot_ordered_target(
  object = data, 
  features = c("feature_1", "feature_2a", "feature_2b")
)
