# Don't perform any further tests on CRAN due to time of running the complete
# test.
testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE

# Without setting the number of important features.
results <- familiar:::test_export_specific(
  export_function = familiar:::export_permutation_vimp,
  data_element = "permutation_vimp",
  outcome_type_available = "continuous",
  create_novelty_detector = FALSE,
  debug = debug_flag
)

testthat::test_that(
  "all feature are assessed",
  {
    data <- results$continuous[[1L]]@data
    features <- unique(data$feature)
    testthat::expect_length(features, 6L)
  }
)


# With setting the number of important features to 2.
results <- familiar:::test_export_specific(
  export_function = familiar:::export_permutation_vimp,
  data_element = "permutation_vimp",
  outcome_type_available = "continuous",
  n_important_features = 2L,
  create_novelty_detector = FALSE,
  debug = debug_flag
)

testthat::test_that(
  "the 2 most important features are assessed",
  {
    data <- results$continuous[[1L]]@data
    features <- unique(data$feature)
    testthat::expect_length(features, 2L)
    testthat::expect_setequal(features, c("feature_1", "feature_2b"))
  }
)


# With setting the number of important features to 2.
results <- familiar:::test_export_specific(
  export_function = familiar:::export_permutation_vimp,
  data_element = "permutation_vimp",
  outcome_type_available = "continuous",
  n_important_features = 2L,
  n_models = 3L,
  create_novelty_detector = FALSE,
  debug = debug_flag
)


# With setting the number of important features to 1.
results <- familiar:::test_export_specific(
  export_function = familiar:::export_permutation_vimp,
  data_element = "permutation_vimp",
  outcome_type_available = "continuous",
  n_important_features = 1L,
  create_novelty_detector = FALSE,
  debug = debug_flag
)

testthat::test_that(
  "the most important feature is assessed",
  {
    data <- results$continuous[[1L]]@data
    features <- unique(data$feature)
    testthat::expect_length(features, 1L)
    testthat::expect_setequal(features, "feature_1")
  }
)
