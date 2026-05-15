# Don't perform any further tests on CRAN due to time of running the complete
# test.
testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE

# Generic test
familiar:::test_plots(
  plot_function = familiar:::plot_feature_similarity,
  not_available_single_feature = TRUE,
  not_available_single_sample = TRUE,
  not_available_all_predictions_fail = FALSE,
  not_available_some_predictions_fail = FALSE,
  outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
  data_element = "feature_similarity",
  debug = debug_flag
)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_feature_similarity,
  data_element = "feature_similarity",
  outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
  debug = debug_flag
)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_feature_similarity,
  data_element = "feature_similarity",
  outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
  plot_args = list("facet_by" = c("learner", "vimp_method", "data_set")),
  debug = debug_flag
)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_feature_similarity,
  data_element = "feature_similarity",
  outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
  plot_args = list("show_dendrogram" = c("left", "bottom")),
  debug = debug_flag
)


# Test normal tests.
familiar:::test_plots(
  plot_function = familiar:::plot_feature_similarity,
  feature_similarity_metric = "spearman",
  outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
  data_element = "feature_similarity",
  debug = debug_flag,
  test_config = "normal"
)


familiar:::test_plots(
  plot_function = familiar:::plot_feature_similarity,
  feature_similarity_metric = "mutual_information",
  outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
  data_element = "feature_similarity",
  debug = debug_flag,
  test_config = "normal"
)


# Plots with selection of features.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_feature_similarity,
  data_element = "feature_similarity",
  outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
  plot_args = list(
    "features" = c("feature_1", "feature_2a", "feature_2b")
    ),
  debug = debug_flag
)

# Plots without tick labels on x and y-axes.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_feature_similarity,
  data_element = "feature_similarity",
  outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
  plot_args = list(
    "remove_feature_labels" = TRUE
  ),
  debug = debug_flag
)



# Test plotting from dataObject.
data <- familiar:::test_create_good_data(outcome_type = "continuous")
p <- familiar::plot_feature_similarity(object = data, feature_similarity_metric = "spearman")
testthat::test_that("Plotting feature similarity using dataObject works.", {
  testthat::expect_true(is(p[[1L]], "gtable"))
})

# Test plotting from dataObject with two groups.
data <- familiar:::test_create_good_data(outcome_type = "continuous", two_groups = TRUE)
p <- familiar::plot_feature_similarity(object = data, feature_similarity_metric = "spearman")
testthat::test_that("Plotting feature similarity for two groups works.", {
  testthat::expect_length(p, 2L)
  testthat::expect_true(is(p[[1L]], "gtable"))
  testthat::expect_true(is(p[[2L]], "gtable"))
})


# Test plotting from data.table.
data <- familiar:::test_create_good_data(outcome_type = "continuous", to_data_object = FALSE)
p <- familiar::plot_feature_similarity(
  object = data,
  feature_similarity_metric = "spearman",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  outcome_type = "continuous",
  outcome_column = "outcome"
)
testthat::test_that("Plotting feature similarity using data.table works.", {
  testthat::expect_true(is(p[[1L]], "gtable"))
})


# Test plotting from data.table without outcome data.
data <- familiar:::test_create_good_data(outcome_type = "continuous", to_data_object = FALSE)
data[, ":="("batch_id" = NULL, "sample_id" = NULL, "series_id" = NULL, "outcome" = NULL)]
p <- familiar::plot_feature_similarity(
  object = data,
  feature_similarity_metric = "spearman"
)
testthat::test_that("Plotting feature similarity using data.table works.", {
  testthat::expect_true(is(p[[1L]], "gtable"))
})



# Test plotting from dataObject with set features.
data <- familiar:::test_create_good_data(outcome_type = "continuous")
p <- familiar::plot_feature_similarity(
  object = data, 
  features = c("feature_1", "feature_2a", "feature_2b"),
  feature_similarity_metric = "spearman"
)
testthat::test_that("Plotting feature similarity using dataObject and a limited number of features works.", {
  testthat::expect_true(is(p[[1L]], "gtable"))
})
