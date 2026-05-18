# Don't perform any further tests on CRAN due to time of running the complete
# test.
testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE

# Generic test
familiar:::test_plots(
  plot_function = familiar:::plot_sample_clustering,
  not_available_all_predictions_fail = FALSE,
  not_available_some_predictions_fail = FALSE,
  outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
  data_element = "feature_expressions",
  plot_args = list(
    "verbose" = FALSE,
    "show_normalised_data" = "set_normalisation"),
  debug = debug_flag
)

# No extra elements
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_sample_clustering,
  data_element = "feature_expressions",
  outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
  plot_args = list(
    "facet_by" = c("learner", "vimp_method", "data_set"),
    "x_axis_by" = "sample",
    "y_axis_by" = "feature",
    "show_outcome" = FALSE,
    "show_feature_dendrogram" = FALSE,
    "show_sample_dendrogram" = FALSE,
    "verbose" = FALSE),
  debug = debug_flag
)

# No normalisation.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_sample_clustering,
  data_element = "feature_expressions",
  outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
  plot_args = list(
    "facet_by" = c("learner", "vimp_method", "data_set"),
    "show_normalised_data" = "none",
    "verbose" = FALSE),
  debug = debug_flag
)

# Normalisation per dataset.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_sample_clustering,
  data_element = "feature_expressions",
  outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
  plot_args = list(
    "facet_by" = c("learner", "vimp_method", "data_set"),
    "show_normalised_data" = "set_normalisation",
    "verbose" = FALSE),
  debug = debug_flag
)


# With sample limit
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_sample_clustering,
  data_element = "feature_expressions",
  outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
  plot_args = list(
    "facet_by" = c("learner", "vimp_method", "data_set"),
    "sample_limit" = 20L,
    "verbose" = FALSE),
  debug = debug_flag
)


# Test multiple evaluation times
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_sample_clustering,
  data_element = "feature_expressions",
  outcome_type_available = c("survival"),
  plot_args = list(
    "evaluation_times" = c(500, 1000, 1500, 2000),
    "verbose" = FALSE),
  debug = debug_flag
)


# Plots without feature tick labels.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_sample_clustering,
  data_element = "feature_expressions",
  outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
  plot_args = list(
    "remove_feature_labels" = TRUE
  ),
  debug = debug_flag
)


# Plots without sample tick labels.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_sample_clustering,
  data_element = "feature_expressions",
  outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
  plot_args = list(
    "remove_sample_labels" = TRUE
  ),
  debug = debug_flag
)


# Plots with feature subset.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_sample_clustering,
  data_element = "feature_expressions",
  outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
  plot_args = list(
    "features" = c("feature_1", "feature_2a", "feature_2b")
  ),
  debug = debug_flag
)



# Test plotting from dataObject.
data <- familiar:::test_create_good_data(outcome_type = "survival")
p <- familiar::plot_sample_clustering(object = data, feature_similarity_metric = "spearman")
testthat::test_that("Plotting sample similarity using dataObject works (survival).", {
  testthat::expect_true(is(p[[1L]], "gtable"))
})

data <- familiar:::test_create_good_data(outcome_type = "continuous")
p <- familiar::plot_sample_clustering(object = data, feature_similarity_metric = "spearman")
testthat::test_that("Plotting sample similarity using dataObject works (continuous).", {
  testthat::expect_true(is(p[[1L]], "gtable"))
})

# Test plotting from dataObject with selection of features.
data <- familiar:::test_create_good_data(outcome_type = "continuous")
p <- familiar::plot_sample_clustering(
  object = data, 
  feature_similarity_metric = "spearman",
  features = c("feature_1", "feature_2a", "feature_2b")
)
testthat::test_that("Plotting sample similarity using dataObject works (continuous).", {
  testthat::expect_true(is(p[[1L]], "gtable"))
})


# Test plotting from dataObject with two groups.
data <- familiar:::test_create_good_data(outcome_type = "continuous", two_groups = TRUE)
p <- familiar::plot_sample_clustering(object = data, feature_similarity_metric = "spearman")
testthat::test_that("Plotting sample similarity for two groups works.", {
  testthat::expect_length(p, 2L)
  testthat::expect_true(is(p[[1L]], "gtable"))
  testthat::expect_true(is(p[[2L]], "gtable"))
})


# Test plotting from data.table.
data <- familiar:::test_create_good_data(outcome_type = "survival", to_data_object = FALSE)
p <- familiar::plot_sample_clustering(
  object = data,
  feature_similarity_metric = "spearman",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  outcome_type = "survival",
  outcome_column = c("outcome_time", "outcome_event")
)
testthat::test_that("Plotting sample similarity using data.table works (survival).", {
  testthat::expect_true(is(p[[1L]], "gtable"))
})

data <- familiar:::test_create_good_data(outcome_type = "continuous", to_data_object = FALSE)
p <- familiar::plot_sample_clustering(
  object = data,
  feature_similarity_metric = "spearman",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  outcome_type = "continuous",
  outcome_column = "outcome"
)
testthat::test_that("Plotting sample similarity using data.table works (continuous).", {
  testthat::expect_true(is(p[[1L]], "gtable"))
})


# Test plotting from data.table without outcome data.
data <- familiar:::test_create_good_data(outcome_type = "continuous", to_data_object = FALSE)
data[, ":="("batch_id" = NULL, "sample_id" = NULL, "series_id" = NULL, "outcome" = NULL)]
p <- familiar::plot_sample_clustering(
  object = data,
  feature_similarity_metric = "spearman"
)
testthat::test_that("Plotting sample similarity using data.table works.", {
  testthat::expect_true(is(p[[1]], "gtable"))
})
