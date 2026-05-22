# Don't perform any further tests on CRAN due to time of running the complete
# test.
testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE

# Generic test
# Note that one-sample kaplan-meier curves can be created.
familiar:::test_plots(
  plot_function = familiar:::plot_kaplan_meier,
  not_available_all_prospective = TRUE,
  outcome_type_available = c("survival"),
  data_element = "risk_stratification_data",
  debug = debug_flag
)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_kaplan_meier,
  data_element = "risk_stratification_data",
  outcome_type_available = c("survival"),
  debug = debug_flag
)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_kaplan_meier,
  data_element = "risk_stratification_data",
  outcome_type_available = c("survival"),
  use_prediction_table = TRUE,
  prediction_type = list("survival" = "risk_stratification"),
  debug = debug_flag
)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_kaplan_meier,
  data_element = "risk_stratification_data",
  outcome_type_available = c("survival"),
  plot_args = list(
    "facet_by" = c("learner", "vimp_method", "data_set"),
    "color_by" = "group"),
  debug = debug_flag
)

# Fixed stratification with 3 groups works correctly.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_kaplan_meier,
  data_element = "risk_stratification_data",
  outcome_type_available = c("survival"),
  experiment_args = list(
    stratification_method = "fixed"),
  plot_args = list(
    "facet_by" = c("learner", "vimp_method", "data_set"),
    "color_by" = "group"),
  debug = debug_flag
)

# Fixed stratification with 5 groups works correctly.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_kaplan_meier,
  data_element = "risk_stratification_data",
  outcome_type_available = c("survival"),
  experiment_args = list(
    stratification_method = "fixed",
    stratification_threshold = c(0.20, 0.40, 0.60, 0.80)),
  plot_args = list(
    "facet_by" = c("learner", "vimp_method", "data_set"),
    "color_by" = "group"),
  debug = debug_flag
)

# Fixed stratification with 5 groups works correctly.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_kaplan_meier,
  data_element = "risk_stratification_data",
  outcome_type_available = c("survival"),
  experiment_args = list(
    stratification_method = c("median", "fixed"),
    stratification_threshold = c(0.20, 0.40, 0.60, 0.80)),
  plot_args = list(
    "facet_by" = c("learner", "vimp_method", "data_set"),
    "color_by" = "group",
    "split_by" = "stratification_method"),
  debug = debug_flag
)



# Test plotting from dataObject.
data <- familiar:::test_create_good_data(outcome_type = "survival")
p <- familiar::plot_kaplan_meier(object = data)
testthat::test_that("Plotting kaplan-meier curves using dataObject works (survival).", {
  testthat::expect_true(is(p[[1L]], "gtable"))
})


# Test plotting from dataObject with two groups.
data <- familiar:::test_create_good_data(outcome_type = "survival", two_groups = TRUE)
p <- familiar::plot_kaplan_meier(object = data)
testthat::test_that("Plotting kaplan-meier curves for two groups works.", {
  testthat::expect_length(p, 1L)
  testthat::expect_true(is(p[[1L]], "gtable"))
})


# Test plotting from data.table.
data <- familiar:::test_create_good_data(outcome_type = "survival", to_data_object = FALSE)
p <- familiar::plot_kaplan_meier(
  object = data,
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  outcome_type = "survival",
  outcome_column = c("outcome_time", "outcome_event")
)
testthat::test_that("Plotting kaplan-meier curves using data.table works (survival).", {
  testthat::expect_true(is(p[[1L]], "gtable"))
})


# Test plotting from data.table without feature data.
data <- familiar:::test_create_data_without_feature(outcome_type = "survival", to_data_object = FALSE)
p <- familiar::plot_kaplan_meier(
  object = data,
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  outcome_type = "survival",
  outcome_column = c("outcome_time", "outcome_event")
)
testthat::test_that("Plotting kaplan-meier curves using data.table without any feature works (survival).", {
  testthat::expect_true(is(p[[1L]], "gtable"))
})


# Test plotting from dataObject with a risk_group_column.
data <- familiar:::test_create_good_data(outcome_type = "survival")
data@data[, "risk_group" := "risk-group A"]
data@data[51L:100L, "risk_group" := "risk-group B"]
p <- familiar::plot_kaplan_meier(
  object = data,
  risk_group_column = "risk_group"
)
testthat::test_that("Plotting kaplan-meier curves using data.table without a risk_group_column works (survival).", {
  testthat::expect_true(is(p[[1L]], "gtable"))
})


# Test plotting from data.table, but with batch_id as risk group.
data <- familiar:::test_create_good_data(outcome_type = "survival", to_data_object = FALSE)
data[, "batch_id" := "batch A"]
data[51L:100L, "batch_id" := "batch B"]
p <- familiar::plot_kaplan_meier(
  object = data,
  risk_group_column = "batch_id",
  outcome_type = "survival",
  outcome_column = c("outcome_time", "outcome_event")
)
testthat::test_that("Plotting kaplan-meier curves using data.table works (survival).", {
  testthat::expect_true(is(p[[1L]], "gtable"))
})
