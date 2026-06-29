# Don't perform any further tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE

familiar:::test_export(
  export_function = familiar:::export_prediction_data,
  not_available_all_predictions_fail = FALSE,
  not_available_some_predictions_fail = FALSE,
  data_element = "prediction_data",
  outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
  detail_level = "ensemble",
  create_novelty_detector = TRUE,
  debug = debug_flag
)

familiar:::test_export(
  export_function = familiar:::export_prediction_data,
  not_available_all_predictions_fail = FALSE,
  not_available_some_predictions_fail = FALSE,
  data_element = "prediction_data",
  outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
  detail_level = "hybrid",
  estimation_type = "bci",
  confidence_level = 0.80,
  aggregate_results = TRUE,
  create_novelty_detector = TRUE,
  n_models = 100,
  test_config = "normal",
  debug = debug_flag
)

familiar:::test_export(
  export_function = familiar:::export_prediction_data,
  not_available_all_predictions_fail = FALSE,
  not_available_some_predictions_fail = FALSE,
  data_element = "prediction_data",
  outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
  detail_level = "hybrid",
  estimation_type = "bias_correction",
  confidence_level = 0.80,
  aggregate_results = TRUE,
  create_novelty_detector = TRUE,
  n_models = 20,
  test_config = "normal",
  debug = debug_flag
)


# Test that prediction data are correctly exported when multiple feature
# selection methods and learners are present.
results <- familiar::summon_familiar(
  data = familiar:::test_create_good_data(outcome_type = "binomial"),
  experimental_design = "mb",
  vimp_method = c("mim", "univariate_regression"),
  learner = c("glm_logistic", "lasso_binomial"),
  evaluation_elements = c("prediction_data"),
  verbose = debug_flag
)

predictions <- familiar::export_prediction_data(results$familiarCollection)
testthat::test_that(
  "All predictions are made.", {
    # Two vimp methods with two learners with 150 samples = 600 predictions.
    testthat::expect_equal(nrow(predictions$classification[[1L]]@data), 600L)
    testthat::expect_setequal(
      unique(predictions$classification[[1L]]@data$vimp_method),
      c("mim", "univariate_regression")
    )
    testthat::expect_setequal(
      unique(predictions$classification[[1L]]@data$learner),
      c("glm_logistic", "lasso_binomial")
    )
  }
)
