# power.transform and other packages are required for testing settings.
if (!rlang::is_installed("power.transform")) testthat::skip()

# sample_limit -----------------------------------------------------------------

testthat::test_that(
  "sample_limit is correctly parsed",
  {
    # Unset by default
    settings <- familiar:::.create_test_settings()
    testthat::expect_null(settings$eval$sample_limit)
    
    # Single value for all relevant data elements.
    settings <- familiar:::.create_test_settings(sample_limit = 30L)
    for (x in familiar:::.get_available_data_elements(check_has_sample_limit = TRUE)) {
      testthat::expect_equal(settings$eval$sample_limit[[x]], 30L)
    }
    
    # Different values for specifc data elements.
    settings <- familiar:::.create_test_settings(
      sample_limit = list(
        "permutation_vimp" = 30L,
        "ice_data" = 25L
      )
    )
    testthat::expect_equal(settings$eval$sample_limit$permutation_vimp, 30L)
    testthat::expect_equal(settings$eval$sample_limit$ice_data, 25L)
    testthat::expect_null(settings$eval$sample_limit$shap)
  }
)


# n_important_features ---------------------------------------------------------
testthat::test_that(
  "n_important_features is correctly parsed",
  {
    # Unset by default
    settings <- familiar:::.create_test_settings()
    testthat::expect_null(settings$eval$n_important_features)
    
    # Single value for all relevant data elements.
    settings <- familiar:::.create_test_settings(n_important_features = 30L)
    for (x in familiar:::.get_available_data_elements(check_has_n_important_features = TRUE)) {
      testthat::expect_equal(settings$eval$n_important_features[[x]], 30L)
    }
    
    # Different values for specifc data elements.
    settings <- familiar:::.create_test_settings(
      n_important_features = list(
        "permutation_vimp" = 30L,
        "ice_data" = 25L
      )
    )
    testthat::expect_equal(settings$eval$n_important_features$permutation_vimp, 30L)
    testthat::expect_equal(settings$eval$n_important_features$ice_data, 25L)
    testthat::expect_null(settings$eval$n_important_features$shap)
  }
)


# detail_level -----------------------------------------------------------------
testthat::test_that(
  "detail_level is correctly parsed",
  {
    # Unset by default
    settings <- familiar:::.create_test_settings()
    testthat::expect_null(settings$eval$detail_level)
    
    # Single value for all relevant data elements.
    settings <- familiar:::.create_test_settings(detail_level = "ensemble")
    for (x in familiar:::.get_available_data_elements(check_has_detail_level = TRUE)) {
      testthat::expect_equal(settings$eval$detail_level[[x]], "ensemble")
    }
    
    # Different values for specifc data elements.
    settings <- familiar:::.create_test_settings(
      detail_level = list(
        "permutation_vimp" = "hybrid",
        "ice_data" = "model"
      )
    )
    testthat::expect_equal(settings$eval$detail_level$permutation_vimp, "hybrid")
    testthat::expect_equal(settings$eval$detail_level$ice_data, "model")
    testthat::expect_null(settings$eval$detail_level$shap)
  }
)


# estimation_type --------------------------------------------------------------
testthat::test_that(
  "estimation_type is correctly parsed",
  {
    # Unset by default
    settings <- familiar:::.create_test_settings()
    testthat::expect_null(settings$eval$estimation_type)
    
    # Single value for all relevant data elements.
    settings <- familiar:::.create_test_settings(estimation_type = "bias_correction")
    for (x in familiar:::.get_available_data_elements(check_has_estimation_type = TRUE)) {
      testthat::expect_equal(settings$eval$estimation_type[[x]], "bias_correction")
    }
    
    # Different values for specifc data elements.
    settings <- familiar:::.create_test_settings(
      estimation_type = list(
        "permutation_vimp" = "bias_correction",
        "ice_data" = "point"
      )
    )
    testthat::expect_equal(settings$eval$estimation_type$permutation_vimp, "bias_correction")
    testthat::expect_equal(settings$eval$estimation_type$ice_data, "point")
    testthat::expect_null(settings$eval$estimation_type$shap)
  }
)


# aggregate_results ------------------------------------------------------------
testthat::test_that(
  "aggregate_results is correctly parsed",
  {
    # Unset by default
    settings <- familiar:::.create_test_settings()
    testthat::expect_null(settings$eval$aggregate_results)
    
    # Single value for all relevant data elements. Note that all data elements
    # that also have an estimation_type are considered relevant.
    settings <- familiar:::.create_test_settings(aggregate_results = FALSE)
    for (x in familiar:::.get_available_data_elements(check_has_estimation_type = TRUE)) {
      testthat::expect_equal(settings$eval$aggregate_results[[x]], "false")
    }
    
    # Different values for specifc data elements.
    settings <- familiar:::.create_test_settings(
      aggregate_results = list(
        "permutation_vimp" = TRUE,
        "ice_data" = FALSE
      )
    )
    testthat::expect_equal(settings$eval$aggregate_results$permutation_vimp, "true")
    testthat::expect_equal(settings$eval$aggregate_results$ice_data, "false")
    testthat::expect_null(settings$eval$aggregate_results$shap)
  }
)
