# power.transform and other packages are required for testing settings.
if (!rlang::is_installed("power.transform")) testthat::skip()

# sample_limit -----------------------------------------------------------------

testthat::test_that(
  "sample_limit is correctly parsed",
  {
    # Unset by default
    settings <- familiar:::.create_test_settings()
    testthat::expect_null(settings$eval$sample_limit)
    
    settings <- familiar:::.create_test_settings(sample_limit = 30L)
    for (x in familiar:::.get_available_data_elements(check_has_sample_limit = TRUE)) {
      testthat::expect_equal(settings$eval$sample_limit[[x]], 30L)
    }
    
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
    
    settings <- familiar:::.create_test_settings(n_important_features = 30L)
    for (x in familiar:::.get_available_data_elements(check_has_n_important_features = TRUE)) {
      testthat::expect_equal(settings$eval$n_important_features[[x]], 30L)
    }
    
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
# estimation_type --------------------------------------------------------------
# aggregate_results ------------------------------------------------------------
