# Don't perform tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()
testthat::skip_on_ci()

# debug flag
debug <- FALSE

# Create datasets
full_data <- familiar:::test_create_good_data("binomial")
full_data@data <- full_data@data[1L:20L]

# Set learner
learner <- "lasso"
  
# Parse hyperparameter list
hyperparameters <- list(
  "lasso" = list(
    "sign_size" = familiar:::get_n_features(full_data),
    "family" = "binomial"
  )
)
            
output <- familiar:::summon_familiar(
  data = full_data,
  experimental_design = "lv(fs+mb)",
  vimp_method = "none",
  cluster_method = "none",
  imputation_method = "simple",
  learner = learner,
  hyperparameter = hyperparameters,
  skip_evaluation_elements = c(
    "auc_data", "calibration_data", "calibration_info",
    "decision_curve_analyis", "feature_expressions", "feature_similarity",
    "fs_vimp", "hyperparameters", "ice_data", "shap",
    "model_vimp", "permutation_vimp", "sample_similarity",
    "risk_stratification_data", "risk_stratification_info", "univariate_analysis"
  ),
  parallel = FALSE,
  verbose = debug
)

testthat::test_that("LOOCV is performed correctly.", {
  
  # Expect that model performance is computed for all data.
  testthat::expect_length(output$familiarCollection@model_performance, 4L)
  testthat::expect_length(output$familiarData, 2L)
  testthat::expect_length(output$familiarData[[1L]]@model_performance, 2L)
  testthat::expect_length(output$familiarData[[2L]]@model_performance, 2L)
  
})
