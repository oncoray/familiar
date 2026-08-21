testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE

# Test that creates a single naive model.
familiar:::integrated_test(
  experimental_design = "fs+mb",
  vimp_method = "no_features",
  cluster_method = "none",
  imputation_method = "simple",
  estimation_type = "point",
  parallel = FALSE,
  debug = debug_flag
)


# Test for additional errors in situations where a naive model is created due
# to lack of actual information among the features.
data <- familiar:::test_create_random_data("continuous", seed = 19L)

# Though the dataset should generally lead to a naive model being formed, this
# is not guaranteed. We iterate until it works.
results <- NULL
while (!is(results$familiarModel, "familiarNaiveModel")) {
  results <- familiar::summon_familiar(
    data = data,
    vimp_method = "mim",
    learner = "glm_gaussian",
    hyperparameter = list("glm_gaussian" = list("sign_size" = c(4, 5, 6))),
    outcome_type = "continuous",
    experimental_design = "fs+mb",
    estimation_type = "point",
    verbose = debug_flag
  )
}

testthat::test_that(
  "naive models are correctly specified", 
  {
    testthat::expect_s4_class(results$familiarModel, "familiarNaiveModel")
    testthat::expect_length(results$familiarModel@required_features, 0L)
    testthat::expect_length(results$familiarModel@model_features, 0L)
    testthat::expect_length(results$familiarModel@novelty_features, 0L)
    
    # Internally, naive models are forced to resolve to not train novelty
    # detectors.
    testthat::expect_s4_class(results$familiarModel@novelty_detector, "familiarNoneNoveltyDetector")
    testthat::expect_length(results$familiarModel@novelty_detector@model_features, 0L)
    testthat::expect_length(results$familiarModel@novelty_detector@required_features, 0L)
  }
)


# Test suppression of naive models.
results <- familiar::summon_familiar(
  data = data,
  vimp_method = "mim",
  learner = "glm_gaussian",
  hyperparameter = list("glm_gaussian" = list("sign_size" = c(4, 5, 6))),
  allow_naive_models = FALSE,
  outcome_type = "continuous",
  experimental_design = "fs+mb",
  estimation_type = "point",
  verbose = debug_flag
)

testthat::test_that(
  "naive models are not trained", 
  {
    testthat::expect_s4_class(results$familiarModel, "familiarGLM")
    testthat::expect_gte(length(results$familiarModel@required_features), 4L)
    testthat::expect_gte(length(results$familiarModel@model_features), 4L)
    testthat::expect_gte(length(results$familiarModel@novelty_features), 4L)
    
    # Internally, naive models are forced to resolve to not train novelty
    # detectors.
    testthat::expect_s4_class(results$familiarModel@novelty_detector, "familiarIsolationForest")
    testthat::expect_gte(length(results$familiarModel@novelty_detector@model_features), 4L)
    testthat::expect_gte(length(results$familiarModel@novelty_detector@required_features), 4L)
  }
)
