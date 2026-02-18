testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE


# With external validation -----------------------------------------------------

data <- familiar:::test_create_good_data(outcome_type = "binomial", to_data_object = FALSE)
data[101L:150L, "batch_id" := "test"]

# Training + external validation
results <- familiar::summon_familiar(
  data = data,
  experimental_design = "mb+ev",
  outcome_type = "binomial",
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  validation_batch_id = "test",
  vimp_method = "mim",
  learner = "glm_logistic",
  estimation_type = "point",
  shap_max_iterations = 10L,
  parallel = FALSE,
  verbose = FALSE
)

testthat::test_that("development + evaluation experiment is correctly created", {
  testthat::expect_length(results$familiarModel, 1L)
  testthat::expect_length(results$familiarData, 2L)
  testthat::expect_equal(results$familiarData[[1L]]@name, "development")
  testthat::expect_equal(results$familiarData[[2L]]@name, "external_validation")

  performance_data <- familiar::export_model_performance(
    results$familiarCollection,
    aggregate_results = FALSE
  )[[1L]]@data
  
  # Expect that the values are not the same.
  testthat::expect_length(unique(performance_data$value), 2L)
})


# Internal bootstraps (incomplete) + external validation
results <- familiar::summon_familiar(
  data = data,
  experimental_design = "bt(mb, 3)+ev",
  outcome_type = "binomial",
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  validation_batch_id = "test",
  vimp_method = "mim",
  learner = "glm_logistic",
  estimation_type = "point",
  shap_max_iterations = 10L,
  parallel = FALSE,
  verbose = debug_flag
)

testthat::test_that("incomplete bootstrap-only + evaluation experiment is correctly created", {
  testthat::expect_length(results$familiarModel, 3L)
  testthat::expect_length(results$familiarData, 2L)
  testthat::expect_equal(results$familiarData[[1L]]@name, "development")
  testthat::expect_equal(results$familiarData[[2L]]@name, "external_validation")

  performance_data <- familiar::export_model_performance(
    results$familiarCollection,
    aggregate_results = FALSE
  )[[1L]]@data
  
  # Expect that the values are not the same.
  testthat::expect_length(unique(performance_data$value), 6L)
})


# Internal cross-validation
results <- familiar::summon_familiar(
  data = data,
  experimental_design = "cv(mb, 3)+ev",
  outcome_type = "binomial",
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  validation_batch_id = "test",
  vimp_method = "mim",
  learner = "glm_logistic",
  estimation_type = "point",
  shap_max_iterations = 10L,
  parallel = FALSE,
  verbose = debug_flag
)

testthat::test_that("cv + evaluation experiment is correctly created", {
  testthat::expect_length(results$familiarModel, 3L)
  testthat::expect_length(results$familiarData, 3L)
  testthat::expect_setequal(
    sapply(results$familiarData, function(x) (x@name)),
    c("development", "internal_validation", "external_validation")
  )
  
  performance_data <- familiar::export_model_performance(
    results$familiarCollection,
    aggregate_results = FALSE
  )[[1L]]@data
  
  # Expect that the values are not the same.
  testthat::expect_length(unique(performance_data$value), 9L)
})


# Internal bootstraps (full)
results <- familiar::summon_familiar(
  data = data,
  experimental_design = "bs(mb, 3)+ev",
  outcome_type = "binomial",
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  validation_batch_id = "test",
  vimp_method = "mim",
  learner = "glm_logistic",
  estimation_type = "point",
  shap_max_iterations = 10L,
  parallel = FALSE,
  verbose = debug_flag
)

testthat::test_that("bootstrap + evaluation experiment is correctly created", {
  testthat::expect_length(results$familiarModel, 3L)
  testthat::expect_length(results$familiarData, 3L)
  testthat::expect_setequal(
    sapply(results$familiarData, function(x) (x@name)),
    c("development", "internal_validation", "external_validation")
  )
  
  performance_data <- familiar::export_model_performance(
    results$familiarCollection,
    aggregate_results = FALSE
  )[[1L]]@data
  
  # Expect that the values are not the same.
  testthat::expect_length(unique(performance_data$value), 9L)
})



results <- familiar::summon_familiar(
  data = data[c(1L:30L, 101L:150L),],
  experimental_design = "lv(mb) + ev",
  outcome_type = "binomial",
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  validation_batch_id = "test",
  vimp_method = "mim",
  learner = "glm_logistic",
  estimation_type = "point",
  shap_max_iterations = 10L,
  parallel = FALSE,
  verbose = debug_flag
)

testthat::test_that("loocv-only experiment is correctly created", {
  testthat::expect_length(results$familiarModel, 30L)
  testthat::expect_length(results$familiarData, 3L)
  testthat::expect_setequal(
    sapply(results$familiarData, function(x) (x@name)),
    c("development", "internal_validation", "external_validation")
  )
  
  performance_data <- familiar::export_model_performance(
    results$familiarCollection,
    aggregate_results = FALSE
  )[[1L]]@data
  
  # Expect that the values are not the same. Note that the detail-level is
  # automatically changed to ensemble because of the limited number of values
  # in the internal validation set (1 per fold.)
  testthat::expect_length(unique(performance_data$value), 3L)
})



# Internal cross-validation with nested (full) bootstraps
results <- familiar::summon_familiar(
  data = data,
  experimental_design = "cv(bs(mb, 2), 3) + ev",
  outcome_type = "binomial",
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  validation_batch_id = "test",
  vimp_method = "mim",
  learner = "glm_logistic",
  estimation_type = "point",
  shap_max_iterations = 10L,
  iteration_seed = 9L,
  parallel = FALSE,
  verbose = debug_flag
)

full_bootstrap_performance_data <- familiar::export_model_performance(
  results$familiarCollection,
  aggregate_results = FALSE
)[[1L]]@data

# Get predicted probabilities for red. The bootstraps might not visit all
# training data. More over the probabilities should generally be different
# because different models are used to predict each sample.
prediction_data <- merge(
  x = results$familiarData[[1L]]@prediction_data[[1L]]@data[, mget(c("sample_id", "red"))],
  y =  results$familiarData[[3L]]@prediction_data[[1L]]@data[, mget(c("sample_id", "red"))],
  by = "sample_id",
  suffixes = c("_dev", "_int"),
  all = FALSE
)

testthat::test_that("cv-only with nested bootstraps experiment is correctly created", {
  testthat::expect_length(results$familiarModel, 6L)
  testthat::expect_length(results$familiarData, 3L)
  testthat::expect_setequal(
    sapply(results$familiarData, function(x) (x@name)),
    c("development", "internal_validation", "external_validation")
  )
  
  # Expect that the values are not the same.
  testthat::expect_gt(length(unique(full_bootstrap_performance_data$value)), 6L)
  
  # Expect that fewer than 150 samples appear in the training dataset. If this
  # fails, check that the iteration seed correctly generates the same sample
  # set consistently.
  testthat::expect_lt(
    nrow(results$familiarData[[1L]]@prediction_data[[1L]]@data),
    nrow(data[batch_id == "basic"])
  )
  
  # Expect that predicted probabilities are not all the same.
  testthat::expect_false(all(prediction_data$red_dev == prediction_data$red_int))
  
  # Expect that there is no overlap between development and external validation.
  testthat::expect_equal(
    nrow(merge(
      x = results$familiarData[[1L]]@prediction_data[[1L]]@data[, mget(c("sample_id", "red"))],
      y =  results$familiarData[[2L]]@prediction_data[[1L]]@data[, mget(c("sample_id", "red"))],
      by = "sample_id",
      suffixes = c("_dev", "_ext"),
      all = FALSE
    )),
    0L
  )
  
  # Expect that there is no overlap between internal and external development.
  testthat::expect_equal(
    nrow(merge(
      x = results$familiarData[[3L]]@prediction_data[[1L]]@data[, mget(c("sample_id", "red"))],
      y =  results$familiarData[[2L]]@prediction_data[[1L]]@data[, mget(c("sample_id", "red"))],
      by = "sample_id",
      suffixes = c("_int", "_ext"),
      all = FALSE
    )),
    0L
  )
})
