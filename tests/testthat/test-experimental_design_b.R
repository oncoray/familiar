testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE

# Without external validation --------------------------------------------------

data <- familiar:::test_create_good_data(outcome_type = "binomial", to_data_object = FALSE)

# Only training
results <- familiar::summon_familiar(
  data = data,
  experimental_design = "mb",
  outcome_type = "binomial",
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  vimp_method = "mim",
  learner = "glm_logistic",
  estimation_type = "point",
  shap_max_iterations = 10L,
  parallel = FALSE,
  verbose = FALSE
)

testthat::test_that("development-only experiment is correctly created", {
  testthat::expect_length(results$familiarModel, 1L)
  testthat::expect_length(results$familiarData, 1L)
  testthat::expect_equal(results$familiarData@name, "development")
})


# Internal bootstraps (incomplete)
results <- familiar::summon_familiar(
  data = data,
  experimental_design = "bt(mb, 3)",
  outcome_type = "binomial",
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  vimp_method = "mim",
  learner = "glm_logistic",
  estimation_type = "point",
  shap_max_iterations = 10L,
  parallel = FALSE,
  verbose = FALSE
)

testthat::test_that("incomplete bootstrap-only experiment is correctly created", {
  testthat::expect_length(results$familiarModel, 3L)
  testthat::expect_length(results$familiarData, 1L)
  testthat::expect_equal(results$familiarData@name, "development")
  
  performance_data <- familiar::export_model_performance(
    results$familiarCollection,
    aggregate_results = FALSE
  )[[1L]]@data
  
  # Expect that the values are not the same.
  testthat::expect_length(unique(performance_data$value), 3L)
})


# Internal cross-validation
results <- familiar::summon_familiar(
  data = data,
  experimental_design = "cv(mb, 3)",
  outcome_type = "binomial",
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  vimp_method = "mim",
  learner = "glm_logistic",
  estimation_type = "point",
  shap_max_iterations = 10L,
  parallel = FALSE,
  verbose = FALSE
)

testthat::test_that("cv-only experiment is correctly created", {
  testthat::expect_length(results$familiarModel, 3L)
  testthat::expect_length(results$familiarData, 2L)
  testthat::expect_equal(results$familiarData[[1L]]@name, "development")
  testthat::expect_equal(results$familiarData[[2L]]@name, "internal_validation")
  
  performance_data <- familiar::export_model_performance(
    results$familiarCollection,
    aggregate_results = FALSE
  )[[1L]]@data
  
  # Expect that the values are not the same.
  testthat::expect_length(unique(performance_data$value), 6L)
})

# Internal bootstraps (full)
results <- familiar::summon_familiar(
  data = data,
  experimental_design = "bs(mb, 3)",
  outcome_type = "binomial",
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  vimp_method = "mim",
  learner = "glm_logistic",
  estimation_type = "point",
  shap_max_iterations = 10L,
  parallel = FALSE,
  verbose = FALSE
)

testthat::test_that("bootstrap-only experiment is correctly created", {
  testthat::expect_length(results$familiarModel, 3L)
  testthat::expect_length(results$familiarData, 2L)
  testthat::expect_equal(results$familiarData[[1L]]@name, "development")
  testthat::expect_equal(results$familiarData[[2L]]@name, "internal_validation")
  
  performance_data <- familiar::export_model_performance(
    results$familiarCollection,
    aggregate_results = FALSE
  )[[1L]]@data
  
  # Expect that the values are not the same.
  testthat::expect_length(unique(performance_data$value), 6L)
})


# Leave-one-out cross-validation
results <- familiar::summon_familiar(
  data = data[1:30L,],
  experimental_design = "lv(mb)",
  outcome_type = "binomial",
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  vimp_method = "mim",
  learner = "glm_logistic",
  estimation_type = "point",
  shap_max_iterations = 10L,
  parallel = FALSE,
  verbose = FALSE
)

testthat::test_that("loocv-only experiment is correctly created", {
  testthat::expect_length(results$familiarModel, 30L)
  testthat::expect_length(results$familiarData, 2L)
  testthat::expect_equal(results$familiarData[[1L]]@name, "development")
  testthat::expect_equal(results$familiarData[[2L]]@name, "internal_validation")
  
  performance_data <- familiar::export_model_performance(
    results$familiarCollection,
    aggregate_results = FALSE
  )[[1L]]@data
  
  # Expect that the values are not the same. Note that the detail-level is
  # automatically changed to ensemble because of the limited number of values
  # in the 
  testthat::expect_length(unique(performance_data$value), 2L)
})



# Internal cross-validation with nested (incomplete) bootstraps
results <- familiar::summon_familiar(
  data = data,
  experimental_design = "cv(bt(mb, 2), 3)",
  outcome_type = "binomial",
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  vimp_method = "mim",
  learner = "glm_logistic",
  estimation_type = "point",
  shap_max_iterations = 10L,
  iteration_seed = 9L,
  parallel = FALSE,
  verbose = FALSE
)

incomplete_bootstrap_performance_data <- familiar::export_model_performance(
  results$familiarCollection,
  aggregate_results = FALSE
)[[1L]]@data

testthat::test_that("nested cv-only experiment is correctly created", {
  testthat::expect_length(results$familiarModel, 6L)
  testthat::expect_length(results$familiarData, 2L)
  testthat::expect_equal(results$familiarData[[1L]]@name, "development")
  testthat::expect_equal(results$familiarData[[2L]]@name, "internal_validation")
  
  # Expect that the values are not the same.
  testthat::expect_length(unique(incomplete_bootstrap_performance_data$value), 12L)
})


# Internal cross-validation with nested (full) bootstraps
results <- familiar::summon_familiar(
  data = data,
  experimental_design = "cv(bs(mb, 2), 3)",
  outcome_type = "binomial",
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  vimp_method = "mim",
  learner = "glm_logistic",
  estimation_type = "point",
  shap_max_iterations = 10L,
  iteration_seed = 9L,
  parallel = FALSE,
  verbose = FALSE
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
  y =  results$familiarData[[2L]]@prediction_data[[1L]]@data[, mget(c("sample_id", "red"))],
  by = "sample_id",
  suffixes = c("_dev", "_int"),
  all = FALSE
)

testthat::test_that("cv-only with nested bootstraps experiment is correctly created", {
  testthat::expect_length(results$familiarModel, 6L)
  testthat::expect_length(results$familiarData, 2L)
  testthat::expect_equal(results$familiarData[[1L]]@name, "development")
  testthat::expect_equal(results$familiarData[[2L]]@name, "internal_validation")
  
  # Expect that the values are not the same.
  testthat::expect_length(unique(full_bootstrap_performance_data$value), 12L)
  
  # Expect that fewer than 150 samples appear in the training dataset. If this
  # fails, check that the iteration seed correctly generates the same sample
  # set consistently.
  testthat::expect_lt(
    nrow(results$familiarData[[1L]]@prediction_data[[1L]]@data),
    nrow(data)
  )
  
  # Expect that predicted probabilities are not all the same.
  testthat::expect_false(all(prediction_data$red_dev == prediction_data$red_int))
})



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
  verbose = FALSE
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
  verbose = FALSE
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
  verbose = FALSE
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
  verbose = FALSE
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
  verbose = FALSE
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


# With unpooled collections ----------------------------------------------------

# Set evaluate_top_level_only to FALSE evaluate underlying data divisions.
results <- familiar::summon_familiar(
  data = data,
  experimental_design = "cv(bs(mb, 2), 3) + ev",
  evaluate_top_level_only = FALSE,
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
  verbose = FALSE
)


testthat::test_that("cv-only with nested bootstraps experiment is correctly created", {
  testthat::expect_length(results$familiarModel, 6L)
  testthat::expect_length(results$familiarData, 12L)
  testthat::expect_length(results$familiarCollection, 4L)
  testthat::expect_setequal(
    sapply(results$familiarData, function(x) (x@name)),
    c("development", "internal_validation", "external_validation")
  )
  
  pooled_collection <- results$familiarCollection[
    sapply(results$familiarCollection, function(x) (endsWith(x@name, "pooled_collection")))
  ][[1L]]
  prediction_data <- familiar::export_prediction_data(pooled_collection)
  prediction_data <- prediction_data$classification[[1L]]@data
  ext_val_samples <- prediction_data[data_set == "ext. validation"]$sample_id
  int_val_samples <- prediction_data[data_set == "int. validation"]$sample_id
  dev_samples <- prediction_data[data_set == "development"]$sample_id
  
  testthat::expect_length(intersect(ext_val_samples, dev_samples), 0L)
  testthat::expect_length(intersect(ext_val_samples, int_val_samples), 0L)
  testthat::expect_length(ext_val_samples, nrow(data[batch_id == "test"]))
  testthat::expect_length(int_val_samples, nrow(data[batch_id == "basic"]))
  testthat::expect_lt(length(dev_samples), nrow(data[batch_id == "basic"]))
  
  cv_1_collection <- results$familiarCollection[
    sapply(results$familiarCollection, function(x) (endsWith(x@name, "2_1_collection")))
  ][[1L]]
  prediction_data <- familiar::export_prediction_data(cv_1_collection)
  prediction_data <- prediction_data$classification[[1L]]@data
  ext_val_samples_1 <- prediction_data[data_set == "ext. validation"]$sample_id
  int_val_samples_1 <- prediction_data[data_set == "int. validation"]$sample_id
  dev_samples_1 <- prediction_data[data_set == "development"]$sample_id
  
  testthat::expect_length(intersect(ext_val_samples_1, dev_samples_1), 0L)
  testthat::expect_length(intersect(ext_val_samples_1, int_val_samples_1), 0L)
  testthat::expect_length(intersect(int_val_samples_1, dev_samples_1), 0L)
  testthat::expect_length(ext_val_samples_1, nrow(data[batch_id == "test"]))
  testthat::expect_lt(length(int_val_samples_1) + length(dev_samples_1), nrow(data[batch_id == "basic"]))
  
  cv_2_collection <- results$familiarCollection[
    sapply(results$familiarCollection, function(x) (endsWith(x@name, "2_2_collection")))
  ][[1L]]
  prediction_data <- familiar::export_prediction_data(cv_2_collection)
  prediction_data <- prediction_data$classification[[1L]]@data
  ext_val_samples_2 <- prediction_data[data_set == "ext. validation"]$sample_id
  int_val_samples_2 <- prediction_data[data_set == "int. validation"]$sample_id
  dev_samples_2 <- prediction_data[data_set == "development"]$sample_id
  
  testthat::expect_length(intersect(ext_val_samples_2, dev_samples_2), 0L)
  testthat::expect_length(intersect(ext_val_samples_2, int_val_samples_2), 0L)
  testthat::expect_length(intersect(int_val_samples_2, dev_samples_2), 0L)
  testthat::expect_length(ext_val_samples_2, nrow(data[batch_id == "test"]))
  testthat::expect_lt(length(int_val_samples_2) + length(dev_samples_2), nrow(data[batch_id == "basic"]))
  
  cv_3_collection <- results$familiarCollection[
    sapply(results$familiarCollection, function(x) (endsWith(x@name, "2_3_collection")))
  ][[1L]]
  prediction_data <- familiar::export_prediction_data(cv_3_collection)
  prediction_data <- prediction_data$classification[[1L]]@data
  ext_val_samples_3 <- prediction_data[data_set == "ext. validation"]$sample_id
  int_val_samples_3 <- prediction_data[data_set == "int. validation"]$sample_id
  dev_samples_3 <- prediction_data[data_set == "development"]$sample_id
  
  testthat::expect_length(intersect(ext_val_samples_3, dev_samples_3), 0L)
  testthat::expect_length(intersect(ext_val_samples_3, int_val_samples_3), 0L)
  testthat::expect_length(intersect(int_val_samples_3, dev_samples_3), 0L)
  testthat::expect_length(ext_val_samples_3, nrow(data[batch_id == "test"]))
  testthat::expect_lt(length(int_val_samples_3) + length(dev_samples_3), nrow(data[batch_id == "basic"]))
  
  # Internal validation folds between experiments do not overlap.
  testthat::expect_length(intersect(int_val_samples_1, int_val_samples_2), 0L)
  testthat::expect_length(intersect(int_val_samples_1, int_val_samples_3), 0L)
  testthat::expect_length(intersect(int_val_samples_2, int_val_samples_3), 0L)
  
  # External validation folds between experiments are the same.
  testthat::expect_setequal(ext_val_samples_1, ext_val_samples_2)
  testthat::expect_setequal(ext_val_samples_1, ext_val_samples_3)
  testthat::expect_setequal(ext_val_samples_2, ext_val_samples_3)
})
