testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE

data <- familiar:::test_create_good_data(outcome_type = "binomial", to_data_object = FALSE)
data[101L:150L, "batch_id" := "test"]


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
  verbose = debug_flag
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



# Set evaluate_top_level_only to FALSE evaluate underlying data divisions.
results <- familiar::summon_familiar(
  data = data[c(1L:30L, 101L:150L),],
  experimental_design = "lv(mb) + ev",
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
