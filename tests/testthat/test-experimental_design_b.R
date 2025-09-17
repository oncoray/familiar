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
})


# With external validation -----------------------------------------------------

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

testthat::test_that("development-only experiment is correctly created", {
  testthat::expect_length(results$familiarModel, 1L)
  testthat::expect_length(results$familiarData, 2L)
  testthat::expect_equal(results$familiarData[[1L]]@name, "development")
  testthat::expect_equal(results$familiarData[[2L]]@name, "external_validation")
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

testthat::test_that("incomplete bootstrap-only experiment is correctly created", {
  testthat::expect_length(results$familiarModel, 3L)
  testthat::expect_length(results$familiarData, 2L)
  testthat::expect_equal(results$familiarData[[1L]]@name, "development")
  testthat::expect_equal(results$familiarData[[2L]]@name, "external_validation")
})
