# Don't perform any further tests on CRAN due to running time.
testthat::skip_on_cran()
testthat::skip_on_ci()

verbose <- FALSE

# Create data.table.
data <- familiar:::test_create_good_data(
  outcome_type = "binomial",
  to_data_object = FALSE
)

# Create data assignment object.
experiment_data_assignment <- familiar::precompute_data_assignment(
  data = data,
  experimental_design = "bs(fs+mb,3)",
  outcome_type = "binomial",
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  class_levels = c("red", "green"),
  verbose = verbose,
  parallel = FALSE
)

# Create feature info object.
experiment_feature_info <- familiar::precompute_feature_info(
  data = data,
  experiment_data = experiment_data_assignment,
  outcome_type = "binomial",
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  class_levels = c("red", "green"),
  verbose = verbose,
  parallel = FALSE
)

testthat::test_that("feature information is present", {
  testthat::expect_false(familiar:::is_empty(experiment_feature_info@feature_info))
})


# Create variable importance
experiment_vimp <- familiar::precompute_vimp(
  data = data,
  experiment_data = experiment_feature_info,
  vimp_method = "mim",
  outcome_type = "binomial",
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  class_levels = c("red", "green"),
  verbose = verbose,
  parallel = FALSE
)

testthat::test_that("variable importance data is present", {
  testthat::expect_false(familiar:::is_empty(experiment_vimp@feature_info))
  testthat::expect_false(familiar:::is_empty(experiment_vimp@vimp_table_list))
})


# Train model
model <- familiar::train_familiar(
  data = data,
  experiment_data = experiment_vimp,
  vimp_method = "mim",
  learner = "glm_logistic",
  outcome_type = "binomial",
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  class_levels = c("red", "green"),
  verbose = verbose,
  parallel = FALSE
)

testthat::test_that("all models are present", {
  testthat::expect_true(all(sapply(model, familiar:::model_is_trained)))
  testthat::expect_false(any(sapply(model, function(x) (is.null(x@vimp_table)))))
})

# Check without explicit variable importance computation -----------------------
# Create variable importance
experiment_vimp <- familiar::precompute_vimp(
  data = data,
  experimental_design = "bs(mb,3)",
  vimp_method = "mim",
  outcome_type = "binomial",
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  class_levels = c("red", "green"),
  verbose = verbose,
  parallel = FALSE
)

testthat::test_that("variable importance data is absent", {
  testthat::expect_null(experiment_vimp@feature_info)
  testthat::expect_null(experiment_vimp@vimp_table_list)
})

# Train model
model <- familiar::train_familiar(
  data = data,
  experiment_data = experiment_vimp,
  vimp_method = "mim",
  learner = "glm_logistic",
  outcome_type = "binomial",
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  class_levels = c("red", "green"),
  verbose = verbose,
  parallel = FALSE
)

testthat::test_that("all models are present", {
  testthat::expect_true(all(sapply(model, familiar:::model_is_trained)))
  testthat::expect_false(any(sapply(model, function(x) (is.null(x@vimp_table)))))
})


# Check using variable importance from feature selection -----------------------
experiment_vimp <- familiar::precompute_vimp(
  data = data,
  experimental_design = "bs(fs,3)+bs(mb,3)",
  vimp_method = "mim",
  outcome_type = "binomial",
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  class_levels = c("red", "green"),
  verbose = verbose,
  parallel = FALSE
)

testthat::test_that("variable importance data is present", {
  testthat::expect_false(familiar:::is_empty(experiment_vimp@feature_info))
  testthat::expect_false(familiar:::is_empty(experiment_vimp@vimp_table_list))
})

# Train model
model <- familiar::train_familiar(
  data = data,
  experiment_data = experiment_vimp,
  optimisation_determine_vimp = FALSE,
  vimp_method = "mim",
  learner = "glm_logistic",
  outcome_type = "binomial",
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  class_levels = c("red", "green"),
  verbose = verbose,
  parallel = FALSE
)

testthat::test_that("all models are present", {
  testthat::expect_true(all(sapply(model, familiar:::model_is_trained)))
  testthat::expect_false(any(sapply(model, function(x) (is.null(x@vimp_table)))))
})


# Including evaluation ---------------------------------------------------------

data <- familiar:::test_create_small_good_data("binomial")

results <- familiar::summon_familiar(
  data = data,
  experimental_design = "bs(fs,3)+bs(mb, 3)",
  evaluation_elements = "auc_data",
  vimp_method = "mim",
  learner = "glm_logistic",
  evaluate_top_level_only = FALSE,
  outcome_type = "binomial",
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  class_levels = c("red", "green"),
  verbose = verbose,
  parallel = FALSE
)

testthat::test_that("all output is present", {
  testthat::expect_true(all(sapply(results$familiarModel, familiar:::model_is_trained)))
  testthat::expect_length(results$familiarModel, 3L)
  testthat::expect_length(results$familiarData, 8L)
  testthat::expect_length(results$familiarCollection, 4L)
})
