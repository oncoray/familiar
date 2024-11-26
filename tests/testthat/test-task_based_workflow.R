# Don't perform any further tests on CRAN due to running time.
testthat::skip_on_cran()
testthat::skip_on_ci()

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
  verbose = FALSE,
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
  verbose = TRUE,
  parallel = FALSE
)

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
  verbose = TRUE,
  parallel = FALSE
)

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
  verbose = TRUE,
  parallel = FALSE
)


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
  verbose = TRUE,
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
  verbose = TRUE,
  parallel = FALSE
)
