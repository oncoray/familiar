# Don't perform any further tests on CRAN due to time of running the complete
# test.
testthat::skip_on_cran()
if (!rlang::is_installed("survival")) testthat::skip()

results_fun <- function(t, x, model) {
  predictions <- familiar::predict(
    object = model,
    newdata = x,
    type = "survival_probability",
    time = t
  )
  
  return(predictions$predicted_outcome)
} 

# Create test data set ---------------------------------------------------------

# Create random stream object so that the same numbers are produced every
# time.
r <- familiar:::.start_random_number_stream(seed = 1963L)
n_series_instances <- 1000L

# Draw random numbers for 1 feature.
feature_1 <- familiar:::fam_rnorm(n_series_instances, rstream_object = r)

# Simulate event times using exponential function and lambda = 0.5
outcome_time <- - log(familiar:::fam_runif(n = n_series_instances, min = 0.0, max = 1.0, rstream_object = r)) / 
  (0.5 * exp(feature_1))

# Simulate censoring times using exponential function and lambda = 0.1
censor_time <- - log(familiar:::fam_runif(n = n_series_instances, min = 0.0, max = 1.0, rstream_object = r)) / 0.1
outcome_event <- rep_len(1L, length.out = n_series_instances)
outcome_event[censor_time < outcome_time] <- 0L
outcome_time[outcome_event == 0L] <- censor_time[outcome_event == 0L]

# Create basic table.
data <- data.table::data.table(
  "batch_id" = "basic",
  "sample_id" = paste0("sample_", seq_len(n_series_instances)),
  "series_id" = 1L,
  "feature_1" = feature_1,
  "outcome_time" = outcome_time,
  "outcome_event" = outcome_event
)

# Turn into a data object.
data <- familiar::as_data_object(
  data = data,
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  outcome_column = c("outcome_time", "outcome_event"),
  outcome_type = "survival"
)

# Single test-dataset
test_data <- data.table::data.table(
  "batch_id" = "basic",
  "sample_id" = 1001L,
  "series_id" = 1L,
  "feature_1" = 0.0,
  "outcome_time" = 1.386,
  "outcome_event" = 1L
)

test_data <- familiar::as_data_object(
  data = test_data,
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  outcome_column = c("outcome_time", "outcome_event"),
  outcome_type = "survival"
)

# Train models -----------------------------------------------------------------

# Cox PH
cox_model <- familiar:::test_train(
  data = data,
  cluster_method = "none",
  normalisation_method = "none",
  transformation_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list("sign_size" = familiar:::get_n_features(data)),
  learner = "cox"
)

# Random forest model (ranger)
rf_model <- familiar:::test_train(
  data = data,
  cluster_method = "none",
  normalisation_method = "none",
  transformation_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list("sign_size" = familiar:::get_n_features(data)),
  learner = "random_forest_ranger_default"
)

# Survival regression model
surv_reg_model <- familiar:::test_train(
  data = data,
  cluster_method = "none",
  normalisation_method = "none",
  transformation_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list("sign_size" = familiar:::get_n_features(data)),
  learner = "survival_regr_weibull"
)

# Evaluate predictions ---------------------------------------------------------

time_points <- (seq_len(51L) - 1L) / 10.0

# Get Kaplan-Meier data.
km_fit <- survival::survfit(
  Surv(outcome_time, outcome_event) ~ 1,
  data = data@data
)

km_fit_prob <- stats::approx(
  x = km_fit$time,
  y = km_fit$surv,
  xout = time_points,
  method = "linear",
  rule = 2L,
  yleft = 1.0
)$y


# Cox PH
cox_predictions <- sapply(
  time_points,
  results_fun,
  x = test_data,
  model = cox_model
)

testthat::test_that(
  "Cox PH predictions are plausible.", 
  {
    # For the current data, predictions should be very close to the KM-curve, 
    # since feature = 0 means a relative risk of approximately 0.
    testthat::expect_true(all(abs(km_fit_prob - cox_predictions) < 0.01))
  }
)


# Weibull model
surv_reg_predictions <- sapply(
  time_points,
  results_fun,
  x = test_data,
  model = surv_reg_model
)

testthat::test_that(
  "Weibull regression predictions are plausible.",
  {
    # For the current data, predictions should be very close to the KM-curve, 
    # since feature = 0 means a relative risk of approximately 0. However,
    # the Weibull model does not rely on the observed survival curve, and thus
    # will deviate from the Kaplan-Meier curve somewhat.
    testthat::expect_true(all(abs(km_fit_prob - surv_reg_predictions) < 0.10))
  }
)


# RF model (ranger)
rf_predictions <- sapply(
  time_points,
  results_fun,
  x = test_data,
  model = rf_model
)

testthat::test_that(
  "Random forest (ranger)-based predictions are plausible.",
  {
    # For the current data, predictions should be very close to the KM-curve, 
    # since feature = 0 means a relative risk of approximately 0. However,
    # the RF model does not rely on the observed survival curve, and thus
    # will deviate from the Kaplan-Meier curve somewhat.
    testthat::expect_true(all(abs(km_fit_prob - rf_predictions) < 0.15))
  }
)
