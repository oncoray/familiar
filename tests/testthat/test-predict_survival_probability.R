# Don't perform any further tests on CRAN due to time of running the complete
# test.
testthat::skip_on_cran()

# Create test data sets.
data <- familiar:::test_create_good_data("survival")

# Cox PH
cox_model <- familiar:::test_train(
  data = data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list("sign_size" = familiar:::get_n_features(data)),
  learner = "cox"
)

# Random forest model
rf_model <- familiar:::test_train(
  data = data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list("sign_size" = familiar:::get_n_features(data)),
  learner = "random_forest_ranger_default"
)

# Survival regression model
surv_reg_model <- familiar:::test_train(
  data = data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list("sign_size" = familiar:::get_n_features(data)),
  learner = "survival_regr_weibull"
)


# Predictions using different models.
cox_predictions <- familiar::predict(
  object = cox_model,
  newdata = data,
  type = "survival_probability",
  time = 1.0
)

rf_predictions <- familiar::predict(
  object = rf_model,
  newdata = data,
  type = "survival_probability",
  time = 1.0
)

surv_reg_predictions <- familiar::predict(
  object = surv_reg_model,
  newdata = data,
  type = "survival_probability",
  time = 1.0
)

predictions_1 <- data.table::data.table(
  cox = cox_predictions$predicted_outcome,
  rf = rf_predictions$predicted_outcome,
  surv_reg = surv_reg_predictions$predicted_outcome
)
