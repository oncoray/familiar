# Don't perform these tests on CRAN due to running time.
testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE

outcome_type_available = c("continuous", "binomial", "multinomial", "survival")

for (outcome_type in outcome_type_available) {
  # Create dataset
  full_data <- familiar:::test_create_good_data(outcome_type)
  full_data@data[, "batch_id" := "train"]
  full_data@data[121:150, "batch_id" := "test"]
  
  # Set variable importance method.
  vimp_method <- c("concordance", "mim")
  
  # Set learner
  learner_glm <- switch(
    outcome_type,
    "continuous" = "glm_gaussian",
    "binomial" = "glm_logistic",
    "multinomial" = "glm_multinomial",
    "survival" = "cox"
  )
  learner_lasso <- switch(
    outcome_type,
    "continuous" = "lasso_gaussian",
    "binomial" = "lasso_binomial",
    "multinomial" = "lasso_multinomial",
    "survival" = "lasso_cox"
  )
  
  # Get output.
  output <- suppressWarnings(familiar::summon_familiar(
    data = full_data,
    experimental_design = "mb + ev",
    validation_batch_id = "test",
    vimp_method = vimp_method,
    learner = c(learner_glm, learner_lasso),
    estimation_type = "point",
    time_max = 3.5,
    verbose = debug_flag,
    parallel = FALSE
  ))
}
