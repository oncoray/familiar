# Don't perform any further tests on CRAN due to time of running the complete
# test.
testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE

familiar:::test_plots(
  plot_function = familiar::plot_shap_waterfall,
  data_element = "shap",
  shap_max_iterations = 10L,
  outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
  debug = debug_flag,
  not_available_no_samples = FALSE
)

# Test only bog-standard data: no edge cases.
familiar:::test_plots(
  plot_function = familiar::plot_shap_waterfall,
  data_element = "shap",
  shap_max_iterations = 10L,
  evaluation_time = c(1.0, 2.0, 3.5),
  test_config = "normal",
  debug = debug_flag
)

# Test with single instance
familiar:::test_plots(
  plot_function = familiar::plot_shap_waterfall,
  data_element = "shap",
  shap_max_iterations = 10L,
  evaluation_time = c(1.0, 2.0, 3.5),
  test_config = "single instance",
  debug = debug_flag
)

# Test with single instance and limit to the number of features.
familiar:::test_plots(
  plot_function = familiar::plot_shap_waterfall,
  data_element = "shap",
  shap_max_iterations = 10L,
  test_config = "single instance",
  plot_args = list("limit_n_features" = 2L),
  debug = debug_flag
)

# Test plotting for specific samples.
for (outcome_type in c("binomial", "multinomial", "continuous", "survival")) {
  data <- familiar:::test_create_good_data(outcome_type = outcome_type)
  
  model <- familiar::train_familiar(
    data = data,
    learner = switch(
      outcome_type,
      "binomial" = "glm_logistic",
      "multinomial" = "glm",
      "continuous" = "glm_gaussian",
      "survival" = "cox"
    ),
    vimp_method = "mim",
    parallel = FALSE,
    verbose = FALSE
  )
  
  plot <- familiar::plot_shap_waterfall(
    object = model,
    data = data@data[1L, ],
    shap_phi_0 = switch(
      outcome_type,
      "binomial" = 0.5,
      "multinomial" = c(0.4, 0.3, 0.3),
      "continuous" = 0.25,
      "survival" = c(0.2, 0.1)
    ),
    evaluation_times = switch(
      outcome_type,
      "survival" = c(2.0, 3.0),
      NULL
    ),
    verbose = FALSE,
    draw = debug_flag
  )
}
