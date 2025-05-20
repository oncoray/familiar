# Don't perform any further tests on CRAN due to time of running the complete
# test.
testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE

familiar:::test_plots(
  plot_function = familiar::plot_shap_summary,
  data_element = "shap",
  outcome_type_available = c("multinomial", "survival"),
  # outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
  debug = debug_flag
)



familiar:::test_plot_ordering(
  plot_function = familiar::plot_shap_summary,
  outcome_type_available = "multinomial",
  data_element = "shap",
  plot_args = list(
    "verbose" = FALSE
),
  debug = debug_flag
)
