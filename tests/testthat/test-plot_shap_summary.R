# Don't perform any further tests on CRAN due to time of running the complete
# test.
testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE

familiar:::test_plots(
  plot_function = familiar::plot_shap_summary,
  data_element = "shap",
  outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
  debug = debug_flag
)


# Swarm plot -------------------------------------------------------------------
familiar:::test_plot_ordering(
  plot_function = familiar::plot_shap_summary,
  data_element = "shap",
  evaluation_time = c(1.0, 2.0, 3.5),
  plot_args = list(
    "verbose" = FALSE,
    "draw" = debug_flag
  ),
  debug = debug_flag
)


familiar:::test_plot_ordering(
  plot_function = familiar::plot_shap_summary,
  data_element = "shap",
  evaluation_time = c(1.0, 2.0, 3.5),
  plot_args = list(
    "verbose" = FALSE,
    "draw" = debug_flag
  ),
  use_single_sample = TRUE,
  debug = debug_flag
)
