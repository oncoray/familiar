# Don't perform any further tests on CRAN due to time of running the complete
# test.
testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE

familiar:::test_plots(
  plot_function = familiar::plot_shap_summary,
  data_element = "shap",
  shap_max_iterations = 10L,
  outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
  debug = debug_flag,
  not_available_no_samples = FALSE
)


# Swarm plot -------------------------------------------------------------------
familiar:::test_plots(
  plot_function = familiar::plot_shap_summary,
  data_element = "shap",
  shap_max_iterations = 10L,
  evaluation_time = c(1.0, 2.0, 3.5),
  test_config = "normal",
  debug = debug_flag
)

# With a limit to the number of features shown.
familiar:::test_plots(
  plot_function = familiar::plot_shap_summary,
  data_element = "shap",
  shap_max_iterations = 10L,
  test_config = "normal",
  plot_args = list("limit_n_features" = 2L),
  debug = debug_flag
)

# Test with single instance
familiar:::test_plots(
  plot_function = familiar::plot_shap_summary,
  data_element = "shap",
  shap_max_iterations = 10L,
  evaluation_time = c(1.0, 2.0, 3.5),
  test_config = "single instance",
  debug = debug_flag
)

# Test with absolute values
familiar:::test_plots(
  plot_function = familiar::plot_shap_summary,
  data_element = "shap",
  shap_max_iterations = 10L,
  evaluation_time = c(1.0, 2.0, 3.5),
  plot_args = list("value_representation" = "abs"),
  test_config = "normal",
  debug = debug_flag
)


# Bar plot ---------------------------------------------------------------------

# By specifying barplot as plot type.
familiar:::test_plots(
  plot_function = familiar::plot_shap_summary,
  data_element = "shap",
  shap_max_iterations = 10L,
  evaluation_time = c(1.0, 2.0, 3.5),
  plot_args = list("plot_type" = "barplot"),
  test_config = "normal",
  debug = debug_flag
)

# By specifying value_representation as abs_mean.
familiar:::test_plots(
  plot_function = familiar::plot_shap_summary,
  data_element = "shap",
  shap_max_iterations = 10L,
  evaluation_time = c(1.0, 2.0, 3.5),
  plot_args = list("value_representation" = "abs_mean"),
  test_config = "normal",
  debug = debug_flag
)

# By specifying value_representation as abs_min.
familiar:::test_plots(
  plot_function = familiar::plot_shap_summary,
  data_element = "shap",
  shap_max_iterations = 10L,
  evaluation_time = c(1.0, 2.0, 3.5),
  plot_args = list("value_representation" = "abs_min"),
  test_config = "normal",
  debug = debug_flag
)

# By specifying value_representation as abs_max.
familiar:::test_plots(
  plot_function = familiar::plot_shap_summary,
  data_element = "shap",
  shap_max_iterations = 10L,
  evaluation_time = c(1.0, 2.0, 3.5),
  plot_args = list("value_representation" = "abs_max"),
  test_config = "normal",
  debug = debug_flag
)


# Box plot ---------------------------------------------------------------------
familiar:::test_plots(
  plot_function = familiar::plot_shap_summary,
  data_element = "shap",
  shap_max_iterations = 10L,
  evaluation_time = c(1.0, 2.0, 3.5),
  plot_args = list("plot_type" = "boxplot"),
  test_config = "normal",
  debug = debug_flag
)


# Violin plot ------------------------------------------------------------------
familiar:::test_plots(
  plot_function = familiar::plot_shap_summary,
  data_element = "shap",
  shap_max_iterations = 10L,
  evaluation_time = c(1.0, 2.0, 3.5),
  plot_args = list("plot_type" = "violinplot"),
  test_config = "normal",
  debug = debug_flag
)
