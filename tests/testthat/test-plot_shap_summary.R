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


# Test with single samples
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


# Test with absolute values.
familiar:::test_plot_ordering(
  plot_function = familiar::plot_shap_summary,
  data_element = "shap",
  evaluation_time = c(1.0, 2.0, 3.5),
  plot_args = list(
    "value_representation" = "abs",
    "verbose" = FALSE,
    "draw" = debug_flag
  ),
  debug = debug_flag
)


# Bar plot ---------------------------------------------------------------------

# By specifying barplot as plot type..
familiar:::test_plot_ordering(
  plot_function = familiar::plot_shap_summary,
  data_element = "shap",
  evaluation_time = c(1.0, 2.0, 3.5),
  plot_args = list(
    "plot_type" = "barplot",
    "verbose" = FALSE,
    "draw" = debug_flag
  ),
  debug = debug_flag
)

# By specifying value_representation as abs_mean.
familiar:::test_plot_ordering(
  plot_function = familiar::plot_shap_summary,
  data_element = "shap",
  evaluation_time = c(1.0, 2.0, 3.5),
  plot_args = list(
    "value_representation" = "abs_mean",
    "verbose" = FALSE,
    "draw" = debug_flag
  ),
  debug = debug_flag
)

# By specifying value_representation as abs_min.
familiar:::test_plot_ordering(
  plot_function = familiar::plot_shap_summary,
  data_element = "shap",
  evaluation_time = c(1.0, 2.0, 3.5),
  plot_args = list(
    "value_representation" = "abs_min",
    "verbose" = FALSE,
    "draw" = debug_flag
  ),
  debug = debug_flag
)

# By specifying value_representation as abs_max.
familiar:::test_plot_ordering(
  plot_function = familiar::plot_shap_summary,
  data_element = "shap",
  evaluation_time = c(1.0, 2.0, 3.5),
  plot_args = list(
    "value_representation" = "abs_max",
    "verbose" = FALSE,
    "draw" = debug_flag
  ),
  debug = debug_flag
)

# Box plot ---------------------------------------------------------------------
familiar:::test_plot_ordering(
  plot_function = familiar::plot_shap_summary,
  data_element = "shap",
  evaluation_time = c(1.0, 2.0, 3.5),
  plot_args = list(
    "plot_type" = "boxplot",
    "verbose" = FALSE,
    "draw" = debug_flag
  ),
  debug = debug_flag
)


# Violin plot ------------------------------------------------------------------
familiar:::test_plot_ordering(
  plot_function = familiar::plot_shap_summary,
  data_element = "shap",
  evaluation_time = c(1.0, 2.0, 3.5),
  plot_args = list(
    "plot_type" = "violinplot",
    "verbose" = FALSE,
    "draw" = debug_flag
  ),
  debug = debug_flag
)
