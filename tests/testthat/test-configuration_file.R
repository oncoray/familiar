# xml2 is required to .
if (!rlang::is_installed("xml2")) testthat::skip()

# Find path to configuration file in package.
config <- system.file("config.xml", package = "familiar")

# Load the configuration file
config <- familiar:::.load_configuration_file(config)

# Check whether the main branches have the correct names.
# Find names of parent nodes.
config_node_names <- names(config)
expected_node_names <- familiar:::.get_all_configuration_parent_node_names()

testthat::test_that(
  "1. All parent nodes are present in the configuration file.", 
  {
    testthat::expect_setequal(config_node_names, expected_node_names)
  }
)

testthat::test_that(
  "2. All parameters are specified.",
  {
    config_args <- unique(unlist(sapply(config, names)))
    testthat::expect_setequal(config_args, familiar:::.get_all_parameter_names())
  }
)

for (parent_node in expected_node_names) {
  # Identify the parsing function for the node.
  parse_fun <- switch(parent_node,
    paths = familiar:::.parse_file_paths,
    data = familiar:::.parse_experiment_settings,
    run = familiar:::.parse_setup_settings,
    preprocessing = familiar:::.parse_preprocessing_settings,
    variable_importance = familiar:::.parse_variable_importance_settings,
    model_development = familiar:::.parse_model_development_settings,
    hyperparameter_optimisation = familiar:::.parse_hyperparameter_optimisation_settings,
    evaluation = familiar:::.parse_evaluation_settings
  )

  # Find the expected arguments.
  expected_config_args <- names(as.list(args(parse_fun)))

  # Remove "" and "..." and other arguments that are not parameters that can be
  # specified using ... .
  expected_config_args <- intersect(expected_config_args, familiar:::.get_all_parameter_names())

  # Remove specific arguments unless they are shared by a specific function.
  if (parent_node != "data") {
    expected_config_args <- setdiff(
      expected_config_args, c("outcome_type", "development_batch_id")
    )
  }

  if (parent_node != "run") {
    expected_config_args <- setdiff(
      expected_config_args, "parallel"
    )
  }

  if (parent_node != "variable_importance") {
    expected_config_args <- setdiff(
      expected_config_args,
      c("vimp_aggregation_rank_threshold", "vimp_aggregation_method")
    )
  }

  # Find the argument names.
  config_args <- names(config[[parent_node]])

  testthat::test_that(
    paste0("3. All parameters are specified for the \"", parent_node, "\" node."),
    {
      testthat::expect_setequal(config_args, expected_config_args)
    }
  )
}
