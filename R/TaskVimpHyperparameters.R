#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL



# familiarTaskVimpHyperparameters ----------------------------------------------
setClass(
  "familiarTaskVimpHyperparameters",
  contains = "familiarTask",
  slots = list(
    "vimp_method" = "character",
    "feature_info_file" = "character"
  ),
  prototype = methods::prototype(
    vimp_method = NA_character_,
    feature_info_file = NA_character_,
    task_name = "set_variable_importance_hyperparameters"
  )
)



# .set_file_name (vimp hyperparameters task) -----------------------------------
setMethod(
  ".set_file_name",
  signature(object = "familiarTaskVimpHyperparameters"),
  function(object, file_paths = NULL) {
    if (is.null(file_paths)) return(object)
    
    # Generate file name of variable importance table
    object@file <- get_object_file_name(
      object_type = "hyperparametersVimp",
      data_id = object@data_id,
      run_id = object@run_id,
      vimp_method = object@vimp_method,
      project_id = object@project_id,
      dir_path = file_paths$vimp_dir
    )
    
    return(object)
  }
)



# .get_task_descriptor (vimp hyperparameters task) -----------------------------
setMethod(
  ".get_task_descriptor",
  signature(object = "familiarTaskVimpHyperparameters"),
  function(object, ...) {
    return(paste0(object@task_name, "_", object@data_id, "_", object@run_id, "_", object@vimp_method))
  }
)



# .perform_task (vimp hyperparameters task , NULL) -----------------------------
setMethod(
  ".perform_task",
  signature(
    object = "familiarTaskVimpHyperparameters",
    data = "NULL"
  ),
  function(
    object,
    data,
    experiment_data = NULL,
    outcome_info = NULL,
    ...
  ) {
    # This method is called when "data" is expected to be available somewhere in
    # the backend.
    
    if (is.null(experiment_data)) {
      ..error_reached_unreachable_code("experiment_data is required for retrieving data from the backend.")
    }
    if (is.null(outcome_info)) {
      ..error_reached_unreachable_code("outcome_info is required.")
    }
    
    # Find the run list.
    run_list <- .get_run_list(
      iteration_list = experiment_data@iteration_list,
      data_id = object@data_id,
      run_id = object@run_id
    )
    
    # Select unique samples.
    sample_identifiers <- .get_sample_identifiers(
      run = run_list,
      train_or_validate = "train"
    )
    sample_identifiers <- unique(sample_identifiers)
    
    # Create a dataObject.
    data <- methods::new(
      "dataObject",
      data = get_data_from_backend(sample_identifiers = sample_identifiers),
      preprocessing_level = "none",
      outcome_type = outcome_info@outcome_type,
      outcome_info = outcome_info
    )
    
    # Pass to method that dispatches with dataObject for further processing.
    return(.perform_task(
      object = object,
      data = data,
      ...
    ))
  }
)


# .perform_task (vimp hyperparameters task, dataObject) ------------------------
setMethod(
  ".perform_task",
  signature(
    object = "familiarTaskVimpHyperparameters",
    data = "dataObject"
  ),
  function(
    object,
    data,
    settings = NULL,
    feature_info_list = NULL,
    hyperparameters = NULL,
    message_indent = 0L,
    verbose = FALSE,
    cl = NULL,
    return_results = TRUE,
    ...
  ) {
    logger_message(
      paste0(
        "Hyperparameter optimisation: Starting variable importance computation using the \"",
        object@vimp_method, "\" method for run ",
        object@task_id, " of ",
        object@n_tasks, "."
      ),
      indent = message_indent,
      verbose = verbose
    )
    
    # Check that outcome_info is present on data
    if (!is(data@outcome_info, "outcomeInfo")) {
      ..error_reached_unreachable_code(
        "outcome_info attribute of data (dataObject) does not contain an outcomeInfo object"
      )
    }
    
    # Check and retrieve feature info list.
    feature_info_list <- .get_feature_info_list(
      object = object,
      feature_info_list = feature_info_list,
      data = data,
      settings = settings,
      message_indent = message_indent,
      verbose = verbose,
      cl = cl,
      ...
    )
    
    # Get user-provided hyperparameters.
    if (is.null(hyperparameters)) {
      hyperparameters <- settings$vimp$param[[object@vimp_method]]
      
    } else if (rlang::is_bare_list(hyperparameters)) {
      if (object@vimp_method %in% names(hyperparameters)) {
        hyperparameters <- hyperparameters[[object@vimp_method]]
      }
    }
    
    hyperparameter_object <- promote_vimp_method(
      object = methods::new(
        "familiarVimpMethod",
        outcome_type = data@outcome_type,
        hyperparameters = NULL,
        vimp_method = object@vimp_method,
        outcome_info = data@outcome_info,
        run_table = object@run_table,
        project_id = object@project_id
      )
    )
    
    # Set multivariate methods.
    if (is(hyperparameter_object, "familiarModel")) is_multivariate <- TRUE
    if (is(hyperparameter_object, "familiarVimpMethod")) is_multivariate <- hyperparameter_object@multivariate
    
    # Find required features.
    required_features <- get_required_features(
      x = feature_info_list,
      exclude_signature = !is_multivariate
    )
    
    # Limit to required features. This removes signature features which are not
    # assessed through variable importance.
    feature_info_list <- feature_info_list[required_features]
    hyperparameter_object@required_features <- required_features
    hyperparameter_object@feature_info <- feature_info_list
    
    # Make sure the input data is processed.
    data <- process_input_data(
      object = hyperparameter_object,
      data = data
    )
    
    # Compute hyperparameters. Function arguments to optimise_hyperparameters
    # are passed from the calling function.
    hyperparameter_object <- optimise_hyperparameters(
      object = hyperparameter_object,
      data = data,
      user_list = hyperparameters,
      verbose = verbose,
      message_indent = message_indent + 1L,
      save_in_place = FALSE,
      is_vimp = TRUE,
      ...
    )
    
    # Set familiar version.
    hyperparameter_object <- add_package_version(hyperparameter_object)
    
    if (!is.na(object@file)) {
      saveRDS(hyperparameter_object, file = object@file)
    }
    
    if (return_results) {
      return(hyperparameter_object)
    }
    
    return(invisible(TRUE))
  }
)


..run_variable_importance_computation_hyperparameters <- function(
    tasks,  
    settings,
    cl,
    message_indent = 0L,
    verbose,
    ...
) {
  logger_message(
    paste0(
      "Hyperparameter optimisation: Starting parameter optimisation for variable importance methods."
    ),
    indent = message_indent,
    verbose = verbose
  )
  
  # Determine how parallel processing takes place.
  if (settings$hpo$do_parallel %in% c("TRUE", "inner")) {
    cl_inner <- cl
    cl_outer <- NULL
    
  } else if (settings$hpo$do_parallel %in% c("outer")) {
    cl_inner <- NULL
    cl_outer <- cl
    
    if (!is.null(cl_outer)) {
      logger_message(
        paste0(
          "Hyperparameter optimisation: Load-balanced parallel processing ",
          "is done in the outer loop. No progress can be displayed."
        ),
        indent = message_indent,
        verbose = verbose
      )
    }
    
  } else {
    cl_inner <- cl_outer <- NULL
  }
  
  # Iterate over data subsets for which parameters have not yet been set.
  fam_mapply_lb(
    cl = cl_outer,
    assign = "all",
    FUN = .perform_task,
    progress_bar = !is.null(cl_outer),
    object = tasks,
    MoreArgs = list(
      "cl" = cl_inner,
      "data" = NULL,
      "settings" = settings,
      "metric" = settings$hpo$hpo_metric,
      "hyperparameters" = settings$vimp$param,
      "optimisation_function" = settings$hpo$hpo_optimisation_function,
      "acquisition_function" = settings$hpo$hpo_acquisition_function,
      "grid_initialisation_method" = settings$hpo$hpo_grid_initialisation_method,
      "n_random_sets" = settings$hpo$hpo_n_grid_initialisation_samples,
      "exploration_method" = settings$hpo$hpo_exploration_method,
      "determine_vimp" = settings$hpo$hpo_determine_vimp,
      "measure_time" = TRUE,
      "hyperparameter_learner" = settings$hpo$hpo_hyperparameter_learner,
      "n_max_bootstraps" = settings$hpo$hpo_max_bootstraps,
      "n_initial_bootstraps" = settings$hpo$hpo_initial_bootstraps,
      "n_intensify_step_bootstraps" = settings$hpo$hpo_bootstraps,
      "n_max_optimisation_steps" = settings$hpo$hpo_smbo_iter_max,
      "n_max_intensify_steps" = settings$hpo$hpo_intensify_max_iter,
      "intensify_stop_p_value" = settings$hpo$hpo_alpha,
      "convergence_tolerance" = settings$hpo$hpo_convergence_tolerance,
      "convergence_stopping" = settings$hpo$hpo_conv_stop,
      "time_limit" = settings$hpo$hpo_time_limit,
      "message_indent" = message_indent + 1L,
      "verbose" = verbose && is.null(cl_outer),
      "return_results" = FALSE,
      ...
    )
  )
  
  logger_message(
    paste0(
      "Hyperparameter optimisation: Completed parameter optimisation for variable importance methods.",
      "\n"
    ),
    indent = message_indent,
    verbose = verbose
  )
  
  return(invisible(TRUE))
}
