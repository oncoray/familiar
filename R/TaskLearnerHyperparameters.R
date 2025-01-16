#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL



# familiarTaskLearnerHyperparameters -------------------------------------------
setClass(
  "familiarTaskLearnerHyperparameters",
  contains = "familiarTask",
  slots = list(
    "learner" = "character",
    "vimp_method" = "character",
    "use_vimp" = "character",
    "feature_info_file" = "character",
    "vimp_table_file" = "character"
  ),
  prototype = methods::prototype(
    learner = NA_character_,
    vimp_method = NA_character_,
    use_vimp = "use_hpo_vimp",
    feature_info_file = NA_character_,
    vimp_table_file = NA_character_,
    task_name = "set_learner_hyperparameters"
  )
)



# .set_file_name (learner hyperparameters task) --------------------------------
setMethod(
  ".set_file_name",
  signature(object = "familiarTaskLearnerHyperparameters"),
  function(object, file_paths = NULL) {
    if (is.null(file_paths)) return(object)
    
    # Generate file name of variable importance table
    object@file <- get_object_file_name(
      object_type = "hyperparametersLearner",
      data_id = object@data_id,
      run_id = object@run_id,
      learner = object@learner,
      vimp_method = object@vimp_method,
      project_id = object@project_id,
      dir_path = file_paths$mb_dir
    )
    
    return(object)
  }
)



# .get_task_descriptor (learner hyperparameters task) --------------------------
setMethod(
  ".get_task_descriptor",
  signature(object = "familiarTaskLearnerHyperparameters"),
  function(object, ...) {
    return(paste0(
      object@task_name, "_",
      object@data_id, "_",
      object@run_id, "_",
      object@vimp_method, "_",
      object@learner
    ))
  }
)



# .perform_task (learner hyperparameters task , NULL) --------------------------
setMethod(
  ".perform_task",
  signature(
    object = "familiarTaskLearnerHyperparameters",
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
      experiment_data = experiment_data,
      ...
    ))
  }
)


# .perform_task (learner hyperparameters task, dataObject) ---------------------
setMethod(
  ".perform_task",
  signature(
    object = "familiarTaskLearnerHyperparameters",
    data = "dataObject"
  ),
  function(
    object,
    data,
    settings = NULL,
    feature_info_list = NULL,
    vimp_table = NULL,
    hyperparameters = NULL,
    message_indent = 0L,
    verbose = FALSE,
    cl = NULL,
    return_results = TRUE,
    ...
  ) {
    logger_message(
      paste0(
        "Hyperparameter optimisation: Starting hyperparameter optimisation for the \"",
        object@learner, "\" learner with the \"",
        object@vimp_method, "\" variable importance method for run ",
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

    # Check and retrieve variable importances.
    if (object@use_vimp == "use_main_vimp") {
      vimp_table <- .get_variable_importance_table(
        object = object,
        vimp_table = vimp_table,
        feature_info_list = feature_info_list,
        data = data,
        settings = settings,
        message_indent = message_indent,
        verbose = verbose,
        cl = cl,
        ...
      )
      
    } else if (object@use_vimp %in% c("use_hpo_vimp", "return_hpo_vimp")) {
      vimp_table <- NULL
      
    } else {
      ..error_reached_unreachable_code(paste0("use_vimp attribute has an unrecognised "))
    }
    
    # Get user-provided hyperparameters.
    if (is.null(hyperparameters)) {
      hyperparameters <- settings$mb$hyper_param[[object@learner]]
      
    } else if (rlang::is_bare_list(hyperparameters)) {
      if (object@learner %in% names(hyperparameters)) {
        hyperparameters <- hyperparameters[[object@learner]]
      }
    }
    
    # Create a model object to set hyperparameters.
    hyperparameter_object <- promote_learner(
      object = methods::new(
        "familiarModel",
        outcome_type = data@outcome_type,
        hyperparameters = NULL,
        learner = object@learner,
        vimp_method = object@vimp_method,
        vimp_table = vimp_table,
        outcome_info = data@outcome_info,
        run_table = .get_current_run_table(object = object),
        settings = settings,
        project_id = object@project_id
      )
    )
    
    # Find required features.
    required_features <- get_required_features(
      x = feature_info_list,
      exclude_signature = FALSE
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
      is_vimp = FALSE,
      ...
    )
    
    if (object@use_vimp != "return_hpo_vimp") {
      # Variable importance should not be passed using the hyperparameter
      # object, which means that any subsequent learner task will use the cached
      # variable importance tables instead.
      hyperparameter_object@vimp_table <- NULL
    }
    
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



..run_learner_computation_hyperparameters <- function(
    tasks,  
    settings,
    cl,
    message_indent = 0L,
    verbose,
    ...
) {
  logger_message(
    paste0(
      "Hyperparameter optimisation: Starting parameter optimisation for learners."
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
      "vimp_aggregation_method" = settings$vimp$aggregation,
      "vimp_rank_threshold" = settings$vimp$aggr_rank_threshold,
      "metric" = settings$hpo$hpo_metric,
      "hyperparameters" = settings$mb$param,
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
      "Hyperparameter optimisation: Completed parameter optimisation for learners.",
      "\n"
    ),
    indent = message_indent,
    verbose = verbose
  )
  
  return(invisible(TRUE))
}
