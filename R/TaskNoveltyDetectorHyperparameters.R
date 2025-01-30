#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL



# familiarTaskNoveltyDetectorHyperparameters -----------------------------------
setClass(
  "familiarTaskNoveltyDetectorHyperparameters",
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
    task_name = "set_novelty_detector_hyperparameters"
  )
)



# .get_task_descriptor (novelty detector hyperparameters task) -----------------
setMethod(
  ".get_task_descriptor",
  signature(object = "familiarTaskNoveltyDetectorHyperparameters"),
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



# .perform_task (novelty detector hyperparameters task , NULL) -----------------
setMethod(
  ".perform_task",
  signature(
    object = "familiarTaskNoveltyDetectorHyperparameters",
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


# .perform_task (novelty detectors hyperparameters task, dataObject) -----------
setMethod(
  ".perform_task",
  signature(
    object = "familiarTaskNoveltyDetectorHyperparameters",
    data = "dataObject"
  ),
  function(
    object,
    data,
    vimp_aggregation_method = NULL,
    vimp_rank_threshold = NULL,
    selected_features = NULL,
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
        object@learner, "\" novelty detector with the \"",
        object@vimp_method, "\" variable importance method for run ",
        object@task_id, " of ",
        object@n_tasks, "."
      ),
      indent = message_indent,
      verbose = verbose
    )
    
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
    
    # If selected features are not provided, attempt to set the variable
    # importance table.
    if (is.null(selected_features)) {
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
    }
    
    # Get user-provided hyperparameters.
    if (is.null(hyperparameters)) {
      hyperparameters <- settings$mb$detector_parameters[[object@learner]]
      
    } else if (rlang::is_bare_list(hyperparameters)) {
      if (object@learner %in% names(hyperparameters)) {
        hyperparameters <- hyperparameters[[object@learner]]
      }
    }
    
    # Set vimp aggregation method and vimp_rank_threshold based on settings.
    if (!is.null(settings)) {
      if (is.null(vimp_aggregation_method)) {
        vimp_aggregation_method <- settings$vimp$aggregation
      }
      if (is.null(vimp_rank_threshold)) {
        vimp_rank_threshold <- settings$vimp$aggr_rank_threshold
      }
    }
    
    # Create the novelty detector object to set hyperparameters.
    hyperparameter_object <- promote_detector(
      object = methods::new(
        "familiarNoveltyDetector",
        hyperparameters = NULL,
        learner = object@learner,
        vimp_method = object@vimp_method,
        vimp_table = vimp_table,
        vimp_aggregation_method = vimp_aggregation_method,
        vimp_rank_threshold = vimp_rank_threshold,
        feature_info = feature_info_list,
        data_id = object@data_id,
        run_id = object@run_id,
        run_table = .get_current_run_table(object = object),
        project_id = object@project_id
      )
    )
    
    # Find required features.
    required_features <- get_required_features(
      x = feature_info_list,
      features = selected_features,
      exclude_signature = FALSE,
      exclude_novelty = FALSE
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
    
    return(TRUE)
  }
)
