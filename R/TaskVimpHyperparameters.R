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


# .perform_task (vimp task, dataObject) ----------------------------------------
setMethod(
  ".perform_task",
  signature(
    object = "familiarTaskVimp",
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
    ...
  ) {
    
    logger_message(
      paste0(
        "\nVariable importance: starting variable importance computation using the \"",
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
    
    # TODO: preprocess data
    
    # Check and retrieve hyperparameters.
    hyperparameters <- .get_hyperparameters(
      object = object,
      hyperparameters = hyperparameters,
      data = data,
      settings = settings,
      message_indent = message_indent,
      verbose = verbose,
      cl = cl,
      ...
    )
    
    # Create the variable importance method object or familiar model object to
    # compute variable importance with.
    vimp_object <- methods::new(
      "familiarVimpMethod",
      outcome_type = data@outcome_type,
      hyperparameters = hyperparameters,
      vimp_method = object@vimp_method,
      outcome_info = data@outcome_info,
      run_table = object@run_table
    )
    
    # Promote to the correct subclass.
    vimp_object <- promote_vimp_method(object = vimp_object)
    
    # Set multivariate methods.
    if (is(vimp_object, "familiarModel")) is_multivariate <- TRUE
    if (is(vimp_object, "familiarVimpMethod")) is_multivariate <- vimp_object@multivariate
    
    # Find required features. Exclude the signature features at this point, as
    # these will have been dropped from the variable importance table.
    required_features <- get_required_features(
      x = data,
      feature_info_list = feature_info_list,
      exclude_signature = !is_multivariate
    )
    
    # Limit to required features.
    vimp_object@required_features <- required_features
    vimp_object@feature_info <- feature_info_list[required_features]
    
    # Compute variable importance.
    vimp_table <- .vimp(
      object = vimp_object, 
      data = data
    )
    
    if (!is.na(object@file)) {
      saveRDS(vimp_table, file = object@file)
    } else {
      return(vimp_table)
    }
    
    return(invisible(TRUE))
  }
)
