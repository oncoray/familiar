# familiarTaskVimp -------------------------------------------------------------
setClass(
  "familiarTaskVimp",
  contains = "familiarTask",
  slots = list(
    "vimp_method" = "character",
    "hyperparameter_file" = "character",
    "feature_info_file" = "character",
    "run_table" = "ANY"
  ),
  prototype = methods::prototype(
    vimp_method = NA_character_,
    hyperparameter_file = NA_character_,
    feature_info_file = NA_character_,
    run_table = NULL,
    task_name = "compute_variable_importance"
  )
)



# .set_file_name (vimp task) ---------------------------------------------------
setMethod(
  ".set_file_name",
  signature(object = "familiarTaskVimp"),
  function(object, file_paths = NULL) {
    if (is.null(file_paths)) return(object)
    
    # Generate file name of variable importance table
    object@file <- get_object_file_name(
      object_type = "vimpTable",
      project_id = object@project_id,
      dir_path = file_paths$vimp_dir
    )
    
    return(object)
  }
)



# .get_task_descriptor (vimp task) ---------------------------------------------
setMethod(
  ".get_task_descriptor",
  signature(object = "familiarTaskVimp"),
  function(object, ...) {
    return(paste0(object@task_name, "_", object@data_id, "_", object@run_id, "_", object@vimp_method))
  }
)



# .perform_task (vimp task , NULL) ---------------------------------------------
setMethod(
  ".perform_task",
  signature(
    object = "familiarTaskVimp",
    data = "NULL"
  ),
  function(
    object,
    data,
    project_info = NULL,
    outcome_info = NULL,
    ...
  ) {
    # This method is called when "data" is expected to be available somewhere in
    # the backend.
    
    if (is.null(project_info)) {
      ..error_reached_unreachable_code("project_info is required for retrieving data from the backend.")
    }
    if (is.null(outcome_info)) {
      ..error_reached_unreachable_code("outcome_info is required.")
    }
    
    # Find the run list.
    run_list <- .get_run_list(
      iteration_list = project_info$iter_list,
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
    
    # Check and retrieve hyperparameters.
    hyperparameters <- .get_hyperparameters(
      object = object,
      hyperparameters = hyperparameters,
      data = data
    )
    
    if (is.null(hyperparameters) && is.na(object@hyperparameter_file)) {
      # Check that a list of hyperparameters is provided, otherwise create an ad-
      # hoc list of hyperparameters.
      browser()
      
    } else if (is.null(hyperparameters)) {
      # Assume that the hyperparameter file attribute contains the path to the
      # file containing hyperparameters.
      
    } else if (is.character(hyperparameters)) {
      # If hyperparameters is a string, interpret this as a path to file
      # containing the feature info.
      
    }
    
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



# .get_feature_info_list (vimp task) -------------------------------------------
setMethod(
  ".get_feature_info_list",
  signature(object = "familiarTaskVimp"),
  function(object, feature_info_list, ...) {
    # Suppress NOTES due to non-standard evaluation in data.table
    can_pre_process <- NULL
    
    # Attempt to get the feature info list from the backend.
    if (is.null(feature_info_list) && !is.null(object@run_table)) {
      # Find the last entry that is available for pre-processing
      pre_processing_run <- tail(object@run_table[can_pre_process == TRUE, ], n = 1L)
      
      feature_info_list <- tryCatch(
        get_feature_info_from_backend(
          data_id = pre_processing_run$data_id[1L],
          run_id = pre_processing_run$run_id[1L]
        ),
        error = NULL
      )
    }
    
    # If no feature list is present on the backend, check other options.
    if (is.null(feature_info_list) && is.na(object@feature_info_file)) {
      # Check that a feature info list is provided, otherwise create an ad-hoc
      # list as an template.
      
      # Set up task, and explicitly don't write to file.
      generic_feature_info_task <- methods::new(
        "familiarTaskFeatureInfo",
        project_id = object@project_id,
        file = NA_character_
      )
      
      # Execute the task.
      feature_info_list <- .perform_task(
        object = generic_feature_info_task,
        ...
      )
      
    } else if (is.null(feature_info_list)) {
      # Assume that the feature info file attribute contains the path to the
      # file containing feature info.
      if (!file.exists(object@feature_info_file)) {
        ..error(paste0("feature info file does not exist at location: ", object@feature_info_file))
      }
      feature_info_list <- readRDS(object@feature_info_file)
      feature_info_list <- update_object(feature_info_list)
      
    } else if (is.character(feature_info_list)) {
      # If the feature info list is a string, interpret this as a path to the
      # file containing the feature info.
      if (!file.exists(feature_info_list)) {
        ..error(paste0("feature info file does not exist at location: ", feature_info_list))
      }
      feature_info_list <- readRDS(feature_info_list)
      feature_info_list <- update_object(feature_info_list)
    }
    
    if (is.null(feature_info_list)) {
      ..error("no feature info objects were found.")
    }
    
    return(feature_info_list)
  }
)



setMethod(
  ".get_hyperparameters",
  signature(object = "familiarTaskVimp"),
  function(object, hyperparameters, ...) {
    
    if (is.null(hyperparameters) && is.na(object@hyperparameter_file)) {
      
      
      
    } else if (is.null(hyperparameters)) {
      # Identify the right entry on hpo_list.
      for (ii in rev(run$run_table$perturb_level)) {
        run_id_list <- .get_iteration_identifiers(
          run = run, 
          perturb_level = ii
        )
        
        # Check whether there are any matching data and run ids by determining the
        # number of rows in the table after matching
        match_hpo <- sapply(
          hpo_list,
          function(iter_hpo, run_id_list) {
            # Determine if there are any rows in the run_table of the parameter list
            # that match the data and run identifiers of the current level.
            match_size <- nrow(iter_hpo@run_table[data_id == run_id_list$data & run_id == run_id_list$run])
            
            # Return TRUE if any matching rows are found.
            return(match_size > 0L)
          },
          run_id_list = run_id_list
        )
        
        # If there is a match, we step out of the loop
        if (any(match_hpo)) break
      }
      
      # Extract the table of parameters
      if (allow_random_selection && sum(match_hpo) > 1L) {
        random_set <- sample(which(match_hpo), size = 1L)
        
        object <- hpo_list[[random_set]]
        
      } else {
        object <- hpo_list[match_hpo][[1L]]
      }
      
      if (as_list) {
        return(object@hyperparameters)
      } else {
        return(object)
      }
    }
  }
)



.generate_vimp_tasks <- function(
    file_paths,
    project_id
) {
  
  task_list <- list()
  
  # Check if vimp should be computed separately or is computed during 
  # hyperparameter optimisation.
  
  for (data_id in data_ids) {
    for (run_id in run_ids) {
      for (vimp_method in vimp_methods) {
        
        # Check if the variable importance method requires any computation.
        # For example, signature_only, none and random do not require
        # computation.
        
        # Set up variable importance computation task.
        
        # Set up variable importance hyperparameter task.
        
      }
    }
  }
  
  # Check if any vimp-related tasks are required.
  if (length(task_list) == 0L) return(NULL)
  
  # Add tasks related to data processing for vimp methods.
  task_list <- c(
    task_list, 
    .generate_vimp_data_preprocessing_tasks(
      file_paths = file_paths,
      project_id = project_id
    )
  )
  
  return(task_list)
}
