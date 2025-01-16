#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL



# familiarTaskTrainNovelty -----------------------------------------------------
setClass(
  "familiarTaskTrainNovelty",
  contains = "familiarTask",
  slots = list(
    "vimp_method" = "character",
    "learner" = "character",
    "vimp_table_file" = "character",
    "hyperparameter_file" = "character",
    "feature_info_file" = "character"
  ),
  prototype = methods::prototype(
    vimp_method = "none",
    learner = NA_character_,
    vimp_table_file = NA_character_,
    hyperparameter_file = NA_character_,
    feature_info_file = NA_character_,
    task_name = "train_novelty_detector"
  )
)



# .get_task_descriptor (train task) --------------------------------------------
setMethod(
  ".get_task_descriptor",
  signature(object = "familiarTaskTrainNovelty"),
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



# .perform_task (train task , NULL) --------------------------------------------
setMethod(
  ".perform_task",
  signature(
    object = "familiarTaskTrainNovelty",
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


# .perform_task (train task, dataObject) ---------------------------------------
setMethod(
  ".perform_task",
  signature(
    object = "familiarTaskTrainNovelty",
    data = "dataObject"
  ),
  function(
    object,
    data,
    selected_features = NULL,
    vimp_aggregation_method = NULL,
    vimp_rank_threshold = NULL,
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
        "Training: Starting training for the \"", object@learner,
        "\" out-of-distribution detector and the \"", object@vimp_method,
        "\" variable importance method for run ",
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
    
    # Set vimp aggregation method and vimp_rank_threshold based on settings.
    if (!is.null(settings)) {
      if (is.null(vimp_aggregation_method)) {
        vimp_aggregation_method <- settings$vimp$aggregation
      }
      if (is.null(vimp_rank_threshold)) {
        vimp_rank_threshold <- settings$vimp$aggr_rank_threshold
      }
    }
    
    # Check and retrieve hyperparameters. We do this prior to retrieving the
    # variable importance tables, as these may be attached to hyperparameter
    # object.
    hyperparameters <- .get_hyperparameters(
      object = object,
      selected_features = selected_features,
      hyperparameters = hyperparameters,
      vimp_aggregation_method = vimp_aggregation_method,
      vimp_rank_threshold = vimp_rank_threshold,
      feature_info_list = feature_info_list,
      data = data,
      settings = settings,
      message_indent = message_indent,
      verbose = verbose,
      cl = cl,
      ...
    )
    
    # If selected features are not provided, create the variable importance
    # table.
    if (is.null(selected_features)) {
      if (is_empty(hyperparameters$vimp_table)) {
        # Check and retrieve variable importances from the drive, or generate in
        # place, if the hyperparameter object did not contain a variable
        # importance table.
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
        
      } else {
        vimp_table <- hyperparameters$vimp_table
      }
      
      if (!is.null(feature_info_list)) {
        # Update using reference cluster table to ensure that the data are correct
        # locally. This clustering table can be derived from the provided feature
        # info list.
        vimp_table <- update_vimp_table_to_reference(
          x = vimp_table,
          reference_cluster_table = .create_clustering_table(
            feature_info_list = feature_info_list
          )
        )
      }
      
      # Recluster the data according to the clustering table corresponding to the
      # model. This ensures that the variable importance table has the features
      # that are seen by the model.
      vimp_table <- recluster_vimp_table(vimp_table)
      
      # Get aggregate variable importances
      vimp_table <- aggregate_vimp_table(
        vimp_table,
        aggregation_method = vimp_aggregation_method,
        rank_threshold = vimp_rank_threshold
      )
      
    } else {
      vimp_table <- NULL
    }
    
    # Create the raw model object for training.
    model_object <- methods::new(
      "familiarNoveltyDetector",
      hyperparameters = hyperparameters$hyperparameters,
      learner = object@learner,
      vimp_method = object@vimp_method,
      vimp_table = vimp_table,
      feature_info = feature_info_list,
      data_id = object@data_id,
      run_id = object@run_id,
      run_table = .get_current_run_table(object = object),
      project_id = object@project_id
    )

    # Promote to the correct type of detector.
    model_object <- promote_detector(object = model_object)
    
    # Select features based on variable importances.
    model_object <- set_signature(
      object = model_object,
      minimise_footprint = FALSE,
      signature_features = selected_features
    )
    
    # Process data, if required.
    data <- process_input_data(
      object = model_object,
      data = data,
      stop_at = "clustering",
      force_check = TRUE
    )
    
    # Train the novelty detector.
    model_object <- .train(
      object = model_object,
      data = data,
      get_additional_info = TRUE,
      ...
    )
    
    # Add model name
    model_object <- set_object_name(model_object)
    
    if (!is.na(object@file)) {
      saveRDS(model_object, file = object@file)
    }
    
    if (return_results) {
      return(model_object)
    }
    
    return(invisible(TRUE))
  }
)



# .get_hyperparameters (train task) --------------------------------------------
setMethod(
  ".get_hyperparameters",
  signature(object = "familiarTaskTrainNovelty"),
  function(
    object,
    hyperparameters,
    file_paths = NULL,
    ...
  ) {
    # Suppress NOTES due to non-standard evaluation in data.table
    can_pre_process <- NULL
    
    hyperparameter_object <- NULL
    if (is.null(hyperparameters) && !is.null(object@run_table)) {
      # This routine loads hyperparameters from disk, and is used when an
      # experiment is run using summon_familiar.
      
      # This check exists to make sure that the standard workflow passes the
      # correct objects.
      if (is.null(file_paths)) {
        ..error_reached_unreachable_code("file_paths was expected, but not provided.")
      }
      
      # Find the last entry on the run table that is marked as available for
      # pre-processing. This is what hyperparameters are based on.
      hyperparameter_run <- tail(
        object@run_table[[paste0(object@data_id, ".", object@run_id)]][can_pre_process == TRUE, ],
        n = 1L
      )
      
      # Find the file name.
      hyperparameter_file <- get_object_file_name(
        project_id = object@project_id,
        data_id = hyperparameter_run$data_id[1L],
        run_id = hyperparameter_run$run_id[1L],
        learner = object@learner,
        vimp_method = object@vimp_method,
        object_type = "hyperparametersNoveltyDetector",
        dir_path = file_paths$mb_dir
      )
      
      if (file.exists(hyperparameter_file)) {
        hyperparameter_object <- update_object(readRDS(hyperparameter_file))
      }
    }
    
    
    if (is.null(hyperparameter_object) && is.na(object@hyperparameter_file)) {
      # Create an ad-hoc list of hyperparameters
      
      # Set up task, and explicitly don't write to file.
      hyperparameter_task <- methods::new(
        "familiarTaskNoveltyDetectorHyperparameters",
        project_id = object@project_id,
        vimp_method = object@vimp_method,
        learner = object@learner,
        file = NA_character_
      )
      
      # Execute the task.
      hyperparameter_object <- .perform_task(
        object = hyperparameter_task,
        ...
      )
      
    } else if (is.null(hyperparameter_object)) {
      # Assume that the hyperparameter_file attribute contains the path to the
      # file containing the vimp method hyperparameters.
      if (!file.exists(object@hyperparameter_file)) {
        ..error(paste0("hyperparameter file does not exist at location: ", object@hyperparameter_file))
      }
      hyperparameter_object <- update_object(readRDS(object@hyperparameter_file))
      
    } else if (is.character(hyperparameters)) {
      # If hyperparameters is a string, interpret this as a path to the
      # file containing the vimp method hyperparameters.
      if (!file.exists(hyperparameters)) {
        ..error(paste0("hyperparameter file does not exist at location: ", hyperparameters))
      }
      hyperparameter_object <- update_object(readRDS(hyperparameters))
    }
    
    if (is(hyperparameter_object, "familiarNoveltyDetector")) {
      hyperparameters <- list(
        "hyperparameters" =  hyperparameter_object@hyperparameters,
        "hyperparameter_data" = hyperparameter_object@hyperparameter_data,
        "vimp_table" = hyperparameter_object@vimp_table
      )
      
    } else {
      hyperparameters <- list("hyperparameters" = hyperparameters)
    }
    
    return(hyperparameters)
  }
)
