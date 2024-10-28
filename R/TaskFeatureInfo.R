#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


# familiarTaskGenericFeatureInfo -----------------------------------------------
setClass(
  "familiarTaskGenericFeatureInfo",
  contains = "familiarTask",
  prototype = methods::prototype(
    task_name = "create_generic_feature_info",
    data_id = 1L,
    run_id = 1L
  )
)


# .set_file_name (generic feature info task) -----------------------------------
setMethod(
  ".set_file_name",
  signature(object = "familiarTaskGenericFeatureInfo"),
  function(object, file_paths = NULL) {
    if (is.null(file_paths)) return(object)
    
    # Generate file name of pre-processing file
    file_name <- paste0(object@project_id, "_generic_feature_info.RDS")
    
    # Add file path and normalise according to the OS
    object@file <- normalizePath(
      file.path(file_paths$process_data_dir, file_name),
      mustWork = FALSE
    )
    
    return(object)
  }
)



# .get_task_descriptor (generic feature info task) -----------------------------
setMethod(
  ".get_task_descriptor",
  signature(object = "familiarTaskGenericFeatureInfo"),
  function(object, ...) {
    return(object@task_name)
  }
)



# .perform_task (generic feature info task, NULL) ------------------------------
setMethod(
  ".perform_task",
  signature(
    object = "familiarTaskGenericFeatureInfo",
    data = "NULL"
  ),
  function(
    object,
    data,
    settings = NULL,
    ...
  ) {
    # This method is called when "data" is expected to be available somewhere in
    # the backend.
    
    if (is.null(project_info)) {
      ..error_reached_unreachable_code("project_info is required for retrieving data from the backend.")
    }
    if (is.null(settings)) {
      ..error_reached_unreachable_code("settings is required for retrieving data from the backend.")
    }
    
    # Create a dataObject.
    data <- methods::new(
      "dataObject",
      data = get_data_from_backend(),
      preprocessing_level = "none",
      outcome_type = settings$data$outcome_type
    )
    
    # Pass to .perform_task for dataObject.
    return(.perform_task(
      object = object,
      data = data,
      ...
    ))
  }
)



# .perform_task (generic feature info task, dataObject) ------------------------
setMethod(
  ".perform_task",
  signature(
    object = "familiarTaskGenericFeatureInfo",
    data = "dataObject"
  ),
  function(
    object,
    data,
    descriptor = NULL,
    ...
  ) {
    # Extract basic feature information from the data.
    feature_info_list <- .get_generic_feature_info(
      data = data,
      outcome_type = data@outcome_type,
      descriptor = descriptor
    )
    
    # Write to file or return.
    if (!is.na(file)) {
      saveRDS(feature_info_list, file = object@file)
    } else {
      return(feature_info_list)
    }
    
    return(invisible(TRUE))
  }
)



# familiarTaskFeatureInfo ------------------------------------------------------
setClass(
  "familiarTaskFeatureInfo",
  contains = "familiarTask",
  prototype = methods::prototype(
    task_name = "create_feature_info"
  )
)



# .set_file_name (feature info task) -------------------------------------------
setMethod(
  ".set_file_name",
  signature(object = "familiarTaskFeatureInfo"),
  function(object, file_paths = NULL) {
    if (is.null(file_paths)) return(object)
    
    # Generate file name of pre-processing file.
    file_name <- paste0(
      object@project_id, "_", object@data_id, "_", object@run_id, "_feature_info.RDS"
    )
    
    # Add file path and normalise according to the OS
    object@file <- normalizePath(
      file.path(file_paths$process_data_dir, file_name),
      mustWork = FALSE
    )
    
    return(object)
  }
)



# .get_task_descriptor (feature info task) -------------------------------------
setMethod(
  ".get_task_descriptor",
  signature(object = "familiarTaskFeatureInfo"),
  function(object, ...) {
    return(paste0(object@task_name, "_", object@data_id, "_", object@run_id))
  }
)



# .perform_task (feature info task , NULL) -------------------------------------
setMethod(
  ".perform_task",
  signature(
    object = "familiarTaskFeatureInfo",
    data = "NULL"
  ),
  function(
    object,
    data,
    settings = NULL,
    project_info = NULL,
    ...
  ) {
    # This method is called when "data" is expected to be available somewhere in
    # the backend.
    
    if (is.null(project_info)) {
      ..error_reached_unreachable_code("project_info is required for retrieving data from the backend.")
    }
    if (is.null(settings)) {
      ..error_reached_unreachable_code("settings is required for retrieving data from the backend.")
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
      outcome_type = settings$data$outcome_type
    )
    
    # Pass to method that dispatches with dataObject for further processing.
    return(.perform_task(
      object = object,
      data = data,
      settings = settings,
      ...
    ))
  }
)


# .perform_task (feature info task, dataObject) --------------------------------
setMethod(
  ".perform_task",
  signature(
    object = "familiarTaskFeatureInfo",
    data = "dataObject"
  ),
  function(
    object,
    data,
    settings = NULL,
    feature_info_list = NULL,
    message_indent = 0L,
    verbose = FALSE,
    cl = NULL,
    signature_features = NULL,
    novelty_features = NULL,
    fairness_features = NULL,
    ...
  ) {
    
    logger_message(
      paste0(
        "\nPre-processing: Starting preprocessing for run ",
        object@task_id, " of ",
        object@n_tasks, "."
      ),
      indent = message_indent,
      verbose = verbose
    )
    
    # Check that a feature info list is provided, otherwise create an ad-hoc
    # list as an template.
    if (is.null(feature_info_list)) {
      # Set up task, and explicitly don't write to file.
      generic_feature_info_task <- methods::new(
        "familiarTaskGenericFeatureInfo",
        project_id = object@project_id,
        file = NA_character_
      )
      
      # Execute the task.
      feature_info_list <- .perform_task(
        object = generic_feature_info_task,
        data = data
      )
    }
    
    # Add workflow control info.
    feature_info_list <- add_control_info(
      feature_info_list = feature_info_list,
      data_id = object@data_id,
      run_id = object@run_id
    )
    
    # Add signature feature info.
    if (is.null(signature_features)) signature_features <- settings$data$signature
    feature_info_list <- add_signature_info(
      feature_info_list = feature_info_list,
      signature = signature_features
    )
    
    # Add novelty feature info.
    if (is.null(novelty_features)) novelty_features <- settings$data$novelty_features
    feature_info_list <- add_novelty_info(
      feature_info_list = feature_info_list,
      novelty_features = novelty_features
    )
    
    # Find currently available features.
    available_features <- get_available_features(feature_info_list = feature_info_list)
    
    # Remove unavailable features from the data object.
    data <- filter_features(
      data = data,
      available_features = available_features
    )
    
    # Use data to determine pre-processing parameters.
    feature_info_list <- .determine_preprocessing_parameters(
      cl = cl,
      data = data,
      feature_info_list = feature_info_list,
      settings = settings,
      message_indent = message_indent + 1L,
      verbose = verbose
    )
    
    if (!is.na(object@file)) {
      saveRDS(feature_info_list, file = object@file)
    } else {
      return(feature_info_list)
    }
    
    return(invisible(TRUE))
  }
)





..generate_data_preprocessing_tasks <- function(
    data_ids,
    run_ids,
    file_paths,
    project_id
  ) {
  task_list <- list()
  
  # Create task to generic feature_info.
  generic_info_task <- methods::new(
    "familiarTaskGenericFeatureInfo",
    project_id = project_id
  )
  
  # Add file names.
  generic_info_task <- .set_file_name(
    object = generic_info_task,
    file_paths = file_paths
  )
  
  # Add to list, if the file does not exist on disk.
  if (!.file_exists(generic_info_task)) {
    task_list[[1L]] <- generic_info_task
  }
  
  ii <- 2L
  for (data_id in data_ids) {
    for (run_id in run_ids) {
      # Create task to generate run-specific feature info.
      run_info_task <- methods::new(
        "familiarTaskFeatureInfo",
        data_id = data_id,
        run_id = run_id,
        project_id = project_id
      )
      
      # Add file names.
      run_info_task <- .set_file_name(
        object = run_info_task,
        file_paths = file_paths
      )
      
      # Add to list, if the file does not exist on disk.
      if (!.file_exists(run_info_task)) {
        task_list[[ii]] <- run_info_task
        ii <- ii + 1L
      }
    }
  }
  
  return(task_list)
}
