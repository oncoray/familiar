# familiarTaskGenericFeatureInfo -----------------------------------------------
setClass(
  "familiarTaskGenericFeatureInfo",
  prototype = methods::prototype(
    name = "create_generic_feature_info"
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
    return(object@name)
  }
)



# .perform_task (generic feature info task) ------------------------------------
setMethod(
  ".perform_task",
  signature(object = "familiarTaskGenericFeatureInfo"),
  function(
    object,
    data,
    outcome_type = NULL,
    descriptor = NULL
  ) {
    if (is(data, "dataObject")) outcome_type <- data@outcome_type
    if (is.null(outcome_type)) {
      ..error_reached_unreachable_code("outcome_type is expected to be provided")
    }
    
    # Extract basic feature information from the data.
    feature_info_list <- .get_generic_feature_info(
      data = data,
      outcome_type = outcome_type,
      descriptor = NULL
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
  prototype = methods::prototype(
    name = "create_feature_info"
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
    return(paste0(object@name, "_", object@data_id, "_", object@run_id))
  }
)



# .perform_task (feature info task) --------------------------------------------
setMethod(
  ".perform_task",
  signature(object = "familiarTaskFeatureInfo"),
  function(
    object,
    data,
    settings,
    feature_info_list = NULL,
    project_info = NULL,
    message_indent = 0L,
    verbose = FALSE,
    cl = NULL
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
        project_id = project_info$project_id,
        file = NA_character_
      )
      
      # Execute the task.
      feature_info_list <- .perform_task(generic_feature_info_task)
    }
    
    # Update feature info list.
    feature_info_list <- determine_preprocessing_parameters(
      cl = cl,
      feature_info_list = feature_info_list,
      data_id = object@data_id,
      run_id = object@run_id,
      project_info = project_info,
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
