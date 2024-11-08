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
    object@file <- get_object_file_name(
      object_type = "genericFeatureInfo",
      project_id = object@project_id,
      dir_path = file_paths$process_data_dir
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
    outcome_info = NULL,
    ...
  ) {
    # This method is called when "data" is expected to be available somewhere in
    # the backend.
    if (is.null(outcome_info)) {
      ..error_reached_unreachable_code("outcome_info is required.")
    }
    
    # Create a dataObject.
    data <- methods::new(
      "dataObject",
      data = get_data_from_backend(),
      preprocessing_level = "none",
      outcome_type = outcome_info@outcome_type,
      outcome_info = outcome_info
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
    experiment_data = NULL,
    return_results = TRUE,
    ...
  ) {
    
    # Check if the desired data already exist elsewhere.
    results_exist <- FALSE
    
    if (is(experiment_data, "experimentData")) {
      if (!is_empty(experiment_data@feature_info[["generic"]])) {
        feature_info_list <- experiment_data@feature_info[["generic"]]
        results_exist <- TRUE
      }
    }
    
    if (file.exists(object@file)) {
      feature_info_list <- update_object(object = readRDS(object@file))
      results_exist <- TRUE
    }
    
    if (results_exist) {
      if (!is.na(object@file)) {
        saveRDS(feature_info_list, file = object@file)
      }
      
      if (return_results) {
        return(feature_info_list)
      }
    }
    
    # If this point is reached, results will be created de novo.
    # Extract basic feature information from the data.
    feature_info_list <- .get_generic_feature_info(
      data = data,
      outcome_type = data@outcome_type,
      descriptor = descriptor
    )
    
    # Write to file or return.
    if (!is.na(object@file)) {
      saveRDS(feature_info_list, file = object@file)
    }
    
    if (return_results) {
      return(feature_info_list)
    }
    
    return(invisible(TRUE))
  }
)



# .get_feature_info_list (generic feature info task) ---------------------------
setMethod(
  ".get_feature_info_list",
  signature(object = "familiarTaskGenericFeatureInfo"),
  function(object, feature_info_list, ...) {
    ..error_reached_unreachable_code(".get_feature_info_list does not exist for this task")
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
    
    # Generate file name of pre-processing file
    object@file <- get_object_file_name(
      object_type = "featureInfo",
      project_id = object@project_id,
      data_id = object@data_id,
      run_id = object@run_id,
      dir_path = file_paths$process_data_dir
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
    experiment_data = NULL,
    outcome_info = NULL,
    ...
  ) {
    # This method is called when "data" is expected to be available somewhere in
    # the backend.
    if (is.null(experiment_data)) {
      ..error_reached_unreachable_code("project_info is required for retrieving data from the backend.")
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
    experiment_data = NULL,
    settings = NULL,
    feature_info_list = NULL,
    message_indent = 0L,
    verbose = FALSE,
    cl = NULL,
    signature_features = NULL,
    novelty_features = NULL,
    fairness_features = NULL,
    return_results = TRUE,
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
    
    # Check if the desired data already exist elsewhere.
    results_exist <- FALSE
    
    if (is(experiment_data, "experimentData")) {
      if (!is_empty(experiment_data@feature_info[[paste0(object@data_id, ".", object@run_id)]])) {
        feature_info_list <- experiment_data@feature_info[[paste0(object@data_id, ".", object@run_id)]]
        results_exist <- TRUE
      }
    }
    
    if (file.exists(object@file)) {
      feature_info_list <- update_object(object = readRDS(object@file))
      results_exist <- TRUE
    }
    
    if (results_exist) {
      if (!is.na(object@file)) {
        saveRDS(feature_info_list, file = object@file)
      }
      
      if (return_results) {
        return(feature_info_list)
      }
    }
    
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
    
    # Add fairness feature info.
    if (is.null(fairness_features)) fairness_features <- settings$data$fairness_features
    feature_info_list <- add_fairness_info(
      feature_info_list = feature_info_list,
      fairness_features = fairness_features
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
    }
    
    if (return_results) {
      return(feature_info_list)
    }
    
    return(invisible(TRUE))
  }
)



# .get_feature_info_list (feature info task) -----------------------------------
setMethod(
  ".get_feature_info_list",
  signature(object = "familiarTaskFeatureInfo"),
  function(object, feature_info_list, ...) {
    ..error_reached_unreachable_code(".get_feature_info_list does not exist for this task")
  }
)



.generate_data_preprocessing_tasks <- function(
    data_ids,
    run_ids,
    file_paths,
    project_id,
    skip_existing = FALSE
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
  if (!skip_existing || !.file_exists(generic_info_task)) {
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
      if (!skip_existing || !.file_exists(run_info_task)) {
        task_list[[ii]] <- run_info_task
        ii <- ii + 1L
      }
    }
  }
  
  return(task_list)
}



.run_preprocessing <- function(
    tasks,
    experiment_data,
    ...
) {
  
  # Suppress NOTES due to non-standard evaluation in data.table
  data_id <- run_id <- list_name <- complete <- NULL
  
  # Create or load generic feature info.
  if (!.file_exists(tasks$generic_feature_info[[1L]])) {
    generic_feature_info <- .perform_task(
      object = tasks$generic_feature_info[[1L]],
      data = NULL,
      ...
    )
    
  } else {
    generic_feature_info <- readRDS(tasks$generic_feature_info[[1L]]@file)
  }
  
  # Determine which feature info objects need to be obtained.
  finished_tasks <- sapply(tasks$feature_info, .file_exists)
  unfinished_tasks <- tasks$feature_info[!finished_tasks]
  finished_tasks <- tasks$feature_info[finished_tasks]
  
  # Process any unfinished tasks.
  if (length(unfinished_tasks) > 0L) {
    ..run_preprocessing(
      tasks = unfinished_tasks,
      generic_feature_info <- generic_feature_info,
      experiment_data = experiment_data,
      ...
    )
  }
  
  # Load processed data from files.
  feature_info_list <- lapply(
    tasks$feature_info,
    function(x) (readRDS(x@file))
  )
  
  # Set names.
  names(feature_info_list) <- sapply(
    tasks$feature_info,
    function(x) (paste0(x@data_id, ".", x@run_id))
  )
  
  # Attach generic feature information.
  feature_info_list[["generic"]] <- generic_feature_info
  
  # Attach feature preprocessing information to the backend.
  .assign_feature_info_to_backend(feature_info_list = feature_info_list)
  
  return(invisible(TRUE))
}



..run_preprocessing <- function(
    tasks,
    generic_feature_info,
    settings,
    cl,
    message_indent = 0L,
    verbose,
    ...
) {

  # Determine how parallel processing takes place.
  if (settings$prep$do_parallel %in% c("TRUE", "inner")) {
    # Parallel processing in inner function, i.e. within each data subset.
    cl_inner <- cl
    cl_outer <- NULL
    
  } else if (settings$prep$do_parallel %in% c("outer")) {
    # Parallel processing in outer loop, i.e. over all data subsets.
    cl_inner <- NULL
    cl_outer <- cl
    
    if (!is.null(cl_outer)) {
      logger_message(
        paste0(
          "\nPre-processing: Load-balanced parallel processing is done in the outer loop. ",
          "No progress can be displayed."
        ),
        indent = message_indent,
        verbose = verbose
      )
    }
    
  } else {
    # No parallel processing.
    cl_inner <- cl_outer <- NULL
  }
  
  # Iterate over data subsets for which parameters have not yet been set.
  fam_mapply_lb(
    cl = cl_outer,
    assign = "data",
    FUN = .perform_task,
    progress_bar = !is.null(cl_outer),
    object = tasks,
    MoreArgs = list(
      "cl" = cl_inner,
      "data" = NULL,
      "feature_info_list" = generic_feature_info,
      "settings" = settings,
      "message_indent" = message_indent,
      "verbose" = verbose && is.null(cl_outer),
      "return_results" = FALSE,
      ...
    )
  )
  
  return(invisible(TRUE))
}
