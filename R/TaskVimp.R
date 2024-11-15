#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL



# familiarTaskVimp -------------------------------------------------------------
setClass(
  "familiarTaskVimp",
  contains = "familiarTask",
  slots = list(
    "vimp_method" = "character",
    "hyperparameter_file" = "character",
    "feature_info_file" = "character"
  ),
  prototype = methods::prototype(
    vimp_method = NA_character_,
    hyperparameter_file = NA_character_,
    feature_info_file = NA_character_,
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
      data_id = object@data_id,
      run_id = object@run_id,
      vimp_method = object@vimp_method,
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
    experiment_data = NULL,
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
        "Variable importance: Starting variable importance computation using the \"",
        object@vimp_method, "\" method for run ",
        object@task_id, " of ",
        object@n_tasks, "."
      ),
      indent = message_indent,
      verbose = verbose
    )
    
    # Check if the desired data already exist elsewhere.
    results_exist <- FALSE
    browser()
    if (is(experiment_data, "experimentData")) {
      if (!is_empty(experiment_data@vimp_table_list)) {
        # Identify if the corresponding vimp_table exists.
        vimp_table <- experiment_data@vimp_table_list[[paste0(object@data_id, ".", object@run_id)]]
        results_exist <- TRUE
      }
    }
    
    if (file.exists(object@file)) {
      vimp_table <- update_object(object = readRDS(object@file))
      results_exist <- TRUE
    }
    
    if (results_exist) {
      if (!is.na(object@file)) {
        saveRDS(vimp_table, file = object@file)
      }
      
      if (return_results) {
        return(vimp_table)
      }
      
      return(invisible(TRUE))
    }
    
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
      run_table = object@run_table,
      project_id = object@project_id
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
    
    # Make sure the input data is processed.
    data <- process_input_data(
      object = vimp_object,
      data = data
    )
    
    # Compute variable importance.
    vimp_table <- .vimp(
      object = vimp_object, 
      data = data
    )
    
    if (!is.na(object@file)) {
      saveRDS(vimp_table, file = object@file)
    }
    
    if (return_results) {
      return(vimp_table)
    }
    
    return(invisible(TRUE))
  }
)



# .get_hyperparameters (vimp task) ---------------------------------------------
setMethod(
  ".get_hyperparameters",
  signature(object = "familiarTaskVimp"),
  function(
    object,
    hyperparameters,
    file_paths = NULL,
    ...
  ) {
    # Suppress NOTES due to non-standard evaluation in data.table
    can_pre_process <- NULL
    
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
        vimp_method = object@vimp_method,
        object_type = "hyperparametersVimp",
        dir_path = file_paths$vimp_dir
      )
      
      if (file.exists(hyperparameter_file)) {
        hyperparameter_object <- update_object(readRDS(hyperparameter_file))
        hyperparameters <- hyperparameter_object@hyperparameters
      }
    }
    
    
    if (is.null(hyperparameters) && is.na(object@hyperparameter_file)) {
      # Create an ad-hoc list of hyperparameters
      
      # Set up task, and explicitly don't write to file.
      hyperparameter_task <- methods::new(
        "familiarTaskVimpHyperparameters",
        project_id = object@project_id,
        vimp_method = object@vimp_method,
        file = NA_character_
      )
      
      # Execute the task.
      hyperparameter_object <- .perform_task(
        object = hyperparameter_task,
        ...
      )
      
      hyperparameters <- hyperparameter_object@hyperparameters
      
    } else if (is.null(hyperparameters)) {
      # Assume that the hyperparameter_file attribute contains the path to the
      # file containing the vimp method hyperparameters.
      if (!file.exists(object@hyperparameter_file)) {
        ..error(paste0("hyperparameter file does not exist at location: ", object@hyperparameter_file))
      }
      hyperparameter_object <- update_object(readRDS(object@hyperparameter_file))
      hyperparameters <- hyperparameter_object@hyperparameters
      
    } else if (is.character(hyperparameters)) {
      # If hyperparameters is a string, interpret this as a path to the
      # file containing the vimp method hyperparameters.
      if (!file.exists(hyperparameters)) {
        ..error(paste0("hyperparameter file does not exist at location: ", hyperparameters))
      }
      hyperparameter_object <- update_object(readRDS(hyperparameters))
      hyperparameters <- hyperparameter_object@hyperparameters
    }
    
    if (!rlang::is_bare_list(hyperparameters)) {
      ..error("No hyperparameters were found.")
    }
    
    return(hyperparameters)
  }
)



.generate_vimp_tasks <- function(
    experiment_data,
    vimp_methods,
    file_paths,
    skip_existing = FALSE
) {
  # Suppress NOTES due to non-standard evaluation in data.table
  vimp <- main_data_id <- can_pre_process <- NULL
  
  # Find the data_id related to computing variable importance.
  data_id <- experiment_data@experiment_setup[vimp == TRUE, ]$main_data_id[1L]
  if (is_empty(data_id)) return(NULL)
  
  # Initialise empty list.
  task_list <- list()
  ii <- 1L
  run_tables <- .collect_run_tables(iteration_list = experiment_data@iteration_list)
  
  # vimp tasks -----------------------------------------------------------------
  
  # Get run ids.
  run_ids <- seq_len(experiment_data@experiment_setup[main_data_id == data_id]$n_runs[1L])
  
  # Set up variable importance computation task.
  for (vimp_method in vimp_methods) {
    for (run_id in run_ids) {
      
      # Create task to generate run-specific feature info.
      vimp_task <- methods::new(
        "familiarTaskVimp",
        data_id = data_id,
        run_id = run_id,
        vimp_method = vimp_method,
        run_table = run_tables,
        project_id = experiment_data@project_id
      )
      
      # Add file names.
      vimp_task <- .set_file_name(
        object = vimp_task,
        file_paths = file_paths
      )
      
      # Add to list, if the file does not exist on disk.
      if (!skip_existing || !.file_exists(vimp_task)) {
        task_list[[ii]] <- vimp_task
        ii <- ii + 1L
      }
    }
  }
  
  # Check if any vimp-related tasks are required.
  if (length(task_list) == 0L) return(NULL)
  
  # vimp hyperparameter tasks --------------------------------------------------
  
  # Identify which data id corresponds to computing hyperparameters.
  vimp_run_table <- .get_run_table_from_experiment_setup(
    data_id = data_id,
    experiment_setup = experiment_data@experiment_setup
  )
  
  vimp_hyperparameter_data_id <- tail(
    vimp_run_table[main_data_id <= data_id & can_pre_process == TRUE, ],
    n = 1L
  )$main_data_id[1L]
  
  # Get run ids.
  run_ids <- seq_len(vimp_run_table[main_data_id == vimp_hyperparameter_data_id, ]$n_runs[1L])
  
  for (vimp_method in vimp_methods) {
    for (run_id in run_ids) {
      # Create task to generate run-specific feature info.
      vimp_hyperparameter_task <- methods::new(
        "familiarTaskVimpHyperparameters",
        data_id = vimp_hyperparameter_data_id,
        run_id = run_id,
        vimp_method = vimp_method,
        run_table = run_tables,
        project_id = experiment_data@project_id
      )
      
      # Add file names.
      vimp_hyperparameter_task <- .set_file_name(
        object = vimp_hyperparameter_task,
        file_paths = file_paths
      )
      
      # Add to list, if the file does not exist on disk.
      if (!skip_existing || !.file_exists(vimp_hyperparameter_task)) {
        task_list[[ii]] <- vimp_hyperparameter_task
        ii <- ii + 1L
      }
    }
  }

  # Add tasks related to data processing for vimp methods.
  task_list <- c(
    task_list, 
    .generate_vimp_data_preprocessing_tasks(
      experiment_data = experiment_data,
      file_paths = file_paths
    )
  )
  
  return(task_list)
}



.run_variable_importance_computation <- function(
  cl,
  tasks,
  message_indent = 0L,
  verbose,
  ...
) {

  # Check that any tasks are available for processing.
  if (is_empty(tasks$hyperparameters_vimp) || is_empty(tasks$vimp)) return(invisible(FALSE))
  
  # Determine which variable importance hyperparameters need to be found.
  finished_tasks <- sapply(tasks$hyperparameters_vimp, .file_exists)
  unfinished_tasks <- tasks$hyperparameters_vimp[!finished_tasks]
  finished_tasks <- tasks$hyperparameters_vimp[finished_tasks]
  
  # Process any unfinished tasks.
  if (length(unfinished_tasks) > 0L) {
    ..run_variable_importance_computation_hyperparameters(
      cl = cl,
      tasks = unfinished_tasks,
      message_indent = message_indent,
      verbose = verbose,
      ...
    )
  }
  
  # Determine which variable importance tasks are required.
  finished_tasks <- sapply(tasks$vimp, .file_exists)
  unfinished_tasks <- tasks$vimp[!finished_tasks]
  finished_tasks <- tasks$vimp[finished_tasks]
  
  # Process any unfinished tasks.
  if (length(unfinished_tasks) > 0L) {
    ..run_variable_importance_computation(
      cl = cl,
      tasks = unfinished_tasks,
      message_indent = message_indent,
      verbose = verbose,
      ...
    )
  }
  
  return(invisible(TRUE))
}



..run_variable_importance_computation <- function(
    tasks,
    cl,
    message_indent = 0L,
    verbose,
    ...
) {
  
  # Message that variable importances computation is starting.
  logger_message(
    paste0(
      "Variable importance: Starting variable importance computation."
    ),
    indent = message_indent,
    verbose = verbose
  )
  
  fam_mapply_lb(
    cl = cl,
    assign = "all",
    FUN = .perform_task,
    progress_bar = FALSE,
    object = tasks,
    MoreArgs = list(
      "data" = NULL,
      "return_results" = FALSE,
      "message_indent" = message_indent + 1L,
      "verbose" = verbose,
      ...
    )
  )
  
  # Message that variable importances have been computed.
  logger_message(
    paste0(
      "Variable importance: Variable importances have been computed.\n"
    ),
    indent = message_indent,
    verbose = verbose
  )
}
