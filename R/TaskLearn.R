#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL



# familiarTaskTrain -------------------------------------------------------------
setClass(
  "familiarTaskTrain",
  contains = "familiarTask",
  slots = list(
    "vimp_method" = "character",
    "learner" = "character",
    "vimp_table_file" = "character",
    "hyperparameter_file" = "character",
    "feature_info_file" = "character"
  ),
  prototype = methods::prototype(
    vimp_method = NA_character_,
    learner = NA_character_,
    vimp_table_file = NA_character_,
    hyperparameter_file = NA_character_,
    feature_info_file = NA_character_,
    task_name = "train_model"
  )
)




# .set_file_name (train task) --------------------------------------------------
setMethod(
  ".set_file_name",
  signature(object = "familiarTaskTrain"),
  function(object, file_paths = NULL) {
    if (is.null(file_paths)) return(object)
    
    # Generate file name of variable importance table
    object@file <- get_object_file_name(
      object_type = "familiarModel",
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



# .get_task_descriptor (train task) --------------------------------------------
setMethod(
  ".get_task_descriptor",
  signature(object = "familiarTaskTrain"),
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
    object = "familiarTaskTrain",
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
    object = "familiarTaskTrain",
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
        "Training: Starting model training for the \"", object@learner,
        "\" learner and the \"", object@vimp_method,
        "\" variable importance method for run ",
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
    
    # Create the raw model object for training..
    model_object <- methods::new(
      "familiarModel",
      outcome_type = data@outcome_type,
      hyperparameters = hyperparameters,
      vimp_method = object@vimp_method,
      learner = object@learner,
      outcome_info = data@outcome_info,
      run_table = .get_current_run_table(object = object),
      project_id = object@project_id
    )
    
    # Promote to the correct subclass.
    model_object <- promote_learner(object = model_object)
  
    # Find required features. Exclude the signature features at this point, as
    # these will have been dropped from the variable importance table.
    required_features <- get_required_features(
      x = data,
      feature_info_list = feature_info_list
    )
    
    # Limit to required features.
    model_object@required_features <- required_features
    model_object@feature_info <- feature_info_list[required_features]
    
    # Make sure the input data is processed.
    data <- process_input_data(
      object = model_object,
      data = data
    )
    
    # Train model..
    model_object <- .train(
      object = model_object,
      data = data
    )
    
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
  signature(object = "familiarTaskTrain"),
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
      hyperparameter_run <- tail(object@run_table[can_pre_process == TRUE, ], n = 1L)
      
      # Find the file name.
      hyperparameter_file <- get_object_file_name(
        project_id = object@project_id,
        data_id = hyperparameter_run$data_id[1L],
        run_id = hyperparameter_run$run_id[1L],
        learner = object@learner,
        vimp_method = object@vimp_method,
        object_type = "hyperparametersLearner",
        dir_path = file_paths$mb_dir
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
        "familiarTaskLearnerHyperparameters",
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



.generate_trainer_tasks <- function(
    experiment_data,
    vimp_methods,
    learners,
    file_paths,
    skip_existing = FALSE
) {
  # Suppress NOTES due to non-standard evaluation in data.table
  train <- main_data_id <- can_pre_process <- NULL
  
  # Find the data_id related to model training.
  data_id <- experiment_data@experiment_setup[train == TRUE, ]$main_data_id[1L]
  if (is_empty(data_id)) return(NULL)
  
  # Initialise empty list.
  task_list <- list()
  ii <- 1L
  run_tables <- .collect_run_tables(iteration_list = experiment_data@iteration_list)
  
  # train tasks ----------------------------------------------------------------
  
  # Get run ids.
  run_ids <- seq_len(experiment_data@experiment_setup[main_data_id == data_id]$n_runs[1L])
  
  # Set up variable importance computation task.
  for (learner in learners) {
    for (vimp_method in vimp_methods) {
      for (run_id in run_ids) {
        
        # Create task to generate run-specific feature info.
        train_task <- methods::new(
          "familiarTaskTrain",
          data_id = data_id,
          run_id = run_id,
          vimp_method = vimp_method,
          learner = learner,
          run_table = run_tables,
          project_id = experiment_data@project_id
        )
        
        # Add file names.
        train_task <- .set_file_name(
          object = train_task,
          file_paths = file_paths
        )
        
        # Add to list, if the file does not exist on disk.
        if (!skip_existing || !.file_exists(train_task)) {
          task_list[[ii]] <- train_task
          ii <- ii + 1L
        }
      }
    }
  }
  
  # Check if any train-related tasks are required.
  if (length(task_list) == 0L) return(NULL)
  
  # learner hyperparameter tasks -----------------------------------------------
  
  # Set up variable importance hyperparameter task.
  train_run_table <- .get_run_table_from_experiment_setup(
    data_id = data_id,
    experiment_setup = experiment_data@experiment_setup
  )
  learner_hyperparameter_data_id <- tail(
    train_run_table[main_data_id <= data_id & can_pre_process == TRUE, ],
    n = 1L
  )$main_data_id[1L]
  
  # Get run ids.
  run_ids <- seq_len(train_run_table[main_data_id == learner_hyperparameter_data_id, ]$n_runs[1L])
  
  for (learner in learners) {
    for (vimp_method in vimp_methods) {
      for (run_id in run_ids) {
        # Create task to generate run-specific feature info.
        learner_hyperparameter_task <- methods::new(
          "familiarTaskLearnerHyperparameters",
          data_id = learner_hyperparameter_data_id,
          run_id = run_id,
          vimp_method = vimp_method,
          learner = learner,
          run_table = run_tables,
          project_id = experiment_data@project_id
        )
        
        # Add file names.
        learner_hyperparameter_task <- .set_file_name(
          object = learner_hyperparameter_task,
          file_paths = file_paths
        )
        
        # Add to list, if the file does not exist on disk.
        if (!skip_existing || !.file_exists(learner_hyperparameter_task)) {
          task_list[[ii]] <- learner_hyperparameter_task
          ii <- ii + 1L
        }
      }
    }
  }
  
  # Add tasks related to data processing for learner methods.
  task_list <- c(
    task_list, 
    .generate_learner_data_preprocessing_tasks(
      experiment_data = experiment_data,
      file_paths = file_paths
    )
  )
  
  # variable importance tasks --------------------------------------------------
  task_list <- c(
    task_list,
    .generate_vimp_tasks(
      experiment_data = experiment_data,
      vimp_methods = vimp_methods,
      file_paths = file_paths,
      skip_existing = skip_existing
    )
  )
  
  return(task_list)
}



.run_learner <- function(
    cl,
    tasks,
    message_indent = 0L,
    verbose,
    ...
) {
  
  # Check that any tasks are available for processing.
  if (is_empty(tasks$hyperparameters_learner) || is_empty(tasks$train)) return(invisible(FALSE))
  
  # Determine which learner hyperparameter sets need to be found.
  finished_tasks <- sapply(tasks$hyperparameters_learner, .file_exists)
  unfinished_tasks <- tasks$hyperparameters_learner[!finished_tasks]
  finished_tasks <- tasks$hyperparameters_learner[finished_tasks]
  
  # Process any unfinished tasks.
  if (length(unfinished_tasks) > 0L) {
    ..run_learner_computation_hyperparameters(
      cl = cl,
      tasks = unfinished_tasks,
      message_indent = message_indent,
      verbose = verbose,
      ...
    )
  }
  
  # Determine which variable importance tasks are required.
  finished_tasks <- sapply(tasks$train, .file_exists)
  unfinished_tasks <- tasks$train[!finished_tasks]
  finished_tasks <- tasks$train[finished_tasks]
  
  # Process any unfinished tasks.
  if (length(unfinished_tasks) > 0L) {
    ..run_learner(
      cl = cl,
      tasks = unfinished_tasks,
      message_indent = message_indent,
      verbose = verbose,
      ...
    )
  }
  
  return(invisible(TRUE))
}



..run_learner <- function(
    tasks,
    cl,
    settings,
    message_indent = 0L,
    verbose,
    ...
) {
  
  # Message that variable importances computation is starting.
  logger_message(
    paste0(
      "Training: Starting model training."
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
      "settings" = settings,
      "vimp_aggregation_method" = settings$vimp$aggregation,
      "vimp_rank_threshold" = settings$vimp$aggr_rank_threshold,
      "message_indent" = message_indent + 1L,
      "verbose" = verbose,
      ...
    )
  )
  
  # Message that variable importances have been computed.
  logger_message(
    paste0(
      "Training: Models were trained.\n"
    ),
    indent = message_indent,
    verbose = verbose
  )
}
