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
    
    # Generate file name of the model.
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
    vimp_aggregation_method = NULL,
    vimp_rank_threshold = NULL,
    settings = NULL,
    feature_info_list = NULL,
    vimp_table = NULL,
    hyperparameters = NULL,
    novelty_detector = NULL,
    detector_parameters = NULL,
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
    
    # Set vimp aggregation method and vimp_rank_threshold based on settings.
    if (!is.null(settings)) {
      if (is.null(vimp_aggregation_method)) {
        vimp_aggregation_method <- settings$vimp$aggregation
      }
      if (is.null(vimp_rank_threshold)) {
        vimp_rank_threshold <- settings$vimp$aggr_rank_threshold
      }
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
    
    # Check and retrieve hyperparameters. We do this prior to retrieving the
    # variable importance tables, as these may be attached to hyperparameter
    # object.
    hyperparameters <- .get_hyperparameters(
      object = object,
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
    
    # Create the raw model object for training.
    model_object <- methods::new(
      "familiarModel",
      outcome_type = data@outcome_type,
      hyperparameters = hyperparameters$hyperparameters,
      hyperparameter_data = hyperparameters$hyperparameter_data,
      vimp_method = object@vimp_method,
      vimp_table = vimp_table,
      vimp_aggregation_method = vimp_aggregation_method,
      vimp_rank_threshold = vimp_rank_threshold,
      learner = object@learner,
      feature_info = feature_info_list,
      outcome_info = data@outcome_info,
      data_id = object@data_id,
      run_id = object@run_id,
      run_table = .get_current_run_table(object = object),
      settings = settings$eval,
      project_id = object@project_id
    )
    
    # Select features based on variable importances.
    model_object <- set_model_features(
      object = model_object,
      minimise_footprint = FALSE
    )
    
    # Train model.
    model_object <- .train(
      object = model_object,
      data = data,
      get_additional_info = TRUE,
      ...
    )
    
    # Override novelty detector. Otherwise it is possible that a novelty
    # detector is trained on data that are not used by the model, due
    # to selected_features = NULL in .perform_task.
    if (is_empty(model_object@novelty_detector)) novelty_detector <- "none"
    
    # Set up task to train novelty detector
    detector_task <- methods::new(
      "familiarTaskTrainNovelty",
      learner = novelty_detector,
      vimp_method = object@vimp_method,
      data_id = object@data_id,
      run_id = object@run_id,
      project_id = object@project_id
    )
    
    # Train novelty detector and add to model.
    model_object@novelty_detector <- .perform_task(
      object = detector_task,
      data = data,
      selected_features = features_after_clustering(
        features = model_object@novelty_features,
        feature_info_list = feature_info_list),
      settings = settings,
      feature_info_list = feature_info_list,
      vimp_table = vimp_table,
      vimp_aggregation_method = vimp_aggregation_method,
      vimp_rank_threshold = vimp_rank_threshold,
      hyperparameters = detector_parameters,
      return_results = TRUE
    )
    
    # Add model name
    model_object <- set_object_name(model_object)
    
    if (!is.na(object@file)) {
      saveRDS(model_object, file = object@file)
    }
    
    if (return_results) {
      return(model_object)
    }
    
    return(TRUE)
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
        object_type = "hyperparametersLearner",
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
        "familiarTaskLearnerHyperparameters",
        project_id = object@project_id,
        vimp_method = object@vimp_method,
        learner = object@learner,
        file = NA_character_
      )
      
      # Execute the task.
      hyperparameter_object <- .perform_task(
        object = hyperparameter_task,
        hyperparameters = hyperparameters,
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
    
    if (is(hyperparameter_object, "familiarModel")) {
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



.generate_trainer_tasks <- function(
    experiment_data,
    optimisation_determine_vimp,
    vimp_methods,
    learners,
    file_paths,
    skip_existing = FALSE
) {
  # Suppress NOTES due to non-standard evaluation in data.table
  train <- main_data_id <- can_pre_process <- vimp <- NULL
  
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
  
  # Check how variable importance data should be handled.
  if (is_empty(experiment_data@experiment_setup[vimp == TRUE, ])) {
    use_vimp <- "return_hpo_vimp"
    
  } else if (optimisation_determine_vimp) {
    use_vimp <- "use_hpo_vimp"
    
  } else {
    use_vimp <- "use_main_vimp"
  }
  
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
          use_vimp = use_vimp,
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
      "novelty_detector" = settings$mb$novelty_detector,
      "detector_paramaters" = settings$mb$detector_parameters[[settings$mb$novelty_detector]],
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
