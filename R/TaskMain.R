#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


# .file_exists (generic task) --------------------------------------------------
setMethod(
  ".file_exists",
  signature(object = "familiarTask"),
  function(object, ...) {
    if (is.na(object@file) || is.null(object@file)) return(FALSE)
    
    return(file.exists(object@file))
  }
)



# .get_current_run_table (generic task) ----------------------------------------
setMethod(
  ".get_current_run_table",
  signature(object = "familiarTask"),
  function(object, ...) {
    if (is_empty(object@run_table)) return(NULL)
    if (is.na(object@data_id) || is.na(object@run_id)) return(NULL)
    
    run_table <- object@run_table[[paste0(object@data_id, ".", object@run_id)]]
    if (!data.table::is.data.table(run_table)) return(NULL)
    
    return(run_table)
  }
)



# .get_feature_info_list (general task) ----------------------------------------
setMethod(
  ".get_feature_info_list",
  signature(object = "familiarTask"),
  function(object, feature_info_list, ...) {
    # Suppress NOTES due to non-standard evaluation in data.table
    can_pre_process <- NULL
    
    # Attempt to get the feature info list from the backend.
    if (is.null(feature_info_list) && !is.null(object@run_table)) {
      # Find the last entry that is available for pre-processing
      run_table <- object@run_table[[paste0(object@data_id, ".", object@run_id)]]
      pre_processing_run <- tail(run_table[can_pre_process == TRUE, ], n = 1L)
      
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
    
    if (!rlang::is_bare_list(feature_info_list)) {
      ..error("no feature info objects were found.")
    }
    
    return(feature_info_list)
  }
)



# .get_variable_importance_table (general task) --------------------------------
setMethod(
  ".get_variable_importance_table",
  signature(object = "familiarTask"),
  function(
    object,
    vimp_table,
    vimp_aggregation_method,
    vimp_rank_threshold,
    experiment_data = NULL,
    file_paths = NULL,
    ...
  ) {
    # Suppress NOTES due to non-standard evaluation in data.table
    vimp <- main_data_id <- data_id <- run_id <- NULL
    
    if (is.null(vimp_table) && !is.null(object@run_table)) {
      # This routine loads variable importances from disk, and is used when an
      # experiment is run using summon_familiar.
      
      # This check exists to make sure that the standard workflow passes the
      # correct objects.
      if (is.null(file_paths)) {
        ..error_reached_unreachable_code("file_paths was expected, but not provided.")
      }
      
      # Find the data and run ids corresponding to variable importance tables
      # relevant to the current run. First we will figure out the data id and
      # run id for ALL variable importance tables.
      vimp_data_id <- experiment_data@experiment_setup[vimp == TRUE, ]$main_data_id[1L]
      vimp_run_ids <- seq_len(experiment_data@experiment_setup[main_data_id == vimp_data_id, ]$n_runs[1L])
      
      # Select all run tables related to variable importance computation.
      vimp_run_tables <- object@run_table[paste(vimp_data_id, vimp_run_ids, sep = ".")]
      
      # Get the run table for training.
      train_run_table <- object@run_table[[paste0(object@data_id, ".", object@run_id)]]
      
      # Iterate backwards on data ids for the train run table to find matching
      # vimp run tables.
      matching <- logical(length(vimp_run_tables))
      train_data_chain_ids <- rev(train_run_table$data_id)
      ii <- 1L
      
      while (!any(matching)) {
        current_data_id <- train_data_chain_ids[ii]
        current_run_id <- train_run_table[data_id == current_data_id, ]$run_id[1L]
        
        for (jj in seq_along(matching)) {
          matching[jj] <- !is_empty(vimp_run_tables[[jj]][data_id == current_data_id & run_id == current_run_id])
        }
        ii <- ii + 1L
      }
      
      # Select matching variable importance run tables.
      vimp_run_tables <- vimp_run_tables[matching]
      vimp_run_ids <- sapply(vimp_run_tables, function(x) (tail(x, n = 1L)$run_id), simplify = TRUE, USE.NAMES = FALSE)
      
      # Get variable importance tables from disk.
      vimp_table <- list()
      for (ii in seq_along(vimp_run_ids)) {
        vimp_table_file <- get_object_file_name(
          project_id = object@project_id,
          data_id = vimp_data_id,
          run_id = vimp_run_ids[ii],
          vimp_method = object@vimp_method,
          object_type = "vimpTable",
          dir_path = file_paths$vimp_dir
        )
        
        if (file.exists(vimp_table_file)) {
          vimp_table[[ii]] <- update_object(readRDS(vimp_table_file))
          
        } else {
          ..error(paste0(
            "A variable importance table object was expected on disk but was not found: ",
            vimp_table_file
          ))
        }
      }
    }
    
    if (is.null(vimp_table) && is.na(object@vimp_table_file)) {
      # Create an ad-hoc list of variable importances.
      
      # Set up task, and explicitly don't write to file.
      vimp_task <- methods::new(
        "familiarTaskVimp",
        project_id = object@project_id,
        vimp_method = object@vimp_method,
        file = NA_character_
      )
      
      # Execute the task.
      vimp_table <- .perform_task(
        object = vimp_task,
        ...
      )
      
    } else if (is.null(vimp_table)) {
      # Assume that the vimp_table_file attribute contains the path to the
      # file containing the variable importance table.
      if (!file.exists(object@vimp_table_file)) {
        ..error(paste0("variable importance table file does not exist at location: ", object@vimp_table_file))
      }
      vimp_table <- update_object(readRDS(object@vimp_table_file))
      
    } else if (is.character(vimp_table)) {
      # If hyperparameters is a string, interpret this as a path to the
      # file containing the vimp method hyperparameters.
      if (!file.exists(vimp_table)) {
        ..error(paste0("variable importance table file does not exist at location: ", vimp_table))
      }
      vimp_table <- update_object(readRDS(vimp_table))
    }
    
    if (!(rlang::is_bare_list(vimp_table) || is(vimp_table, "vimpTable"))) {
      ..error("No variable importance table was found.")
    }
    
    # Get aggregate variable importances
    vimp_table <- aggregate_vimp_table(
      vimp_table,
      aggregation_method = vimp_aggregation_method,
      rank_threshold = vimp_rank_threshold
    )
    
    # Extract rank table.
    return(get_vimp_table(vimp_table))
  }
)




.generate_learner_data_preprocessing_tasks <- function(
    experiment_data,
    file_paths
) {
  
  # Suppress NOTES due to non-standard evaluation in data.table
  train <- can_pre_process <- perturbation_level <- main_data_id <- NULL
  
  # Find the data_id related to training.
  train_data_id <- experiment_data@experiment_setup[train == TRUE, ]$main_data_id[1L]
  if (is_empty(train_data_id)) return(NULL)
  
  # Determine which parts of the experimental setup are used by training.
  run_table <- .get_run_table_from_experiment_setup(
    data_id = train_data_id,
    experiment_setup = experiment_data@experiment_setup
  )
  
  # Find the data_id and run_ids for preprocessing.
  pre_process_data_id <- tail(run_table[main_data_id <= train_data_id & can_pre_process == TRUE], n = 1L)$main_data_id[1L]
  pre_process_run_ids <- seq_len(run_table[main_data_id == pre_process_data_id]$n_runs[1L])
  
  # Set up tasks.
  task_list <- .generate_data_preprocessing_tasks(
    data_ids = pre_process_data_id,
    run_ids = pre_process_run_ids,
    file_paths = file_paths,
    project_id = experiment_data@project_id
  )
  
  return(task_list)
}



.generate_vimp_data_preprocessing_tasks <- function(
    experiment_data,
    file_paths
) {
  # Suppress NOTES due to non-standard evaluation in data.table
  vimp <- can_pre_process <- main_data_id <- NULL
  
  # Find the data_id related to computing variable importance.
  vimp_data_id <- experiment_data@experiment_setup[vimp == TRUE, ]$main_data_id[1L]
  if (is_empty(vimp_data_id)) return(NULL)
  
  # Determine which parts of the experimental setup are used for assessing
  # variable importance..
  run_table <- .get_run_table_from_experiment_setup(
    data_id = vimp_data_id,
    experiment_setup = experiment_data@experiment_setup
  )
  
  # Find the data_id and run_ids for preprocessing.
  pre_process_data_id <- tail(run_table[main_data_id <= vimp_data_id & can_pre_process == TRUE], n = 1L)$main_data_id[1L]
  pre_process_run_ids <- seq_len(run_table[main_data_id == pre_process_data_id]$n_runs[1L])
  
  # Set up tasks.
  task_list <- .generate_data_preprocessing_tasks(
    data_ids = pre_process_data_id,
    run_ids = pre_process_run_ids,
    file_paths = file_paths,
    project_id = experiment_data@project_id
  )
  
  return(task_list)
}



.generate_evaluation_tasks <- function(
    file_paths,
    project_id
) {
  
}



.sort_tasks <- function(task_list) {
  # Select unique tasks.
  duplicate_tasks <- duplicated(sapply(task_list, FUN = .get_task_descriptor))
  task_list <- task_list[!duplicate_tasks]
  
  # Determine class of tasks.
  task_class <- sapply(task_list, class)
  
  task_list <- list(
    "generic_feature_info" = task_list[task_class == "familiarTaskGenericFeatureInfo"],
    "feature_info" = task_list[task_class == "familiarTaskFeatureInfo"],
    "hyperparameters_vimp" = task_list[task_class == "familiarTaskVimpHyperparameters"],
    "vimp" = task_list[task_class == "familiarTaskVimp"],
    "hyperparameters_learner" = task_list[task_class == "familiarTaskLearnerHyperparameters"],
    "train" = task_list[task_class == "familiarTaskTrain"],
    "ensemble" = task_list[task_class == "familiarTaskEnsemble"],
    "evaluate" = task_list[task_class == "familiarTaskEvaluate"]
  )
  
  # Update task_id and n_tasks attribute of the tasks.
  task_list <- lapply(
    task_list,
    function(x) {
      lapply(
        seq_along(x),
        function(ii, x, n) {
          object <- x[[ii]]
          object@task_id <- ii
          object@n_tasks <- n
          return(object)
        },
        x = x,
        n = length(x)
      )
    }
  )
  
  return(task_list)
}
