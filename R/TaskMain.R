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



.generate_learner_data_preprocessing_tasks <- function(
    experiment_data,
    file_paths
) {
  
  # Suppress NOTES due to non-standard evaluation in data.table
  train <- can_pre_process <- main_data_id <- NULL
  
  # Find the data_id related to training.
  train_data_id <- experiment_data@experiment_setup[train == TRUE, ]$main_data_id[1L]
  if (is_empty(train_data_id)) return(NULL)
  
  # Find the data_id and run_ids for preprocessing.
  pre_process_data_id <- tail(experiment_data@experiment_setup[main_data_id <= train_data_id & can_pre_process == TRUE], n = 1L)$main_data_id[1L]
  pre_process_run_ids <- seq_len(experiment_data@experiment_setup[main_data_id == pre_process_data_id]$n_runs[1L])
  
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
  
  # Find the data_id and run_ids for preprocessing.
  pre_process_data_id <- tail(experiment_data@experiment_setup[main_data_id <= vimp_data_id & can_pre_process == TRUE], n = 1L)$main_data_id[1L]
  pre_process_run_ids <- seq_len(experiment_data@experiment_setup[main_data_id == pre_process_data_id]$n_runs[1L])
  
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
