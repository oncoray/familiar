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



.generate_trainer_tasks <- function(
    file_paths,
    project_id  
) {
  
  for (data_id in data_ids) {
    for (run_id in run_ids) {
      for (vimp_method in vimp_methods) {
        for (learner in learners) {
          # Set up trainer task.
          
          # Set up hyperparameter extraction task.
          
        }
      }
    }
  }
  
  # Check if any learner-related tasks are required.
  if (len(task_list) == 0L) return(NULL)
  
  # Add tasks related to data processing for variable importance objects.
  task_list <- c(
    task_list,
    .generate_vimp_tasks(
      file_paths = file_paths,
      project_id = project_id
    )
  )
  
  # Add tasks related to data processing for learners.
  task_list <- c(
    task_list, 
    .generate_learner_data_preprocessing_tasks(
      file_paths = file_paths,
      project_id = project_id
    )
  )
  
  return(task_list)
}






.generate_learner_data_preprocessing_tasks <- function(
    experiment_data,
    file_paths
) {
  
  # Suppress NOTES due to non-standard evaluation in data.table
  train <- can_pre_process <- NULL
  
  # Find the data_id related to training.
  train_data_id <- experiment_data@experiment_setup[train == TRUE, ]$main_data_id[1L]
  if (is_empty(train_data_id)) return(NULL)
  
  # Find the corresponding data_id for pre-processing.
  run_table <- experiment_data@iteration_list[[as.character(train_data_id)]]$run[[1L]]$run_table
  pre_process_data_id <- tail(run_table[can_pre_process == TRUE, ], n = 1L)$data_id[1L]
  
  # Get run ids.
  run_ids <- names(experiment_data@iteration_list[[as.character(pre_process_data_id)]]$run)
  run_ids <- as.integer(run_ids)
  
  # Set up tasks.
  task_list <- .generate_data_preprocessing_tasks(
    data_ids = pre_process_data_id,
    run_ids = run_ids,
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
  vimp <- can_pre_process <- NULL
  
  # Find the data_id related to computing variable importance.
  vimp_data_id <- experiment_data@experiment_setup[vimp == TRUE, ]$main_data_id[1L]
  if (is_empty(vimp_data_id)) return(NULL)
  
  # Find the corresponding data_id for pre-processing.
  run_table <- experiment_data@iteration_list[[as.character(vimp_data_id)]]$run[[1L]]$run_table
  pre_process_data_id <- tail(run_table[can_pre_process == TRUE, ], n = 1L)$data_id[1L]
  
  # Get run ids.
  run_ids <- names(experiment_data@iteration_list[[as.character(pre_process_data_id)]]$run)
  run_ids <- as.integer(run_ids)
  
  # Set up tasks.
  task_list <- .generate_data_preprocessing_tasks(
    data_ids = pre_process_data_id,
    run_ids = run_ids,
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
