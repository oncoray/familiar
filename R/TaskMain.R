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
    .generate_learner_tasks(
      file_paths = file_paths,
      project_id = project_id
    )
  )
  
  return(task_list)
}


.generate_vimp_tasks <- function() {
  
  # Check if vimp should be computed separately or is computed during 
  # hyperparameter optimisation.
  
  for (data_id in data_ids) {
    for (run_id in run_ids) {
      for (vimp_method in vimp_methods) {
        
        # Check if the variable importance method requires any computation.
        # For example, signature_only, none and random do not require
        # computation.
        
        # Set up variable importance computation task.
        
        # Set up variable importance hyperparameter task.
        
      }
    }
  }
  
  # Add tasks related to data processing for vimp methods.
  
}



.generate_learner_data_preprocessing_tasks <- function(
    file_paths,
    project_id
) {
  
  # Find the data_id related to training the learner.
  learner_data_id <- .get_process_step_data_identifier(
      project_info = project_info,
      process_step = "mb"
    )
  
  # Find the corresponding pre-processing data_id.
  pre_process_data_id <- .get_preprocessing_iteration_identifiers(
    run = .get_run_list(
      iteration_list = project_info$iter_list,
      data_id = learner_data_id,
      run_id = 1L
    )
  )$data
  
  # Use the data_id to find the list of runs.
  iteration_list <- .get_run_list(
    iteration_list = project_info$iter_list,
    data_id = pre_process_data_id
  )
  
  # Get run ids
  browser()
  
  # Set up tasks.
  task_list <- ..generate_data_preprocessing_tasks(
    data_ids = pre_process_data_id,
    run_ids = run_ids,
    file_paths = file_paths,
    project_id = project_id
  )
  
  return(task_list)
}



.generate_vimp_data_preprocessing_tasks <- function(
    file_paths,
    project_id
) {
  
  # Find the data_id related to computing variable importance..
  vimp_data_id <- .get_process_step_data_identifier(
    project_info = project_info,
    process_step = "vimp"
  )
  
  # Find the corresponding data_id for pre-processing.
  pre_process_data_id <- .get_preprocessing_iteration_identifiers(
    run = .get_run_list(
      iteration_list = project_info$iter_list,
      data_id = vimp_data_id,
      run_id = 1L
    )
  )$data
  
  # Use the data_id to find the list of runs.
  iteration_list <- .get_run_list(
    iteration_list = project_info$iter_list,
    data_id = pre_process_data_id
  )
  
  # Get run ids
  browser()
  
  # Set up tasks.
  task_list <- ..generate_data_preprocessing_tasks(
    data_ids = pre_process_data_id,
    run_ids = run_ids,
    file_paths = file_paths,
    project_id = project_id
  )
  
  return(task_list)
}


