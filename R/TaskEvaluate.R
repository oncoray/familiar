#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

# familiarTaskCollect ----------------------------------------------------------
setClass(
  "familiarTaskCollect",
  contains = "familiarTask",
  slots = list(
    "data_file" = "character"
  ),
  prototype = methods::prototype(
    data_file = NA_character_,
    task_name = "collect_data"
  )
)



.generate_evaluation_tasks <- function(
  experiment_data,
  vimp_methods,
  learners,
  file_paths,
  pool_only,
  skip_existing = FALSE,
  ...
) {
  
  # Suppress NOTES due to non-standard evaluation in data.table
  train <- can_pre_process <- perturbation_level <- main_data_id <- NULL
  
  # collection tasks -----------------------------------------------------------
  
  # Always created the top-layer pooled collection.
  collect_task_list <- list(
    methods::new(
      "familiarTaskCollect",
      data_id = 1L,
      run_id = 1L,
      project_id = experiment_data@project_id
    )
  )
  
  if (!pool_only) {
    # Determine the collections at the last experimental level that can
    # pre-process and is part of the model-building branch.
    
    # Find the data_id related to training.
    train_data_id <- experiment_data@experiment_setup[train == TRUE, ]$main_data_id[1L]
    if (is_empty(train_data_id)) return(NULL)
    
    # Determine which parts of the experimental setup are used by training.
    run_table <- .get_run_table_from_experiment_setup(
      data_id = train_data_id,
      experiment_setup = experiment_data@experiment_setup
    )
    
    # Find the data_id and run_ids for preprocessing.
    collection_data_id <- tail(run_table[main_data_id <= train_data_id & can_pre_process == TRUE], n = 1L)$main_data_id[1L]
    collection_run_ids <- seq_len(run_table[main_data_id == collection_data_id]$n_runs[1L])
    
    for (run_id in collection_run_ids) {
      collect_task_list <- c(
        collect_task_list,
        methods::new(
          "familiarTaskCollect",
          data_id = collection_data_id,
          run_id = run_id,
          project_id = experiment_data@project_id
        )
      )
    }
  }
  
  # evaluation tasks -----------------------------------------------------------
  
  # Use collection tasks to set up the evaluation tasks, including for internal validation.
  
  # train and variable importance tasks ----------------------------------------
  task_list <- c(
    task_list,
    .generate_learner_tasks(
      experiment_data = experiment_data,
      vimp_methods = vimp_methods,
      file_paths = file_paths,
      skip_existing = skip_existing,
      ...
    )
  )
}
