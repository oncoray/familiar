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


# .set_file_name (collection task) ---------------------------------------------
setMethod(
  ".set_file_name",
  signature(object = "familiarTaskCollect"),
  function(object, file_paths = NULL) {
    if (is.null(file_paths)) return(object)
    
    name <- NULL
    if (object@data_id == 1L && object@run_id == 1L) {
      name <- "pooled"
    }
    
    # Generate file name of the model.
    object@file <- get_object_file_name(
      object_type = "familiarCollection",
      data_id = object@data_id,
      run_id = object@run_id,
      name = name,
      project_id = object@project_id,
      dir_path = file_paths$fam_coll_dir
    )
    
    return(object)
  }
)



# .get_task_descriptor (collection task) ---------------------------------------
setMethod(
  ".get_task_descriptor",
  signature(object = "familiarTaskCollect"),
  function(object, ...) {
    return(paste0(
      object@task_name, "_",
      object@data_id, "_", 
      object@run_id
    ))
  }
)





# familiarTaskEvaluate ---------------------------------------------------------
setClass(
  "familiarTaskEvaluate",
  contains = "familiarTask",
  slots = list(
    "validation" = "logical",
    "ensemble_data_id" = "integer",
    "ensemble_run_id" = "integer",
    "vimp_method" = "character",
    "learner" = "character",
    "data_set_name" = "character"
  ),
  prototype = methods::prototype(
    validation = NA, 
    # Whereas data_id describes where the data comes from, the ensemble_data_id
    # describes where ensembles are formed.
    ensemble_data_id = NA_integer_,
    ensemble_run_id = NA_integer_,
    vimp_method = NA_character_,
    learner = NA_character_,
    data_set_name = NA_character_,
    task_name = "evaluate"
  )
)



# .set_file_name (evaluation task) ---------------------------------------------
setMethod(
  ".set_file_name",
  signature(object = "familiarTaskEvaluate"),
  function(object, file_paths = NULL) {
    if (is.null(file_paths)) return(object)
    
    # Generate file name of the model.
    object@file <- get_object_file_name(
      object_type = "familiarData",
      data_id = object@data_id,
      run_id = object@run_id,
      learner = object@learner,
      vimp_method = object@vimp_method,
      ensemble_data_id = object@ensemble_data_id,
      ensemble_run_id = object@ensemble_run_id,
      name = object@data_set_name,
      project_id = object@project_id,
      dir_path = file_paths$fam_data_dir
    )
    
    return(object)
  }
)



# .get_task_descriptor (evaluation task) ---------------------------------------
setMethod(
  ".get_task_descriptor",
  signature(object = "familiarTaskEvaluate"),
  function(object, ...) {
    return(paste0(
      object@task_name, "_",
      object@data_id, "_", 
      object@run_id, "_", 
      object@vimp_method, "_", 
      object@learner, "_",
      object@ensemble_data, "_",
      object@ensemble_run_id, "_",
      object@data_set_name
    ))
  }
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
  
  # Find the data_id related to ensembling of models.
  train_data_id <- experiment_data@experiment_setup[train == TRUE, ]$main_data_id[1L]
  if (is_empty(train_data_id)) return(NULL)
  
  # Determine which parts of the experiment can be used for internal validation..
  run_table <- .get_run_table_from_experiment_setup(
    data_id = train_data_id,
    experiment_setup = experiment_data@experiment_setup
  )
  
  # Internal validation could exist at the level where pre-processing for
  # training is allowed.
  internal_validation_data_id <- tail(run_table[main_data_id <= train_data_id & can_pre_process == TRUE], n = 1L)$main_data_id[1L]
  
  if (!pool_only) {
    # Determine the collections at the last experimental level that can
    # pre-process and is part of the model-building branch.
    collection_run_ids <- seq_len(run_table[main_data_id == internal_validation_data_id]$n_runs[1L])
    
    ii <- 2L
    for (run_id in collection_run_ids) {
      collect_task_list[[ii]] <- methods::new(
        "familiarTaskCollect",
        data_id = internal_validation_data_id,
        run_id = run_id,
        project_id = experiment_data@project_id
      )
      
      ii <- ii + 1L
    }
  }
  
  # evaluation tasks -----------------------------------------------------------
  
  # Use collection tasks to set up the evaluation tasks, including for internal
  # validation.
  evaluate_task_list <- list()
  ii <- 1L
  for (jj in seq_along(collect_task_list)) {
    data_file_names <- NULL
    
    for (learner in learners) {
      for (vimp_method in vimp_methods) {
        
        ## external validation -------------------------------------------------
        
        # External validation: the top level has associated validation data.
        if (experiment_data@experiment_setup[main_data_id == 1L]$max_validation_instances > 0L) {
          # Initialise task.
          evaluate_task <- methods::new(
            "familiarTaskEvaluate",
            data_id = 1L,
            run_id = 1L,
            validation = TRUE,
            ensemble_data_id = collect_task_list[[jj]]@data_id,
            ensemble_run_id = collect_task_list[[jj]]@run_id,
            learner = learner,
            vimp_method = vimp_method,
            data_set_name = "external_validation",
            project_id = experiment_data@project_id
          )
          
          # Set file name.
          evaluate_task <-.set_file_name(
            object = evaluate_task,
            file_paths = file_paths
          )
          
          # Make task and associated file names available.
          data_file_names <- c(data_file_names, evaluate_task@file)
          evaluate_task_list[[ii]] <- evaluate_task
          
          ii <- ii + 1L
        }
        
        # internal validation --------------------------------------------------
        
        # Internal validation: the ensembling level has associated validation
        # data.
        if (experiment_data@experiment_setup[main_data_id == internal_validation_data_id]$max_validation_instances > 0L) {
          # Initialise task.
          evaluate_task <- methods::new(
            "familiarTaskEvaluate",
            data_id = collect_task_list[[jj]]@data_id,
            run_id = collect_task_list[[jj]]@run_id,
            validation = TRUE,
            ensemble_data_id = collect_task_list[[jj]]@data_id,
            ensemble_run_id = collect_task_list[[jj]]@run_id,
            learner = learner,
            vimp_method = vimp_method,
            data_set_name = "internal_validation",
            project_id = experiment_data@project_id
          )
          
          # Set file name.
          evaluate_task <-.set_file_name(
            object = evaluate_task,
            file_paths = file_paths
          )
          
          # Make task and associated file names available.
          data_file_names <- c(data_file_names, evaluate_task@file)
          evaluate_task_list[[ii]] <- evaluate_task
          
          ii <- ii + 1L
        }
        
        # development ----------------------------------------------------------
        # Development at the ensembling level.
        evaluate_task <- methods::new(
          "familiarTaskEvaluate",
          data_id = collect_task_list[[jj]]@data_id,
          run_id = collect_task_list[[jj]]@run_id,
          validation = FALSE,
          ensemble_data_id = collect_task_list[[jj]]@data_id,
          ensemble_run_id = collect_task_list[[jj]]@run_id,
          learner = learner,
          vimp_method = vimp_method,
          data_set_name = "development",
          project_id = experiment_data@project_id
        )
        
        # Set file name.
        evaluate_task <-.set_file_name(
          object = evaluate_task,
          file_paths = file_paths
        )
        
        # Make task and associated file names available.
        data_file_names <- c(data_file_names, evaluate_task@file)
        evaluate_task_list[[ii]] <- evaluate_task
        
        ii <- ii + 1L
      }
    }
    
    # Update collection tasks by adding file paths to 
    collect_task_list[[jj]]@data_file <- data_file_names
    collect_task_list[[jj]] <- .set_file_name(
      object = collect_task_list[[jj]],
      file_paths = file_paths
    )
  }
  
  # ensembles ------------------------------------------------------------------
  
  # Obtain run tables related to models.
  train_data_id <- experiment_data@experiment_setup[train == TRUE, ]$main_data_id[1L]
  if (is_empty(train_data_id)) return(NULL)
  
  run_tables <- .collect_run_tables(iteration_list = experiment_data@iteration_list)
  run_tables <- run_tables[sapply(
    run_tables,
    function(x, data_id) {
      return (tail(x, n = 1L)$data_id == data_id)
    },
    data_id = train_data_id
  )]
  browser()
  
  # Iterate over evaluation tasks and add corresponding models based on ensemble
  # data id and run id.
  for (ii in seq_along(evaluate_task_list)) {
    ensemble_data_id <- evaluate_task_list[[ii]]@ensemble_data_id
    ensemble_run_id <- evaluate_task_list[[ii]]@ensemble_run_id
    
    
  }
  
  browser()
  # train and variable importance tasks ----------------------------------------
  task_list <- .generate_trainer_tasks(
    experiment_data = experiment_data,
    vimp_methods = vimp_methods,
    file_paths = file_paths,
    skip_existing = skip_existing,
    ...
  )
}
