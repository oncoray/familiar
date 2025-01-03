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



# .perform_task (collection task, NULL) ----------------------------------------
setMethod(
  ".perform_task",
  signature(
    object = "familiarTaskCollect",
    data = "NULL"
  ),
  function(
    object,
    data,
    return_results = TRUE,
    ...
  ) {
    # Process collection.
    collection_object <- suppressWarnings(
      as_familiar_collection(
        object = object@data_file
      )
    )
    
    if (!is.na(object@file)) {
      saveRDS(collection_object, file = object@file)
    }
    
    if (return_results) {
      return(collection_object)
    }
  }
)



# .perform_task (collection task, ANY) -----------------------------------------
setMethod(
  ".perform_task",
  signature(
    object = "familiarTaskCollect",
    data = "ANY"
  ),
  function(
    object,
    data,
    return_results = TRUE,
    ...
  ) {
    # Process collection.
    collection_object <- suppressWarnings(
      as_familiar_collection(
        object = data
      )
    )
    
    if (!is.na(object@file)) {
      saveRDS(collection_object, file = object@file)
    }
    
    if (return_results) {
      return(collection_object)
    }
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
    "get_predictions_at_model_level" = "logical",
    "force_ensemble_detail_level" = "logical",
    "vimp_method" = "character",
    "learner" = "character",
    "data_set_name" = "character",
    "model_files" = "character"
  ),
  prototype = methods::prototype(
    validation = NA, 
    # Whereas data_id describes where the overall data comes from, the
    # ensemble_data_id describes where ensembles are formed.
    ensemble_data_id = NA_integer_,
    ensemble_run_id = NA_integer_,
    # This parameter determines which data is used for generating predictions.
    # If FALSE, data will be obtained using data_id and run_id of the task. If
    # TRUE, data will be obtained using the data_id and run_id associated with
    # each model. In practice, for internal runs this will be set to TRUE except
    # for external validation data, whereas for external tasks (e.g. on existing
    # models) this will be FALSE, and the provided data will be used directly.
    get_predictions_at_model_level = FALSE,
    # If individual models do not have sufficient data to perform hybrid
    # analysis (each model is used to compute part of the bootstraps when
    # computing confidence intervals) for an evaluation step, that evaluation
    # step may fail, even though over all models, sufficient data are present.
    # In that case, we need to force an ensemble detail level.
    force_ensemble_detail_level = FALSE,
    vimp_method = NA_character_,
    learner = NA_character_,
    data_set_name = NA_character_,
    model_files = NA_character_,
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
      object@ensemble_data_id, "_",
      object@ensemble_run_id, "_",
      object@data_set_name
    ))
  }
)



# .perform_task (evaluation task , NULL) --------------------------------------------
setMethod(
  ".perform_task",
  signature(
    object = "familiarTaskEvaluate",
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
    
    # Set up a delayed 
    data <- methods::new(
      "delayedDataObject",
      data = NULL,
      preprocessing_level = "none",
      outcome_type = outcome_info@outcome_type,
      outcome_info = outcome_info,
      validation = object@validation,
      aggregate_on_load = FALSE
    )
    
    # Set the data_id and run_id for the data itself.
    if (object@force_ensemble_detail_level) {
      data@data_id <- object@ensemble_data_id
      data@run_id <- object@ensemble_run_id
      
    } else {
      data@data_id <- object@data_id
      data@run_id <- object@run_id
    }
    
    # Determine whether model data and run ids should be used for predictions.
    data@defer_to_model_data_and_run_id <- object@get_predictions_at_model_level
    
    # Pass to method that dispatches with dataObject for further processing.
    return(.perform_task(
      object = object,
      data = data,
      experiment_data = experiment_data,
      ...
    ))
  }
)




# .perform_task (evaluation task, dataObject) ----------------------------------
setMethod(
  ".perform_task",
  signature(
    object = "familiarTaskEvaluate",
    data = "dataObject"
  ),
  function(
    object,
    data,
    settings,
    cl = NULL,
    message_indent = 0L,
    return_results = TRUE,
    ...
  ) {
    
    # Signal evaluation start for the current task.
    logger_message(
      paste0(
        "Evaluation: Starting evaluation for the \"", object@learner,
        "\" learner and the \"", object@vimp_method,
        "\" variable importance method for ",
        object@data_set_name, " data (data_id: ",
        object@data_id, "; run_id: ", object@run_id,
        "). This is task ",
        object@task_id, " of ",
        object@n_tasks, "."
      ),
      indent = message_indent,
      verbose = verbose
    )
    
    # Form an ensemble using the associated or provided models.
    
    # Check which detail level should be provided based on the number of
    # available instances for each model.
    fam_ensemble <- methods::new(
      "familiarEnsemble",
      model_list = as.list(object@model_files),
      learner = object@learner,
      vimp_method = object@vimp_method,
      data_id = object@ensemble_data_id,
      run_id = object@ensemble_run_id
    )
    
    # Add package version.
    fam_ensemble <- add_package_version(object = fam_ensemble)
    
    # Load models and prevent auto-detaching.
    fam_ensemble <- load_models(
      object = fam_ensemble,
      suppress_auto_detach = TRUE
    )
    
    # Create a run table
    fam_ensemble@run_table <- list(
      "run_table" = lapply(
        fam_ensemble@model_list,
        function(fam_model) fam_model@run_table
      ),
      "ensemble_data_id" = object@ensemble_data_id,
      "ensemble_run_id" = object@ensemble_run_id
    )
    
    # Complete the ensemble using information provided by the model
    fam_ensemble <- complete_familiar_ensemble(object = fam_ensemble)
    
    # Set evaluation level.
    if (object@force_ensemble_detail_level) {
      detail_level <- "ensemble"
    } else {
      detail_level <- settings$eval$detail_level
    }
    
    # Compute evaluation data.
    evaluation_data <- extract_data(
      object = fam_ensemble,
      data = data,
      cl = cl,
      data_element = settings$eval$evaluation_data_elements,
      time_max = settings$eval$time_max,
      evaluation_times = settings$eval$eval_times,
      sample_limit = settings$eval$sample_limit,
      detail_level = detail_level,
      estimation_type = settings$eval$estimation_type,
      aggregate_results = settings$eval$aggregate_results,
      aggregation_method = settings$eval$aggregation,
      rank_threshold = settings$eval$aggr_rank_threshold,
      ensemble_method = settings$eval$ensemble_method,
      stratification_method = settings$eval$strat_method,
      metric = settings$eval$metric,
      feature_cluster_method = settings$eval$feature_cluster_method,
      feature_cluster_cut_method = settings$eval$feature_cluster_cut_method,
      feature_linkage_method = settings$eval$feature_linkage_method,
      feature_similarity_metric = settings$eval$feature_similarity_metric,
      feature_similarity_threshold = settings$eval$feature_similarity_threshold,
      sample_cluster_method = settings$eval$sample_cluster_method,
      sample_linkage_method = settings$eval$sample_linkage_method,
      sample_similarity_metric = settings$eval$sample_similarity_metric,
      confidence_level = settings$eval$confidence_level,
      bootstrap_ci_method = settings$eval$bootstrap_ci_method,
      dynamic_model_loading = settings$eval$auto_detach,
      icc_type = settings$eval$icc_type,
      message_indent = message_indent + 1L,
      verbose = verbose
    )
    
    # Add additional details.
    evaluation_data@name <- object@data_set_name
    evaluation_data@project_id <- object@project_id
    
    if (!is.na(object@file)) {
      saveRDS(evaluation_data, file = object@file)
    }
    
    if (return_results) {
      return(evaluation_data)
    }
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
  data_id <- run_id <- NULL
  
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
  
  n_min_model_instances <- Inf
  evaluate_external_validation <- evaluate_internal_validation <- evaluate_development <- FALSE
  
  # External validation: the top level has associated validation data.
  if (experiment_data@experiment_setup[main_data_id == 1L]$max_validation_instances > 0L) {
    evaluate_external_validation <- TRUE
    n_min_model_instances <- min(
      c(experiment_data@experiment_setup[main_data_id == 1L]$max_validation_instances),
      n_min_model_instances
    )
  }
  
  # Internal validation: the lowest model ensembling level has associated
  # validation data.
  if (experiment_data@experiment_setup[main_data_id == internal_validation_data_id]$max_validation_instances > 0L) {
    evaluate_internal_validation <- TRUE
    n_min_model_instances <- min(
      c(experiment_data@experiment_setup[main_data_id == internal_validation_data_id]$max_validation_instances),
      n_min_model_instances
    )
  }
  
  # Development data: the lowest model ensembling level has associated
  # development data. NOTE: this should always be the case.
  if (experiment_data@experiment_setup[main_data_id == internal_validation_data_id]$max_training_instances > 0L) {
    evaluate_development <- TRUE
    n_min_model_instances <- min(
      c(experiment_data@experiment_setup[main_data_id == internal_validation_data_id]$max_training_instances),
      n_min_model_instances
    )
  }
  
  # Check model instances and determine if we need to force ensemble detail
  # level for evaluation.
  force_ensemble_detail_level <- n_min_model_instances < 10L
  
  # Use collection tasks to set up the evaluation tasks, including for internal
  # validation.
  evaluate_task_list <- list()
  ii <- 1L
  
  for (jj in seq_along(collect_task_list)) {
    # Initialise file names.
    data_file_names <- NULL
    for (learner in learners) {
      for (vimp_method in vimp_methods) {
        
        ## external validation -------------------------------------------------
        
        if (evaluate_external_validation) {
          # Initialise task.
          evaluate_task <- methods::new(
            "familiarTaskEvaluate",
            data_id = 1L,
            run_id = 1L,
            validation = TRUE,
            ensemble_data_id = collect_task_list[[jj]]@data_id,
            ensemble_run_id = collect_task_list[[jj]]@run_id,
            get_predictions_at_model_level = FALSE,
            force_ensemble_detail_level = force_ensemble_detail_level,
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
        
        if (experiment_data@experiment_setup[main_data_id == internal_validation_data_id]$max_validation_instances > 0L) {
          # Initialise task.
          evaluate_task <- methods::new(
            "familiarTaskEvaluate",
            data_id = collect_task_list[[jj]]@data_id,
            run_id = collect_task_list[[jj]]@run_id,
            validation = TRUE,
            ensemble_data_id = collect_task_list[[jj]]@data_id,
            ensemble_run_id = collect_task_list[[jj]]@run_id,
            get_predictions_at_model_level = TRUE,
            force_ensemble_detail_level = force_ensemble_detail_level,
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
        
        if (evaluate_development) {
          # Initialise task.
          evaluate_task <- methods::new(
            "familiarTaskEvaluate",
            data_id = collect_task_list[[jj]]@data_id,
            run_id = collect_task_list[[jj]]@run_id,
            validation = FALSE,
            ensemble_data_id = collect_task_list[[jj]]@data_id,
            ensemble_run_id = collect_task_list[[jj]]@run_id,
            get_predictions_at_model_level = TRUE,
            force_ensemble_detail_level = force_ensemble_detail_level,
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
          
        } else {
          ..error_reached_unreachable_code("development data are always present.")
        }
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
  
  # Obtain run tables that directly refer to data on which models were trained.
  # First select the data_id related to training.
  train_data_id <- experiment_data@experiment_setup[train == TRUE, ]$main_data_id[1L]
  if (is_empty(train_data_id)) return(NULL)
  
  # Then select the run-tables which contain this data_id as their final
  # (bottom) level.
  run_tables <- .collect_run_tables(iteration_list = experiment_data@iteration_list)
  run_tables <- run_tables[sapply(
    run_tables,
    function(x, data_id) {
      return (tail(x, n = 1L)$data_id == data_id)
    },
    data_id = train_data_id
  )]

  # Iterate over evaluation tasks and add corresponding models based on ensemble
  # data id and run id.
  for (ii in seq_along(evaluate_task_list)) {
    # Select run tables where the ensemble data and run identifiers appear.
    selected_run_tables <- run_tables[sapply(
      run_tables,
      function(x, ensemble_data_id, ensemble_run_id) {
        return(!is_empty(x[data_id == ensemble_data_id & run_id == ensemble_run_id]))
      },
      ensemble_data_id = evaluate_task_list[[ii]]@ensemble_data_id,
      ensemble_run_id = evaluate_task_list[[ii]]@ensemble_run_id
    )]
    
    # Create corresponding model object names.
    evaluate_task_list[[ii]]@model_files <- unname(sapply(
      selected_run_tables,
      function(x, ...) {
        get_object_file_name(
          object_type = "familiarModel",
          data_id = tail(x, n = 1L)$data_id,
          run_id = tail(x, n = 1L)$run_id,
          ...
        )
      },
      learner = evaluate_task_list[[ii]]@learner,
      vimp_method = evaluate_task_list[[ii]]@vimp_method,
      project_id = evaluate_task_list[[ii]]@project_id,
      dir_path = file_paths$mb_dir
    ))
  }

  # train and variable importance tasks ----------------------------------------
  task_list <- .generate_trainer_tasks(
    experiment_data = experiment_data,
    learners = learners,
    vimp_methods = vimp_methods,
    file_paths = file_paths,
    skip_existing = skip_existing,
    ...
  )
  
  return(c(
    task_list,
    evaluate_task_list,
    collect_task_list
  ))
}



.run_evaluation <- function(
    cl,
    tasks,
    message_indent = 0L,
    verbose,
    ...
) {
  
  # Check that any tasks are available for processing.
  if (is_empty(tasks$evaluate) || is_empty(tasks$collect)) return(invisible(FALSE))
  
  # Determine which evaluation tasks need to be performed.
  finished_tasks <- sapply(tasks$evaluate, .file_exists)
  unfinished_tasks <- tasks$evaluate[!finished_tasks]
  finished_tasks <- tasks$evaluate[finished_tasks]
  
  # Process any unfinished tasks.
  if (length(unfinished_tasks) > 0L) {
    ..run_evaluation(
      cl = cl,
      tasks = unfinished_tasks,
      message_indent = message_indent,
      verbose = verbose,
      ...
    )
  }
  
  # Determine which collection tasks are required.
  finished_tasks <- sapply(tasks$collect, .file_exists)
  unfinished_tasks <- tasks$collect[!finished_tasks]
  finished_tasks <- tasks$collect[finished_tasks]
  
  # Process any unfinished tasks.
  if (length(unfinished_tasks) > 0L) {
    ..run_collection(
      tasks = unfinished_tasks,
      message_indent = message_indent,
      verbose = verbose,
      ...
    )
  }
  
  return(invisible(TRUE))
}



..run_evaluation <- function(
    tasks,
    cl,
    settings,
    message_indent = 0L,
    verbose,
    ...
) {
  
  # Message that evaluation is starting.
  logger_message(
    paste0(
      "Evaluation: Starting model evaluation."
    ),
    indent = message_indent,
    verbose = verbose
  )
  
  # Set outer vs. inner loop parallelisation.
  if (settings$eval$do_parallel %in% c("TRUE", "inner")) {
    cl_inner <- cl
    cl_outer <- NULL
    
  } else if (settings$eval$do_parallel %in% c("outer")) {
    cl_inner <- NULL
    cl_outer <- cl
    
    logger_message(
      paste0(
        "Evaluation: Parallel processing is done in the outer loop. ",
        "No progress can be displayed."
      ),
      indent = message_indent,
      verbose = verbose && !is.null(cl_outer)
    )
    
  } else {
    cl_inner <- cl_outer <- NULL
  }
  
  fam_mapply_lb(
    cl = cl,
    assign = "all",
    FUN = .perform_task,
    progress_bar = !is.null(cl_outer),
    object = tasks,
    MoreArgs = list(
      "data" = NULL,
      "return_results" = FALSE,
      "settings" = settings,
      "message_indent" = message_indent + 1L,
      "verbose" = verbose && is.null(cl_outer),
      ...
    )
  )

  # Message that variable importances have been computed.
  logger_message(
    paste0(
      "Evaluation: Models were evaluated.\n"
    ),
    indent = message_indent,
    verbose = verbose
  )
}



..run_collection <- function(
  tasks,
  message_indent = 0L,
  verbose,
  ...
) {
  
  # Message that evaluation is starting.
  logger_message(
    paste0(
      "Evaluation: Starting collection of evaluation datasets."
    ),
    indent = message_indent,
    verbose = verbose
  )
  
  fam_mapply_lb(
    cl = NULL,
    assign = "all",
    FUN = .perform_task,
    progress_bar = verbose,
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
      "Evaluation: Evaluation datasets were collected.\n"
    ),
    indent = message_indent,
    verbose = verbose
  )
}
