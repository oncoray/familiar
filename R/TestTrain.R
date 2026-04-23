#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL



# test_train (generic) ---------------------------------------------------------
setGeneric(
  "test_train",
  function(data, ...) standardGeneric("test_train")
)



# test_train (data.table) ------------------------------------------------------
setMethod(
  "test_train",
  signature(data = "data.table"),
  function(
    data,
    data_bypass = NULL,
    learner,
    hyperparameter_list = list(),
    create_bootstrap = FALSE,
    ...
  ) {
    if (!is.null(data_bypass)) {
      # Convert data_bypass to dataObject.
      data_bypass <- do.call(
        as_data_object,
        args = c(
          list("data" = data_bypass),
          list(...)
        )
      )
    }

    # Convert data to dataObject.
    data <- do.call(
      as_data_object,
      args = c(
        list("data" = data),
        list(...)
      )
    )

    return(do.call(
      test_train, 
      args = c(
        list(
          "data" = data,
          "data_bypass" = data_bypass,
          "learner" = learner,
          "hyperparameter_list" = hyperparameter_list,
          "create_bootstrap" = create_bootstrap
        ),
        list(...)
      )
    ))
  }
)



# test_train (dataObject) ------------------------------------------------------
setMethod(
  "test_train",
  signature(data = "dataObject"),
  function(
    data,
    data_bypass = NULL,
    learner,
    hyperparameter_list = list(),
    create_bootstrap = FALSE,
    create_novelty_detector = FALSE,
    create_naive = FALSE,
    cl = NULL,
    trim_model = FALSE,
    verbose = FALSE,
    ...
  ) {
    # The bypass data allows for bypassing important aspects of the
    # pre-processing pipeline, e.g. the preprocessing checks. This enables
    # testing of very rare cases where preprocessing may run fine, but the
    # subsample does not allow for training.
    if (is.null(data_bypass)) data_bypass <- data
    
    # Prepare settings ---------------------------------------------------------
    
    # Reconstitute settings from the data.
    settings <- extract_settings_from_data(data_bypass)
    
    # Update some missing settings that can be fixed within this method.
    settings$data$train_cohorts <- unique(data_bypass@data[[get_id_columns(single_column = "batch")]])
    
    # Parse the remaining settings that are important. Remove outcome_type from
    # ... This prevents an error caused by multiple matching arguments.
    dots <- list(...)
    dots$parallel <- NULL
    dots$vimp_method <- NULL

    # Determine if a naive model should be forced.
    vimp_method <- ifelse(create_naive, "no_features", "none")
    
    settings <- do.call(
      .parse_general_settings,
      args = c(
        list(
          "settings" = settings,
          "data" = data_bypass@data,
          "parallel" = FALSE,
          "vimp_method" = vimp_method,
          "learner" = learner
        ),
        dots
      )
    )
    
    # Push settings to the backend as some functions require this.
    .assign_settings_to_global(settings = settings)
    
    
    # Prepare hyperparameters --------------------------------------------------
    
    # Get default hyperparameters.
    param_list <- .get_preset_hyperparameters(
      data = data,
      learner = learner,
      names_only = FALSE
    )
    
    # Update with user-provided settings.
    param_list <- .update_hyperparameters(
      parameter_list = param_list,
      user_list = hyperparameter_list
    )
    
    # Determine which hyperparameters still need to be specified.
    unset_parameters <- sapply(
      param_list,
      function(hyperparameter_entry) hyperparameter_entry$randomise
    )
    
    # Raise an error if any hyperparameters were not set.
    if (any(unset_parameters)) {
      ..error(paste0(
        "The following hyperparameters need to be specified: ",
        paste_s(names(unset_parameters)[unset_parameters])
      ))
    }
    
    # Obtain the final list of hyperparameters.
    param_list <- lapply(
      param_list, 
      function(hyperparameter_entry) hyperparameter_entry$init_config
    )
    
    
    # Create feature information list ------------------------------------------
    
    feature_info_task <- methods::new(
      "familiarTaskFeatureInfo"
    )
    
    # Feature information objects are created from the bypass dataset.
    feature_info <- .perform_task(
      object = feature_info_task,
      data = data_bypass,
      settings = settings,
      verbose = verbose
    )
    
    
    # Create learner -----------------------------------------------------------
    
    train_task <- methods::new(
      "familiarTaskTrain",
      vimp_method = vimp_method,
      learner = learner
    )
    
    if (create_bootstrap) {
      data <- select_data_from_samples(
        data = data,
        samples = fam_sample(
          x = data@data,
          replace = TRUE,
          seed = 19L
        )
      )
    }
    
    # Learners are trained using the actual data.
    object <- .perform_task(
      object = train_task,
      data = data,
      settings = settings,
      feature_info_list = feature_info,
      hyperparameters = param_list,
      novelty_detector = ifelse(create_novelty_detector, "isolation_forest", "none"),
      detector_parameters = NULL,
      trim_model = trim_model,
      verbose = verbose
    )
    
    return(object)
  }
)
