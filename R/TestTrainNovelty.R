# test_train_novelty_detector (generic) ----------------------------------------
setGeneric("test_train_novelty_detector", function(data, ...) standardGeneric("test_train_novelty_detector"))



# test_train_novelty_detector (data.table) -------------------------------------
setMethod(
  "test_train_novelty_detector",
  signature(data = "data.table"),
  function(
    data, 
    data_bypass = NULL,
    detector,
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
      test_train_novelty_detector,
      args = c(
        list(
          "data" = data,
          "data_bypass" = data_bypass,
          "detector" = detector,
          "hyperparameter_list" = hyperparameter_list,
          "create_bootstrap" = create_bootstrap
        ),
        list(...)
      )
    ))
  }
)


# test_train_novelty_detector (dataObject) -------------------------------------
setMethod(
  "test_train_novelty_detector",
  signature(data = "dataObject"),
  function(
    data,
    data_bypass = NULL,
    detector,
    hyperparameter_list = list(),
    create_bootstrap = FALSE,
    cl = NULL,
    trim_model = FALSE,
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
    dots$hyperparameter <- NULL
    
    # Create setting_hyperparam so that it can be parsed correctly.
    if (!detector %in% names(hyperparameter_list) && length(hyperparameter_list) > 0L) {
      setting_hyperparam <- list()
      setting_hyperparam[[detector]] <- hyperparameter_list
    } else {
      setting_hyperparam <- hyperparameter_list
    }
    
    settings <- do.call(
      .parse_general_settings,
      args = c(
        list(
          "settings" = settings,
          "data" = data_bypass@data,
          "parallel" = FALSE,
          "vimp_method" = "none",
          "learner" = "glm",
          "novelty_detector" = detector,
          "detector_parameters" = setting_hyperparam
        ),
        dots
      )
    )
    
    # Push settings to the backend.
    .assign_settings_to_global(settings = settings)
    
    
    # Prepare hyperparameters --------------------------------------------------
    
    # Get default hyperparameters.
    param_list <- .get_preset_hyperparameters(
      data = data,
      detector = detector,
      names_only = FALSE
    )
    
    # Update with user-provided settings.
    param_list <- .update_hyperparameters(
      parameter_list = param_list,
      user_list = settings$mb$detector_parameters[[detector]]
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
      settings = settings
    )
    
    
    # Create novelty detector --------------------------------------------------
    
    detector_task <- methods::new(
      "familiarTaskTrainNovelty",
      learner = detector,
      vimp_method = "none"
    )
    
    # Create bootstraps.
    if (create_bootstrap) {
      data <- select_data_from_samples(
        data = data,
        samples = fam_sample(
          x = data@data,
          replace = TRUE
        )
      )
    }
    
    # Train novelty detector and add to model.
    object <- .perform_task(
      object = detector_task,
      data = data,
      settings = settings,
      feature_info_list = feature_info,
      hyperparameters = param_list,
      trim_model = trim_model
    )
    
    return(object)
  }
)
