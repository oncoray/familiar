# test_create_vimp_method (data.table) -----------------------------------------
setMethod(
  "test_create_vimp_method",
  signature(data = "data.table"),
  function(
    data, 
    vimp_method, 
    vimp_method_parameter_list = list(),
    ...
  ) {
    # Convert data to dataObject.
    data <- do.call(
      as_data_object, 
      args = c(
        list("data" = data),
        list(...)
      )
    )
    
    return(do.call(
      test_create_vimp_method, 
      args = c(
        list(
          "data" = data,
          "vimp_method" = vimp_method,
          "vimp_method_parameter_list" = vimp_method_parameter_list
        ),
        list(...)
      )
    ))
  }
)



# test_create_vimp_method (dataObject) -----------------------------------------
setMethod(
  "test_create_vimp_method",
  signature(data = "dataObject"),
  function(
    data,
    data_bypass = NULL,
    vimp_method, 
    vimp_method_parameter_list = list(), 
    ...
  ) {
    # The bypass data allows for bypassing important aspects of the
    # pre-processing pipeline, e.g. the preprocessing checks. This enables
    # testing of very rare cases where preprocessing may run fine, but the
    # subsample does not allow for training.
    if (is.null(data_bypass)) data_bypass <- data
    
    # Prepare setting ----------------------------------------------------------
    
    # Reconstitute settings from the data.
    settings <- extract_settings_from_data(data)
    
    # Update some missing settings that can be fixed within this method.
    settings$data$train_cohorts <- unique(data@data[[get_id_columns(single_column = "batch")]])
    
    # Parse the remaining settings that are important. Remove outcome_type from
    # ... This prevents an error caused by multiple matching arguments.
    dots <- list(...)
    dots$parallel <- NULL
    dots$vimp_method <- NULL
    
    if (!is.null(dots$signature)) settings$data$signature <- dots$signature
    
    settings <- do.call(
      .parse_general_settings,
      args = c(
        list(
          "settings" = settings,
          "data" = data_bypass@data,
          "parallel" = FALSE,
          "vimp_method" = vimp_method,
          "learner" = "glm"
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
      vimp_method = vimp_method,
      names_only = FALSE
    )
    
    # Update with user-provided settings.
    param_list <- .update_hyperparameters(
      parameter_list = param_list,
      user_list = vimp_method_parameter_list
    )
    
    # Determine which hyperparameters still need to be specified.
    unset_parameters <- sapply(
      param_list,
      function(hyperparameter_entry) hyperparameter_entry$randomise
    )
    
    # Mark sign-size as set, as it is not used for variable importance.
    if ("sign_size" %in% names(unset_parameters)) {
      unset_parameters["sign_size"] <- FALSE
    }
    
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
    
    
    # Prepare vimp object ------------------------------------------------------
    
    # Get required features.
    required_features <- get_required_features(
      x = data,
      feature_info_list = feature_info
    )
    
    # Create a familiar variable importance method.
    object <- methods::new(
      "familiarVimpMethod",
      outcome_type = data@outcome_type,
      vimp_method = vimp_method,
      hyperparameters = param_list,
      outcome_info = data@outcome_info,
      feature_info = feature_info[required_features],
      required_features = required_features
    )
    
    # Promote object to correct subclass.
    object <- promote_vimp_method(object)
    
    # Return in list.
    return(object)
  }
)
