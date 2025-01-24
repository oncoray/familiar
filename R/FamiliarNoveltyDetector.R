#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

# .train -----------------------------------------------------------------------
setMethod(
  ".train",
  signature(
    object = "familiarNoveltyDetector",
    data = "dataObject"
  ),
  function(
    object,
    data,
    get_additional_info = FALSE,
    is_pre_processed = FALSE,
    trim_model = TRUE,
    timeout = 60000.0,
    ...
  ) {
    # Train method for novelty detectors.
    
    # Check if the class of object is a subclass of familiarNoveltyDetector..
    if (!is_subclass(class(object)[1L], "familiarNoveltyDetector")) {
      object <- promote_detector(object)
    }

    # Process data, if required.
    data <- process_input_data(
      object = object,
      data = data,
      is_pre_processed = is_pre_processed,
      stop_at = "clustering",
      force_check = TRUE
    )

    # Set the training flag
    can_train <- TRUE

    # Check if there are any data entries. The familiar model cannot be trained
    # otherwise
    if (is_empty(x = data)) can_train <- FALSE

    # Check the number of features in data; if it has no features, the familiar
    # model can not be trained
    if (!has_feature_data(x = data)) can_train <- FALSE

    # Check if the hyperparameters are plausible.
    if (!has_optimised_hyperparameters(object = object)) can_train <- FALSE

    # Train a new model based on data.
    if (can_train) object <- ..train(object = object, data = data)

    # Extract information required for external use.
    if (get_additional_info) {
      # Add column data
      object <- add_data_column_info(
        object = object,
        data = data
      )
    }

    if (trim_model) object <- trim_model(object = object, timeout = timeout)

    # Empty slots if a model can not be trained.
    if (!can_train) {
      object@required_features <- NULL
      object@model_features <- NULL
    }

    return(object)
  }
)



# show -------------------------------------------------------------------------
setMethod(
  "show",
  signature(object = "familiarNoveltyDetector"),
  function(object) {
    # Make sure the model object is updated.
    object <- update_object(object = object)

    if (!model_is_trained(object)) {
      cat(paste0(
        "A ", object@learner, " novelty detector (class: ", class(object)[1L],
        ") that was not successfully trained (", 
        .familiar_version_string(object), ").\n"
      ))
      
      return(invisible(NULL))
    }
    
    # Describe the learner and the version of familiar.
    message_str <- paste0(
      "A ", object@learner, " novelty detector (class: ", class(object)[1L],
      "; ", .familiar_version_string(object), ")"
    )
    
    # Describe the package(s), if any
    if (!is.null(object@package)) {
      message_str <- c(
        message_str,
        paste0(" trained using "),
        paste_s(mapply(
          ..message_package_version, 
          x = object@package, 
          version = object@package_version
        )),
        ifelse(length(object@package) > 1L, " packages", " package")
      )
    }
    
    # Complete message and write.
    message_str <- paste0(c(message_str, ".\n"), collapse = "")
    cat(message_str)
    
    cat(paste0("\n--------------- Detector details ---------------\n"))
    
    # Model details
    if (object@is_trimmed) {
      cat(object@trimmed_function$show, sep = "\n")
    } else {
      show(object@model)
    }
    
    cat(paste0("---------------------------------------------\n"))
    
    # Details concerning hyperparameters.
    cat("\nThe novelty detector was trained using the following hyperparameters:\n")
    invisible(lapply(
      names(object@hyperparameters),
      function(x, object) {
        cat(paste0("  ", x, ": ", object@hyperparameters[[x]], "\n"))
      },
      object = object
    ))
    
    # Details concerning model features:
    cat("\nThe following features were used for the novelty detector:\n")
    lapply(
      object@model_features, 
      function(x, object) show(object@feature_info[[x]]),
      object = object
    )
    
    # Check package version.
    check_package_version(object)
  }
)



# require_package --------------------------------------------------------------
setMethod(
  "require_package",
  signature(x = "familiarNoveltyDetector"),
  function(
    x, 
    purpose = NULL,
    message_type = "error",
    ...
  ) {
    # Skip if no package is required.
    if (is_empty(x@package)) return(invisible(TRUE))

    # Set standard purposes for common uses.
    if (!is.null(purpose)) {
      if (purpose %in% c("train", "predict")) {
        purpose <- switch(
          purpose,
          "train" = "to train a novelty detector",
          "predict" = "to assess novelty",
          "show" = "to capture output"
        )
      }
    }

    return(invisible(.require_package(
      x = x@package, 
      purpose = purpose, 
      message_type = message_type
    )))
  }
)



# set_package_version ----------------------------------------------------------
setMethod(
  "set_package_version",
  signature(object = "familiarNoveltyDetector"),
  function(object) {
    # Do not add package versions if there are no packages.
    if (is_empty(object@package)) return(object)

    # Obtain package versions.
    object@package_version <- sapply(
      object@package, 
      function(x) (as.character(utils::packageVersion(x)))
    )

    return(object)
  }
)



# check_package_version --------------------------------------------------------
setMethod(
  "check_package_version", 
  signature(object = "familiarNoveltyDetector"),
  function(object) {
    # Check whether installed packages are outdated or newer.
    .check_package_version(
      name = object@package,
      version = object@package_version,
      when = "when creating the novelty detector"
    )
  }
)



# model_is_trained (familiarNoveltyDetector) -----------------------------------
setMethod(
  "model_is_trained",
  signature(object = "familiarNoveltyDetector"),
  function(object) {
    # Check if a model was trained
    if (is.null(object@model)) {
      # Check if a model is present
      return(FALSE)
      
    } else {
      # Assume that the model is present if it is not specifically stated using
      # the model_trained element
      return(TRUE)
    }
  }
)



# is_available (familiarNoveltyDetector)----------------------------------------
setMethod(
  "is_available", 
  signature(object = "familiarNoveltyDetector"),
  function(object, ...) {
    return(FALSE)
  }
)



# get_default_hyperparameters (familiarNoveltyDetector) ------------------------
setMethod(
  "get_default_hyperparameters",
  signature(object = "familiarNoveltyDetector"),
  function(object, ...) {
    return(list())
  }
)



# ..train (familiarNoveltyDetector, dataObject) --------------------------------
setMethod(
  "..train",
  signature(
    object = "familiarNoveltyDetector",
    data = "dataObject"
  ),
  function(object, data, ...) {
    # Set a NULL model
    object@model <- NULL

    return(object)
  }
)



# ..train (familiarNoveltyDetector, NULL) --------------------------------------
setMethod(
  "..train",
  signature(
    object = "familiarNoveltyDetector",
    data = "NULL"
  ),
  function(object, data, ...) {
    # Set a NULL model
    object@model <- NULL

    return(object)
  }
)



# ..predict (familiarNoveltyDetector, dataObject) ------------------------------
setMethod(
  "..predict",
  signature(
    object = "familiarNoveltyDetector",
    data = "dataObject"
  ),
  function(object, data, ...) {
    return(NULL)
  }
)



# trim_model (familiarNoveltyDetector) -----------------------------------------
setMethod(
  "trim_model",
  signature(object = "familiarNoveltyDetector"),
  function(object, timeout = 60000.0, ...) {
    # Do not trim the model if there is nothing to trim.
    if (!model_is_trained(object)) return(object)

    # Trim the model.
    trimmed_object <- .trim_model(object = object)

    # Skip further processing if the model object was not trimmed.
    if (!trimmed_object@is_trimmed) return(object)

    # Go over different functions.
    trimmed_object <- .replace_broken_functions(
      object = object,
      trimmed_object = trimmed_object,
      timeout = timeout
    )

    return(trimmed_object)
  }
)


# .trim_model (familiarNoveltyDetector) ----------------------------------------
setMethod(
  ".trim_model",
  signature(object = "familiarNoveltyDetector"),
  function(object, ...) {
    # Default method for models that lack a more specific method.
    return(object)
  }
)


# add_package_version (familiarModel) ------------------------------------------
setMethod(
  "add_package_version",
  signature(object = "familiarNoveltyDetector"),
  function(object) {
    # Set version of familiar
    return(.add_package_version(object = object))
  }
)


# add_data_column_info (familiarNoveltyDetector) -------------------------------
setMethod(
  "add_data_column_info",
  signature(object = "familiarNoveltyDetector"),
  function(
    object, 
    data = NULL, 
    sample_id_column = NULL, 
    batch_id_column = NULL, 
    series_id_column = NULL
  ) {
    # Don't determine new column information if this information is already
    # present.
    if (!is.null(object@data_column_info)) return(object)

    # Don't determine new column information if this information can be
    # inherited from a dataObject.
    if (is(data, "dataObject")) {
      if (!is_empty(data@data_column_info)) {
        object@data_column_info <- data@data_column_info

        return(object)
      }
    }

    # Load settings to find identifier columns
    settings <- get_settings()

    # Read from settings. If not set, these will be NULL.
    if (is.null(sample_id_column)) sample_id_column <- settings$data$sample_col
    if (is.null(batch_id_column)) batch_id_column <- settings$data$batch_col
    if (is.null(series_id_column)) series_id_column <- settings$data$series_col

    # Replace any missing.
    if (is.null(sample_id_column)) sample_id_column <- NA_character_
    if (is.null(batch_id_column)) batch_id_column <- NA_character_
    if (is.null(series_id_column)) series_id_column <- NA_character_

    # Repetition column ids are only internal.
    repetition_id_column <- NA_character_

    # Create table
    data_info_table <- data.table::data.table(
      "type" = c("batch_id_column", "sample_id_column", "series_id_column", "repetition_id_column"),
      "internal" = get_id_columns(),
      "external" = c(batch_id_column, sample_id_column, series_id_column, repetition_id_column)
    )

    # Combine into one table and add to object
    object@data_column_info <- data_info_table

    return(object)
  }
)



# set_model_features (familiarNoveltyDetector)----------------------------------
setMethod(
  "set_model_features",
  signature(object = "familiarNoveltyDetector"),
  function(
    object, 
    signature_features = NULL, 
    minimise_footprint = FALSE, 
    ...
  ) {
    
    if (is.null(signature_features)) {
      # Get signature features. This is a list features after clustering.
      signature_features <- get_signature_features(object = object)
    }
    
    # Find novelty features. The resulting list of features are features prior
    # to clustering.
    novelty_features <- find_novelty_features(
      model_features = signature_features,
      feature_info_list = object@feature_info
    )
    
    if (minimise_footprint) {
      # Find only features that are required for running the model.
      required_features <- novelty_features
      
    } else {
      # Find features that are required for processing the data.
      required_features <- get_required_features(
        x = novelty_features,
        is_clustered = FALSE,
        feature_info_list = object@feature_info
      )
    }
    
    # Select only necessary feature info objects.
    available_feature_info <- names(object@feature_info) %in% required_features
    object@feature_info <- object@feature_info[available_feature_info]
    
    # Set feature-related attribute slots
    object@required_features <- required_features
    object@model_features <- novelty_features
    
    return(object)
  }
)



# get_signature_features (familiarNoveltyDetector)------------------------------
setMethod(
  "get_signature_features",
  signature(object = "familiarNoveltyDetector"),
  function(
    object
  ) {
    # Attempt to get signature directly from the object.
    if (!is_empty(object@model_features)) {
      # If model features have been set (i.e. using set_model_features), 
      # return those features instead. Since the model_features attribute stores
      # feature names prior to clustering, make sure to update these.
      return(features_after_clustering(
        features = object@model_features,
        feature_info_list = object@feature_info
      ))
    }
    
    # Get signature based on the stored feature information.
    return(.get_signature_features(object))
  }
)



# set_object_name (familiarNoveltyDetector) ------------------------------------

#' @title Set the name of a `familiarNoveltyDetector` object.
#'
#' @description Set the `name` slot using the object name.
#'
#' @param x A `familiarNoveltyDetector` object.
#'
#' @return A `familiarNoveltyDetector` object with a generated or a provided name.
#' @md
#' @keywords internal
setMethod(
  "set_object_name",
  signature(x = "familiarNoveltyDetector"),
  function(x, new = NULL) {
    
    if (is.null(x@project_id) && is.null(new)) {
      # Generate a random object name. A project_id of 0 means that the objects
      # was auto-generated (i.e. through object conversion). We randomly
      # generate characters and add a time stamp, so that collision is
      # practically impossible.
      slot(object = x, name = "name") <- paste0(
        as.character(as.numeric(format(Sys.time(), "%H%M%S"))),
        "_", rstring(n = 20L)
      )
      
    } else if (is.null(new)) {
      # Generate a sensible object name.
      slot(object = x, name = "name") <- get_object_name(object = x)
      
    } else {
      slot(object = x, name = "name") <- new
    }
    
    return(x)
  }
)



# get_object_name (model) ------------------------------------------------------
setMethod(
  "get_object_name",
  signature(object = "familiarNoveltyDetector"),
  function(object, abbreviated = FALSE) {
    
    if (abbreviated) {
      # Create an abbreviated name
      model_name <- paste0("model.", object@data_id, ".", object@run_id)
      
    } else {
      # Create the full name of the model
      model_name <- get_object_file_name(
        learner = object@learner,
        vimp_method = object@vimp_method,
        project_id = object@project_id,
        data_id = object@data_id,
        run_id = object@run_id,
        object_type = "familiarNoveltyDetector",
        with_extension = FALSE
      )
    }
    
    return(model_name)
  }
)
