#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


load_experiment_data <- function(x, file_paths) {
  # This function restores the content of the experimentData object to file
  # system - basically allowing for a reproducible hot start.
  
  # Attempt to load from file.
  project_id <- NULL
  if (is.character(x)) {
    project_id <- gsub(
      x = basename(x),
      pattern = "[[:alpha:]]|[.]RDS$|[_]",
      replacement = ""
    )
    
    # Read from file system.
    x <- readRDS(x)
  } 
  
  # Users may have added a configuration
  if (is.list(x)) {
    if (all(c("iteration_list", "experiment_setup") %in% names(x))) {
      x <- methods::new(
        "experimentData",
        iteration_list = x$iteration_list,
        experiment_setup = x$experiment_setup,
        project_id = project_id
      )
    }
  }
  
  # Expect that the file is an experimentData object.
  if (!is(x, "experimentData")) {
    ..error(
      paste0(
        "An experimentData object was expected. Found: a ",
        paste_s(class(x)), " object."
      ),
      error_class = "input_argument_error"
    )
  }
  
  # Update the experimentData object.
  x <- update_object(x)
  
  # Start writing the contents of the object to the working directory to deploy
  # from there.
  if (!is.null(x@experiment_setup) && !is.null(x@iteration_list)) {
    
    # Set file name
    file_name <- .get_iteration_file_name(
      file_paths = file_paths,
      project_id = x@project_id
    )
    
    # Check if the directory exists, and create it otherwise.
    if (!dir.exists(file_paths$iterations_dir)) {
      dir.create(file_paths$iterations_dir, recursive = TRUE)
    } 
    
    # Save both files to the expected location.
    saveRDS(
      list(
        "iteration_list" = x@iteration_list,
        "experiment_setup" = x@experiment_setup
      ),
      file = file_name
    )
  }
  
  # Start writing feature information.
  if (!is.null(x@feature_info)) {
    
    for (feature_info_name in names(x@feature_info)) {
      feature_info <- x@feature_info[[feature_info_name]]
      
      # Set file name.
      if (feature_info_name == "generic") {
        file_name <- get_object_file_name(
          object_type = "genericFeatureInfo",
          project_id = feature_info[[1L]]@project_id,
          dir_path = file_paths$process_data_dir
        )
        
      } else {
        file_name <- get_object_file_name(
          object_type = "featureInfo",
          project_id = feature_info[[1L]]@project_id,
          data_id = feature_info[[1L]]@data_id,
          run_id = feature_info[[1L]]@run_id,
          dir_path = file_paths$process_data_dir
        )
      }
      
      # Check if the directory exists, and create it otherwise.
      if (!dir.exists(file_paths$process_data_dir)) {
        dir.create(file_paths$process_data_dir, recursive = TRUE)
      } 
    
      # Write to file.
      saveRDS(feature_info, file = file_name)
    }
  }
  
  # Write variable importance information.
  if (!is.null(x@vimp_table_list)) {
    for (vimp_table in x@vimp_table_list) {
      
      # Set file name
      file_name <- get_object_file_name(
        object_type = "vimpTable",
        data_id = vimp_table@data_id,
        run_id = vimp_table@run_id,
        vimp_method = vimp_table@vimp_method,
        project_id = vimp_table@project_id,
        dir_path = file_paths$vimp_dir
      )
      
      # Check if the directory exists, and create it otherwise.
      if (!dir.exists(dirname(file_name))) {
        dir.create(dirname(file_name), recursive = TRUE)
      } 
      
      # Write to file.
      saveRDS(vimp_table, file = file_name)
    }
  }
  
  return(x)
}



set_experiment_data <- function(
    x = NULL,
    project_id,
    experiment_setup = NULL,
    iteration_list = NULL,
    feature_info = NULL,
    vimp_hyperparameter_list = NULL,
    vimp_table_list = NULL
) {

  # Create new object.
  if (!is(x, "experimentData")) {
    x <- methods::new(
      "experimentData",
      project_id = project_id
    )
    
    # Add package version
    x <- add_package_version(x)
  }
  
  if (is.null(x@experiment_setup) && !is.null(experiment_setup)) x@experiment_setup <- experiment_setup
  if (is.null(x@iteration_list) && !is.null(iteration_list)) x@iteration_list <- iteration_list
  if (is.null(x@feature_info) && !is.null(feature_info)) x@feature_info <- feature_info
  if (is.null(x@vimp_hyperparameter_list) && !is.null(vimp_hyperparameter_list)) x@vimp_hyperparameter_list <- vimp_hyperparameter_list
  if (is.null(x@vimp_table_list) && !is.null(vimp_table_list)) x@vimp_table_list <- vimp_table_list
  
  return(x)
}



# show (experimentData) --------------------------------------------------------
setMethod(
  "show",
  signature(object = "experimentData"),
  function(object) {
    
    # Make sure the model object is updated.
    object <- update_object(object = object)
    
    # Experiment data is always present.
    content_str <- c("experiment data")
    
    # Check if feature info is present.
    if (!is.null(object@feature_info)) {
      if (length(object@feature_info) > 1L) {
        content_str <- c(
          content_str,
          "basic and extended feature information"
        )
        
      } else {
        content_str <- c(
          content_str,
          "basic feature information"
        )
      }
    }
    
    # Check if variable importance information is present.
    if (!is.null(object@vimp_table_list)) {
      content_str <- c(
        content_str,
        paste0("variable importance (", paste_s(names(object@vimp_table_list)), ")")
      )
    }
    
    cat(paste0(
      "Experiment data object (", .familiar_version_string(object), ") with project id ",
      object@project_id, " containing ", paste_s(content_str), ".\n"
    ))
  }
)


  
# add_package_version (experiment data) ----------------------------------------
setMethod(
  "add_package_version",
  signature(object = "experimentData"),
  function(object) {
    
    # Set version of familiar
    return(.add_package_version(object = object))
  }
)
