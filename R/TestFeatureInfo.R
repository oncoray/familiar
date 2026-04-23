test_create_generic_info <- function(
    data
) {
  # Setup feature info task.
  feature_info_task <- methods::new(
    "familiarTaskGenericFeatureInfo"
  )
  
  # Feature information objects are created from the bypass dataset.
  feature_info <- .perform_task(
    object = feature_info_task,
    data = data
  )
  
  return(feature_info)
}



test_create_feature_info <- function(
    data,
    signature = NULL,
    ...
) {
  # This creates a list of featureInfo objects, with processing, based on data.
  # This code is primarily used within unit tests.
  
  # Reconstitute settings from the data.
  settings <- extract_settings_from_data(data = data)
  
  # Update some missing settings that can be fixed within this method.
  settings$data$train_cohorts <- unique(data@data[[get_id_columns(single_column = "batch")]])
  
  # Parse the remaining settings that are important.
  settings <- do.call(
    .parse_general_settings,
    args = c(
      list(
        "settings" = settings,
        "data" = data@data
      ),
      list(...)
    )
  )
  
  # Setup feature info task.
  feature_info_task <- methods::new(
    "familiarTaskFeatureInfo"
  )
  
  # Feature information objects are created from the bypass dataset.
  feature_info <- .perform_task(
    object = feature_info_task,
    data = data,
    settings = settings
  )
  
  return(feature_info)
}
