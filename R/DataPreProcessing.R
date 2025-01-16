.determine_preprocessing_parameters <- function(
    cl = NULL,
    data,
    feature_info_list,
    settings,
    message_indent = 0L,
    verbose = FALSE
) {
  
  if (!is(data, "dataObject")) {
    ..error_reached_unreachable_code(
      ".determine_preprocessing_parameters: data is not a dataObject."
    )
  }
  
  if (is_empty(data)) ..error_data_set_is_empty()
  if (!has_feature_data(data)) ..error_data_set_has_no_features()
  
  # Remove samples with missing outcome data -----------------------------------
  n_samples_current <- get_n_samples(data)
  
  logger_message(
    paste0("Pre-processing: ", n_samples_current, " samples were initially available."),
    indent = message_indent,
    verbose = verbose
  )
  
  # Remove all samples with missing outcome data
  data <- filter_missing_outcome(data = data, is_validation = FALSE)
  if (is_empty(data)) ..error_data_set_has_no_outcome()
  
  n_samples_remain <- get_n_samples(data)
  n_samples_removed <- n_samples_current - n_samples_remain
  
  logger_message(
    paste0(
      "Pre-processing: ", n_samples_removed,
      " samples were removed because of missing outcome data. ",
      n_samples_remain, " samples remain."
    ),
    indent = message_indent,
    verbose = verbose
  )
  
  
  #Remove features with a large fraction of missing values ---------------------
  n_features_current <- get_n_features(data)
  logger_message(
    paste0("Pre-processing: ", n_features_current, " features were initially available."),
    indent = message_indent,
    verbose = verbose
  )
  
  # Determine the fraction of missing values
  feature_info_list <- add_missing_value_fractions(
    cl = cl,
    feature_info_list = feature_info_list,
    data = data,
    threshold = settings$prep$feature_max_fraction_missing
  )
  
  # Find features that are not missing too many values.
  available_features <- get_available_features(feature_info_list = feature_info_list)
  
  # Remove features with a high fraction of missing values
  data <- filter_features(data = data, available_features = available_features)
  
  if (!has_feature_data(data)) {
    ..error(
      paste0(
        "The provided dataset lacks features with sufficient available values. ",
        "Please investigate missing values in the dataset or increase the missingness ",
        "threshold by increasing the feature_max_fraction_missing configuration parameter."
      ),
      error_class = "dataset_error"
    )
  } 
  
  # Message how many features were removed
  logger_message(
    paste0(
      "Pre-processing: ", n_features_current - length(available_features),
      " features were removed because of a high fraction of missing values. ",
      length(available_features), " features remain."
    ),
    indent = message_indent,
    verbose = verbose
  )
  
  n_samples_current <- n_samples_remain
  
  
  # Remove samples with a large fraction of missing values ---------------------
  
  data <- filter_bad_samples(
    data = data,
    threshold = settings$prep$sample_max_fraction_missing
  )
  
  if (is_empty(data)) {
    ..error(
      paste0(
        "The provided dataset lacks samples with sufficient available feature values. ",
        "Please investigate missing values in the dataset or increase the missingness ",
        "threshold by increasing the sample_max_fraction_missing configuration parameter."
      ),
      error_class = "dataset_error"
    )
  }
  
  # Message how many samples were removed
  n_samples_remain <- get_n_samples(data)
  
  logger_message(
    paste0(
      "Pre-processing: ", n_samples_current - n_samples_remain,
      " samples were removed because of missing feature data. ",
      n_samples_remain, " samples remain."
    ),
    indent = message_indent,
    verbose = verbose
  )
  
  n_features_current <- length(available_features)
  
  
  # Remove invariant features --------------------------------------------------
  
  # Filter features that are invariant.
  feature_info_list  <- find_invariant_features(
    cl = cl,
    feature_info_list = feature_info_list,
    data = data
  )
  
  # Find available features.
  available_features <- get_available_features(feature_info_list = feature_info_list)
  
  # Remove invariant features from the data
  data <- filter_features(data = data, available_features = available_features)
  
  if (!has_feature_data(data)) {
    ..error(
      paste0(
        "Remaining features in the dataset only have a single value for all samples ",
        "and cannot be used for training."
      ),
      error_class = "dataset_error"
    )
  }
  
  # Message number of features removed by the no-variance filter.
  logger_message(
    paste0(
      "Pre-processing: ", n_features_current - length(available_features),
      " features were removed due to invariance. ",
      length(available_features), " features remain."
    ),
    indent = message_indent,
    verbose = verbose
  )
  
  
  # Add feature distribution data ----------------------------------------------
  logger_message(
    paste0("Pre-processing: Adding value distribution statistics to features."),
    indent = message_indent,
    verbose = verbose
  )
    
  # Add feature distribution data
  feature_info_list <- compute_feature_distribution_data(
    cl = cl,
    feature_info_list = feature_info_list,
    data = data
  )
  
  
  # Transform features ---------------------------------------------------------
  logger_message(
    "Pre-processing: Performing transformations to normalise feature value distributions.",
    indent = message_indent,
    verbose = verbose && settings$prep$transform_method != "none"
  )
  
  # Add skeletons to the feature information list.
  feature_info_list <- create_transformation_parameter_skeleton(
    feature_info_list = feature_info_list,
    transformation_method = settings$prep$transform_method,
    transformation_optimisation_criterion = settings$prep$transformation_optimisation_criterion,
    transformation_gof_p_value = settings$prep$transformation_gof_test_p_value
  )
  
  # Add transformation parameters to the feature information list
  feature_info_list <- add_transformation_parameters(
    cl = cl,
    feature_info_list = feature_info_list,
    data = data,
    verbose = verbose
  )
  
  # Apply transformation.
  data <- transform_features(
    data = data,
    feature_info_list = feature_info_list
  )
  
  logger_message(
    "Pre-processing: Feature distributions have been transformed for normalisation.",
    indent = message_indent,
    verbose = verbose & settings$prep$transform_method != "none"
  )
  

  # Remove low-variance features -----------------------------------------------
  if ("low_variance" %in% settings$prep$filter_method) {
    n_features_current <- length(available_features)
    
    # Filter features that are invariant.
    feature_info_list <- find_low_variance_features(
      cl = cl,
      feature_info_list = feature_info_list,
      data = data,
      settings = settings
    )
    
    # Check available features.
    available_features <- get_available_features(feature_info_list = feature_info_list)
    
    # Remove invariant features from the data
    data <- filter_features(data = data, available_features = available_features)
    
    if (!has_feature_data(data)) {
      ..error(
        paste0(
          "Remaining features in the dataset have a variance that is lower than the threshold ",
          "and were therefore all removed. Please investigate your data, or increase the threshold ",
          "through the low_var_minimum_variance_threshold configuration parameter."
        ),
        error_class = "dataset_error"
      )
    } 
    
    # Message number of features removed by the low-variance filter.
    logger_message(
      paste0(
        "Pre-processing: ", n_features_current - length(available_features),
        " features were removed due to low variance. ",
        length(available_features), " features remain."
      ),
      indent = message_indent,
      verbose = verbose
    )
  }
  
  
  
  # Normalise features ---------------------------------------------------------
  logger_message(
    "Pre-processing: Extracting normalisation parameters from feature data.",
    indent = message_indent,
    verbose = verbose && settings$prep$normalisation_method != "none"
  )
  
  
  # Add skeletons to the feature information list.
  feature_info_list <- create_normalisation_parameter_skeleton(
    feature_info_list = feature_info_list,
    normalisation_method = settings$prep$normalisation_method
  )
  
  # Add normalisation parameters to the feature information list.
  feature_info_list <- add_normalisation_parameters(
    cl = cl,
    feature_info_list = feature_info_list,
    data = data,
    verbose = verbose
  )

  # Apply normalisation to data before clustering
  data <- normalise_features(
    data = data,
    feature_info_list = feature_info_list
  )
  
  logger_message(
    "Pre-processing: Feature data were normalised.",
    indent = message_indent,
    verbose = verbose && settings$prep$normalisation_method != "none"
  )
  
  
  # Batch normalise features ---------------------------------------------------
  logger_message(
    "Pre-processing: Extracting batch normalisation parameters from feature data.",
    indent = message_indent,
    verbose = verbose && settings$prep$batch_normalisation_method != "none"
  )
  
  # Check that assumptions for batch normalisation are fulfilled.
  .check_batch_normalisation_assumptions(
    data = data,
    normalisation_method = settings$prep$batch_normalisation_method
  )
  
  # Add batch normalisation skeletons.
  feature_info_list <- create_batch_normalisation_parameter_skeleton(
    feature_info_list = feature_info_list,
    normalisation_method = settings$prep$batch_normalisation_method
  )
  
  # Add batch normalisation parameters to the feature information list.
  feature_info_list <- add_batch_normalisation_parameters(
    cl = cl, 
    feature_info_list = feature_info_list,
    data = data,
    verbose = verbose
  )
  
  # Batch-normalise feature values
  data <- batch_normalise_features(
    data = data,
    feature_info_list = feature_info_list
  )
  
  logger_message(
    "Pre-processing: Feature data were batch-normalised.",
    indent = message_indent,
    verbose = verbose && settings$prep$batch_normalisation_method != "none"
  )
  
  
  # Remove non-robust features -------------------------------------------------
  if ("robustness" %in% settings$prep$filter_method) {
    n_features_current <- length(available_features)
    
    # Filter features that are not robust
    feature_info_list  <- find_non_robust_features(
      cl = cl,
      feature_info_list = feature_info_list,
      data = data,
      settings = settings
    )
    
    available_features <- get_available_features(feature_info_list = feature_info_list)
    
    # Remove non-robust features from the data
    data <- filter_features(data = data, available_features = available_features)
    
    if (!has_feature_data(data)) {
      ..error(
        paste0(
          "Remaining features in the dataset have a robustness that is lower than the threshold ",
          "and were therefore all removed. Please investigate your data, or decrease the threshold ",
          "through the robustness_threshold_value configuration parameter."
        ),
        error_class = "dataset_error"
      )
    }
    
    # Message number of features removed by the robustness filter.
    logger_message(
      paste0(
        "Pre-processing: ", n_features_current - length(available_features),
        " features were removed due to low robustness. ",
        length(available_features), " features remain."
      ),
      indent = message_indent,
      verbose = verbose
    )
  }
  
  
  # Remove unimportant features ------------------------------------------------
  if ("univariate_test" %in% settings$prep$filter_method) {
    n_features_current <- length(available_features)
    
    # Filter features that are not relevant
    feature_info_list  <- find_unimportant_features(
      cl = cl,
      feature_info_list = feature_info_list,
      data = data,
      settings = settings
    )
    
    available_features <- get_available_features(feature_info_list = feature_info_list)
    
    # Remove unimportant features from the data
    data <- filter_features(data = data, available_features = available_features)
    
    if (!has_feature_data(data)) {
      ..error(
        paste0(
          "Remaining features in the dataset have a p-value that is higher than the threshold ",
          "and were therefore all removed. Please investigate your data, or increase the threshold ",
          "through the univariate_test_threshold configuration parameter."
        ),
        error_class = "dataset_error"
      )
    }
    
    # Message number of features removed by the importance filter.
    logger_message(
      paste0(
        "Pre-processing: ", n_features_current - length(available_features),
        " features were removed due to low importance. ",
        length(available_features), " features remain."
      ),
      indent = message_indent,
      verbose = verbose
    )
  }
  
  
  # Impute missing values ------------------------------------------------------
  logger_message(
    "Pre-processing: Adding imputation information to features.",
    indent = message_indent,
    verbose = verbose
  )
  
  # Add imputation skeletons.
  feature_info_list <- create_imputation_parameter_skeleton(
    feature_info_list = feature_info_list,
    imputation_method = settings$prep$imputation_method
  )
  
  # Add imputation info
  feature_info_list <- add_imputation_info(
    cl = cl,
    feature_info_list = feature_info_list,
    data = data,
    verbose = verbose
  )
  
  # Impute features with censored data prior to clustering
  data <- impute_features(
    data = data,
    feature_info_list = feature_info_list
  )
  
  
  # Cluster features -----------------------------------------------------------
  logger_message(
    "Pre-processing: Starting clustering of redundant features",
    indent = message_indent,
    verbose = verbose && settings$prep$cluster_method != "none"
  )
  
  # Add clustering skeletons.
  feature_info_list <- create_cluster_parameter_skeleton(
    feature_info_list = feature_info_list,
    cluster_method = settings$prep$cluster_method,
    cluster_linkage = settings$prep$cluster_linkage,
    cluster_cut_method = settings$prep$cluster_cut_method,
    cluster_similarity_threshold = settings$prep$cluster_similarity_threshold,
    cluster_similarity_metric = settings$prep$cluster_similarity_metric,
    cluster_representation_method = settings$prep$cluster_representation_method
  )
  
  # Extract clustering information
  feature_info_list  <- add_cluster_info(
    cl = cl,
    feature_info_list = feature_info_list,
    data = data,
    message_indent = message_indent + 1L,
    verbose = verbose
  )
  
  # Build cluster table.
  cluster_table <- .create_clustering_table(feature_info_list = feature_info_list)
  
  # Determine the number of features prior to clustering.
  n_features_current <- nrow(cluster_table)
  
  # Further summarise the clusters by grouping.
  cluster_table <- cluster_table[, list("cluster_size" = .N), by = "cluster_name"]
  
  logger_message(
    paste0(
      nrow(cluster_table),
      ifelse(nrow(cluster_table) == 1L, " feature cluster was", " feature clusteres were"),
      " created from ", n_features_current,
      ifelse(n_features_current == 1L, " feature. ", " features. "),
      sum(cluster_table$cluster_size > 1L),
      ifelse(sum(cluster_table$cluster_size > 1L) == 1L, " cluster contains", " clusters contain"),
      " more than one feature. The remaining ",
      sum(cluster_table$cluster_size == 1L),
      ifelse(sum(cluster_table$cluster_size == 1L) == 1L, " cluster is", " clusters are"),
      " singular."
    ),
    indent = message_indent + 1L,
    verbose = verbose && settings$prep$cluster_method != "none"
  )
  
  # Add required features
  feature_info_list <- add_required_features(feature_info_list = feature_info_list)
  
  # Filter features that are not required from the list
  feature_info_list <- trim_unused_features_from_list(feature_info_list = feature_info_list)
  
  # Return list of featureInfo objects
  return(feature_info_list)
}



combine_feature_info_list <- function(
    preferred = NULL,
    custom = NULL,
    generic = NULL
) {
  
  # Suppress NOTES due to non-standard evaluation in data.table
  name <- present <- list_name <- complete <- NULL
  
  # Get all features.
  feature_names <- unique(c(
    names(preferred),
    names(custom),
    names(generic)
  ))
  
  # Identify which features appear where.
  data <- mapply(
    FUN = function(x, x_name, feature_names) {
      
      # Default dataset.
      data <- data.table::data.table(
        "list_name" = x_name,
        "name" = feature_names,
        "present" = FALSE,
        "complete" = FALSE
      )
      
      # Mark feature names that are present in the current dataset.
      data[name %in% names(x), "present" := TRUE]
      
      # Check whether data are present, are complete.
      data[
        present == TRUE,
        "complete" := feature_info_complete(object = x[[name]]),
        by = "name"
      ]
      
      return(data)
    },
    x = list(
      "preferred" = preferred,
      "custom" = custom,
      "generic" = generic
    ),
    x_name = c("preferred", "custom", "generic"),
    MoreArgs = list("feature_names" = feature_names),
    SIMPLIFY = FALSE
  )
  
  # Combine list.
  data <- data.table::rbindlist(data)
  
  # Start new feature list.
  new_feature_list <- NULL
  
  # Preference to get features with complete information first from the
  # preferred set.
  selected_feature_names <- data[list_name == "preferred" & complete == TRUE]$name
  
  # Add to feature list and remove from set of features.
  if (length(selected_feature_names) > 0L) {
    new_feature_list <- c(new_feature_list, preferred[selected_feature_names])
    feature_names <- setdiff(feature_names, selected_feature_names)
  }
  
  # Then, preference to get complete information from the custom set.
  selected_feature_names <- data[name %in% feature_names & list_name == "custom" & complete == TRUE, ]$name
  
  # Add to feature list and remove from set of features.
  if (length(selected_feature_names) > 0L) {
    new_feature_list <- c(new_feature_list, custom[selected_feature_names])
    feature_names <- setdiff(feature_names, selected_feature_names)
  }
  
  # Then, preference to get incomplete information first from the preferred set.
  selected_feature_names <- data[name %in% feature_names & list_name == "preferred" & present == TRUE, ]$name
  
  # Add to feature list and remove from set of features.
  if (length(selected_feature_names) > 0L) {
    new_feature_list <- c(new_feature_list, preferred[selected_feature_names])
    feature_names <- setdiff(feature_names, selected_feature_names)
  }
  
  # Then, preference to get incomplete information first from the custom set.
  selected_feature_names <- data[name %in% feature_names & list_name == "custom" & present == TRUE, ]$name
  
  # Add to feature list and remove from set of features.
  if (length(selected_feature_names) > 0L) {
    new_feature_list <- c(new_feature_list, custom[selected_feature_names])
    feature_names <- setdiff(feature_names, selected_feature_names)
  }
  
  # Then, preference to get incomplete information first from the generic set.
  selected_feature_names <- data[name %in% feature_names & list_name == "generic" & present == TRUE, ]$name
  
  # Add to feature list and remove from set of features.
  if (length(selected_feature_names) > 0L) {
    new_feature_list <- c(new_feature_list, generic[selected_feature_names])
    feature_names <- setdiff(feature_names, selected_feature_names)
  }
  
  return(new_feature_list)
}
