
.get_available_data_elements <- function(
    check_has_estimation_type = FALSE,
    check_has_detail_level = FALSE, 
    check_has_sample_limit = FALSE,
    check_has_n_important_features = FALSE,
    check_from_prediction_table = FALSE,
    check_from_data_object = FALSE
) {
  
  # All data elements.
  all_data_elements <- c(
    "auc_data", "calibration_data", "calibration_info", "confusion_matrix",
    "decision_curve_analyis", "feature_expressions",
    "fs_vimp", "hyperparameters", "model_performance",
    "model_vimp", "permutation_vimp", "prediction_data",
    "risk_stratification_data", "risk_stratification_info",
    "univariate_analysis", "feature_similarity", "sample_similarity", "ice_data",
    "shap"
  )
  
  # Data elements that allow setting an estimation type.
  can_set_estimation_type <- c(
    "auc_data", "calibration_data", "decision_curve_analyis",
    "model_performance", "permutation_vimp",  "prediction_data", "ice_data"
  )
  
  # Data elements that allow setting a detail level.
  can_set_detail_level <- c(
    can_set_estimation_type, "calibration_info", "confusion_matrix",
    "risk_stratification_data", "risk_stratification_info", "shap"
  )
   
  # Data elements that allow for setting an estimation type but not detail
  # level.
  can_set_estimation_type <- c(can_set_estimation_type, "feature_similarity")
  
  # Data elements that allow for setting a sample limit.
  can_set_sample_limit <- c("sample_similarity", "ice_data", "shap", "permutation_vimp")
  
  # Data elements that allow for setting the number of important features.
  can_set_n_important_features <- c("permutation_vimp", "ice_data", "shap")
  
  # Data elements that can be computed from prediction table objects.
  can_use_prediction_table <- c(
    "prediction_data", "auc_data", "calibration_data", "decision_curve_analyis",
    "model_performance", "risk_stratification_data"
  )
  
  # Data elements that can be computed from data objects.
  can_use_data_object <- c("risk_stratification_data", "feature_similarity", "sample_similarity", "feature_expressions")
  
  if (check_has_sample_limit) {
    all_data_elements <- intersect(all_data_elements, can_set_sample_limit)
  }
  
  if (check_has_estimation_type) {
    all_data_elements <- intersect(all_data_elements, can_set_estimation_type)
  } 
  
  if (check_has_detail_level) {
    all_data_elements <- intersect(all_data_elements, can_set_detail_level)
  }
  
  if (check_from_prediction_table) {
    all_data_elements <- intersect(all_data_elements, can_use_prediction_table)
  }
  
  if (check_from_data_object) {
    all_data_elements <- intersect(all_data_elements, can_use_data_object)
  }
  
  return(all_data_elements)
}



.parse_detail_level <- function(
    x,
    object, 
    default, 
    data_element) {
  
  if (is.waive(x)) x <- object@settings$detail_level
  
  if (is.null(x)) return(default)
  
  # detail level is stored in a list, by data_element.
  if (is.list(x)) x <- x[[data_element]]
  
  if (is.null(x)) return(default)
  
  .check_parameter_value_is_valid(
    x = x,
    var_name = "detail_level",
    values = c("ensemble", "hybrid", "model")
  )
  
  return(x)
}



.parse_estimation_type <- function(
    x,
    object,
    default,
    data_element,
    detail_level,
    has_internal_bootstrap
) {
  
  # Change to default to point if the detail_level is model.
  if (detail_level == "model") default <- "point"
  
  # In case there is no internal bootstrap, we can only determine point
  # estimates for ensemble and model detail levels (but potentially more for
  # hybrid).
  if (
    !has_internal_bootstrap &&
    detail_level %in% c("ensemble", "model") &&
    default != "point"
  ) {
    default <- "point"
  }
  
  if (is.waive(x) && .hasSlot(object, "settings")) x <- object@settings$estimation_type
  
  if (is.null(x)) return(default)
  
  # detail level is stored in a list, by data_element.
  if (is.list(x)) x <- x[[data_element]]
  
  if (is.null(x)) return(default)
  
  .check_parameter_value_is_valid(
    x = x,
    var_name = "estimation_type",
    values = c(
      "point", "bias_correction", "bc",
      "bootstrap_confidence_interval", "bci"
    )
  )
  
  return(x)
}



.parse_aggregate_results <- function(
    x,
    object, 
    default, 
    data_element) {
  
  if (is.waive(x) && methods::.hasSlot(object, "settings")) {
    x <- object@settings$aggregate_results
    
  } else if (is.waive(x)) {
    return(default)
  }
  
  if (is.null(x)) return(default)
  
  # detail level is stored in a list, by data_element.
  if (is.list(x)) x <- x[[data_element]]
  
  if (is.null(x)) return(default)
  
  x <- tolower(x)
  .check_parameter_value_is_valid(
    x = x,
    var_name = "aggregate_results",
    values = c("true", "false", "none", "all", "default")
  )
  
  if (x == "default") return(default)
  if (x %in% c("true", "all")) return(TRUE)
  
  return(FALSE)
}



.parse_sample_limit <- function(
    x,
    object,
    default,
    data_element
) {
  if (is.waive(x) && .hasSlot(object, "settings")) x <- object@settings$sample_limit
  
  if (is.null(x)) return(default)
  
  # detail level is stored in a list, by data_element.
  if (is.list(x)) x <- x[[data_element]]
  
  if (is.null(x)) return(default)
  
  if (x == "default") return(default)
  
  .check_number_in_valid_range(
    x = x,
    var_name = "sample_limit",
    range = c(20L, Inf)
  )
  
  return(x)
}



.parse_n_important_features <- function(
    x,
    object,
    default,
    data_element
) {
  if (is.waive(x)) x <- object@settings$n_important_features
  
  if (is.null(x)) return(default)
  
  # detail level is stored in a list, by data_element.
  if (is.list(x)) x <- x[[data_element]]
  
  if (is.null(x)) return(default)
  
  if (x == "default") return(default)
  
  .check_number_in_valid_range(
    x = x,
    var_name = "n_important_features",
    range = c(1L, Inf)
  )
  
  return(x)
}



.select_important_features <- function(
    object,
    data,
    fallback_vimp_method = "mim",
    n_important_features = Inf
) {
  # Suppress NOTES due to non-standard evaluation in data.table
  name <- rank <- NULL
  
  if (!(is(object, "familiarModel") || is(object, "familiarNoveltyDetector") || is(object, "familiarEnsemble"))) {
    ..error_reached_unreachable_code(paste0("invalid object class: ", class(object)))
  }
  
  # Check that the model has any features, i.e. is not naive.
  if (object@vimp_method %in% .get_available_no_features_vimp_methods()) return(NULL)
  if (length(object@model_features) == 0L) return(NULL)
  
  if (is(object, "familiarEnsemble")) {
    # Make sure that models are loaded:
    object <- load_models(object, suppress_auto_detach = TRUE)
    if (!model_is_trained(object)) return(NULL)
    
    vimp_table <- lapply(object@model_list, function(x) (x@vimp_table))
    vimp_aggregation_method <- object@model_list[[1L]]@vimp_aggregation_method
    vimp_rank_threshold <- object@model_list[[1L]]@vimp_rank_threshold
    
  } else {
    if (!model_is_trained(object)) return(NULL)
    
    vimp_table <- object@vimp_table
    vimp_aggregation_methpd <- object@vimp_aggregation_method
    vimp_rank_threshold <- object@vimp_rank_threshold
  }
  
  # Flatten lists, if necessary.
  if (rlang::is_bare_list(vimp_table)) {
    vimp_table <- unlist(vimp_table)
  }
  
  # Get available features for the model or ensemble.
  features <- features_after_clustering(
    features = object@model_features,
    feature_info_list = object@feature_info
  )
  if (length(features) <= n_important_features) return(features)
  
  # Determine which features are pre-assigned to the signature.
  signature_features <- names(object@feature_info)[sapply(object@feature_info, is_in_signature)]
  
  # Check that fallback is required: in case of none, random, or no features
  # being selected.
  use_fallback <- object@vimp_method %in% c(
    .get_available_none_vimp_methods(),
    .get_available_random_vimp_methods()
  )
  
  # Check that fallback is required: in case there is no variable importance
  # table.
  if (!use_fallback) {
    if (rlang::is_bare_list(vimp_table)) {
      use_fallback <- all(sapply(vimp_table, is_empty))
    } else {
      use_fallback <- is_empty(vimp_table)
    }
  }
  
  # If signature is used, don't use fall-back option.
  if (object@vimp_method %in% .get_available_signature_only_vimp_methods()) use_fallback <- FALSE
  
  # Set-up fallback vimp-table
  if (use_fallback) {
    browser()
    # TODO: SPAWN TASK FOR EACH MODEL>>>>
    
    # Spawn task to obtain variable importance tables.
    vimp_task <- methods::new(
      "familiarTaskVimp",
      project_id = object@project_id,
      vimp_method = fallback_vimp_method,
      data_id = object@data_id,
      run_id = object@run_id,
      file = NA_character_
    )
    
    # Fill details required to get the data, in case the data is delayed.
    # Note that training data is used for obtaining variable importance.
    if (is(data, "delayedDataObject")) {
      data@data_id <- object@data_id
      data@run_id <- object@run_id
    }
    
    # Create variable importance table.
    vimp_table <- .perform_task(
      object = vimp_task,
      feature_info_list = object@feature_info,
      vimp_aggregation_method = vimp_aggregation_method,
      vimp_rank_threshold = vimp_rank_threshold,
      data = data
    )
  }
  
  # For signature-only, return all signature features, with no preference.
  if (object@vimp_method %in% .get_available_signature_only_vimp_methods()) {
    # Only select signature.
    if (length(signature_features) == 0L) {
      ..error(
        "No signature was provided.",
        error_class = "input_argument_error"
      )
    }
    
    return(signature_features)
  }
    
  # Select signature and any additional features according to rank.
  selected_features <- signature_features
  
  # Get number remaining available features
  n_allowed_features <- n_important_features - length(signature_features)
  
  # Check that features may be added, and the rank table is not empty.
  if (n_allowed_features > 0L && !is_empty(vimp_table)) {

    # Remove signature features, if any, to prevent duplicates.
    features <- setdiff(features, signature_features)
    
    # Extract aggregated rank table. First, ensure that the associated cluster
    # table is correct.
    vimp_table <- update_vimp_table_to_reference(
      x = vimp_table,
      reference_cluster_table = .create_clustering_table(
        feature_info_list = object@feature_info
      )
    )
    
    # Recluster the data according to the clustering table corresponding to the
    # model. This ensures that the variable importance table has the features
    # that are seen by the model.
    vimp_table <- recluster_vimp_table(vimp_table)
    
    # Get aggregate variable importances
    vimp_table <- aggregate_vimp_table(
      vimp_table,
      aggregation_method = vimp_aggregation_method,
      rank_threshold = vimp_rank_threshold
    )
    
    if (is_empty(vimp_table)) return(signature_features)
    
    # Keep only feature ranks of feature corresponding to available
    # features, and order by rank.
    rank_table <- get_vimp_table(vimp_table)[name %in% features, ][order(rank)]
    
    # Add good features (low rank) to the selection
    selected_features <- c(
      signature_features,
      head(x = rank_table, n = n_allowed_features)$name
    )
  }
  
  if (length(selected_features) == 0L) return(NULL)
  
  return(selected_features)
}
