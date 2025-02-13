#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL



# familiarDataElementSHAP object -----------------------------------------------
setClass(
  "familiarDataElementSHAP",
  contains = "familiarDataElement",
  prototype = methods::prototype(
    detail_level = "ensemble",
    estimation_type = "point"
  )
)



# extract_shap (generic) -------------------------------------------------------

#'@title Internal function to compute SHAP values.
#'
#'@description Computes SHAP values for features using a `familiarEnsemble`.
#'
#'@inheritParams .extract_data
#'
#'@return A list of familiarDataElements with SHAP values.
#'@md
#'@keywords internal
setGeneric(
  "extract_shap",
  function(
    object,
    message_indent = 0L,
    verbose = FALSE,
    ...
  ) {
    standardGeneric("extract_shap")
  }
)



# extract_shap (familiarEnsemble) ----------------------------------------------
setMethod(
  "extract_shap",
  signature(object = "familiarEnsemble"),
  function(
    object,
    data,
    cl = NULL,
    features = NULL,
    n_sample_points = 20L,
    ensemble_method = waiver(),
    evaluation_times = waiver(),
    sample_limit = waiver(),
    detail_level = waiver(),
    aggregate_results = waiver(),
    is_pre_processed = FALSE,
    message_indent = 0L,
    verbose = FALSE
  ) {
    # Compute SHAP values.
    
    # Message extraction start
    logger_message(
      paste0("Extracting SHAP values for the ensemble."),
      indent = message_indent,
      verbose = verbose
    )
    browser()
    if (is.null(features)) {
      logger_message(
        paste0(
          "Computing SHAP values for features in the dataset."
        ),
        indent = message_indent,
        verbose = verbose
      )
      
    } else {
      logger_message(
        paste0(
          "Computing SHAP values for selected features: ", paste_s(features), "."
        ),
        indent = message_indent,
        verbose = verbose
      )
    }
    
    # Load evaluation_times from the object settings attribute, if it is not provided.
    if (is.waive(evaluation_times)) evaluation_times <- object@settings$eval_times
    
    # Check evaluation_times argument
    if (object@outcome_type %in% c("survival")) {
      sapply(
        evaluation_times,
        .check_number_in_valid_range,
        var_name = "evaluation_times",
        range = c(0.0, Inf),
        closed = c(FALSE, TRUE)
      )
    }
    
    # Check n_sample_points argument. This defines the maximum number of feature
    # values for which SHAP values are computed.
    .check_number_in_valid_range(
      x = n_sample_points,
      var_name = "n_sample_points",
      range = c(2L, Inf),
      closed = c(TRUE, TRUE)
    )
    
    # Obtain ensemble method from stored settings, if required.
    if (is.waive(ensemble_method)) ensemble_method <- object@settings$ensemble_method
    
    # Check ensemble_method argument
    .check_parameter_value_is_valid(
      x = ensemble_method,
      var_name = "ensemble_method",
      values = .get_available_ensemble_prediction_methods()
    )
    
    # Check the sample limit. This defines the subset of samples that are being
    # assessed (if real data is used instead of a minimum subset).
    sample_limit <- .parse_sample_limit(
      x = sample_limit,
      object = object,
      default = 200L,
      data_element = "shap_data"
    )
    
    # Check the level detail.
    detail_level <- .parse_detail_level(
      x = detail_level,
      object = object,
      default = "ensemble",
      data_element = "shap_data"
    )
    
    # Check whether results should be aggregated.
    aggregate_results <- .parse_aggregate_results(
      x = aggregate_results,
      object = object,
      default = FALSE,
      data_element = "shap_data"
    )
    
    # Test if models are properly loaded
    if (!is_model_loaded(object = object)) ..error_ensemble_models_not_loaded()
    
    # Test if any model in the ensemble was successfully trained.
    if (!model_is_trained(object = object)) return(NULL)
    
    # Get and process the input data. Since we define SHAP values using the
    # features in their original scales, we need to apply only minimal
    # pre-processing.
    data <- process_input_data(
      object = object,
      data = data, 
      stop_at = "signature"
    )
    
    # Use sample limit to cap the number of samples that are assessed.
    data <- get_subsample(
      data = data,
      size = sample_limit,
      seed = 0L
    )
    
    # Generate a prototype data element.
    proto_data_element <- new(
      "familiarDataElementSHAP",
      detail_level = detail_level
    )
    
    # Generate elements to send to dispatch.
    shap_data <- extract_dispatcher(
      FUN = .extract_shap,
      has_internal_bootstrap = FALSE,
      cl = cl,
      object = object,
      data = data,
      features = features,
      n_sample_points = n_sample_points,
      proto_data_element = proto_data_element,
      is_pre_processed = is_pre_processed,
      ensemble_method = ensemble_method,
      evaluation_times = evaluation_times,
      aggregate_results = aggregate_results,
      message_indent = message_indent + 1L,
      verbose = verbose
    )
  }
)



# extract_shap (prediction table) ----------------------------------------------
setMethod(
  "extract_shap",
  signature(object = "familiarDataElementSHAP"),
  function(object, ...) {
    ..warning_no_data_extraction_from_prediction_table("SHAP values")
    
    return(NULL)
  }
)


.extract_shap <- function(
    object,
    data = NULL,
    proto_data_element,
    evaluation_times = NULL,
    features = NULL,
    n_sample_points,
    aggregate_results,
    is_pre_processed = FALSE,
    cl,
    message_indent = 0L,
    verbose = FALSE,
    progress_bar = FALSE,
    ...
) {
  # Step 1: Determine feature values that are to be sampled for determining SHAP
  # values.
  #
  # Step 2: Determine the minimum sampleset X required to determine SHAP values.
  # The number of samples (n) is equal to the feature with the largest number of 
  # values (m_i) to sample. Feature values can be randomly ordered for each
  # feature. Features with m_i < n can randomly draw additional features.
  #
  # Alternative: Use the actual dataset X (trigger on function argument).
  #
  # Step 3: Create coalition sets for coalitions with all but one off and all but
  # one on.
  #
  # Step 4: Iterate samples in sampleset X. Generate samples corresponding to
  # coalitions for each sample. Concatenate generated samples and add to table
  # with previously generated samples X_gen.
  #
  # Step 5: Select samples without existing predictions.
  #
  # Step 6: Predict samples. Merge new predictions into existing predictions.
  #
  # Step 7: Compute average predicted value (phi_0).
  #
  # Step 8: For each sample in sampleset X, determine coalition represented by
  # each sample in X_gen. Compute kernel weight based on coalition. Compute SHAP
  # values by solving linear equation.
  #
  # Step 9: Average SHAP value for each feature value.
  #
  # Step 10: Determine convergence and repeat steps 4-9 until convergence is
  # reached, or capacity is exhausted.
  #
  # Parallel processing: perform steps 4-6 multiple times within a parallel loop.
  # This allows for faster convergence.
  
  # Check that the model requires any features.
  if (is_empty(object@model_features)) return(NULL)
  
  # Get set of feature values.
  feature_set <- .get_shap_feature_set(
    data = data,
    features = object@model_features,
    feature_info = object@feature_info[object@model_features],
    n_sample_points = n_sample_points
  )
  
  # Generate 
  if (is_empty(data)) {
    data <- .get_shap_sample_set(
      object = object,
      feature_set = feature_set,
    )
    
  } 
  
}



.get_shap_feature_set <- function(
    data = NULL,
    features,
    feature_info,
    n_sample_points
) {
  # Gets set of feature values for the features of interest. For categorical
  # features, all levels are used. For numerical features, the data is sampled
  # (if available), and additional values are drawn based on the known
  # distribution of feature values for each feature.
  feature_set <- list()
  
  for (feature in features) {
    # For categorical features, use all levels.
    if (feature_info[[feature]]@feature_type == "factor") {
      feature_set[[feature]] <- feature_info[[feature]]@levels
      next
    }
    
    # Select (numeric) feature values from the data.
    feature_values <- NULL
    if (!is_empty(data)) {
      feature_values <- unique(data@data[[feature]])
    }
    
    # Check number of values to sample.
    n_to_sample <- n_sample_points - length(feature_values)
    if (n_to_sample <= 0L) {
      feature_set[[feature]] <- feature_values
      next
    }
    
    # Add feature values by sampling distribution.
    feature_set[[feature]] <- unique(c(
      feature_values,
      stats::spline(
        x = (seq_along(feature_info[[feature]]@distribution$pctl) - 1L) / 
          (length(feature_info[[feature]]@distribution$pctl) - 1L),
        y = as.numeric(feature_info[[feature]]@distribution$pctl),
        xout = get_percentiles(n_to_sample),
        method = "hyman"
      )$y
    ))
  }
  
  return(feature_set)
}
