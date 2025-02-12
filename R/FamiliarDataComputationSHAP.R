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
    # 
    # # Test if models are properly loaded
    # if (!is_model_loaded(object = object)) ..error_ensemble_models_not_loaded()
    # 
    # # Test if the any of the models in the ensemble were trained.
    # if (!model_is_trained(object)) return(NULL)
    # 
    # proto_data_element <- methods::new("familiarDataElementHyperparameters")
    # 
    # # Generate elements to send to dispatch.
    # hyperparameter_data <- extract_dispatcher(
    #   FUN = .extract_hyperparameters,
    #   cl = NULL,
    #   has_internal_bootstrap = FALSE,
    #   object = object,
    #   proto_data_element = proto_data_element,
    #   aggregate_results = FALSE,
    #   message_indent = message_indent + 1L,
    #   verbose = verbose
    # )
    # 
    # return(hyperparameter_data)
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


# Step 1: Determine feature values that are to be sampled for determining SHAP
# values.

# Step 2: Determine the minimum sampleset X required to determine SHAP values.
# The number of samples (n) is equal to the feature with the largest number of 
# values (m_i) to sample. Feature values can be randomly ordered for each
# feature. Features with m_i < n can randomly draw additional features.

# NOTE: Is step 2 really necessary? We can also determine SHAP values for each
# feature value, by randomly generating coalitions, keeping track of the
# results, and using these in subsequent calculations.

# Yes necessary -- however, we can still track which predictions have been made
# for which generated feature sets. Depending on the current sample in the 
# minimum sample set, these predictions represent different coalitions, which we
# then get to use for free!

# Step 3: Create coalition sets for coalitions with all but one off and all but
# one on.

# Step 4: Iterate samples in sampleset X. Generate samples corresponding to
# coalitions for each sample. Concatenate generated samples and add to table
# with previously generated samples X_gen.

# Step 5: Select samples without existing predictions.

# Step 6: Predict samples. Merge new predictions into existing predictions.

# Step 7: Compute average predicted value (phi_0).

# Step 8: For each sample in sampleset X, determine coalition represented by
# each sample in X_gen. Compute kernel weight based on coalition. Compute SHAP
# values by solving linear equation.

# Step 9: Average SHAP value for each feature value.

# Step 10: Determine convergence and repeat steps 4-9.
