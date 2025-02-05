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
