#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


# familiarNoneNoveltyDetector --------------------------------------------------
setClass(
  "familiarNoneNoveltyDetector",
  contains = "familiarNoveltyDetector"
)



# is_available -----------------------------------------------------------------
setMethod(
  "is_available",
  signature(object = "familiarNoneNoveltyDetector"),
  function(object, ...) {
    # We can always not create a novelty detector.
    return(TRUE)
  }
)



.get_available_none_detectors <- function(show_general = TRUE) {
  return(c("none", "false", "no"))
}



# set_model_features (familiarNoveltyDetector)----------------------------------
setMethod(
  "set_model_features",
  signature(object = "familiarNoneNoveltyDetector"),
  function(
    object, 
    signature_features = NULL, 
    minimise_footprint = FALSE, 
    ...
  ) {
    object@feature_info <- NULL
    
    # Set feature-related attribute slots
    object@required_features <- NULL
    object@model_features <- NULL
    
    return(object)
  }
)
