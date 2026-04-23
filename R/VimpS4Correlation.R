#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass(
  "familiarCorrelationVimp",
  contains = "familiarVimpMethod"
)



.get_available_correlation_vimp_methods <- function(show_general = TRUE) {
  return(c("pearson", "spearman", "kendall"))
}



# is_available -----------------------------------------------------------------
setMethod(
  "is_available",
  signature(object = "familiarCorrelationVimp"),
  function(object, ...) {
    
    if (object@outcome_type == "count") {
      ..deprecation_count()
      return(FALSE)
    }
    
    return(object@outcome_type %in% c("continuous", "survival"))
  }
)



# get_default_hyperparameters --------------------------------------------------
setMethod(
  "get_default_hyperparameters",
  signature(object = "familiarCorrelationVimp"),
  function(object, data = NULL, ...) {
    return(list())
  }
)



# ..vimp -----------------------------------------------------------------------
setMethod(
  "..vimp",
  signature(object = "familiarCorrelationVimp"),
  function(
    object,
    data,
    cl = NULL,
    ...
  ) {
    # Suppress NOTES due to non-standard evaluation in data.table
    outcome_event <- NULL

    if (is_empty(data)) return(callNextMethod())

    # Drop non-event data for censored data analysis for calculating correlation
    # and set outcome column.
    if (object@outcome_type == "survival") {
      data@data <- data@data[outcome_event == 1L, ]

      # Check whether the filtered data does not allow for assessing variable
      # importance.
      if (has_bad_training_data(object = object, data = data)) {
        return(callNextMethod)
      }
    }

    # Use effect coding to convert categorical data into encoded data - this is
    # required to deal with factors with missing/new levels between training and
    # test data sets.
    encoded_data <- encode_categorical_variables(
      data = data,
      object = object,
      encoding_method = "dummy",
      drop_levels = FALSE
    )

    # Find feature columns in the data.
    feature_columns <- get_feature_columns(x = encoded_data$encoded_data)

    if (object@outcome_type =="survival") {
      outcome <- encoded_data$encoded_data@data$outcome_time
      
    } else {
      outcome <- encoded_data$encoded_data@data$outcome
    }
    
    # Compute correlation coefficients.
    correlation_coefficients <- fam_sapply(
      cl = cl,
      X = encoded_data$encoded_data@data[, mget(feature_columns)],
      FUN = stats::cor,
      y = outcome,
      method = object@vimp_method,
      chopchop = TRUE
    )

    # Create variable importance object.
    vimp_object <- methods::new("vimpTable",
      vimp_table = data.table::data.table(
        "score" = abs(correlation_coefficients),
        "name" = feature_columns
      ),
      encoding_table = encoded_data$reference_table,
      score_aggregation = "max",
      invert = TRUE
    )

    return(vimp_object)
  }
)
