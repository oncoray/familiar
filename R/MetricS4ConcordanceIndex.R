#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL



# familiarMetricConcordanceIndex -----------------------------------------------
setClass(
  "familiarMetricConcordanceIndex",
  contains = "familiarMetric",
  slots = list("time" = "numeric"),
  prototype = list("time" = Inf)
)



# Harrell's Concordance Index --------------------------------------------------
setClass(
  "familiarMetricConcordanceIndexHarrell",
  contains = "familiarMetricConcordanceIndex",
  prototype = methods::prototype(
    name = "Concordance Index",
    baseline_value = 0.5,
    value_range = c(0.0, 1.0),
    higher_better = TRUE
  )
)



# initialize -------------------------------------------------------------------
setMethod(
  "initialize",
  signature(.Object = "familiarMetricConcordanceIndex"),
  function(
    .Object,
    time = NULL,
    object = NULL,
    prediction_type = NULL,
    ...
  ) {
    # Update with parent class first.
    .Object <- callNextMethod(.Object, ...)

    # Attempt to set time.
    if (!is.null(time)) {
      .Object@time <- time
      
    } else if (
      is(object, "familiarModel") ||
      is(object, "familiarEnsemble") ||
      is(object, "familiarVimpMethod")
    ) {
      # Obtain time from the outcome info.
      .Object@time <- object@outcome_info@time
    }

    return(.Object)
  }
)



# is_available -----------------------------------------------------------------
setMethod(
  "is_available",
  signature(object = "familiarMetricConcordanceIndex"),
  function(object, ...) {
    return(object@outcome_type %in% c("survival", "competing_risk"))
  }
)



# compute_metric_score (Harrell's Concordance Index) ---------------------------
setMethod(
  "compute_metric_score",
  signature(metric = "familiarMetricConcordanceIndexHarrell"),
  function(metric, data, ...) {
    # Compute a standard concordance index.
    score <- .compute_concordance_index(
      metric = metric,
      data = data,
      ...
    )

    # Invert the score for risk-like predictions.
    if (!is(data, "predictionTableSurvivalTime")) {
      score <- 1.0 - score
    }
    
    return(score)
  }
)



.compute_concordance_index <- function(
    metric,
    data,
    time = NULL,
    weight_function = NULL
) {
  # Suppress NOTES due to non-standard evaluation in data.table
  outcome_time <- outcome_event <- NULL

  if (is_empty(data)) return(NA_real_)
  
  if (!is(data, "predictionTableSurvival")) {
    ..error_data_not_prediction_table(data, "predictionTableSurvival")
  }
  
  # Remove any entries that lack valid predictions.
  data <- remove_invalid_predictions(data)
  
  # Remove any entries that lack observed values.
  data <- filter_missing_outcome(data)
  if (is_empty(data)) return(NA_real_)

  # Determine maximum time for censoring. There are multiple ways to obtain
  # maximum time, in order of preference:
  #
  # 1. From function input.
  #
  # 2. From the metric object (metric), which may have inherited it from the
  # model or model ensemble.
  #
  # 3. From the prediction table object (data).
  #
  # As fallback, max time is set to infinite.
  max_time <- time
  if (is.null(max_time) && is.finite(metric@time)) max_time <- metric@time
  if (is.null(max_time) && methods::.hasSlot(data, "time")) {
    if (is.finite(data@time)) max_time <- data@time 
  }
  if (is.null(max_time)) max_time <- Inf
  
  # Check max_time
  .check_number_in_valid_range(
    x = max_time,
    var_name = "time",
    range = c(0.0, Inf),
    closed = c(FALSE, TRUE)
  )
  
  # Apply max time. Everything beyond max time is censored for the purpose of
  # computing a concordance index.
  data <- .as_data_table(data)
  data[outcome_time > max_time, "outcome_event" := 0L]

  # All competing risks are treated as censoring for computing the concordance
  # index.
  data[outcome_event > 1L, "outcome_event" := 0L]

  # Check that there are any events.
  if (nrow(data[outcome_event == 1L]) == 0L) return(NA_real_)

  # Remove any samples that are censored prior to the first event.
  earliest_event <- min(data[outcome_event == 1L]$outcome_time)

  # Keep only data from the earliest event onward.
  data <- data[outcome_time >= earliest_event]

  # Check that sufficient data is remaining.
  if (nrow(data) < 2L) return(NA_real_)

  # Compute a concordance index score
  score <- ..compute_concordance_index(
    x = data$predicted_outcome,
    time = data$outcome_time,
    event = data$outcome_event,
    weight_function = weight_function
  )

  return(score)
}



..compute_concordance_index <- function(
    x,
    time,
    event, 
    weight_function = NULL,
    ...
) {
  # Compute using concordance in the survival package.
  ci <- survival::concordance(
    Surv(time, event) ~ x,
    data = list("x" = x, "time" = time, "event" = event)
  )$concordance
  
  # Check if the concordance index is valid
  if (!is.finite(ci)) ci <- NA_real_
  
  return(ci)
}



.get_available_concordance_index_metrics <- function() {
  return(c(.get_available_concordance_index_harrell()))
}


.get_available_concordance_index_harrell <- function() {
  return(c("concordance_index", "c_index", "concordance_index_harrell", "c_index_harrell"))
}
