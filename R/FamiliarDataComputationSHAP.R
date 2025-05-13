#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL



# familiarDataElementSHAP object -----------------------------------------------
setClass(
  "familiarDataElementSHAP",
  contains = "familiarDataElement",
  slots = list(
    "data_mapping" = "ANY",
    "predicted_values" = "ANY",
    "lookup_table" = "ANY"
  ),
  prototype = methods::prototype(
    detail_level = "ensemble",
    estimation_type = "point",
    value_column = "shap_value",
    grouping_column = c("feature_name", "feature_value_mapping"),
    data_mapping = NULL,
    predicted_values = NULL,
    lookup_table = NULL
  )
)


# familiarDataElementSHAPSummary object ----------------------------------------

# Objects for creating SHAP summary plots. These are created at run-time from
# data included in familiarDataElementSHAP objects.
setClass(
  "familiarDataElementSHAPSummary",
  contains = "familiarDataElement"
)


# familiarDataElementSHAPForce object ------------------------------------------

# Objects for creating SHAP force plots. These are created at run-time from
# data included in familiarDataElementSHAP objects.
setClass(
  "familiarDataElementSHAPForce",
  contains = "familiarDataElement"
)


# familiarDataElementSHAPDependence object -------------------------------------

# Objects for creating SHAP dependence plots. These are created at run-time from
# data included in familiarDataElementSHAP objects.
setClass(
  "familiarDataElementSHAPDependence",
  contains = "familiarDataElement",
  slots = list(
    "feature_x" = "character",
    "feature_y" = "character"
  ),
  prototype = methods::prototype(
    feature_x = NA_character_,
    feature_y = NA_character_
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
    verbose = FALSE,
    ...
  ) {
    # Compute SHAP values.
    
    # Message extraction start
    logger_message(
      paste0("Extracting SHAP values for the ensemble."),
      indent = message_indent,
      verbose = verbose
    )
    
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
    ensemble_method,
    cl,
    tolerance = 0.001,
    n_max_iter = 100L,
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
  
  # Generate data if absent.
  if (is_empty(data)) {
    data <- .get_shap_sample_set(
      object = object,
      feature_set = feature_set
    )
  }
  
  # Generate coalitions (Z)
  coalitions <- .get_shap_coalitions(
    feature_set = feature_set,
    depth = 1L
  )
  
  # Check that coalitions are not empty: this happens if the data contains a
  # single feature: SHAP values cannot be computed.
  if (is.null(coalitions)) return(NULL)
  
  # From here, work with mapping representations of the data (h).
  mapping_input <- .shap_data_to_mapping(
    data = data,
    feature_set = feature_set
  )
  
  # Determine unique mappings.
  mapping_hash_mapping <- .hash_mapping(mapping_input)
  unique_mappings <- !duplicated(mapping_hash_mapping)
  
  # Select only unique mappings as input.
  mapping_input <- mapping <- mapping_input[unique_mappings, , drop = FALSE]
  mapping_hash_mapping <- mapping_hash_mapping[unique_mappings]
  
  if (is_empty(mapping_hash_mapping)) return(NULL)
  
  # Predict outcome values from the input data. Output may be more than one
  # column.
  predicted_values <- predicted_values_input <- .predict_from_coalition(
    mapping = mapping_input,
    feature_set = feature_set,
    object = object,
    ensemble_method = ensemble_method,
    evaluation_time = NULL
  )
  
  # Compute phi_0.
  phi_0 <- colMeans(predicted_values)
  
  # Looping variabes.
  iter_id <- 1L
  all_shap_converged <- FALSE
  shap_values <- NULL
  
  while (!all_shap_converged && iter_id < n_max_iter) {
    
    # Determine additional mapping.
    mapping_iter <- .shap_randomise_mapping_from_coalition(
      samples = mapping_input,
      coalitions = coalitions,
      feature_set = feature_set,
      seed = iter_id
    )
    
    # Determine new, unique mappings in this iteration.
    mapping_hash_iter <- .hash_mapping(mapping_iter)
    new_mappings <- !mapping_hash_iter %in% mapping_hash_mapping
    unique_mappings <- !duplicated(mapping_hash_iter)
    
    # Select only new unique mappings
    mapping_iter <- mapping_iter[new_mappings & unique_mappings, , drop = FALSE]
    mapping_hash_iter <- mapping_hash_iter[new_mappings & unique_mappings]
    
    # Skip if all parts of mapping have predictions.
    if (is_empty(mapping_hash_iter)) {
      iter_id <- iter_id + 1L
      next
    }
    
    # Predict from new unique mappings.
    predicted_values_iter <- .predict_from_coalition(
      mapping = mapping_iter,
      feature_set = feature_set,
      object = object,
      ensemble_method = ensemble_method,
      evaluation_time = NULL
    )
    
    # Update iterative data.
    mapping <- rbind(mapping, mapping_iter)
    mapping_hash_mapping <- c(mapping_hash_mapping, mapping_hash_iter)
    predicted_values <- rbind(predicted_values, predicted_values_iter)
    
    # Compute SHAP values for this iteration.
    shap_values <- .compute_shap_value(
      samples = mapping_input,
      mapping = mapping,
      feature_set = feature_set,
      predicted_values = predicted_values,
      phi_0 = phi_0
    )
    
    if (is.null(shap_values)) return(NULL)
    
    # Check convergence.
    all_shap_converged <- .evaluate_shap_convergence(
      shap_values = shap_values,
      predicted_values = predicted_values,
      tolerance = tolerance
    )
    
    iter_id <- iter_id + 1L
  }
  
  # Add model name to data element.
  proto_data_element <- add_model_name(
    proto_data_element,
    object = object
  )
  
  # Store data mapping of feature values for input data.
  proto_data_element@data_mapping <- mapping_input
  
  # Store lookup-table translate feature mapping back to feature values.
  proto_data_element@lookup_table <- feature_set
  
  # Add predictions for input data.
  proto_data_element@predicted_values <- predicted_values_input
  
  # Store shap data. Value column is "shap_value", grouping columns are
  # "feature_name" and "feature_value_mapping". For multinomial and survival
  # outcomes, "shap_outcome" is an additional grouping column.
  if (object@outcome_type %in% c("multinomial", "survival")) {
    proto_data_element@data <- data.table::copy(
      shap_values[, mget(c("feature_name", "feature_value_mapping", "shap_outcome", "shap_value"))]
    )
    
    # Add shap_outcome as additional grouping level.
    proto_data_element@grouping_column <- c(proto_data_element@grouping_column, "shap_outcome")
    
    if (object@outcome_type %in% c("multinomial")) {
      # Convert shap_outcome to categorical values corresponding to the levels
      # in the modelled endpoint.
      proto_data_element@data$shap_outcome <- factor(
        proto_data_element@data$shap_outcome,
        levels = get_outcome_class_levels(object)
      )
    }
    
  } else {
    proto_data_element@data <- data.table::copy(
      shap_values[, mget(c("feature_name", "feature_value_mapping", "shap_value"))]
    )
  }
  
  return(proto_data_element)
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
      feature_set[[feature]] <- factor(feature_info[[feature]]@levels, levels = feature_info[[feature]]@levels)
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



.get_shap_sample_set <- function(
    object,
    feature_set
) {
  # Determine the number of samples that are required.
  n_samples <- max(lengths(feature_set))
  
  # Fill sample set. Feature values are randomly ordered and then distributed
  # over features.
  sample_set <- list()
  for (ii in seq_along(feature_set)) {
    feature <- names(feature_set)[ii]
    feature_values <- feature_set[[feature]]
    feature_values <- feature_values[fam_sample(
      seq_along(feature_values),
      n = length(feature_values),
      replace = FALSE,
      seed = ii
    )]
    sample_set[[feature]] <- rep_len(feature_values, length.out = n_samples)
  }
  
  # Convert to data.table and batch and sample identifiers.
  data <- data.table::as.data.table(sample_set)
  data[, ":="(
    "batch_id" = "generated",
    "sample_id" = seq_len(nrow(data))
  )]
  
  return(as_data_object(
    data = data,
    object = object,
    batch_id_column = "batch_id",
    sample_id_column = "sample_id"
  ))
}



.get_shap_coalitions <- function(
    feature_set,
    depth = 2L
) {
  # Helper function that inserts TRUE at the indices indicated by ones.
  ..fun <- function(ones, n_features) {
    x <- logical(n_features)
    x[ones] <- TRUE
    return(x)
  }

  # Set number of features.
  n_features <- length(feature_set)
  
  # Check that depth is at most n_features - 1L.
  if (depth >= n_features) depth <- n_features - 1L
  if (depth < 1L) return(NULL)
  
  z <- list()
  for (ii in seq_len(depth)) {
    z <- c(
      z,
      utils::combn(
        n_features,
        m = ii,
        FUN = ..fun,
        simplify = FALSE,
        n_features = n_features
      )
    )
  }
  
  # To matrix. Data are stored row-wise.
  z <- matrix(unlist(z), ncol = n_features, byrow = TRUE)
  colnames(z) <- names(feature_set)

  # Use adversarial sampling and keep unique coalitions. Adversarial sampling
  # relies on coalitions that are directly orthogonal to another.
  z <- unique(rbind(z, !z))
  
  return(z)
}



.shap_data_to_mapping <- function(
  data,
  feature_set
) {
  # Convert data to mapping matrix. This uses the fact that all the values in
  # the feature set
  mapping <- list()
  
  # Maps data to a matrix of integers that establishes a mapping to the feature
  # values in feature set.
  for (feature in names(feature_set)) {
    mapping[[feature]] <- match(data@data[[feature]], feature_set[[feature]])
  }
  
  # Create matrix.
  h <- matrix(unlist(mapping), ncol = length(feature_set))
  colnames(h) <- names(feature_set)
  
  return(h)
}



.shap_mapping_to_data <- function(
    mapping,
    feature_set,
    object
) {
  ..fun <- function(feature, y, x) {
    # Use column in mapping matrix x to lookup value from y.
    return(y[x[, feature]])
  }
  
  # Use lookup to fill data.
  data <- mapply(
    ..fun,
    feature = names(feature_set),
    y = feature_set,
    MoreArgs = list("x" = mapping),
    SIMPLIFY = FALSE
  )
  
  # Set names of list elements.
  names(data) <- names(feature_set)
  
  # Convert to data.table and add identifiers.
  data <- data.table::as.data.table(data)
  
  return(as_data_object(
    data = data,
    object = object,
    check_stringency = "external"
  ))
}



.shap_mapping_to_feature_list <- function(
    feature,
    mapping_value, 
    lookup_table
) {
  y <- lookup_table[[feature[1L]]]
  if (is.factor(y)) {
    feature_value <- as.numeric(y)[mapping_value]
    feature_label <- as.character(y)[mapping_value]
  } else {
    feature_value <- y[mapping_value]
    feature_label <- rep_len(NA_character_, length(mapping_value))
  }
  
  return(list(
    "feature_value" = feature_value,
    "feature_label" = feature_label
  ))
}




.shap_randomise_mapping_from_coalition <- function(
  samples,
  coalitions,
  feature_set,
  seed
) {
  # Determine the number of feature values for each value.
  n_feature_values <- lengths(feature_set)
  
  # Start random stream
  rstream_object <- .start_random_number_stream(seed)
  
  # Randomise.
  mapping <- apply(
    samples,
    MARGIN = 1L,
    ..shap_randomise_mapping_from_coalition,
    coalitions = coalitions,
    n_feature_values = n_feature_values,
    rstream_object = rstream_object,
    simplify = FALSE
  )
  
  # Concatenate by rows.
  mapping <- do.call(rbind, mapping)

  return(mapping)
}



..shap_randomise_mapping_from_coalition <- function(
    x,
    coalitions,
    n_feature_values,
    rstream_object
) {
  # Determine the number of feature values to samples for off-coalition
  # features. This should be the same number for each feature in antithetical
  # sampling.
  n_to_draw <- colSums(!coalitions)
  
  mapping <- list()
  for (feature in names(n_feature_values)) {
    # Determine eligible features from in-coalition (on) and off-coalition
    # (off) features.
    on_feature_set <- unname(x[feature])
    off_feature_set <- seq_len(n_feature_values[feature])[-on_feature_set]
    
    # Sample the off-feature set, and append to the on-feature set. This forms
    # the look-up table for forming coalitions.
    feature_set <- c(
      on_feature_set,
      fam_sample(
        off_feature_set,
        size = n_to_draw[feature],
        replace = TRUE,
        rstream_object = rstream_object
      )
    )
    
    # Accumulate off-coalition elements. E.g. with coalitions (across samples
    # for each feature) [0, 1, 1, 0], the lookup-vector is [1, 1, 1, 2].
    lookup_vector <- cumsum(!coalitions[, feature])
    
    # Reset in-coalition elements of the lookup vector, yielding, e.g. [1, 0, 0,
    # 2], and increment by 1. This results in indices (e.g. [2, 1, 1, 3])
    # referring to the feature set, with index 1 corresponding to the
    # in-coalition value.
    lookup_vector <- 1L + lookup_vector * !coalitions[, feature]
    
    # Add features to mapping.
    mapping[[feature]] <- feature_set[lookup_vector]
  }
  
  # Convert to matrix. Mapping consists of columns, and the matrix is sorted
  # this way.
  mapping <- matrix(unlist(mapping), ncol = length(n_feature_values))
  colnames(mapping) <- names(n_feature_values)
  
  return(mapping)
}



.compute_shap_value <- function(
    samples,
    mapping,
    feature_set,
    predicted_values,
    phi_0
) {
  # Initialise values to prevent CRAN NOTEs.
  df <- shap_value <- shap_variance <- m_est <- NULL
  
  # For each sample in mapping_input, compute SHAP.
  shap_values <- apply(
    samples,
    MARGIN = 1L,
    ..compute_shap_value,
    mapping = mapping,
    predicted_values = predicted_values,
    phi_0 = phi_0,
    simplify = FALSE
  )
  
  shap_values <- data.table::rbindlist(shap_values)
  if (is_empty(shap_values)) return(NULL)
  
  # Merge shap-values and update estimates, variance and estimated number of
  # experiments.
  shap_values <- shap_values[
    ,
    list(
      "shap_value" = sum(df * shap_value) / sum(df),
      "shap_variance" = sum(df * shap_variance) / sum(df),
      "m_est" = sum(m_est)
    ),
    by = c("feature_name", "feature_value_mapping", "shap_outcome")
  ]
  
  return(shap_values)
}



..compute_shap_value <- function(
    x,
    mapping,
    predicted_values,
    phi_0
) {
  # x is the mapping corresponding to the sample. First we determine the 
  # coalitions pertaining to current sample. Since `==` is operating by column,
  # we can simply transpose the mapping matrix so that rows become columns. Then
  # the comparison is performed on the columns representing each row, and the
  # result is transposed again.
  coalitions <- t(t(mapping) == x)
  
  # Form a lookup-table for kernel weights.
  n_max_present <- ncol(coalitions)
  n_present <- seq_len(n_max_present + 1L) - 1L
  n_permutations <- choose(n_max_present, n_present)
  kernel_weights <- (n_max_present - 1.0) / (n_permutations * n_present * (n_max_present - n_present))
  kernel_weights[!is.finite(kernel_weights)] <- 0.0
  
  # The total kernel-weight of a single "iteration" is 2 times the number of
  # features (equal to the number of coalitions with a one on/off configuration)
  # times the respective kernel weight. We use this to determine the sample
  # error of the mean for convergence purposes.
  iteration_weight <- 2.0 * n_max_present * kernel_weights[2L]
  
  # Compute the number of features "present" in each coalition. 
  n_present <- rowSums(coalitions)
  
  # Lookup the corresponding kernel weights, and filter non-zero weights.
  kernel_weights <- kernel_weights[n_present + 1L]
  non_zero_weights <- kernel_weights > 0.0

  # Check for empty weights.  
  if (!any(non_zero_weights)) return(NULL)
  
  # Weighted least squares solves for coefficients beta as follows:
  # beta = (t(X) W X)^-1 t(X)W y
  # In the context of kernelSHAP, this means:
  #    beta = phi
  #       X = Z (coalitions),
  #       W = diag(pi) (kernel_weights)
  #   and y = f(h(z)) - phi_0
  
  X <- coalitions[non_zero_weights, , drop = FALSE]
  y <- predicted_values[non_zero_weights, , drop = FALSE] - phi_0
  
  # Instead of computing a diagonal matrix, we rely on equivalent element-wise
  # multiplications (which are considerably cheaper).
  w <- kernel_weights[non_zero_weights]

  # Pre-compute inverse matrix because we need it for computing both the
  # coefficients and their variance.
  inv_mat <- matrix_pseudo_inverse(t(X) %*% (X * w))
  
  # Compute coefficients.
  phi <- inv_mat %*% t(X) %*% (w * y)
  
  # Compute variance for each coefficient: sigma^2 * t(X) W X)^-1,
  # with sigma^2 = 1 / (n - p) * sum (w * (y - X * phi) ^2). This is computed
  # for each component of phi, e.g. probability for a specific class.
  overall_var <- colSums(w * (y - X %*% phi)^2.0) / (length(w) - ncol(X))
  phi_var <- do.call(
    cbind,
    lapply(
      overall_var,
      function(var, inv_mat) (matrix(diag(inv_mat * var), ncol = 1L)),
      inv_mat = inv_mat
    )
  )
  prediction_names <- colnames(phi)
  colnames(phi_var) <- prediction_names
  
  data <- data.table::data.table(
    "feature_name" = rep(colnames(mapping), times = length(prediction_names)),
    "feature_value_mapping" = rep(x, times = length(prediction_names)),
    "m_est" = sum(kernel_weights) / iteration_weight,
    "df" = length(w) - ncol(X),
    "shap_value" = c(phi),
    "shap_variance" = c(phi_var),
    "shap_outcome" = rep(prediction_names, each = ncol(mapping))
  )
  
  return(data)
}



.evaluate_shap_convergence <- function(
  shap_values,
  predicted_values,
  tolerance
) {
  # Determine tolerance scaled to the scale of the problem.
  scale <- max(predicted_values) - min(predicted_values)
  tolerance <- tolerance * scale
  
  return(all(sqrt(shap_values$shap_variance / shap_values$m_est) < tolerance))
}



.predict_from_coalition <- function(
    mapping,
    feature_set,
    object,
    ensemble_method,
    evaluation_time
) {
  # Set prediction type.
  prediction_type <- ifelse(
    object@outcome_type %in% c("survival", "competing_risk"),
    "survival_probability", 
    "default"
  )
  
  # Convert input to dataObject
  data <- .shap_mapping_to_data(
    mapping = mapping,
    feature_set = feature_set,
    object = object
  )
  
  # Predict input data
  prediction_data <- predict(
    object = object,
    newdata = data,
    ensemble_method = ensemble_method,
    time = evaluation_time,
    type = prediction_type
  )
  
  if (object@outcome_type == "continuous") {
    prediction_data <- matrix(prediction_data$predicted_outcome, ncol = 1L)
    colnames(prediction_data) <- "predicted_outcome"
    
  } else if (object@outcome_type %in% c("binomial", "multinomial")) {
    probability_columns <- setdiff(colnames(prediction_data), "predicted_class")
    prediction_data <- as.matrix(prediction_data[, mget(probability_columns)])
    
  } else {
    browser()
  }
  
  return(prediction_data)
}



.hash_mapping <- function(x) {
  return(apply(
    x,
    MARGIN = 1L,
    FUN = rlang::hash,
    simplify = TRUE
  ))
}



.extract_shap_summary <- function(
    x
) {
  # Prevent NOTES due to non-standard evaluation
  feature_name <- feature_value_mapping <- NULL
  
  # Generate object using the incoming familiarDataElementSHAP object as a
  # template.
  data_element <- methods::new(
    "familiarDataElementSHAPSummary",
    x
  )
  
  # Clean reporting elements.
  data_element@data <- NULL
  
  if (is_empty(x@data)) return(data_element)

  # Establish same hashes.
  sample_hashes <- .hash_mapping(x@data_mapping)
  
  # Get data_mapping and turn into a long data.table.
  mapping_data <- data.table::as.data.table(x@data_mapping)
  mapping_data[, "sample_hash" := sample_hashes]
  mapping_data <- data.table::melt(
    data = mapping_data,
    id.vars = "sample_hash",
    variable.name = "feature_name",
    value.name = "feature_value_mapping"
  )
  
  # Insert feature values in mapping data.
  mapping_data[
    ,
    c("feature_value", "feature_label") := .shap_mapping_to_feature_list(
      feature = feature_name,
      mapping_value = feature_value_mapping, 
      lookup_table = x@lookup_table
    ),
    by = "feature_name"
  ]
  
  # Cartesian merge.
  summary_data <- merge(
    x = x@data,
    y = mapping_data,
    by = c("feature_name", "feature_value_mapping"),
    allow.cartesian = TRUE
  )
  
  # Set data.
  data_element@data <- summary_data
  
  # Set identifiers
  data_element@grouping_column <- c(
    data_element@grouping_column,
    c("sample_hash", "feature_value", "feature_label")
  )
  
  return(data_element)
}



.extract_shap_force <- function(
    x
) {
  # Prevent NOTES due to non-standard evaluation
  feature_name <- feature_value_mapping <- NULL
  
  # Generate object using the incoming familiarDataElementSHAP object as a
  # template.
  data_element <- methods::new(
    "familiarDataElementSHAPForce",
    x
  )
  
  # Clean reporting elements.
  data_element@data <- NULL
  
  if (is_empty(x@data)) return(data_element)
browser()
  # Establish same hashes.
  sample_hashes <- .hash_mapping(x@data_mapping)
  
  # Get data_mapping and turn into a long data.table.
  mapping_data <- data.table::as.data.table(x@data_mapping)
  mapping_data[, "sample_hash" := sample_hashes]
  mapping_data <- data.table::melt(
    data = mapping_data,
    id.vars = "sample_hash",
    variable.name = "feature_name",
    value.name = "feature_value_mapping"
  )
  
  # Prediction data
  prediction_data <- data.table::as.data.table(x@predicted_values)
  prediction_data[, "sample_hash" := sample_hashes]
  prediction_data <- data.table::melt(
    data = prediction_data,
    id.vars = "sample_hash",
    variable.name = "shap_outcome",
    value.name = "prediction"
  )
  
  # Cartesian merge.
  force_data <- merge(
    x = x@data,
    y = mapping_data,
    by = c("feature_name", "feature_value_mapping"),
    allow.cartesian = TRUE
  )
  
  merge_cols <- "sample_hash"
  if ("shap_outcome" %in% colnames(force_data)) merge_cols <- c(merge_cols, "shap_outcome")
  
  force_data <- merge(
    x = force_data,
    y = prediction_data,
    by = merge_cols
  )
  
  return(data_element)
}

# 
# # merge_data_elements (familiarDataElementSHAP) --------------------------------
# setMethod(
#   "merge_data_elements",
#   signature(x = "familiarDataElementSHAP"),
#   function(
#     x,
#     x_list,
#     as_data = NULL,
#     as_grouping_column = TRUE,
#     force_data_table = FALSE,
#     ...
#   ) {
#     # For each of the underlying data elements, 
#     
#     # Move identifiers from the identifiers attribute to the data attribute. The
#     # primary reason for doing so is to group and merge similar elements, byt
#     # e.g. from different models.
#     if (!is.null(as_data)) {
#       x_list <- lapply(
#         x_list,
#         .identifier_as_data_attribute,
#         identifier = as_data,
#         as_grouping_column = as_grouping_column
#       )
#     }
#     
#     # Identify items that can be joined.
#     id_table <- identify_element_sets(x = x_list, ...)
#     
#     # Identify the element identifiers that should be grouped.
#     grouped_data_element_ids <- lapply(
#       split(id_table[, c("element_id", "group_id")], by = "group_id"),
#       function(id_table) (id_table$element_id)
#     )
#     
#     # List of data elements.
#     data_elements <- list()
#     
#     for (current_group_data_element_ids in grouped_data_element_ids) {
#       # Copy the first data element in the group and use it as a prototype.
#       prototype_data_element <- x_list[[current_group_data_element_ids[1L]]]
#       
#       # Skip if all shap data elements for the current group are empty.
#       all_is_empty <- all(sapply(
#         x_list[current_group_data_element_ids],
#         function(x) (is_empty(x@data))
#       ))
#       if (all_is_empty) next
#       
#       # Isolate data elements with the same identifiers.
#       current_data_elements <- x_list[current_group_data_element_ids]
#       
#       # Skip if there is nothing to merge.
#       if (length(current_data_elements) == 1L){
#         data_elements <- c(data_elements, current_data_elements)
#         next
#       } 
#       
#       # Update stuff.
#       browser()
#       
#       # Add merged data element to the list.
#       data_elements <- c(data_elements, list(prototype_data_element))
#     }
#     
#     return(data_elements)
#   }
# )




# export_shap (generic) --------------------------------------------------------

#'@title Extract and export individual conditional expectation data.
#'
#'@description Extract and export individual conditional expectation data.
#'
#'@inheritParams export_all
#'@inheritParams export_univariate_analysis_data
#'
#'@inheritDotParams extract_ice
#'@inheritDotParams as_familiar_collection
#'
#'@details Data is usually collected from a `familiarCollection` object.
#'  However, you can also provide one or more `familiarData` objects, that will
#'  be internally converted to a `familiarCollection` object. It is also
#'  possible to provide a `familiarEnsemble` or one or more `familiarModel`
#'  objects together with the data from which data is computed prior to export.
#'  Paths to the previous files can also be provided.
#'
#'  All parameters aside from `object` and `dir_path` are only used if `object`
#'  is not a `familiarCollection` object, or a path to one.
#'
#'@return A list of data.tables (if `dir_path` is not provided), or nothing, as
#'  all data is exported to `csv` files.
#'@exportMethod export_ice_data
#'@md
#'@rdname export_shap-methods
setGeneric(
  "export_shap",
  function(
    object,
    dir_path = NULL,
    aggregate_results = TRUE,
    export_collection = FALSE,
    ...
  ) {
    standardGeneric("export_shap")
  }
)



# export_shap (collection) -----------------------------------------------------

#'@rdname export_shap-methods
setMethod(
  "export_shap",
  signature(object = "familiarCollection"),
  function(
    object,
    dir_path = NULL,
    aggregate_results = TRUE,
    export_collection = FALSE,
    feature_x = NULL,
    feature_y = NULL,
    ...
  ) {
    
    
    # Make sure the collection object is updated.
    object <- update_object(object = object)

    # Generate data for summary plots.
    summary_data_elements <- lapply(
      object@shap_data,
      .extract_shap_summary
    )
    
    # Export summary data.
    summary_data <- .export(
      x = object,
      data_elements = summary_data_elements,
      dir_path = dir_path,
      aggregate_results = TRUE,
      object_class = "familiarDataElementSHAPSummary",
      type = "explanation",
      subtype = "shap_summary"
    )
    
    # Generate data for force plots.
    force_data_elements <- lapply(
      object@shap_data,
      .extract_shap_force
    )
    
    # Export data for force
    
    
    if (!is.null(feature_x) && !is.null(feature_y)) {
      # Generate data for SHAP dependence plots.
    }
    
    browser()
    
    if (!is.null(time_range)) {
      # Check that time_range is valid.
      .check_argument_length(
        time_range, 
        var_name = "time_range",
        min = 2L,
        max = 2L
      )
      
      sapply(
        time_range,
        .check_number_in_valid_range,
        var_name = "time_range",
        range = c(0.0, Inf)
      )
    }
    
    if (export_strata) {
      # Compute kaplan-meier curves.
      strata_data <- lapply(
        object@km_data,
        .compute_risk_stratification_curves,
        time_range = time_range
      )
      
      # Determine hazard ratio and logrank tests. We do this here because the
      # data needs to be aggregated.
      test_data <- lapply(
        object@km_data,
        .compute_risk_stratification_tests,
        time_range = time_range
      )
      
      # Export raw data.
      raw_data <- .export(
        x = object,
        data_slot = "km_data",
        dir_path = dir_path,
        aggregate_results = TRUE,
        type = "stratification",
        subtype = "data"
      )
      
      # Export strata.
      strata_data <- .export(
        x = object,
        data_elements = strata_data,
        dir_path = dir_path,
        aggregate_results = TRUE,
        type = "stratification",
        subtype = "strata"
      )
      
      # Export logrank data.
      logrank_data <- .export(
        x = object,
        data_elements = test_data,
        dir_path = dir_path,
        aggregate_results = TRUE,
        object_class = "familiarDataElementRiskLogrank",
        type = "stratification",
        subtype = "logrank"
      )
      
      # Export hazard ratio data.
      hazard_ratio_data <- .export(
        x = object,
        data_elements = test_data,
        dir_path = dir_path,
        aggregate_results = TRUE,
        object_class = "familiarDataElementRiskHazardRatio",
        type = "stratification",
        subtype = "hazard_ratio"
      )
      
      data_list <- list(
        "data" = raw_data,
        "strata" = strata_data,
        "logrank" = logrank_data,
        "hazard_ratio_data" = hazard_ratio_data
      )
      
      if (export_collection) {
        data_list <- c(
          data_list,
          list("collection" = object)
        )
      } 
      
      return(data_list)
      
    } else {
      return(.export(
        x = object,
        data_slot = "shap_data",
        dir_path = dir_path,
        aggregate_results = TRUE,
        type = "explanation",
        subtype = "shap",
        object_class = "familiarDataElementSHAP",
        export_collection = export_collection
      ))
    }
  }
)



# export_shap (general) ----------------------------------------------------

#'@rdname export_shap-methods
setMethod(
  "export_shap",
  signature(object = "ANY"),
  function(
    object,
    dir_path = NULL,
    aggregate_results = TRUE,
    export_collection = FALSE,
    ...
  ) {
    
    # Attempt conversion to familiarCollection object.
    object <- do.call(
      as_familiar_collection,
      args = c(
        list(
          "object" = object,
          "data_element" = "export_shap",
          "aggregate_results" = aggregate_results
        ),
        list(...)
      )
    )
    
    return(do.call(
      export_shap,
      args = c(
        list(
          "object" = object,
          "dir_path" = dir_path,
          "aggregate_results" = aggregate_results,
          "export_collection" = export_collection
        ),
        list(...)
      )
    ))
  }
)


# 
# # .export (familiarDataElementSHAP) --------------------------------------------
# setMethod(
#   ".export",
#   signature(x = "familiarDataElementSHAP"),
#   function(
#     x,
#     x_list, 
#     aggregate_results = FALSE,
#     ...
#   ) {
#     
#     if (aggregate_results) {
#       x_list <- .compute_data_element_estimates(x_list)
#     }
#     
#     # Determine identifiers that should be merged. Since the feature values of
#     # the x and y features may be different (e.g. numeric and factor), merging
#     # them would cause features values to merged incorrectly.
#     browser()
#     merging_identifiers <- setdiff(names(x@identifiers), c("feature_x", "feature_y"))
#     
#     # Merge data elements.
#     x <- merge_data_elements(
#       x = x_list,
#       as_data = merging_identifiers,
#       as_grouping_column = TRUE,
#       force_data_table = TRUE
#     )
#     
#     return(x)
#   }
# )
