#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


# promote_vimp_method ----------------------------------------------------------
setMethod(
  "promote_vimp_method",
  signature(object = "familiarVimpMethod"),
  function(object) {
    # Extract vimp_method.
    method <- object@vimp_method

    if (method %in% .get_available_concordance_vimp_method()) {
      # Concordance-based methods.
      object <- methods::new("familiarConcordanceVimp", object)
    } else if (method %in% .get_available_univariate_mutual_information_vimp_method()) {
      # Mutual information maximisation.
      object <- methods::new("familiarUnivariateMutualInfoVimp", object)
    } else if (method %in% .get_available_multivariate_mutual_information_vimp_method()) {
      # Multivariate information methods.
      object <- methods::new("familiarMultivariateMutualInfoVimp", object)
    } else if (method %in% .get_available_correlation_vimp_methods()) {
      # Correlation-based methods.
      object <- methods::new("familiarCorrelationVimp", object)
    } else if (method %in% .get_available_corelearn_gini_vimp_method()) {
      # Gini measure.
      object <- methods::new("familiarCoreLearnGiniVimp", object)
    } else if (method %in% .get_available_corelearn_mdl_vimp_method()) {
      # MDL variable importance method.
      object <- methods::new("familiarCoreLearnMDLVimp", object)
    } else if (method %in% .get_available_corelearn_relieff_exp_rank_vimp_method()) {
      # ReliefF with exponentially decreasing rank.
      object <- methods::new("familiarCoreLearnRelieffExpRankVimp", object)
    } else if (method %in% .get_available_corelearn_gain_ratio_vimp_method()) {
      # Gain ratio measure.
      object <- methods::new("familiarCoreLearnGainRatioVimp", object)
    } else if (method %in% .get_available_univariate_regression_vimp_methods()) {
      # Univariate regression-based methods.
      object <- methods::new("familiarUnivariateRegressionVimp", object)
    } else if (method %in% .get_available_multivariate_regression_vimp_methods()) {
      # Multivariate regression-based methods.
      object <- methods::new("familiarMultivariateRegressionVimp", object)
    } else if (method %in% .get_available_glmnet_ridge_vimp_methods()) {
      # Ridge penalised regression model-based methods.

      # Create a familiarModel and promote to the right class.
      object <- methods::new(
        "familiarModel",
        vimp_method = "none",
        learner = method,
        outcome_type = object@outcome_type,
        hyperparameters = object@hyperparameters,
        outcome_info = object@outcome_info,
        feature_info = object@feature_info,
        required_features = object@required_features,
        run_table = object@run_table,
        project_id = object@project_id
      )

      # Promote to the correct subclass.
      object <- promote_learner(object)
      
    } else if (method %in% .get_available_glmnet_lasso_vimp_methods()) {
      # Lasso penalised regression model-based methods.

      # Create a familiarModel and promote to the right class.
      object <- methods::new(
        "familiarModel",
        vimp_method = "none",
        learner = method,
        outcome_type = object@outcome_type,
        hyperparameters = object@hyperparameters,
        outcome_info = object@outcome_info,
        feature_info = object@feature_info,
        required_features = object@required_features,
        run_table = object@run_table,
        project_id = object@project_id
      )

      # Promote to the correct subclass.
      object <- promote_learner(object)
      
    } else if (method %in% .get_available_glmnet_elastic_net_vimp_methods()) {
      # Elastic net penalised regression model-based methods.

      # Create a familiarModel and promote to the right class.
      object <- methods::new("familiarModel",
        vimp_method = "none",
        learner = method,
        outcome_type = object@outcome_type,
        hyperparameters = object@hyperparameters,
        outcome_info = object@outcome_info,
        feature_info = object@feature_info,
        required_features = object@required_features,
        run_table = object@run_table,
        project_id = object@project_id
      )

      # Promote to the correct subclass.
      object <- promote_learner(object)
      
    } else if (
      method %in% .get_available_rfsrc_vimp_methods() ||
      method %in% .get_available_rfsrc_default_vimp_methods()
    ) {
      # Random forest variable importance methods.

      # Create a familiarModel and promote to the right class.
      object <- methods::new("familiarModel",
        vimp_method = "none",
        learner = ifelse(method %in% .get_available_rfsrc_vimp_methods(),
          "random_forest_rfsrc",
          "random_forest_rfsrc_default"
        ),
        outcome_type = object@outcome_type,
        hyperparameters = object@hyperparameters,
        outcome_info = object@outcome_info,
        feature_info = object@feature_info,
        required_features = object@required_features,
        run_table = object@run_table,
        project_id = object@project_id
      )

      # Promote to the correct subclass.
      object <- promote_learner(object)

      # Set the variable importance parameters for the method.
      object <- ..set_vimp_parameters(object, method = method)
      
    } else if (
      method %in% .get_available_ranger_vimp_methods() ||
      method %in% .get_available_ranger_default_vimp_methods()
    ) {
      # Ranger random forest variable importance methods.

      # Create a familiarModel and promote to the right class.
      object <- methods::new("familiarModel",
        vimp_method = "none",
        learner = ifelse(
          method %in% .get_available_ranger_vimp_methods(),
          "random_forest_ranger",
          "random_forest_ranger_default"
        ),
        outcome_type = object@outcome_type,
        hyperparameters = object@hyperparameters,
        outcome_info = object@outcome_info,
        feature_info = object@feature_info,
        required_features = object@required_features,
        run_table = object@run_table,
        project_id = object@project_id
      )

      # Promote to the correct subclass.
      object <- promote_learner(object)

      # Set the variable importance parameters for the method.
      object <- ..set_vimp_parameters(object, method = method)
      
    } else if (method %in% .get_available_random_vimp_methods()) {
      # Random variable importances.
      object <- methods::new("familiarRandomVimp", object)
      
    } else if (method %in% .get_available_none_vimp_methods()) {
      # No variable importance: all features are equally important.
      object <- methods::new("familiarNoneVimp", object)
      
    } else if (method %in% .get_available_signature_only_vimp_methods()) {
      # Signature only methods.
      object <- methods::new("familiarSignatureVimp", object)
      
    } else if (method %in% .get_available_no_features_vimp_methods()) {
      # No variable importance methods - no features are selected, leading to
      # naive models.
      object <- methods::new("familiarNoFeaturesVimp", object)
    }

    return(object)
  }
)



.get_vimp_hyperparameters <- function(
    data, 
    method, 
    outcome_type, 
    names_only = FALSE
) {
  # Get the outcome type from the data object, if available
  if (!is.null(data)) outcome_type <- data@outcome_type

  # Create familiarModel
  vimp_method_object <- methods::new(
    "familiarVimpMethod",
    vimp_method = method,
    outcome_type = outcome_type
  )

  # Set up the specific model
  vimp_method_object <- promote_vimp_method(vimp_method_object)

  # Variable importance hyperparameters
  model_hyperparameters <- get_default_hyperparameters(vimp_method_object, data = data)

  # Extract names from parameter list
  if (names_only == TRUE) {
    model_hyperparameters <- names(model_hyperparameters)
  }

  # Return hyperparameter list, or names of parameters
  return(model_hyperparameters)
}



.check_vimp_outcome_type <- function(
    method, 
    outcome_type, 
    as_flag = FALSE
) {
  # Create familiarModel
  vimp_method_object <- methods::new(
    "familiarVimpMethod",
    vimp_method = method,
    outcome_type = outcome_type
  )

  # Set up the specific model
  vimp_method_object <- promote_vimp_method(vimp_method_object)

  # Check validity.
  vimp_method_available <- is_available(vimp_method_object)

  if (as_flag) return(vimp_method_available)

  # Check if the vimp method or familiar model has been successfully promoted.
  if (
    !is_subclass(class(vimp_method_object)[1L], "familiarVimpMethod") &&
    !is_subclass(class(vimp_method_object)[1L], "familiarModel")
  ) {
    ..error(paste0(
      method, " is not a valid variable importance method. ",
      "Please check the vignette for available methods."
    ))
  }

  if (!vimp_method_available) {
    ..error(paste0(method, " is not available for \"", outcome_type, "\" outcomes."))
  }

  # Check that the required package can be loaded.
  require_package(
    x = vimp_method_object,
    purpose = paste0("to assess variable importance using the ", method, " method"),
    message_type = "backend_error"
  )
}



# add_package_version ----------------------------------------------------------
setMethod(
  "add_package_version",
  signature(object = "familiarVimpMethod"),
  function(object) {
    # Set version of familiar
    return(.add_package_version(object = object))
  }
)
