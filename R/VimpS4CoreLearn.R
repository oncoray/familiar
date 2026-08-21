#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarCoreLearnVimp",
  contains = "familiarVimpMethod"
)

setClass(
  "familiarCoreLearnGiniVimp",
  contains = "familiarCoreLearnVimp"
)

setClass(
  "familiarCoreLearnMDLVimp",
  contains = "familiarCoreLearnVimp"
)

setClass(
  "familiarCoreLearnRelieffExpRankVimp",
  contains = "familiarCoreLearnVimp"
)

setClass(
  "familiarCoreLearnGainRatioVimp",
  contains = "familiarCoreLearnVimp"
)


# initialize -------------------------------------------------------------------
setMethod(
  "initialize",
  signature(.Object = "familiarCoreLearnVimp"),
  function(.Object, ...) {
    # Update with parent class first.
    .Object <- callNextMethod()

    # Update package
    .Object@package <- "CORElearn"

    return(.Object)
  }
)


.get_available_corelearn_gini_vimp_method <- function(show_general = TRUE) {
  return("gini")
}

.get_available_corelearn_mdl_vimp_method <- function(show_general = TRUE) {
  return("mdl")
}

.get_available_corelearn_relieff_exp_rank_vimp_method <- function(show_general = TRUE) {
  return("relieff_exp_rank")
}

.get_available_corelearn_gain_ratio_vimp_method <- function(show_general = TRUE) {
  return("gain_ratio")
}



# is_available (gini) ----------------------------------------------------------
setMethod(
  "is_available",
  signature(object = "familiarCoreLearnGiniVimp"),
  function(object, ...) {
    ..deprecation_corelearn()
    return(FALSE)
  }
)

# is_available (mdl) -----------------------------------------------------------
setMethod(
  "is_available",
  signature(object = "familiarCoreLearnMDLVimp"),
  function(object, ...) {
    ..deprecation_corelearn()
    return(FALSE)
  }
)

# is_available (relieff) -------------------------------------------------------
setMethod(
  "is_available",
  signature(object = "familiarCoreLearnRelieffExpRankVimp"),
  function(object, ...) {
    ..deprecation_corelearn()
    return(FALSE)
  }
)

# is_available (gain ratio) ----------------------------------------------------
setMethod(
  "is_available", 
  signature(object = "familiarCoreLearnGainRatioVimp"),
  function(object, ...) {
    ..deprecation_corelearn()
    return(FALSE)
  }
)
