#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
#' @include Transformation.R
NULL

# update_object (generic) ------------------------------------------------------

#' @title Update familiar S4 objects to the most recent version.
#'
#' @description Provides backward compatibility for familiar objects exported to
#'   a file. This mitigates compatibility issues when working with files that
#'   become outdated as new versions of familiar are released, e.g. because
#'   slots have been removed.
#'   
#'   Major version releases (e.g., versions 1.0.0, 2.0.0) introduce breaking
#'   changes which can result in objects that cannot be updated.
#'
#' @param object A `familiarModel`, a `familiarEnsemble`, a `familiarData` or
#'   `familiarCollection` object.
#' @param ... Unused arguments.
#'
#' @return An up-to-date version of the respective S4 object.
#' @exportMethod update_object
#' @md
#' @rdname update_object-methods
setGeneric("update_object", function(object, ...) standardGeneric("update_object"))



## update_object (familiarModel) -----------------------------------------------

#' @rdname update_object-methods
setMethod(
  "update_object",
  signature(object = "familiarModel"),
  function(object, ...) {
    if (tail(object@familiar_version, n = 1L) < "2.0.0") {
      ..error_cannot_update_object(object, "2.0.0")
    }
    
    if (!methods::validObject(object)) ..error_updated_object_invalid(object)

    # Update package version.
    object <- add_package_version(object = object)

    return(object)
  }
)



## update_object (familiarEnsemble) --------------------------------------------

#' @rdname update_object-methods
setMethod(
  "update_object",
  signature(object = "familiarEnsemble"),
  function(object, ...) {
    if (tail(object@familiar_version, n = 1L) < "2.0.0") {
      ..error_cannot_update_object(object, "2.0.0")
    }
    
    if (!methods::validObject(object)) ..error_updated_object_invalid(object)
    
    # Update package version.
    object <- add_package_version(object = object)

    return(object)
  }
)


## update_object (familiarData) ------------------------------------------------

#' @rdname update_object-methods
setMethod(
  "update_object",
  signature(object = "familiarData"),
  function(object, ...) {
    if (tail(object@familiar_version, n = 1L) < "2.0.0") {
      ..error_cannot_update_object(object, "2.0.0")
    }
    
    if (!methods::validObject(object)) ..error_updated_object_invalid(object)
    
    # Update package version.
    object <- add_package_version(object = object)

    return(object)
  }
)



## update_object (familiarCollection) ------------------------------------------

#' @rdname update_object-methods
setMethod(
  "update_object", signature(object = "familiarCollection"),
  function(object, ...) {
    if (tail(object@familiar_version, n = 1L) < "2.0.0") {
      ..error_cannot_update_object(object, "2.0.0")
    }
    
    if (!methods::validObject(object)) ..error_updated_object_invalid(object)
    
    # Update package version.
    object <- add_package_version(object = object)
    
    return(object)
  }
)



## update_object (vimpTable) ---------------------------------------------------
#' @rdname update_object-methods
setMethod(
  "update_object",
  signature(object = "vimpTable"),
  function(object, ...) {
    # Update package version.
    object <- add_package_version(object = object)

    return(object)
  }
)



## update_object (familiarNoveltyDetector) -------------------------------------
#' @rdname update_object-methods
setMethod(
  "update_object",
  signature(object = "familiarNoveltyDetector"),
  function(object, ...) {
    if (tail(object@familiar_version, n = 1L) < "2.0.0") {
      ..error_cannot_update_object(object, "2.0.0")
    }
    
    if (!methods::validObject(object)) ..error_updated_object_invalid(object)
    
    # Update package version.
    object <- add_package_version(object = object)

    return(object)
  }
)



## update_object (featureInfo) -------------------------------------------------
#' @rdname update_object-methods
setMethod(
  "update_object",
  signature(object = "featureInfo"),
  function(object, ...) {
    if (tail(object@familiar_version, n = 1L) < "2.0.0") {
      ..error_cannot_update_object(object, "2.0.0")
    }
    
    if (!methods::validObject(object)) ..error_updated_object_invalid(object)
    
    # Update package version.
    object <- add_package_version(object = object)

    return(object)
  }
)



## update_object (featureInfoParametersTransformationPowerTransform) -----------

#' @rdname update_object-methods
setMethod(
  "update_object",
  signature(object = "featureInfoParametersTransformationPowerTransform"),
  function(object, ...) {
    
    if (tail(object@familiar_version, n = 1L) < "2.0.0") {
      ..error_cannot_update_object(object, "2.0.0")
    }
    
    if (!methods::validObject(object)) ..error_updated_object_invalid(object)
    
    # Update package version.
    object <- add_package_version(object = object)
    
    return(object)
  }
)



## update_object (experimentData) ----------------------------------------------

#' @rdname update_object-methods
setMethod(
  "update_object",
  signature(object = "experimentData"),
  function(object, ...) {
    if (tail(object@familiar_version, n = 1L) < "2.0.0") {
      ..error_cannot_update_object(object, "2.0.0")
    }
    
    if (!methods::validObject(object)) ..error_updated_object_invalid(object)
    
    # Update package version.
    object <- add_package_version(object = object)

    return(object)
  }
)



## update_object (list) ------------------------------------------------------

#' @rdname update_object-methods
setMethod(
  "update_object",
  signature(object = "list"),
  function(object, ...) {
    # Pass to underlying methods.
    object <- lapply(
      object,
      update_object,
      ...
    )

    return(object)
  }
)



## update_object (ANY) ------------------------------------------------------
#' @rdname update_object-methods
setMethod(
  "update_object",
  signature(object = "ANY"),
  function(object, ...) {
    # Fallback method for missing or unknown items.
    return(object)
  }
)
