#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


# .file_exists (generic task) --------------------------------------------------
setMethod(
  ".file_exists",
  signature(object = "familiarTask"),
  function(object, ...) {
    if (is.na(object@file) || is.null(object@file)) return(FALSE)
    
    return(file.exists(object@file))
  }
)


.generate_trainer_tasks <- function() {
  
  for (data_id in data_ids) {
    for (run_id in run_ids) {
      for (vimp_method in vimp_methods) {
        for (learner in learners) {
          # Set up trainer task.
          
          # Set up hyperparameter extraction task.
          
        }
      }
    }
  }
  
  # Add tasks related to variable importance objects.
  
  # Add tasks related to data processing for learners.
}


.generate_vimp_tasks <- function() {
  
  # Check if vimp should be computed separately or is computed during 
  # hyperparameter optimisation.
  
  for (data_id in data_ids) {
    for (run_id in run_ids) {
      for (vimp_method in vimp_methods) {
        
        # Check if the variable importance method requires any computation.
        # For example, signature_only, none and random do not require
        # computation.
        
        # Set up variable importance computation task.
        
        # Set up variable importance hyperparameter task.
        
      }
    }
  }
  
  # Add tasks related to data processing for vimp methods.
  
}



.generate_learner_data_preprocessing_tasks <- function() {
  
}


.generate_vimp_data_preprocessing_tasks <- function() {
  
}


