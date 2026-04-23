# This contains the archiving function for creating the zip files. We move it
# here so that CRAN doesn't get antsy. This is not a function that should be
# used (or visible to) users, and is not part of familiar.

test_run_archive_experiment <- function(parameters) {
  # Create data.
  data <- familiar:::test_create_good_data_random_missing(
    outcome_type = parameters$outcome_type
  )

  # Run the experiment and return the relevant familiar objects.
  familiar_objects <- do.call(
    familiar::summon_familiar,
    args = c(
      list(
        "data" = data,
        "parallel" = FALSE,
        ".force_output" = TRUE
      ),
      parameters
    )
  )

  # Set the file name of the container.
  rds_file_name <- paste0(
    gsub(
      pattern = ".",
      replacement = "_",
      x = as.character(utils::packageVersion("familiar")),
      fixed = TRUE
    ),
    "_", parameters$outcome_type,
    ".rds"
  )

  # Get the current working directory.
  current_wd <- getwd()
  
  saveRDS(
    familiar_objects,
    file = file.path(
      current_wd,
      "tests",
      "old_experiments",
      rds_file_name
    )
  )
}



test_create_experiment_archive <- function(
    outcome_type = c("binomial", "multinomial", "continuous", "survival"),
    parallel = TRUE
) {
  # Creates zip files for testing update_object methods.

  test_generate_experiment_parameters <- coro::generator(function(outcome_type) {
    for (current_outcome_type in outcome_type) {
      # Set learner
      learner <- switch(
        current_outcome_type,
        "binomial" = c("glm_logistic", "lasso"),
        "multinomial" = c("glm", "lasso"),
        "continuous" = c("glm_gaussian", "lasso"),
        "survival" = c("cox", "survival_regr_weibull"))

      coro::yield(list(
        "experimental_design" = "bs(fs+mb,3)",
        "outcome_type" = current_outcome_type,
        "vimp_method" = c("mim", "concordance"),
        "learner" = learner))
    }
  })

  # Yield current set of parameters.
  config_parameters <- coro::collect(
    test_generate_experiment_parameters(outcome_type = outcome_type))

  if (parallel == TRUE) {
    cl <- parallel::makeCluster(type = "PSOCK", length(config_parameters))
    
    # Iterate over parameter sets.
    parallel::parLapply(
      cl = cl,
      X = config_parameters,
      test_run_archive_experiment
    )
    
    parallel::stopCluster(cl)
    
  } else {
    lapply(
      config_parameters,
      test_run_archive_experiment
    )
  }
}
