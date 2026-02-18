# Don't perform any further tests on CRAN due to running time.
testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE
exp_dir <- file.path(tempdir(), "familiar_intro_test")

testthat::test_that(
  "Basic example works",
  {
    if (!debug_flag) on.exit(unlink(exp_dir, recursive = TRUE), add = TRUE)
    
    testthat::expect_success(
      suppressWarnings(
        familiar::summon_familiar(
          data = datasets::iris,
          experiment_dir = exp_dir,
          outcome_type = "multinomial",
          outcome_column = "Species",
          experimental_design = "fs+mb",
          vimp_method = "mrmr",
          learner = "glm",
          parallel = FALSE,
          verbose = debug_flag
        )
      )
    )
  }
)

unlink(exp_dir, recursive = TRUE)

testthat::test_that(
  "Basic example with formula interface",
  {
    if (!debug_flag) on.exit(unlink(exp_dir, recursive = TRUE), add = TRUE)
    
    testthat::expect_success(
      suppressWarnings(
        familiar::summon_familiar(
          Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
          data = datasets::iris,
          experiment_dir = exp_dir,
          outcome_type = "multinomial",
          experimental_design = "fs+mb",
          vimp_method = "mrmr",
          learner = "glm",
          parallel = FALSE,
          verbose = debug_flag
        )
      )
    )
  }
)

unlink(exp_dir, recursive = TRUE)



testthat::test_that(
  "Example with warm start",
  {
    if (!debug_flag) on.exit(unlink(exp_dir, recursive = TRUE), add = TRUE)
    
    testthat::expect_success(
      suppressWarnings(
        experiment_data <- familiar::precompute_feature_info(
          data = iris,
          experiment_dir = exp_dir,
          outcome_type = "multinomial",
          outcome_column = "Species",
          experimental_design = "bs(fs+mb,5)",
          parallel = FALSE,
          verbose = debug_flag
        )
      )
    )
    unlink(exp_dir, recursive = TRUE)
    
    testthat::expect_success(
      suppressWarnings(
        familiar::summon_familiar(
          data = iris,
          experiment_data = experiment_data,
          experiment_dir = exp_dir,
          outcome_type = "multinomial",
          outcome_column = "Species",
          vimp_method = "mrmr",
          learner = "glm",
          parallel = FALSE,
          verbose = debug_flag
        )
      )
    )
  }
)

unlink(exp_dir, recursive = TRUE)
