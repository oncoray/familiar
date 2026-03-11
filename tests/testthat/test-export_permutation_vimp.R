# Don't perform any further tests on CRAN due to time of running the complete
# test.
testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE

model <- familiar::train_familiar(
  data = familiar:::test_create_good_data("binomial"),
  vimp_method = "mim",
  learner = "glm_logistic",
  parallel = FALSE
)


results <- familiar:::test_export_specific(
  export_function = familiar:::export_permutation_vimp,
  data_element = "permutation_vimp",
  create_novelty_detector = FALSE,
  debug = debug_flag
)
