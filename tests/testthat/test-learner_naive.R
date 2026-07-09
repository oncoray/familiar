# First test if all selectable learners are also available
familiar:::test_all_learners_available(
  learners = familiar:::.get_available_naive_learners(show_general = TRUE)
)

# Don't perform any further tests on CRAN due to time of running the complete
# test.
testthat::skip_on_cran()
testthat::skip_on_ci()

familiar:::test_all_learners_train_predict_vimp(
  learners = familiar:::.get_available_naive_learners(show_general = FALSE),
  has_vimp = FALSE
)

familiar:::test_all_learners_parallel_train_predict_vimp(
  learners = familiar:::.get_available_naive_learners(show_general = FALSE),
  has_vimp = FALSE
)

testthat::skip("Skip hyperparameter optimisation, unless manual.")

familiar:::test_hyperparameter_optimisation(
  learners = familiar:::.get_available_naive_learners(show_general = TRUE),
  debug = FALSE
)
