# Don't perform any further tests on CRAN due to time of running the complete
# test.
testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE

# Test plotting from dataObject with selection of numeric features.
data <- familiar:::test_create_good_data(outcome_type = "binomial")
p_bin <- familiar::plot_ordered_target(
  object = data, 
  features = c("feature_1", "feature_2a", "feature_2b")
)

data <- familiar:::test_create_good_data(outcome_type = "multinomial")
p_mult <- familiar::plot_ordered_target(
  object = data, 
  features = c("feature_1", "feature_2a", "feature_2b")
)

data <- familiar:::test_create_good_data(outcome_type = "continuous")
p_cont <- familiar::plot_ordered_target(
  object = data, 
  features = c("feature_1", "feature_2a", "feature_2b")
)

data <- familiar:::test_create_good_data(outcome_type = "survival")
p_surv_1 <- familiar::plot_ordered_target(
  object = data, 
  features = c("feature_1", "feature_2a", "feature_2b")
)
p_surv_2 <- familiar::plot_ordered_target(
  object = data, 
  features = c("feature_1", "feature_2a", "feature_2b"),
  evaluation_times = c(1.0, 5.0, 10.0)
)
testthat::test_that(
  "Ordered target plot for numeric features work correctly.",
  {
    testthat::expect_true(is(p_bin[[1L]], "ggplot"))
    testthat::expect_true(is(p_mult[[1L]], "ggplot"))
    testthat::expect_true(is(p_cont[[1L]], "ggplot"))
    testthat::expect_true(is(p_surv_1[[1L]], "ggplot"))
    testthat::expect_true(is(p_surv_2[[1L]], "ggplot"))
    
    testthat::expect_length(p_bin, 3L)
    testthat::expect_length(p_mult, 3L)
    testthat::expect_length(p_cont, 3L)
    testthat::expect_length(p_surv_1, 3L)
    testthat::expect_length(p_surv_2, 3L)
  }
)


# Test plotting from dataObject with selection of categorical features.
data <- familiar:::test_create_good_data(outcome_type = "binomial")
p_bin <- familiar::plot_ordered_target(
  object = data,
  features = c("feature_3a", "feature_3b", "feature_4")
)


# Test plotting from dataObject with features contain NA values.
