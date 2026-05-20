testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE

# Simple test
vimp_methods <- c("none", "mim", "mim")
experimental_designs <- c("fs+mb", "fs+mb", "mb")
for (ii in seq_along(vimp_methods)) {
  familiar:::integrated_test(
    experimental_design = experimental_designs[ii],
    vimp_method = vimp_methods[ii],
    parallel = FALSE,
    estimation_type = "point",
    n_important_features = 2L,
    outcome_type_available = "binomial",
    debug = debug_flag
  )
}


# Bootstrap (without optimisation within bootstraps)
experimental_designs <- c("bt(fs+mb, 3)", "bt(fs+mb, 3)", "bt(mb, 3)")
for (ii in seq_along(vimp_methods)) {
  familiar:::integrated_test(
    experimental_design = experimental_designs[ii],
    vimp_method = vimp_methods[ii],
    parallel = FALSE,
    estimation_type = "point",
    n_important_features = 2L,
    outcome_type_available = "binomial",
    debug = debug_flag
  )
}


# Bootstrap (with pre-processing and optimisation within bootstraps)
experimental_designs <- c("bs(fs+mb, 3)", "bs(fs+mb, 3)", "bs(mb, 3)")
for (ii in seq_along(vimp_methods)) {
  familiar:::integrated_test(
    experimental_design = experimental_designs[ii],
    vimp_method = vimp_methods[ii],
    parallel = FALSE,
    estimation_type = "point",
    n_important_features = 2L,
    outcome_type_available = "binomial",
    debug = debug_flag
  )
}


# Cross-validation
experimental_designs <- c("cv(fs+mb, 3)", "cv(fs+mb, 3)", "cv(mb, 3)")
for (ii in seq_along(vimp_methods)) {
  familiar:::integrated_test(
    experimental_design = experimental_designs[ii],
    vimp_method = vimp_methods[ii],
    parallel = FALSE,
    estimation_type = "point",
    n_important_features = 2L,
    outcome_type_available = "binomial",
    debug = debug_flag
  )
}


# Leave-one-out cross-validation
experimental_designs <- c("lv(fs+mb)", "lv(fs+mb)", "lv(mb)")
for (ii in seq_along(vimp_methods)) {
  familiar:::integrated_test(
    experimental_design = experimental_designs[ii],
    vimp_method = vimp_methods[ii],
    parallel = FALSE,
    estimation_type = "point",
    n_important_features = 2L,
    outcome_type_available = "binomial",
    debug = debug_flag
  )
}

# Imbalance corrections using full undersampling
experimental_designs <- c("ip(fs+mb)", "ip(fs+mb)", "ip(mb)")
for (ii in seq_along(vimp_methods)) {
  familiar:::integrated_test(
    experimental_design = experimental_designs[ii],
    imbalance_correction_method = "full_undersampling",
    vimp_method = vimp_methods[ii],
    parallel = FALSE,
    estimation_type = "point",
    n_important_features = 2L,
    outcome_type_available = "binomial",
    debug = debug_flag
  )
}


# Imbalance corrections using full undersampling
experimental_designs <- c("ip(fs+mb)", "ip(fs+mb)", "ip(mb)")
for (ii in seq_along(vimp_methods)) {
  familiar:::integrated_test(
    experimental_design = experimental_designs[ii],
    imbalance_correction_method = "random_undersampling",
    imbalance_n_partitions = 3,
    vimp_method = vimp_methods[ii],
    parallel = FALSE,
    skip_evaluation_elements = "all",
    outcome_type_available = "binomial",
    debug = debug_flag
  )
}
