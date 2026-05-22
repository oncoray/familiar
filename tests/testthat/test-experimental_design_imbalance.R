testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE

# Simple test
vimp_methods <- c("none", "mim")

# Imbalance corrections using full undersampling
experimental_designs <- c("ip(fs+mb)", "ip(mb)")
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
