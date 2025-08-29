#' Create randomised groups Creates randomised groups, e.g. for tests that
#' depend on splitting (continuous) data into groups, such as the
#' Hosmer-Lemeshow test
#'
#' The default fast mode is based on random sampling, whereas the slow mode is
#' based on probabilistic joining of adjacent groups. As the name suggests, fast
#' mode operates considerably more efficient.
#'
#' @param x Vector with data used for sorting. Groups are formed based on
#'   adjacent values.
#' @param y Vector with markers, e.g. the events. Should be 0 or 1 (for an
#'   event).
#' @param sample_identifiers data.table with sample_identifiers. If provide, a
#'   list of grouped sample_identifiers will be returned, and integers
#'   otherwise.
#' @param n_max_groups Maximum number of groups that need to be formed.
#' @param n_min_groups Minimum number of groups that need to be formed.
#' @param n_min_y_in_group Minimum number of y=1 in each group for a valid
#'   group.
#'
#' @details Creates randomised groups, e.g. for tests that depend on splitting
#'   (continuous) data into groups, such as the Hosmer-Lemeshow test
#'
#' @return List of group sample ids or indices.
#' @md
#' @keywords internal
create_randomised_groups <- function(
    x, 
    y = NULL, 
    sample_identifiers, 
    n_max_groups = NULL, 
    n_min_groups = NULL, 
    n_min_y_in_group = NULL, 
    n_groups_init = 30L,
    unique_samples_only = TRUE
) {

  # Suppress NOTES due to non-standard evaluation in data.table
  group_id <- cum_y <- weight <- cum_prob_lower <- cum_prob_upper <- NULL
  exclude <- y_in_group <- NULL

  # Populate the generic table.
  data <- data.table::copy(sample_identifiers[, mget(get_id_columns(id_depth = "series"))])
  data[, "x" := x]
  if (is.null(y)) {
    data[, "y" := 0L]
  } else {
    data[, "y" := y]
  }
  if (unique_samples_only) data <- unique(data)
  
  # initial parsing ------------------------------------------------------------
  # - Get number of x
  n_x <- nrow(data)

  # - Get number of y
  n_y <- sum(data$y)

  # - Get the maximum of groups
  if (is.null(n_max_groups)) {
    n_max_groups <- ceiling(2.5 * n_x^(1.0 / 3.0))
  }

  # - Update maximum number of groups
  if (n_y > 0.0 && !is.null(n_min_y_in_group)) {
    n_max_groups <- min(c(n_max_groups, floor(n_y / n_min_y_in_group)))
  }

  # - Update mininum number of groups based on n_max_groups
  if (is.null(n_min_groups)) {
    n_min_groups <- min(c(n_max_groups, ceiling(1.0 * n_x^(1.0 / 3.0))))
  }

  # Some checks
  if (n_max_groups < n_min_groups || n_max_groups > n_x || n_max_groups < 2L) {
    return(NULL)
  }
  data <- data[order(x)]

  # Initial loop counter
  loop_iter <- 0L

  while (TRUE) {
    # Draw a random number of groups between n_min_groups and n_max_groups
    if (n_min_groups == n_max_groups) {
      n_group_draw <- n_max_groups
    } else {
      n_group_draw <- fam_sample(
        x = seq.int(from = n_min_groups, to = n_max_groups, by = 1L),
        size = 1L, 
        replace = FALSE
      )
    }

    # Draw a randomised groups assignment
    random_group_id <- sort(
      fam_sample(
        x = seq_len(n_group_draw),
        size = n_x, 
        replace = TRUE
      )
    )

    # Assign group id
    data[, "group_id" := random_group_id]

    # Check if all groups have the minimum required y (e.g. events/group size)
    if (n_y > 0L && !is.null(n_min_y_in_group)) {
      dt_y <- data[, list(y_in_group = sum(y)), by = group_id]
      if (all(dt_y$y_in_group >= n_min_y_in_group)) break
      
    } else {
      # If there is no minimum number of events required, only run one
      # iteration
      break
    }

    # Update loop_iter in case the sample was not good enough
    loop_iter <- loop_iter + 1L

    # Break from outer loop after 10 unsuccessful random samplings and create
    # a static split instead
    if (loop_iter >= 10L) {
      # Cumulative sum over y
      data[, "cum_y" := cumsum(y)]

      # Assign static group ids
      data[, "group_id" := findInterval(
        x = cum_y,
        vec = c(
          -Inf,
          stats::quantile(x = cum_y, (1L:(n_min_groups - 1L)) / n_min_groups),
          Inf
        )
      )]
      
      # Break from loop
      break
    }
  }
  
  # Get sample identifiers for each group
  out_groups <- split(
    data[, mget(c(get_id_columns(id_depth = "series"), "group_id"))], 
    by = "group_id",
    keep.by = FALSE
  )

  return(out_groups)
}
