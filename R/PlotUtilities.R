..required_plotting_packages <- function(extended = FALSE) {
  plot_packages <- c("ggplot2", "labeling", "scales", "rlang")

  if (extended) plot_packages <- c(plot_packages, "gtable")

  return(plot_packages)
}



#' Familiar ggplot2 theme
#'
#' This is the default theme used for plots created by familiar. The theme uses
#' `ggplot2::theme_light` as the base template.
#'
#' @param base_size Base font size in points. Size of other plot text elements
#'   is based off this.
#' @param base_family Font family used for text elements.
#' @param base_line_size Base size for line elements, in points.
#' @param base_rect_size Base size for rectangular elements, in points.
#'
#' @return A complete plotting theme.
#' @export
theme_familiar <- function(
    base_size = 10.0,
    base_family = "",
    base_line_size = 0.5,
    base_rect_size = 0.5
) {
  
  # The default familiar theme is based on ggplot2::theme_light.
  ggtheme <- ggplot2::theme_light(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  )

  # Set colour to black.
  ggtheme$axis.text$colour <- "black"
  ggtheme$axis.ticks$colour <- "black"
  ggtheme$axis.line <- ggplot2::element_line(
    colour = "black",
    lineend = "square"
  )

  # Legend does not have a legend or border, and reserves less space.
  ggtheme$legend.background <- ggplot2::element_blank()
  ggtheme$legend.key <- ggplot2::element_blank()
  ggtheme$legend.key.size <- grid::unit(base_size * 1.1, "pt")
  ggtheme$legend.justification <- c("left", "center")
  ggtheme$legend.margin <- ggplot2::margin(0.0, 0.0, 0.0, 0.0, "pt")

  # Panel does not have a background, grid, or border.
  ggtheme$panel.background <- ggplot2::element_blank()
  ggtheme$panel.border <- ggplot2::element_blank()
  ggtheme$panel.grid <- ggplot2::element_blank()

  # Avoid removing some elements altogether by directly assigning NULL.
  ggtheme["panel.grid.major"] <- list(NULL)
  ggtheme["panel.grid.minor"] <- list(NULL)

  # Minimal plot margins
  ggtheme$plot.margin <- ggplot2::margin(1.0, 1.0, 1.0, 1.0, "pt")

  # The plot does not have a background
  ggtheme$plot.background <- ggplot2::element_blank()

  # Make title a bit smaller, and bold.
  ggtheme$plot.title <- ggplot2::element_text(
    face = "bold",
    size = ggplot2::rel(1.1),
    hjust = 0.0,
    vjust = 1.0,
    margin = ggplot2::margin(b = base_size / 2.0)
  )

  # Make subtitle a bit smaller.
  ggtheme$plot.subtitle <- ggplot2::element_text(
    size = ggplot2::rel(0.8),
    hjust = 0.0,
    vjust = 1.0,
    margin = ggplot2::margin(b = base_size / 2.0)
  )

  # Make caption a bit smaller.
  ggtheme$plot.caption <- ggplot2::element_text(
    size = ggplot2::rel(0.7),
    hjust = 1.0,
    vjust = 1.0,
    margin = ggplot2::margin(b = base_size / 2.0)
  )

  # Make tag bold.
  ggtheme$plot.tag <- ggplot2::element_text(
    face = "bold",
    hjust = 0.0,
    vjust = 0.7
  )

  # Make strip text black.
  ggtheme$strip.text <- ggplot2::element_text(
    size = ggplot2::rel(0.8),
    colour = "grey10",
    margin = ggplot2::margin(
      t = base_size / 4.0, 
      r = base_size / 4.0, 
      b = base_size / 4.0,
      l = base_size / 4.0,
      unit = "pt"
    )
  )

  # Remove strip background
  ggtheme$strip.background <- ggplot2::element_blank()

  return(ggtheme)
}



.check_ggtheme <- function(ggtheme) {
  # Check if the provided theme is a suitable theme.
  if (inherits(ggtheme, "theme")) {
    # Check if the theme is complete.
    if (!attr(ggtheme, "complete")) {
      ..error(paste0(
        "The plotting theme is not complete. The most likely cause is lack ",
        "of a valid template, such as theme_familiar or ggplot2::theme_light. ",
        "Note that ggplot2::theme is designed to tweak existing themes when creating a plot."
      ))
    }
    
  } else if (is.null(ggtheme)) {
    ggtheme <- theme_familiar(base_size = 9.0)
    
  } else {
    # Get the specified theme.
    ggtheme_fun <- switch(
      ggtheme,
      "default" = theme_familiar,
      "theme_familiar" = theme_familiar,
      "theme_gray" = ggplot2::theme_gray,
      "theme_grey" = ggplot2::theme_grey,
      "theme_bw" = ggplot2::theme_bw,
      "theme_linedraw" = ggplot2::theme_linedraw,
      "theme_light" = ggplot2::theme_light,
      "theme_dark" = ggplot2::theme_dark,
      "theme_minimal" = ggplot2::theme_minimal,
      "theme_classic" = ggplot2::theme_classic
    )

    if (is.null(ggtheme_fun)) {
      ..error(paste0(
        "The selected theme is not the default theme, or a standard ggplot2 theme. Found: ", ggtheme
      ))
    }

    # Generate theme
    ggtheme <- ggtheme_fun(base_size = 9.0)
  }

  return(ggtheme)
}



#' Checks and sanitizes splitting variables for plotting.
#'
#' @param x data.table or data.frame containing the data used for splitting.
#' @param split_by (*optional*) Splitting variables. This refers to column names
#'   on which datasets are split. A separate figure is created for each split.
#'   See details for available variables.
#' @param color_by (*optional*) Variables used to determine fill colour of plot
#'   objects. The variables cannot overlap with those provided to the `split_by`
#'   argument, but may overlap with other arguments. See details for available
#'   variables.
#' @param linetype_by (*optional*) Variables that are used to determine the
#'   linetype of lines in a plot. The variables cannot overlap with those
#'   provided to the `split_by` argument, but may overlap with other arguments.
#'   Sett details for available variables.
#' @param facet_by (*optional*) Variables used to determine how and if facets of
#'   each figure appear. In case the `facet_wrap_cols` argument is `NULL`, the
#'   first variable is used to define columns, and the remaing variables are
#'   used to define rows of facets. The variables cannot overlap with those
#'   provided to the `split_by` argument, but may overlap with other arguments.
#'   See details for available variables.
#' @param x_axis_by (*optional*) Variable plotted along the x-axis of a plot.
#'   The variable cannot overlap with variables provided to the `split_by` and
#'   `y_axis_by` arguments (if used), but may overlap with other arguments. Only
#'   one variable is allowed for this argument. See details for available
#'   variables.
#' @param y_axis_by (*optional*) Variable plotted along the y-axis of a plot.
#'   The variable cannot overlap with variables provided to the `split_by` and
#'   `x_axis_by` arguments (if used), but may overlap with other arguments. Only
#'   one variable is allowed for this argument. See details for available
#'   variables.
#' @param available Names of columns available for splitting.
#'
#' @details This internal function allows some flexibility regarding the exact
#'   input. Allowed splitting variables should be defined by the available
#'   argument.
#'
#' @return A sanitized list of splitting variables.
#' @md
#' @keywords internal
.check_plot_splitting_variables <- function(
    x,
    split_by = NULL,
    color_by = NULL,
    linetype_by = NULL,
    facet_by = NULL,
    x_axis_by = NULL,
    y_axis_by = NULL,
    available = NULL
) {
  # Find unique variables
  splitting_vars <- c(split_by, color_by, linetype_by, facet_by, x_axis_by, y_axis_by)

  if (is.null(available) && length(splitting_vars) == 0L) {
    return(list())
    
  } else if (is.null(available) && length(splitting_vars) > 0L) {
    ..error(paste0(
      "The current plot has no required splitting variables defined, but ",
      paste_s(splitting_vars),
      ifelse(length(splitting_vars) == 1L, " was assigned.", " were assigned.")
    ))
  }

  # Filter available down to those present in the data.
  filter_available <- intersect(available, colnames(x))

  # Filter available down to those that have more than one variable
  filter_available <- filter_available[sapply(
    filter_available,
    function(ii, x) (data.table::uniqueN(x = x, by = ii) > 1L),
    x = x
  )]
  
  if (is.null(filter_available)) {
    return(list())
    
  } else if (!all(filter_available %in% splitting_vars)) {
    missing_vars <- filter_available[!filter_available %in% splitting_vars]
    ..error(paste0(
      "The current plot requires ",
      paste_s(filter_available),
      ifelse(length(filter_available) > 1L, " as splitting variables", " as a splitting_variable"),
      ", but ",
      paste_s(missing_vars),
      ifelse(length(missing_vars) == 1L, " was not assigned.", " were not assigned.")
    ))
  }

  # Update available
  available <- filter_available

  # Generate output
  output_list <- list()

  # Update split_by
  if (!is.null(split_by) && any(split_by %in% available)) {
    output_list$split_by <- intersect(split_by, available)
  }

  # Update color_by
  if (!is.null(color_by) && any(color_by %in% available)) {
    output_list$color_by <- intersect(color_by, available)
  }

  # update linetype_by
  if (!is.null(linetype_by) && any(linetype_by %in% available)) {
    output_list$linetype_by <- intersect(linetype_by, available)
  }

  # update facet_by
  if (!is.null(facet_by) && any(facet_by %in% available)) {
    output_list$facet_by <- intersect(facet_by, available)
  }

  # update x_axis_by
  if (!is.null(x_axis_by) && any(x_axis_by %in% available)) {
    output_list$x_axis_by <- intersect(x_axis_by, available)
  }

  # update y_axis_by
  if (!is.null(y_axis_by) && any(y_axis_by %in% available)) {
    output_list$y_axis_by <- intersect(y_axis_by, available)
  }

  # Check split_by variable.
  .check_value_not_shared(output_list$split_by, output_list$color_by, "split_by", "color_by")
  .check_value_not_shared(output_list$split_by, output_list$linetype_by, "split_by", "linetype_by")
  .check_value_not_shared(output_list$split_by, output_list$facet_by, "split_by", "facet_by")
  .check_value_not_shared(output_list$split_by, output_list$x_axis_by, "split_by", "x_axis_by")
  .check_value_not_shared(output_list$split_by, output_list$y_axis_by, "split_by", "y_axis_by")

  # Check x_axis_by variable and y_axis_by variables.
  .check_value_not_shared(output_list$x_axis_by, output_list$y_axis_by, "x_axis_by", "y_axis_by")

  # Check length of x_axis_by and y_axis_by variables.
  .check_argument_length(output_list$x_axis_by, "x_axis_by", min = 0L, max = 1L)
  .check_argument_length(output_list$y_axis_by, "y_axis_by", min = 0L, max = 1L)

  return(output_list)
}



.parse_plot_facet_by <- function(x, facet_by, facet_wrap_cols) {
  if (is.null(facet_by)) {
    return(list())
    
  } else if (length(facet_by) == 1L) {
    if (is.null(facet_wrap_cols)) {
      return(list("facet_cols" = quos(!!ensym(facet_by))))
    } else {
      return(list("facet_by" = quos(!!ensym(facet_by))))
    }
    
  } else {
    if (is.null(facet_wrap_cols)) {
      facet_col <- facet_by[1L]
      facet_rows <- facet_by[2L:length(facet_by)]
      return(list(
        "facet_cols" = quos(!!ensym(facet_col)),
        "facet_rows" = quos(!!!parse_exprs(facet_rows))
      ))
      
    } else {
      return(list("facet_by" = quos(!!!parse_exprs(facet_by))))
    }
  }
}



.create_plot_subtitle <- function(
    x, 
    split_by = NULL, 
    additional = NULL
) {
  # Do not create a subtitle if there is no subtitle to be created.
  subtitle <- NULL
  
  # Generate subtitle from splitting variables and data.
  if (!is.null(split_by)) {
    subtitle <- c(
      subtitle,
      sapply(
        split_by,
        function(name, x) {
          split_variable_name <- name

          if (split_variable_name == "vimp_method") {
            split_variable_name <- "VIMP method"
          } else if (split_variable_name == "learner") {
            split_variable_name <- "learner"
          } else if (split_variable_name == "data_set") {
            split_variable_name <- "data set"
          } else if (split_variable_name == "evaluation_time") {
            split_variable_name <- "time point"
          } else if (split_variable_name == "sample_id") {
            split_variable_name <- "sample"
          } else if (split_variable_name %in% c("feature_name", "feature")) {
            split_variable_name <- "feature"
          }

          # Remove all underscores.
          split_variable_name <- gsub(
            x = split_variable_name, 
            pattern = "_",
            replacement = " ",
            fixed = TRUE
          )

          # Parse to an elementary string.
          split_variable_name <- paste0(split_variable_name, ": ", x[[name]][1L])

          return(split_variable_name)
        },
        x = x
      )
    )
  }
  
  # Generate additional strings from additional.
  if (!is.null(additional)) {
    subtitle <- c(
      subtitle,
      mapply(
        function(name, value) {
          # Remove all underscores.
          split_variable_name <- gsub(
            x = name, 
            pattern = "_",
            replacement = " ",
            fixed = TRUE
          )

          # Parse to an elementary string.
          split_variable_name <- paste0(split_variable_name, ": ", value)
        },
        name = names(additional),
        value = additional
      )
    )
  }

  # Check if any subtitle was generated.
  if (is.null(subtitle)) return(NULL)

  # Combine into single string.
  subtitle <- paste_s(subtitle)

  return(subtitle)
}



.add_time_to_plot_subtitle <- function(value) {
  return(list("time point" = value))
}



.create_plot_subtype <- function(
    x,
    subtype = NULL, 
    split_by = NULL, 
    additional = NULL
) {
  # Generate additional terms for the subtype, based on splits.
  if (!is.null(split_by)) {
    subtype <- c(
      subtype,
      as.character(sapply(
        split_by,
        function(jj, x) (x[[jj]][1L]),
        x = x
      ))
    )
  }

  if (!is.null(additional)) {
    subtype <- c(
      subtype,
      sapply(additional, function(jj) as.character(jj[1L]))
    )
  }

  if (is.null(subtype)) return(NULL)

  # Combine into a single string.
  subtype <- paste0(subtype, collapse = "_")

  return(subtype)
}



.create_plot_legend_title <- function(
    user_label, 
    color_by = NULL,
    linetype_by = NULL, 
    combine_legend = FALSE
) {
  # Sent for inspection
  .check_input_plot_args(
    legend_label = user_label,
    combine_legend = combine_legend
  )
  
  if (is.null(color_by) && is.null(linetype_by)) {
    # No splitting variables are used
    return(list(
      "guide_color" = NULL,
      "guide_linetype" = NULL
    ))
  }
  
  # Collect required list entries
  req_entries <- character(0L)
  if (!is.null(color_by)) {
    req_entries <- c(req_entries, "guide_color")
  }
  if (!is.null(linetype_by)) {
    req_entries <- c(req_entries, "guide_linetype")
  }
  
  # Waiver: no user input
  if (is.waive(user_label)) {
    if (combine_legend) {
      legend_label <- gsub(
        x = paste0(unique(c(color_by, linetype_by)), collapse = " & "),
        pattern = "_",
        replacement = " ",
        fixed = TRUE
      )
      
      return(list(
        "guide_color" = legend_label,
        "guide_linetype" = legend_label
      ))
      
    } else {
      # Colour labels
      if (!is.null(color_by)) {
        color_guide_label <- gsub(
          x = paste0(color_by, collapse = " & "),
          pattern = "_",
          replacement = " ",
          fixed = TRUE
        )
        
      } else {
        color_guide_label <- NULL
      }
      
      # Linetype labels
      if (!is.null(linetype_by)) {
        linetype_guide_label <- gsub(
          x = paste0(linetype_by, collapse = " & "),
          pattern = "_",
          replacement = " ",
          fixed = TRUE
        )
        
      } else {
        linetype_guide_label <- NULL
      }
      
      return(list(
        "guide_color" = color_guide_label,
        "guide_linetype" = linetype_guide_label
      ))
    }
  } else if (is.null(user_label)) {
    # NULL input
    
    return(list(
      "guide_color" = NULL,
      "guide_linetype" = NULL
    ))
    
  } else if (is.list(user_label)) {
    # List input
    
    # Check entries for existence
    for (current_entry in req_entries) {
      if (!current_entry %in% names(user_label)) {
        ..error(paste0(
          "A legend name is missing for ", current_entry, 
          ". Please set this name to a \"", current_entry, 
          "\" list element, e.g. list(\"",  current_entry, 
          "\"=\"some name\", ...)."
        ))
      }
    }
    
    # Select required entries
    user_label <- user_label[names(user_label) %in% req_entries]
    
    # Check that all entries are the same
    if (combine_legend && length(req_entries) >= 2L) {
      if (!all(sapply(
        user_label[2L:length(user_label)],
        identical,
        user_label[[1L]]
      ))) {
        ..error(paste0(
          "Not all provided legend names are identical, but identical legend ",
          "names are required for combining the legend."
        ))
      }
    }
    
    return(user_label)
    
  } else if (length(req_entries) >= 2L && !combine_legend) {
    # Single input where multiple is required
    
    ..error(paste0(
      "Multiple legend names are required, but only one is provided. ",
      "Please return a list with ",
      paste0("\"", req_entries, "\"", collapse = ", "), " elements."
    ))
    
  } else {
    # Single input
    
    return(list(
      "guide_color" = user_label, 
      "guide_linetype" = user_label
    ))
  }
}



.create_plot_guide_table <- function(
    x, 
    color_by = NULL, 
    linetype_by = NULL, 
    discrete_palette = NULL, 
    combine_legend = TRUE
) {
  
  .get_guide_tables <- function(x, color_by, linetype_by, discrete_palette) {
    # Suppress NOTES due to non-standard evaluation in data.table
    color_id <- linetype_id <- NULL
    
    # Select unique variables
    unique_vars <- unique(c(color_by, linetype_by))
    
    # Check whether there are any unique splitting variables
    if (is.null(unique_vars)) return(NULL)
    
    # Generate a guide table
    guide_table <- data.table::data.table(expand.grid(lapply(
      rev(unique_vars), 
      function(ii, x) (levels(x[[ii]])), 
      x = x
    )))
    
    # Rename variables
    data.table::setnames(x = guide_table, rev(unique_vars))
    
    # Convert to factors
    for (ii in unique_vars) {
      guide_table[[ii]] <- factor(
        x = guide_table[[ii]],
        levels = levels(x[[ii]])
      )
    }
    
    # Order columns according to unique_vars
    data.table::setcolorder(
      x = guide_table, 
      neworder = unique_vars
    )
    
    # Order data set by columns
    data.table::setorderv(
      x = guide_table, 
      cols = unique_vars
    )
    
    # Set breaks
    breaks <- apply(guide_table, 1L, paste, collapse = ", ")
    
    # Extend guide table
    if (!is.null(color_by)) {
      # Generate breaks
      guide_table$color_breaks <- factor(
        x = breaks,
        levels = breaks
      )
      
      # Define colour groups
      guide_table[, "color_id" := .GRP, by = color_by]
      
      # Get the palette to use.
      discr_palette <- .get_palette(
        x = discrete_palette,
        n = max(guide_table$color_id),
        palette_type = "qualitative"
      )
      
      # Assign colour values
      guide_table[, "color_values" := discr_palette[color_id]]
    }
    
    if (!is.null(linetype_by)) {
      # Generate breaks
      guide_table$linetype_breaks <- factor(
        x = breaks,
        levels = breaks
      )
      
      # Define linetype groups
      guide_table[, "linetype_id" := .GRP, by = linetype_by]
      
      # Get the palette to use
      line_palette <- scales::linetype_pal()(max(guide_table$linetype_id))
      
      # Assign linetypes
      guide_table[, "linetype_values" := line_palette[linetype_id]]
    }
    
    return(guide_table)
  }
  
  if (is_empty(x)) return(list("data" = x))
  
  # Extract guide tables
  if (combine_legend) {
    guide_list <- list(
      "guide_color" = .get_guide_tables(
        x = x, 
        color_by = color_by, 
        linetype_by = linetype_by, 
        discrete_palette = discrete_palette
      ),
      "guide_linetype" = .get_guide_tables(
        x = x, 
        color_by = color_by, 
        linetype_by = linetype_by, 
        discrete_palette = discrete_palette
      )
    )
    
  } else {
    guide_list <- list(
      "guide_color" = .get_guide_tables(
        x = x, 
        color_by = color_by, 
        linetype_by = NULL, 
        discrete_palette = discrete_palette
      ),
      "guide_linetype" = .get_guide_tables(
        x = x, 
        color_by = NULL, 
        linetype_by = linetype_by, 
        discrete_palette = discrete_palette
      )
    )
  }
  
  # Filter out lists corresponding to missing split variables
  guide_list <- guide_list[!sapply(list(color_by, linetype_by), is.null)]
  
  if (length(guide_list) == 0L) return(list("data" = x))
  
  # Initialise return list
  return_list <- list()
  
  # Add break column of the remaining lists to x
  for (guide_type in names(guide_list)) {
    if (guide_type == "guide_color") {
      # Add color_breaks to x
      if (combine_legend) {
        x <- merge(
          x = x,
          y = guide_list[[guide_type]][, mget(c(unique(c(color_by, linetype_by)), "color_breaks"))],
          by = unique(c(color_by, linetype_by)),
          all.x = TRUE,
          all.y = FALSE
        )
        
      } else {
        x <- merge(
          x = x,
          y = guide_list[[guide_type]][, mget(c(color_by, "color_breaks"))],
          by = color_by,
          all.x = TRUE, 
          all.y = FALSE
        )
      }
      
      # Return guide_color
      return_list[[guide_type]] <- guide_list[[guide_type]]
      
    } else if (guide_type == "guide_linetype") {
      # Add linetype_breaks to x
      if (combine_legend) {
        x <- merge(
          x = x,
          y = guide_list[[guide_type]][, mget(c(unique(c(color_by, linetype_by)), "linetype_breaks"))],
          by = unique(c(color_by, linetype_by)),
          all.x = TRUE, 
          all.y = FALSE
        )
        
      } else {
        x <- merge(
          x = x,
          y = guide_list[[guide_type]][, mget(c(linetype_by, "linetype_breaks"))],
          by = linetype_by,
          all.x = TRUE, 
          all.y = FALSE
        )
      }
      
      # Return guide_linetype
      return_list[[guide_type]] <- guide_list[[guide_type]]
    }
  }
  
  # Add updated data
  return_list$data <- x
  
  return(return_list)
}



.add_plot_cluster_name <- function(
    x, 
    color_by = NULL, 
    facet_by = NULL, 
    singular_cluster_character = "\u2014"
) {
  # Suppress NOTES due to non-standard evaluation in data.table
  cluster_size <- cluster_id <- feature <- new_cluster_id <- cluster_name <- NULL

  ..integer_to_char <- function(x) {
    # Initialise placeholders
    x_remain <- x
    new_string <- character(0L)

    while (as.integer(ceiling(x_remain / 26L)) > 0L) {
      # Determine the modulo.
      mod <- x_remain %% 26L

      # Find if mod is equal to 0, which would indicate Z.
      mod <- ifelse(mod == 0L, 26L, mod)

      # Add letter
      new_string <- c(new_string, LETTERS[mod])

      # Update the remain variable
      x_remain <- (x_remain - mod) / 26L
    }

    return(paste(rev(new_string), collapse = ""))
  }

  # Identify splitting variables
  splitting_vars <- unique(c(color_by, facet_by))

  # Split x by splitting variables.
  if (length(splitting_vars) > 0L) {
    x <- split(x, by = splitting_vars)
  } else {
    x <- list(x)
  }

  # Iterate and add cluster names.
  x <- lapply(
    x,
    function(y) {
      # Check if x is empty.
      if (is_empty(y)) return(NULL)
      
      # This is for backward compatibility.
      if (!all(c("cluster_id", "cluster_size") %in% colnames(y))) {
        # Add cluster name
        y[, "cluster_name" := singular_cluster_character]
        
        return(y)
      }
      
      # Only determine cluster_name for those clusters that have cluster_size >
      # 1. Also, the most important features should receive a higher replacement
      # cluster_id.
      y_short <- y[cluster_size > 1L, mget(c("feature", "cluster_id"))]
      
      if (!is_empty(y_short)) {
        # Remove unused levels for the name column. The levels of name are
        # ordered according to importance.
        y_short <- droplevels(y_short)
        
        # Set placeholder cluster id
        y_short[, "new_cluster_id" := NA_integer_]
        
        new_id <- 1L
        for (current_feature in levels(y_short$feature)) {
          # Provide new cluster id in case none exists.
          if (is.na(y_short[feature == current_feature, ]$new_cluster_id[1L])) {
            # Find the old cluster id.
            old_cluster_id <- y_short[feature == current_feature, ]$cluster_id[1L]
            
            # Update all entries with the same old cluster id.
            y_short[cluster_id == old_cluster_id, "new_cluster_id" := new_id]
            
            # Increment new cluster id.
            new_id <- new_id + 1L
          }
        }
        
        # Determine cluster name based on id.
        y_short[, "cluster_name" := ..integer_to_char(new_cluster_id), by = "feature"]
        
        # Drop redundant columns
        y_short[, ":="(
          "cluster_id" = NULL, 
          "new_cluster_id" = NULL
        )]
        
        # Merge with y.
        y <- merge(
          x = y, 
          y = y_short,
          by = "feature",
          all = TRUE
        )
        
        # Mark singular clusters
        y[is.na(cluster_name), "cluster_name" := singular_cluster_character]
        
      } else {
        # Mark singular clusters
        y[, "cluster_name" := singular_cluster_character]
      }
      
      return(y)
    }
  )
  
  x <- data.table::rbindlist(x, use.names = TRUE)
  
  return(x)
}


.split_data_by_plot_facet <- function(x, plot_layout_table = NULL, ...) {
  if (is_empty(x)) return(NULL)
  
  if (is.null(plot_layout_table)) {
    plot_layout_table <- do.call(
      .get_plot_layout_table,
      args = c(list("x" = x), list(...))
    )
  }

  # Derive facet_by
  facet_by <- setdiff(
    colnames(plot_layout_table),
    c("col_id", "row_id")
  )

  if (length(facet_by) > 0L) {
    # Merge the plot_layout_table into x. This will keep things in order. All
    # levels are kept.
    x <- merge(
      x = x,
      y = plot_layout_table,
      by = facet_by,
      all = TRUE
    )
    
  } else {
    x <- cbind(x, plot_layout_table)
  }

  # Split data by row, then column
  split_data <- split(
    x, 
    by = c("col_id", "row_id"), 
    sorted = TRUE
  )

  return(split_data)
}



.get_plot_layout_dims <- function(plot_layout_table = NULL, ...) {
  # Create the plot_layout_table if it is not provided.
  if (is.null(plot_layout_table)) {
    plot_layout_table <- do.call(
      .get_plot_layout_table, 
      args = list(...)
    )
  }

  # Return (nrows, ncols)
  return(c(
    max(plot_layout_table$row_id),
    max(plot_layout_table$col_id)
  ))
}



.get_plot_layout_table <- function(x, facet_by, facet_wrap_cols) {
  if (is.null(facet_by)) {
    # Simple 1x1 layout without facets.
    plot_layout_table <- data.table::data.table(
      "col_id" = 1L,
      "row_id" = 1L
    )
    
  } else if (is.null(facet_wrap_cols)) {
    # Generate a plot_layout_table and order it
    plot_layout_table <- expand.grid(
      lapply(
        facet_by,
        function(column, x) levels(x[[column]]),
        x = x
      ),
      KEEP.OUT.ATTRS = FALSE
    )
    
    plot_layout_table <- data.table::as.data.table(plot_layout_table)
    data.table::setnames(plot_layout_table, facet_by)
    data.table::setorderv(x = plot_layout_table, cols = facet_by)

    # Find the number of columns
    n_cols <- length(unique(x[[facet_by[1L]]]))

    # Add column id to the plot_layout_table
    plot_layout_table[, "col_id" := .GRP, by = get(facet_by[1L])]

    if (length(facet_by) > 1L) {
      # Find the number of rows
      n_levels <- sapply(
        facet_by[2L:length(facet_by)],
        function(ii, x) {
          if (is.factor(x[[ii]])) {
            return(nlevels(x[[ii]]))
          } else {
            return(length(unique(x[[ii]])))
          }
        },
        x = x
      )
      n_rows <- prod(n_levels)

      # Add row id to the plot_layout_table
      facet_row_cols <- facet_by[2L:length(facet_by)]
      plot_layout_table[, "row_id" := .GRP, by = mget(facet_row_cols)]
      
    } else {
      # There is only one row
      n_rows <- 1L
      plot_layout_table[, "row_id" := 1L]
    }
    
  } else {
    # Generate a plot_layout_table, and order
    plot_layout_table <- unique(x[, (facet_by), with = FALSE], by = facet_by)
    data.table::setorderv(x = plot_layout_table, cols = facet_by)

    # Number of columns is provided using facet_wrap_cols.
    len_table <- nrow(plot_layout_table)
    n_cols <- facet_wrap_cols
    n_rows <- ceiling(len_table / n_cols)

    # Generate the column and row positions.
    col_ids <- rep(seq_len(n_cols), times = n_rows)[seq_len(len_table)]
    row_ids <- rep(seq_len(n_rows), each = n_cols)[seq_len(len_table)]

    # Add column and row ids to the plot_layout_table.
    plot_layout_table[, ":="(
      "col_id" = col_ids,
      "row_id" = row_ids
    )]
  }

  return(plot_layout_table)
}



.combine_plot_elements <- function(
  g_main,
  g_new,
  element_name,
  spacer = NULL,
  stack_direction = "vertical"
) {
  
  for (current_element_name in element_name) {
    # Determine matching elements in both g_main and g_new.
    element_names_main <- g_main$layout$name
    present_elements_main <- element_names_main[sapply(
      element_names_main, 
      startswith_any, 
      prefix = current_element_name
    )]
    
    element_names_new <- g_new$layout$name
    present_elements_new <- element_names_new[sapply(
      element_names_new,
      startswith_any,
      prefix = current_element_name
    )]
    
    if (length(present_elements_main) != 1L || length(present_elements_new) != 1L) next
    
    grob_index_main <- which(g_main$layout$name == present_elements_main)
    grob_index_new <- which(g_new$layout$name == present_elements_new)
    
    # Determine if there is anything to add from g_new.
    if (any(sapply(c("zeroGrob", "nullGrob"), function(ii, x) (is(x, ii)), x = g_new$grobs[[grob_index_new]]))) next
    
    grob_main <- g_main$grobs[[grob_index_main]]
    grob_new <- g_new$grobs[[grob_index_new]]
    
    # If there is an empty grob in g_main, simply copy the element from g_new
    # into g_main.
    if (any(sapply(c("zeroGrob", "nullGrob"), function(ii, x) (is(x, ii)), x = g_main$grobs[[grob_index_main]]))) {
      g_main$grobs[[grob_index_main]] <- grob_new
      
    } else {
      g <- list(grob_main, grob_new)
      
      # Find widths and heights
      widths <- lapply(g, .gtable_get_grob_aspect_size, aspect = "width")
      if (!is.null(spacer) && stack_direction == "horizontal") {
        widths <- append(widths, 0L, after = 1L)
        # Direct insertion of spacer with append results in spacer losing its
        # simpleUnit class.
        widths[[2L]] <- spacer
      }
      widths <- do.call(grid::unit.c, widths)
      
      heights <- lapply(g, .gtable_get_grob_aspect_size, aspect = "height")
      if (!is.null(spacer) && stack_direction == "vertical") {
       heights <- append(heights, 0L, after = 1L)
       # Direct insertion of spacer with append results in spacer losing its
       # simpleUnit class.
       heights[[2L]] <- spacer
      }
      heights <- do.call(grid::unit.c, heights)
      
      if (stack_direction == "vertical") {
        # Concatenate the widths.
        widths <- max(widths)

      } else {
        # Concatenate the heights.
        heights <- max(heights)
      }
      
      # Set up basic gtable.
      g_combine <- gtable::gtable(
        widths = widths,
        heights = heights,
        name = present_elements_main
      )
      
      # Insert grob from main.
      g_combine <- gtable::gtable_add_grob(
        g_combine,
        grobs = g[[1L]],
        t = 1L,
        l = 1L
      )
      
      # Insert grob from new.
      g_combine <- gtable::gtable_add_grob(
        g_combine,
        grobs = g[[2L]],
        t = length(heights),
        l = length(widths)
      )
      
      # Insert spacer.
      if (!is.null(spacer) && stack_direction == "vertical") {
        spacer_position <- c(2L, 2L, 1L, 1L)
        names(spacer_position) <- c("t", "b", "l", "r")
        
        g_combine <- .gtable_insert_spacer(
          g_combine,
          position = spacer_position,
          height = spacer
        )
        
      } else if (!is.null(spacer) && stack_direction == "horizontal") {
        spacer_position <- c(1L, 1L, 2L, 2L)
        names(spacer_position) <- c("t", "b", "l", "r")
        
        g_combine <- .gtable_insert_spacer(
          g_combine,
          position = spacer_position,
          width = spacer
        )
      }
      
      g_main$grobs[[grob_index_main]] <- g_combine
    }
  }
  
  g_main <- .gtable_update_layout(g = g_main)
  
  return(g_main)
}



.compose_figure <- function(
    figure_list,
    plot_layout_table,
    x_text_shared = "overall",
    x_label_shared = "overall",
    y_text_shared = "overall",
    y_label_shared = "overall",
    facet_wrap_cols = NULL,
    ggtheme = NULL
) {
  # Suppress NOTES due to non-standard evaluation in data.table
  col_id <- row_id <- is_present <- n_present <- NULL
  type <- NULL
  
  # Global layout --------------------------------------------------------------
  
  # Add figure names to plot_layout_table.
  plot_layout_table[, figure_name := paste0(row_id, ".", col_id)]
  plot_layout_table[, is_present := figure_name %in% names(figure_list)]
  
  # Drop columns with only missing information.
  empty_cols <- plot_layout_table[
    ,
    list("n_present" = sum(is_present)),
    by = "col_id"
  ]
  empty_cols <- empty_cols[n_present == 0L]$col_id
  if (length(empty_cols) > 0L) {
    plot_layout_table <- plot_layout_table[!col_id %in% empty_cols]
  }
  
  # Drop rows with only missing information.
  empty_rows <- plot_layout_table[
    ,
    list("n_present" = sum(is_present)),
    by = "row_id"
  ]
  empty_rows <- empty_rows[n_present == 0L]$row_id
  if (length(empty_rows) > 0L) {
    plot_layout_table <- plot_layout_table[!row_id %in% empty_rows]
  }

  # Check that any part of the plot is remaining
  if (is_empty(plot_layout_table)) return(NULL)
  
  # Create missing figure panels -----------------------------------------------
  
  # Identify and add missing figures.
  missing_figures <- plot_layout_table[is_present == FALSE]$figure_name
  for (missing_figure in missing_figures) {
    # Get col_id and row_id to identify template figures.
    current_row_id <- plot_layout_table[figure_name == missing_figure]$row_id
    current_col_id <- plot_layout_table[figure_name == missing_figure]$col_id
    
    # Identify figures to use as templates.
    template_figure_row_name <- head(plot_layout_table[row_id == current_row_id & is_present == TRUE]$figure_name, n = 1L)
    template_figure_col_name <- head(plot_layout_table[col_id == current_col_id & is_present == TRUE]$figure_name, n = 1L)
    
    # Add template.
    figure_list[[missing_figure]] <- .create_placeholder_figure(
      template_figure_row = figure_list[[template_figure_row_name]],
      template_figure_col = figure_list[[template_figure_col_name]],
      row_id = current_row_id,
      col_id = current_col_id
    )
  }
  
  # Check that no figures are missing now.
  plot_layout_table[, is_present := figure_name %in% names(figure_list)]
  if (!all(plot_layout_table$is_present)) {
    ..error_reached_unreachable_code("All panels of the figure should be present, but one or more are missing.")
  }
  
  # Shared elements between figure panels --------------------------------------
  
  # Determine top and bottom rows, and left and right columns.
  top_row_id <- min(plot_layout_table$row_id)
  bottom_row_id <- max(plot_layout_table$row_id)
  left_col_id <- min(plot_layout_table$col_id)
  right_col_id <- max(plot_layout_table$col_id)
  
  # Remove elements from figures.
  for (figure_name in names(figure_list)) {
    # Configure removal.
    figure_list[[figure_name]] <- .set_figure_element_removal(
      object = figure_list[[figure_name]],
      top_row_id = top_row_id,
      bottow_row_id = bottom_row_id,
      left_col_id = left_col_id,
      right_col_id = right_col_id,
      x_text_shared = x_text_shared,
      y_text_shared = y_text_shared,
      x_label_shared = x_label_shared,
      y_label_shared = y_label_shared
    )
    
    # Remove elements by replacing them with a zeroGrob. This maintains the
    # size of the figure's gtable.
    figure_list[[figure_name]] <- .remove_figure_elements(
      object = figure_list[[figure_name]],
      replace_by_zero_grob = TRUE
    )
  }
  
  # Compose figure panels ------------------------------------------------------
  
  # Form plot rows.
  g <- NULL
  spacer_width_x <- .get_plot_panel_spacing(ggtheme = ggtheme, axis = "x")
  spacer_width_y <- .get_plot_panel_spacing(ggtheme = ggtheme, axis = "y")
  unique_rows <- sort(unique(plot_layout_table$row_id))
  unique_cols <- sort(unique(plot_layout_table$col_id))
  for (current_row_id in unique_rows) {
    # Merge columns within each row.
    g_row <- NULL
    for (current_col_id in unique_cols) {
      if (is.null(g_row)) {
        # Use 
        g_row <- figure_list[[paste0(current_row_id, ".", current_col_id)]]@gtable
        
      } else {
        # Insert column for spacer.
        g_row <- gtable::gtable_add_cols(
          g_row,
          widths = spacer_width_x,
          pos = ncol(g_row)
        )
        
        # Add spacer.
        g_row <- .gtable_insert_spacer(
          g = g_row,
          position = c("t" = 1L, "b" = nrow(g_row), "l" = ncol(g_row), "r" = ncol(g_row)),
          width = spacer_width_x
        )
        
        # Combine gtable by columns.
        g_row <- cbind(g_row, figure_list[[paste0(current_row_id, ".", current_col_id)]]@gtable)
      }
    }
    
    # Merge with existing rows.
    if (is.null(g)) {
      g <- g_row
      
    } else {
      # Insert row for spacer.
      g_row <- gtable::gtable_add_rows(
        g_row,
        heights = spacer_width_y,
        pos = nrow(g_row)
      )
      
      # Add spacer element in the new row.
      g_row <- .gtable_insert_spacer(
        g = g_row,
        position = c("t" = nrow(g_row), "b" = nrow(g_row), "l" = 1L, "r" = ncol(g_row)),
        height = spacer_width_y
      )
      
      # Combine gtable by rows.
      g <- rbind(g, g_row)
    }
  }
  
  # Insert global elements -----------------------------------------------------
  
  # Insert global elements.
  global_element_names <- c(
    .all_gtable_title_names(),
    .all_gtable_guide_names()
  )
  if (x_text_shared %in% c("overall", "TRUE")) {
    global_element_names <- c(global_element_names, .all_gtable_axis_x_names())
  }
  if (x_label_shared %in% c("overall", "TRUE")) {
    global_element_names <- c(global_element_names, .all_gtable_label_x_names())
  }
  if (y_text_shared %in% c("overall", "TRUE")) {
    global_element_names <- c(global_element_names, .all_gtable_axis_y_names())
  }
  if (y_label_shared %in% c("overall", "TRUE")) {
    global_element_names <- c(global_element_names, .all_gtable_label_x_names())
  }
  
  # Isolate global elements that need to be updated.
  global_elements <- figure_list[[1L]]@global_elements
  global_element_name_list <- lapply(
    names(global_elements),
    function(x, y) {
      if (!startswith_any(x, y)) return(NULL)
      return(list(
        "name" = x,
        "type" =  y[sapply(y, function(y, x) {startsWith(x, y)}, x = x)]
      ))
    },
    y = global_element_names
  )
  global_element_name_list <- data.table::rbindlist(global_element_name_list)
  
  # For title-like objects, insert in place, at the top.
  if (any(global_element_name_list$type %in% .all_gtable_title_names("title"))) {
    x <- global_element_name_list[type %in% .all_gtable_title_names("title")]
    for (ii in seq_len(nrow(x))) {
      element_positions <- g$layout[g$layout$name == x$name[ii], c("t", "l", "b", "r"), drop = FALSE]
      position <- c("t" = 0L, "l" = 0L, "b" = 0L, "r" = 0L)
      position["t"] <- position["b"] <- min(element_positions$t)
      position["l"] <- min(element_positions$l)
      position["r"] <- max(element_positions$r)
      g <- .gtable_insert(
        g = g,
        g_new = global_elements[[x$name[ii]]],
        where = c("at", position),
        grob_name = x$name[ii]
      )
    }
  }
  
  # For caption-like objects, insert in place, at the bottom.
  if (any(global_element_name_list$type %in% .all_gtable_title_names("caption"))) {
    x <- global_element_name_list[type %in% .all_gtable_title_names("caption")]
    for (ii in seq_len(nrow(x))) {
      element_positions <- g$layout[g$layout$name == x$name[ii], c("t", "l", "b", "r"), drop = FALSE]
      position <- c("t" = 0L, "l" = 0L, "b" = 0L, "r" = 0L)
      position["t"] <- position["b"] <- max(element_positions$b)
      position["l"] <- min(element_positions$l)
      position["r"] <- max(element_positions$r)
      g <- .gtable_insert(
        g = g,
        g_new = global_elements[[x$name[ii]]],
        where = c("at", position),
        grob_name = x$name[ii]
      )
    }
  }

  # For guide-like objects, insert at the corresponding position.
  if (any(global_element_name_list$type %in% .all_gtable_guide_names())) {
    x <- global_element_name_list[type %in% .all_gtable_guide_names()]
    for (ii in seq_len(nrow(x))) {
      element_positions <- g$layout[g$layout$name == x$name[ii], c("t", "l", "b", "r"), drop = FALSE]
      position <- c("t" = 0L, "l" = 0L, "b" = 0L, "r" = 0L)
      
      # Positioning 
      if (x$type[ii] %in% .all_gtable_guide_names("top")) {
        position["t"] <- position["b"] <- min(element_positions$t)
        position["l"] <- min(element_positions$l)
        position["r"] <- max(element_positions$r)
        
        spacer_position <- position
        spacer_position[["t"]] <- spacer_position[["b"]] <- position[["b"]] + 1L
        g <- .gtable_insert_spacer(
          g = g,
          position = spacer_position,
          height = .get_plot_panel_spacing(ggtheme = ggtheme, axis = "y"),
          make_space = TRUE
        )

      } else if (x$type[ii] %in% .all_gtable_guide_names("bottom")) {
        position["t"] <- position["b"] <- max(element_positions$b)
        position["l"] <- min(element_positions$l)
        position["r"] <- max(element_positions$r)
        
        spacer_position <- position
        spacer_position[["t"]] <- spacer_position[["b"]] <- position[["t"]]
        g <- .gtable_insert_spacer(
          g = g,
          position = spacer_position,
          height = .get_plot_panel_spacing(ggtheme = ggtheme, axis = "y"),
          make_space = TRUE
        )
        
        # Inserting the spacer moves the guide object down.
        position[["t"]] <- position[["t"]] + 1L
        position[["b"]] <- position[["b"]] + 1L
        
      } else if (x$type[ii] %in% .all_gtable_guide_names("left")) {
        position["l"] <- position["r"] <- min(element_positions$l)
        position["t"] <- min(element_positions$t)
        position["b"] <- max(element_positions$b)
        
        spacer_position <- position
        spacer_position[["l"]] <- spacer_position[["r"]] <- position[["r"]] + 1L
        g <- .gtable_insert_spacer(
          g = g,
          position = spacer_position,
          width = .get_plot_panel_spacing(ggtheme = ggtheme, axis = "x"),
          make_space = TRUE
        )
        
      } else if (x$type[ii] %in% .all_gtable_guide_names("right")) {
        position["l"] <- position["r"] <- max(element_positions$r)
        position["t"] <- min(element_positions$t)
        position["b"] <- max(element_positions$b)
        
        spacer_position <- position
        spacer_position[["l"]] <- spacer_position[["r"]] <- position[["l"]]
        g <- .gtable_insert_spacer(
          g = g,
          position = spacer_position,
          width = .get_plot_panel_spacing(ggtheme = ggtheme, axis = "x"),
          make_space = TRUE
        )
        
        # Inserting the spacer moves the guide object right.
        position[["l"]] <- position[["l"]] + 1L
        position[["r"]] <- position[["r"]] + 1L
        
      } else {
        ..error_reached_unreachable_code(paste0("unknown type: ", x$type[1L]))
      }
      
      g <- .gtable_insert(
        g = g,
        g_new = global_elements[[x$name[ii]]],
        where = c("at", position),
        grob_name = x$name[ii]
      )
    }
  }
  
  # Labels
  if (any(global_element_name_list$type %in% .all_gtable_label_names())) {
    x <- global_element_name_list[type %in% .all_gtable_label_names()]
    for (ii in seq_len(nrow(x))) {
      element_positions <- g$layout[g$layout$name == x$name[ii], c("t", "l", "b", "r"), drop = FALSE]
      position <- c("t" = 0L, "l" = 0L, "b" = 0L, "r" = 0L)
      
      # Positioning 
      if (x$type[ii] %in% .all_gtable_label_names("top")) {
        position["t"] <- position["b"] <- min(element_positions$t)
        position["l"] <- min(element_positions$l)
        position["r"] <- max(element_positions$r)
        
      } else if (x$type[ii] %in% .all_gtable_label_names("bottom")) {
        position["t"] <- position["b"] <- max(element_positions$b)
        position["l"] <- min(element_positions$l)
        position["r"] <- max(element_positions$r)
        
      } else if (x$type[ii] %in% .all_gtable_label_names("left")) {
        position["l"] <- position["r"] <- min(element_positions$l)
        position["t"] <- min(element_positions$t)
        position["b"] <- max(element_positions$b)
        
      } else if (x$type[ii] %in% .all_gtable_label_names("right")) {
        position["l"] <- position["r"] <- max(element_positions$r)
        position["t"] <- min(element_positions$t)
        position["b"] <- max(element_positions$b)
        
      } else {
        ..error_reached_unreachable_code(paste0("unknown type: ", x$type[1L]))
      }
      
      g <- .gtable_insert(
        g = g,
        g_new = global_elements[[x$name[ii]]],
        where = c("at", position),
        grob_name = x$name[ii]
      )
    }
  }
  
  # Axis elements
  if (any(global_element_name_list$type %in% .all_gtable_axis_names())) {
    x <- global_element_name_list[type %in% .all_gtable_axis_names()]
    for (ii in seq_len(nrow(x))) {
      element_positions <- g$layout[g$layout$name == x$name[ii], c("t", "l", "b", "r"), drop = FALSE]
      position <- c("t" = 0L, "l" = 0L, "b" = 0L, "r" = 0L)
      
      # Positioning 
      if (x$type[ii] %in% .all_gtable_axis_names("top")) {
        position["t"] <- position["b"] <- min(element_positions$t)
        position["l"] <- min(element_positions$l)
        position["r"] <- max(element_positions$r)
        
      } else if (x$type[ii] %in% .all_gtable_axis_names("bottom")) {
        position["t"] <- position["b"] <- max(element_positions$b)
        position["l"] <- min(element_positions$l)
        position["r"] <- max(element_positions$r)
        
      } else if (x$type[ii] %in% .all_gtable_axis_names("left")) {
        position["l"] <- position["r"] <- min(element_positions$l)
        position["t"] <- min(element_positions$t)
        position["b"] <- max(element_positions$b)
        
      } else if (x$type[ii] %in% .all_gtable_axis_names("right")) {
        position["l"] <- position["r"] <- max(element_positions$r)
        position["t"] <- min(element_positions$t)
        position["b"] <- max(element_positions$b)
        
      } else {
        ..error_reached_unreachable_code(paste0("unknown type: ", x$type[1L]))
      }
      
      g <- .gtable_insert(
        g = g,
        g_new = global_elements[[x$name[ii]]],
        where = c("at", position),
        grob_name = x$name[ii]
      )
    }
  }
  
  # Clean-up -------------------------------------------------------------------
  
  # Drop all zeroGrob elements to prevent occlusion by empty objects.
  matched_elements <- !sapply(g$grobs, is, "zeroGrob")
  g$layout <- g$layout[matched_elements, , drop = FALSE]
  g$grobs <- g$grobs[matched_elements]
  g <- gtable::gtable_trim(g)
  
  # Update heights and widths.
  g <- .gtable_update_layout(g = g)

  return(g)
}



..get_plot_theme_linewidth <- function(ggtheme = NULL) {
  # Import default ggtheme in case none is provided.
  ggtheme <- .check_ggtheme(ggtheme)

  # Since ggplot 3.4.0, the width of a line is determined by linewidth instead
  # of size.
  linewidth <- ggtheme$line$linewidth

  return(linewidth)
}



..get_plot_element_spacing <- function(ggtheme = NULL, axis, theme_element) {
  # Obtain spacing from a ggtheme element

  # Import default ggtheme in case none is provided.
  ggtheme <- .check_ggtheme(ggtheme)
  
  # Basic spacing settings
  spacing <- ggtheme$spacing
  spacing_rel <- 1.0
  
  # Attempt to base the text size on the general axis.text attribute.
  if (!is.null(ggtheme[[theme_element]])) {
    if (inherits(ggtheme[[theme_element]], "rel")) {
      # Find the relative text size of axis text.
      spacing_rel <- as.numeric(ggtheme[[theme_element]])
      
    } else {
      # Set absolute text size.
      spacing <- ggtheme[[theme_element]]
      spacing_rel <- 1.0
    }
  }
  
  # Attempt to refine the text size using the axis.text.y attribute in
  # particular.
  if (!is.null(ggtheme[[paste0(theme_element, ".", axis)]])) {
    if (inherits(ggtheme[[paste0(theme_element, ".", axis)]], "rel")) {
      # Set relative text size of axis text
      spacing_rel <- as.numeric(ggtheme[[paste0(theme_element, ".", axis)]])
      
    } else {
      # Set absolute text size.
      spacing <- ggtheme[[paste0(theme_element, ".", axis)]]
      spacing_rel <- 1.0
    }
  }
  
  spacing <- spacing * spacing_rel

  # If no spacing is provided, produce 0.0 length spacing.
  if (!grid::is.unit(spacing)) spacing <- grid::unit(0.0, "pt")

  return(spacing)
}



.get_plot_panel_spacing <- function(ggtheme = NULL, axis) {
  # Obtain spacing between panels. This determines distance between facets.
  return(..get_plot_element_spacing(
    ggtheme = ggtheme,
    axis = axis,
    theme_element = "panel.spacing"
  ))
}



.get_plot_legend_spacing <- function(ggtheme = NULL, axis) {
  # Obtain spacing between legend and the main panel.
  return(..get_plot_element_spacing(
    ggtheme = ggtheme,
    axis = axis,
    theme_element = "legend.spacing"
  ))
}



.get_plot_geom_text_settings <- function(ggtheme = NULL) {
  # Import formatting settings from the provided ggtheme.

  # Import default ggtheme in case none is provided.
  ggtheme <- .check_ggtheme(ggtheme)

  # Find the text size for the table. This is based on text sizes in the
  # ggtheme.
  fontsize <- ggtheme$text$size
  fontsize_rel <- 1.0

  # Attempt to base the text size on the general axis.text attribute.
  if (!is.null(ggtheme$axis.text$size)) {
    if (inherits(ggtheme$axis.text$size, "rel")) {
      # Find the relative text size of axis text.
      fontsize_rel <- as.numeric(ggtheme$axis.text$size)
    } else {
      # Set absolute text size.
      fontsize <- ggtheme$axis.text$size
      fontsize_rel <- 1.0
    }
  }

  # Attempt to refine the text size using the axis.text.y attribute in
  # particular.
  if (!is.null(ggtheme$axis.text.y$size)) {
    if (inherits(ggtheme$axis.text.y$size, "rel")) {
      # Set relative text size of axis text
      fontsize_rel <- as.numeric(ggtheme$axis.text.y$size)
    } else {
      # Set absolute text size.
      fontsize <- as.numeric(ggtheme$axis.text.y$size)
      fontsize_rel <- 1.0
    }
  }

  # Update the text size using the magical ggplot2 point size (ggplot2:::.pt).
  geom_text_size <- fontsize * fontsize_rel / ggplot2::.pt

  # Obtain lineheight
  lineheight <- ggtheme$text$lineheight
  if (!is.null(ggtheme$axis.text$lineheight)) lineheight <- ggtheme$axis.text$lineheight
  if (!is.null(ggtheme$axis.text.y$lineheight)) lineheight <- ggtheme$axis.text.y$lineheight

  # Obtain family
  fontfamily <- ggtheme$text$family
  if (!is.null(ggtheme$axis.text$family)) fontfamily <- ggtheme$axis.text$family
  if (!is.null(ggtheme$axis.text.y$family)) fontfamily <- ggtheme$axis.text.y$family
  if (!is.null(ggtheme$axis.text.x$family)) fontfamily <- ggtheme$axis.text.x$family

  # Obtain face
  fontface <- ggtheme$text$face
  if (!is.null(ggtheme$axis.text$face)) fontface <- ggtheme$axis.text$face
  if (!is.null(ggtheme$axis.text.y$face)) fontface <- ggtheme$axis.text.y$face
  if (!is.null(ggtheme$axis.text.x$face)) fontface <- ggtheme$axis.text.x$face

  # Obtain colour
  colour <- ggtheme$text$colour
  if (!is.null(ggtheme$axis.text$colour)) colour <- ggtheme$axis.text$colour
  if (!is.null(ggtheme$axis.text.y$colour)) colour <- ggtheme$axis.text.y$colour
  if (!is.null(ggtheme$axis.text.x$colour)) colour <- ggtheme$axis.text.x$colour

  return(list(
    "geom_text_size" = geom_text_size,
    "fontsize" = fontsize,
    "fontsize_rel" = fontsize_rel,
    "colour" = colour,
    "family" = fontfamily,
    "face" = fontface,
    "lineheight" = lineheight
  ))
}











.rename_plot_grobs <- function(g, extension = "main", use_generic = TRUE) {
  if (is.null(g)) return(g)

  element_names <- c(
    .all_gtable_panel_names(),
    .all_gtable_label_x_names(),
    .all_gtable_label_y_names(),
    .all_gtable_axis_x_names(),
    .all_gtable_axis_y_names()
  )
  
  # Filter only elements that are present, and generate list of matching
  # local and global elements.
  renamable_elements <- g$layout$name
  element_list <- lapply(
    renamable_elements,
    function(x, y) {
      if (!startswith_any(x, prefix = y)) return(NULL)
      
      y <- y[sapply(y, function(y, x) {startsWith(x, y)}, x = x)]
      return(list("local" = x, "global" = y))
    },
    y = element_names
  )
  element_list <- data.table::rbindlist(element_list)
  
  # If generic names are not used, replace global (generic) names by the local
  # names.
  if (!use_generic) {
    element_list$global <- element_list$local
  }

  # Rename elements and add extension.
  for (ii in seq_len(nrow(element_list))) {
    g <- .gtable_rename_element(
      g = g,
      old = element_list$local[ii],
      new = paste0(element_list$global[ii], "-", extension),
      partial_match = FALSE,
      allow_missing = FALSE
    )
  }
  
  return(g)
}



.convert_to_grob <- function(plots_or_grobs) {
  # Convert to list if the input is a single grob or
  unlist_grobs <- FALSE
  if (grid::is.grob(plots_or_grobs) || ggplot2::is_ggplot(plots_or_grobs)) {
    plots_or_grobs <- list(plots_or_grobs)

    # Set a flag so that we unlist the results after conversion.
    unlist_grobs <- TRUE
  }

  # Initialise list of grobs
  grobs <- list()

  for (p in plots_or_grobs) {
    if (inherits(p, "familiar_ggplot")) {
      # Convert to grob
      g <- suppressWarnings(tryCatch(
        ggplot2::ggplotGrob(p),
        error = identity
      ))
      
      if (inherits(g, "error")) g <- NULL

      # Make changes to g according to p$custom_grob.
      if (!is.null(p$custom_grob)) {
        if (!is.null(p$custom_grob$heights)) {
          # Iterate over the elements that need to be updated.
          for (ii in seq_len(length(p$custom_grob$heights$name))) {
            # Extract name and height
            name <- p$custom_grob$heights$name[ii]
            height <- p$custom_grob$heights$height[ii]

            # Find the intended grob.
            grob_index <- which(g$layout$name == name)

            # Update the height in the layout table.
            g$heights[g$layout[grob_index, "t"]] <- height

            # Update (or set) the height of the grob.
            g$grobs[[grob_index]]$heights <- height
          }
        }

        if (!is.null(p$custom_grob$widths)) {
          # Iterate over the elements that need to be updated.
          for (ii in seq_len(length(p$custom_grob$widths$name))) {
            name <- p$custom_grob$widths$name[ii]
            width <- p$custom_grob$widths$width[ii]

            # Find the intended grob.
            grob_index <- which(g$layout$name == name)

            # Update the height in the layout table.
            g$widths[g$layout[grob_index, "l"]] <- width

            # Update (or set) the height of the grob.
            g$grobs[[grob_index]]$widths <- width
          }
        }
      }
      
    } else if (inherits(p, "ggplot")) {
      # Convert to grob
      g <- suppressWarnings(tryCatch(
        ggplot2::ggplotGrob(p),
        error = identity
      ))
      
      if (inherits(g, "error")) g <- NULL
      
    } else if (inherits(p, "grob")) {
      # Assign grob
      g <- p
      
    } else {
      ..warning(paste0(
        "Could not convert an object of class ", class(p), " to a grob."
      ))
      g <- NULL
    }
    
    # Set panel sizes.
    if (!is.null(g)) {
      # Make panels inherit heights and widths, if they don't have any. This is done
      # to ensure that panels retain heights and widths, even if supporting elements
      # such as the axis text and label elements are stripped on figure composition.
      g <- .gtable_update_panel_aspects(g = g)
    }
    
    grobs <- c(grobs, list(g))
  }

  if (unlist_grobs) grobs <- grobs[[1L]]
  
  return(grobs)
}



.draw_plot <- function(plot_or_grob) {
  suppress_warnings(
    ..draw_plot(plot_or_grob),
    regexp = c("containing missing values", "containing non-finite values")
  )
}



..draw_plot <- function(plot_or_grob) {
  if (ggplot2::is_ggplot(plot_or_grob)) {
    show(plot_or_grob)
    
  } else if (grid::is.grob(plot_or_grob)) {
    grid::grid.newpage()
    grid::grid.draw(plot_or_grob)
    
  } else {
    ..error("Plot could not be drawn.")
  }
  
  return(invisible(NULL))
}



.save_plot_to_file <- function(
    plot_or_grob,
    object,
    dir_path,
    type,
    x,
    subtype = NULL,
    split_by = NULL,
    additional = NULL,
    filename = NULL,
    device = "png",
    ...
) {
  # ... are passed to ggplot2::ggsave

  # Check if the plot object exists
  if (is.null(plot_or_grob)) return(NULL)

  # Check if directory exists
  if (is.encapsulated_path(dir_path)) {
    file_dir <- normalizePath(
      file.path(dir_path, type),
      mustWork = FALSE
    )
    
  } else {
    file_dir <- normalizePath(dir_path, mustWork = FALSE)
  }

  if (!dir.exists(file_dir)) {
    dir.create(file_dir, recursive = TRUE)
  }

  if (!is.null(filename)) {
    # These are the file extensions supported by ggsave (v3.4.0).
    file_extensions <- c(
      "eps", "ps", "tex", "pdf", "svg", "emf", "wmf",
      "png", "jpg", "jpeg", "bmp", "tiff"
    )

    # Test if a file extension is present.
    device_present <- endswith_any(
      filename, 
      suffix = paste0(".", file_extensions)
    )
    
    if (any(device_present)) {
      # Update device indicated by the filename.
      device <- head(file_extensions[device_present], n = 1L)

      # Remove device from filename.
      filename <- sub_last(
        pattern = paste0(".", device),
        replacement = "",
        x = filename
      )
    }

    # Extend the filename if multiple plots are created from the same data.
    if (!is.null(split_by)) {
      subtype <- paste0(
        as.character(sapply(split_by, function(jj, x) (x[[jj]][1L]), x = x)),
        collapse = "_"
      )

      filename <- paste0(filename, subtype, collapse = "_")
    }
    
  } else {
    # Set subtype.
    subtype <- .create_plot_subtype(
      x = x,
      subtype = subtype,
      split_by = split_by,
      additional = additional
    )

    # Combine type and subtype as the filename.
    filename <- paste0(
      type,
      ifelse(is.null(subtype), "", paste0("_", subtype))
    )
  }

  for (current_device in device) {
    # Add in extension again.
    filename <- paste0(filename, ".", current_device)

    # There may be an issue with a cold RStudio where the plotting devices have
    # not started.
    tryCatch(
      
      # Call ggsave to save the data
      suppressMessages(
        do.call(
          ggplot2::ggsave,
          args = c(
            list(
              "filename" = filename,
              "plot" = plot_or_grob,
              "device" = current_device,
              "path" = file_dir
            ),
            list(...)
          )
        )
      ),
      error = function(err) {
        logger_warning(
          paste0(
            "Could not create plot ",
            filename,
            ". The OS may not allow long file names."
          )
        )
      }
    )
  }
  
  return(invisible(NULL))
}



.get_plot_results <- function(
    dir_path = NULL,
    plot_list = NULL,
    export_collection = FALSE,
    object = NULL
) {
  
  # Do not return plot information.
  if (!is.null(dir_path)) plot_list <- NULL

  if (export_collection) {
    return(list(
      "collection" = object,
      "plot_list" = plot_list
    ))
    
  } else {
    return(plot_list)
  }
}



.format_plot_number <- function(
    x,
    digits = 3L,
    common_base = NULL,
    min_common_base = NULL,
    max_common_base = NULL,
    character_out = TRUE
) {
  # Determine the common base.
  if (is.null(common_base)) {
    common_base <- ..format_get_common_base(x)
    if (is.numeric(max_common_base)) {
      common_base <- ifelse(common_base > max_common_base, max_common_base, common_base)
    }
    if (is.numeric(min_common_base)) {
      common_base <- ifelse(common_base < min_common_base, min_common_base, common_base)
    }
  }

  # Round numbers.
  x <- as.integer(round(x / 10.0^(1.0 + common_base - digits))) * 10.0^(1.0 + common_base - digits)

  # Format output.
  if (character_out) {
    return(format(x, digits = digits, trim = TRUE))
    
  } else {
    return(x)
  }
}



..format_get_common_base <- function(x) {
  # Find the base-10 integer of the data.
  x_base <- floor(log10(abs(x)))
  x_base <- x_base[is.finite(x_base)]
  
  return(as.integer(max(x_base)))
}




.format_plot_number_nice_range <- function(input_range, x) {
  # Shrink input range to first and last value
  input_range <- c(
    head(input_range, n = 1L),
    tail(input_range, n = 1L)
  )

  # Find values in input_range that should be replaced.
  replace_index <- is.na(input_range)

  # Return input range if no updating is required.
  if (!any(replace_index)) return(input_range)

  # Find range of values in x.
  value_range <- range(x)

  # Replace missing elements of the input range.
  input_range[replace_index] <- value_range[replace_index]

  # Make the input range nice
  nice_range <- range(labeling::extended(
    dmin = input_range[1L],
    dmax = input_range[2L],
    m = 5L,
    only.loose = TRUE
  ))

  # Update the input_range with nice values
  input_range[replace_index] <- nice_range[replace_index]

  return(input_range)
}



.convert_dendrogram_to_table <- function(h, similarity_metric) {
  # Suppress NOTES due to non-standard evaluation in data.table
  x_1 <- y_1 <- x_2 <- NULL

  # Convert to dendrogram
  h <- stats::as.dendrogram(h)

  # Determine the metric range.
  metric_range <- get_similarity_range(
    similarity_metric = similarity_metric, 
    as_distance = TRUE
  )

  # Convert dendogram to a list of connectors that can later be used for
  # plotting. Note that we do not know where the origin should be located on the
  # x-axis. We will correct for that later.
  connectors <- .decompose_dendrogram(
    h = h,
    parent_height = max(metric_range)
  )

  # Combine into single data.table.
  connectors <- data.table::rbindlist(connectors)

  # Keep only nodes with finite parent height (y_1). Depending on the metric,
  # this means that the connector with the origin may not be drawn.
  connectors <- connectors[is.finite(y_1)]

  # Return null if there are no connectors to be drawn.
  if (is_empty(connectors)) return(NULL)

  # Reposition the left-most leaf to 0.0.
  min_leaf_pos <- min(c(connectors$x_1, connectors$x_2))
  connectors[, ":="(
    "x_1" = x_1 - min_leaf_pos,
    "x_2" = x_2 - min_leaf_pos
  )]
  
  return(connectors)
}



.decompose_dendrogram <- function(
    h, 
    parent_height = Inf, 
    parent_x = NA, 
    leafs_visited = 0L
) {
  # Decompose dendogram. The function is designed to iterate through a
  # dendogram, and obtain the connector between node (h) and its parent, as well
  # as the connectors between the node and its children h[[1]] and h[[2]],
  # unless it has no children.

  dend_attr <- attributes(h)

  if (is.na(parent_x)) {
    parent_x <- ifelse(is.null(dend_attr$midpoint), 0.0, dend_attr$midpoint)
  }

  if (is.null(dend_attr$midpoint)) {
    # This indicates that the node has no children.

    # Connector from parent.
    conn_parent_child <- data.table::data.table(
      "x_1" = parent_x,
      "y_1" = parent_height,
      "x_2" = parent_x,
      "y_2" = dend_attr$height,
      "feature" = dend_attr$label
    )

    return(list(conn_parent_child))
  }

  # Connector from parent.
  conn_parent_child <- data.table::data.table(
    "x_1" = parent_x,
    "y_1" = parent_height,
    "x_2" = parent_x,
    "y_2" = dend_attr$height,
    "feature" = NA_character_
  )
  
  # Left child node x-axis location
  if (!is.null(attributes(h[[1L]])$midpoint)) {
    child_l_pos <- leafs_visited + attributes(h[[1L]])$midpoint
  } else {
    child_l_pos <- leafs_visited
  }

  # Connector to left leaf.
  conn_child_l_leaf <- data.table::data.table(
    "x_1" = parent_x,
    "y_1" = dend_attr$height,
    "x_2" = child_l_pos,
    "y_2" = dend_attr$height,
    "feature" = NA_character_
  )

  # Right child node x-axis location
  if (
    !is.null(attributes(h[[2L]])$midpoint) &&
    !is.null(attributes(h[[1L]])$members)
  ) {
    child_r_pos <- leafs_visited + attributes(h[[1L]])$members + attributes(h[[2L]])$midpoint
  } else if (!is.null(attributes(h[[1L]])$members)) {
    child_r_pos <- leafs_visited + attributes(h[[1L]])$members
  } else {
    child_r_pos <- leafs_visited
  }

  # Connector to right leaf.
  conn_child_r_leaf <- data.table::data.table(
    "x_1" = parent_x,
    "y_1" = dend_attr$height,
    "x_2" = child_r_pos,
    "y_2" = dend_attr$height,
    "feature" = NA_character_
  )

  # Add data.tables as list elements.
  connector_list <- list(
    conn_parent_child,
    conn_child_l_leaf, 
    conn_child_r_leaf
  )

  # Left leaf
  if (!is.null(h[[1L]])) {
    left_leaf_connectors <- .decompose_dendrogram(
      h = h[[1L]],
      parent_height = dend_attr$height,
      parent_x = child_l_pos,
      leafs_visited = leafs_visited
    )

    # Append to list
    connector_list <- append(connector_list, left_leaf_connectors)
  }

  # Right leaf
  if (!is.null(h[[2L]])) {
    right_leaf_connectors <- .decompose_dendrogram(
      h = h[[2L]],
      parent_height = dend_attr$height,
      parent_x = child_r_pos,
      leafs_visited = ifelse(
        !is.null(attributes(h[[1L]])$members),
        leafs_visited + attributes(h[[1L]])$members, 
        leafs_visited
      )
    )

    # Append to list
    connector_list <- append(connector_list, right_leaf_connectors)
  }

  return(connector_list)
}



..set_edge_points <- function(x, range, type) {
  # Function used to determine edge points, such as used for ggplot2::geom_rect.
  if (!is.numeric(x)) {
    x <- as.numeric(x)
    range <- c(0.5, length(x) + 0.5)
  }

  if (length(x) > 1L) {
    # Make sure x is sorted ascendingly.
    sort_index <- sort(x, index.return = TRUE)$ix
    x <- x[sort_index]

    # Compute difference between subsequent values.
    diff_x <- diff(x)

    # Compute edges.
    xmax <- c(
      head(x, n = length(x) - 1L) + diff_x / 2.0,
      tail(x, n = 1L) + tail(diff_x, n = 1L) / 2.0
    )
    xmin <- c(
      head(x, n = 1L) - head(diff_x, n = 1L) / 2.0,
      tail(x, n = length(x) - 1L) - diff_x / 2.0
    )

    # Shuffle back to input order.
    xmax[sort_index] <- xmax
    xmin[sort_index] <- xmin
    
  } else {
    xmin <- range[1L]
    xmax <- range[2L]
  }

  edge_points <- list(xmin, xmax)
  if (type == "x") {
    names(edge_points) <- c("xmin", "xmax")
  } else if (type == "y") {
    names(edge_points) <- c("ymin", "ymax")
  } else {
    ..error_reached_unreachable_code(paste0(
      "..set_edge_points: unknown type specified: ", type
    ))
  }

  return(edge_points)
}



..get_luminosity <- function(col) {
  values <- as.vector(grDevices::col2rgb(col)) / 255.0
  return ((min(values) + max(values)) / 2.0)
}
