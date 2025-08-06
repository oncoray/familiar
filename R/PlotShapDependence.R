#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL



# plot_shap_dependence (generic) -----------------------------------------------

#' @title Create SHAP dependence plot.
#'
#' @description This method creates a SHAP dependence plot that shows the
#'   dependence of the SHAP value of a feature against its value.
#'
#' @param dir_path (*optional*) Path to the directory where created SHAP
#'   dependence plots are saved to. Output is saved in the `explanation`
#'   subdirectory. If `NULL` no figures are saved, but are returned instead.
#' @param gradient_palette (*optional*) Divergent palette used to colour the
#'   elements of dependence plots for interactions with another feature.
#'   `familiar` has a default palette. Other palettes are supported by the
#'   `paletteer` package, `grDevices::palette.pals()` (requires R >= 4.0.0),
#'   `grDevices::hcl.pals()` (requires R >= 3.6.0) and `rainbow`, `heat.colors`,
#'   `terrain.colors`, `topo.colors` and `cm.colors`, which correspond to the
#'   palettes of the same name in `grDevices`. You may also specify your own
#'   palette by providing a vector of colour names listed by
#'   `grDevices::colors()` or through hexadecimal RGB strings.
#'
#'   If no `interaction_feature` is set, the gradient palette is not used.
#'
#' @param shap_feature (*optional*)
#' @param interaction_feature (*optional*)
#' @inheritParams as_familiar_collection
#' @inheritParams plot_univariate_importance
#' @inheritParams .check_input_plot_args
#' @inheritParams .check_plot_splitting_variables
#' @inheritDotParams extract_performance -object
#' @inheritDotParams as_familiar_collection -object
#' @inheritDotParams ggplot2::ggsave -height -width -units -path -filename -plot
#'
#' @details This function creates SHAP dependence plots, which show how the
#'   marginal contributions of a feature to the predicted value depend on its
#'   value.
#'
#'   Available splitting variables are: `vimp_method`, `learner`,
#'   `evaluation_time` (survival outcome only) and `positive_class` (categorical
#'   outcomes). The default is to facet by `evaluation_time` or
#'   `positive_class`, and split by `vimp_method` and `learner`. `color_by` is
#'   not used.
#'
#'   Labelling methods such as `set_vimp_method_names` or `set_learner_names`
#'   can be applied to the `familiarCollection` object to update labels, and
#'   order the output in the figure.
#'
#' @return `NULL` or list of plot objects, if `dir_path` is `NULL`.
#'
#' @exportMethod plot_shap_dependence
#' @md
#' @rdname plot_shap_dependence-methods
setGeneric(
  "plot_shap_dependence",
  function(
    object,
    draw = FALSE,
    dir_path = NULL,
    split_by = NULL,
    facet_by = NULL,
    facet_wrap_cols = NULL,
    ggtheme = NULL,
    gradient_palette = NULL,
    x_label = waiver(),
    y_label = waiver(),
    legend_label = waiver(),
    plot_title = waiver(),
    plot_sub_title = waiver(),
    caption = NULL,
    x_range = NULL,
    x_n_breaks = 5L,
    x_breaks = NULL,
    y_range = NULL,
    y_n_breaks = 5L,
    y_breaks = NULL,
    shap_feature = NULL,
    interaction_feature = NULL,
    width = waiver(),
    height = waiver(),
    units = waiver(),
    export_collection = FALSE,
    ...
  ) {
    standardGeneric("plot_shap_dependence")
  }
)



# plot_shap_dependence (general) -----------------------------------------------

#' @rdname plot_shap_dependence-methods
setMethod(
  "plot_shap_dependence",
  signature(object = "ANY"),
  function(
    object,
    draw = FALSE,
    dir_path = NULL,
    split_by = NULL,
    facet_by = NULL,
    facet_wrap_cols = NULL,
    ggtheme = NULL,
    gradient_palette = NULL,
    x_label = waiver(),
    y_label = waiver(),
    legend_label = waiver(),
    plot_title = waiver(),
    plot_sub_title = waiver(),
    caption = NULL,
    x_range = NULL,
    x_n_breaks = 5L,
    x_breaks = NULL,
    y_range = NULL,
    y_n_breaks = 5L,
    y_breaks = NULL,
    shap_feature = NULL,
    interaction_feature = NULL,
    width = waiver(),
    height = waiver(),
    units = waiver(),
    export_collection = FALSE,
    ...
  ) {
    # Attempt conversion to familiarCollection object.
    object <- do.call(
      as_familiar_collection,
      args = c(
        list(
          "object" = object,
          "data_element" = "shap"
        ),
        list(...)
      )
    )
    
    return(do.call(
      plot_shap_dependence,
      args = list(
        "object" = object,
        "draw" = draw,
        "dir_path" = dir_path,
        "split_by" = split_by,
        "facet_by" = facet_by,
        "facet_wrap_cols" = facet_wrap_cols,
        "ggtheme" = ggtheme,
        "gradient_palette" = gradient_palette,
        "x_label" = x_label,
        "y_label" = y_label,
        "legend_label" = legend_label,
        "plot_title" = plot_title,
        "plot_sub_title" = plot_sub_title,
        "caption" = caption,
        "x_range" = x_range,
        "x_n_breaks" = x_n_breaks,
        "x_breaks" = x_breaks,
        "y_range" = y_range,
        "y_n_breaks" = y_n_breaks,
        "y_breaks" = y_breaks,
        "shap_feature" = shap_feature,
        "interaction_feature" = interaction_feature,
        "width" = width,
        "height" = height,
        "units" = units,
        "export_collection" = export_collection
      )
    ))
  }
)



# plot_shap_dependence (collection) --------------------------------------------

#' @rdname plot_shap_dependence-methods
setMethod(
  "plot_shap_dependence",
  signature(object = "familiarCollection"),
  function(
    object,
    draw = FALSE,
    dir_path = NULL,
    split_by = NULL,
    facet_by = NULL,
    facet_wrap_cols = NULL,
    ggtheme = NULL,
    gradient_palette = NULL,
    x_label = waiver(),
    y_label = waiver(),
    legend_label = waiver(),
    plot_title = waiver(),
    plot_sub_title = waiver(),
    caption = NULL,
    x_range = NULL,
    x_n_breaks = 5L,
    x_breaks = NULL,
    y_range = NULL,
    y_n_breaks = 5L,
    y_breaks = NULL,
    shap_feature = NULL,
    interaction_feature = NULL,
    width = waiver(),
    height = waiver(),
    units = waiver(),
    export_collection = FALSE,
    ...
  ) {
    # Make sure the collection object is updated.
    object <- update_object(object = object)
    
    # Check input arguments ----------------------------------------------------
    
    # ggtheme
    ggtheme <- .check_ggtheme(ggtheme)
    
    # Load the data.
    x <- export_shap(object = object)
    x <- x$shap_summary
    if (is_empty(x)) return(NULL)
    
    # Obtain single data element from list.
    if (is.list(x)) {
      if (length(x) > 1L) {
        ..error_reached_unreachable_code(
          "plot_shap_dependence: list of data elements contains unmerged elements."
        )
      }
      x <- x[[1L]]
    }
    
    # Check that the data are not evaluated at the model level.
    if (x@detail_level == "model") {
      ..warning_no_comparison_between_models()
      return(NULL)
    }
    
    # Check that the data are not empty.
    if (is_empty(x)) return(NULL)
    
    # Ensure that we work with a copy of the data.
    x@data <- data.table::copy(x@data)
    
    # Check package requirements for plotting.
    if (!require_package(
      x = ..required_plotting_packages(extended = FALSE),
      purpose = "to create a SHAP dependence plot",
      message_type = "warning"
    )) {
      return(NULL)
    }

    # Add evaluation time or positive class as a splitting variable.
    additional_variable <- NULL
    if (object@outcome_type %in% c("survival")) {
      additional_variable <- "evaluation_time"
      data.table::setnames(x@data, old = "shap_outcome", new = "evaluation_time")
      
    } else if (object@outcome_type %in% c("multinomial")) {
      additional_variable <- "positive_class"
      data.table::setnames(x@data, old = "shap_outcome", new = "positive_class")
    }
    
    # Add default splitting variables.
    if (
      is.null(split_by) &&
      is.null(facet_by)
    ) {
      # Split by vimp_method, learner.
      split_by <- c("vimp_method", "learner", "feature_name")
      facet_by <- additional_variable
    }
    all_variables <- c("vimp_method", "learner", additional_variable, "feature_name")
    
    # Check splitting variables and generate sanitised output
    split_var_list <- .check_plot_splitting_variables(
      x = x@data,
      split_by = split_by,
      facet_by = facet_by,
      available = all_variables
    )
    
    # Update splitting variables
    split_by <- split_var_list$split_by
    facet_by <- split_var_list$facet_by
    
    # y_label
    if (is.waive(y_label)) {
      y_label <- "shap value"
    }
    
    .check_input_plot_args(
      facet_wrap_cols = facet_wrap_cols,
      y_label = y_label,
      plot_title = plot_title,
      plot_sub_title = plot_sub_title,
      caption = caption
    )
    
    # Determine which features to use.
    selected_shap_features <- levels(x@data$feature_name)
    if (!is.null(shap_feature)) {
      feature_in_data <- shap_feature %in% selected_shap_features
      if (!all(feature_in_data)) {
        ..warning(paste0(
          "Not all features in shap_feature could be found in the data. Missing: ",
          paste_s(shap_feature[!feature_in_data])
        ))
      }
      selected_shap_features <- intersect(selected_shap_features, shap_feature)
    }
    if (length(selected_shap_features) == 0L) return(NULL)
    
    # Determine which interaction features exist.
    selected_interaction_features <- NULL
    if (!is.null(interaction_feature)) {
      feature_in_data <- interaction_feature %in% levels(x@data$feature_name)
      if (!all(feature_in_data)) {
        ..warning(paste0(
          "Not all features in interaction_feature could be found in the data. Missing: ",
          paste_s(interaction_feature[!feature_in_data])
        ))
      }
      selected_interaction_features <- intersect(interaction_feature, levels(x@data$feature_name))
      if (length(selected_interaction_features) == 0L) interaction_features <- NULL
    }
    
    # Create plots -------------------------------------------------------------
    
    # Determine if subtitle should be generated.
    autogenerate_plot_subtitle <- is.waive(plot_sub_title)
    
    # Split data.
    if (!is.null(split_by)) {
      x_split <- split(
        x@data, 
        by = split_by, 
        drop = FALSE
      )
      
    } else {
      x_split <- list("null.name" = x@data)
    }
    
    # Select plot data based on split.
    plot_data <- list()
    plot_data_index <- 1L
    for (ii in seq_along(x_split)) {
      main_data <- x_split[[ii]]
      if (is_empty(main_data)) next
      
      # Skip if current feature is not in selected_shap_features.
      current_shap_feature <- main_data$feature_name[1L]
      if (!current_shap_feature %in% selected_shap_features) next
      
      interaction_data <- NULL
      if (length(selected_interaction_features) > 0L) {
        for (current_interaction_feature in selected_interaction_features) {
          interaction_selection_data <- unique(main_data[, mget(split_by)])
          interaction_selection_data[, "feature_name" := current_interaction_feature]
          interaction_data <- x@data[interaction_selection_data, on = .NATURAL]
          
          plot_data[[plot_data_index]] <- list(
            "main_data" = main_data,
            "interaction_data" = interaction_data,
            "feature" = current_shap_feature,
            "interaction_feature" = current_interaction_feature
          )
          plot_data_index <- plot_data_index + 1L
        }
        
      } else {
        plot_data[[plot_data_index]] <- list(
          "main_data" = main_data,
          "interaction_data" = NULL,
          "feature" = current_shap_feature,
          "interaction_feature" = NULL
        )
        plot_data_index <- plot_data_index + 1L
      }
    }
    
    # Store plots to list in case dir_path is absent.
    if (is.null(dir_path)) plot_list <- list()
    
    # Iterate over data splits.
    for (current_data in plot_data) {
      if (is.waive(plot_title)) plot_title <- "SHAP dependence"
      
      # Declare subtitle components.
      additional_subtitle <- NULL
      
      # Add evaluation time as subtitle component if it is not used
      # otherwise.
      if (
        !"evaluation_time" %in% c(split_by, facet_by) &&
        object@outcome_type %in% c("survival")
      ) {
        additional_subtitle <- c(
          additional_subtitle,
          .add_time_to_plot_subtitle(current_data$main_data$evaluation_time[1L])
        )
      }
      
      # Add feature as subtitle component if it is not used otherwise.
      if (!"feature_name" %in% c(split_by)) {
        additional_subtitle <- c(
          additional_subtitle,
          list("feature", current_data$feature)
        )
      }
      
      # Add interaction feature as subtitle component.
      if (!is.null(current_data$interaction_feature)) {
        additional_subtitle <- c(
          additional_subtitle,
          list("interaction", current_data$interaction_feature)
        )
      }
      
      
      if (autogenerate_plot_subtitle) {
        plot_sub_title <- .create_plot_subtitle(
          split_by = split_by,
          additional = additional_subtitle,
          x = plot_data$main_data
        )
      }
      
      # Generate plot
      p <- .plot_shap_dependence_plot(
        x = current_data$main_data,
        facet_by = facet_by,
        facet_wrap_cols = facet_wrap_cols,
        ggtheme = ggtheme,
        gradient_palette = gradient_palette,
        x_label = x_label,
        y_label = y_label,
        legend_label = legend_label,
        plot_title = plot_title,
        plot_sub_title = plot_sub_title,
        caption = caption,
        x_range = x_range,
        x_n_breaks = x_n_breaks,
        x_breaks = x_breaks,
        y_range = y_range,
        y_n_breaks = y_n_breaks,
        y_breaks = y_breaks,
        outcome_type = object@outcome_type
      )
      
      # Check empty output
      if (is.null(p)) next
      
      # Draw figure.
      if (draw) .draw_plot(plot_or_grob = p)
      
      # Save and export
      if (!is.null(dir_path)) {
        # Obtain decent default values for the plot.
        def_plot_dims <- .determine_shap_dependence_plot_dimensions(
          x = current_data$main_data,
          facet_by = facet_by,
          facet_wrap_cols = facet_wrap_cols
        )
        
        # Set subtype
        subtype <- "shap_dependence"
        if (!is.null(current_data$interaction_feature)
        
        # Save to file.
        do.call(
          .save_plot_to_file,
          args = c(
            list(
              "plot_or_grob" = p,
              "object" = object,
              "dir_path" = dir_path,
              "type" = "explanation",
              "subtype" = paste0("shap_dependence"),
              "x" = current_data$main_data,
              "split_by" = split_by,
              "height" = ifelse(is.waive(height), def_plot_dims[1L], height),
              "width" = ifelse(is.waive(width), def_plot_dims[2L], width),
              "units" = ifelse(is.waive(units), "cm", units)
            ),
            list(...)
          )
        )
        
      } else {
        # Store as list for export.
        plot_list <- c(plot_list, list(p))
      }
    }
    
    # Generate output
    return(.get_plot_results(
      dir_path = dir_path,
      plot_list = plot_list,
      export_collection = export_collection,
      object = object
    ))
  }
)



.plot_shap_dependence_plot <- function(
    x,
    color_by,
    facet_by,
    facet_wrap_cols,
    plot_type,
    value_representation,
    ggtheme,
    discrete_palette,
    gradient_palette,
    x_label,
    y_label,
    legend_label,
    plot_title,
    plot_sub_title,
    caption,
    x_range,
    x_n_breaks,
    x_breaks,
    outcome_type
) {
  # Suppress NOTES due to non-standard evaluation in data.table
  shap_value <- vimp <- feature_value <- NULL
  
  # x_label
  if (is.waive(x_label)) {
    browser()
  }
  
  value_group_columns <- c("vimp_method", "learner", "feature_name")
  if ("evaluation_time" %in% colnames(x)) value_group_columns <- c(value_group_columns, "evaluation_time")
  if ("positive_class" %in% colnames(x)) value_group_columns <- c(value_group_columns, "positive_class")
  
  # Apply representation.
  if (value_representation == "abs") {
    x[, "shap_value" := abs(shap_value)]
    
  } else if (value_representation == "abs_mean") {
    x <- x[, list("shap_value" = mean(abs(shap_value))), by = value_group_columns]
    
  } else if (value_representation == "abs_max") {
    x <- x[, list("shap_value" = max(abs(shap_value))), by = value_group_columns]
    
  } else if (value_representation == "abs_min") {
    x <- x[, list("shap_value" = min(abs(shap_value))), by = value_group_columns]
  }
  
  # Map feature values to (-1, 1) range for raw representation.
  if (value_representation == "raw") {
    x[
      ,
      "feature_value" := 2.0 * (feature_value - min(feature_value)) / (max(feature_value) - min(feature_value))- 1.0,
      by = "feature_name"
    ]
  }
  
  # Sort features by importance (mean absolute SHAP).
  feature_importance <- x[, list("vimp" = mean(abs(shap_value))), by = value_group_columns]
  feature_importance <- feature_importance[, list("vimp" = max(vimp)), by = "feature_name"][order(vimp)]
  x$feature_name <- factor(
    x = x$feature_name,
    levels = feature_importance$feature_name
  )
  
  # Check x-range.
  if (is.null(x_range)) {
    if (value_representation == "raw") {
      x_range <- c(min(x$shap_value), max(x$shap_value))
      
    } else {
      x_range <- c(0.0, max(x$shap_value))
    }
    
  } else {
    .check_input_plot_args(x_range = x_range)
  }
  
  # x_breaks
  if (is.null(x_breaks)) {
    .check_input_plot_args(
      x_range = x_range,
      x_n_breaks = x_n_breaks
    )
    
    # Create breaks and update x_range
    x_breaks <- labeling::extended(
      m = x_n_breaks,
      dmin = x_range[1L],
      dmax = x_range[2L],
      only.loose = TRUE
    )
    
    x_range <- c(
      head(x_breaks, n = 1L),
      tail(x_breaks, n = 1L)
    )
    
  } else {
    .check_input_plot_args(x_breaks = x_breaks)
  }
  
  # Create a legend label.
  legend_label <- .create_plot_legend_title(
    user_label = legend_label,
    color_by = if(value_representation == "raw") "feature_value" else color_by
  )
  
  # Check remaining input arguments.
  .check_input_plot_args(
    legend_label = legend_label
  )
  
  # Generate a guide table
  guide_list <- .create_plot_guide_table(
    x = x,
    color_by = color_by,
    discrete_palette = discrete_palette
  )
  
  # Extract data
  x <- guide_list$data
  
  # Extract guide_table for color.
  g_color <- guide_list$guide_color
  
  # Create basic plot
  p <- ggplot2::ggplot(
    data = x,
    mapping = ggplot2::aes(
      x = !!sym("shap_value"),
      y = !!sym("feature_name")
    ))
  p <- p + ggtheme
  
  # Set breaks and range.
  p <- p + ggplot2::scale_x_continuous(breaks = x_breaks)
  p <- p + ggplot2::coord_cartesian(xlim = x_range)
  
  if (plot_type == "swarmplot") {
    # Swarm plot ---------------------------------------------------------------
    
    # Determine the density of points for each feature as function of the
    # shap-value.
    grouping_variables <- c("feature_name", facet_by)
    if (!is.null(color_by)) {
      grouping_variables <- c(grouping_variables, "color_breaks")
    }
    
    x[
      ,
      "y_offset" := ..set_shap_swarmplot_jitter(shap_value, feature_value, value_representation = value_representation),
      by = c("feature_name", facet_by, color_by)
    ]
    
    if (value_representation == "raw") {
      p <- p + ggplot2::geom_point(
        mapping = ggplot2::aes(color = !!sym("feature_value")),
        position = ggplot2::position_nudge(y = x$y_offset)
      )
      
      # Colors
      gradient_colours <- .get_palette(
        x = gradient_palette, 
        palette_type = "divergent"
      )
      
      # Add gradient palette.
      p <- p + ggplot2::scale_colour_gradientn(
        name = legend_label,
        colors = gradient_colours,
        limits = c(-1.0, 1.0)
      )
      
    } else if (is.null(color_by)) {
      p <- p + ggplot2::geom_point(position = ggplot2::position_nudge(y = x$y_offset))
      
    } else {
      p <- p + ggplot2::geom_jitter(
        mapping = ggplot2::aes(color = !!sym("color_breaks")),
        position = ggplot2::position_nudge(y = x$y_offset)
      )
      
      # Set fill colours.
      p <- p + ggplot2::scale_color_manual(
        name = legend_label$guide_color,
        values = g_color$color_values,
        breaks = g_color$color_breaks,
        drop = FALSE
      )
    }
    
  } else if (plot_type == "barplot") {
    # Barplot ------------------------------------------------------------------
    
    if (is.null(color_by)) {
      p <- p + ggplot2::geom_bar(
        stat = "identity",
        position = ggplot2::position_dodge(width = 0.9),
      )
      
    } else {
      # Add barplot.
      p <- p + ggplot2::geom_bar(
        mapping = ggplot2::aes(
          fill = !!sym("color_breaks")
        ),
        stat = "identity",
        position = ggplot2::position_dodge(width = 0.9)
      )
      
      # Set fill colours.
      p <- p + ggplot2::scale_fill_manual(
        name = legend_label$guide_color,
        values = g_color$color_values,
        breaks = g_color$color_breaks,
        drop = FALSE
      )
    }
    
  } else if (plot_type == "boxplot") {
    # Boxplot ------------------------------------------------------------------
    
    if (is.null(color_by)) {
      p <- p + ggplot2::geom_boxplot()
      
    } else {
      p <- p + ggplot2::geom_boxplot(
        mapping = ggplot2::aes(
          colour = !!sym("color_breaks")
        )
      )
      
      # Set fill colours.
      p <- p + ggplot2::scale_colour_manual(
        name = legend_label$guide_color,
        values = g_color$color_values,
        breaks = g_color$color_breaks,
        drop = FALSE
      )
    }
    
  } else if (plot_type == "violinplot") {
    # Violinplot ---------------------------------------------------------------
    
    if (is.null(color_by)) {
      # Create boxplot.
      p <- p + ggplot2::geom_violin(
        draw_quantiles = c(0.025, 0.5, 0.975),
        scale = "width",
        position = ggplot2::position_dodge(width = 1.0)
      )
      
    } else {
      # Create boxplot.
      p <- p + ggplot2::geom_violin(
        mapping = ggplot2::aes(
          fill = !!sym("color_breaks")
        ),
        draw_quantiles = c(0.025, 0.5, 0.975),
        scale = "width",
        position = ggplot2::position_dodge(width = 1.0)
      )
      
      # Set fill colours.
      p <- p + ggplot2::scale_fill_manual(
        name = legend_label$guide_color,
        values = g_color$color_values,
        breaks = g_color$color_breaks,
        drop = FALSE
      )
    }
  }
  
  # Determine how things are faceted.
  facet_by_list <- .parse_plot_facet_by(
    x = x, 
    facet_by = facet_by, 
    facet_wrap_cols = facet_wrap_cols
  )
  
  if (!is.null(facet_by)) {
    if (is.null(facet_wrap_cols)) {
      # Use a grid
      p <- p + ggplot2::facet_grid(
        rows = facet_by_list$facet_rows, 
        cols = facet_by_list$facet_cols, 
        labeller = "label_context"
      )
      
    } else {
      p <- p + ggplot2::facet_wrap(
        facets = facet_by_list$facet_by, 
        labeller = "label_context"
      )
    }
  }
  
  # Update labels.
  p <- p + ggplot2::labs(
    x = x_label, 
    y = y_label, 
    title = plot_title, 
    subtitle = plot_sub_title, 
    caption = caption
  )
  
  return(p)
}



.determine_shap_dependence_plot_dimensions <- function(
    x,
    x_axis_by,
    y_axis_by,
    facet_by,
    facet_wrap_cols
) {
  
  # Obtain facetting dimensions
  plot_dims <- .get_plot_layout_dims(
    x = x, 
    facet_by = facet_by, 
    facet_wrap_cols = facet_wrap_cols
  )
  
  # Set default height and width for each subplot (in cm).
  default_width <- 6.0
  default_height <- 4.0
  
  # Set overall plot height, but limit to small-margin A4 (27.7 cm)
  height <- min(c(2.0 + plot_dims[1L] * default_height, 27.7))
  
  # Set overall plot width, but limit to small-margin A4 (19 cm)
  width <- min(c(2.0 + plot_dims[2L] * default_width, 19.0))
  
  return(c(height, width))
}
