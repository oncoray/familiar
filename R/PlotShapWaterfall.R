#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL



# plot_shap_waterfall (generic) ------------------------------------------------

#' @title Create SHAP waterfall plot
#'
#' @description This method creates plots that show a summary of SHAP values
#'   obtained from the data stored in a familiarCollection object. .
#'
#' @param dir_path (*optional*) Path to the directory where created performance
#'   plots are saved to. Output is saved in the `explanation` subdirectory. If
#'   `NULL` no figures are saved, but are returned instead.
#' @param plot_type (*optional*) Type of plot to draw. This is one of
#'   `waterfall` or `stacked`.
#'
#'   The choice for `plot_type` affects several other arguments.
#' @param gradient_palette (*optional*) Sequential or divergent palette used to
#'   colour the elements of waterfall plots. `familiar` has a default
#'   palette. Other palettes are supported by the `paletteer` package,
#'   `grDevices::palette.pals()` (requires R >= 4.0.0), `grDevices::hcl.pals()`
#'   (requires R >= 3.6.0) and `rainbow`, `heat.colors`, `terrain.colors`,
#'   `topo.colors` and `cm.colors`, which correspond to the palettes of the same
#'   name in `grDevices`. You may also specify your own palette by providing a
#'   vector of colour names listed by `grDevices::colors()` or through
#'   hexadecimal RGB strings.
#' @inheritParams as_familiar_collection
#' @inheritParams plot_univariate_importance
#' @inheritParams .check_input_plot_args
#' @inheritParams .check_plot_splitting_variables
#' @inheritDotParams extract_performance -object
#' @inheritDotParams as_familiar_collection -object
#' @inheritDotParams ggplot2::ggsave -height -width -units -path -filename -plot
#'
#' @details This function plots model performance based on empirical bootstraps,
#'   using various plot representations.
#'
#'   Available splitting variables are: `vimp_method`, `learner`,
#'   `evaluation_time` (survival outcome only) and `positive_class` (categorical
#'   outcomes). The default for is to facet by `evaluation_time` or 
#'   `positive_class`, and split by `vimp_method` and
#'   `learner`. `color_by` is not used.
#'
#'   Labelling methods such as `set_vimp_method_names` or `set_learner_names`
#'   can be applied to the `familiarCollection` object to update labels, and
#'   order the output in the figure.
#'
#' @return `NULL` or list of plot objects, if `dir_path` is `NULL`.
#'
#' @exportMethod plot_shap_waterfall
#' @md
#' @rdname plot_shap_waterfall-methods
setGeneric(
  "plot_shap_waterfall",
  function(
    object,
    draw = FALSE,
    dir_path = NULL,
    split_by = NULL,
    x_axis_by = NULL,
    y_axis_by = NULL,
    color_by = NULL,
    facet_by = NULL,
    facet_wrap_cols = NULL,
    plot_type = NULL,
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
    width = waiver(),
    height = waiver(),
    units = waiver(),
    export_collection = FALSE,
    ...
  ) {
    standardGeneric("plot_shap_waterfall")
  }
)



# plot_shap_waterfall (general) ------------------------------------------------

#' @rdname plot_shap_waterfall-methods
setMethod(
  "plot_shap_waterfall",
  signature(object = "ANY"),
  function(
    object,
    draw = FALSE,
    dir_path = NULL,
    split_by = NULL,
    x_axis_by = NULL,
    y_axis_by = NULL,
    color_by = NULL,
    facet_by = NULL,
    facet_wrap_cols = NULL,
    plot_type = NULL,
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
      plot_shap_waterfall,
      args = list(
        "object" = object,
        "draw" = draw,
        "dir_path" = dir_path,
        "split_by" = split_by,
        "x_axis_by" = x_axis_by,
        "y_axis_by" = y_axis_by,
        "color_by" = color_by,
        "facet_by" = facet_by,
        "facet_wrap_cols" = facet_wrap_cols,
        "ggtheme" = ggtheme,
        "plot_type" = plot_type,
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
        "width" = width,
        "height" = height,
        "units" = units,
        "export_collection" = export_collection
      )
    ))
  }
)



# plot_shap_waterfall (collection) -----------------------------------------------

#' @rdname plot_shap_waterfall-methods
setMethod(
  "plot_shap_waterfall",
  signature(object = "familiarCollection"),
  function(
    object,
    draw = FALSE,
    dir_path = NULL,
    split_by = NULL,
    x_axis_by = NULL,
    y_axis_by = NULL,
    color_by = NULL,
    facet_by = NULL,
    facet_wrap_cols = NULL,
    plot_type = NULL,
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
    
    x <- x$shap_force
    if (is_empty(x)) return(NULL)
    
    # Obtain single data element from list.
    if (is.list(x)) {
      if (length(x) > 1L) {
        ..error_reached_unreachable_code(
          "plot_shap_waterfall: list of data elements contains unmerged elements."
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
      purpose = "to create a SHAP waterfall plot",
      message_type = "warning"
    )) {
      return(NULL)
    }
    
    # Check input arguments ----------------------------------------------------
    
    value_data <- x@data[, list("n" = .N), by = setdiff(x@grouping_column, c("sample_id", "feature_value", "feature_label"))]
    is_single_sample <- all(value_data$n == 1L)
    
    if (is.null(plot_type)) {
      plot_type <- ifelse(is_single_sample, "waterfall", "stacked")
    }
    
    # Check plot_type.
    .check_parameter_value_is_valid(
      x = plot_type,
      var_name = "plot_type",
      values = c("waterfall", "stacked")
    )
    
    
    # Add evaluation time or class as a splitting variable.
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
      is.null(color_by) &&
      is.null(facet_by)
    ) {
      # Split by vimp_method, learner and sample id.
      split_by <- c("vimp_method", "learner")
      if (plot_type == "waterfall") split_by <- c(split_by, "sample_id")
      facet_by <- additional_variable
    }
    
    all_variables <- c("vimp_method", "learner", additional_variable)
    if (plot_type == "waterfall") all_variables <- c(all_variables, "sample_id")
    
    # Check splitting variables and generate sanitised output
    split_var_list <- .check_plot_splitting_variables(
      x = x@data,
      split_by = split_by,
      color_by = color_by,
      facet_by = facet_by,
      available = all_variables
    )
    
    # Update splitting variables
    split_by <- split_var_list$split_by
    color_by <- split_var_list$color_by
    facet_by <- split_var_list$facet_by
    
    # x_label
    if (is.waive(x_label)) {
      x_label <- "predicted value"
    }
    
    # y_label
    if (is.waive(y_label)) {
      y_label <- "feature"
    }
    
    .check_input_plot_args(
      facet_wrap_cols = facet_wrap_cols,
      x_label = x_label,
      y_label = y_label,
      plot_title = plot_title,
      plot_sub_title = plot_sub_title,
      caption = caption
    )
    
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
    
    # Store plots to list in case dir_path is absent.
    if (is.null(dir_path)) plot_list <- list()
    
    # Iterate over data splits.
    for (ii in names(x_split)) {
      # Skip empty datasets.
      if (is_empty(x_split[[ii]])) next
      
      if (is.waive(plot_title)) plot_title <- "SHAP waterfall"
      
      # Declare subtitle components.
      additional_subtitle <- NULL
      
      # Add evaluation time as subtitle component if it is not used
      # otherwise.
      if (
        !"evaluation_time" %in% c(split_by, color_by, facet_by) &&
        object@outcome_type %in% c("survival")
      ) {
        additional_subtitle <- c(
          additional_subtitle,
          .add_time_to_plot_subtitle(x_split[[ii]]$evaluation_time[1L])
        )
      }
      
      if (autogenerate_plot_subtitle) {
        plot_sub_title <- .create_plot_subtitle(
          split_by = split_by,
          additional = additional_subtitle,
          x = x_split[[ii]]
        )
      }
      
      # Generate plot
      p <- .plot_shap_waterfall_plot(
        x = x_split[[ii]],
        color_by = color_by,
        facet_by = facet_by,
        facet_wrap_cols = facet_wrap_cols,
        plot_type = plot_type,
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
        outcome_type = object@outcome_type
      )
      
      # Check empty output
      if (is.null(p)) next
      
      # Draw figure.
      if (draw) .draw_plot(plot_or_grob = p)
      
      # Save and export
      if (!is.null(dir_path)) {
        # Obtain decent default values for the plot.
        def_plot_dims <- .determine_shap_waterfall_plot_dimensions(
          x = x_split[[ii]],
          plot_type = plot_type,
          facet_by = facet_by,
          facet_wrap_cols = facet_wrap_cols
        )
        
        # Save to file.
        do.call(
          .save_plot_to_file,
          args = c(
            list(
              "plot_or_grob" = p,
              "object" = object,
              "dir_path" = dir_path,
              "type" = "explanation",
              "subtype" = paste0(plot_type),
              "x" = x_split[[ii]],
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



.plot_shap_waterfall_plot <- function(
    x,
    color_by,
    facet_by,
    facet_wrap_cols,
    plot_type,
    ggtheme,
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
  # TODO:
  # faceting: free x-range in each facet
  # compute start and stop points based on shap value and iterate from the most important feature to the least.
  # set colour based on whether SHAP value is positive or not.
  # 
  
  # Suppress NOTES due to non-standard evaluation in data.table
  shap_value <- vimp <- feature_value <- feature_name <- prediction <- NULL
  
  # Sort features by importance (mean absolute SHAP).
  feature_importance <- x[, list("vimp" = mean(abs(shap_value))), by = c(facet_by, "feature_name")]
  feature_importance <- feature_importance[, list("vimp" = max(vimp)), by = "feature_name"][order(vimp)]
  x$feature_name <- factor(
    x = x$feature_name,
    levels = feature_importance$feature_name
  )
  x[, "y" := as.numeric(feature_name)]
  
  # Common base for formatting prediction and shap values.
  common_base <- ..format_get_common_base(c(x$shap_value, x$prediction))

  # Update start and end positions.
  if (!is.null(facet_by)) {
    x[, (c("x_start", "x_end")) := ..set_shap_waterfall_positions(shap_value, prediction, feature_name), by = facet_by]
    
  } else {
    x[, (c("x_start", "x_end")) := ..set_shap_waterfall_positions(shap_value, prediction, feature_name)]
  }
  browser()
  # Check x-range.
  # if (is.null(x_range)) {
  #   if (value_representation == "raw") {
  #     x_range <- c(min(x$shap_value), max(x$shap_value))
  #     
  #   } else {
  #     x_range <- c(0.0, max(x$shap_value))
  #   }
  #   
  # } else {
  #   .check_input_plot_args(x_range = x_range)
  # }
  # 
  # # x_breaks
  # if (is.null(x_breaks)) {
  #   .check_input_plot_args(
  #     x_range = x_range,
  #     x_n_breaks = x_n_breaks
  #   )
  #   
  #   # Create breaks and update x_range
  #   x_breaks <- labeling::extended(
  #     m = x_n_breaks,
  #     dmin = x_range[1L],
  #     dmax = x_range[2L],
  #     only.loose = TRUE
  #   )
  #   
  #   x_range <- c(
  #     head(x_breaks, n = 1L),
  #     tail(x_breaks, n = 1L)
  #   )
  #   
  # } else {
  #   .check_input_plot_args(x_breaks = x_breaks)
  # }
  # 
  # Create a legend label.
  legend_label <- .create_plot_legend_title(
    user_label = legend_label,
    color_by = "shap_value"
  )
  
  # Check remaining input arguments.
  .check_input_plot_args(
    legend_label = legend_label
  )
  
  # Generate a guide table
  guide_list <- .create_plot_guide_table(
    x = x,
    color_by = color_by
  )
  
  # Extract data
  x <- guide_list$data
  
  # Extract guide_table for color.
  g_color <- guide_list$guide_color
  
  # Set up basic waterfall plot.
  p <- ggplot2::ggplot(data = x)
  p <- p + ggtheme
  p <- p + ggplot2::geom_rect(
    data = x,
    mapping = ggplot2::aes(
      xmin = !!sym("x_start"),
      xmax = !!sym("x_end"),
      ymin = !!sym("y") - 0.4,
      ymax = !!sym("y") + 0.4,
      fill = !!sym("shap_value")
    )
  )
  p <- p + ggplot2::scale_y_continuous(
    breaks = seq_len(nlevels(x$feature_name)),
    labels = levels(x$feature_name)
  )
  
  # Add gradient palette.
  gradient_colours <- .get_palette(
    x = gradient_palette, 
    palette_type = "divergent"
  )
  
  p <- p + ggplot2::scale_fill_gradientn(
    name = legend_label,
    colors = gradient_colours,
    limits = c(-max(abs(x$shap_value)), max(abs(x$shap_value)))
  )
  
  browser()
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



..set_shap_waterfall_positions <- function(x, predictions, feature_name) {
  # Prevent notes.
  feature_value <- density <- y_offset <- NULL
  
  # Initialise.
  x_start <- x_end <- numeric(length(x))
  
  # Set feature order.
  feature_order <- order(feature_name, decreasing = TRUE)
  
  # We fill start and end positions in reverse order, beginning with the most
  # important feature.
  previous_start <- utils::head(predictions, n = 1L)
  for (ii in feature_order) {
    x_end[ii] <- previous_start
    x_start[ii] <- previous_start - x[ii]
    previous_start <- x_start[ii]
  }

  return(list("x_start" = x_start, "x_end" = x_end))
}



.determine_shap_waterfall_plot_dimensions <- function(
    x,
    plot_type,
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
