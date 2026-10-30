#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL



# plot_ordered_target (generic) ---------------------------------------------

#' @title Plot heatmaps for pairwise similarity between features.
#'
#' @description This method creates a heatmap based on data stored in a
#'   `familiarCollection` object. Features in the heatmap are ordered so that
#'   more similar features appear together.
#'
#' @param dir_path (*optional*) Path to the directory where created performance
#'   plots are saved to. Output is saved in the `feature_similarity`
#'   subdirectory. If `NULL` no figures are saved, but are returned instead.
#' @param gradient_palette (*optional*) Sequential or divergent palette used to
#'   colour the similarity or distance between features in a heatmap. `familiar`
#'   has a default palette. Other palettes are supported by the `paletteer`
#'   package, `grDevices::palette.pals()` (requires R >= 4.0.0),
#'   `grDevices::hcl.pals()` (requires R >= 3.6.0) and `rainbow`, `heat.colors`,
#'   `terrain.colors`, `topo.colors` and `cm.colors`, which correspond to the
#'   palettes of the same name in `grDevices`. You may also specify your own
#'   palette by providing a vector of colour names listed by
#'   `grDevices::colors()` or through hexadecimal RGB strings.
#' @param gradient_palette_range (*optional*) Numerical range used to span the
#'   gradient. This should be a range of two values, e.g. `c(0, 1)`. Lower or
#'   upper boundary can be unset by using `NA`. If not set, the full
#'   metric-specific range is used.
#' @param outcome_palette (*optional*) Sequential (`continuous` outcomes) or
#'   qualitative (other outcome types) palette used to show outcome values. This
#'   argument is ignored if the outcome is not shown.
#' @param outcome_palette_range (*optional*) Numerical range used to span the
#'   gradient of numeric (`continuous`) outcome values. This argument is ignored
#'   for other outcome types or if the outcome is not shown.
#' @param outcome_legend_label (*optional*) Label to provide to the legend for
#'   outcome data. If NULL, the legend will not have a name. By default,
#'   `class`, `value` and `event` are used for `binomial` and `multinomial`,
#'   `continuous`, and `survival` outcome types, respectively.
#'
#' @param evaluation_times (*optional*) Times at which the event status of
#'   time-to-event survival outcomes are determined. Only used for `survival`
#'   outcome. If not specified, the values used when creating the underlying
#'   `familiarData` objects are used.
#' @inheritParams export_feature_similarity
#' @inheritParams export_sample_similarity
#' @inheritParams as_familiar_collection
#' @inheritParams plot_univariate_importance
#' @inheritParams .check_input_plot_args
#' @inheritParams .check_plot_splitting_variables
#' @inheritDotParams as_familiar_collection -object
#' @inheritDotParams ggplot2::ggsave -height -width -units -path -filename -plot
#' @inheritDotParams extract_feature_expression -object -feature_cluster_method -feature_linkage_method -sample_cluster_method -sample_linkage_method
#'
#' @details This function generates area under the ROC curve plots.
#'
#'   Available splitting variables are: `vimp_method`, `learner`, and `data_set`.
#'   By default, the data is split by `vimp_method` and `learner` and `data_set`,
#'   since the number of samples will typically differ between data sets, even
#'   for the same variable importance method and learner.
#'
#'   The `x_axis_by` and `y_axis_by` arguments determine what data are shown
#'   along which axis. Each argument takes one of `feature` and `sample`, and
#'   both arguments should be unique. By default, features are shown along the
#'   x-axis and samples along the y-axis.
#'
#'   Note that similarity is determined based on the underlying data. Hence the
#'   ordering of features may differ between facets, and tick labels are
#'   maintained for each panel.
#'
#'   Labeling methods such as `set_vimp_method_names` or `set_data_set_names` can
#'   be applied to the `familiarCollection` object to update labels, and order
#'   the output in the figure.
#'   
#'   This plot can be created from `dataObject`, or `data.table` objects.
#'   For `data.table`, see \code{\link{as_data_object}} for additional arguments.
#'
#' @return `NULL` or list of plot objects, if `dir_path` is `NULL`.
#'
#' @exportMethod plot_ordered_target
#' @md
#' @rdname plot_ordered_target-methods
setGeneric(
  "plot_ordered_target",
  function(
    object,
    features = waiver(),
    draw = FALSE,
    dir_path = NULL,
    split_by = NULL,
    facet_by = NULL,
    facet_wrap_cols = NULL,
    ggtheme = NULL,
    outcome_palette = NULL,
    outcome_palette_range = waiver(),
    x_label = waiver(),
    y_label = waiver(),
    legend_label = waiver(),
    plot_title = waiver(),
    plot_sub_title = waiver(),
    caption = NULL,
    x_n_breaks = 3L,
    y_range = NULL,
    y_n_breaks = 3L,
    y_breaks = NULL,
    evaluation_times = waiver(),
    width = waiver(),
    height = waiver(),
    units = waiver(),
    export_collection = FALSE,
    verbose = TRUE,
    ...
  ) {
    standardGeneric("plot_ordered_target")
  }
)



# plot_ordered_target (general) ---------------------------------------------

#' @rdname plot_ordered_target-methods
setMethod(
  "plot_ordered_target",
  signature(object = "ANY"),
  function(
    object,
    features = waiver(),
    draw = FALSE,
    dir_path = NULL,
    split_by = NULL,
    facet_by = NULL,
    facet_wrap_cols = NULL,
    ggtheme = NULL,
    outcome_palette = NULL,
    outcome_palette_range = waiver(),
    x_label = waiver(),
    y_label = waiver(),
    legend_label = waiver(),
    plot_title = waiver(),
    plot_sub_title = waiver(),
    caption = NULL,
    x_n_breaks = 3L,
    y_range = NULL,
    y_n_breaks = 3L,
    y_breaks = NULL,
    evaluation_times = waiver(),
    width = waiver(),
    height = waiver(),
    units = waiver(),
    export_collection = FALSE,
    verbose = TRUE,
    ...
  ) {
    # Attempt conversion to familiarCollection object.
    if (is.waive(features)) {
      object <- do.call(
        as_familiar_collection,
        args = c(
          list(
            "object" = object,
            "data_element" = "prediction_data",
            "features" = features
          ),
          list(...)
        )
      )
      
    } else {
      object <- do.call(
        as_familiar_collection,
        args = c(
          list(
            "object" = object,
            "data_element" = "feature_expressions",
            "features" = features
          ),
          list(...)
        )
      )
    }
    
    return(do.call(
      plot_ordered_target,
      args = list(
        "object" = object,
        "features" = features,
        "draw" = draw,
        "dir_path" = dir_path,
        "split_by" = split_by,
        "facet_by" = facet_by,
        "facet_wrap_cols" = facet_wrap_cols,
        "ggtheme" = ggtheme,
        "outcome_palette" = outcome_palette,
        "outcome_palette_range" = outcome_palette_range,
        "x_label" = x_label,
        "y_label" = y_label,
        "legend_label" = legend_label,
        "plot_title" = plot_title,
        "plot_sub_title" = plot_sub_title,
        "caption" = caption,
        "x_n_breaks" = x_n_breaks,
        "y_range" = y_range,
        "y_n_breaks" = y_n_breaks,
        "y_breaks" = y_breaks,
        "evaluation_times" = evaluation_times,
        "width" = width,
        "height" = height,
        "units" = units,
        "export_collection" = export_collection,
        "verbose" = verbose
      )
    ))
  }
)



# plot_ordered_target (collection) ------------------------------------------

#' @rdname plot_ordered_target-methods
setMethod(
  "plot_ordered_target",
  signature(object = "familiarCollection"),
  function(
    object,
    features = waiver(),
    draw = FALSE,
    dir_path = NULL,
    split_by = NULL,
    facet_by = NULL,
    facet_wrap_cols = NULL,
    ggtheme = NULL,
    outcome_palette = NULL,
    outcome_palette_range = waiver(),
    x_label = waiver(),
    y_label = waiver(),
    legend_label = waiver(),
    plot_title = waiver(),
    plot_sub_title = waiver(),
    caption = NULL,
    x_n_breaks = 3L,
    y_range = NULL,
    y_n_breaks = 3L,
    y_breaks = NULL,
    evaluation_times = waiver(),
    width = waiver(),
    height = waiver(),
    units = waiver(),
    export_collection = FALSE,
    verbose = TRUE,
    ...
  ) {
    # Suppress NOTES due to non-standard evaluation in data.table
    .NATURAL <- NULL
    
    # Make sure the collection object is updated.
    object <- update_object(object = object)
    
    use_predictions <- is.waive(features)
    if (use_predictions) {
      # Get predicted values.
      browser()
    } else {
      
      # Get feature expression data
      feature_expression <- export_feature_expressions(
        object = object,
        features = features,
        evaluation_time = evaluation_times
      )
      
      # Check that the data are not empty.
      if (is_empty(feature_expression)) return(NULL)
      if (all(sapply(feature_expression, is_empty))) return(NULL)
      
      feature_expression <- feature_expression[!sapply(feature_expression, is_empty)]
      
      # Add the outcome columns as grouping column, and melt the table to have
      # features as a grouping variable.
      x <- lapply(
        feature_expression,
        function(x, outcome_columns) {
          
          return(data.table::melt(
            data = x@data,
            id.vars = c(x@grouping_column, outcome_columns),
            measure.vars = x@value_column,
            variable.name = "feature",
            value.name = "value"
          ))
          
          return(x)
        },
        outcome_columns = get_outcome_columns(object@outcome_type)
      )

      # Flatten to single data.table.
      x <- data.table::rbindlist(x, use.names = TRUE)
    }
    
    # Check package requirements for plotting.
    if (!require_package(
      x = ..required_plotting_packages(extended = TRUE),
      purpose = "to create sample clustering heatmaps",
      message_type = "warning"
    )) {
      return(NULL)
    }
    
    # Check input arguments ----------------------------------------------------
    
    # ggtheme
    ggtheme <- .check_ggtheme(ggtheme)
    
    # x_label
    if (is.waive(x_label)) x_label <- NULL
    
    # y_label
    if (is.waive(y_label)) y_label <- NULL
    
    # outcome_legend_label
    if (is.waive(legend_label)) {
      # Assign default label to the legend.
      if (object@outcome_type %in% c("binomial", "multinomial")) {
        legend_label <- "class"
      } else if (object@outcome_type %in% c("continuous")) {
        legend_label <- "value"
      } else if (object@outcome_type %in% c("survival", "competing_risk")) {
        legend_label <- "event"
      } else if (object@outcome_type == "unsupervised") {
        legend_label <- "unset"
      } else {
        ..error_outcome_type_not_implemented(object@outcome_type)
      }
    }
    
    # check outcome_legend_label
    .check_input_plot_label(
      label_var = legend_label,
      var_name = "legend_label"
    )
    
    available_splitting_vars <- c("vimp_method", "learner", "data_set")
    default_facet_vars <- NULL
    if (!use_predictions) {
      available_splitting_vars <- c(available_splitting_vars, "feature")
    }
    if (object@outcome_type == "multinomial") {
      available_splitting_vars <- c(available_splitting_vars, "positive_class")
      default_facet_vars <- "positive_class"
    }
    if (object@outcome_type %in% c("survival", "competing_risk")) {
      available_splitting_vars <- c(available_splitting_vars, "evaluation_time")
      default_facet_vars <- "evaluation_time"
    }
    
    # Add default splitting variables
    if (is.null(split_by) & is.null(facet_by)) {
      # Split by variable importance method, learner and data set, plus feature
      # if applicable.
      split_by <- setdiff(available_splitting_vars, default_facet_vars)
      
      # Use facets for multinomial or survival outcomes..
      facet_by <- default_facet_vars
    }

    # Check splitting variables and generate sanitised output
    split_var_list <- .check_plot_splitting_variables(
      x = x,
      split_by = split_by,
      facet_by = facet_by,
      available = available_splitting_vars
    )
    
    # Update splitting variables
    split_by <- split_var_list$split_by
    facet_by <- split_var_list$facet_by
    
    # Check input arguments
    .check_input_plot_args(
      facet_wrap_cols = facet_wrap_cols,
      x_label = x_label,
      y_label = y_label,
      legend_label = legend_label,
      plot_title = plot_title,
      plot_sub_title = plot_sub_title,
      caption = caption
    )
    
    # Create plots -------------------------------------------------
    
    # Determine if subtitle should be generated.
    autogenerate_plot_subtitle <- is.waive(plot_sub_title)
    
    # Split data.
    if (!is.null(split_by)) {
      x_split <- split(
        x,
        by = split_by, 
        drop = FALSE
      )
      
    } else {
      x_split <- list("null.name" = x)
    }
    
    # Store plots to list in case dir_path is absent.
    if (is.null(dir_path)) plot_list <- list()
    
    # Iterate over splits
    for (x_sub in x_split) {
      if (is_empty(x_sub)) next
      
      # Declare subtitle components.
      additional_subtitle <- NULL
      
      if (is.waive(plot_title)) plot_title <- "Ordered target plot"
      
      if (autogenerate_plot_subtitle) {
        plot_sub_title <- .create_plot_subtitle(
          split_by = split_by,
          additional = additional_subtitle,
          x = x_sub
        )
      }
      
      # Generate plot
      p <- .plot_ordered_target_plot(
        x = x_sub,
        outcome_type = object@outcome_type,
        facet_by = facet_by,
        facet_wrap_cols = facet_wrap_cols,
        ggtheme = ggtheme,
        outcome_palette = outcome_palette,
        x_label = x_label,
        y_label = y_label,
        legend_label = legend_label,
        plot_title = plot_title,
        plot_sub_title = plot_sub_title,
        caption = caption,
        x_n_breaks = x_n_breaks,
        y_range = y_range,
        y_n_breaks = y_n_breaks,
        y_breaks = y_breaks
      )
      
      # Check empty output
      if (is.null(p)) next
      
      # Draw figure.
      if (draw) .draw_plot(plot_or_grob = p)
      
      # Save and export
      if (!is.null(dir_path)) {
        browser()

        # Obtain decent default values for the plot.
        def_plot_dims <- .determine_ordered_target_plot_dimensions(
          x = x_sub,
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
              "type" = "calibration",
              "subtype" = "ordered_target",
              "x" = x_sub,
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



.plot_ordered_target_plot <- function(
    x,
    outcome_type,
    facet_by,
    facet_wrap_cols,
    ggtheme,
    outcome_palette,
    x_label,
    y_label,
    legend_label,
    plot_title,
    plot_sub_title,
    caption,
    x_n_breaks,
    y_range,
    y_n_breaks,
    y_breaks
) {
  # Suppress NOTES due to non-standard evaluation in data.table
  .NATURAL <- value <- eval_time <- NULL

  # Set type of palette that is to be used for default palettes.
  if (outcome_type %in% c("binomial", "multinomial")) {
    palette_type <- "qualitative"
    x[, "outcome_value" := outcome]
    
  } else if (outcome_type %in% c("continuous")) {
    palette_type <- "sequential"
    x[, "outcome_value" := outcome]
    
  } else if (outcome_type %in% c("survival", "competing_risk")) {
    palette_type <- "qualitative"
    browser()
    # Mark all as no event initially.
    x[, "outcome_value" := "no"]
    
    # Mark all data with missing outcome_time or outcome_event as missing.
    x[!is.finite(outcome_time) | is.na(outcome_event), "outcome_value" := NA_character_]
    
    # All samples that have an event before or at the evaluation time.
    x[outcome_time <= eval_time & outcome_event == 1L, "outcome_value" := "yes"]
    
    # All samples that were censored (lost to follow-up) at the evaluation time.
    x[outcome_time <= eval_time & outcome_event == 0L, "outcome_value" := "cens."]
    
    # Set as factor
    x$outcome_value <- factor(x$outcome_value, levels = c("no", "yes", "cens."))
    
  } else {
    ..error_outcome_type_not_implemented(outcome_type)
  }
  
  # y_range
  if (is.null(y_range)) {
    # Obtain default ranges for the metrics.
    y_range <- c(min(x$value), max(x$value))
    
  } else {
    .check_input_plot_args(y_range = y_range)
  }
  
  # y_breaks
  if (is.null(y_breaks)) {
    .check_input_plot_args(
      y_range = y_range,
      y_n_breaks = y_n_breaks
    )
    
    # Create breaks and update y_range
    y_breaks <- labeling::extended(
      m = y_n_breaks,
      dmin = y_range[1L],
      dmax = y_range[2L],
      only.loose = FALSE
    )
    
    y_range <- c(
      head(y_breaks, n = 1L),
      tail(y_breaks, n = 1L)
    )
    
  } else {
    .check_input_plot_args(y_breaks = y_breaks)
  }
  
  # Set the x-range values
  if (is.null(facet_by)) {
    x[, "plot_index" := rank(value, ties.method = "random")]
    
  } else {
    x[, "plot_index" := rank(value, ties.method = "random"), by = facet_by]
  }

  # x_range
  x_range <- c(min(x$plot_index), max(x$plot_index))
  
  # x_breaks
  x_breaks <- labeling::extended(
    m = x_n_breaks,
    dmin = x_range[1L],
    dmax = x_range[2L],
    only.loose = FALSE
  )
  
  # Update x_range
  x_range <- c(
    head(x_breaks, n = 1L),
    tail(x_breaks, n = 1L)
  )
  
  p <- ggplot2::ggplot(
    data = x,
    mapping = ggplot2::aes(
      x = !!sym("plot_index"),
      y = !!sym("value"),
      fill = !!sym("outcome_value")
    )
  )
  # plot creation --------------------------------------------------------------
  
  # Add plot theme
  p <- p + ggtheme
  
  # Set bars.
  p <- p + ggplot2::geom_bar(stat = "identity")
  
  if (palette_type == "qualitative") {
    # Colours
    outcome_colours <- .get_palette(
      x = outcome_palette,
      n = nlevels(x$outcome_value),
      palette_type = palette_type
    )
    
    # Set the qualitative scale
    p <- p + ggplot2::scale_fill_manual(
      name = legend_label,
      values = outcome_colours[seq_along(levels(x$outcome_value))],
      breaks = levels(x$outcome_value),
      drop = FALSE
    )
    
  } else {
    outcome_colours <- .get_palette(
      x = outcome_palette,
      palette_type = palette_type
    )
    
    # Set the gradient
    p <- p + ggplot2::scale_fill_gradientn(
      name = legend_label,
      colors = outcome_colours,
      limits = range(labeling::extended(
        dmin = min(x$outcome_value),
        dmax = max(x$outcome_value),
        m = 5L,
        only.loose = FALSE
      )),
      oob = scales::squish
    )
  }
  
  p <- p + ggplot2::scale_x_continuous(
    limits = x_range,
    breaks = x_breaks
  )
  p <- p + ggplot2::scale_y_continuous(
    limits = y_range,
    breaks = y_breaks
  )
  
  # Determine how things are facetted
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




.determine_ordered_target_plot_dimensions <- function(
    x,
    facet_by,
    facet_wrap_cols
) {
  # Obtain facetting dimensions
  plot_dims <- .get_plot_layout_dims(
    x = x,
    facet_by = facet_by,
    facet_wrap_cols = facet_wrap_cols
  )
  
  # Determine the number of elements along the x-axis.
  if (x_axis_by == "feature") {
    x_n_elements <- length(features)
    x_longest_element <- max(sapply(features, nchar))
    y_n_elements <- length(samples)
    y_longest_element <- max(sapply(samples, nchar))
  } else {
    x_n_elements <- length(samples)
    x_longest_element <- max(sapply(samples, nchar))
    y_n_elements <- length(features)
    y_longest_element <- max(sapply(features, nchar))
  }
  
  # Assume each x-axis element takes up about 0.5 cm. Then add some room for
  # other plot elements.
  default_width <- x_n_elements * 0.5 + 1.0
  
  # Assume each y-axis element takes up about 0.5 cm as well.
  default_height <- y_n_elements * 0.5 + 1.0
  
  # Reserve space for y-axis and x-axis tick labels. Assume that the typical
  # width of a character is about 5 points (1.8 mm). For the x-axis we only
  # reserve extra space in case the ticks are rotated, otherwise we just
  # assume a typical height of 10 points (3.6 mm).
  y_tick_space <- y_longest_element * 0.18
  x_tick_space <- ifelse(rotate_x_tick_labels, x_longest_element * 0.18, 0.36)
  
  # Reserve space for the dendrograms (1.5 cm)
  dendro_height <- ifelse(
    any(c("top", "bottom") %in% c(show_feature_dendrogram, show_sample_dendrogram)),
    1.5, 0.0
  )
  dendro_width <- ifelse(
    any(c("left", "right") %in% c(show_feature_dendrogram, show_sample_dendrogram)),
    1.5, 0.0
  )
  
  # Set overall plot height, but limit to small-margin A4 (27.7 cm)
  height <- min(c(2.0 + plot_dims[1L] * (default_height + x_tick_space + dendro_height), 27.7))
  
  # Set overall plot width, but limit to small-margin A4 (19 cm). We leave some
  # room for the legend on the right.
  width <- min(c(5.0 + plot_dims[2L] * (default_width + y_tick_space + dendro_width), 19.0))
  
  return(c(height, width))
}
