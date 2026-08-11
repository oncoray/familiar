#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL



# plot_shap_waterfall (generic) ------------------------------------------------

#' @title Create SHAP waterfall plot
#'
#' @description This method creates plots that show a waterfall of SHAP values
#'   obtained from the data stored in a familiarCollection object.
#'
#' @param dir_path (*optional*) Path to the directory where created 
#'   plots are saved to. Output is saved in the `explanation` subdirectory. If
#'   `NULL` no figures are saved, but are returned instead.
#' @param gradient_palette (*optional*) Divergent palette used to
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
#' @details This function creates SHAP waterfall plots, which show the
#'   individual marginal contributions of feature values to the predicted value.
#'
#'   Available splitting variables are: `vimp_method`, `learner`, `data_set`,
#'   `evaluation_time` (survival outcome only) and `positive_class` (categorical
#'   outcomes), `sample_id`. The default for is to facet by `evaluation_time` or 
#'   `positive_class`, and split by `vimp_method`,
#'   `learner`, `data_set`, and `sample_id`. `color_by` is not used.
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
    limit_n_features = waiver(),
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
    limit_n_features = waiver(),
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
        "limit_n_features" = limit_n_features,
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
    limit_n_features = waiver(),
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
      is.null(facet_by)
    ) {
      # Split by vimp_method, learner and sample id.
      split_by <- c("vimp_method", "learner", "data_set", "sample_id")
      facet_by <- additional_variable
    }
    
    all_variables <- c("vimp_method", "learner", "data_set", "sample_id", additional_variable)
    
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
      caption = caption,
      limit_n_features = limit_n_features
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
        !"evaluation_time" %in% c(split_by, facet_by) &&
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
        limit_n_features = limit_n_features,
        x_range = x_range,
        x_n_breaks = x_n_breaks,
        x_breaks = x_breaks
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
              "subtype" = "shap_waterfall",
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
    facet_by,
    facet_wrap_cols,
    ggtheme,
    gradient_palette,
    x_label,
    y_label,
    legend_label,
    plot_title,
    plot_sub_title,
    caption,
    limit_n_features,
    x_range,
    x_n_breaks,
    x_breaks
) {
  # Suppress NOTES due to non-standard evaluation in data.table
  shap_value <- vimp <- feature_value <- feature_name <- NULL
  feature_label <- prediction <- y <- label_text <- phi_0 <- NULL
  
  # Sort features by importance (mean absolute SHAP).
  feature_importance <- x[, list("vimp" = mean(abs(shap_value))), by = c(facet_by, "feature_name")]
  feature_importance <- feature_importance[, list("vimp" = max(vimp)), by = "feature_name"][order(vimp)]
  
  # Determine the features that need to be plotted.
  if (is.numeric(limit_n_features)) {

    # Explicitly select features based on threshold value instead of simply 
    # selecting the best features: features may have the same value because they
    # belong to the same cluster.
    threshold_value <- min(tail(unique(sort(feature_importance$vimp)), n = limit_n_features))
    selected_features <- feature_importance[vimp >= threshold_value]$feature_name
    
    # Determine contribution of features that were not selected (if any)
    not_selected_features <- setdiff(feature_importance$feature_name, selected_features)
    
    if (!is_empty(not_selected_features)) {
      # Use a template to prevent having to manually fill out columns that are
      # not updated.
      x_template <- data.table::copy(x[feature_name == not_selected_features[1L]])
      x_template[, "feature_name" := "other"]
      x_template[, "feature_value" := NA_real_]
      x_template[, "feature_label" := NA_character_]
      x_template[, "shap_value" := NULL]
      
      other_shap_value <- x[
        feature_name %in% not_selected_features,
        list(
          "feature_name" = "other",
          "shap_value" = sum(shap_value)
        ),
        by = facet_by
      ]
      
      x_template <- merge(
        x = x_template,
        y = other_shap_value,
        by = c("feature_name", facet_by)
      )
      
      x <- x[feature_name %in% selected_features, ]
      x <- data.table::rbindlist(list(x, x_template), use.names = TRUE)
      
      feature_importance <- feature_importance[vimp >= threshold_value][order(vimp)]

      x$feature_name <- factor(
        x = x$feature_name,
        levels = c("other", as.character(feature_importance$feature_name))
      )
      
    } else {
      # All features were selected: use default procedure.
      x$feature_name <- factor(
        x = x$feature_name,
        levels = feature_importance$feature_name
      )
    }
    
  } else {
    # Default procedure without selection.
    x$feature_name <- factor(
      x = x$feature_name,
      levels = feature_importance$feature_name
    )
  }
  
  # Add y (for positioning).
  x[, "y" := as.numeric(feature_name)]
  
  y_label_table <- unique(x[, mget(c("feature_name", "feature_value", "feature_label", "y"))])
  y_label_table[, "feature_name_value" := ..set_shap_waterfall_feature_name_value(feature_name, feature_value, feature_label)]
  y_label_table <- y_label_table[order(y)]

  # Common base for formatting prediction and shap values.
  common_base <- ..format_get_common_base(c(x$shap_value, x$prediction))
  n_small <- max(c(-(common_base - 2L), 0.0))

  # Update start and end positions for force elements.
  if (!is.null(facet_by)) {
    x[, (c("x_start", "x_end")) := ..set_shap_waterfall_positions(shap_value, prediction, feature_name), by = facet_by]
    
  } else {
    x[, (c("x_start", "x_end")) := ..set_shap_waterfall_positions(shap_value, prediction, feature_name)]
  }
  
  # Derive information for the average prediction and extract the instance
  # prediction.
  if (!is.null(facet_by)) {
    f_average_data <- x[, list("prediction" = max(phi_0)), by = facet_by]
  } else {
    f_average_data <- x[, list("prediction" = max(phi_0))]
  }
  
  f_instance_data <- unique(x[, mget(c("prediction", facet_by))])
  
  # Check x-range.
  if (!is.null(x_range)) {
    .check_input_plot_args(x_range = x_range)
    
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
  }

  # Create a legend label.
  legend_label <- .create_plot_legend_title(
    user_label = legend_label,
    color_by = "shap_value"
  )
  
  # Check remaining input arguments.
  .check_input_plot_args(
    legend_label = legend_label
  )
  
  # Add gradient palette.
  gradient_colours <- .get_palette(
    x = gradient_palette, 
    palette_type = "divergent"
  )
  
  # Set up shap value labels
  x[, "label_align" := data.table::fifelse(shap_value >= 0.0, yes = "left", no = "right")]
  x[, "label_colour" := data.table::fifelse(shap_value >= 0.0, yes = tail(gradient_colours, n = 1L), no = head(gradient_colours, n = 1L))]
  x[, "label_text" := format(round(shap_value, digits = n_small), nsmall = n_small)]
  x[, "label_text" := paste0(" ", label_text, " ")]
  
  # Set up basic waterfall plot.
  p <- ggplot2::ggplot(data = x)
  p <- p + ggtheme
  p <- p + geom_waterfall_shap(
    data = x,
    mapping = ggplot2::aes(
      x = !!sym("x_start"),
      xend = !!sym("x_end"),
      y = !!sym("y"),
      ymin = !!sym("y") - 0.4,
      ymax = !!sym("y") + 0.4,
      fill = !!sym("shap_value")
    )
  )
  
  # Set labels for y-axis.
  p <- p + ggplot2::scale_y_continuous(
    breaks = y_label_table$y,
    labels = y_label_table$feature_name_value
  )
  
  # Add spacing for text values.
  p <- p + ggplot2::scale_x_continuous(
    expand = ggplot2::expansion(mult = 0.2)
  )
  
  p <- p + ggplot2::scale_fill_gradientn(
    name = legend_label,
    colors = gradient_colours,
    limits = c(-max(abs(x$shap_value)), max(abs(x$shap_value)))
  )
  
  # Add instance and group average prediction.
  p <- p + ggplot2::geom_vline(
    data = f_average_data,
    mapping = ggplot2::aes(xintercept = !!sym("prediction")),
    color = "grey80",
    linetype = 2L
  )
  p <- p + ggplot2::geom_vline(
    data = f_instance_data,
    mapping = ggplot2::aes(xintercept = !!sym("prediction")),
    color = "grey80",
    linetype = 2L
  )
  
  # Use geom-segment to connect segments.
  p <- p + ggplot2::geom_segment(
    data = x,
    mapping = ggplot2::aes(
      x = !!sym("x_start"),
      xend = !!sym("x_start"),
      y = !!sym("y") - 0.5,
      yend = !!sym("y") + 0.45,
    ),
    color = "grey60"
  )
  p <- p + ggplot2::geom_segment(
    data = x,
    mapping = ggplot2::aes(
      x = !!sym("x_end"),
      xend = !!sym("x_end"),
      y = !!sym("y") - 0.45,
      yend = !!sym("y") + 0.5,
    ),
    color = "grey60"
  )
  
  text_settings <- .get_plot_geom_text_settings(ggtheme = ggtheme)
  
  # Add shap label.
  for (x_text in split(x, by = "label_colour", drop = TRUE)) {
    p <- p + ggplot2::geom_text(
      data = x_text,
      mapping = ggplot2::aes(
        x = !!sym("x_end"),
        y = !!sym("y"),
        label = !!sym("label_text"),
        hjust = !!sym("label_align")
      ),
      colour = head(x_text$label_colour, n = 1L),
      family = text_settings$family,
      fontface = text_settings$face,
      size = text_settings$geom_text_size,
      show.legend = FALSE
    )
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
        scales = ifelse(is.null(x_range), "free_x", "fixed"),
        labeller = "label_context"
      )
      
    } else {
      p <- p + ggplot2::facet_wrap(
        facets = facet_by_list$facet_by, 
        scales = ifelse(is.null(x_range), "free_x", "fixed"),
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
  
  # Prevent clipping of confidence intervals.
  if (!is.null(x_range)) p <- p + ggplot2::coord_cartesian(xlim = x_range)
  
  return(p)
}



..set_shap_waterfall_positions <- function(x, predictions, feature_name) {
  # Prevent notes.
  feature_value <- density <- y_offset <- NULL
  
  # Initialise.
  x_start <- x_end <- y <- y_seg_start <- y_seg_end <- numeric(length(x))
  
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

  return(list(
    "x_start" = x_start,
    "x_end" = x_end
  ))
}



..set_shap_waterfall_feature_name_value <- function(
    feature_name,
    feature_value, 
    feature_label
) {
  actual_label <- feature_label
  actual_label[is.na(feature_label)] <- signif(feature_value[is.na(feature_label)], 3L)
  actual_label[!is.na(actual_label)] <- paste0(": ", actual_label[!is.na(actual_label)])
  actual_label[is.na(actual_label)] <- ""
  
  return(paste0(feature_name, actual_label))
}



.determine_shap_waterfall_plot_dimensions <- function(
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


# GeomSHAPWaterfall ------------------------------------------------------------
# Placeholder to prevent NOTES if ggplot2 is not installed.
GeomSHAPWaterfall <- NULL
if (rlang::is_installed("ggplot2")) {
  GeomSHAPWaterfall <- ggplot2::ggproto(
    "GeomPolygon",
    ggplot2::Geom,
    required_aes = c("x", "xend", "y", "ymin", "ymax"),
    default_aes = ggplot2::aes(
      colour = NA,
      fill = "grey35",
      linewidth = 0.5,
      linetype = 1,
      alpha = NA
    ),
    draw_key = ggplot2::draw_key_polygon,
    draw_panel = function(
    data,
    panel_params,
    coord,
    lineend = "butt", linejoin = "round", linemitre = 10
    ) {
      # Compute coordinates based on data.
      coords <- coord$transform(data, panel_params)
      
      # Instantiate parameters to feed to grid::polygonGrob. These vectors are 
      # sufficiently large to hold all polygons with taper.
      x <- y <- numeric(nrow(coords) * 5L)
      id <- integer(nrow(coords) * 5L)
      
      # Iterate over features.
      idx_offset <- 0L
      for (ii in seq_len(nrow(coords))) {
        # Set up coordinates.
        x_start = coords$x[ii]
        x_end = coords$xend[ii]
        y_down <- coords$ymin[ii]
        y_up <- coords$ymax[ii]
        
        if (abs(x_start - x_end) > 0.05) {
          # Add taper if there is sufficient room on the plot.
          x_mid <- ifelse(x_start < x_end, x_end - 0.05, x_end + 0.05)
          y_mid <- (y_up + y_down) * 0.5
          
          # Define coordinates for polygon.
          idx <- idx_offset + (1L:5L)
          x[idx] <- c(x_start, x_start, x_mid, x_end, x_mid)
          y[idx] <- c(y_down, y_up, y_up, y_mid, y_down)
          
        } else {
          # Avoid taper if there is not sufficient room.
          idx <- idx_offset + (1L:4L)
          x[idx] <- c(x_start, x_start, x_end, x_end)
          y[idx] <- c(y_down, y_up, y_up, y_down)
        }
        
        # Set grouping.
        id[idx] <- ii
        
        # Update offset.
        idx_offset <- idx_offset + length(idx)
      }
      
      # Select elements which are correctly set.
      valid_idx <- id > 0L
      
      return(grid::polygonGrob(
        x[valid_idx],
        y[valid_idx],
        id = id[valid_idx],
        default.units = "native",
        gp = grid::gpar(
          col = coords$colour,
          fill = ggplot2::fill_alpha(coords$fill, coords$alpha),
          lwd = coords$linewidth * ggplot2::.pt,
          lty = coords$linetype,
          lineend = lineend,
          linejoin = linejoin,
          linemitre = linemitre
        )
      ))
    }
  )
}



geom_waterfall_shap <- function(
    mapping = NULL,
    data = NULL,
    stat = "identity",
    position = "identity",
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    ...
) {
  if (is.null(GeomSHAPWaterfall)) {
    GeomSHAPWaterfall <- ggplot2::GeomBlank
    ..warning_geom_not_available("GeomSHAPWaterfall")
  }
  
  ggplot2::layer(
    geom = GeomSHAPWaterfall,
    mapping = mapping,
    data = data, 
    stat = stat, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
