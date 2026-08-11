#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL



# plot_shap_force (generic) ----------------------------------------------------

#' @title Create SHAP force plot
#'
#' @description This method creates plots that show stacked SHAP force values
#'   obtained from the data stored in a familiarCollection object.
#'
#' @param dir_path (*optional*) Path to the directory where created SHAP force
#'   plots are saved to. Output is saved in the `explanation` subdirectory. If
#'   `NULL` no figures are saved, but are returned instead.
#' @param discrete_palette (*optional*) Discrete palette used to colour the
#'   elements of force plots. `familiar` has a default palette. Other palettes
#'   are supported by the `paletteer` package, `grDevices::palette.pals()`
#'   (requires R >= 4.0.0), `grDevices::hcl.pals()` (requires R >= 3.6.0). You
#' may also specify your own palette by providing a vector of colour names
#' listed by `grDevices::colors()` or through hexadecimal RGB strings.
#' @param highlight_feature (*optional*) Name of one or more features that
#'   should be highlighted in the force plot.
#' @param sample_order (*optional*) Ordering of samples, one of:
#'
#'   * `prediction`: samples are ordered by increasing predicted value. Sample
#'   order between facets may differ.
#'
#'   * `original`: samples retain the original ordering. Sample order between
#'   facets is consistent.
#' 
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
#'   Available splitting variables are: `vimp_method`, `learner`, `data_set`,
#'   `evaluation_time` (survival outcome only) and `positive_class` (categorical
#'   outcomes). The default for is to facet by `evaluation_time` or
#'   `positive_class`, and split by `vimp_method`, `learner` and `data_set`.
#'   `color_by` is not used.
#'
#'   Labelling methods such as `set_vimp_method_names` or `set_learner_names`
#'   can be applied to the `familiarCollection` object to update labels, and
#'   order the output in the figure.
#'
#' @return `NULL` or list of plot objects, if `dir_path` is `NULL`.
#'
#' @exportMethod plot_shap_force
#' @md
#' @rdname plot_shap_force-methods
setGeneric(
  "plot_shap_force",
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
    discrete_palette = NULL,
    x_label = waiver(),
    x_label_shared = "column",
    y_label = waiver(),
    y_label_shared = "row",
    legend_label = waiver(),
    plot_title = waiver(),
    plot_sub_title = waiver(),
    caption = NULL,
    y_range = NULL,
    y_n_breaks = 5L,
    y_breaks = NULL,
    highlight_feature = NULL,
    sample_order = "prediction",
    width = waiver(),
    height = waiver(),
    units = waiver(),
    export_collection = FALSE,
    ...
  ) {
    standardGeneric("plot_shap_force")
  }
)



# plot_shap_force (general) ------------------------------------------------

#' @rdname plot_shap_force-methods
setMethod(
  "plot_shap_force",
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
    discrete_palette = NULL,
    x_label = waiver(),
    x_label_shared = "column",
    y_label = waiver(),
    y_label_shared = "row",
    legend_label = waiver(),
    plot_title = waiver(),
    plot_sub_title = waiver(),
    caption = NULL,
    y_range = NULL,
    y_n_breaks = 5L,
    y_breaks = NULL,
    highlight_feature = NULL,
    sample_order = "prediction",
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
      plot_shap_force,
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
        "discrete_palette" = discrete_palette,
        "x_label" = x_label,
        "x_label_shared" = x_label_shared,
        "y_label" = y_label,
        "y_label_shared" = y_label_shared,
        "legend_label" = legend_label,
        "plot_title" = plot_title,
        "plot_sub_title" = plot_sub_title,
        "caption" = caption,
        "y_range" = y_range,
        "y_n_breaks" = y_n_breaks,
        "y_breaks" = y_breaks,
        "highlight_feature" = highlight_feature,
        "sample_order" = sample_order,
        "width" = width,
        "height" = height,
        "units" = units,
        "export_collection" = export_collection
      )
    ))
  }
)



# plot_shap_force (collection) -----------------------------------------------

#' @rdname plot_shap_force-methods
setMethod(
  "plot_shap_force",
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
    discrete_palette = NULL,
    x_label = waiver(),
    x_label_shared = "column",
    y_label = waiver(),
    y_label_shared = "row",
    legend_label = waiver(),
    plot_title = waiver(),
    plot_sub_title = waiver(),
    caption = NULL,
    y_range = NULL,
    y_n_breaks = 5L,
    y_breaks = NULL,
    highlight_feature = NULL,
    sample_order = "prediction",
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
          "plot_shap_force: list of data elements contains unmerged elements."
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
      # Split by vimp_method, learner.
      split_by <- c("vimp_method", "learner", "data_set")
      facet_by <- additional_variable
    }
    
    all_variables <- c("vimp_method", "learner", "data_set", additional_variable)
    
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
      x_label <- "sample"
    }
    
    # y_label
    if (is.waive(y_label)) {
      y_label <- "predicted value"
    }
    
    # x_label_shared
    if (!is.waive(x_label_shared)) {
      .check_input_plot_args(x_label_shared = x_label_shared)
    } else {
      x_label_shared <- "column"
    }
    
    # y_label_shared
    if (!is.waive(y_label_shared)) {
      .check_input_plot_args(y_label_shared = y_label_shared)
    } else {
      y_label_shared <- "row"
    }
    
    .check_input_plot_args(
      facet_wrap_cols = facet_wrap_cols,
      x_label = x_label,
      y_label = y_label,
      plot_title = plot_title,
      plot_sub_title = plot_sub_title,
      caption = caption
    )
    
    # Check that highlight_feature appears as a feature.
    if (!is.null(highlight_feature)) {
      features_in_data <- highlight_feature %in% levels(x@data$feature_name)
      
      if (!all(features_in_data)) {
        ..warning(
          paste0(
            "Not all features to highlight for SHAP force plots were found in the dataset. Missing: ",
            paste_s(highlight_feature[!features_in_data])
          )
        )
      }
    }
    
    # sample_order
    .check_parameter_value_is_valid(
      x = sample_order, 
      var_name = "sample_order", 
      values = c("prediction", "original")
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
      
      if (is.waive(plot_title)) plot_title <- "SHAP force"
      
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
      
      p <- .plot_shap_force_plot(
        x = x_split[[ii]],
        facet_by = facet_by,
        facet_wrap_cols = facet_wrap_cols,
        ggtheme = ggtheme,
        discrete_palette = discrete_palette,
        x_label = x_label,
        x_label_shared = x_label_shared,
        y_label = y_label,
        y_label_shared = y_label_shared,
        legend_label = legend_label,
        plot_title = plot_title,
        plot_sub_title = plot_sub_title,
        caption = caption,
        y_range = y_range,
        y_n_breaks = y_n_breaks,
        y_breaks = y_breaks,
        highlight_feature = highlight_feature,
        sample_order = sample_order
      )
      
      # Check empty output
      if (is.null(p)) next
      
      # Draw figure.
      if (draw) .draw_plot(plot_or_grob = p)
      
      # Save and export
      if (!is.null(dir_path)) {
        # Obtain decent default values for the plot.
        def_plot_dims <- .determine_shap_force_plot_dimensions(
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
              "subtype" = "shap_force",
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



.plot_shap_force_plot <- function(
    x,
    facet_by,
    facet_wrap_cols,
    ggtheme,
    discrete_palette,
    x_label,
    x_label_shared,
    y_label,
    y_label_shared,
    legend_label,
    plot_title,
    plot_sub_title,
    caption,
    y_range,
    y_n_breaks,
    y_breaks,
    highlight_feature,
    sample_order
) {
  # Suppress NOTES due to non-standard evaluation in data.table
  shap_value <- prediction <- NULL
  
  # Split by facet. This generates a list of data splits with faceting
  # information that allows for positioning.
  plot_layout_table <- .get_plot_layout_table(
    x = x,
    facet_by = facet_by,
    facet_wrap_cols = facet_wrap_cols
  )
  
  # Set the y-range, as this should be fixed across facets.
  if (is.null(y_range)) {
    # Find the correct y-range
    y_range_data <- x[
      ,
      list(
        "y_min" = prediction - sum(pmax(shap_value, 0.0)),
        "y_max" = prediction - sum(pmin(shap_value, 0.0))
      ),
      by = c("sample_id", facet_by)
    ]
    y_range <- c(min(y_range_data$y_min, na.rm = TRUE), max(y_range_data$y_max, na.rm = TRUE))
    if (y_range[1L] == y_range[2L]) y_range <- y_range + c(-0.1, 0.1)
  }

  .check_input_plot_args(y_range = y_range)
  
  # x_breaks
  if (is.null(y_breaks)) {
    .check_input_plot_args(
      y_range = y_range,
      y_n_breaks = y_n_breaks
    )
    
    # Create breaks and update x_range
    y_breaks <- labeling::extended(
      m = y_n_breaks,
      dmin = y_range[1L],
      dmax = y_range[2L],
      only.loose = TRUE
    )
    
    y_range <- c(
      head(y_breaks, n = 1L),
      tail(y_breaks, n = 1L)
    )
    
  } else {
    .check_input_plot_args(y_breaks = y_breaks)
  }
  
  # Split data into facets. This is done by row.
  data_facet_list <- .split_data_by_plot_facet(
    x = x,
    plot_layout_table = plot_layout_table
  )
  
  # Used for ordering of composite figures.
  layout_split <- split(
    plot_layout_table,
    by = c("col_id", "row_id"),
    sorted = TRUE
  )
  
  # Placeholders for plots.
  figure_list <- list()
  extracted_element_list <- list()
  
  # Iterate over facets
  for (ii in names(layout_split)) {
    # Create calibration plot.
    p_shap_force <- .create_shap_force_plot(
      x = data_facet_list[[ii]],
      facet_by = facet_by,
      facet_wrap_cols = facet_wrap_cols,
      ggtheme = ggtheme,
      discrete_palette = discrete_palette,
      x_label = x_label,
      y_label = y_label,
      legend_label = legend_label,
      plot_title = plot_title,
      plot_sub_title = plot_sub_title,
      caption = caption,
      y_range = y_range,
      y_breaks = y_breaks,
      highlight_feature = highlight_feature,
      sample_order = sample_order
    )
    
    # Rename plot elements.
    g_shap_force <- .rename_plot_grobs(
      g = .convert_to_grob(p_shap_force),
      extension = "main"
    )
    if (!gtable::is.gtable(g_shap_force)) next
    
    # Attach to figure list.
    figure_list[[paste0(layout_split[[ii]]$row_id, ".", layout_split[[ii]]$col_id)]] <- as_familiar_plot(
      g = g_shap_force,
      layout = layout_split[[ii]]
    )
  }
  
  # Compose the final figure. Magic.
  g <- .compose_figure(
    figure_list = figure_list,
    plot_layout_table = plot_layout_table,
    x_text_shared = x_label_shared,
    x_label_shared = x_label_shared,
    y_text_shared = y_label_shared,
    y_label_shared = y_label_shared,
    facet_wrap_cols = facet_wrap_cols,
    ggtheme = ggtheme
  )
  
  return(g)
}



.create_shap_force_plot <- function(
    x,
    facet_by,
    facet_wrap_cols,
    ggtheme,
    discrete_palette,
    x_label,
    y_label,
    legend_label,
    plot_title,
    plot_sub_title,
    caption,
    y_range,
    y_breaks,
    highlight_feature,
    sample_order
) {
  # Suppress NOTES due to non-standard evaluation in data.table
  shap_value <- vimp <- feature_value <- feature_name <- NULL
  feature_label <- prediction <- y <- label_text <- NULL
  
  # The force plot has two axes: a sample axis, and a prediction axis. By
  # default, samples are sorted according to the prediction value within the
  # facet. For each sample, the marginal feature value contributions to the
  # prediction are sorted by the absolute value of the contribution.
  
  # Set sample order in each facet.
  if (sample_order == "prediction") {
    
    # Order samples by increasing predicted value.
    prediction_table <- unique(x[, mget(c("prediction", "sample_id"))])
    prediction_table[, "sample_order" := order(order(prediction, decreasing = FALSE))]
    
    x <- merge(
      x = x,
      y = prediction_table[, "prediction" := NULL],
      by = "sample_id"
    )
    
  } else if (sample_order == "original") {
    sample_table <- unique(x[, mget(c("sample_id"))])
    sample_table[, "sample_order" := .I]
    
    x <- merge(
      x = x,
      y = sample_table,
      by = "sample_id"
    )
    
  } else {
    ..error_reached_unreachable_code(paste0("encountered invalid value for sample_order: ", sample_order))
  }
  
  # Update start and end positions for force elements.
  x[
    ,
    (c("shap_start", "shap_end")) := ..set_shap_force_positions(shap_value, prediction),
    by = c(facet_by, "sample_id")
  ]

  
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
  discrete_palette <- .get_palette(
    x = discrete_palette, 
    palette_type = "qualitative",
    n = 2L
  )
  x[, "shap_positive" := shap_value >= 0.0]
  x[, "shap_highlight" := feature_name %in% highlight_feature]
  
  # Set up basic force plot.
  p <- ggplot2::ggplot(data = x)
  p <- p + ggtheme
  p <- p + geom_fam_force_shap(
    data = x,
    mapping = ggplot2::aes(
      x = !!sym("sample_order"),
      xmin = !!sym("sample_order") - 0.4,
      xmax = !!sym("sample_order") + 0.4,
      y = !!sym("prediction"),
      ymin = !!sym("shap_start"),
      ymax = !!sym("shap_end"),
      fill = !!sym("shap_positive"),
      colour = !!sym("shap_positive"),
      alpha = !!sym("shap_highlight")
    )
  )
  
  # Set alpha values.
  p <- p + ggplot2::scale_alpha_manual(
    values = c("TRUE" = 1.0, "FALSE" = 0.4),
    guide = "none"
  )
  
  # Set colour and fill.
  p <- p + ggplot2::scale_fill_manual(
    values = c("FALSE" = discrete_palette[1L], "TRUE" = discrete_palette[2L]),
    guide = "none",
    aesthetics = c("colour", "fill")
  )
  
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
  
  # Set breaks and  limits on the x and y-axis
  # p <- p + ggplot2::scale_x_continuous(breaks = x_breaks)
  p <- p + ggplot2::scale_y_continuous(breaks = y_breaks, limits = y_range)
  
  return(p)
}



..set_shap_force_positions <- function(x, predictions) {
  # Prevent notes.
  feature_value <- density <- y_offset <- NULL
  
  # Initialise.
  x_start <- x_end <- numeric(length(x))

  # Set feature order based on the absolute shap value.
  feature_order <- order(abs(x), decreasing = TRUE)

  # Initialise positions.
  previous_end_pos <- previous_end_neg <- utils::head(predictions, n = 1L)
  
  # We fill start and end positions starting with the most
  # important feature first.
  for (ii in feature_order) {
    if (x[ii] >= 0.0){
      x_start[ii] <- previous_end_pos
      previous_end_pos <- x_end[ii] <- previous_end_pos - x[ii]
      
    } else {
      x_start[ii] <- previous_end_neg
      previous_end_neg <- x_end[ii] <- previous_end_neg - x[ii]
    }
  }

  return(list(
    "shap_start" = x_start,
    "shap_end" = x_end
  ))
}

# 
# 
# ..set_shap_waterfall_feature_name_value <- function(
#     feature_name,
#     feature_value, 
#     feature_label
# ) {
#   actual_label <- feature_label
#   actual_label[is.na(feature_label)] <- signif(feature_value[is.na(feature_label)], 3L)
#   
#   return(paste0(feature_name, ": ", actual_label))
# }



.determine_shap_force_plot_dimensions <- function(
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

# GeomSHAPForce ----------------------------------------------------------------

# Placeholder to prevent NOTES if ggplot2 is not installed.
GeomSHAPForce <- NULL
if (rlang::is_installed("ggplot2")) {
  GeomSHAPForce <- ggplot2::ggproto(
    "GeomPolygon",
    ggplot2::Geom,
    required_aes = c("x", "xmin", "xmax", "y", "ymin", "ymax"),
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
      lineend = "butt",
      linejoin = "round",
      linemitre = 10
    ) {
      # Compute coordinates based on data.
      coords <- coord$transform(data, panel_params)

      # Instantiate parameters to feed to grid::polygonGrob. These vectors are 
      # sufficiently large to hold all polygons with taper.
      x <- y <- numeric(nrow(coords) * 6L)
      id <- integer(nrow(coords) * 6L)
      
      # Iterate over features.
      idx_offset <- 0L
      for (ii in seq_len(nrow(coords))) {
        # x is over the samples; y is over the predictions.
        x_min <- coords$xmin[ii]
        x_max <- coords$xmax[ii]
        y_min <- coords$ymin[ii]
        y_max <- coords$ymax[ii]
        
        idx <- idx_offset + (1L:6L)
        
        if (y_max <= y_min) {
          y_max_flank <- y_max - 0.01
          y_min_flank <- y_min - 0.01
          
        } else {
          y_max_flank <- y_max + 0.01
          y_min_flank <- y_min + 0.01
        }
        
        x_mid <- (x_min + x_max) / 2.0
        
        x[idx] <- c(x_min, x_mid, x_max, x_max, x_mid, x_min)
        y[idx] <- c(y_max_flank, y_max, y_max_flank, y_min_flank, y_min, y_min_flank)
        
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



geom_fam_force_shap <- function(
    mapping = NULL,
    data = NULL,
    stat = "identity",
    position = "identity",
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    ...
) {
  if (is.null(GeomSHAPForce)) {
    GeomSHAPForce <- ggplot2::GeomBlank
    ..warning_geom_not_available("GeomSHAPForce")
  }
  
  ggplot2::layer(
    geom = GeomSHAPForce,
    mapping = mapping,
    data = data, 
    stat = stat, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
