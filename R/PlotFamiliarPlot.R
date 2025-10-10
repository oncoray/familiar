#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


as_familiar_plot <- function(
    p = NULL,
    g = NULL,
    layout
) {
  fam_plot <- methods::new("familiarPlot")
  
  if (is.null(g) && ggplot2::is_ggplot(p)) {
    # Get gtable from ggplot2 object.
    fam_plot@gtable <- .convert_to_grob(p)
    
  } else if (gtable::is.gtable(g)) {
    fam_plot@gtable <- g
  }
  
  # Set column and row id.
  fam_plot@row_id <- layout$row_id
  fam_plot@col_id <- layout$col_id
  
  # Add global plot elements.
  fam_plot@global_elements <- .extract_global_plot_elements(g)
  
  return(fam_plot)
}



.extract_global_plot_elements <- function(g) {
  
  element_list <- list()
  
  # Export list of elements.
  if (is.null(g)) return(element_list)
  
  # Find names of all existing elements.
  elements_names <- g$layout$name
  
  # Set names of all global elements.
  global_elements <- c(
    .all_gtable_guide_names(),
    .all_gtable_strip_x_names(),
    .all_gtable_strip_y_names(),
    .all_gtable_label_x_names(),
    .all_gtable_label_y_names(),
    .all_gtable_title_names()
  )
  
  # Identify which global elements are present.
  present_elements <- elements_names[sapply(
    elements_names, 
    startswith_any, 
    prefix = global_elements
  )]
  if (length(present_elements) == 0L) return(element_list)
  
  # Add elements that are present in the table and are related to the global
  # elements.
  for (present_element in present_elements) {
    element_list[[present_element]] <- .gtable_extract(
      g = g,
      element = present_element,
      partial_match = FALSE,
      drop_empty = TRUE
    )
  }
  
  # Remove empty plot elements.
  element_list <- element_list[!sapply(element_list, is.null)]
  
  return(element_list)
}



.create_placeholder_figure <- function(
    template_figure_row,
    template_figure_col,
    row_id,
    col_id
) {
  # Creates placeholder for missing figures in faceted panel, e.g. because no
  # data were present.
  if (
    !is(template_figure_row, "familiarPlot") ||
    !is(template_figure_col, "familiarPlot")
  ) {
    ..error_reached_unreachable_code("both templates should be familiarPlot objects.")
  }
  browser()
  # Use the row item as the initial template.
  figure <- template_figure_row
  
  # Ensure that panels are removed.
  figure@remove_panel <- TRUE
  
  # Drop global plot elements -- we will extract these again later.
  figure@global_elements <- NULL
  
  # We need to update elements from the column template, e.g. axis-t, xlab-t,
  # and strip-t-1.
  col_element_names <- c(
    .all_gtable_strip_x_names(),
    .all_gtable_label_x_names(),
    .all_gtable_axis_x_names()
  )
  
  # Find names of all existing elements.
  updatable_elements <- figure@gtable$layout$name
  updatable_elements <- updatable_elements[sapply(
    updatable_elements, 
    startswith_any, 
    prefix = col_element_names
  )]
  
  for (update_element in updatable_elements) {
    figure@gtable <- .gtable_insert(
      g = figure@gtable,
      g_new = .gtable_extract(template_figure_col, element = update_element),
      where = c("replace", update_element)
    )
  }
  
  # Update row_id and col_id.
  figure@row_id <- row_id
  figure@col_id <- col_id
  
  # Add global elements again.
  figure@global_elements <- .extract_global_plot_elements(figure@gtable)
  
  return(figure)
}



.set_figure_element_removal <- function(
  object,
  top_row_id,
  bottow_row_id,
  left_col_id,
  right_col_id,
  x_text_shared,
  y_text_shared,
  x_label_shared,
  y_label_shared
) {
  is_top_row <- object@row_id == top_row_id
  is_bottom_row <- object@row_id == bottow_row_id
  is_left_col <- object@col_id == left_col_id
  is_right_col <- object@col_id == right_col_id
  
  # Facet strips
  if (!is_top_row) {
    object@remove_strip_x <- TRUE
  }
  if (!is_right_col) {
    object@remove_strip_y <- TRUE
  }
  
  # x-axis text. "individual" and "FALSE" do not lead to removal.
  if (x_text_shared %in% c("overall", "TRUE")) {
    object@remove_axis_text_x <- TRUE
    
  } else if (x_text_shared == "column" && !is_bottom_row) {
    object@remove_axis_text_x <- TRUE
  }
  
  # x-axis label. "individual" and "FALSE" do not lead to removal.
  if (x_label_shared %in% c("overall", "TRUE")) {
    object@remove_axis_label_x <- TRUE
    
  } else if (x_label_shared == "column" && !is_bottom_row) {
    object@remove_axis_label_x <- TRUE
  }
  
  # y-axis text. "individual" and "FALSE" do not lead to removal.
  if (y_text_shared %in% c("overall", "TRUE")) {
    object@remove_axis_text_y <- TRUE
    
  } else if (y_text_shared == "row" && !is_left_col) {
    object@remove_axis_text_y <- TRUE
  }
  
  # x-axis label. "individual" and "FALSE" do not lead to removal.
  if (y_label_shared %in% c("overall", "TRUE")) {
    object@remove_axis_label_y <- TRUE
    
  } else if (y_label_shared == "row" && !is_left_col) {
    object@remove_axis_label_y <- TRUE
  }
  
  return(object)
}



.remove_figure_elements <- function(
  object,
  replace_by_zero_grob = FALSE
) {
  
  # Always remove guide and title.
  base_elements <- c(
    .all_gtable_guide_names(),
    .all_gtable_title_names()
  )
  
  # First determine which stuff can be removed, and then match any elements in
  # the gtable.
  if (object@remove_strip_x) {
    base_elements <- c(base_elements, .all_gtable_strip_x_names())
  }
  if (object@remove_strip_y) {
    base_elements <- c(base_elements, .all_gtable_strip_y_names())
  }
  if (object@remove_axis_text_x) {
    base_elements <- c(base_elements, .all_gtable_axis_x_names())
  }
  if (object@remove_axis_text_y) {
    base_elements <- c(base_elements, .all_gtable_axis_y_names())
  }
  if (object@remove_axis_label_x) {
    base_elements <- c(base_elements, .all_gtable_label_x_names())
  }
  if (object@remove_axis_label_y) {
    base_elements <- c(base_elements, .all_gtable_label_y_names())
  }
  if (object@remove_panel) {
    base_elements <- c(base_elements, .all_gtable_panel_names())
  }
  
  removable_elements <- object@gtable$layout$name
  removable_elements <- removable_elements[sapply(
    removable_elements, 
    startswith_any, 
    prefix = base_elements
  )]
  
  # Iterate to remove or replace with zeroGrob.
  zeroGrob <- ggplot2::zeroGrob()
  for (removable_element in removable_elements) {
    if (replace_by_zero_grob) {
      object@gtable <- .gtable_insert(
        g = object@gtable,
        g_new = list(zeroGrob),
        where = c("replace", removable_element)
      )
      
    } else {
      object@gtable <- .gtable_remove(
        g = object@gtable,
        removed_element = removable_element
      )
    }
  }
  # TODO: check that space for zero-grobs is correctly set to 0.
  # TODO: check that subtitle, strip_x are actually removed.
  browser()
  # Update widths and heights.
  object@gtable <- .gtable_update_layout(g = object@gtable)
  
  return(object)
}
