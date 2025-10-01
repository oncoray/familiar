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
