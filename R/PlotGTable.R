.all_gtable_guide_names <- function(type = "all") {
  if (type == "all") {
    return(c(
      "guide-box-right", "guide-box-left",
      "guide-box-top", "guide-box-bottom", "guide-box-inside"
    ))
    
  } else if (type == "right") {
    return("guide-box-right")
    
  } else if (type == "left") {
    return("guide-box-left")
    
  } else if (type == "top") {
    return("guide-box-top")
    
  } else if (type == "bottom") {
    return("guide-box-bottom")
    
  } else if (type == "inside") {
    return("guide-box-inside")
    
  } else {
    ..error_reached_unreachable_code(paste0("unknown type: ", type))
  }
}



.all_gtable_title_names <- function(type = "all") {
  if (type == "all") {
    return(c("title", "subtitle", "caption"))
    
  } else if (type == "title") {
    return(c("title", "subtitle"))
    
  } else if (type == "caption") {
    return("caption")
    
  } else {
    ..error_reached_unreachable_code(paste0("unknown type: ", type))
  }
}



.all_gtable_panel_names <- function() {
  return("panel")
}



.all_gtable_strip_x_names <- function() {
  return(c("strip-t", "strip-b"))
}



.all_gtable_strip_y_names <- function() {
  return(c("strip-l", "strip-r"))
}



.all_gtable_label_names <- function(type = "all") {
  if (type == "all") {
    return(c(.all_gtable_label_x_names(), .all_gtable_label_y_names()))
    
  } else if (type == "right") {
    return(.all_gtable_label_y_names("right"))
    
  } else if (type == "left") {
    return(.all_gtable_label_y_names("left"))
    
  } else if (type == "top") {
    return(.all_gtable_label_x_names("top"))
    
  } else if (type == "bottom") {
    return(.all_gtable_label_x_names("bottom"))
    
  } else {
    ..error_reached_unreachable_code(paste0("unknown type: ", type))
  }
}



.all_gtable_label_x_names <- function(type = "all") {
  if (type == "all") {
    return(c("xlab-b", "xlab-t"))
    
  } else if (type == "top") {
    return("xlab-t")
    
  } else if (type == "bottom") {
    return("xlab-b")
    
  } else {
    ..error_reached_unreachable_code(paste0("unknown type: ", type))
  }
}



.all_gtable_label_y_names <- function(type = "all") {
  if (type == "all") {
    return(c("ylab-l", "ylab-r"))
    
  } else if (type == "left") {
    return("ylab-l")
    
  } else if (type == "right") {
    return("ylab-r")
    
  } else {
    ..error_reached_unreachable_code(paste0("unknown type: ", type))
  }
}



.all_gtable_axis_names <- function(type = "all") {
  if (type == "all") {
    return(c(.all_gtable_axis_x_names(), .all_gtable_axis_y_names()))
    
  } else if (type == "right") {
    return(.all_gtable_axis_y_names("right"))
    
  } else if (type == "left") {
    return(.all_gtable_axis_y_names("left"))
    
  } else if (type == "top") {
    return(.all_gtable_axis_x_names("top"))
    
  } else if (type == "bottom") {
    return(.all_gtable_axis_x_names("bottom"))
    
  } else {
    ..error_reached_unreachable_code(paste0("unknown type: ", type))
  }
}



.all_gtable_axis_x_names <- function(type = "all") {
  if (type == "all") {
    return(c("axis-b", "axis-t"))
    
  } else if (type == "top") {
    return("axis-t")
    
  } else if (type == "bottom") {
    return("axis-b")
    
  } else {
    ..error_reached_unreachable_code(paste0("unknown type: ", type))
  }
}



.all_gtable_axis_y_names <- function(type = "all") {
  if (type == "all") {
    return(c("axis-l", "axis-r"))
    
  } else if (type == "left") {
    return("axis-l")
    
  } else if (type == "right") {
    return("axis-r")
    
  } else {
    ..error_reached_unreachable_code(paste0("unknown type: ", type))
  }
}



.gtable_element_in_layout <- function(g, element, partial_match = FALSE) {
  if (partial_match) {
    return(any(grepl(pattern = element, x = g$layout$name)))
  } else {
    return(any(element %in% g$layout$name))
  }
}



.gtable_get_position <- function(
    g,
    element, 
    where = NULL,
    partial_match = FALSE, 
    allow_multiple = FALSE
) {
  # Find position.
  if (partial_match) {
    # Based on partial matching.
    position <- g$layout[
      grepl(pattern = element, x = g$layout$name),
      c("t", "l", "b", "r")
    ]
    
  } else {
    # Based on exact matching.
    position <- g$layout[
      g$layout$name == element,
      c("t", "l", "b", "r")
    ]
  }

  if (nrow(position) == 0L) {
    ..error_reached_unreachable_code(
      ".gtable_get_position: element not found in layout table."
    )
  }

  if (is.null(where)) {
    if (nrow(position) != 1L) {
      if (allow_multiple) {
        browser()
      } else {
        ..error_reached_unreachable_code(
          paste0("Multiple matches, please set where attribute.")
        )
      }
    }
  } else if (where == "top") {
    # Select the uppermost element.
    position <- position[position$t == min(position$t), ][1L, ]
    
  } else if (where == "bottom") {
    # Select the bottommost element.
    position <- position[position$b == max(position$b), ][1L, ]
    
  } else if (where == "left") {
    # Select the leftmost element
    position <- position[position$l == min(position$l), ][1L, ]
    
  } else if (where == "right") {
    # Select the rightmost element
    position <- position[position$r == max(position$r), ][1L, ]
    
  } else {
    ..error_value_not_allowed(
      x = where,
      var_name = "where",
      values = c("top", "bottom", "left", "right")
    )
  }

  # Return as array.
  position <- simplify2array(position)

  return(position)
}



.gtable_extract_grob <- function(
    g,
    element
) {
  grob_id <- which(g$layout$name == element)
  
  if (length(grob_id) == 0L) {
    ..error_reached_unreachable_code("element could not be found in gtable")
    
  } else if (length(grob_id) > 1L) {
    ..error_reached_unreachable_code("more than one element was found in gtable")
  }
  
  return(g$grobs[[grob_id]])
}



.gtable_insert_spacer <- function(
    g,
    position,
    width = NULL,
    height = NULL,
    make_space = FALSE,
    name = NULL
) {
  # Generate name.
  if (is.null(name)) name <- "spacer"
  
  # Create empty grob.
  spacer <- grid::grob(cl = "spacerGrob", name = name)
  
  # Set width and height.
  spacer$widths <- grid::unit(0.0, "cm")
  if (!is.null(width)) spacer$widths <- width
  spacer$heights <- grid::unit(0.0, "cm")
  if (!is.null(height)) spacer$heights <- height
  
  # If make_space is TRUE, insert a column or row at the given position, and then
  # place the spacer in the new column or row. Otherwise the spacer is inserted
  # in place.
  if (make_space) {
    if (!is.null(width)) {
      g <- gtable::gtable_add_cols(
        x = g,
        widths = width,
        pos = position[["l"]]
      )
    }
    if (!is.null(height)) {
      g <- gtable::gtable_add_rows(
        x = g,
        heights = height,
        pos = position[["t"]]
      )
    }
  }
  
  g <- gtable::gtable_add_grob(
    g,
    grobs = spacer,
    t = position[["t"]],
    b = position[["b"]],
    l = position[["l"]],
    r = position[["r"]],
    name = name
  )
  
  return(g)
}



.gtable_remove <- function(
    g,
    removed_element = NULL,
    trim = FALSE
) {
  matched_elements <- !(g$layout$name %in% removed_element)
  
  # gtable::gtable_filter uses partial matching, which leads to issues with,
  # e.g., "title" and "subtitle" elements.
  g$layout <- g$layout[matched_elements, , drop = FALSE]
  g$grobs <- g$grobs[matched_elements]
  
  if (trim) g <- gtable::gtable_trim(g)
  
  return(g)
}



.gtable_insert <- function(
    g,
    g_new,
    where,
    grob_name = NULL,
    spacer = NULL
) {
  # Intended for inserting elements that stretch multiple along_elements. It can
  # also be used for inserting elements directly (without along_elements) and/or
  # replacing existing elements (attempt_replace=TRUE)
  if (length(g_new) == 0L) {
    return(g)
  }
  
  if (where[1L] == "at") {
    position <- c(
      "t" = as.integer(where[2L]),
      "l" = as.integer(where[3L]),
      "b" = as.integer(where[4L]),
      "r" = as.integer(where[5L])
    )
    
    g <- ..gtable_insert_at(
      g = g,
      g_new = g_new,
      grob_name = grob_name,
      position = position
    )
    
  } else if (where[1L] == "replace") {
    g <- ..gtable_insert_replace(
      g = g,
      g_new = g_new,
      ref_element = where[2L]
    )
    
  } else if (where[1L] == "intersect") {
    g <- ..gtable_insert_intersect(
      g = g,
      g_new = g_new,
      where_1 = where[2L],
      ref_element_1 = where[3L],
      where_2 = where[4L],
      ref_element_2 = where[5L],
      grob_name = grob_name
    )
    
  } else if (where[1L] == "left") {
    g <- ..gtable_insert_left(
      g = g,
      g_new = g_new,
      ref_element = where[2L],
      grob_name = grob_name,
      spacer = spacer
    )
    
  } else if (where[1L] == "right") {
    g <- ..gtable_insert_right(
      g = g,
      g_new = g_new,
      ref_element = where[2L],
      grob_name = grob_name,
      spacer = spacer
    )
    
  } else if (where[1L] == "above") {
    g <- ..gtable_insert_above(
      g = g,
      g_new = g_new,
      ref_element = where[2L],
      grob_name = grob_name,
      spacer = spacer
    )
    
  } else if (where[1L] == "below") {
    g <- ..gtable_insert_below(
      g = g,
      g_new = g_new,
      ref_element = where[2L],
      grob_name = grob_name,
      spacer = spacer
    )
    
  } else {
    ..error_reached_unreachable_code(
      "The first element of where should be one of replace, intersect, left, right, above or below."
    )
  }

  # Update widths and heights.
  g <- .gtable_update_layout(g = g)
  
  return(g)
}



..gtable_insert_at <- function(g, g_new, grob_name = NULL, position) {
  # Set clip.
  clip <- "on"
  if (is(g_new, "TableGrob") || is(g_new, "gtable")) clip <- g_new$layout$clip[1L]
  
  # Set name.
  name <- grob_name
  if (is.null(name) && (is(g_new, "TableGrob") || is(g_new, "gtable"))) name <- g_new$layout$name[1L]
  
  # Insert new element.
  g <- gtable::gtable_add_grob(
    g,
    grobs = g_new,
    t = position[["t"]],
    l = position[["l"]],
    b = position[["b"]],
    r = position[["r"]],
    name = name,
    clip = clip
  )
  
  return(g)
}



..gtable_insert_replace <- function(g, g_new, ref_element) {
  # Find position where new element is to be inserted.
  position <- .gtable_get_position(
    g = g, 
    element = ref_element
  )
  
  # Remove original element.
  g <- .gtable_remove(g = g, removed_element = ref_element)
  
  # Insert at position.
  g <- ..gtable_insert_at(
    g = g,
    g_new = g_new,
    grob_name = ref_element,
    position = position
  )
  
  return(g)
}



..gtable_insert_intersect <- function(
    g,
    g_new,
    where_1,
    ref_element_1,
    where_2,
    ref_element_2,
    grob_name = NULL
) {
  
  # Check that where is correctly specified.
  if (
    all(c(where_1, where_2) %in% c("above", "below")) ||
    all(c(where_1, where_2) %in% c("left", "right"))
  ) {
    ..error_reached_unreachable_code("where needs to be orthogonal positions")
  }
  
  elem_position <- integer(4L)
  names(elem_position) <- c("t", "l", "b", "r")
  
  # Get reference positions for each reference element.
  ref_position_1 <- .gtable_get_position(
    g = g, 
    element = ref_element_1
  )
  
  ref_position_2 <- .gtable_get_position(
    g = g, 
    element = ref_element_2
  )
  
  if (where_1 %in% c("above", "below")) {
    # Inherit l and r from the first reference element and t and b from the
    # second.
    elem_position[["l"]] <- ref_position_1[["l"]]
    elem_position[["r"]] <- ref_position_1[["r"]]
    elem_position[["t"]] <- ref_position_2[["t"]]
    elem_position[["b"]] <- ref_position_2[["b"]]
    
  } else {
    # Inherit t and b from the first reference element and l and r from the
    # second.
    elem_position[["t"]] <- ref_position_1[["t"]]
    elem_position[["b"]] <- ref_position_1[["b"]]
    elem_position[["l"]] <- ref_position_2[["l"]]
    elem_position[["r"]] <- ref_position_2[["r"]]
  }
  
  # Add element to g.
  g <- ..gtable_insert_at(
    g = g,
    g_new = g_new,
    grob_name = grob_name,
    position = elem_position
  )
  
  return(g)
}


..gtable_insert_below <- function(
    g,
    g_new, 
    ref_element,
    grob_name = NULL,
    spacer = NULL
) {
  # Create an offset.
  spacer_ref_offset <- elem_ref_offset <- integer(4L)
  names(spacer_ref_offset) <- c("t", "l", "b", "r")
  names(elem_ref_offset) <- c("t", "l", "b", "r")
  
  # Find position of reference element.
  ref_position <- .gtable_get_position(
    g = g, 
    element = ref_element
  )
  
  # Force the "top" of the reference position to the bottom, because we don't
  # need to copy the number of rows of the reference object.
  ref_position[["t"]] <- ref_position[["b"]]
  
  # Add space between the reference element and the element to be inserted.
  if (!is.null(spacer)) {
    g <- gtable::gtable_add_rows(
      g,
      heights = spacer,
      pos = ref_position[["b"]]
    )
    
    # Update offsets for spacer and new grob.
    spacer_ref_offset[["t"]] <- spacer_ref_offset[["b"]] <- 1L
    elem_ref_offset[["t"]] <- elem_ref_offset[["b"]] <- 1L
    
    # Add spacer.
    g <- .gtable_insert_spacer(
      g = g,
      position = ref_position + spacer_ref_offset,
      height = spacer
    )
  }
  
  # Add row below b or below spacer.
  g <- gtable::gtable_add_rows(
    g,
    heights = g_new$heights,
    pos = ref_position[["b"]] + elem_ref_offset[["b"]]
  )
  
  elem_ref_offset[["t"]] <- elem_ref_offset[["t"]] + 1L
  elem_ref_offset[["b"]] <- elem_ref_offset[["b"]] + length(g_new$heights)
  
  # Set new position
  new_position <- ref_position + elem_ref_offset

  # Add element to g.
  g <- ..gtable_insert_at(
    g = g,
    g_new = g_new,
    grob_name = grob_name,
    position = new_position
  )
  
  return(g)
}



..gtable_insert_above <- function(
    g,
    g_new, 
    ref_element,
    grob_name = NULL,
    spacer = NULL
) {
  # Create an offset.
  spacer_ref_offset <- elem_ref_offset <- integer(4L)
  names(spacer_ref_offset) <- c("t", "l", "b", "r")
  names(elem_ref_offset) <- c("t", "l", "b", "r")
  
  # Find position of reference element.
  ref_position <- .gtable_get_position(
    g = g, 
    element = ref_element
  )
  
  # Force the "top" of the reference position to the bottom, because we don't
  # need to copy the number of rows of the reference object.
  ref_position[["t"]] <- ref_position[["b"]]
  
  # Add space between the element that should be inserted and the reference
  # element.
  if (!is.null(spacer)) {
    g <- gtable::gtable_add_rows(
      g,
      heights = spacer,
      pos = ref_position[["t"]] - 1L
    )
  
    # Add spacer.
    g <- .gtable_insert_spacer(
      g = g,
      position = ref_position,
      height = spacer
    )
  }
  
  g <- gtable::gtable_add_rows(
    g,
    heights = g_new$heights,
    pos = ref_position[["t"]] - 1L
  )
  
  # Set new position
  new_position <- ref_position
  new_position[["b"]] <- new_position[["b"]] + length(g_new$heights) - 1L
  
  # Add element to g.
  g <- ..gtable_insert_at(
    g = g,
    g_new = g_new,
    grob_name = grob_name,
    position = new_position
  )
  
  return(g)
}



..gtable_insert_left <- function(
    g,
    g_new, 
    ref_element,
    grob_name = NULL,
    spacer = NULL
) {
  # Create an offset.
  spacer_ref_offset <- elem_ref_offset <- integer(4L)
  names(spacer_ref_offset) <- c("t", "l", "b", "r")
  names(elem_ref_offset) <- c("t", "l", "b", "r")
  
  # Find position of reference element.
  ref_position <- .gtable_get_position(
    g = g, 
    element = ref_element
  )
  
  # Force the "left" of the reference position to the right, because we don't
  # need to copy the number of columns of the reference object.
  ref_position[["l"]] <- ref_position[["r"]]
  
  # Add space between the element that should be inserted and the reference
  # element.
  if (!is.null(spacer)) {
    g <- gtable::gtable_add_cols(
      g,
      widths = spacer,
      pos = ref_position[["l"]] - 1L
    )
    
    # Add spacer.
    g <- .gtable_insert_spacer(
      g = g,
      position = ref_position,
      width = spacer
    )
  }
  
  # Make room for the grob that we want to insert.
  g <- gtable::gtable_add_cols(
    g,
    widths = g_new$widths,
    pos = ref_position[["l"]] - 1L
  )
  
  # Set new position
  new_position <- ref_position
  new_position[["r"]] <- new_position[["r"]] + length(g_new$widths) - 1L
  
  # Add element to g.
  g <- ..gtable_insert_at(
    g = g,
    g_new = g_new,
    grob_name = grob_name,
    position = new_position
  )
  
  return(g)
}



..gtable_insert_right <- function(
    g,
    g_new, 
    ref_element,
    grob_name = NULL,
    spacer = NULL
) {
  # Create an offset.
  spacer_ref_offset <- elem_ref_offset <- integer(4L)
  names(spacer_ref_offset) <- c("t", "l", "b", "r")
  names(elem_ref_offset) <- c("t", "l", "b", "r")
  
  # Find position of reference element.
  ref_position <- .gtable_get_position(
    g = g, 
    element = ref_element
  )
  
  # Force the "left" of the reference position to the right, because we don't
  # need to copy the number of rows of the reference object.
  ref_position[["l"]] <- ref_position[["r"]]
  
  # Add space between the reference element and the element to be inserted.
  if (!is.null(spacer)) {
    g <- gtable::gtable_add_cols(
      g,
      widths = spacer,
      pos = ref_position[["r"]]
    )
    
    # Update offsets for spacer and new grob.
    spacer_ref_offset[["l"]] <- spacer_ref_offset[["r"]] <- 1L
    elem_ref_offset[["l"]] <- elem_ref_offset[["r"]] <- 1L
    
    # Add spacer.
    g <- .gtable_insert_spacer(
      g = g,
      position = ref_position + spacer_ref_offset,
      width = spacer
    )
  }
  
  # Add row below b or below spacer.
  g <- gtable::gtable_add_cols(
    g,
    widths = g_new$widths,
    pos = ref_position[["r"]] + elem_ref_offset[["r"]]
  )
  
  elem_ref_offset[["l"]] <- elem_ref_offset[["l"]] + 1L
  elem_ref_offset[["r"]] <- elem_ref_offset[["r"]] + length(g_new$widths)
  
  # Set new position
  new_position <- ref_position + elem_ref_offset
  
  # Add element to g.
  g <- ..gtable_insert_at(
    g = g,
    g_new = g_new,
    grob_name = grob_name,
    position = new_position
  )
  
  return(g)
}





.gtable_rename_element <- function(
    g,
    old,
    new,
    partial_match = FALSE,
    allow_missing = FALSE
) {
  if (!.gtable_element_in_layout(
    g = g,
    element = old,
    partial_match = partial_match
  )) {
    
    if (allow_missing) {
      return(g)
    }

    ..error(".gtable_rename_element: element not found in layout table.")
  }

  if (partial_match) {
    updated_element <- grepl(pattern = old, x = g$layout$name)
  } else {
    updated_element <- g$layout$name == old
  }

  if (sum(updated_element) > 1L) {
    ..warning(".gtable_rename_element: multiple elements will be updated.")
  }

  g$layout$name[updated_element] <- new

  return(g)
}



.gtable_update_panel_aspects <- function(g) {
    # Make panels inherit heights and widths, if they don't have any. This is done
  # to ensure that panels retain heights and widths, even if supporting elements
  # such as the axis text and label elements are stripped on figure composition.
  element_names <- g$layout$name
  panel_elements <- element_names[sapply(
    element_names, 
    startswith_any, 
    prefix = .all_gtable_panel_names()
  )]
  
  for (panel_element in panel_elements) {
    for (aspect in c("height", "width")) {
      grob_id <- which(element_names == panel_element)
      panel_size <- .gtable_get_aspect_size(
        grob_id = grob_id,
        g = g,
        aspect = aspect
      )
      
      # Only inherit aspect size if the panel element does not have its own
      # size set.
      if (is.null(panel_size)) {
        if (aspect == "height") {
          position <-g$layout[grob_id, "t", drop = TRUE]
          panel_size <- g$heights[position]
          g$grobs[[grob_id]]$height <- panel_size
          
        } else {
          position <- g$layout[grob_id, "l", drop = TRUE]
          panel_size <- g$widths[position]
          g$grobs[[grob_id]]$width <- panel_size
        }
      }
    }
  }
  
  return(g)
}



.gtable_update_layout <- function(g) {
  # gtable uses its heights and widths attributes to structure and plot an 
  # image. Because we sometimes work with composite plots in familiar, we
  # need to update the gtable object. In particular, we need to set heights and
  # widths attributes correctly.
  #
  # To do so, we read the heights and widths of individual objects in a column
  # or row, depending on the aspect. These sizes are then converted to absolute
  # measures (points), with the exception of objects that have non-zero npc or
  # null as size. In that case, null elements take precedence over absolute
  # values. For example, the figure panel has 1null height and 1null width,
  # making it resizable.
  
  ..get_updated_aspect <- function(g, aspect = "width") {
    if (aspect == "width") {
      new_sizes <- g$widths
      aspect_length <- ncol(g)
      
    } else if (aspect == "height") {
      new_sizes <- g$heights
      aspect_length <- nrow(g)
      
    } else {
      ..error_reached_unreachable_code("aspect was not height or width")
    }
    
    for (ii in seq_len(aspect_length)) {
      # Select candidates.
      if (aspect == "width") {
        candidates <- which(g$layout$l == ii & g$layout$r == ii)
      } else {
        candidates <- which(g$layout$t == ii & g$layout$b == ii)
      }
      
      # Skip if there are no candidates.
      if(length(candidates) == 0L) {
        new_sizes[ii] <- grid::unit(0.0, "points")
        next
      } 
      # Identify the aspect size of the grobs.
      grob_sizes <- lapply(candidates, .gtable_get_aspect_size, g = g, aspect = aspect)
      grob_sizes <- grob_sizes[sapply(grob_sizes, grid::is.unit)]
      
      # Skip if there are no valid aspect sizes.
      if (length(grob_sizes) == 0L) {
        new_sizes[ii] <- grid::unit(0.0, "points")
        next
      }
      
      # Set grob sizes.
      grob_sizes <- do.call(grid::unit.c, grob_sizes)
      grob_sizes <- ..gtable_filter_aspect_sizes(grob_sizes)
      
      # Check if there any non-zero sizes.
      if (all(as.numeric(grob_sizes) == 0.0)) {
        new_sizes[ii] <- grid::unit(0.0, "points")
        next
      }
      
      if (length(grob_sizes) > 1L) grob_sizes <- max(grob_sizes)
      
      # Update size based on elements that are present, as long as the
      # previously provided size did not contain "null" size types.
      new_sizes[ii] <- grob_sizes
    }
    
    return(new_sizes)
  }
  
  new_heights <- ..get_updated_aspect(g = g, aspect = "height")
  new_widths <- ..get_updated_aspect(g = g, aspect = "width")
  
  # Update heights and widths in the table.
  g$heights <- new_heights
  g$widths <- new_widths
  
  return(g)
}



.gtable_get_aspect_size <- function(grob_id, g, aspect = "width") {
  
  grob_size <- .gtable_get_grob_aspect_size(
    grob = g$grobs[[grob_id]],
    aspect = aspect
  )
  
  return(grob_size)
}



.gtable_get_grob_aspect_size <- function(grob, aspect = "width") {
  
  if (aspect == "width") {
    aspect_names <- c("widths", "width")
    
  } else if (aspect == "height") {
    aspect_names <- c("heights", "height")
    
  } else {
    ..error_reached_unreachable_code("aspect was not height or width")
  }
  
  grob_size <- NULL
  
  # Attempt to get the size of the grob in absolute units, but maintain size
  # if this is null or npc.
  for (aspect_name in aspect_names) {
    current_size <- grob[[aspect_name]]
    if (!is.null(current_size)) {
      if (all(grid::unitType(current_size) %in% c("null", "npc"))) {
        grob_size <- current_size
        break
        
      } else {
        grob_size <- sum(grid::convertUnit(current_size, "points"))
        break
      }
    }
  }
  
  return(grob_size)
}



..gtable_filter_aspect_sizes <- function(x) {
  if (length(x) == 0L) return(NULL)
  
  # Filter items that have no dimension.
  grob_zero <- as.numeric(x) == 0.0
  if (length(x) == 1L && all(grob_zero)) return(grid::unit(0.0, "points"))
  
  x <- x[!grob_zero]
  if (length(x) == 0L) return(grid::unit(0.0, "points"))
  
  # null overrides everything else.
  grob_null <- grid::unitType(x) == "null"
  if (any(grob_null)) {
    x <- x[grob_null]
    return(grid::unit(max(as.numeric(x)), "null"))
  }
  
  # npc is only relevant if there are no other grobs with more specific unit
  # types.
  grob_npc <- grid::unitType(x) == "npc"
  if (any(grob_npc) && !all(grob_npc)) x <- x[!grob_npc]
  
  return(x)
}
