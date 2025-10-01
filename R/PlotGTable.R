.all_gtable_guide_names <- function() {
  return(c(
    "guide-box-right", "guide-box-left",
    "guide-box-top", "guide-box-bottom", "guide-box-inside"
  ))
}

.all_gtable_title_names <- function() {
  return(c("title", "subtitle", "caption"))
}

.all_gtable_strip_x_names <- function() {
  return(c("strip-t", "strip-b"))
}

.all_gtable_strip_y_names <- function() {
  return(c("strip-l", "strip-r"))
}

.all_gtable_label_x_names <- function() {
  return(c("xlab-b", "xlab-t"))
}

.all_gtable_label_y_names <- function() {
  return(c("ylab-l", "ylab-r"))
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



.gtable_get_extent <- function(g, element, partial_match = FALSE) {
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
      ".gtable_get_extent: element not found in layout table."
    )
  }

  # Find extent by deriving the bounding box of the elements.
  extent <- list()
  extent$t <- min(position$t)
  extent$b <- max(position$b)
  extent$l <- min(position$l)
  extent$r <- max(position$r)

  # Return as array.
  extent <- simplify2array(extent)

  return(extent)
}



.gtable_extract <- function(
    g,
    element,
    partial_match = FALSE, 
    drop_empty = FALSE
) {
  # Extract partially matching elements
  if (partial_match) {
    extracted_table <- gtable::gtable_filter(
      x = g, 
      pattern = paste0(element, collapse = "|")
    )
    
  } else {
    # Extract exactly matching elements
    extracted_table <- .gtable_filter_exact(
      g = g, 
      element = element
    )
  }

  # Drop empty elements
  if (drop_empty) {
    extracted_table <- .gtable_drop_empty(g = extracted_table)
  }

  if (length(extracted_table) == 0L) {
    extracted_table <- NULL
  }

  return(extracted_table)
}



.gtable_drop_empty <- function(g, trim = TRUE) {
  # Find grob classes
  grob_classes <- lapply(g$grobs, class)

  # Find zeroGrob and nullGrob classes, which represent empty elements.
  matches <- sapply(
    grob_classes, 
    function(ii) any(ii %in% c("zeroGrob", "nullGrob"))
  )

  # Filter layout and grobs of the gtable g by keeping non-empty elements.
  g$layout <- g$layout[!matches, , drop = FALSE]
  g$grobs <- g$grobs[!matches]

  if (trim) g <- gtable::gtable_trim(g)

  return(g)
}



.gtable_filter_exact <- function(
    g,
    element, 
    trim = TRUE, 
    invert = FALSE
) {
  # Similar to gtable::gtable_filter, but with exact matching.

  # Find exact matches
  matches <- g$layout$name %in% element

  # If invert is TRUE, select only non-matching entries.
  if (invert) matches <- !matches

  # Filter layout and grobs of the gtable g.
  g$layout <- g$layout[matches, , drop = FALSE]
  g$grobs <- g$grobs[matches]

  if (trim) g <- gtable::gtable_trim(g)

  return(g)
}



.gtable_insert_spacer <- function(
    g,
    position,
    width = NULL,
    height = NULL,
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
  elements_kept <- g$layout$name[!(g$layout$name %in% removed_element)]
  g <- gtable::gtable_filter(
    x = g,
    pattern = paste(elements_kept, sep = "", collapse = "|"),
    trim = trim
  )
  
  return(g)
}



.gtable_insert <- function(
    g,
    g_new,
    where,
    spacer = NULL
) {
  # Intended for inserting elements that stretch multiple along_elements. It can
  # also be used for inserting elements directly (without along_elements) and/or
  # replacing existing elements (attempt_replace=TRUE)
  if (length(g_new) == 0L) {
    return(g)
  }
  browser()
  if (where[1L] == "replace") {
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
      ref_element_2 = where[5L]
    )
    
  } else if (where[1L] == "left") {
    g <- ..gtable_insert_left(
      g = g,
      ref_element = where[2L],
      spacer = spacer
    )
    
  } else if (where[1L] == "right") {
    g <- ..gtable_insert_right(
      g = g,
      ref_element = where[2L],
      spacer = spacer
    )
    
  } else if (where[1L] == "above") {
    g <- ..gtable_insert_above(
      g = g,
      ref_element = where[2L],
      spacer = spacer
    )
    
  } else if (where[1L] == "below") {
    g <- ..gtable_insert_below(
      g = g,
      ref_element = where[2L],
      spacer = spacer
    )
    
  } else {
    ..error_reached_unreachable_code(
      "The first element of where should be one of replace, intersect, left, right, above or below."
    )
  }

  # Update widths and heights.
  g <- .gtable_update_layout(g = g)
  browser()
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
  
  # Insert new element.
  g <- gtable::gtable_add_grob(
    g,
    grobs = g_new,
    t = position[["t"]],
    l = position[["l"]],
    b = position[["b"]],
    r = position[["r"]],
    name = g_new$layout$name[1L],
    clip = g_new$layout$clip[1L]
  )
  
  return(g)
}



..gtable_insert_intersect <- function(g, ref_element, spacer = NULL) {

}


..gtable_insert_below <- function(
    g,
    g_new, 
    ref_element, 
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
  g <- gtable::gtable_add_grob(
    g,
    grobs = g_new,
    t = new_position[["t"]],
    l = new_position[["l"]],
    b = new_position[["b"]],
    r = new_position[["r"]],
    name = g_new$layout$name[1L],
    clip = g_new$layout$clip[1L]
  )
  
  return(g)
}



# .gtable_insert <- function(
#     g, 
#     g_new, 
#     where = "top", 
#     ref_element = "panel", 
#     spacer = NULL, 
#     partial_match = FALSE
# ) {
#   g <- .gtable_insert_along(
#     g = g,
#     g_new = g_new,
#     where = where,
#     ref_element = ref_element,
#     spacer = spacer,
#     partial_match_ref = partial_match,
#     partial_match_along = partial_match
#   )
#   
#   return(g)
# }



.gtable_insert_along <- function(
    g,
    g_new,
    where = "top",
    ref_element = "panel",
    along_element = ref_element,
    spacer = NULL,
    attempt_replace = FALSE,
    partial_match_ref = FALSE,
    partial_match_along = FALSE,
    update_dimensions = TRUE
) {
  # Intended for inserting elements that stretch multiple along_elements. It can
  # also be used for inserting elements directly (without along_elements) and/or
  # replacing existing elements (attempt_replace=TRUE)
  if (length(g_new) == 0L) {
    return(g)
  }
  
  # Create an offset.
  ref_shift <- integer(4L)
  names(ref_shift) <- c("t", "l", "b", "r")
  spacer_ref_offset <- elem_ref_offset <- integer(4L)
  names(spacer_ref_offset) <- c("t", "l", "b", "r")
  names(elem_ref_offset) <- c("t", "l", "b", "r")
  
  # Find position to insert this element
  ref_position <- .gtable_get_position(
    g = g, 
    element = ref_element, 
    where = where, 
    partial_match = partial_match_ref
  )

  if (attempt_replace) {
    # Find if there is a grob with the same name at the intended position.
    g_index <- .gtable_which_aligned(g,
      element = g_new$layout$name,
      ref_element = ref_element,
      where = where,
      partial_match_ref = partial_match_ref
    )

    if (!is.null(g_index)) {
      # Replace the grob.
      g$grobs[[g_index]] <- g_new

      # Update heights and widths to get the accurate figures.
      g <- .gtable_update_layout(g)

      return(g)
    }
  }

  # Make room to insert the stuff.
  if (where == "top") {
    # Add row below t-1 (i.e. at t, and move existing rows down).
    g <- gtable::gtable_add_rows(
      g,
      heights = g_new$heights,
      pos = ref_position[["t"]] - 1L
    )

    # This shifts the rest of the elements (including the reference element)
    # down by a number of rows, which means that we need an offset.
    ref_shift[["t"]] <- ref_shift[["b"]] <- length(g_new$heights)
    
    # Add space between the inserted element and the reference element.
    if (!is.null(spacer)) {
      g <- gtable::gtable_add_rows(
        g,
        heights = spacer,
        pos = ref_position[["t"]] + ref_shift[["t"]] - 1L
      )
      
      spacer_ref_offset[["t"]] <- spacer_ref_offset[["b"]] <- -1L
      ref_shift[["t"]] <- ref_shift[["t"]] + 1L
      ref_shift[["b"]] <- ref_shift[["b"]] + 1L
    }
    
    elem_ref_offset[["t"]] <- -ref_shift[["t"]]
    elem_ref_offset[["b"]] <- -ref_shift[["b"]] + length(g_new$heights) - 1L
    
  } else if (where == "bottom") {
    # This does not shift the reference element down.
    ref_shift[["t"]] <- ref_shift[["b"]] <- 0L
    
    # Add space between the reference element and the element to be inserted.
    if (!is.null(spacer)) {
      g <- gtable::gtable_add_rows(
        g,
        heights = spacer,
        pos = ref_position[["b"]]
      )
      
      spacer_ref_offset[["t"]] <- spacer_ref_offset[["b"]] <- 1L
      elem_ref_offset[["t"]] <- elem_ref_offset[["b"]] <- 1L
    }
    
    # Add row below b (i.e. at b+1, and move existing rows down).
    g <- gtable::gtable_add_rows(
      g,
      heights = g_new$heights,
      pos = ref_position[["b"]] + elem_ref_offset[["b"]]
    )
    
    elem_ref_offset[["t"]] <- elem_ref_offset[["t"]] + 1L
    elem_ref_offset[["b"]] <- elem_ref_offset[["b"]] + length(g_new$heights)

  } else if (where == "left") {
    # Add column at l-1 (i.e. at l, and move existing columns to right)
    g <- gtable::gtable_add_cols(
      g,
      widths = g_new$widths,
      pos = ref_position[["l"]] - 1L
    )

    # This shifts the rest of the elements (including the reference element) to
    # the right by a number of rows, which means that we need an offset.
    ref_shift[["l"]] <- ref_shift[["r"]] <- length(g_new$widths)
    
    # Add space between the inserted element and the reference element.
    if (!is.null(spacer)) {
      g <- gtable::gtable_add_cols(
        g,
        widths = spacer,
        pos = ref_position[["l"]] + ref_shift[["l"]] - 1L
      )
      
      spacer_ref_offset[["l"]] <- spacer_ref_offset[["r"]] <- -1L
      ref_shift[["l"]] <- ref_shift[["l"]] + 1L
      ref_shift[["r"]] <- ref_shift[["r"]] + 1L
    }
    
    elem_ref_offset[["l"]] <- -ref_shift[["l"]]
    elem_ref_offset[["r"]] <- -ref_shift[["r"]] + length(g_new$widths) - 1L
    
  } else if (where == "right") {
    # This does not shift the reference element to the right.
    ref_shift[["l"]] <- ref_shift[["r"]] <- 0L
    
    # Add space between the reference element and the element to be inserted.
    if (!is.null(spacer)) {
      g <- gtable::gtable_add_cols(
        g,
        widths = spacer,
        pos = ref_position[["r"]]
      )
      
      spacer_ref_offset[["l"]] <- spacer_ref_offset[["r"]] <- 1L
      elem_ref_offset[["l"]] <- elem_ref_offset[["r"]] <- 1L
    }
    
    # Add column at r (i.e. at r+1, and move existing columns to the right).
    g <- gtable::gtable_add_cols(
      g,
      widths = g_new$widths,
      pos = ref_position[["r"]] + elem_ref_offset[["r"]]
    )
    
    elem_ref_offset[["l"]] <- elem_ref_offset[["l"]] + 1L
    elem_ref_offset[["r"]] <- elem_ref_offset[["r"]] + length(g_new$widths)
    
  } else {
    ..error_reached_unreachable_code(paste0(
      "Unknown where argument: ", where
    ))
  }

  # Re-establish position of reference element.
  ref_position <- .gtable_get_position(
    g = g, 
    element = ref_element, 
    where = where, 
    partial_match = partial_match_ref
  )

  # Find element name
  element_name <- g_new$layout$name[1L]

  # Find the extent of the along_elements
  extent <- .gtable_get_extent(
    g = g, 
    element = along_element, 
    partial_match = partial_match_along
  )
  
  # Set new position
  new_position <- ref_position + elem_ref_offset

  if (where %in% c("top", "bottom")) {
    new_position[["l"]] <- extent[["l"]]
    new_position[["r"]] <- extent[["r"]]
  } else {
    new_position[["t"]] <- extent[["t"]]
    new_position[["b"]] <- extent[["b"]]
  }

  # Add element to g.
  g <- gtable::gtable_add_grob(
    g,
    grobs = g_new,
    t = new_position[["t"]],
    l = new_position[["l"]],
    b = new_position[["b"]],
    r = new_position[["r"]],
    name = element_name,
    clip = g_new$layout$clip[1L]
  )
  
  # Add spacer.
  if (!is.null(spacer)) {
    spacer_position <- ref_position + spacer_ref_offset

    if (where %in% c("top", "bottom")) {
      spacer_position[["l"]] <- extent[["l"]]
      spacer_position[["r"]] <- extent[["r"]]
      g <- .gtable_insert_spacer(
        g = g,
        position = spacer_position,
        height = spacer
      )
      
    } else {
      spacer_position[["t"]] <- extent[["t"]]
      spacer_position[["b"]] <- extent[["b"]]
      
      g <- .gtable_insert_spacer(
        g = g,
        position = spacer_position,
        width = spacer
      )
    }
    
  }
  
  # Update widths and heights.
  g <- .gtable_update_layout(g = g)
  
  return(g)
}



.gtable_which_aligned <- function(
    g,
    element,
    ref_element,
    where,
    partial_match_ref = FALSE,
    only_nearby = TRUE
) {
  # Identify the element that is located as close as possible to the reference
  # element, and is aligned with it.

  # Find position of the reference element.
  ref_position <- .gtable_get_position(
    g = g,
    element = ref_element,
    where = where,
    partial_match = partial_match_ref
  )

  # As a list
  ref_position <- as.list(ref_position)

  # Identify candidates
  if (where == "top") {
    # Any candidates should span the left-right extent of the reference element,
    # and be entirely above it.
    candidates <- which(
      g$layout$name == element &
      g$layout$l == ref_position$l &
      g$layout$r == ref_position$r &
      g$layout$t < ref_position$t &
      g$layout$b < ref_position$t
    )
    
  } else if (where == "bottom") {
    # Any candidates should span the left-right extent of the reference element,
    # and be entirely below it.
    candidates <- which(
      g$layout$name == element &
      g$layout$l == ref_position$l &
      g$layout$r == ref_position$r &
      g$layout$t > ref_position$b &
      g$layout$b > ref_position$b
    )
    
  } else if (where == "left") {
    # Any candidates should span the top-bottom extent of the reference element,
    # and be entirely to the left it.
    candidates <- which(
      g$layout$name == element &
      g$layout$l < ref_position$l &
      g$layout$r < ref_position$l &
      g$layout$t == ref_position$t &
      g$layout$b == ref_position$b
    )
    
  } else if (where == "right") {
    # Any candidates should span the top-bottom extent of the reference element,
    # and be entirely to the right it.
    candidates <- which(
      g$layout$name == element &
      g$layout$l > ref_position$r &
      g$layout$r > ref_position$r &
      g$layout$t == ref_position$t &
      g$layout$b == ref_position$b
    )
    
  } else {
    ..error_reached_unreachable_code("Unknown where argument.")
  }

  if (length(candidates) == 0L) {
    return(NULL)
  }

  if (length(candidates) > 1L && only_nearby) {
    # Identify the candidate that is located nearest to the reference element.
    layout_table <- g$layout[candidates, ]

    if (where == "top") {
      distance <- ref_position$t - layout_table$t
    } else if (where == "bottom") {
      distance <- layout_table$b - ref_position$b
    } else if (where == "left") {
      distance <- ref_position$l - layout_table$l
    } else if (where == "right") {
      distance <- layout_table$r - ref_position$r
    }

    # Select the candidate with minimal distance.
    candidates <- candidates[which.min(distance)[1L]]
  }

  return(candidates)
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



.gtable_update_layout <- function(g) {
  
  
  ..filter_aspect_sizes <- function(x) {
    if (length(x) == 0L) return(NULL)
    
    # Filter items that have no dimension.
    grob_zero <- as.numeric(x) == 0.0
    x <- x[!grob_zero]
    if (length(x) == 0L) return(grid::unit(0.0, "points"))
    
    # npc is only relevant if there are no other grobs with more specific unit
    # types.
    grob_npc <- grid::unitType(x) == "npc"
    if (any(grob_npc) && !all(grob_npc)) x <- x[!grob_npc]
    
    return(x)
  }
  
  
  ..get_aspect <- function(grob_id, g, aspect = "width") {
    
    if (aspect == "width") {
      aspect_names <- c("widths", "width")
      
    } else if (aspect == "height") {
      aspect_names <- c("heights", "height")
      
    } else {
      ..error_reached_unreachable_code("aspect was not height or width")
    }
    
    if (grid::is.unit(g$grobs[[grob_id]][[aspect_names[1L]]])) {
      grob_size <- g$grobs[[grob_id]][[aspect_names[1L]]]
      grob_size <- ..filter_aspect_sizes(grob_size)
      if (length(grob_size) > 1L) grob_size <- sum(grob_size)
      
    } else if (grid::is.unit(g$grobs[[grob_id]][[aspect_names[2L]]])) {
      grob_size <- g$grobs[[grob_id]][[aspect_names[2L]]]
      
    } else {
      grob_size <- NULL
    }
    
    return(grob_size)
  }
  
  
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
      # Do not update "null" elements.
      if (any(unlist(grid::unitType(new_sizes[ii], recurse = TRUE)) == "null")) next
      
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
      grob_sizes <- lapply(candidates, ..get_aspect, g = g, aspect = aspect)
      grob_sizes <- grob_sizes[sapply(grob_sizes, grid::is.unit)]
      
      # Skip if there are no valid aspect sizes.
      if (length(grob_sizes) == 0L) {
        new_sizes[ii] <- grid::unit(0.0, "points")
        next
      }
      
      # Set grob sizes.
      grob_sizes <- do.call(grid::unit.c, grob_sizes)
      grob_sizes <- ..filter_aspect_sizes(grob_sizes)
      
      if (length(grob_sizes) > 1L) grob_sizes <- max(grob_sizes)
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
