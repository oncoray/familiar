.get_palette <- function(
    x = NULL,
    palette_type,
    n = NULL, 
    invert = FALSE,
    use_alternative = FALSE,
    diverge_to_white = FALSE
) {
  
  # Check whether the provided palette is known, a set of colours, or a default.
  if (is.null(x)) {
    colours <- .get_default_palette(
      n = n,
      palette_type = palette_type,
      invert = invert,
      use_alternative = use_alternative,
      diverge_to_white = diverge_to_white
    )
    
  } else if (all(is.character(x))) {
    if (length(x) > 1L) {
      # Check that all elements are colours.
      valid_colours <- sapply(x, .is_colour)

      if (!all(valid_colours)) {
        ..error(paste0(
          "The following palette colours could not be interpreted: ",
          paste_s(x[!valid_colours]),
          " . A valid colour is either a hexadecimal string (e.g. \"#4F94CD\"), ",
          "a colour specified in grDevices::colors() (e.g. \"steelblue3\"), ",
          "or \"transparant\". Alternatively, a palette can be specified by name."
        ))
      }

      colours <- x
      
    } else if (length(x) == 1L) {
      if (palette_type %in% c("divergent", "sequential") && is.null(n)) {
        # Set n if unspecified.
        n <- 31L
        
      } else if (palette_type == "qualitative" && is.null(n)) {
        # In case of qualitative palettes, n should be set explicitly by familiar.
        ..error_reached_unreachable_code("Qualitative palettes requires the number of discrete cases n.")
      }
      
      # Obtain colours from a predefined palette.
      colours <- .palette_to_colour(
        x = x,
        n = n
      )
    }
    
  } else {
    ..error(paste0(
      "The requested palette are neither colours nor a palette: ",
      paste_s(x)
    ))
  }

  return(colours)
}



.is_colour <- function(x) {
  return(
    x %in% grDevices::colors() ||
      x == "transparant" ||
      grepl(pattern = "^#(\\d|[a-f]){6,8}$", x, ignore.case = TRUE)
  )
}



.palette_to_colour <- function(x, n) {
  # Determine if the string ends with _, _r or _rev.
  invert_colours <- grepl(pattern = "_$|_r$|_rev$", x, ignore.case = TRUE)

  # Strip from x
  x <- gsub(pattern = "_$|_r$|_rev$", replacement = "", x)

  colours <- NULL
  # Try paletteer.
  if (rlang::is_installed("paletteer")) {
    # Continuous palette
    colours <- tryCatch(
      paletteer::paletteer_c(
        palette = x,
        n = n
      ),
      error = function(err) (NULL)
    )
    
    # Dynamic palette
    if (is.null(colours)) {
      colours <- tryCatch(
        paletteer::paletteer_dynamic(
          palette = x,
          n = n
        ),
        error = function(err) (NULL)
      )
    }
    
    # Discrete palette
    if (is.null(colours)) {
      colours <- tryCatch(
        paletteer::paletteer_d(
          palette = x,
          n = n
        ),
        error = function(err) (NULL)
      )
    }
  }
  
  # Try grDevices::palette (requires R version >= 4.0.0)
  if (is.null(colours)) {
    colours <- tryCatch(
      grDevices::palette.colors(n = n, palette = x),
      error = function(err) (NULL)
    )
  }

  # Try grDevices::hcl.colors (requires R version >= 3.6.0)
  if (is.null(colours)) {
    colours <- tryCatch(
      grDevices::hcl.colors(n = n, palette = x),
      error = function(err) (NULL)
    )
  }
  
  
  # Palettes that are always available.
  if (is.null(colours)) {
    if (x == "default") {
      colours <- grDevices::palette()
    } else if (x == "rainbow") {
      colours <- grDevices::rainbow(n = n)
    } else if (x == "heat.colors") {
      colours <- grDevices::heat.colors(n = n)
    } else if (x == "terrain.colors") {
      colours <- grDevices::terrain.colors(n = n)
    } else if (x == "topo.colors") {
      colours <- grDevices::topo.colors(n = n)
    } else if (x == "cm.colors") {
      colours <- grDevices::cm.colors(n = n)
    }
  }

  if (is.null(colours)) {
    ..error(paste0(
      "The palette was not recognised: ", x,
      ". Please check the spelling. Note that some options may not be available prior ",
      "to R 4.0.0 (grDevices::palette.pals(), and R 3.6.0 (grDevices::hcl.pals()))."
    ))
  }

  if (invert_colours) colours <- rev(colours)

  return(colours)
}


.get_default_palette <- function(
    n,
    palette_type,
    invert,
    use_alternative = FALSE,
    diverge_to_white = FALSE
) {
  
  .check_parameter_value_is_valid(
    x = palette_type, var_name = "palette_type",
    values = c("qualitative", "sequential", "divergent")
  )

  if (palette_type == "qualitative") {
    # Palette derived from:
    #
    # colorspace::diverge_hcl(n = 21, h = c(245, 35), c = c(130, 85), l = c(65,
    # 80), power = c(1, 1.3), gamma = NULL, fixup = TRUE)
    #
    # colorspace::diverge_hcl(n = 21, h = c(185, 10), c = c(130, 85), l = c(60,
    # 80), power = c(1, 1.3), gamma = NULL, fixup = TRUE)
    #
    # colorspace::diverge_hcl(n = 21, h = c(65, 125), c = c(100, 85), l = c(70,
    # 80), power = c(1, 1.3), gamma = NULL, fixup = TRUE)
    #
    # colorspace::diverge_hcl(n = 21, h = c(210, 320), c = c(60, 85), l = c(60,
    # 80), power = c(1, 1.3), gamma = NULL, fixup = TRUE)
    #
    # colorspace::diverge_hcl(n = 21, h = c(100, 0), c = c(50, 85), l = c(70,
    # 80), power = c(1, 1.3), gamma = NULL, fixup = TRUE)
    
    palette_10 <- c(
      "#00A8FF",
      "#F77D00",
      "#00BCAB",
      "#FE535E",
      "#D4A600",
      "#40C41C",
      "#00A2B7",
      "#C973B6",
      "#9FB368",
      "#E495A5"
    )
    
    palette_20 <- c(
      "#00A8FF",
      "#93BDF0",
      "#F77D00",
      "#E9AD8D",
      "#00BCAB",
      "#96C9C5",
      "#FE535E",
      "#DFB6B7",
      "#D4A600",
      "#CEC2AC",
      "#40C41C",
      "#B2C9B0",
      "#00A2B7",
      "#8AC4D1",
      "#C973B6",
      "#DCADD0",
      "#9FB368",
      "#B3C680",
      "#E495A5",
      "#ECB1BC"
    )
    
    if (!use_alternative) {
      if (n <= 10L) {
        colours <- palette_10[1L:n]
        
      } else if (n <= 20L) {
        colours <- palette_20[1L:n]
        
      } else {
        ..error(paste0(
          "The required number (", n, ") of discrete colors is too large for the ",
          "default qualitative score (max 20). "
        ))
      }
    } else {
      # Alternative colour schemes were the blue and orange colours come last.
      # This is to avoid confusion with other gradients that may be used in the
      # plot.
      if (n <= 10L) {
        colours <- rev(palette_10)[1L:n]
        
      } else if (n <= 20L) {
        colours <- rev(palette_20)[1L:n]
        
      } else {
        ..error(paste0(
          "The required number (", n, ") of discrete colors is too large for ",
          "the default qualitative score (max 20). "
        ))
      }
    }
  } else if (palette_type == "sequential") {
    if (!use_alternative) {
      # A palette with the same hue (blue) as the first color of the qualitative
      # palette. Based on the blue part of colorspace::diverge_hcl(n = 21, h =
      # c(245, 35), c = c(130, 85), l = c(65, 80), power = c(1, 1.3), gamma =
      # NULL, fixup = TRUE)
      colours <- c(
        "#C6C6C6", "#BCC5D2", "#B0C3DD", "#A3C0E7", "#93BDF0", "#81BAF8",
        "#6AB7FF", "#49B3FF", "#00B0FF", "#00ACFF", "#00A8FF"
      )
      
    } else {
      # Alternative reddish colour scheme that avoids the use of blues and
      # orange tones that may have been used as a primary palette. Based on the
      # red part of colorspace::diverge_hcl(n = 21, h = c(185, 10), c = c(130,
      # 85), l = c(60, 80), power = c(1, 1.3), gamma = NULL, fixup = TRUE)
      colours <- c(
        "#C6C6C6", "#D4BFBF", "#DFB6B7", "#E8ACAE", "#EFA1A4", "#F5969A",
        "#F98A8F", "#FC7E84", "#FE7178", "#FE636C", "#FE535E"
      )
    }

    if (invert) colours <- rev(colours)
    
  } else if (palette_type == "divergent") {
    if (!use_alternative) {
      # A palette with the same hues (blue and orange) as the first two colors
      # of the qualitative palette.
      if (!diverge_to_white) {
        # Bright center.
        #
        # colorspace::diverge_hcl(n = 21, h = c(245, 35), c = c(130, 85), l =
        # c(65, 80), power = c(1, 1.3), gamma = NULL, fixup = TRUE)
        colours <- c(
          "#00A8FF", "#00ACFF", "#00B0FF", "#49B3FF", "#6AB7FF", "#81BAF8",
          "#93BDF0", "#A3C0E7", "#B0C3DD", "#BCC5D2",
          "#C6C6C6",
          "#D2C1BA", "#DBBBAC", "#E3B49D", "#E9AD8D", "#EEA57C", "#F29D68",
          "#F49650", "#F68E2F", "#F78600", "#F77D00"
        )
        
      } else {
        # Dark centre.
        #
        # colorspace::diverge_hcl(n = 21, h = c(245, 35), c = c(130, 85), l =
        # c(65, 30), power = c(1, 1.3), gamma = NULL, fixup = TRUE)
        colours <- c(
          "#00A8FF", "#009BFF", "#008FEF", "#0083D8", "#0078C1", "#056DAB",
          "#266495", "#345B81", "#3C526D", "#424C59",
          "#474747",
          "#584740", "#694A39", "#794E32", "#8A522A", "#9A581E", "#AC5E07",
          "#BE6500", "#D06D00", "#E37500" ,"#F77D00"
        )
      }
      
    } else {
      # A palette based on the same hues the first two colours of the
      # alternative qualitative palette.
      if (!diverge_to_white) {
        # Bright centre.
        #
        # colorspace::diverge_hcl(n = 21, h = c(185, 10), c = c(130, 85), l =
        # c(60, 80), power = c(1, 1.3), gamma = NULL, fixup = TRUE)
        colours <- c(
          "#00BCAB", "#00BFAF", "#00C2B4", "#00C4B7", "#00C6BB", "#00C8BE",
          "#40C9C1", "#74C9C3", "#96C9C5", "#B0C9C6",
          "#C6C6C6",
          "#D4BFBF", "#DFB6B7", "#E8ACAE", "#EFA1A4", "#F5969A", "#F98A8F",
          "#FC7E84", "#FE7178", "#FE636C", "#FE535E"
        )
        
      } else {
        # Dark centre.
        #
        # colorspace::diverge_hcl(n = 21, h = c(185, 10), c = c(130, 85), l =
        # c(60, 30), power = c(1, 1.3), gamma = NULL, fixup = TRUE)
        colours <- c(
          "#00BCAB", "#00AE9F", "#00A093", "#009387", "#00877C", "#007A71",
          "#006F67", "#00645E", "#065955", "#334F4D",
          "#474747",
          "#5A4546", "#6D4446", "#7E4547", "#904649", "#A1474C", "#B3494F",
          "#C54B52", "#D74D56", "#EB505A", "#FE535E"
        )
      }
    }

    if (invert) colours <- rev(colours)
  }

  return(colours)
}
