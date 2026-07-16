#####===========================================================================
# This script contains the rendering engines and styling helpers for
# drawing the final dendrogram plots.
#
# - get_color()          : generates a named color vector based on class labels
# - plot_dendro_ggplot() : renders a static, highly-polished ggplot2 dendrogram
# - plot_dendro_plotly() : renders an interactive, dynamic plotly dendrogram
#####===========================================================================

###===========================================================================
#                               GET_COLOR
#
# Generates a named color vector mapping each unique class to a specific hex code.
# Dynamically samples from viridis, RColorBrewer, or a custom default fallback list.
# Returns a translation vector that guarantees "Default" maps to black.
#####===========================================================================
get_color <- function(class_labels, palette) {
  
  # check input validity; return default black early if vectors are missing
  if (is.null(class_labels) || is.null(palette)) {
    return(c("Default" = "black"))
  }
  
  # extract unique groups and clean the vector by removing empty strings, NAs AND "Default"
  
  detected_classes <- unique(class_labels)
  detected_classes <- detected_classes[!is.na(detected_classes) & detected_classes != "" & detected_classes != "Default"]
  number_of_classes <- length(detected_classes)
  
  # if no classes remain after cleaning, fall back to default black mapping
  if (number_of_classes == 0) {
    return(c("Default" = "black"))
  }
  
  # define standard hex colors as a backup if no library palette is selected
  default_colors <- c(
    "#0000FF", "#FF6C00", "#005300", "#A100FA", "#B2DF8A",
    "#FFD300", "#0096FF", "#9B4D00", "#00FFD2", "#FDBF6F",
    "#7B8100", "#960000", "#00646B", "#D60072", "#00FF00",
    "#FF0000", "#540066", "#00A278", "#000094", "#FFC0CB"
  )
  
  # palette cannot be NULL at this point => the early return above already caught that,
  # so no second check is needed and the switch can pick the color source directly
  colors <- switch(
    palette,
    "viridis" = viridis::viridis(number_of_classes, end = 0.8),
    
    # the three brewer palettes only differ by their name => letting brewer.pal()
    # read it from the parameter lets all three share a single branch
    "RdYlBu" = , "RdBu" = , "PRGn" = {
      full_palette <- RColorBrewer::brewer.pal(11, palette)[-c(5, 6, 7)]
      
      # asking for more classes than the palette holds would silently map two
      # classes onto the same color, so falling back to the backup list instead
      if (number_of_classes > length(full_palette)) {
        warning(paste0("Palette ", palette, " bietet nur ", length(full_palette),
                       " Farben fuer ", number_of_classes, " Klassen -> verwende Standardfarben"))
        default_colors[1:number_of_classes]
      } else {
        full_palette[round(seq(1, length(full_palette), length.out = number_of_classes))]
      }
    },
    
    {
      warning(paste("Unbekannte Palette:", palette, "-> verwende Standardfarben"))
      default_colors[1:number_of_classes]
    }
  )
  
  # build the final translation vector and explicitly bind "Default" to black
  color_vector <- c(setNames(colors, detected_classes), "Default" = "black")
  
  return(color_vector)
}
#####===========================================================================
#                         PLOT_DENDRO_GGPLOT
#
# Renders a static dendrogram using ggplot2.
# 
# Features:
# - Dynamically adjusts leaf label font sizes and line weights based on dataset size.
# - Calculates bottom margins dynamically to prevent long labels from being clipped.
# - Generates a beautiful palette at runtime and hides the default monochrome legend.
# - Restricts zooming strictly within coordinate boundaries without clipping.
#####===========================================================================
#####===========================================================================
#                         PLOT_DENDRO_GGPLOT
#####===========================================================================
plot_dendro_ggplot <- function(dendro_data, title="", names_vector=NULL, palette_name="RdBu", show_legend=FALSE, show_x_axis=TRUE, show_y_axis=TRUE) {
  
  ##### DATA PREPARATION #########################################################
  
  # unpack pre-calculated structures
  segments_dataframe <- dendro_data$draw_result$segments
  labels_dataframe   <- dendro_data$draw_result$labels
  max_height  <- dendro_data$max_height
  
  # generate dynamic palette at runtime based on all unique classes found
  all_classes <- c(segments_dataframe$class, labels_dataframe$class)
  palette     <- get_color(all_classes, palette_name)
  
  # map original observation IDs to display names if a names vector is supplied
  unique_classes <- unique(all_classes)
  has_real_classes <- any(unique_classes != "Default") && length(unique_classes) > 0
  
  labels_dataframe$label <- if (!is.null(names_vector)) {
    names_vector[labels_dataframe$id]
  } else {
    as.character(labels_dataframe$id)
  }
  
  ##### LAYOUT & SPACING CALCULATIONS ############################################
  
  # get max character length of leaf labels for dynamic bottom margin scaling
  max_character_length <- max(nchar(labels_dataframe$label), na.rm = TRUE)
  
  # position labels slightly below 0 
  labels_dataframe$y <- - (max_height * 0.01)
  
  # set safe rendering floor for coordinate limits to protect hanging text
  y_min <- - (max_height * 0.05)
  
  # generate rounded breaks for the vertical distance axis
  y_breaks <- pretty(c(0, max_height), n = 5)
  final_max_y <- max(max(y_breaks), max_height)
  
  # calculate element density for sizing elements
  total_elements <- nrow(labels_dataframe)
  
  # set fixed typographic scale for readability => y axis
  axis_title_size <- 12  
  axis_text_size  <- 10  
  
  # dynamically shrink line thickness as the leaf density increases
  dynamic_line_width <- max(0.15, min(0.7, 0.7 - ((total_elements - 50) * 0.001)))
  
  ##### PLOT GENERATION ##########################################################
  
  plot <- ggplot() +
    
    # draw dendrogram lines 
    geom_segment(
      data      = segments_dataframe, 
      aes(x=x0, y=y0, xend=x1, yend=y1, color=class), 
      linewidth = dynamic_line_width
    ) +
    
    # map colors manually and hide the redundant "Default" category from the legend
    scale_color_manual(
      values = palette, 
      breaks = names(palette)[names(palette) != "Default"]
    ) +
    
    # apply calculated tick marks
    scale_y_continuous(breaks = y_breaks) +
    
    # restrict viewport cleanly while fully supporting out-of-bounds leaf annotations
    coord_cartesian(ylim = c(y_min, final_max_y), expand = FALSE, clip = "off") +
    
    # set axis and title labels
    labs(y = "Distance", x = "") +
    ggtitle(title) +
    
    # establish base style
    theme_classic() +
    theme(
      panel.grid   = element_blank(),
      axis.line    = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x  = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_text(size = axis_title_size),
      axis.text.y  = element_text(size = axis_text_size),
      plot.title   = element_text(size = axis_title_size + 2, face = "bold", hjust = 0.5),
      
      # scale bottom margin dynamically
      plot.margin  = margin(15, 15, max(60, max_character_length * 4.5), 15, "pt")
    )
  
  ##### CONDITIONAL PLOT LAYERS ##################################################
  
  # draw text labels rotated at 90 degrees if enabled
  if (show_x_axis) {
    
    # calculate dynamic font scale to prevent dense overlays
    font_size <- max(0.6, min(5.5, 220 / total_elements))
    
    plot <- plot + geom_text(
      data        = labels_dataframe, 
      aes(x = x, y = y, label = label, color = class), 
      angle       = 90, 
      hjust       = 1, 
      vjust       = 0.5, 
      size        = font_size,    
      show.legend = FALSE # prevents "a" symbol overlay in legend box
    )
  }
  
  # toggle legend display based on user preference and class presence
  if (show_legend && has_real_classes) {
    plot <- plot + theme(legend.position = "right")
  } else {
    plot <- plot + theme(legend.position = "none")
  }
  
  # completely hide distance axis text and titles if requested
  if (!show_y_axis) {
    plot <- plot + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
  }
  
  return(plot)
}
#####===========================================================================
#                         PLOT_DENDRO_PLOTLY
#
# Renders an interactive, dynamic dendrogram using plotly.
#
# Features:
# - Supports both horizontal ("left") and vertical ("top") orientations.
# - Dynamically adjusts branch line thickness based on dataset size to prevent clutter.
# - Pre-calculates exact leaf label lengths to scale plot margins dynamically.
# - Implements rich interactive hover templates for branches and leaf nodes.
# - Renders rotated, color-coded text annotations for all leaf labels.
#####===========================================================================

plot_dendro_plotly <- function(
    dendro_data, 
    side = "top",         
    names_vector = NULL,  
    palette_name = "RdBu", 
    show_legend = FALSE,
    show_x_axis = TRUE,  
    show_y_axis = TRUE    
) {
  
  # unpack pre-calculated structures from the data package
  segments_dataframe <- dendro_data$draw_result$segments
  labels_dataframe   <- dendro_data$draw_result$labels
  max_height         <- dendro_data$max_height
  total_elements     <- nrow(labels_dataframe)
  
  # safety check: if no segments are present, return an empty plotly canvas
  if (is.null(segments_dataframe) || nrow(segments_dataframe) == 0) {
    return(plotly::plot_ly())
  }
  
  # generate dynamic palette mapping at runtime based on all classes found
  all_classes        <- c(segments_dataframe$class, labels_dataframe$class)
  unique_classes     <- unique(all_classes)
  palette            <- get_color(all_classes, palette_name)
  
  # resolve all display names immediately to allow accurate margin calculations
  labels_dataframe$label <- if (!is.null(names_vector)) {
    names_vector[labels_dataframe$id]
  } else {
    as.character(labels_dataframe$id)
  }
  
  ##### DYNAMIC LAYOUT & SIZING CALCULATIONS #####################################
  
  # get maximum character length of resolved labels for margin scaling
  max_character_length <- max(nchar(labels_dataframe$label), na.rm = TRUE)
  
  # dynamic bottom/left margin: 80px base + 5.5px per character of the longest gene name
  dynamic_margin       <- max(80, max_character_length * 6.2)
  
  # dynamically scale branch thickness (thinner lines for high-density trees)
  dynamic_line_width   <- max(0.5, min(1.5, 2.0 - ((total_elements - 50) * 0.005)))
  
  ##### INITIALIZE PLOT ##########################################################
  
  plot <- plotly::plot_ly()
  
  ##### DRAW BRANCHES & CONNECTIONS ##############################################
  
  # Loop over ALL unique classes (guarantees perfect leaf coloring even if segments are missing)
  for (class_name in unique_classes) {
    class_segments <- segments_dataframe[segments_dataframe$class == class_name, ]
    class_color    <- if (class_name %in% names(palette)) palette[class_name] else "black"
    
    # Draw tree group-by-group (class-by-class)
    # to allow independent color-coding and clean legend separatio
    if (nrow(class_segments) > 0) {
      
      # format coordinates for plotly path-drawing
      if (side == "top") {
        x_coordinates <- as.vector(t(cbind(class_segments$x0, class_segments$x1, NA)))
        y_coordinates <- as.vector(t(cbind(class_segments$y0, class_segments$y1, NA)))
      } else {
        
        # Horizontal layout: Mirror distance on X-axis 
        x_coordinates <- as.vector(t(cbind(-class_segments$y0, -class_segments$y1, NA)))
        y_coordinates <- as.vector(t(cbind(class_segments$x0, class_segments$x1, NA)))
      }
      
      # format hover labels: omit group category for "Default" connections
      # to keep tooltips clean and avoid redundant text
      hover_texts <- if (class_name == "Default") {
        paste0("Distance: <b>", round(y_coordinates, 3), "</b>")
      } else {
        paste0("Group: <b>", class_name, "</b><br>Distance: <b>", round(y_coordinates, 3), "</b>")
      }
      
      # add branch line traces using our calculated dynamic line width
      plot <- plot %>% plotly::add_lines(
        x           = x_coordinates, 
        y           = y_coordinates,
        line        = list(color = class_color, width = dynamic_line_width),
        name        = as.character(class_name),
        legendgroup = as.character(class_name),
        showlegend  = (show_legend && class_name != "Default"), # matches ggplot legend filtering
        hoverinfo   = "text",
        text        = hover_texts
      )
    }
    
    class_labels_data <- labels_dataframe[labels_dataframe$class == class_name, ]
    
    # Process leaf labels only if group contains leaf nodes
    if (nrow(class_labels_data) > 0) {
      
      display_names <- class_labels_data$label
      
      # Clean up hover tooltips for leaf markers
      leaf_hover_text <- if (class_name == "Default") {
        paste0("Name: <b>", display_names, "</b>")
      } else {
        paste0("Name: <b>", display_names, "</b><br>Group: <b>", class_name, "</b>")
      }
      
      # raw invisible markers => opacity=0 at branch ends
      # as larger hitboxes to make hover triggers smooth and responsive
      if (side == "top") {
        plot <- plot %>% plotly::add_trace(
          type = "scatter", mode = "markers",
          x = class_labels_data$x, y = 0,
          marker = list(size = 4, color = class_color, opacity = 0),
          hoverinfo = "text", text = leaf_hover_text, showlegend = FALSE
        )
      } else {
        plot <- plot %>% plotly::add_trace(
          type = "scatter", mode = "markers",
          x = 0, y = class_labels_data$x,
          marker = list(size = 4, color = class_color, opacity = 0),
          hoverinfo = "text", text = leaf_hover_text, showlegend = FALSE
        )
      }
      
      # render rotated text annotations representing leaf labels
      if (show_x_axis) {
        
        # Dynamically shrink font size as dataset density increases
        # to prevent labels from overlapping and maintain readability.
        dynamic_font_size <- max(6.5, min(12, 13 - (total_elements / 20)))
        
        # bold labels on smaller datasets (< 100 elements) for higher quality aesthetics
        formatted_names   <- display_names
        if (total_elements < 100) {
          formatted_names <- paste0("<b>", formatted_names, "</b>")
        }
        
        # Bind X to data coordinates and Y to paper margins (y=0)
        # so labels stay perfectly aligned at the edge during zooms.
        if (side == "top") {
          plot <- plot %>% plotly::add_annotations(
            x         = class_labels_data$x, y = 0,
            xref      = "data", yref = "paper",
            text      = formatted_names,
            showarrow = FALSE, textangle = -90,
            xanchor   = "center", yanchor = "top",
            font      = list(color = class_color, size = dynamic_font_size),
            hoverinfo = "none"
          )
        } else {
          plot <- plot %>% plotly::add_annotations(
            x         = 0, y = class_labels_data$x,
            xref      = "paper", yref = "data",
            text      = formatted_names,
            showarrow = FALSE, textangle = 0,
            xanchor   = "right", yanchor = "middle",
            font      = list(color = class_color, size = dynamic_font_size),
            hoverinfo = "none"
          )
        }
      }
    }
  }
  
  ##### AXIS SEGMENTATION ########################################################
  
  # create flat layout templates to clear background grid and ticks
  clean_axis <- list(
    showgrid       = FALSE,
    showline       = FALSE,
    zeroline       = FALSE,
    showticklabels = FALSE,
    ticks          = "",
    title          = "",
    fixedrange     = FALSE
  )
  
  xaxis_config <- clean_axis
  yaxis_config <- clean_axis
  
  # configure layout parameters based on orientation
  if (side == "top") {
    xaxis_config$range <- c(0.5, total_elements + 0.5)
    yaxis_config$range <- c(0, max_height * 1.05)
    
    # enable vertical distance axis ticks and add titles
    if (show_y_axis) {
      yaxis_config$showticklabels <- TRUE
      yaxis_config$nticks         <- 18
      
      plot <- plot %>% plotly::add_annotations(
        x = -0.06, y = 0.5, xref = "paper", yref = "paper",
        text = "Distance", showarrow = FALSE, textangle = -90,
        font = list(size = 12, color = "black"), hoverinfo = "none"
      )
    }
  } else {
    yaxis_config$range <- c(0.5, total_elements + 0.5)
    xaxis_config$range <- c(-max_height * 1.05, 0)
    
    # enable horizontal distance axis ticks and add titles
    if (show_y_axis) {
      xaxis_config$showticklabels <- TRUE
      xaxis_config$nticks         <- 18
      
      plot <- plot %>% plotly::add_annotations(
        x = 0.5, y = -0.06, xref = "paper", yref = "paper",
        text = "Distance", showarrow = FALSE, textangle = 0,
        font = list(size = 12, color = "black"), hoverinfo = "none"
      )
    }
  }
  
  ##### GENERATE LAYOUT ##########################################################
  
  plot <- plot %>% 
    plotly::layout(
      xaxis         = xaxis_config,
      yaxis         = yaxis_config,
      plot_bgcolor  = "white",
      paper_bgcolor = "white",
      showlegend    = show_legend,
      margin        = list(
        b = if (show_x_axis && side == "top")  dynamic_margin else 40,
        l = if (show_x_axis && side == "left") dynamic_margin else 75,
        r = 40,
        t = 40
      )
    ) %>% 
    plotly::config(scrollZoom = TRUE) # allow users to zoom in/out with scroll wheel
  
  return(plot)
}