####===========================================================================
# This is the main entry point for the dendrogram rendering engines.
#
# - plot_dendro_ggplot() : takes pre-calculated data and renders a ggplot2 object
# - plot_dendro_plotly() : takes pre-calculated data and renders a plotly object
#
# Dependencies (loaded automatically via source):
#    - generate_dendro_data() from your data script
#####===========================================================================

#####===========================================================================
#                         PLOT_DENDRO_GGPLOT
#
# Akzeptiert jetzt das 'dendro_data' Objekt, behält das originale Layout 
# aber zu 100% exakt bei.
#####===========================================================================
#####===========================================================================
#                         PLOT_DENDRO_GGPLOT
#####===========================================================================
plot_dendro_ggplot <- function(dendro_data, title="", names_vector=NULL, show_legend=FALSE, show_x_axis=TRUE, show_y_axis=TRUE) {
  
  segments_df <- dendro_data$draw_result$segments
  labels_df   <- dendro_data$draw_result$labels
  max_height  <- dendro_data$max_height
  palette     <- dendro_data$color_vector
  
  labels_df$label <- if (!is.null(names_vector)) {
    names_vector[labels_df$id]
  } else {
    as.character(labels_df$id)
  }
  
  # pushing labels just below the leaf lines
  labels_df$y <- -0.1
  
  # calculate dynamic font sizes based on the dataset size
  n_elements <- nrow(labels_df)
  axis_title_size <- max(8, min(14, 14 - (n_elements / 30)))
  axis_text_size  <- max(6, min(10, 10 - (n_elements / 40)))
  
  # generate the empty plot and build layer by layer
  plot <- ggplot() +
    
    # layer 1: drawing all branch segments, colored by class
    geom_segment(data = segments_df, aes(x=x0, y=y0, xend=x1, yend=y1, color=class), linewidth = 0.7) +
    
    # layer 2: applying the color palette to both segments and labels
    scale_color_manual(values = palette, breaks = names(palette)[names(palette) != "Default"]) +
    
    # layer 3: setting y range => leaving enough space below for the rotated labels
    scale_y_continuous(limits = c(-8, max_height),
                       breaks = seq(0, max_height, by = 5)) +
    labs(y = "Distance", x = "") +
    ggtitle(title) +
    
    # layer 4: clean white background, removing grid, axis lines and x ticks
    theme_classic() +
    theme(
      panel.grid   = element_blank(),
      axis.line    = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x  = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_text(size = axis_title_size),
      axis.text.y  = element_text(size = axis_text_size),
      plot.title   = element_text(size = axis_title_size + 2, face = "bold")
    )
  
  # geom_text always renders an "a" glyph into the legend — suppressed unconditionally
  if (show_x_axis) {
    
    n_patients <- nrow(labels_df)
    
    font_size <- max(1.2, min(3.5, 120 / n_patients))
    
    plot <- plot + geom_text(
      data        = labels_df, 
      aes(x = x, y = y, label = label, color = class), 
      angle       = 90, 
      hjust       = 1, 
      vjust       = 0.5, 
      size        = font_size,    
      show.legend = FALSE
    )
  }
  
  if (!show_legend) {
    plot <- plot + theme(legend.position = "none")
  }
  if (!show_y_axis) {
    plot <- plot + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
  }
  
  return(plot)
}

#####===========================================================================
#                         PLOT_DENDRO_PLOTLY
#####===========================================================================
plot_dendro_plotly <- function(dendro_data, title="", names_vector=NULL, show_legend=FALSE, show_x_axis=TRUE, show_y_axis=TRUE) {
  
  segments_df <- dendro_data$draw_result$segments
  labels_df   <- dendro_data$draw_result$labels
  max_height  <- dendro_data$max_height
  palette     <- dendro_data$color_vector
  
  # resolving display names => falling back to raw id if no names provided
  labels_df$label <- if (!is.null(names_vector)) {
    names_vector[labels_df$id]
  } else {
    as.character(labels_df$id)
  }
  
  # calculate dynamic font sizes based on the dataset size
  n_elements <- nrow(labels_df)
  axis_title_size <- max(8, min(14, 14 - (n_elements / 30)))
  axis_text_size  <- max(6, min(10, 10 - (n_elements / 40)))
  
  # initialize the plotly object
  plot <- plotly::plot_ly()
  
  # layer 1 & 2: drawing branch segments and text labels grouped by class to preserve coloring
  # To map the colors exactly, we loop through the unique classes and add them as grouped traces
  unique_classes <- unique(segments_df$class)
  
  for (classes in unique_classes) {
    classes_segments <- segments_df[segments_df$class == classes, ]
    classes_color <- if (classes %in% names(palette)) palette[classes] else "black"
    
    # We need to structure the coordinates for vector-optimized line drawing in plotly:
    # alternating x0, x1, NA and y0, y1, NA creates disconnected segments within a single trace
    x_coords <- as.vector(t(cbind(classes_segments$x0, classes_segments$x1, NA)))
    y_coords <- as.vector(t(cbind(classes_segments$y0, classes_segments$y1, NA)))
    
    # evaluate legend visibility for specific classes analogously to scale_color_manual breaks
    is_default <- (classes == "Default")
    include_in_legend <- show_legend && !is_default
    
    plot <- plot %>% plotly::add_lines(
      x = x_coords,
      y = y_coords,
      line = list(color = classes_color, width = 1.5),
      name = classes,
      legendgroup = classes,
      showlegend = include_in_legend,
      hoverinfo = "none"
      
    )
    
    if (show_x_axis) {
      font_size_plotly <- max(8, min(18, 180 / n_elements))
      class_labels_df <- labels_df[labels_df$class == classes, ]
      
      if (nrow(class_labels_df) > 0) {
        plot <- plot %>% plotly::add_annotations(
          x = class_labels_df$x,
          y = rep(0, nrow(class_labels_df)), # Bleibt mathematisch exakt auf der Nulllinie
          text = class_labels_df$label,
          showarrow = FALSE,
          textangle = -90,                 # Text verläuft senkrecht von oben nach unten
          xanchor = "center",              # ERZWINGT: Absolut kein Links-/Rechts-Versatz mehr!
          yanchor = "bottom",              # Dockt die Basis des Texts (das Textende) an y=0 an
          yshift = -15,                    # KORREKTUR: Schiebt das Textende starr im Raum nach unten
          font = list(
            size = font_size_plotly,        
            color = classes_color
          ),
          legendgroup = classes,
          showlegend = FALSE,
          hoverinfo = "none"
        )
      }
    }
  }
  # layer 4: clean white background, removing grid, axis lines and x ticks
  # Setting up the layout configuration to exactly mimic theme_classic()
  xaxis_config <- list(
    title = "",
    showgrid = FALSE,
    showline = FALSE,
    zeroline = FALSE,
    showticklabels = FALSE,
    ticks = "",
    range = c(0.5, n_elements + 0.5)
  )
  
  yaxis_config <- list(
    title = list(text = "Distance", font = list(size = axis_title_size)),
    showgrid = FALSE,
    showline = FALSE,
    zeroline = FALSE,
    showticklabels = show_y_axis,
    ticks = "",
    tickvals = seq(0, max_height, by = 5),
    tickfont = list(size = axis_text_size),
    range = c(-8, max_height)
  )
  
  if (!show_y_axis) {
    yaxis_config$title <- list(text = "")
  }
  
  # assembling the final plot layout
  plot <- plot %>% plotly::layout(
    title = list(
      text = paste0("<b>", title, "</b>"),
      font = list(size = axis_title_size + 2)
    ),
    xaxis = xaxis_config,
    yaxis = yaxis_config,
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    showlegend = show_legend,
    margin = list(b = if (show_x_axis) 160 else 40, l = 60, r = 40, t = 60)
  )
  
  return(plot)
}