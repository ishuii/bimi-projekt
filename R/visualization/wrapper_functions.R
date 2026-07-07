generate_dendro_plotly <- function(cluster_result, tree_result, order_vector,
                                   title="", names_vector=NULL, class_labels=NULL, palette=NULL,
                                   show_x_axis=TRUE, show_y_axis=TRUE) {
  
  ## --- 1. BASE PLOT ---
  ## We generate your original ggplot first
  dendro <- generate_dendro(
    cluster_result = cluster_result, tree_result = tree_result,
    order_vector   = order_vector,   title        = title,
    names_vector   = names_vector,   class_labels = class_labels,
    palette        = palette,        show_x_axis  = show_x_axis,
    show_y_axis    = show_y_axis
  )
  
  ## --- 2. EXTRACT TEXT DATA ---
  ## We only extract the text data from ggplot because plotly breaks rotated text
  text_layer_i    <- which(sapply(dendro$layers, function(l) inherits(l$geom, "GeomText")))
  text_data       <- layer_data(dendro, text_layer_i)
  
  ## --- 3. CONVERT LINES ONLY ---
  ## We remove the text from ggplot before converting, so plotly only converts the lines
  dendro_no_labels        <- dendro
  dendro_no_labels$layers <- dendro$layers[-text_layer_i]
  dendro_plotly           <- ggplotly(dendro_no_labels, tooltip = "all")
  
  ## --- 4. FIX COLORS MANUALLY (SIMPLE LOGIC) ---
  ## Plotly saves each class in a list called 'data'. 
  ## We look at each element, check its name, and assign the color.
  built <- plotly_build(dendro_plotly)
  
  for (i in seq_along(built$x$data)) {
    # Get the name of the current trace (e.g., "Klasse A", "Klasse B", "Default")
    current_name <- built$x$data[[i]]$name
    
    if (!is.null(current_name)) {
      
      ## Logic: Find a patient in text_data that belongs to this class,
      ## and steal their color!
      matching_row <- which(class_labels == current_name)[1]
      
      if (!is.na(matching_row)) {
        # If it's a real class, look up which color ggplot gave to this label
        name <- names_vector[matching_row]
        text_color   <- text_data$colour[text_data$label == name][1]
        
        # Force plotly to use this exact color for the line
        dendro_plotly <- plotly::style(dendro_plotly, line = list(color = text_color, width = 0.5), traces = i)
      } else if (current_name == "Default") {
        # If it's the default background tree, make it black and hide from legend
        dendro_plotly <- plotly::style(dendro_plotly, line = list(color = "black", width = 0.5), showlegend = FALSE, traces = i)
      }
    }
  }
  
  ## --- 6. BUILD ANNOTATIONS (MAXIMUM CLOSE DISTANCE) ---
  ## We set target_y to exactly 0.0 and change the anchor to "middle"
  ## to completely eliminate any remaining visual gap.
  annotations <- lapply(seq_len(nrow(text_data)), function(i) {
    plotly_font_size <- max(8, text_data$size[i] * 3.5)
    
    list(
      x         = text_data$x[i],
      y         = 0.0,                    # <--- Positioned exactly at the bottom of the line
      text      = text_data$label[i],
      textangle = -90,
      showarrow = FALSE,
      font      = list(color = substr(text_data$colour[i], 1, 7), size = plotly_font_size),
      xanchor   = "center",
      yanchor   = "top",               # <--- Centers the text box vertically on y=0
      xref      = "x",
      yref      = "y"
    )
  })
  
  ## --- 7. ADJUST PLOTLY AXIS RANGE ---
  ## We tighten the lower limit of the y-axis even further (from -0.5 to -0.1)
  ## so that Plotly cuts off the plotting area directly at the baseline.
  max_tree_y <- max(sapply(built$x$data, function(t) max(t$y, na.rm = TRUE)), na.rm = TRUE)
  
  dendro_plotly <- plotly::layout(
    dendro_plotly,
    annotations = annotations,
    yaxis       = list(range = c(-0.1, max_tree_y * 1.05)),   # <--- Tightened lower range
    margin      = list(l = 5, r = 5, t = 30, b = 120)            # <--- Slightly more margin for the rotated text
  )
}