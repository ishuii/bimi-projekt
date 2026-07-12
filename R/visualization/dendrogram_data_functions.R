#####===========================================================================
# This script contains the core functions for rendering the dendrogram data.
#
# - calculate_coords()     : calculates x/y coordinates for each node in the tree
# - draw_segments()        : traverses the tree and collects all line segments and leaf metadata
# - get_color()            : generates a named color vector based on class labels and a palette
# - generate_dendro_data() : runs the full data pipeline and prepares the final data package
#####===========================================================================


#####===========================================================================
#                         CALCULATE_COORDS
#
# Figures out where each node should sit on the plot.
# Leaves always sit at y=0, internal nodes sit at their merge height.
# The x position of an internal node is the midpoint between its children.
#####===========================================================================

calculate_coords <- function(tree, order, height) {
  
  # no tree => nothing to calculate
  if (is.null(tree)) return(NULL)
  
  ## leaf node => no children, so x comes directly from its position in the order vector
  if (!is.null(tree$id)) {
    
    return(list(
      x = which(tree$id == order),
      y = 0
    ))
  }
  
  ## internal node — x is the midpoint between children, y is where this merge happened
  left_coord  <- calculate_coords(tree$left,  order, height)
  right_coord <- calculate_coords(tree$right, order, height)
  
  # returning midpoint as x and merge height as y for this internal node
  return(list(
    x = mean(c(left_coord$x, right_coord$x)),
    y = tree$height
  ))
}

####===========================================================================
#                         DRAW_SEGMENTS
#
# Traverses the entire tree recursively and collects two things:
# the line segments that make up the dendrogram, and the leaf metadata
# (id, position, class) needed for coloring and labeling later.
#
# The horizontal connector between two children is split into two halves —
# each half gets the color of its respective child. This way mixed-class
# branches are still colored as far down as the classes agree.
#
# Note: display names are not resolved here. The leaf id is passed through
# so plot_dendro can look up names_vector[id] when drawing the labels.
#####===========================================================================

draw_segments <- function(node_coords, tree, order, height, class_labels) {
  
  ##### LEAF NODE ##############################################################
  
  if (is.null(tree$left) && is.null(tree$right)) {
    
    # no children => this is a leaf, looking up its class for coloring
    # falling back to "Default" if the class is missing or NA
    classlabel <- if (!is.null(class_labels)) as.character(class_labels[tree$id]) else "Default"
    leaf_class <- if (is.na(classlabel) || length(classlabel) == 0) "Default" else classlabel
    
    return(list(
      segments      = NULL,
      labels        = data.frame(id = tree$id, x = node_coords$x, y = 0, class = leaf_class),
      current_class = leaf_class
    ))
  }
  
  ##### INTERNAL NODE ##########################################################
  
  # not a leaf => calculating child coordinates to draw the connector lines
  left_coords  <- calculate_coords(tree$left,  order, height)
  right_coords <- calculate_coords(tree$right, order, height)
  
  # recursing into both subtrees to collect their segments and labels
  left_result  <- draw_segments(left_coords,  tree$left,  order, height, class_labels)
  right_result <- draw_segments(right_coords, tree$right, order, height, class_labels)
  
  # both sides got the same class => color the branch, otherwise fall back to Default
  parent_class <- if (left_result$current_class == right_result$current_class) {
    left_result$current_class
  } else {
    "Default"
  }
  
  # splitting horizontal bar into two halves, each colored by its child
  # adding vertical drops down to each child
  mid_x <- node_coords$x
  
  segments_df <- data.frame(
    x0    = c(mid_x,                      left_coords$x,       mid_x,                       right_coords$x),
    y0    = c(node_coords$y,              node_coords$y,       node_coords$y,               node_coords$y),
    x1    = c(left_coords$x,              left_coords$x,       right_coords$x,              right_coords$x),
    y1    = c(node_coords$y,              left_coords$y,       node_coords$y,               right_coords$y),
    class = c(left_result$current_class,  left_result$current_class,
              right_result$current_class, right_result$current_class)
  )
  
  # merging segments and labels from both subtrees and passing the parent class upward
  return(list(
    segments      = rbind(segments_df, left_result$segments, right_result$segments),
    labels        = rbind(left_result$labels, right_result$labels),
    current_class = parent_class
  ))
}



####===========================================================================
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
  
  # extract unique groups and clean the vector by removing empty strings and NAs
  detected_classes <- unique(class_labels)
  detected_classes <- detected_classes[!is.na(detected_classes) & detected_classes != ""]
  n <- length(detected_classes)
  
  # if no classes remain after cleaning, fall back to default black mapping
  if (n == 0) {
    return(c("Default" = "black"))
  }
  
  # define standard hex colors as a backup if no library palette is selected
  default_colors <- c(
    "#0000FF", "#FF0000", "#00FF00", "#D60072", "#B2DF8A",
    "#005300", "#FFD300", "#0096FF", "#9B4D00", "#00FFD2", 
    "#A100FA", "#7B8100", "#960000", "#00646B", "#FDBF6F",
    "#FF6C00", "#540066", "#00A278", "#000094", "#FFC0CB"
  )
  
  # evaluate palette selection; handle viridis natively first
  if (palette == "viridis") {
    colors <- viridis::viridis(n, end = 0.8)
    
    # for brewer palettes, extract the full scale to resample from it later
  } else if (palette %in% c("RdYlBu", "RdBu", "PRGn")) {
    full <- switch(palette,
                   "RdYlBu" = RColorBrewer::brewer.pal(11, "RdYlBu")[-c(5, 6, 8)], # remove muddy center colors
                   "RdBu"   = RColorBrewer::brewer.pal(11, "RdBu"),
                   "PRGn"   = RColorBrewer::brewer.pal(11, "PRGn")
    )
    # interpolate the brewer palette to match the exact number of detected classes
    colors <- if (n <= length(full)) {
      full[round(seq(1, length(full), length.out = n))]
    } else {
      default_colors[1:n] # fallback to defaults if n exceeds brewer limits
    }
    
    # trigger warning for unsupported palette strings and apply default colors
  } else {
    warning(paste("Unbekannte Palette:", palette, "-> verwende Standardfarben"))
    colors <- default_colors[1:n]
  }
  
  # build the final translation vector and explicitly bind "Default" to black
  color_vector <- c(setNames(colors, detected_classes), "Default" = "black")
  
  return(color_vector)
}


#####===========================================================================
#                         GENERATE_DENDRO_DATA
#
# The engine function — extracts the cluster heights, sets up the translation
# table for the requested palette, and triggers the recursive tree traversal.
# Returns a packaged list containing all structural and aesthetic components
# required by the final rendering engines (ggplot/plotly).
#####===========================================================================
generate_dendro_data <- function(cluster_result, tree_result, order_vector, class_labels = NULL, palette = NULL) {
  cluster_height <- cluster_result$matched_at
  
  # 1. generating named color vector (e.g. c("ClassA" = "#FF0000", "Default" = "black"))
  color_vector   <- get_color(class_labels, palette)
  
  # 2. calculating root coordinates and collecting all branch segments
  coords         <- calculate_coords(tree_result, order_vector, cluster_height)
  draw_result    <- draw_segments(
    node_coords  = coords,
    tree         = tree_result,
    order        = order_vector,
    height       = cluster_height,
    class_labels = class_labels
  )
  
  # returning pre-calculated structure and mapping table for the plotting functions
  return(list(
    draw_result  = draw_result,
    max_height   = max(cluster_height),
    color_vector = color_vector
  ))
}



