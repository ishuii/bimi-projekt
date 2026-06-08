#####===========================================================================
# This script contains functions to:
# 1. Calculate precise X and Y coordinates for tree nodes
# 2. Traverse a binary tree recursively to collect line segments and labels
# 3. Render a clean, publication-ready dendrogram using ggplot2
#####===========================================================================

#####===========================================================================
#                         CALCULATE_COORDS FUNCTION
#
# Recursively calculates the (x, y) coordinates for any given node in the tree
# - For leaves: x is determined by its position in the order vector, y is 0
# - For internal nodes: x is the mean x-position of its left and right children, 
#   y is determined by the merge height
#
# @return      : A list containing:
#                - $x : Numeric x-coordinate on the plot axis
#                - $y : Numeric y-coordinate representing the cluster distance
####============================================================================

calculate_coords <- function(tree, order, height){
  
  ### Check if tree is available ###############################################
  if(is.null(tree)) return(NULL)
  
  ### Caculating Coords ########################################################
  
  ## Case 1: Leaf Node (tree$id is present) -------------------------------------
  if(!is.null(tree$id)){
    
    # x-position = index in the ordered leaf vector
    x = which(tree$id == order)
    
    # y-position for original observations is always ground level (0)
    y = 0
    
    return(list(x=x,y=y))
  }
  
  ## Case 2: Internal Node / Cluster -------------------------------------------
  else {
    
    # x-position is the center (mean) between left and right child coordinates
    left_coord= calculate_coords(tree$left, order, height)
    right_coord= calculate_coords(tree$right, order, height)
    x = mean(c(left_coord$x,right_coord$x))
    
    # y-position represents the distance at which this specific merge happened
    y = tree$height
    
    return(list(x=x,y=y))
  }
}

#####===========================================================================
#                         DRAW_SEGMENTS
#
# Traverses the binary tree recursively and collects:
#   - Segment coordinates (geometry for ggplot lines)
#   - Leaf metadata: id, x-position, class (for coloring)
#
# NOTE: Display names are NOT assigned here.
#       The leaf id is passed through so plot_dendro can resolve
#       names_vector[id] at render time.
#####===========================================================================

draw_segments <- function(node_coords, tree, order, height, class_labels) {
  
  ##### BASE CASE: Leaf node ####################################################
  
  if (is.null(tree$left) && is.null(tree$right)) {
    
    classlabel <- if (!is.null(class_labels)) as.character(class_labels[tree$id]) else "Default"
    leaf_class <- if (is.na(classlabel) || length(classlabel) == 0) "Default" else classlabel
    
    return(list(
      segments      = NULL,
      labels        = data.frame(
        id    = tree$id,
        x     = which(order == tree$id),
        y     = 0,
        class = leaf_class
      ),
      current_class = leaf_class
    ))
  }
  
  ##### INTERNAL NODE: calculate child coordinates #############################
  
  left_coords  <- calculate_coords(tree$left,  order, height)
  right_coords <- calculate_coords(tree$right, order, height)
  
  ##### RECURSIVE STEP #########################################################
  
  left_result  <- draw_segments(left_coords,  tree$left,  order, height, class_labels)
  right_result <- draw_segments(right_coords, tree$right, order, height, class_labels)
  
  ##### DETERMINE PARENT CLASS (bubble up for branch coloring) #################
  
  parent_class <- if (left_result$current_class == right_result$current_class) {
    left_result$current_class
  } else {
    "Default"
  }
  
  ##### BUILD SEGMENT DATA FRAME ###############################################
  # Row 1: horizontal bar connecting left and right child
  # Row 2: vertical bar down to left child
  # Row 3: vertical bar down to right child
  
  mid_x <- node_coords$x
  
  segments_df <- data.frame(
    x0    = c(mid_x,           left_coords$x,  mid_x,           right_coords$x),
    y0    = c(node_coords$y,   node_coords$y,  node_coords$y,   node_coords$y),
    x1    = c(left_coords$x,   left_coords$x,  right_coords$x,  right_coords$x),
    y1    = c(node_coords$y,   left_coords$y,  node_coords$y,   right_coords$y),
    class = c(left_result$current_class, left_result$current_class,
              right_result$current_class, right_result$current_class)
  )
  
  ##### AGGREGATE AND RETURN ###################################################
  
  return(list(
    segments      = rbind(segments_df, left_result$segments, right_result$segments),
    labels        = rbind(left_result$labels, right_result$labels),
    current_class = parent_class
  ))
}


#####===========================================================================
#                         PLOT_DENDRO
#
# Renders the final ggplot2 dendrogram.
# Resolves leaf display names from names_vector here.
#####===========================================================================

plot_dendro <- function(draw_result, max_height, title="", palette=NULL, names_vector=NULL, show_legend=TRUE) {
  
  # 1. Tabellen entpacken
  segments_df <- draw_result$segments
  labels_df   <- draw_result$labels
  
  # 2. Display names auflösen
  labels_df$label <- if (!is.null(names_vector)) {
    names_vector[labels_df$id]
  } else {
    as.character(labels_df$id)
  }
  
  # 3. Namen direkt unter die Blattlinien setzen
  labels_df$y <- -0.1
  
  # 4. Alle vorhandenen Klassen sammeln (für schwarzen Fallback)
  all_classes <- unique(c(segments_df$class, labels_df$class))
  
  # 5. Palette festlegen
  if (!is.null(palette)) {
    if (!"Default" %in% names(palette)) palette["Default"] <- "black"
  } else {
    palette <- setNames(rep("black", length(all_classes)), all_classes)
  }
  
  # 6. Plot
  plot <- ggplot() +
    geom_segment(data = segments_df, aes(x=x0, y=y0, xend=x1, yend=y1, color=class)) +
    geom_text(data = labels_df, aes(x=x, y=y, label=label, color=class), angle=90, hjust=1, size=2.5) +
    scale_color_manual(values = palette) +
    ylim(c(-8, max_height)) +
    labs(y = "Distanz", x = "") +
    ggtitle(title) +
    theme_classic() +
    theme(
      panel.grid   = element_blank(),
      axis.line    = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x  = element_blank()
    )
  
  # 7. Legende ausblenden wenn nicht gewünscht
  if (!show_legend) {
    plot <- plot + theme(legend.position = "none")
  }
  
  return(plot)
}
