#####===========================================================================
# This script contains functions to:
# 1. Traversed a binary tree recursively to collect line segments and labels
# 2. Render a clean, publication-ready dendrogram using ggplot2
#####===========================================================================

library(ggplot2)

#####===========================================================================
#                         DRAW_SEGMENTS_V2
#
# THOUGHT PROCESS: 
# In Base R, we drew lines instantly onto the screen. In ggplot2, we can't do that.
# Strategy: We use recursion not to draw, but to COLLECT coordinates into data frames, 
# stack them via rbind(), and hand them over to the plotting function at the very end.
#####===========================================================================

draw_segments_V2 <- function(coords, tree, labels, height, order, names) {
  
  ##### LOGICAL STOPPING POINTS (BASE CASES) ###################################
  
  ## 1. Basecase: If the tree or branch is empty, there is nothing to collect.--
  # Return empty slots
  if (is.null(tree)) return(list(segments = NULL, labels = NULL))
  
  # 2. Basecase: If we reach a leaf, we don't draw any branches here -----------
  # But we must catch the text label and calculate its dynamic X position based on the 
  # sorted order
  
  if (!is.null(tree$id)) {
    return(list(
      segments = NULL,
      labels   = data.frame(
        x     = which(tree$id == order),
        y     = 0,
        label = names[tree$id]
      )
    ))
  }
  
  #### PARENT NODE LOGIC #######################################################
  #For an internal node, we need to know where its left and right children sit
  # so we can draw the connector lines. We use our existing calculator function for this
  
  left_coords  <- calculate_coords(order, height, tree$left)
  right_coords <- calculate_coords(order, height, tree$right)
  
  
  # Instead of calling segments(), we build a local data frame with 3 rows.
  # Row 1: horizontal bar -> left and right chil
  # Row 2: vertical bar -> down to the left child's height
  # Row 3: vertical bar -> down to the right child's height
  segments_df <- data.frame(
    x0 = c(left_coords$x,  left_coords$x,  right_coords$x),
    y0 = c(coords$y,       coords$y,       coords$y),
    x1 = c(right_coords$x, left_coords$x, right_coords$x),
    y1 = c(coords$y,       left_coords$y, right_coords$y)
  )
  
  #### RECURSIVE STEP ##########################################################
  
  # To get the rest of the tree, we delegate the tracking to the left and right children
  # They will independently loop through their own sub-branches and return their maps
  left_result <- draw_segments_V2(left_coords, tree$left, labels, height, order, names)
  right_result <- draw_segments_V2(right_coords, tree$right, labels, height, order, names)
  
  #### DATA AGGREGATION TO FINAL RESULT #########################################
  # This builds a single, complete dataset as we move back up to the root.
  return(list(
    segments = rbind(segments_df, left_result$segments, right_result$segments),
    labels   = rbind(left_result$labels, right_result$labels)
  ))
}

#####===========================================================================
#                         PLOT_DENDRO_V2
#
# THOUGHT PROCESS:
# This is the master function. It acts as the container. It triggers the 
# coordinate-collector and feeds the final, massive data frames into ggplot's geoms.
#####===========================================================================

plot_dendro_V2 <- function(coords, tree, order, height, labels, names, title=""){
  
  #First, run the recursion to get the big list of segments and labels.
  result <- draw_segments_V2(
    coords,
    tree,
    labels,
    height,
    order,
    names)
  
  #Build the plot layer by layer using standard ggplot2 mechanics
  plot <- ggplot() +
    
    # Layer 1: Draw all the collected tree branches at once
    geom_segment(data = result$segments,aes(x=x0, y=y0, xend=x1, yend=y1)) +
    
    # Layer 2: Draw all the collected text labels at once, rotated by 90 degrees
    # hjust = 1.1 to push the text dynamically below the 0-line
    geom_text(data = result$labels,aes(x=x, y=y, label=label), angle=90, hjust=1.1) +
    
    # Layer 3: Formatting and Styling
    ylim(c(-1.5, max(height))) +
    labs(y = "Distanz", x = "") +
    theme(panel.grid = element_blank()) + #remove grid lines
    theme_classic()+ #white background
    ggtitle(title) 
  
  # Output final plot object
  print(plot)
}