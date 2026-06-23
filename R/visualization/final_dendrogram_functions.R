####===========================================================================
# This script contains the core functions for rendering the dendrogram.
#
# - calculate_coords() : calculates x/y coordinates for each node in the tree
# - draw_segments()    : traverses the tree and collects all line segments and leaf metadata
# - plot_dendro()      : takes the collected data and renders the final ggplot2 object
#####===========================================================================

library(ggplot2)

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
      labels        = data.frame(
        id    = tree$id,
        x     = which(order == tree$id),
        y     = 0,
        class = leaf_class
      ),
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

#####===========================================================================
#                         PLOT_DENDRO
#
# Takes the collected segments and labels and turns them into a ggplot object.
# Display names are resolved here from names_vector using the leaf ids.
# If no palette is given, everything is drawn in black.
# The legend is only shown when class labels were provided.
#####===========================================================================

plot_dendro <- function(draw_result, max_height, title="", palette=NULL, names_vector=NULL) {
  
  # unpacking segments and labels from draw_segments output
  segments_df <- draw_result$segments
  labels_df   <- draw_result$labels
  
  # resolving display names => falling back to raw id if no names provided
  labels_df$label <- if (!is.null(names_vector)) {
    names_vector[labels_df$id]
  } else {
    as.character(labels_df$id)
  }
  
  # pushing labels just below the leaf lines
  labels_df$y <- -0.1
  
  # collecting all classes for the fallback palette
  all_classes <- unique(c(segments_df$class, labels_df$class))
  
  # setting palette => mapping everything to black if none provided
  if (!is.null(palette)) {
    if (!"Default" %in% names(palette)) palette["Default"] <- "black"
  } else {
    palette <- setNames(rep("black", length(all_classes)), all_classes)
  }
  
  # generate the empty plot and build layer by layer 
  plot <- ggplot() +
    
    # layer 1: drawing all branch segments, colored by class
    geom_segment(data = segments_df, aes(x=x0, y=y0, xend=x1, yend=y1, color=class)) +
    
    # layer 2: drawing leaf labels rotated 90 degrees, colored by class
    # suppressing legend glyph => geom_text würde sonst ein "a" in der Legende erzeugen
    geom_text(data = labels_df, aes(x=x, y=y, label=label, color=class),
              angle=90, hjust=1, size=2, show.legend=FALSE) +
    
    # layer 3: applying the color palette to both segments and labels
    scale_color_manual(values = palette) +
    
    # layer 4: setting y range => leaving enough space below for the rotated labels
    scale_y_continuous(limits = c(-8, max_height), 
                       breaks = seq(0, max_height, by = 5)) +
    labs(y = "Distance", x = "") +
    ggtitle(title) +
    
    # layer 5: clean white background, removing grid, axis lines and x ticks
    theme_classic() +
    theme(
      panel.grid   = element_blank(),
      axis.line    = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x  = element_blank(),
      axis.ticks.y = element_blank()
    )
  
  # legend logic
  if (!show_legend) {
    plot <- plot + theme(legend.position = "none")
  }
  
  return(plot)
}

