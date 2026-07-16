
#####===========================================================================
# This script contains functions to prepare the coordinates, segments, and 
# export options needed for rendering the dendrogram.
#
# - calculate_coords()     : recursively calculates x and y coordinates for each node
# - draw_segments()        : builds dendrogram lines and collects leaf metadata
# - generate_dendro_data() : runs the full pipeline to assemble the plotting data
# - save_dendro_pdf()      : exports the final plot to PDF with dynamic height scaling
#####===========================================================================

#####===========================================================================
#                         CALCULATE_COORDS
#
# Recursively traverses the tree to determine the spatial coordinates (x, y) 
# for every node. Leaves are placed on the baseline (y = 0) sequenced by 
# their order, while internal nodes sit at their respective merge heights.
# The x-coordinate of an internal node is calculated as the center point (mean) 
# of its two direct children's x-coordinates.
#####===========================================================================

calculate_coords <- function(tree, order, height) {
  
  # no tree => nothing to calculate
  if (is.null(tree)) return(NULL)
  
  ##### LEAF NODE ################################################################
  
  # if an ID is present => leaf
  if (!is.null(tree$id)) {
    return(list(
      
      # x-position is its index in the leaf order
      x     = which(tree$id == order),
      
      # leaves always sit at y = 0
      y     = 0,
      left  = NULL,
      right = NULL
    ))
  }
  
  ##### INTERNAL NODE ############################################################
  
  # recursively calculate coordinates for both subtrees
  left_full  <- calculate_coords(tree$left,  order, height)
  right_full <- calculate_coords(tree$right, order, height)
  
  # parent node sits at the horizontal center of its children and at its merge height
  return(list(
    x     = mean(c(left_full$x, right_full$x)),
    y     = tree$height,
    left  = left_full,
    right = right_full
  ))
}

#####===========================================================================
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
  
  # the recursion itself only needs to build up a plain
  # list of pieces => building a internal helper function, to collect from tree 
  # the actual data.frame assembly is deferred
  # and done just once, after the whole tree has been walked
  collect <- function(node_coords, tree) {
    
    ##### LEAF NODE ##############################################################
    
    if (is.null(tree$left) && is.null(tree$right)) {
      
      # children = NULL => this is a leaf, looking up its class for coloring
      # falling back to "Default" if the class is missing or NA
      classlabel <- if (!is.null(class_labels)) as.character(class_labels[tree$id]) else "Default"
      leaf_class <- if (is.na(classlabel) || length(classlabel) == 0) "Default" else classlabel
      
      # return leaf coordinates and its class
      # keep results as lists for now instead of merging right away.
      # the actual data.frame only gets built once, at the end
      return(list(
        segments      = list(),
        labels        = list(data.frame(id = tree$id, x = node_coords$x, y = 0, class = leaf_class)),
        current_class = leaf_class
      ))
    }
    
    ##### INTERNAL NODE ##########################################################
    
    # not a leaf => extracting child coordinates to draw the connector lines
    left_coords  <- node_coords$left
    right_coords <- node_coords$right
    
    # recursing into both subtrees to collect their segments and labels
    left_result  <- collect(left_coords,  tree$left)
    right_result <- collect(right_coords, tree$right)
    
    # both sides got the same class => color the branch, otherwise fall back to Default
    parent_class <- if (left_result$current_class == right_result$current_class) {
      left_result$current_class
    } else {
      "Default"
    }
    
    ##### CALCULATE SEGMENTS #####################################################
    
    # splitting horizontal bar into two halves, each colored by its child
    # adding vertical drops down to each child
    mid_x <- node_coords$x
    
    segments_df <- data.frame(
      
      # storing all 4 lines for this node in one data.frame: two rows for the
      # horizontal bar (split into a left and right half so each half can carry
      # its own child's color), and two rows for the vertical drops down to
      # each child
      x0    = c(mid_x,                      left_coords$x,       mid_x,                       right_coords$x),
      y0    = c(node_coords$y,              node_coords$y,       node_coords$y,               node_coords$y),
      x1    = c(left_coords$x,              left_coords$x,       right_coords$x,              right_coords$x),
      y1    = c(node_coords$y,              left_coords$y,       node_coords$y,               right_coords$y),
      class = c(left_result$current_class,  left_result$current_class,
                right_result$current_class, right_result$current_class)
    )
    
    # merging segments and labels from both subtrees and passing the parent class upward
    return(list(
      segments      = c(list(segments_df), left_result$segments, right_result$segments),
      labels        = c(left_result$labels, right_result$labels),
      current_class = parent_class
    ))
  }
  
  ##### ASSEMBLE RESULTS #######################################################
  
  # run recursion starting from the root node
  result <- collect(node_coords, tree)
  
  # bind all individual lists into single data.frames once 
  return(list(
    segments      = do.call(rbind, result$segments),
    labels        = do.call(rbind, result$labels),
    current_class = result$current_class
  ))
}

#####===========================================================================
#                         GENERATE_DENDRO_DATA
#
# The engine function — extracts the cluster heights, sets up the translation
# table for the requested palette, and triggers the recursive tree traversal.
# Returns a packaged list containing all structural and aesthetic components
# required by the final rendering engines (ggplot/plotly).
#####===========================================================================

generate_dendro_data <- function(cluster_result, tree_result, order_vector, class_labels = NULL) {
  
  ##### PREPARE PARAMETERS #######################################################
  
  # extracting height from cluster_result$matched_at
  cluster_height <- cluster_result$matched_at
  
  ##### PROCESS PIPELINE #########################################################
  
  # collecting all branch segments and leaf metadata recursively
  coords         <- calculate_coords(tree_result, order_vector, cluster_height)
  draw_result    <- draw_segments(
    node_coords  = coords,
    tree         = tree_result,
    order        = order_vector,
    height       = cluster_height,
    class_labels = class_labels
  )
  
  ##### RETURN PACKAGE ###########################################################
  
  # returning pre-calculated structure and max height for axis scaling
  return(list(
    draw_result  = draw_result,
    max_height   = max(cluster_height)
  ))
}

#####===========================================================================
#                         SAVE_DENDRO_PDF
#
# Exports the generated ggplot dendrogram to a PDF file. Automatically
# ensures the correct file extension and dynamically calculates the PDF height
# based on the longest leaf label in the plot to prevent text clipping or
# excessive blank space.
#####===========================================================================

save_dendro_pdf <- function(plot, dateiname, pfad) {
  
  ##### FILE PREPARATION #########################################################
  
  # append .pdf extension if it is missing from the filename
  if (!grepl("\\.pdf$", dateiname, ignore.case = TRUE)) {
    dateiname <- paste0(dateiname, ".pdf")
  }
  
  ##### CALCULATE DYNAMIC HEIGHT #################################################
  
  # safely find the text layer inside the ggplot object
  text_layer_idx <- which(sapply(plot$layers, function(l) inherits(l$geom, "GeomText")))
  
  if (length(text_layer_idx) > 0) {
    
    # extract the labels directly from the layer's local data to find the longest name
    labels_vec <- plot$layers[[text_layer_idx[1]]]$data$label
    max_char_len <- max(nchar(as.character(labels_vec)), na.rm = TRUE)
    
    # fallback value if no text layer is found
  } else {
    max_char_len <- 10 
  }
  
  # calculate height: start at 4.5 inches, scale up by label length, clamp between 5 and 11
  dynamic_height <- 4.5 + (max_char_len * 0.12)
  dynamic_height <- pmin(pmax(dynamic_height, 5), 11) 
  
  # combine path and filename to create the final destination path
  zielpfad <- file.path(pfad, dateiname)
  
  ##### SAVE PLOT ################################################################
  
  # save the plot with the perfectly calculated dynamic height and a fixed wide format
  ggsave(
    filename  = zielpfad,
    plot      = plot,
    width     = 22,            
    height    = dynamic_height,
    units     = "in",
    device    = "pdf",
    limitsize = FALSE
  )
}