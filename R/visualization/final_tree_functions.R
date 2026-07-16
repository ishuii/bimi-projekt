#####===========================================================================
# This script contains two functions to prepare the tree structure
# needed for the dendrogram visualization.
#
# - build_tree()       : converts the clustering merge matrix into a binary tree
#                         (see function header for the merge matrix sign convention)
# - get_order_vector() : extracts the leaf order from that tree (left to right)
#####===========================================================================


#####===========================================================================
#                         BUILD_TREE
#
# Takes the merge matrix from hierarchical clustering and converts it into
# a nested binary tree. Each node stores its left/right children and the
# height at which the merge happened. Leaves store their original observation ID.
#
# Merge matrix convention: (-id) => original observation (leaf), 
#                           (id)  => result of an earlier merge step (internal node)
#####===========================================================================

build_tree <- function(cluster_result) {
  
  # extracting mergematrix from cluster result
  mergematrix <- cluster_result$merge

  # extracting height from cluster result
  height <- cluster_result$matched_at
  
  # initializing list to store each node as the tree is built step by step
  nodes <- list()
  
  for (i in 1:nrow(mergematrix)) {
    
    left_index  <- mergematrix[i, 1]
    right_index <- mergematrix[i, 2]
    
    ##### LEFT NODE ################################################################
    
    # negative index => this is a leaf (original observation)
    if (left_index < 0) {
      left_node <- list(
        left   = NULL,
        right  = NULL,
        height = 0,
        id     = abs(left_index)
      )
    } else {
      
      # positive index => this subtree was already built earlier
      left_node <- nodes[[left_index]]
    }
    
    ##### RIGHT NODE ###############################################################
    
    # same logic as for left node
    if (right_index < 0) {
      right_node <- list(
        left   = NULL,
        right  = NULL,
        height = 0,
        id     = abs(right_index)
      )
    } else {
      right_node <- nodes[[right_index]]
    }
    
    ##### MERGE INTO NEW INTERNAL NODE #############################################
    
    # internal nodes have no id 
    new_node <- list(
      left   = left_node,
      right  = right_node,
      height = height[i],
      id     = NULL
    )
    
    nodes[[i]] <- new_node
  }
  
  # returning root node => last merge step is always the root
  return(nodes[[nrow(mergematrix)]])
}


#####===========================================================================
#                         GET_ORDER_VECTOR
#
# Does an in-order traversal of the tree to collect the leaf IDs from
# left to right. The resulting vector tells us in which order the observations
# should appear on the x-axis of the dendrogram.
#####===========================================================================

get_order_vector <- function(tree) {
  
  # no tree => nothing to return
  if (is.null(tree)) return(NULL)
  
  # leaf reached => this is the id we want, returning it
  if (!is.null(tree$id)) return(tree$id)
  
  # internal node => going left first to preserve left-to-right order, then right
  left_value  <- get_order_vector(tree$left)
  right_value <- get_order_vector(tree$right)
  
  # combine left and right value
  return(c(left_value, right_value))
}
