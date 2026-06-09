#####===========================================================================
# This script contains functions to:
# 1. build a binary tree from a merge matrix
# 2. extract the leaf order (order vector) from that tree
# -> The order vector can later be used for dendrogram or heatmap alignment
#####===========================================================================

#####===========================================================================
#                         BUILD_TREE FUNCTION
#
# This function builds a binary tree based on the merge steps
####===============================================================================

build_tree_V2 <- function(mergematrix,height,leaf_names){      
  
  ### initialize a data structure to store nodes / subtrees
  nodes <- list()
  
  #### iterate over each merge step (row of the merge matrix)
  for (i in 1:nrow(mergematrix)){
    
    # extract left and right indices for current merge
    left_index  <- mergematrix[i,1]
    right_index <- mergematrix[i,2]
    
    ## --- LEFT NODE -----------------------------------------------------------
    
    # negative index → leaf node (original observation)
    if (left_index < 0) {
      
      idx <- abs(left_index)  
      
      left_node <- list(
        left = NULL,
        right = NULL,
        height = 0,
        id = leaf_names[idx]    ###IDs of observation -> labels = rownames(data)
      )
    } 
    
    # positive index → reference to a previously created node
    else {
      left_node <- nodes[[left_index]]
    }
    
    ## --- RIGHT NODE ----------------------------------------------------------
    if (right_index < 0) {
      
      idx <- abs(right_index)
      
      # same logic as for left node: create leaf node
      right_node <- list(
        left = NULL,
        right = NULL,
        height = 0,
        id = leaf_names[idx] 
      )
    } 
    
    # reference existing subtree
    else {
      right_node <- nodes[[right_index]]
    }
    
    
    ## --- CREATE NEW INTERNAL NODE --------------------------------------------
    
    # combine left and right child into a new subtree
    new_node <- list(
      left = left_node,
      right = right_node,
      height=height[i]
    )
    
    # store newly created node
    nodes[[i]] <- new_node
  }
  
  # return root node (last merge step)
  return(nodes[[nrow(mergematrix)]])
}


#####===========================================================================
#                         GET_ORDER_VECTOR FUNCTION
#
# This function extracts the order of leaf nodes from a binary tree that was built
####===============================================================================

get_order_vector <- function(tree){
  
  # Base case 1 ----------------------------------------------------------------
  # if the node is NULL → nothing to return
  if(is.null(tree)) return(NULL)
  
  # Base case 2 ----------------------------------------------------------------
  # if the node is a leaf → return its label
  if(!is.null(tree$id)){
    return(tree$id)
  }
  
  # Recursive step -------------------------------------------------------------
  
  # traverse left subtree first to preserve left-to-right order -> in order traversal   
  left_value  <- get_order_vector(tree$left)
  
  # Then recursively traverse right subtree
  right_value <- get_order_vector(tree$right)
  
  # combine both results into final order vector
  return(c(left_value, right_value))
}