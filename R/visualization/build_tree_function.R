#####This function builds up a tree, following different tree nodes#######

####function header with given variable: mergematrix ###################################
build_tree <- function(mergematrix){
  
  ### initialize a data structure to store nodes / subtrees
  nodes <- list()
  
  ### loop through each merge step   
  for (i in 1:nrow(mergematrix)){
    
    # store left and right index of row i
    left_index  <- mergematrix[i,1]
    right_index <- mergematrix[i,2]
    
    ## --- LEFT NODE ---
    if (left_index < 0) {
      
      # create left leaf node
      left_node <- list(
        left = NULL,
        right = NULL,
        height = 0,
        label = abs(left_index)
      )
      
    } else {
      left_node <- nodes[[left_index]]
    }
    
    
    ## --- RIGHT NODE ---
    if (right_index < 0) {
      
      # create right leaf node
      right_node <- list(
        left = NULL,
        right = NULL,
        height = 0,
        label = abs(right_index)
      )
      
    } else {
      right_node <- nodes[[right_index]]
    }
    
    
    ## --- CREATE NEW NODE ---
    new_node <- list(
      left = left_node,
      right = right_node,
      height = i,   
      label=NULL 
    )
    
    # store node
    nodes[[i]] <- new_node
  }
  return(nodes[[nrow(mergematrix)]])
}


mergematrix <- matrix(
  c(
    -1, -2,
    -3,  1,
    -4,  2
  ),
  ncol = 2,
  byrow = TRUE
)

collect_labels <- function(tree){
  
  if(is.null(tree))return()
  
  if(!is.null(tree$label)){
    return(tree$label)
    
  }else{
    left_value=collect_labels(tree$left)
    right_value=collect_labels(tree$right)
    
    order_vector=c(left_value,right_value)
  }
  return(order_vector)

}

tree<-build_tree(mergematrix)
