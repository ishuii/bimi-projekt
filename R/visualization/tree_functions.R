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

get_order_vector <- function(tree){
  
  if(is.null(tree)) return(NULL)
  
  if(!is.null(tree$label)){
    return(tree$label)
  }
  
  left_value  <- get_order_vector(tree$left)
  right_value <- get_order_vector(tree$right)
  
  return(c(left_value, right_value))
}

###################################Test mit Beispielwerten#############################
# 5 Beobachtungen → 4 Merge-Schritte
# Genexpression: 6 Gene, 4 Bedingungen
labels <- c("BRCA1", "TP53", "MYC", "EGFR", "VEGF", "CDH1")

set.seed(42)
expr_matrix <- matrix(
  c(8.2, 8.5, 2.1, 9.3,
    7.9, 4.0, 1.9, 2.0,
    9.9, 1.8, 9.1, 8.7,
    2.3, 2.0, 10.0, 9.2,
    2.0, 2.2, 8.5, 9.8,
    7.8, 8.3, 5.4, 2.1),
  nrow = 6, byrow = TRUE,
  dimnames = list(labels, c("Cond1", "Cond2", "Cond3", "Cond4"))
)

# Clustering
hc <- hclust(dist(expr_matrix))
mergematrix <- hc$merge

# Tree + Order Vektor
tree         <- build_tree(mergematrix)
order_vector <- collect_labels(tree)

# Sort Labels
ordered_labels <- labels[order_vector]

print(order_vector)
print(ordered_labels)

# Heatmap
heatmap(expr_matrix[order_vector, ],
        Rowv = NA,        # kein internes Clustering — du steuerst die Reihenfolge
        Colv = NA,
        labRow = ordered_labels,
        scale = "row")