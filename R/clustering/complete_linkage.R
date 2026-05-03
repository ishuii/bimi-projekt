# Implementation of the complete linkage algorithm

##### complete linkage function

complete_linkage <- function(d_mat) {
  n <- nrow(d_mat)
  diag(d_mat) <- Inf  # ignore self-distances
  
  matched_at <- numeric(n - 1)
  merge <- matrix(0, n - 1, 2)
  
  cluster_id <- -seq_len(n)  # initial clusters
  
  for (k in 1:(n - 1)) {
    
    pos <- which.min(d_mat)
    d <- arrayInd(pos, dim(d_mat))
    i <- d[1]
    j <- d[2]
    
    # store merge info
    matched_at[k] <- d_mat[i, j]
    merge[k, ] <- sort(c(cluster_id[i], cluster_id[j]), decreasing = TRUE)
    
    # update cluster id
    cluster_id[i] <- k
    
    new_dist <- pmax(d_mat[i, ], d_mat[j, ])
    
    # update distance matrix
    d_mat[i, ] <- new_dist
    d_mat[, i] <- new_dist
    d_mat[i, i] <- Inf
    
    # deactivate merged cluster j
    d_mat[j, ] <- Inf
    d_mat[, j] <- Inf
    cluster_id[j] <- NA
  }
  
  return(list(matched_at = matched_at, merge = merge))
}
