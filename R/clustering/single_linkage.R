# Implementation of the single linkage algorithm

##### single linkage function

single_linkage <- function(d_mat) {
  n <- nrow(d_mat)
  diag(d_mat) <- Inf # we don't want to have 0 as the min distance
  
  # initialize the variables we will return later
  matched_at <- numeric(length = n-1)
  merge <- matrix(0, (n-1), 2)
  
  cluster_id <- -seq_len(n)  # negative for the merge matrix: the original clusters
  
  for (k in 1:(n-1)) {
    # select the two clusters with the minimal distance
    d <- which(d_mat == min(d_mat), arr.ind = T)[1,]
    i <- d[1]
    j <- d[2]
    
    # store height and the clusters that where matched
    matched_at[k] <- d_mat[i,j]
    merge[k,] <- sort(c(cluster_id[i], cluster_id[j]), decreasing = TRUE)
    
    # update the cluster id for the newly matched cluster
    cluster_id[i] <- k
    
    # new distance for the new cluster (single linkage --> min)
    new_dist <- pmin(d_mat[i, ], d_mat[j, ])
    
    # update distance matrix
    # remove old clusters and add new cluster values (new_dist)
    # update cluster i
    d_mat[i, ] <- new_dist
    d_mat[, i] <- new_dist
    d_mat[i, i] <- Inf
    
    # deactivate cluster j
    d_mat[j, ] <- Inf # we don't remove it to keep the matrix size the same
    d_mat[, j] <- Inf
    cluster_id[j] <- NA
  }
  
  return(list(matched_at = matched_at, merge = merge))
}
  