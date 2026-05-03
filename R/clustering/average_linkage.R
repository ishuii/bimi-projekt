# Implementation of average-linkage

# Average Linkage

average_linkage <- function(d_mat) {
  n <- nrow(d_mat)
  diag(d_mat) <- Inf
  
  matched_at <- numeric(length = n-1)
  merge <- matrix(0, (n-1), 2)
  
  cluster_id <- -seq_len(n)
  cluster_size <- rep(1, n)  # <-- NEU: jedes Objekt startet mit Größe 1
  
  for (k in 1:(n-1)) {
    d <- which(d_mat == min(d_mat), arr.ind = TRUE)[1,]
    i <- d[1]
    j <- d[2]
    
    matched_at[k] <- d_mat[i, j]
    merge[k, ] <- sort(c(cluster_id[i], cluster_id[j]), decreasing = TRUE)
    
    # neue Clustergröße
    new_size <- cluster_size[i] + cluster_size[j]
    
    # gewichtetes Mittel berechnen
    new_dist <- (cluster_size[i] * d_mat[i, ] +
                   cluster_size[j] * d_mat[j, ]) / new_size
    
    # Update Matrix
    d_mat[i, ] <- new_dist
    d_mat[, i] <- new_dist
    d_mat[i, i] <- Inf
    
    d_mat[j, ] <- Inf
    d_mat[, j] <- Inf
    
    # Updates
    cluster_id[i] <- k
    cluster_id[j] <- NA
    
    cluster_size[i] <- new_size
    cluster_size[j] <- 0
  }
  
  return(list(matched_at = matched_at, merge = merge))
}

