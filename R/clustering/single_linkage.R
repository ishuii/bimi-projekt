# Implementation of the single linkage algorithm

##### Get data and normalize it -----------------------------------------------------

# first column is treated as row names
df <- read.csv("data/TCGA_kidney_unnormalized_TOP10.csv", row.names = 1)

source("R/clustering/normalization_methods.R")
df <- prepare_data(df)[[1]]                     # get label vector with [[2]]
df_normalized <- normalize_log_zscore(df)

# ------------------------------------------------------------------------------------
##### distance matrix

# clustering ... (1) genes --> rows, ... (2) patients --> transpose
# information whether (1) or (2) will come from team GUI (selection by operator)

source("R/clustering/distance_matrix.R")

dist_mat <- dist_cpp(t(df_normalized), "euclidean")

# ------------------------------------------------------------------------------------
##### single linkage function

single_linkage <- function(d_mat) {
  n <- nrow(d_mat)
  initial_clusters <- c(-(1:ncol(d_mat))) # indices as clusters - negative for merge matrix
  
  diag(d_mat) <- Inf # we don't want to have 0 as the min distance
  
  # for dendrogram, we will remember all the min dist we matched clusters at
  matched_at <- numeric(length = n-1)
  
  # merge matrix
  merge <- matrix(0, (n-1), 2)
  
  # we will store the cluster structure
  clusters <- as.list(initial_clusters)
  cluster_history <- list(clusters)
  
  for (k in 1:(n-1)) {
    # select the two clusters with the minimal distance
    d <- which(d_mat == min(d_mat), arr.ind = T)[1,]
    i <- d[1]
    j <- d[2]
    
    # store min distance
    matched_at[k] <- d_mat[i,j]
    
    # combine the two clusters from d 
    new_cluster <- c(clusters[[i]], clusters[[j]])
    # they need new distances (single linkage --> min)
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
    
    # update clusters
    clusters[[i]] <- new_cluster
    clusters[[j]] <- list(NULL)
    
    # store history
    cluster_history[[k+1]] <- clusters
    
  }
  
  return(list(matched_at = matched_at, cluster_history = cluster_history))
}

cluster_res <- single_linkage(dist_mat, initial_clusters)


########### TESTING ######################

df_test <- df[,1:10]

dist_test <- as.matrix(dist(t(df_normalized), method = "euclidean"))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  