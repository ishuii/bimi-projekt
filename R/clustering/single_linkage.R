# Implementation of the single linkage algorithm

##### Get data and normalize it -----------------------------------------------------

# data will be read in by team GUI and received from it
# here we do this step directly just for the first implementation

# first column is treated as row names
df <- read.csv("data/TCGA_kidney_unnormalized_TOP10.csv", row.names = 1)

# extract labels and remove them from the dataset
labels <- df[c(11),]
df <- df[-c(11),]

# data is unnormalized --> difficult to compare
# for clustering, Z-score normalization is best
df_log <- log2(df + 1)
df_normalized <- t(scale(t(df_log)))

# ------------------------------------------------------------------------------------
##### distance matrix

# clustering ... (1) genes --> rows, ... (2) patients --> transpose
# information whether (1) or (2) will come from team GUI (selection by operator)

# requires a distance matrix for the dendrogram
# are we allowed to use the existing dist method?

library(stats) # for the dist function --> otherwise use distance_matrix.R

dist_object <- dist(t(df_normalized), method = "euclidean")
dist_mat <- as.matrix(dist_object)

diag(dist_mat) <- Inf
d <- which(dist_mat == min(dist_mat), arr.ind = T)[1,]
max(d)
# ------------------------------------------------------------------------------------
##### single linkage function

initial_clusters <- c(1:ncol(df)) # numeric for easier understandig, can also use colnames

single_linkage <- function(d_mat, initial_clusters) {
  n <- nrow(d_mat)
  
  diag(d_mat) <- Inf # we don't want to have 0 as the min distance
  
  # for dendrogram, we will remember all the min dist we matched clusters at
  matched_at <- numeric(length = n-1)
  
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
