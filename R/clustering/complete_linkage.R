# Implementation of the complete linkage algorithm


library(cluster)

# Implementation of the single linkage algorithm

##### Get data and normalize it -----------------------------------------------------

# data will be read in by team GUI and received from it
# here we do this step directly just for the first implementation

# first column is treated as row names
df <- read.csv("C:/Users/rosin/Downloads/TCGA_kidney_unnormalized_TOP10.csv", row.names = 1)

df <- df[, 1:20]  # only first 20 cols OPTIONAL

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

library(stats)

dist_object <- dist(t(df_normalized), method = "euclidean")
dist_mat <- as.matrix(dist_object)

diag(dist_mat) <- Inf

d <- arrayInd(which.max(dist_mat), dim(dist_mat))

max(d)


##### complete linkage function

complete_linkage <- function(d_mat) {
  n <- nrow(d_mat)
  diag(d_mat) <- Inf  # ignore self-distances
  
  matched_at <- numeric(n - 1)
  merge <- matrix(0, n - 1, 2)
  
  cluster_id <- -seq_len(n)  # initial clusters
  
  for (k in 1:(n - 1)) {
    
    pos <- which.max(d_mat)
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

#test mit anderen cluster codes
cluster_res <- complete_linkage(dist_mat)

hc <- hclust(dist_object, method = "complete")

# numerischer Vergleich der Höhen (best practice)
all.equal(cluster_res$matched_at, hc$height)
