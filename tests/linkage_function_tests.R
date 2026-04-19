# Testing: Linkage functions

##### Get data and normalize it -----------------------------------------------------

# first column is treated as row names
df <- read.csv("data/TCGA_kidney_unnormalized_TOP10.csv", row.names = 1)

source("R/clustering/normalization_methods.R")
df <- prepare_data(df)[[1]]                     # get label vector with [[2]]
df_normalized <- normalize_log_zscore(df)

##### distance matrix

source("R/clustering/distance_matrix.R")
dist_mat <- dist_cpp(t(df_normalized), "euclidean")

##### single linkage function

source("R/clustering/single_linkage.R")

cluster_result <- single_linkage(dist_mat)  
mmat <- cluster_result$merge

library(stats)
h <- hclust(stats::dist(t(df_normalized), "euclidean"), "single")

all.equal(h$height, cluster_result$matched_at)
# TRUE

all.equal(h$merge, cluster_result$merge)
# [1] "Mean relative difference: 1.624504"

normalize <- function(m) t(apply(m, 1, sort))
all.equal(normalize(h$merge), normalize(cluster_result$merge))
# TRUE
# virtually the same, only difference is leaf order (relevant for visualization?)
