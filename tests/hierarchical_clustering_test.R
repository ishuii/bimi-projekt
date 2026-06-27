# Testing: Linkage functions

##### Get data and normalize it -----------------------------------------------------

# first column is treated as row names
df <- read.csv("data/TCGA_kidney_unnormalized_TOP10.csv", row.names = 1)

source("R/clustering/normalization_methods.R")
df <- prepare_data(df)[[1]]                     # get label vector with [[2]]
df_normalized <- normalize_log_zscore(df)

##### distance matrix

library(distRcpp)
dist_mat <- dist_cpp(t(df_normalized), "euclidean")

# ----------------------------------------------------------------------------------
# MAIN TEST

source("R/clustering/hierarchical_clustering.R")

# Single Linkage ---------------------------------------------------------------
cluster_single <- hierarchical_clustering(dist_mat, "single")  

library(stats)
h <- hclust(stats::dist(t(df_normalized), "euclidean"), "single")

all.equal(h$height, cluster_single$matched_at)
# TRUE

all.equal(h$merge, cluster_single$merge)
# [1] "Mean relative difference: 1.624504"

normalize <- function(m) t(apply(m, 1, sort))
all.equal(normalize(h$merge), normalize(cluster_single$merge))
# TRUE

# Average Linkage ---------------------------------------------------------------
cluster_average <- hierarchical_clustering(dist_mat, "average")  

h_average <- hclust(stats::dist(t(df_normalized), "euclidean"), "average")

all.equal(h_average$height, cluster_average$matched_at)
# TRUE

all.equal(h_average$merge, cluster_average$merge)
# [1] "Mean relative difference: 0.9272949"

all.equal(normalize(h_average$merge), normalize(cluster_average$merge))
# TRUE

# Complete Linkage ---------------------------------------------------------------
cluster_complete <- hierarchical_clustering(dist_mat, "complete")  

h_complete <- hclust(stats::dist(t(df_normalized), "euclidean"), "complete")

all.equal(h_complete$height, cluster_complete$matched_at)
# TRUE

all.equal(h_complete$merge, cluster_complete$merge)
# [1] "Mean relative difference: 0.7648641"

all.equal(normalize(h_complete$merge), normalize(cluster_complete$merge))
# TRUE

# Custom Linkage ---------------------------------------------------------------
cluster_custom <- hierarchical_clustering(dist_mat, "custom", create_param_list(0.5, 0.5, 0, -0.5))
all.equal(h$height, cluster_custom$matched_at)
all.equal(normalize(h$merge), normalize(cluster_custom$merge))
