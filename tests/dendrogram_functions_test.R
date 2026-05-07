#####Test 

# Testing: Linkage functions

##### Get data and normalize it -----------------------------------------------------

# first column is treated as row names
df <- read.csv("data/TCGA_kidney_unnormalized_TOP10.csv", row.names = 1)

source("R/clustering/normalization_methods.R")
df <- prepare_data(df)[[1]]                     # get label vector with [[2]]
df_normalized <- normalize_log_zscore(df)

##### distance matrix

source("R/clustering/distance_matrix.R")

df_small <- df_normalized[,1:250]
dist_mat <- dist_cpp(t(df_small), "euclidean")

##### single linkage function

source("R/clustering/single_linkage.R")

cluster_result <- single_linkage(dist_mat)  

#print(cluster_result$merge)
#print(cluster_result$matched_at)

source("R/visualization/tree_functions.R")
source("R/visualization/dendro_functions.R")

##------ Gene --------
dist_mat_genes   <- dist_cpp(df_small, "euclidean")
cluster_genes    <- single_linkage(dist_mat_genes)
tree_genes       <- build_tree(cluster_genes$merge, cluster_genes$matched_at)
order_genes      <- get_order_vector(tree_genes)
coords_genes     <- calculate_coords(order_genes, cluster_genes$matched_at, tree_genes)

plot_dendro(coords_genes, tree_genes, order_genes, cluster_genes$matched_at, NULL, rownames(df_small), "Gene")
draw_segments(coords_genes, tree_genes, NULL, cluster_genes$matched_at, order_genes, rownames(df_small))

##------ Patienten --------
dist_mat_pat   <- dist_cpp(t(df_small), "euclidean")
cluster_pat    <- single_linkage(dist_mat_pat)
tree_pat       <- build_tree(cluster_pat$merge, cluster_pat$matched_at)
order_pat      <- get_order_vector(tree_pat)
coords_pat     <- calculate_coords(order_pat, cluster_pat$matched_at, tree_pat)

plot_dendro(coords_pat, tree_pat, order_pat, cluster_pat$matched_at, NULL, colnames(df_small),"Patienten")
draw_segments(coords_pat, tree_pat, NULL, cluster_pat$matched_at, order_pat, colnames(df_small))