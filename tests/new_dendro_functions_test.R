

library(ggplot2)
library(distRcpp)

df <- read.csv("data/TCGA_kidney_unnormalized_TOP10.csv", row.names = 1)
source("R/clustering/normalization_methods.R")
df           <- prepare_data(df)[[1]]
df_normalized <- normalize_log_zscore(df)
df_small     <- df_normalized[, 1:250]
df_small <- df_small[-nrow(df_small), ]

source("R/clustering/hierarchical_clustering.R")
source("R/visualization/tree_functions.R")
source("R/visualization/dendro_functions_V2.R")
source("R/visualization/dendro_functions.R")




# ============================================================
# TEST — GENE
# ============================================================
dist_mat_genes <- dist_cpp(df_small, "euclidean")
cluster_genes  <- hierarchical_clustering(dist_mat_genes, "single")
tree_genes     <- build_tree(cluster_genes$merge, cluster_genes$matched_at)
order_genes    <- get_order_vector(tree_genes)
coords_genes   <- calculate_coords(order_genes, cluster_genes$matched_at, tree_genes)


# ============================================================
# TEST — PATIENTEN
# ============================================================
dist_mat_pat <- dist_cpp(t(df_small), "euclidean")
cluster_pat  <- hierarchical_clustering(dist_mat_pat, "complete")
tree_pat     <- build_tree(cluster_pat$merge, cluster_pat$matched_at)
order_pat    <- get_order_vector(tree_pat)
coords_pat   <- calculate_coords(order_pat, cluster_pat$matched_at, tree_pat)

# Gene
plot_dendro_V2(coords_genes, tree_genes, order_genes,
               cluster_genes$matched_at, NULL, rownames(df_small), "Gene")

# Patienten
plot_dendro_V2(coords_pat, tree_pat, order_pat,
               cluster_pat$matched_at, NULL, colnames(df_small), "Patienten")