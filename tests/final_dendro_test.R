#####===========================================================================
# Test file for the dendrogram visualization.
#
# - Patient dendrogram : colored by class labels, legend shown
# - Gene dendrogram    : no class labels, everything in black
#####===========================================================================

library(ggplot2)
library(distRcpp)
library(RSQLite)
library(DBI)
library(reshape2)

# ============================================================
# SOURCES
# ============================================================

source("data/database_functions_v4.r")
source("R/clustering/normalization_methods.R")
source("R/clustering/prepare_data.R")
source("R/clustering/hierarchical_clustering.R")
source("R/visualization/final_dendrogram.R") # Sourcing this file is enough => final_dendrogram.R loads all dependencies
source("R/visualization/heatmap.R")


# ============================================================
# DATABASE AND DATASET
# ============================================================

con <- dbConnect(RSQLite::SQLite(), "GeneDatabase.sqlite")

# dataset always placed under data/ so this path works for everyone
dataset_kidney_meta <- read.csv("data/TCGA_kidney_unnormalized_meta.csv", header = TRUE)


# ============================================================
# PATHWAY SELECTION AND DATA INTEGRATION
# ============================================================

# hardcoded here for testing — normally selected via the GUI
meine_pathways <- c("Biosynthesis of amino acids")
message("Using pathway for kidney test: ", meine_pathways)

preprocess        <- preprocess_general(dataset_kidney_meta)
data_preprocessed <- preprocess$dataset_preprocessed

result <- run_data_integration(
  dataset         = data_preprocessed,
  chosen_pathways = meine_pathways,
  con             = con
)

dbDisconnect(con)


# ============================================================
# PREPARE AND NORMALIZE
# ============================================================

df_prepared   <- prepare_data(result$filtered_dataset)
df_normalized <- normalization(df_prepared, 1)


# ============================================================
# NAMES AND CLASS LABELS
# ============================================================

patient_names <- colnames(result$meta_data)
gene_names    <- result$gene_names
class_labels  <- as.character(result$meta_data["Meta_labels", ])


# ============================================================
# DISTANCE MATRICES AND CLUSTERING
# ============================================================

# patients — transposed so columns (patients) are clustered
dist_mat_pat <- dist_cpp(t(df_normalized), "euclidean")
cluster_pat  <- hierarchical_clustering(dist_mat_pat, "complete")

# genes
dist_mat_genes <- dist_cpp(df_normalized, "euclidean")
cluster_genes  <- hierarchical_clustering(dist_mat_genes, "complete")


# ============================================================
# BUILD TREES
# cluster result contains the merge matrix and heights — both needed for build_tree
# ============================================================

# patients
baum_patienten  <- build_tree(cluster_pat, cluster_pat$matched_at)
order_patienten <- get_order_vector(baum_patienten)

# genes
baum_gene  <- build_tree(cluster_genes, cluster_genes$matched_at)
order_gene <- get_order_vector(baum_gene)


# ============================================================
# DENDROGRAMS
# ============================================================

# patients — colored by class labels, legend shown
generate_dendro(
  cluster_result = cluster_pat,
  tree_result    = baum_patienten,
  order_vector   = order_patienten,
  title          = "TCGA Kidney Cancer: Patient Clustering",
  names_vector   = patient_names,
  class_labels   = class_labels
)

# genes — no class labels, everything black, no legend
generate_dendro(
  cluster_result = cluster_genes,
  tree_result    = baum_gene,
  order_vector   = order_gene,
  title          = "TCGA Kidney Cancer: Gene Clustering",
  names_vector   = gene_names,
  class_labels   = NULL
)