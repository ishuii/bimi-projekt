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
library(plotly)
library(RColorBrewer)
library(colorspace)
# ============================================================
# SOURCES
# ============================================================

source("data/database_functions_v4.r")
source("R/clustering/normalization_methods.R")
source("R/clustering/prepare_data.R")
source("R/clustering/hierarchical_clustering.R")
source("R/visualization/final_dendrogram.R") # Sourcing this file is enough => final_dendrogram.R loads all dependencies
source("R/visualization/saving_functions.R")
source("R/visualization/wrapper_functions.R")


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
message("Using pathway for kidney test: ", NULL)

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

# Matcht: "labels", "meta_labels", "my_lab", "CLASS_LABEL", etc.
label_row    <- grep("lab", rownames(result$meta_data), ignore.case = TRUE, value = TRUE)[1]
class_labels <- if (!is.na(label_row)) as.character(result$meta_data[label_row, ]) else NULL


# ============================================================
# DISTANCE MATRICES AND CLUSTERING
# ============================================================

# patients transposed so columns (patients) are clustered
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
baum_patienten  <- build_tree(cluster_pat)
order_patienten <- get_order_vector(baum_patienten)

# genes
baum_gene  <- build_tree(cluster_genes)
order_gene <- get_order_vector(baum_gene)

# ============================================================
# DENDROGRAMS
# ============================================================

# patients — colored by class labels, legend shown
final_plot_pat <- generate_dendro(
  cluster_result = cluster_pat,
  tree_result    = baum_patienten,
  order_vector   = order_patienten,
  title          = "TCGA Kidney Cancer: Patient Clustering",
  names_vector   = patient_names,
  class_labels   = class_labels,
  palette        = "viridis",
  show_x_axis    = TRUE,
  show_y_axis    = TRUE
)

# genes — no class labels, everything black, no legend
final_plot_den <- generate_dendro(
  cluster_result = cluster_genes,
  tree_result    = baum_gene,
  order_vector   = order_gene,
  title          = "TCGA Kidney Cancer: Gene Clustering",
  names_vector   = gene_names,
  class_labels   = NULL,
  show_x_axis    = TRUE,
  show_y_axis    = TRUE
)

print(final_plot_pat)
print(final_plot_den)


export_dendro_pdf(
  plot     = final_plot_pat, 
  filename = "dendro_patients", 
  filepath = "C:/Users/domif/Desktop"                     
)

export_dendro_pdf(
  plot     = final_plot_den, 
  filename = "dendro_genes",    
  filepath = "C:/Users/domif/Desktop"                       
)

# ============================================================
# PLOTLY WRAPPER TEST
# ============================================================

# patients — plotly version
plotly_pat <- generate_dendro_plotly(
  cluster_result = cluster_pat,
  tree_result    = baum_patienten,
  order_vector   = order_patienten,
  title          = "TCGA Kidney Cancer: Patient Clustering",
  names_vector   = patient_names,
  class_labels   = class_labels,
  palette        = "RdYlBu",
  show_x_axis    = TRUE,
  show_y_axis    = TRUE
)

# genes — plotly version
plotly_gen <- generate_dendro_plotly(
  cluster_result = cluster_genes,
  tree_result    = baum_gene,
  order_vector   = order_gene,
  title          = "TCGA Kidney Cancer: Gene Clustering",
  names_vector   = gene_names,
  class_labels   = NULL,
  show_x_axis    = TRUE,
  show_y_axis    = TRUE
)

plotly_pat
plotly_gen



