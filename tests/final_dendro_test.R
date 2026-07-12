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
source("R/visualization/dendrogram_data_functions.R") 
source("R/visualization/final_tree_functions.R") 
source("R/visualization/dendrogram_plotter.R") 
source("R/visualization/heatmap_final.R")
source("R/visualization/grafik2.R")
source("R/visualization/saving_functions.R")



# ============================================================
# DATABASE AND DATASET
# ============================================================

con <- dbConnect(RSQLite::SQLite(), "GeneDatabase.sqlite")

# dataset always placed under data/ so this path works for everyone
dataset_kidney_meta <- read.csv("data/SHIPP_microarray.csv", header = TRUE)

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
dendro_data_pat <- generate_dendro_data(
  cluster_result = cluster_pat,
  tree_result    = baum_patienten,
  order_vector   = order_patienten,
  class_labels   = class_labels,
  palette        = "viridis"
)

final_plot_pat <- plot_dendro_ggplot(
  dendro_data  = dendro_data_pat,
  title        = "TCGA Kidney Cancer: Patient Clustering",
  names_vector = patient_names,
  show_legend  = TRUE,
  show_x_axis  = TRUE,
  show_y_axis  = TRUE
)

# genes — no class labels, everything black, no legend
dendro_data_genes <- generate_dendro_data(
  cluster_result = cluster_genes,
  tree_result    = baum_gene,
  order_vector   = order_gene,
  class_labels   = NULL,
  palette        = NULL
)

final_plot_den <- plot_dendro_ggplot(
  dendro_data  = dendro_data_genes,
  title        = "TCGA Kidney Cancer: Gene Clustering",
  names_vector = gene_names,
  show_legend  = FALSE,
  show_x_axis  = TRUE,
  show_y_axis  = TRUE
)

print(final_plot_pat)
print(final_plot_den)

final_plotly_pat <- plot_dendro_plotly(
  dendro_data  = dendro_data_pat,
  title        = "TCGA Kidney Cancer: Patient Clustering",
  names_vector = patient_names,
  show_legend  = TRUE,
  show_x_axis  = TRUE,
  show_y_axis  = TRUE
)

final_plotly_den <- plot_dendro_plotly(
  dendro_data  = dendro_data_genes,
  title        = "TCGA Kidney Cancer: Gene Clustering",
  names_vector = gene_names,
  show_legend  = FALSE,
  show_x_axis  = TRUE,
  show_y_axis  = TRUE
)

print(final_plotly_den)
print(final_plotly_pat)


# ============================================================
# 1. ERSTELLUNG DER EINZEL-OBJEKTE (Vorbereitung)
# ============================================================

# Das fertige ggplotly-Objekt deiner Kollegin generieren
heatmap_objekt <- generate_heatmap_plotly(
  data_matrix   = df_normalized,
  gene_order    = order_gene,
  patient_order = order_patienten,
  gene_names    = gene_names,
  palette       = "viridis",
  show_x_axis   = TRUE
)

# Hinweis: dendro_data_genes und dendro_data_pat wurden bereits
# mit deiner generate_dendro_data() Funktion erstellt.

#============================================================
  # 2. GRAFIKPANEL — Zusammenführung im koordinierten Grid
  # ============================================================

final_panel <- grafikpanel(
  heatmap_plot        = heatmap_objekt,
  gene_dendro         = dendro_data_genes,
  patient_dendro      = dendro_data_pat,
  gene_order          = order_gene,
  patient_order       = order_patienten,
  data_matrix         = df_prepared,         # Die Rohmatrix aus prepare_data()
  metaDaten_gefiltert = result$meta_data,
  gene_names = genes# Die Metadaten aus dem Result-Objekt
)

# Plot im Viewer anzeigen
print(final_panel)