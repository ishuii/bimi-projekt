#####===========================================================================
# This script drives the complete dendrogram pipeline for testing
#####===========================================================================

library(ggplot2)
library(distRcpp)
library(RSQLite)
library(DBI)
library(reshape2)
library(plotly)
library(RColorBrewer)
library(colorspace)
library(viridis)

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
source("R/visualization/graphics_panel.R")
source("R/visualization/saving_functions.R")


# ============================================================
# DATABASE AND DATASET
# ============================================================

con <- dbConnect(RSQLite::SQLite(), "GeneDatabase.sqlite")

dataset_kidney_meta <- read.csv("data/SHIPP_microarray.csv", header = TRUE)

# ============================================================
# PATHWAY SELECTION AND DATA INTEGRATION
# ============================================================

meine_pathways <- c("Biosynthesis of amino acids")
message("Using pathway for kidney test: ", meine_pathways)

preprocess        <- preprocess_general(dataset_kidney_meta)
data_preprocessed <- preprocess$dataset_preprocessed

result <- run_data_integration(
  dataset         = data_preprocessed,
  chosen_pathways = meine_pathways,
  con             = con
)


# ============================================================
# PREPARE AND NORMALIZE
# ============================================================

df_prepared   <- prepare_data(result$filtered_dataset)
df_normalized <- normalization(df_prepared, 4)


# ============================================================
# NAMES AND CLASS LABELS
# ============================================================

patient_names <- colnames(result$meta_data)
gene_names    <- result$gene_names

label_row    <- grep("lab", rownames(result$meta_data), ignore.case = TRUE, value = TRUE)[1]
class_labels <- if (!is.na(label_row)) as.character(result$meta_data[label_row, ]) else NULL


# ============================================================
# DISTANCE MATRICES AND CLUSTERING
# ============================================================

dist_mat_pat <- dist_cpp(t(df_normalized), "euclidean")
cluster_pat  <- hierarchical_clustering(dist_mat_pat, "single")

dist_mat_genes <- dist_cpp(df_normalized, "euclidean")
cluster_genes  <- hierarchical_clustering(dist_mat_genes, "single")


# ============================================================
# BUILD TREES
# ============================================================

baum_patienten  <- build_tree(cluster_pat)
order_patienten <- get_order_vector(baum_patienten)

baum_gene  <- build_tree(cluster_genes)
order_gene <- get_order_vector(baum_gene)


dbDisconnect(con)

# ============================================================
# DENDROGRAM DATA
# ============================================================

dendro_data_pat <- generate_dendro_data(
  cluster_result = cluster_pat,
  tree_result    = baum_patienten,
  order_vector   = order_patienten,
  class_labels   = class_labels
)

dendro_data_genes <- generate_dendro_data(
  cluster_result = cluster_genes,
  tree_result    = baum_gene,
  order_vector   = order_gene,
  class_labels   = NULL
)


# ============================================================
# PLOTTING GGPLOT
# ============================================================

final_plot_pat <- plot_dendro_ggplot(
  dendro_data  = dendro_data_pat,
  title        = "TCGA Kidney Cancer: Patient Clustering",
  names_vector = patient_names,
  palette_name = "PRGn",
  show_legend  = TRUE,
  show_x_axis  = TRUE,
  show_y_axis  = TRUE
)

final_plot_den <- plot_dendro_ggplot(
  dendro_data  = dendro_data_genes,
  title        = "TCGA Kidney Cancer: Gene Clustering",
  names_vector = gene_names,
  palette_name = NULL,
  show_legend  = FALSE,
  show_x_axis  = TRUE,
  show_y_axis  = TRUE
)

print(final_plot_pat)
print(final_plot_den)

heatmap_plot   <- generate_heatmap(data_matrix = df_normalized, gene_order = order_gene,
                                   patient_order = order_patienten, gene_names = gene_names,
                                   palette = "PRGn", show_x_axis = TRUE)
print(heatmap_plot)


# ============================================================
# PLOTTING PLOTLY STANDALONE
# ============================================================

final_plotly_pat <- plot_dendro_plotly(
  dendro_data  = dendro_data_pat,
  side         = "top",
  names_vector = patient_names,
  palette_name = "PRGn",
  show_legend  = TRUE,
  show_x_axis  = TRUE,
  show_y_axis  = TRUE
)

final_plotly_den <- plot_dendro_plotly(
  dendro_data  = dendro_data_genes,
  side         = "top",
  names_vector = gene_names,
  palette_name = NULL,
  show_legend  = FALSE,
  show_x_axis  = TRUE,
  show_y_axis  = TRUE
)

print(final_plotly_pat)
print(final_plotly_den)


# ============================================================
# GRAPHICS PANEL
# ============================================================

mein_panel <- grafikpanel(
  gene_dendro_data     = dendro_data_genes, 
  patient_dendro_data  = dendro_data_pat,   
  gene_order           = order_gene,
  patient_order        = order_patienten,   
  data_matrix          = df_normalized,
  metaDaten_gefiltert  = result$meta_data,  
  gene_names           = gene_names,
  patient_names        = patient_names,
  palette_name         = "viridis"
)

print(mein_panel)


# ============================================================
# PDF EXPORT
# ============================================================

mein_pfad <- "C:/Users/domif/OneDrive/Dokumente" 

save_dendro_pdf(
  plot      = final_plot_pat, 
  dateiname = "patient_clustering_large", 
  pfad      = mein_pfad
)

save_dendro_pdf(
  plot      = final_plot_den, 
  dateiname = "gene_clustering_large", 
  pfad      = mein_pfad
)