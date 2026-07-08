# ============================================================
# ZIEL:
# Diese Datei ist eine Vorlage für Tests, die alle Funktionsaufrufe in der korrekten Reihenfolge enthält
# ============================================================

library(ggplot2)
library(distRcpp)
library(RSQLite)
library(DBI)
library(reshape2) 
library(viridis) 
library(RColorBrewer) 

# ============================================================
# SOURCE FILES
# ============================================================

source("data/database_functions_v4.r")
source("R/clustering/normalization_methods.R")
source("R/clustering/prepare_data.R")
source("R/clustering/hierarchical_clustering.R")
source("R/visualization/heatmap_final.R")
source("R/visualization/final_dendrogram.R")
source("R/visualization/saving_functions.R")
source("R/visualization/wrapper_functions.R")
source("R/visualization/Grafikpanel.R")

# ============================================================
# DATENBANK und DATENSATZ
# ============================================================

con <- dbConnect(RSQLite::SQLite(), "GeneDatabase.sqlite")

# Anmerkung: Datensatz immer unter data/... ablegen, damit dieser Aufruf für alle reproduzierbar ist!
dataset_golub <- read.csv("data/GOLUB_microarray.csv", header = TRUE)
dataset_kidney <- read.csv("data/TCGA_kidney_unnormalized_meta.csv")
dataset_ship <- read.csv("data/SHIPP_microarray.csv")

# ============================================================
# PATHWAYS und INTEGRATION
# ============================================================

# diese Auswahl ist hier hart codiert, wird normalerweise in der GUI ausgewählt
meine_pathways <- c("Pathways in cancer")
message("Nutze Pathway: ", meine_pathways)

preprocess        <- preprocess_general(dataset_ship)
data_preprocessed <- preprocess$dataset_preprocessed

result <- run_data_integration(
  dataset         = data_preprocessed,
  chosen_pathways = meine_pathways,
  con             = con
)

gefilteterDatensatz <- result$filtered_dataset
metaDaten_gefiltert <- result$meta_data

# dbDisconnect(con)

# ============================================================
# PREPARE DATA UND NORMALISIERUNG
# ============================================================

df_prepared   <- prepare_data(gefilteterDatensatz)
df_normalized <- normalization(df_prepared, 3)

patient_names <- colnames(metaDaten_gefiltert)
gene_names <- result$gene_names

label_row <- grep("lab", rownames(metaDaten_gefiltert), ignore.case = TRUE, value = TRUE)[1]
class_labels  <- if (!is.na(label_row)) as.character(metaDaten_gefiltert[label_row, ]) else NULL

# ============================================================
# DISTANZMATRIX UND CLUSTERING
# ============================================================

# PATIENTEN
dist_mat_pat <- dist_cpp(t(df_normalized), "euclidean")
cluster_pat  <- hierarchical_clustering(dist_mat_pat, "complete")

# GENE
dist_mat_genes <- dist_cpp(df_normalized, "euclidean")
cluster_genes  <- hierarchical_clustering(dist_mat_genes, "complete")

# ============================================================
# BUILD TREES
# ============================================================

# PATIENTEN
tree_pat   <- build_tree(cluster_pat)
order_pat  <- get_order_vector(tree_pat)

# GENE
tree_genes   <- build_tree(cluster_genes)
order_genes  <- get_order_vector(tree_genes)

# ============================================================
# DENDROGRAMS - generate_dendro
# ============================================================

final_plot_pat <- generate_dendro(
  cluster_result = cluster_pat,
  tree_result    = tree_pat,
  order_vector   = order_pat,
  title          = "TCGA Kidney Cancer: Patient Clustering",
  names_vector   = patient_names,
  class_labels   = class_labels,
  palette        = "viridis",
  show_x_axis    = TRUE,
  show_y_axis    = TRUE
)

final_plot_den <- generate_dendro(
  cluster_result = cluster_genes,
  tree_result    = tree_genes,
  order_vector   = order_genes,
  title          = "TCGA Kidney Cancer: Gene Clustering",
  names_vector   = gene_names,
  class_labels   = NULL,
  show_x_axis    = TRUE,
  show_y_axis    = TRUE
)

print(final_plot_pat)
print(final_plot_den)

# ============================================================
# DENDROGRAMS - plotly wrapper
# ============================================================

plotly_pat <- generate_dendro_plotly(
  cluster_result = cluster_pat,
  tree_result    = tree_pat,
  order_vector   = order_pat,
  title          = "TCGA Kidney Cancer: Patient Clustering",
  names_vector   = patient_names,
  class_labels   = class_labels,
  palette        = "viridis",
  show_x_axis    = TRUE,
  show_y_axis    = TRUE
)

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

# ============================================================
# HEATMAP
# ============================================================

viridis <- viridis::viridis(100) 
RdYlBu <- brewer.pal(11, "RdYlBu") 
RdBu <- brewer.pal(11, "RdBu") 
PRGn <- brewer.pal(11, "PRGn") 

heatmap1 <- generate_heatmap(df_normalized, order_genes, order_pat)
grafikpanel(heatmap1, patient_dendro, gene_dendro)

# zum Vergleichen
stats::heatmap(df_normalized)

