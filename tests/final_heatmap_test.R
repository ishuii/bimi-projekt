# ============================================================
# ZIEL:
# Testdatei für die Heatmap-Funktionalität
# ============================================================

library(ggplot2)
library(distRcpp)
library(Rcpp)
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

# ============================================================
# DATABASE AND DATASET
# ============================================================

con <- dbConnect(RSQLite::SQLite(), "GeneDatabase.sqlite")

# dataset always placed under data/ so this path works for everyone
dataset_kidney_meta <- read.csv("data/GOLUB_microarray.csv", header = TRUE)


# ============================================================
# PATHWAY SELECTION AND DATA INTEGRATION
# ============================================================

# hardcoded here for testing — normally selected via the GUI
meine_pathways <- c("Metabolic pathways")
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
#df_normalized <- normalization(df_prepared, 1)


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
dist_mat_pat <- dist_cpp(t(df_prepared), "euclidean")
cluster_pat  <- hierarchical_clustering(dist_mat_pat, "complete")

# genes
dist_mat_genes <- dist_cpp(df_prepared, "euclidean")
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
# HEATMAP FELDER
# ============================================================
metaDaten_gefiltert <- result$meta_data

heatmap_fields <- create_heatmap_field_data(
  data_matrix = df_prepared,
  metaDaten_gefiltert = metaDaten_gefiltert
)

meta_names <- extract_metadata_names(
  metaDaten_gefiltert
)

heatmap_fields <- heatmap_fields[, c(
  "Expression",
  "Gene",
  "Patient",
  meta_names
)]

print(
  head(heatmap_fields)
)

# ============================================================
# HEATMAP
# ============================================================

generate_heatmap(df_prepared, order_gene,
                 order_patienten
)
print(order_patienten)

# ============================================================
# PDF EXPORT
# ============================================================

heatmap_pdf(
  df_normalized = df_prepared,
  patient_order = order_pat,
  gene_order = order_genes,
  file = "heatmap_all.pdf",
  show_x_axis = TRUE
)

