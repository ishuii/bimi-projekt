
library(ggplot2)
library(patchwork)
library(distRcpp)
library(RSQLite)
library(DBI)
library(reshape2)

# ============================================================
# SOURCE
# ============================================================

source("R/visualization/final_dendrogram.R")
source("R/visualization/heatmap_final.R")
source("R/visualization/Grafikpanel.R")
source("R/clustering/hierarchical_clustering.R")
source("R/clustering/normalization_methods.R")
source("R/clustering/prepare_data.R")

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
# DENDROGRAMS
# ============================================================

# patients — colored by class labels, legend shown
patient_dendro <- generate_dendro(
  cluster_result = cluster_pat,
  tree_result    = baum_patienten,
  order_vector   = order_patienten,
  title          = "Patienten",
  names_vector   = patient_names,
  class_labels   = class_labels
)

# genes — no class labels, everything black, no legend
gene_dendro <- generate_dendro(
  cluster_result = cluster_genes,
  tree_result    = baum_gene,
  order_vector   = order_gene,
  title          = "Gene",
  names_vector   = gene_names,
  class_labels   = NULL
)


heatmap_plot <- generate_heatmap(
  df_prepared, order_gene, order_patienten
)

# ============================================================
# GRAFIKPANEL
# ============================================================

final_plot <- grafikpanel(
  heatmap_plot   = heatmap_plot,
  patient_dendro = patient_dendro,
  gene_dendro    = gene_dendro
)

print(final_plot)

