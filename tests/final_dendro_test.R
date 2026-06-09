# ============================================================
# ZIEL:
# Testdatei für das Patienten- und Gen-Dendrogram
# Es reicht diese eine Datei zu sourcen, da final_dendrogram.R
# alle weiteren Abhängigkeiten automatisch lädt.
# ============================================================

library(ggplot2)
library(distRcpp)
library(RSQLite)
library(DBI)
library(reshape2)

# ============================================================
# SOURCE
# ============================================================

source("R/visualization/final_dendrogram.R")

# ============================================================
# DATENBANK UND DATENSATZ
# ============================================================

con <- dbConnect(RSQLite::SQLite(), "GeneDatabase.sqlite")

# Anmerkung: Datensatz immer unter data/... ablegen
dataset_kidney_meta <- read.csv("data/TCGA_kidney_unnormalized_meta.csv", header = TRUE)

# ============================================================
# PATHWAYS UND INTEGRATION
# ============================================================

# diese Auswahl ist hier hart codiert, wird normalerweise in der GUI ausgewählt
meine_pathways <- c("Biosynthesis of amino acids")
message("Nutze Pathway für den Nieren-Test: ", meine_pathways)

preprocess        <- preprocess_general(dataset_kidney_meta)
data_preprocessed <- preprocess$dataset_preprocessed

result <- run_data_integration(
  dataset         = data_preprocessed,
  chosen_pathways = meine_pathways,
  con             = con
)

dbDisconnect(con)

# ============================================================
# PREPARE DATA UND NORMALISIERUNG
# ============================================================

df_prepared   <- prepare_data(result$filtered_dataset)
df_normalized <- normalization(df_prepared, 1)

# ============================================================
# NAMEN UND KLASSENLABELS
# ============================================================

patient_names <- colnames(result$meta_data)
gene_names    <- result$gene_names
class_labels  <- as.character(result$meta_data["Meta_labels", ])

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
# BAUMSTRUKTUREN
# ============================================================

baum_patienten  <- build_tree(cluster_pat$merge, cluster_pat$matched_at)
order_patienten <- get_order_vector(baum_patienten)

baum_gene  <- build_tree(cluster_genes$merge, cluster_genes$matched_at)
order_gene <- get_order_vector(baum_gene)

# ============================================================
# DENDROGRAMME
# ============================================================

# PATIENTEN — mit Klassenfärbung
generate_dendro(
  cluster_result = cluster_pat,
  tree_result    = baum_patienten,
  order_vector   = order_patienten,
  title          = "TCGA Nierenkrebs: Patienten-Clustering",
  names_vector   = patient_names,
  class_labels   = class_labels
)

# GENE — ohne Klassenfärbung
generate_dendro(
  cluster_result = cluster_genes,
  tree_result    = baum_gene,
  order_vector   = order_gene,
  title          = "TCGA Nierenkrebs: Gen-Clustering",
  names_vector   = gene_names,
  class_labels   = NULL
)