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
#source("R/visualization/tree_functions.R") 
#source("R/visualization/dendro_functions_V2.R") 
#source("R/visualization/dendro_functions.R")
source("R/visualization/heatmap.R")
source("R/visualization/final_dendrogram_functions.R")
source("R/visualization/final_dendrogram.R")
source("R/visualization/final_tree_functions.R")

# ============================================================
# DATENBANK und DATENSATZ
# ============================================================

con <- dbConnect(RSQLite::SQLite(), "GeneDatabase.sqlite")

# Anmerkung: Datensatz immer unter data/... ablegen, damit dieser Aufruf für alle reproduzierbar ist!
dataset_golub <- read.csv("data/GOLUB_microarray.csv", header = TRUE)
dataset_kidney <- read.csv("data/TCGA_kidney_unnormalized_meta.csv")

# ============================================================
# PATHWAYS und INTEGRATION
# ============================================================

# diese Auswahl ist hier hart codiert, wird normalerweise in der GUI ausgewählt
meine_pathways <- c("Pathways in cancer")
message("Nutze Pathway für den Nieren-Test: ", meine_pathways)

preprocess        <- preprocess_general(dataset_golub)
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
df_normalized <- normalization(df_prepared, 1)

patient_names <- colnames(df_normalized)
class_labels  <- as.character(metaDaten_gefiltert["Meta_labels", patient_names])

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
# DENDROGRAMM
# ============================================================

# PATIENTEN
tree_pat   <- build_tree(cluster_pat)
order_pat  <- get_order_vector(tree_pat)
generate_dendro(cluster_pat, tree_pat, order_pat, title="Patienten", names_vector=patient_names, class_labels=class_labels)

# GENE
tree_genes   <- build_tree(cluster_genes)
order_genes  <- get_order_vector(tree_genes)
generate_dendro(cluster_genes, tree_genes, order_genes, title="Gene", names_vector=NULL, class_labels=NULL)

# ============================================================
# HEATMAP
# ============================================================

viridis <- viridis::viridis(100) 
RdYlBu <- brewer.pal(11, "RdYlBu") 
RdBu <- brewer.pal(11, "RdBu") 
PRGn <- brewer.pal(11, "PRGn") 

generate_heatmap(df_normalized, order_genes, order_pat)

# funktioniert nicht wirklich:
heatmap_fields <- create_heatmap_field_data(data_matrix = df_normalized, 
                                            metadata_df = metaDaten_gefiltert)

