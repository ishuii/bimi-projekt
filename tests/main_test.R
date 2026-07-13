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
library(stringr)
library(plotly)

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
source("R/utils/na_preprocessing.R")

# ============================================================
# DATENBANK und DATENSATZ
# ============================================================

con <- dbConnect(RSQLite::SQLite(), "GeneDatabase.sqlite")

# Anmerkung: Datensatz immer unter data/... ablegen, damit dieser Aufruf für alle reproduzierbar ist!
dataset_golub <- read.csv("data/GOLUB_microarray.csv", header = TRUE)
dataset_kidney <- read.csv("data/TCGA_kidney_unnormalized_meta.csv")
dataset_ship <- read.csv("data/SHIPP_microarray.csv")

# ============================================================
# Datensatz und NA Preprozess
# ============================================================
### In meiner Funktion ließt er den Datensatz ein deswegen kann die oberen Datasets nicht verwenden, zum 
# ! Zum Testen Datapath hier verändern !
dataset_path <- "data/SHIPP_microarray.csv"

uploaded <- read_uploaded_csv(dataset_path)
cleaned <- auto_clean_na_upload(uploaded$df)

cleaned <- User_handle_na_decision(
  df = cleaned$df,
  info = cleaned$info,
  action = "mean" # Hardcoded auf mean "drop" auch möglich mean= Mittelwert // drop = Remove
)

Na_removed_dataset <- cleaned$df
na_info <- cleaned$info


# ============================================================
# PATHWAYS und INTEGRATION
# ============================================================

# diese Auswahl ist hier hart codiert, wird normalerweise in der GUI ausgewählt
meine_pathways <- c("Pathways in cancer")
message("Nutze Pathway: ", meine_pathways)

preprocess        <- preprocess_general(Na_removed_dataset)
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
  tree_result    = tree_genes,
  order_vector   = order_genes,
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

heatmap_fields <- create_heatmap_field_data(data_matrix = df_prepared, 
                                            metaDaten_gefiltert = metaDaten_gefiltert)

meta_names <- extract_metadata_names(metaDaten_gefiltert)
heatmap_fields <- heatmap_fields[, c("Expression", "Gene", "Patient", meta_names)]
print(head(heatmap_fields))

heatmap_plotly <- generate_heatmap_plotly(
  data_matrix   = df_normalized,
  gene_order    = order_genes,
  patient_order = order_pat,
  gene_names    = gene_names,
  palette       = "PRGn",
  show_x_axis   = TRUE
)

final_plot <- grafikpanel(
  
  heatmap_plot   = heatmap_plotly,
  patient_dendro = plotly_pat,
  gene_dendro    = plotly_gen,
  
  gene_order     = order_genes,
  patient_order  = order_pat
)

final_plot


# zum Vergleichen
hc.rows = hclust(dist(df_normalized))
hc.cols = hclust(dist(t(df_normalized)))
stats::heatmap(df_normalized, Rowv = as.dendrogram(hc.rows), Colv = as.dendrogram(hc.cols))

# ===========================================================
# COMPARISONS 
# ===========================================================

# Setting: SHIPP dataset, filtered by "Pathways in Cancer"
# normalization method: 3
# distance method: euclidean
# clusteirng method: complete

# (1) DISTANCE MATRIX
d_ref  <- as.matrix(dist(df_normalized)) 
d_ours <- as.matrix(dist_cpp(df_normalized, "euclidean"))

max(abs(d_ref - d_ours))
all.equal(as.vector(d_ref), as.vector(d_ours), tolerance = 1e-10) 

# RESULT: TRUE

# -----------------------------------------------------------
# (2) MERGE PROCESS
hc_ref <- hclust(dist(df_normalized))
merge_ref <- hc_ref$merge
height_ref <- hc_ref$height
order_ref <- hc_ref$order

hc_ours <- hierarchical_clustering(d_ours, method = "complete") 
merge_ours <- hc_ours$merge
height_ours <- hc_ours$matched_at

our_tree   <- build_tree(hc_ours)
order_ours  <- get_order_vector(our_tree)

all.equal(height_ref, height_ours)
# RESULT: TRUE

hc_ours <- list(merge = merge_ours, height = height_ours, order = order_ours,
                labels = rownames(df_normalized), method = "complete")
class(hc_ours) <- "hclust"

# -----------------------------------------------------------
# (3) CLUSTER ASSIGNMENT
ref  <- cutree(hc_ref, k = 4)
ours <- cutree(hc_ours, k = 4)

same_ref <- outer(ref, ref , "==")
same_ours <- outer(ours, ours, "==")

identical(same_ref, same_ours)
# RESULT: TRUE

# ----------------------------------------------------------
# (4) COPHENETIC DISTANCE

cop_ref <- cophenetic(hc_ref)
cop_ours <- cophenetic(hc_ours)

cor(cop_ref, cop_ours)
# RESULT: 1 --> result is the same

# ----------------------------------------------------------
# (5) DENDROS

plot(as.dendrogram(hc_ref))
plot(as.dendrogram(hc_ours))

# ----------------------------------------------------------
# (5) HEATMAPS

stats::heatmap(df_normalized, col = brewer.pal(11,"PRGn"))

heatmap_plotly <- generate_heatmap_plotly(
  data_matrix   = df_normalized,
  gene_order    = order_genes,
  patient_order = order_pat,
  gene_names    = gene_names,
  palette       = "PRGn",
  show_x_axis   = TRUE
)

heatmap_plotly

final_plot <- grafikpanel(
  heatmap_plot   = heatmap_plotly,
  patient_dendro = plotly_pat,
  gene_dendro    = plotly_gen,
  gene_order     = order_genes,
  patient_order  = order_pat
)

final_plot

