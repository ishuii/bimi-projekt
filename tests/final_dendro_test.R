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
source("R/visualization/graphics_panel.R")
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
df_normalized <- normalization(df_prepared, 3)


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
cluster_pat  <- hierarchical_clustering(dist_mat_pat, "average")

# genes
dist_mat_genes <- dist_cpp(df_normalized, "euclidean")
cluster_genes  <- hierarchical_clustering(dist_mat_genes, "average")


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
# DENDROGRAMS (DATEN-GENERIERUNG: Rein mathematisch strukturell)
# ============================================================

# patients — strukturelle Klassenzuweisung wird mitgegeben, aber KEINE Palette
dendro_data_pat <- generate_dendro_data(
  cluster_result = cluster_pat,
  tree_result    = baum_patienten,
  order_vector   = order_patienten,
  class_labels   = class_labels
)

# genes — keine Klassenlabels
dendro_data_genes <- generate_dendro_data(
  cluster_result = cluster_genes,
  tree_result    = baum_gene,
  order_vector   = order_gene,
  class_labels   = NULL
)


# ============================================================
# PLOTTING GGPLOT (PLOT-SACHE: Palette wird erst hier übergeben)
# ============================================================

final_plot_pat <- plot_dendro_ggplot(
  dendro_data  = dendro_data_pat,
  title        = "TCGA Kidney Cancer: Patient Clustering",
  names_vector = patient_names,
  palette_name = "PRGn",         # <-- Hier übergeben
  show_legend  = TRUE,
  show_x_axis  = TRUE,
  show_y_axis  = TRUE
)

final_plot_den <- plot_dendro_ggplot(
  dendro_data  = dendro_data_genes,
  title        = "TCGA Kidney Cancer: Gene Clustering",
  names_vector = gene_names,
  palette_name = NULL,           # <-- Keine Palette (wird standardmäßig schwarz)
  show_legend  = FALSE,
  show_x_axis  = TRUE,
  show_y_axis  = TRUE
)

print(final_plot_pat)
print(final_plot_den)


# ============================================================
# PLOTTING PLOTLY STANDALONE (PLOT-SACHE)
# ============================================================

# 1) Patienten-Dendrogramm (Flach oben drüber)
final_plotly_pat <- plot_dendro_plotly(
  dendro_data  = dendro_data_pat,
  side         = "top",
  names_vector = patient_names,
  palette_name = "PRGn",         # <-- Hier übergeben
  show_legend  = TRUE,
  show_x_axis  = TRUE,   # Zeigt Patienten-Namen
  show_y_axis  = TRUE    # Zeigt Distanz-Skala links
)

# 2) Gen-Dendrogramm (Soll als Standalone ebenfalls flach bleiben!)
final_plotly_den <- plot_dendro_plotly(
  dendro_data  = dendro_data_genes,
  side         = "top",  # Hier wieder "top", damit es flach bleibt
  names_vector = gene_names,
  palette_name = NULL,           # <-- Keine Palette
  show_legend  = FALSE,
  show_x_axis  = TRUE,   # Zeigt Gen-Namen
  show_y_axis  = TRUE    # Zeigt Distanz-Skala links
)

print(final_plotly_pat)
print(final_plotly_den)


# ============================================================
# GRAFIKPANEL — Zusammenführung im koordinierten Grid
# ============================================================

# Globale Wunschpalette für das gesamte Panel definieren
wunsch_palette <- "viridis" 

#"viridis" = viridis::viridis(100),
#"RdYlBu"  = brewer.pal(11,"RdYlBu"),
#"RdBu"    = brewer.pal(11,"RdBu"),
#"PRGn"    = brewer.pal(11,"PRGn"),

mein_panel <- grafikpanel(
  gene_dendro_data     = dendro_data_genes, 
  patient_dendro_data  = dendro_data_pat,   
  gene_order           = order_gene,
  patient_order        = order_patienten,   
  data_matrix          = df_normalized,
  metaDaten_gefiltert  = result$meta_data,  
  gene_names           = gene_names,        # Für die Achsenbeschriftung rechts
  patient_names        = patient_names,     # Für die Achsenbeschriftung unten
  palette_name         = "PRGn"    # Steuert Heatmap & Patientenzweige synchron
)

# Plot im Viewer anzeigen
print(mein_panel)
