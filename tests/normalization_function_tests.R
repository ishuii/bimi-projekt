
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
source("R/visualization/final_dendrogram_functions.R")
source("R/visualization/final_dendrogram.R")
source("R/visualization/final_tree_functions.R")
source("R/visualization/Grafikpanel.R")


# ------------------------------ TESTING ------------------------------------------

df <- read.csv("data/TCGA_kidney_unnormalized_TOP10.csv", row.names = 1)

##### Test 1 - prepare_data() ----------------------------------------------------
prepared_data_list <- prepare_data(df)

# first list entry is a matrix
prepared_data <- prepared_data_list[[1]]

##### Test 2 - normalize_log_zscore() ---------------------------------------------
log_zscore_df <- normalize_log_zscore(prepared_data)   # returns a matrix

# margin = 1 if looking at rows, if at cols margin = 2
test_zscore <- function(x, z_func, margin = 1, tol = 1e-6) {
  y <- z_func(x)
  
  if (margin == 1) {
    means <- rowMeans(y)    # means should be close to 0 (< 1e-6)
    sds <- apply(y, 1, sd)  # sd should be close to 1 ( (sd-1) < 1e-6 )
  } else {
    means <- colMeans(y)
    sds <- apply(y, 2, sd)
  }
  
  if (!all(abs(means) < tol)) {
    stop("Z-score test failed: means not ~0")
  }
  
  if (!all(abs(sds - 1) < tol)) {
    stop("Z-score test failed: sds not ~1")
  }
  
  message("Z-score test passed")
}

test_zscore(prepared_data, normalize_log_zscore, margin = 1)

# seems to work!

##### Test 3 - normalize_cpm_log_zscore -------------------------------------------

# log and zscore parts have already been tested
# here, we test only the cpm section: df_cpm <- t(t(df) / col_sums) * 1e6

cpm_data <- t(t(prepared_data) / colSums(prepared_data)) * 1e6

test_cpm <- function(x, tol = 1e-6) {
  col_sums <- colSums(x)
  
  if (!all(abs(col_sums - 1e6) < tol)) {
    stop("CPM test failed: column sums are not ~1e6")
  }
  
  message("CPM test passed")
}

test_cpm(cpm_data)
# CPM section works

##### Test 4 - normalize_log_only ------------------------------------------------

test_log <- function(x, log_func) {
  y <- log_func(x)
  
  # Check finite values
  if (any(!is.finite(y))) {
    stop("Log test failed: non-finite values present")
  }
  
  # Check monotonicity (flattened)
  if (!all(order(x) == order(y))) {
    stop("Log test failed: not monotonic")
  }
  
  message("Log test passed")
}

test_log(prepared_data, normalize_log_only)
# Log test passed

##### Test 5 - get_correlation_distance() ----------------------------------------
corr_df <- get_correlation_distance(prepared_data)

# returns a dist object
# should this step skip the distance function??

# Function testing

source("R/clustering/normalization_methods.R")

# ------------------------------ TESTING ------------------------------------------

df <- read.csv("data/TCGA_kidney_unnormalized_TOP10.csv", row.names = 1)

##### Test 1 - prepare_data() ----------------------------------------------------
prepared_data_list <- prepare_data(df)

# first list entry is a matrix
prepared_data <- prepared_data_list[[1]]

##### Test 2 - normalize_log_zscore() ---------------------------------------------
log_zscore_df <- normalize_log_zscore(prepared_data)   # returns a matrix

# margin = 1 if looking at rows, if at cols margin = 2
test_zscore <- function(x, z_func, margin = 1, tol = 1e-6) {
  y <- z_func(x)
  
  if (margin == 1) {
    means <- rowMeans(y)    # means should be close to 0 (< 1e-6)
    sds <- apply(y, 1, sd)  # sd should be close to 1 ( (sd-1) < 1e-6 )
  } else {
    means <- colMeans(y)
    sds <- apply(y, 2, sd)
  }
  
  if (!all(abs(means) < tol)) {
    stop("Z-score test failed: means not ~0")
  }
  
  if (!all(abs(sds - 1) < tol)) {
    stop("Z-score test failed: sds not ~1")
  }
  
  message("Z-score test passed")
}

test_zscore(prepared_data, normalize_log_zscore, margin = 1)

# seems to work!

##### Test 3 - normalize_cpm_log_zscore -------------------------------------------

# log and zscore parts have already been tested
# here, we test only the cpm section: df_cpm <- t(t(df) / col_sums) * 1e6

cpm_data <- t(t(prepared_data) / colSums(prepared_data)) * 1e6

test_cpm <- function(x, tol = 1e-6) {
  col_sums <- colSums(x)
  
  if (!all(abs(col_sums - 1e6) < tol)) {
    stop("CPM test failed: column sums are not ~1e6")
  }
  
  message("CPM test passed")
}

test_cpm(cpm_data)
# CPM section works

##### Test 4 - normalize_log_only ------------------------------------------------

test_log <- function(x, log_func) {
  y <- log_func(x)
  
  # Check finite values
  if (any(!is.finite(y))) {
    stop("Log test failed: non-finite values present")
  }
  
  # Check monotonicity (flattened)
  if (!all(order(x) == order(y))) {
    stop("Log test failed: not monotonic")
  }
  
  message("Log test passed")
}

test_log(prepared_data, normalize_log_only)
# Log test passed

##### Test 5 - get_correlation_distance() ----------------------------------------
corr_df <- get_correlation_distance(prepared_data)

# returns a dist object
# should this step skip the distance function??
####################################################################################

####################################################################################


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
message("Nutze Pathway für den Nieren-Test: ", meine_pathways)

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
df_normalized <- normalization(df_prepared, 5)

patient_names <- colnames(df_normalized)
class_labels  <- as.character(metaDaten_gefiltert["Meta_labels", patient_names])

# ============================================================
# DISTANZMATRIX UND CLUSTERING
# ============================================================

# PATIENTEN
dist_mat_pat <- dist_cpp(t(df_normalized), "euclidean")
cluster_pat  <- hierarchical_clustering(dist_mat_pat, "single")

# GENE
dist_mat_genes <- dist_cpp(df_normalized, "euclidean")
cluster_genes  <- hierarchical_clustering(dist_mat_genes, "single")

# ============================================================
# DENDROGRAMM
# ============================================================

# PATIENTEN
tree_pat   <- build_tree(cluster_pat)
order_pat  <- get_order_vector(tree_pat)
patient_dendro <- generate_dendro(cluster_pat, tree_pat, order_pat, title="Patienten", names_vector=patient_names, class_labels=class_labels)

# GENE
tree_genes   <- build_tree(cluster_genes)
order_genes  <- get_order_vector(tree_genes)
gene_dendro <- generate_dendro(cluster_genes, tree_genes, order_genes, title="Gene", names_vector=NULL, class_labels=NULL)

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

##########################################################################################

##########################################################################################

library(pheatmap)

pheatmap(
  df_prepared,
  cluster_rows = TRUE,      # Gene clustern
  cluster_cols = TRUE,      # Patienten clustern
  show_rownames = FALSE,
  show_colnames = TRUE,
  color = colorRampPalette(c("blue", "white", "red"))(100)
)
