# Function testing
library(ggplot2)
library(distRcpp)
library(RSQLite)
library(DBI)
library(reshape2) 
library(viridis) 
library(RColorBrewer) 

source("data/database_functions_v4.r")
source("R/clustering/normalization_methods.R")
source("R/clustering/prepare_data.R")
source("R/clustering/hierarchical_clustering.R")
source("R/visualization/tree_functions.R") 
source("R/visualization/dendro_functions_V2.R") 
source("R/visualization/dendro_functions.R")
source("R/visualization/heatmap.R")


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
#test NaN-erzeugung

con <- dbConnect(RSQLite::SQLite(), "GeneDatabase.sqlite")

# Anmerkung: Datensatz immer unter data/... ablegen, damit dieser Aufruf für alle reproduzierbar ist!
dataset_kidney_meta <- read.csv("data/colon_vs_pancreas_meta.csv", header = TRUE)

# ============================================================
# PATHWAYS und INTEGRATION
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

gefilteterDatensatz <- result$filtered_dataset
metaDaten_gefiltert <- result$meta_data

dbDisconnect(con)

# ============================================================
# PREPARE DATA UND NORMALISIERUNG
# ============================================================

df_prepared   <- prepare_data(gefilteterDatensatz)
#summary(df_prepared)
#min(df_prepared, na.rm = TRUE)
#dim(df_prepared)
#sum(df_prepared < -1, na.rm = TRUE)
#which(df_prepared < -1, arr.ind = TRUE)

df_normalized <- normalization(df_prepared, 4)

patient_names <- colnames(df_normalized)
class_labels  <- as.character(metaDaten_gefiltert["Meta_labels", patient_names])


