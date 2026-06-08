
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

# these are test dataframes to see if error-treatment works

test_df1 <- data.frame(
  V1 = c("1", "4", "a", NA, "10"),
  V2 = c("2", NA, "b", NA, "20"),
  V3 = c("3", "6", "7", NA, "30"),
  stringsAsFactors = FALSE
)

test_df2 <- data.frame(
  V1 = c("1", "4", "a", "10", "b"),
  V2 = c("2", NA, "b", "20", "NA"),
  V3 = c("3", "6", "7", "30", "b"),
  stringsAsFactors = FALSE
)

test_df3 <- data.frame(
  V1 = c("1", "4", "a", "10", "b"),
  V2 = c("2", NA, "b", "20", "6"),
  V3 = c("3", "6", "7", "30", "3"),
  stringsAsFactors = FALSE
)



prep1 <- prepare_data(test_df1)  #both dont work as there are rows with only NAs / non-numeric
prep2 <-prepare_data(test_df2)
prep3 <- prepare_data(test_df3)


# test to look if the preparedata funktion worked correctly for main-test dataset
#"data/TCGA_kidney_unnormalized_meta.csv"
# -> seems to work correctly
# ============================================================
# DATENBANK und DATENSATZ
# ============================================================

con <- dbConnect(RSQLite::SQLite(), "GeneDatabase.sqlite")

# Anmerkung: Datensatz immer unter data/... ablegen, damit dieser Aufruf für alle reproduzierbar ist!
dataset_kidney_meta <- read.csv("data/TCGA_kidney_unnormalized_meta.csv", header = TRUE)

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
df_normalized <- normalization(df_prepared, 1)

patient_names <- colnames(df_normalized)
class_labels  <- as.character(metaDaten_gefiltert["Meta_labels", patient_names])




