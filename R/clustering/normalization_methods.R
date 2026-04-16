
# !!!These functions are not yet tested!!!

# preparation
# goal: remove labels row, save it in list if needed later
# we only normalize and cluster the rest of the data

prepare_data <- function(df) {
  # Labels = last row
  labels <- as.numeric(df[nrow(df), ])
  
  # remove labels on lowest row
  df <- df[-nrow(df), ]
  
  # check if everything is numerical
  df <- as.matrix(df)     
  mode(df) <- "numeric"
  
  # we return matrix to make sure it stays numeric (no switch back to dataframe)
  
  return(list(data = df, labels = labels))
}

################################################################################
# Log + Z-Score (STANDARD) -> the first one we used, and a pretty good start
normalize_log_zscore <- function(df) {
  df_log <- log2(df + 1)
  df_norm <- t(scale(t(df_log)))
  return(df_norm)
}

# note Wiki: should df be overriden with normalization? or keep "old" form?
# i.e. in code: (1) same variable: df <- normalize_log_zscore(df)
#          or   (2) new variable:  df_norm <- normalize_log_zscore(df)

# what it does:
# Log reduces outliers
# Z-Score → same scaling per gene

# best choice for:
# Clustering
# Heatmaps

################################################################################
# CPM+log+z-score 
normalize_cpm_log_zscore <- function(df) {
  col_sums <- colSums(df)
  
  # stop to prevent division by 0
  if (any(col_sums == 0)) {
    stop("Some columns have sum = 0 (cannot compute CPM)")
  }
  
  df_cpm <- t(t(df) / col_sums) * 1e6
  df_log <- log2(df_cpm + 1)
  df_norm <- t(scale(t(df_log)))
  return(df_norm)
}

# what it does:
# corrects sequencing depth (CPM = counts per million)
# afterwards like the standard

# best for:
# true RNA-Seq Counts (like TCGA)

################################################################################
# just Log (if absolut differences are important)
normalize_log_only <- function(df) {
  return(log2(df + 1))
}

# only does transformation
# disadvantage: genes with high variation dominate

################################################################################
# correlation based method (very good for clustering)
get_correlation_distance <- function(df) {
  df_log <- log2(df + 1)
  dist_mat <- as.dist(1 - cor(df_log))
  return(dist_mat)
}

#what it does:  measures similarities of patterns
#very good for: Gene-expression-clustering, often better than euclidian


# ------------------------------ TESTING ------------------------------------------

df <- read.csv("data/TCGA_kidney_unnormalized_TOP10.csv", row.names = 1)

##### Test 1 - prepare_data()
prepared_data_list <- prepare_data(df)

# first list entry is a matrix -
prepared_data <- prepared_data_list[[1]]

##### Test 2 - normalize_log_zscore()
log_zscore_df <- normalize_log_zscore(prepared_data)   # returns a matrix

mean(log_zscore_df)  # returns 1.407791e-17 --> very close to 0 (should be ~0)
sd(log_zscore_df)    # returns 0.9995578 --> very close to 1 (should be ~1)

# seems to work !

##### Test 3 - normalize_cpm_log_zscore
cpm_log_zscore_df <- normalize_cpm_log_zscore(prepared_data)  # returns a matrix

if (!all(abs(colSums(cpm_log_zscore_df) - 1e6) < 1e-6)) {
  cat("test failed")
} else {
  cat("passt")
}

# to be continued

