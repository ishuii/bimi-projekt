
# !!!These functions are not yet tested!!!


#preparation

prepare_data <- function(df) {
  # Labels = letzte Zeile
  labels <- as.numeric(df[nrow(df), ])
  
  # remove labels on lowest row
  df <- df[-nrow(df), ]
  
  # check if everything is numerical
  df <- as.matrix(df)
  mode(df) <- "numeric"
  
  return(list(data = df, labels = labels))
}

################################################################################
# Log + Z-Score (STANDARD) -> the first one we used, and a pretty good start
normalize_log_zscore <- function(df) {
  df_log <- log2(df + 1)
  df_norm <- t(scale(t(df_log)))
  return(df_norm)
}

#what it does:
# Log reduces outliers
# Z-Score → same scaling per gene

#best choice for:
# Clustering
# Heatmaps

################################################################################
# CPM+log+z-score 
normalize_cpm_log_zscore <- function(df) {
  df_cpm <- t(t(df) / colSums(df)) * 1e6
  df_log <- log2(df_cpm + 1)
  df_norm <- t(scale(t(df_log)))
  return(df_norm)
}

#what it does:
# corrects sequencing depth (CPM)
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
#very good for: Genexpression-clustering, often better than euclidian

