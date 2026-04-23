# data preparation
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
# (1) standard: log + z-score
# -> the first one we used, and a pretty good start
normalize_log_zscore <- function(df) {
  df_log <- log2(df + 1)
  df_norm <- t(scale(t(df_log)))
  return(df_norm)
}

# what it does:
# Log reduces outliers
# Z-Score → same scaling per gene

# best choice for:
# Clustering
# Heatmaps

################################################################################
# (2) CPM+log+z-score 
# doesn't work for gene/row only normalization
#normalize_cpm_log_zscore <- function(df) {
#  col_sums <- colSums(df)
#  
  # stop to prevent division by 0
#  if (any(col_sums == 0)) {
#    stop("Some columns have sum = 0 (cannot compute CPM)")
#  }
  
#  df_cpm <- t(t(df) / col_sums) * 1e6
#  df_log <- log2(df_cpm + 1)
#  df_norm <- t(scale(t(df_log)))
#  return(df_norm)
#}

# what it does:
# corrects sequencing depth (CPM = counts per million)
# afterwards like the standard

# best for:
# true RNA-Seq Counts (like TCGA)

################################################################################

# (3) just Log (if absolut differences are important)
# works on each element of the dataset
normalize_log_only <- function(df) {
  return(log2(df + 1))
}

# only does transformation
# disadvantage: genes with high variation dominate

################################################################################

# (4) correlation based method (very good for clustering)
get_correlation_distance <- function(df) {
  df_log <- log2(df + 1)
  cor_mat <- cor(t(df_log))     #transpose the dataset so it works for rows instead of cols
  dist_mat <- as.dist(1 - cor_mat)
  return(dist_mat)
}

# what it does:  measures similarities of patterns
# very good for: Gene-expression-clustering, often better than euclidian
