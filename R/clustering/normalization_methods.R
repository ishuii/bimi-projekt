
normalization <- function(df, norm_method) {
  # 1 == normalize_log_zscore
  # 2 == normalize_log_only
  # 3 == get_correlation_distance
  # 4 == normalize_log_mad
  
#--------------    
  
  if (norm_method == 1) {
    
      df_log <- log2(df + 1)
      df_norm <- t(scale(t(df_log)))
      return(df_norm)
  }
  # (1) standard: log + z-score
  # what it does:
  # Log reduces outliers
  # Z-Score → same scaling per gene
  
  # best choice for:
  # Clustering
  # Heatmaps
  
#--------------  
  
  if (norm_method == 2) {

      return(log2(df + 1))
    
  }
  # (3) just Log (if absolut differences are important)
  # works on each element of the dataset
  # only does transformation
  # disadvantage: genes with high variation dominate
  
#--------------  
  
  if (norm_method == 3) {

      df_log <- log2(df + 1)
      cor_mat <- cor(t(df_log))     #transpose the dataset so it works for rows instead of cols
      dist_mat <- as.dist(1 - cor_mat)
      return(dist_mat)
  }
  # (4) correlation based method (very good for clustering)
  # what it does:  measures similarities of patterns
  # very good for: Gene-expression-clustering, often better than euclidian
  
#--------------  
  
  # logarithmn with mad(median absolut deviation)
  if (norm_method == 4) {

      df_log <- log2(df + 1)
      df_norm <- t(apply(df_log, 1, function(x) {
        (x - median(x)) / (mad(x) + 1e-8)
      }))
      return(df_norm)
  }
  # logarithmn with mad(median absolut deviation)
  # Each gene (row) is centered on its median and normalized by a robust measure of spread (MAD)
  
}

