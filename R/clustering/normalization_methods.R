
normalization <- function(df, norm_method) {
  # 0 == no normalization
  # 1 == normalize_log_zscore
  # 2 == normalize_log_only
  # 3 == normalize_log_median_centering
  # 4 == normalize_log_mad
  
#--------------    
#no normalization but will keep the name df_norm
  
  if (norm_method == 0) {
    
    df_norm <- df
    
    return(df_norm)
  }
  
#--------------  
  
  if (norm_method == 1) {
    
      df_log <- log2(df + 1)
      
      if(any(apply(df_log, 1, sd, na.rm=TRUE) == 0)){
        warning("Fehler: Zeilen mit Standardabweichung 0 gefunden. Diese Normalisierungsmethode ist nicht passend.")
      }
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
  # (2) just Log (if absolut differences are important)
  # works on each element of the dataset
  # only does transformation
  # disadvantage: genes with high variation dominate
  
#--------------  
  

  if (norm_method == 3) {

    df_log <- log2(df + 1)

    df_norm <- t(apply(df_log, 1, function(x) {
      (x - median(x)) / (max(x) - min(x))
    }))

    return(df_norm)
}
# (3) Log + Median-Centering
# Centers each gene (row) around its median.
# Preserves differences in variability between genes.
# Useful when comparing expression patterns while
# retaining information about regulation strength.
  
#--------------  
  
  if (norm_method == 4) {

      df_log <- log2(df + 1)
      df_norm <- t(apply(df_log, 1, function(x) {
        (x - median(x)) / (mad(x) + 1e-8)
      }))
      return(df_norm)
  }
  # (4) Log + mad(median absolut deviation)
  # Each gene (row) is centered on its median and normalized by a robust measure of spread (MAD)
}

