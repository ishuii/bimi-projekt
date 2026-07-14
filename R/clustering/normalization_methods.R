
normalization <- function(df, norm_method) {
  # 0 == no normalization
  # 1 == normalize_log_zscore
  # 2 == normalize_zscore
  # 3 == normalize_log_only
  # 4 == normalize_log_median_centering
  # 5 == normalize_median_centering
  # 6 == normalize_log_mad
  # 7 == normalize_mad
  
  #--------------
  #no normalization but will keep the name df_norm
  
  if (norm_method == 0) {
    df_norm <- df
    
    return(df_norm)
  }
  
  #--------------
  #log_zscore
  if (norm_method == 1) {
    
    if (any(df <= -1, na.rm = TRUE)) {
      stop(
        "Der Datensatz enthält Werte <= -1. Eine Log2(x + 1)-Transformation ist daher nicht möglich. Vermutlich sind die Daten bereits normalisiert oder z-standardisiert."
      )
    }
    
    df_log <- log2(df + 1)
    
    if (any(apply(df_log, 1, sd, na.rm = TRUE) == 0)) {
      stop(
        "Fehler: Zeilen mit Standardabweichung 0 gefunden. Diese Normalisierungsmethode ist nicht passend."
      )
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
  #zscore only
  if (norm_method == 2) {
    if (any(apply(df, 1, sd, na.rm = TRUE) == 0)) {
      stop(
        "Fehler: Zeilen mit Standardabweichung 0 gefunden. Diese Normalisierungsmethode ist nicht passend."
      )
    }
    df_norm <- t(scale(t(df)))
    return(df_norm)
  }
  
  #--------------
  # log only
  if (norm_method == 3) {
    if (any(df <= -1, na.rm = TRUE)) {
      stop(
        "Der Datensatz enthält Werte <= -1. Eine Log2(x + 1)-Transformation ist daher nicht möglich. Vermutlich sind die Daten bereits normalisiert oder z-standardisiert."
      )
    }
    return(log2(df + 1))
    
  }
  # (2) just Log (if absolut differences are important)
  # works on each element of the dataset
  # only does transformation
  # disadvantage: genes with high variation dominate
  
  #--------------
  
  # log median-centering
  if (norm_method == 4) {
    
    if (any(df <= -1, na.rm = TRUE)) {
      stop(
        "Der Datensatz enthält Werte <= -1. Eine Log2(x + 1)-Transformation ist daher nicht möglich. Vermutlich sind die Daten bereits normalisiert oder z-standardisiert."
      )
    }
    
    df_log <- log2(df + 1)
    
    df_norm <- t(apply(df_log, 1, function(x) {
      (x - median(x)) / ((max(x) - min(x)) + 1e-8)
    }))
    
    return(df_norm)
  }
  # (3) Log + Median-Centering
  # Centers each gene (row) around its median.
  # Preserves differences in variability between genes.
  # Useful when comparing expression patterns while
  # retaining information about regulation strength.
  
  
  #--------------
  
  #median centering only
  if (norm_method == 5) {
    df_norm <- t(apply(df, 1, function(x) {
      (x - median(x)) / ((max(x) - min(x)) + 1e-8)
    }))
    
    return(df_norm)
  }
  
  #--------------
  #log mad
  if (norm_method == 6) {
    
    if (any(df <= -1, na.rm = TRUE)) {
      stop(
        "Der Datensatz enthält Werte <= -1. Eine Log2-Transformation ist daher nicht möglich. Vermutlich sind die Daten bereits normalisiert oder z-standardisiert."
      )
    }
    
    df_log <- log2(df + 1)
    df_norm <- t(apply(df_log, 1, function(x) {
      (x - median(x)) / (mad(x) + 1e-8)
    }))
    return(df_norm)
  }
  # (4) Log + mad(median absolut deviation)
  # Each gene (row) is centered on its median and normalized by a robust measure of spread (MAD)
  
  
  #--------------
  #mad only
  if (norm_method == 7) {
    df_norm <- t(apply(df, 1, function(x) {
      (x - median(x)) / (mad(x) + 1e-8)
    }))
    return(df_norm)
  }
}
