# data preparation
# goal: remove labels row, save it in list if needed later
# we only normalize and cluster the rest of the data

prepare_data <- function(df) {
  # Labels = last row
  #labels <- as.numeric(df[nrow(df), ])
  
  # remove labels on lowest row -> not necessary anymore
  #df <- df[-nrow(df), ]
  
  # see if dataset is empty
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) {
    stop("Fehler: Der Datensatz ist leer.")
  }
  
  # mind 2 rows necessary
  if (nrow(df) < 2) {
    stop("Fehler: Zu wenige Zeilen für Clustering.")
  }
  
  # check for NAs
  if (any(is.na(df))) {
    stop("Fehler: Der Datensatz enthält NA-Werte.")
  }
  
  # convert to matrix
  df <- as.matrix(df)
  # check if all values are numeric, suppressWarnings -> So we get no warnings from R, only from us
  suppressWarnings(mode(df) <- "numeric")
  
  #check for new NAs again -> while converting to numeric non-numeric values are set to NA so gotta check for that again
  if (any(is.na(df))) {
    stop("Fehler: Nicht alle Werte sind numerisch.")
  }
  
  # check for infinite values
  if (any(is.infinite(df))) {
    stop("Fehler: Der Datensatz enthält Inf oder -Inf Werte.")
  }
  
  
  # we return matrix to make sure it stays numeric (no switch back to dataframe)
  
  return(list(data = df, labels = labels))
}

################################################################################

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
