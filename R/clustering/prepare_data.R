# data preparation
# we only normalize and cluster the rest of the data

prepare_data <- function(df) {
  
  # check if dataset is empty
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) {
    stop("Fehler: Der Datensatz ist leer.")
  }
  
  # at least 2 rows necessary
  if (nrow(df) < 2) {
    stop("Fehler: Zu wenige Zeilen für Clustering.")
  }
  
  # convert to matrix
  df <- as.matrix(df)
  
  # convert everything to numeric
  suppressWarnings(mode(df) <- "numeric")
  
  # process rows
  df <- t(apply(df, 1, function(row_data) {
    
    if (any(is.na(row_data))) {
      
      # count non-NA values
      n_valid <- sum(!is.na(row_data))
      
      # at least one numeric value required
      if (n_valid == 0) {
        stop("Fehler: Eine Zeile enthält keine numerischen Werte.")
      }
      
      # calculate mean of available values
      row_mean <- mean(row_data, na.rm = TRUE)
      
      # replace NAs with row mean
      row_data[is.na(row_data)] <- row_mean
    }
    
    row_data
  }))
  
  # check for infinite values
  if (any(is.infinite(df))) {
    stop("Fehler: Der Datensatz enthält Inf oder -Inf Werte.")
  }
  
  return(df)
}
