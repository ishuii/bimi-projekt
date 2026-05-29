# data preparation
# goal: remove labels row, save it in list if needed later
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
  
  # process each row separately
  for (i in 1:nrow(df)) {
    
    # convert current row to numeric
    row_numeric <- suppressWarnings(as.numeric(df[i, ]))
    
    # check if row contains invalid values (NA after conversion)
    if (any(is.na(row_numeric))) {
      
      # calculate mean from valid numeric values only
      row_mean <- mean(row_numeric, na.rm = TRUE)
      
      # if no numeric value exists -> error
      if (is.nan(row_mean)) {
        stop(paste(
          "Fehler: Zeile", i,
          "enthält keine numerischen Werte."
        ))
      }
      
      # replace all invalid / NA values with row mean
      row_numeric[is.na(row_numeric)] <- row_mean
    }
    
    # replace row in matrix
    df[i, ] <- row_numeric
  }
  
  # convert whole matrix to numeric
  suppressWarnings(mode(df) <- "numeric")
  
  # check for infinite values
  if (any(is.infinite(df))) {
    stop("Fehler: Der Datensatz enthält Inf oder -Inf Werte.")
  }
  
  # return numeric matrix
  return(df)
}
