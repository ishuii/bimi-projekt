analyze_na_status <- function(df, bereits_bereinigt = FALSE, entfernte_all_na_spalten = character(0), entfernte_user_spalten = character(0),
                              imputierte_spalten = character(0), entfernte_zeilen = integer(0)) {
  
  req(ncol(df) >= 1)
  
  id_col <- names(df)[1]
  data_cols <- names(df)[-1]
  
  if (length(data_cols) == 0) {
    return(list(
      id_col = id_col,
      na_gesamt = 0,
      zeilen_mit_na = 0,
      zeilen_gesamt = nrow(df),
      spalten_gesamt = ncol(df),
      na_pro_spalte = setNames(integer(0), character(0)),
      spalten_mit_na = character(0),
      spalten_mit_na_counts = integer(0),
      rows_over_50_na = integer(0),
      rows_over_50_na_names = character(0),
      meta_rows = integer(0),
      bereits_bereinigt = bereits_bereinigt,
      entfernte_all_na_spalten = entfernte_all_na_spalten,
      entfernte_user_spalten = entfernte_user_spalten,
      imputierte_spalten = imputierte_spalten,
      entfernte_zeilen = entfernte_zeilen
    ))
  }
  
  df_data <- df[, data_cols, drop = FALSE]
  
  na_pro_spalte <- colSums(is.na(df_data))
  spalten_mit_na <- names(na_pro_spalte)[na_pro_spalte > 0]
  
  row_na_ratio <- rowMeans(is.na(df_data))
  
  first_col_values <- as.character(df[[id_col]])
  first_col_values[is.na(first_col_values)] <- ""
  
  # ignore meta rows
  meta_rows <- which(grepl("^meta_", first_col_values, ignore.case = TRUE))
  
  # Search for rows with more than 50%
  rows_over_50_na <- which(row_na_ratio > 0.5)
  
  # remove meta rows from beeing selected
  rows_over_50_na_without_meta <- setdiff(rows_over_50_na, meta_rows)
  
  list(id_col = id_col, 
       na_gesamt = sum(is.na(df_data)),
       zeilen_mit_na = sum(rowSums(is.na(df_data)) > 0), 
       zeilen_gesamt = nrow(df), 
       spalten_gesamt = ncol(df), 
       na_pro_spalte = na_pro_spalte, 
       spalten_mit_na = spalten_mit_na, 
       spalten_mit_na_counts = na_pro_spalte[spalten_mit_na], 
       rows_over_50_na = rows_over_50_na_without_meta,
       rows_over_50_na_names = first_col_values[rows_over_50_na_without_meta],
       meta_rows = meta_rows,
       bereits_bereinigt = bereits_bereinigt,
       entfernte_all_na_spalten = entfernte_all_na_spalten,
       entfernte_user_spalten = entfernte_user_spalten,
       imputierte_spalten = imputierte_spalten,
       entfernte_zeilen = entfernte_zeilen)
}

# returns a list as "info", which is then shown to the user 

# --------------------------------------------------------------------------------
# NA - replace with mean
replace_na_with_row_mean <- function(df, target_cols = NULL) {
  
  data_cols <- names(df)[-1]
  
  for (col in data_cols) {
    df[[col]] <- as.numeric(df[[col]])
  }
  
  if (is.null(target_cols)) {
    target_cols <- data_cols
  }
  
  row_means <- rowMeans(df[, data_cols, drop = FALSE], na.rm = TRUE)
  
  imputierte_spalten <- character(0)
  imputierte_werte <- 0
  
  for (col in target_cols) {
    
    na_idx <- is.na(df[[col]])
    
    df[[col]][na_idx] <- row_means[na_idx]
    
    if (any(na_idx)) {
      imputierte_spalten <- c(imputierte_spalten, col)
      imputierte_werte <- imputierte_werte + sum(na_idx)
    }
  }
  
  list(
    df = df,
    imputierte_spalten = unique(imputierte_spalten),
    imputierte_werte = imputierte_werte
  )
}

# ----------------------------------------------------------------------------------------
# Refresh NA column map ???

refresh_na_column_map <- function(info) {
  cols_with_na <- info$spalten_mit_na
  
  if (length(cols_with_na) > 0) {
    na_column_map(
      data.frame(
        input_id = paste0("na_col_action_", seq_along(cols_with_na)),
        colname = cols_with_na,
        stringsAsFactors = FALSE
      )
    )
  } else {
    na_column_map(NULL)
  }
}
