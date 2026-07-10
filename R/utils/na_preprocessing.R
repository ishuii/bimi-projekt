analyze_na_status <- function(
    df,
    bereits_bereinigt = FALSE,
    auto_removed_rows = integer(0),
    auto_removed_cols = character(0),
    removed_50_rows = integer(0),
    removed_50_cols = character(0),
    imputed_values = 0
) {
  
  if (ncol(df) < 1) {
    stop("Datensatz enthält keine Spalten.")
  }
  
  id_col <- names(df)[1]
  data_cols <- names(df)[-1]
  
  first_col_values <- as.character(df[[id_col]])
  first_col_values[is.na(first_col_values)] <- ""
  
  meta_rows <- which(grepl("^meta_", first_col_values, ignore.case = TRUE))
  data_rows <- setdiff(seq_len(nrow(df)), meta_rows)
  
  if (length(data_cols) == 0 || length(data_rows) == 0) {
    return(list(
      na_gesamt = 0,
      zeilen_mit_na = 0,
      zeilen_gesamt = nrow(df),
      spalten_gesamt = ncol(df),
      spalten_mit_na = character(0),
      spalten_mit_na_counts = integer(0),
      rows_over_50_na = integer(0),
      rows_over_50_na_names = character(0),
      cols_over_50_na = character(0),
      meta_rows = meta_rows,
      bereits_bereinigt = bereits_bereinigt,
      auto_removed_rows = auto_removed_rows,
      auto_removed_cols = auto_removed_cols,
      removed_50_rows = removed_50_rows,
      removed_50_cols = removed_50_cols,
      imputed_values = imputed_values
    ))
  }
  
  df_data <- df[data_rows, data_cols, drop = FALSE]
  
  na_pro_spalte <- colSums(is.na(df_data))
  spalten_mit_na <- names(na_pro_spalte)[na_pro_spalte > 0]
  
  row_na_ratio <- rowMeans(is.na(df_data))
  rows_over_50_na_local <- which(row_na_ratio >= 0.5)
  rows_over_50_na <- data_rows[rows_over_50_na_local]
  
  col_na_ratio <- colMeans(is.na(df_data))
  cols_over_50_na <- names(col_na_ratio)[col_na_ratio >= 0.5]
  
  list(
    na_gesamt = sum(is.na(df_data)),
    zeilen_mit_na = sum(rowSums(is.na(df_data)) > 0),
    zeilen_gesamt = nrow(df),
    spalten_gesamt = ncol(df),
    spalten_mit_na = spalten_mit_na,
    spalten_mit_na_counts = na_pro_spalte[spalten_mit_na],
    rows_over_50_na = rows_over_50_na,
    rows_over_50_na_names = first_col_values[rows_over_50_na],
    cols_over_50_na = cols_over_50_na,
    meta_rows = meta_rows,
    bereits_bereinigt = bereits_bereinigt,
    auto_removed_rows = auto_removed_rows,
    auto_removed_cols = auto_removed_cols,
    removed_50_rows = removed_50_rows,
    removed_50_cols = removed_50_cols,
    imputed_values = imputed_values
  )
}

# returns a list as "info", which is then shown to the user 
#-----------------------------------------------------------------------------------------------------------------

read_uploaded_csv <- function(datapath) {
  
  df <- read.table(
    datapath,
    header = TRUE,
    stringsAsFactors = FALSE,
    sep = ",",
    dec = ".",
    quote = "\"",
    na.strings = c("", " ", "NA", "NaN", "NULL", "N/A", "n/a"),
    colClasses = "character",
    check.names = FALSE
  )
  
  if (ncol(df) < 1) {
    stop("Die Datei enthält keine Spalten.")
  }
  
  if (ncol(df) < 2) {
    stop("Die Datei enthält nur eine Spalte. Bitte prüfen, ob die CSV wirklich mit Komma getrennt ist.")
  }
  
  # Spaltennamen säubern
  names(df) <- trimws(names(df))
  
  # Leere oder fehlende Spaltennamen ersetzen
  bad_names <- is.na(names(df)) | names(df) == ""
  
  if (any(bad_names)) {
    names(df)[bad_names] <- paste0("Spalte_", which(bad_names))
  }
  
  # Doppelte Spaltennamen eindeutig machen
  names(df) <- make.unique(names(df), sep = "_")
  
  # Erste Spalte als ID-Spalte schützen
  df[[1]] <- as.character(df[[1]])
  
  list(df = df)
}

# Checks file, catches name errors in first row

#-----------------------------------------------------------------------------------------------------------------

auto_clean_na_upload <- function(df) {
  
  if (ncol(df) < 1) {
    stop("Datensatz enthält keine Spalten.")
  }
  
  data_cols <- names(df)[-1]
  
  auto_removed_rows <- integer(0)
  auto_removed_cols <- character(0)
  
  if (length(data_cols) == 0) {
    info <- analyze_na_status(
      df,
      bereits_bereinigt = FALSE,
      auto_removed_rows = auto_removed_rows,
      auto_removed_cols = auto_removed_cols
    )
    
    return(list(df = df, info = info))
  }
  
  first_col_values <- as.character(df[[1]])
  first_col_values[is.na(first_col_values)] <- ""
  
  meta_rows <- which(grepl("^meta_", first_col_values, ignore.case = TRUE))
  non_meta_rows <- setdiff(seq_len(nrow(df)), meta_rows)
  
  if (length(non_meta_rows) > 0) {
    
    df_data <- df[non_meta_rows, data_cols, drop = FALSE]
    
    all_na_rows_local <- which(rowSums(!is.na(df_data)) == 0)
    all_na_rows <- non_meta_rows[all_na_rows_local]
    
    if (length(all_na_rows) > 0) {
      auto_removed_rows <- all_na_rows
      df <- df[-all_na_rows, , drop = FALSE]
    }
  }
  data_cols <- names(df)[-1]
  
  if (length(data_cols) > 0) {
    
    first_col_values <- as.character(df[[1]])
    first_col_values[is.na(first_col_values)] <- ""
    
    meta_rows <- which(grepl("^meta_", first_col_values, ignore.case = TRUE))
    non_meta_rows <- setdiff(seq_len(nrow(df)), meta_rows)
    
    if (length(non_meta_rows) > 0) {
      df_data_no_meta <- df[non_meta_rows, data_cols, drop = FALSE]
      all_na_cols <- names(df_data_no_meta)[colSums(!is.na(df_data_no_meta)) == 0]
    } else {
      all_na_cols <- character(0)
    }
    
    if (length(all_na_cols) > 0) {
      auto_removed_cols <- all_na_cols
      df <- df[, !names(df) %in% all_na_cols, drop = FALSE]
    }
  }
  
  info <- analyze_na_status(
    df,
    bereits_bereinigt = FALSE,
    auto_removed_rows = auto_removed_rows,
    auto_removed_cols = auto_removed_cols
  )
  
  list(df = df, info = info)
}

#Auto Deletes Rows and Cols with 100% NA

#-------------------------------------------------------------------

User_replace_na_with_row_mean <- function(df) {
  
  if (ncol(df) < 2) {
    return(list(df = df, imputed_values = 0))
  }
  
  id_col <- names(df)[1]
  data_cols <- names(df)[-1]
  
  first_col_values <- as.character(df[[id_col]])
  first_col_values[is.na(first_col_values)] <- ""
  
  meta_rows <- which(grepl("^meta_", first_col_values, ignore.case = TRUE))
  non_meta_rows <- setdiff(seq_len(nrow(df)), meta_rows)
  
  if (length(non_meta_rows) == 0) {
    return(list(df = df, imputed_values = 0))
  }
  
  numeric_data <- as.data.frame(lapply(
    df[non_meta_rows, data_cols, drop = FALSE],
    function(col) suppressWarnings(as.numeric(as.character(col)))
  ))
  
  row_means <- rowMeans(numeric_data, na.rm = TRUE)
  row_means[is.nan(row_means)] <- NA # Cant happen cause of auto_clean
  
  imputed_values <- 0
  
  for (col in data_cols) {
    
    values <- suppressWarnings(as.numeric(as.character(df[non_meta_rows, col])))
    na_idx <- is.na(values)
    
    if (any(na_idx)) {
      values[na_idx] <- row_means[na_idx]
      df[non_meta_rows, col] <- values
      imputed_values <- imputed_values + sum(na_idx)
    }
  }
  
  list(
    df = df,
    imputed_values = imputed_values
  )
}
#----------------------------------------------------------------


User_handle_na_decision <- function(df, info, action = "mean") {
  
  removed_50_rows <- integer(0)
  removed_50_cols <- character(0)
  
  if (identical(action, "drop")) {
    
    if (!is.null(info) && length(info$rows_over_50_na) > 0) {
      
      rows_to_remove <- info$rows_over_50_na
      rows_to_remove <- rows_to_remove[rows_to_remove <= nrow(df)]
      
      if (length(rows_to_remove) > 0) {
        df <- df[-rows_to_remove, , drop = FALSE]
        removed_50_rows <- rows_to_remove
      }
    }
    
    if (!is.null(info) && length(info$cols_over_50_na) > 0) {
      
      cols_to_remove <- info$cols_over_50_na
      cols_to_remove <- cols_to_remove[cols_to_remove %in% names(df)]
      
      if (length(cols_to_remove) > 0) {
        df <- df[, !names(df) %in% cols_to_remove, drop = FALSE]
        removed_50_cols <- cols_to_remove
      }
    }
  }
  
  impute_result <- User_replace_na_with_row_mean(df)
  df <- impute_result$df
  
  new_info <- analyze_na_status(
    df,
    bereits_bereinigt = TRUE,
    auto_removed_rows = if (!is.null(info)) info$auto_removed_rows else integer(0),
    auto_removed_cols = if (!is.null(info)) info$auto_removed_cols else character(0),
    removed_50_rows = removed_50_rows,
    removed_50_cols = removed_50_cols,
    imputed_values = impute_result$imputed_values
  )
  
  list(
    df = df,
    info = new_info
  )
}
  
 #User can chose to drop or replace Cols and Rows or drop them, after that repalce all Na-Values with row mean 

#----------------------------------------------------------

check_50_na <- function(info) {
  !is.null(info) &&
    (
      length(info$rows_over_50_na) > 0 ||
        length(info$cols_over_50_na) > 0
    )
}

#Checks if there are rows or cols >=50 NA-values

#-------------------------------------------------

make_distance_cache_key <- function(
    df_normalized,
    method,
    selected_pathways,
    normalisation,
    minkowski_p = NA
) {
  
  paste(
    method,
    normalisation,
    minkowski_p,
    paste(selected_pathways, collapse = "|"),
    nrow(df_normalized),
    ncol(df_normalized),
    paste(rownames(df_normalized), collapse = "|"),
    paste(colnames(df_normalized), collapse = "|"),
    round(sum(df_normalized, na.rm = TRUE), 6),
    round(sum(df_normalized^2, na.rm = TRUE), 6),
    sep = "___"
  )
}
#------------------------------------Preset loading-------------------------------------
refresh_presets <- function(session) {
  
  if (!dir.exists(preset_dir)) {
    dir.create(preset_dir, recursive = TRUE)
  }
  
  preset_files <- list.files(
    preset_dir,
    pattern = "\\.json$",
    full.names = TRUE
  )
  
  if (length(preset_files) == 0) {
    updateSelectInput(
      session,
      "preset_datei",
      choices = character(0),
      selected = character(0)
    )
    return()
  }
  
  preset_names <- tools::file_path_sans_ext(basename(preset_files))
  
  updateSelectInput(
    session,
    "preset_datei",
    choices = setNames(preset_files, preset_names),
    selected = preset_files[1]
  )
}

