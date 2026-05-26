
library(RSQLite)
library(DBI)


# ============================================================
# HILFSFUNKTIONEN
# ============================================================
#####################################################################################################
# First Step of Preprocessing the data after the User chooses the dataset which will be analyzed
#####################################################################################################
preprocess_general <- function(data){


  #######
  #rows
  ######
  # amount of rows which do not have any value ==> no ID or Gen corresponding
  amount_na_values_genes <- sum(is.na(data[ ,1]))
  amount_zero_values_genes <- sum(data[ ,1] == 0, na.rm = TRUE)
  amount_empty_values_genes <- sum(data[ ,1] == "", na.rm = TRUE)


  # indices of the rows 
  indices_na <- which(is.na(data[,1]))
  indices_zero <- which(data[ ,1] == "0")
  indices_empty <- which(data[ ,1]== "")

  # vector which stores the amount of na values per row 
  na_vec_rows <- apply(data, 1, function(x) {
    sum(is.na(x))
  })

  # indices where the whole row is just NA
  na_whole_row <- which(na_vec_rows == ncol(data))
  
  # combine all indices which should be removed
  indices_remove_row <- unique(c(indices_na, indices_zero, indices_empty, na_whole_row))

  # if there are any indices which should be removed cut them out of the dataset 
  if(length(indices_remove_row) > 0){
      data_cleaned <- data[-indices_remove_row, ]
  } else {
      data_cleaned <- data
  }

  #column => remove column if the whole column is na
  #vector which stores the amount of na values per column
  na_vec_cols <- apply(data_cleaned, 2, function(x) {
    sum(is.na(x))
  })
  # indices where the whole column is just NA
  na_whole_column <- which(na_vec_cols == nrow(data_cleaned))
  na_patients <- which(is.na(colnames(data)))

  indices_remove_column <- c(na_whole_column, na_patients)

  # if there are na values for the whole column, remove them 
  if (length(indices_remove_column) > 0) {
    data_cleaned <- data_cleaned[, -indices_remove_column]
  }


  return (list(number_na = amount_na_values_genes,
               number_zero = amount_zero_values_genes, 
               number_empty = amount_empty_values_genes, 
               rows_removed = length(indices_remove_row), 
               columns_removed = length(indices_remove_column),
               dataset_preprocessed = data_cleaned))
}



#########################################################
#Preprocess dataset with meta data and ID as first column 
#########################################################
# it searches the index of the row Meta_labels ==> it has to be the first one 
# and get all indices from that index till the last row of the dataset
# it renames the first column as Entrez_ID which equals the name in the database
# it changes the Datatype of the ID to integer

# Input Values:
# Dataset which was read by read csv and Header = True ===> important!!
# Return value:
# Named list with meta data and pure data 


preprocess_dataset_meta <- function(data) {
  
  #indices for all rows where the columns contains the value Meta
  meta_indices <- grep("^Meta", data[, 1], ignore.case = TRUE, perl = FALSE)
  # split dataset
  data_withoutmeta <- data[-meta_indices, ]
  

  #dataframe with meta information as row name 
  data_meta <- data[meta_indices, ]
  rownames(data_meta) <- data_meta[, 1]
  data_meta <- data_meta[, -1]
  
  # First column: Entrez_ID and integer
  data_withoutmeta[[1]] <- as.integer(data_withoutmeta[[1]])
  colnames(data_withoutmeta)[1] <- "Entrez_ID"
  
  # return named list 
  return(list(
    data_withoutmeta = data_withoutmeta,
    meta_data = data_meta
  ))
}

###############################################################
#Preprocess dataset with meta data and gen name as first column 
###############################################################



preprocess_dataset_meta_gennames <- function(data) {
  
  meta_indices <- grep("^Meta_", data[, 1], ignore.case = FALSE, perl = FALSE)
  # split dataset 
  data_withoutmeta <- data[-meta_indices, ]
  
  #dataframe with meta information as row name 
  data_meta <- data[meta_indices, ]
  rownames(data_meta) <- data_meta[, 1]
  data_meta <- data_meta[, -1]
  
  # return named list 
  return(list(
    data_withoutmeta = data_withoutmeta,
    meta_data = data_meta
  ))
}


#################################################################
#function which returns a list including a Matrix and a vector
#################################################################
#input value: the chosen pathway by the user, and the dataset which 
#includes the ids 
#database object con

#return:
#matrix:  where the rows stand for the chosen pathways and columns represent how
#many genes are part of a pathway and how many are found in the dataset
#columns: total, found, missing, and covering which is simply the proportion in percent  
#vector of the ids which could not be found in the dataset


analyze_pathways_coverage <- function(chosen_pathways, dataset_ids, con) {
  
  result <- matrix(NA, nrow = length(chosen_pathways), ncol = 4,dimnames = list(chosen_pathways, c("Total", "Found Genes", "Missing Genes", "Coverage")))
  missing_ids <- c()
  
  for (i in chosen_pathways) {
    pathway_genes <- unique(get_geneIDS_for_pathways(i, con))
    missing <- pathway_genes[!(pathway_genes %in% dataset_ids[, 1])]
    found <- length(pathway_genes) - length(missing)
    coverage <- round(found/ length(pathway_genes) * 100, digits = 2)
    
    result[i, ]  <- c(length(pathway_genes), found, length(missing), coverage)
    missing_ids   <- unique(c(missing_ids, missing))
  }
  
  return(list(matrix_unused = result, missing_ids = missing_ids))
}

######################################################################
# matches the extracted Gene IDs with their name in the Database
######################################################################

# Input values:
# Database connection object, vector of entrez_ids
# Return: 
# character vector of gene names

get_chosen_gennames_from_database <- function(con, entrez_ids) {
  
  platzhalter <- paste(rep("?", length(entrez_ids)), collapse = ",")
  query       <- paste0("SELECT Entrez_ID, Genname FROM Gene WHERE Entrez_ID IN (", platzhalter, ")")
  result      <- dbGetQuery(con, query, params = as.list(entrez_ids))
  
  # keep input order
  result <- result[match(entrez_ids, result$Entrez_ID), ]
  
  return(result$Genname)
}

######################################################################
# matches the extracted gene names with their ID in the Database
######################################################################

# Input values:
# Database connection object, vector of gene names
# Return: 
# character vector of entrez Ids

get_chosen_IDs_from_database <- function(con, gene_names) {
  
  platzhalter <- paste(rep("?", length(gene_names)), collapse = ",")
  query       <- paste0("SELECT Genname, Entrez_ID FROM Gene WHERE Genname IN (", platzhalter, ")")
  result      <- dbGetQuery(con, query, params = as.list(gene_names))
  
  # keep input order
  result <- result[match(gene_names, result$Genname), ]
  
  return(result$Entrez_ID)
}


######################################################################
# returns all pathway names which are stored in the database
######################################################################
# the output should be used for the GUI selection
# ==> it grants that the same name is used as stored in the database
# Input value
# database connection object
# Return: 
# Character vector of pathway names

get_pathwaynames_from_database <- function(con) {
  
  query    <- "SELECT Name FROM Pathway"
  pathways <- dbGetQuery(con, query)
  
  return(pathways[[1]])
}


#get all gennames from the database
get_all_genes_from_database <- function(con){


  query    <- "SELECT Genname FROM Gene"
  genes <- dbGetQuery(con, query)
  
  return(genes[[1]])


}


######################################################################
# returns all entrez ids which belong to the chosen pathway(s)
######################################################################
# unique gen IDs will be returned 
# Input values:
# database connection object
# a character vector of chosen pathways in the GUI
# Return: 
# integer vector of unique entrez IDs ==> no duplicates 

get_geneIDS_for_pathways <- function(chosen_pathways, con) {
  resultvec <- c()
  
  for (i in seq_along(chosen_pathways)) {
    query <- "SELECT l.Entrez_ID FROM Pathway AS p, Lookup_Gene_Pathway AS l 
              WHERE p.Pathway_ID = l.Pathway_ID AND p.Name = ?"
    
    result    <- dbGetQuery(con, query, params = chosen_pathways[i])
    resultvec <- c(resultvec, result$Entrez_ID)
  }
  
  return(unique(resultvec))
}

######################################################################
# returns all gene names which belong to the chosen pathway(s)
######################################################################
# unique gen IDs will be returned 
# Input values:
# database connection object
# a character vector of chosen pathways in the GUI
# Return: 
# character vector of unique gene names  ==> no duplicates 

get_gene_names_for_pathways <- function(chosen_pathways, con) {
  resultvec <- c()
  
  for (i in seq_along(chosen_pathways)) {
    query <- "SELECT g.Genname 
              FROM Pathway AS p, Lookup_Gene_Pathway AS l, Gene AS g
              WHERE p.Pathway_ID = l.Pathway_ID 
              AND l.Entrez_ID = g.Entrez_ID
              AND p.Name = ?"
    
    result    <- dbGetQuery(con, query, params = chosen_pathways[i])
    resultvec <- c(resultvec, result$Genname)
  }
  
  return(unique(resultvec))
}


###########################################################################
# filters the original dataset and only shows the genes which were selected
############################################################################
# previously selected 
# Input Value: 
# Integer vector of previously selected Entrez IDs
# originally processed dataframe without meta data but which adjusted first column 
# Return: 
# filtered dataset

extract_relevant_genes <- function(extracted_genes, original_data) {
  
  extracted_dataset <- original_data[original_data[, 1] %in% extracted_genes, ]
  
  return(extracted_dataset)
}

#######################################################################
# it handles duplicate Entrez_IDs by appending a numeric suffix 
######################################################################
# it Converts Entrez_ID to character in order to compare the ids with suffixes
# Input value:
# the final minimized Dataset 
# Return.
# dataframe with unique entrez IDs possibly with suffixes 

rename_duplikate_genes <- function(extracted_dataset) {
  
  ids <- as.character(extracted_dataset$Entrez_ID)  
  duplicate_values <- unique(ids[duplicated(ids)])
  
  if (length(duplicate_values) > 0) {
    for (dup_id in duplicate_values) {
      positions <- which(ids == dup_id)
      for (i in seq_along(positions)) {
        ids[positions[i]] <- paste0(ids[positions[i]], "_", i)
      }
    }
  }
  
  extracted_dataset$Entrez_ID <- ids  # ← zurückschreiben
  return(extracted_dataset)
}

# ============================================================
# HAUPTFUNKTION 
# ============================================================

# the function needs the chosen pathways from the GUI selection
# furthermore it needs the original dataset the connection object for the database
# named list is returned: filtered dataset, metadata, gene vector, gene names, list of unused 
# gene names and ids 
# if there are no matches with the chosen pathways ==> stop 


run_data_integration <- function(dataset, chosen_pathways, con) {


#if the first column is numeric 
if (any(grepl("^[0-9]+$", dataset[, 1]))) {

    preprocessed <- preprocess_dataset_meta(dataset)
    data_clean <- preprocessed$data_withoutmeta
    meta_data <- preprocessed$meta_data

    relevant_ids <- get_geneIDS_for_pathways(chosen_pathways, con)

    filtered <- extract_relevant_genes(relevant_ids, data_clean)

    if (nrow(filtered) == 0) {
        stop("No genes found which correspond to the chosen pathway ")
    }

    list_unused <- analyze_pathways_coverage(chosen_pathways, filtered,con)
    unused_matrix <- list_unused$matrix_unused
    unused_ids <- list_unused$missing_ids
  
    gene_names <- get_chosen_gennames_from_database(con, filtered$Entrez_ID)

} else {

    
    #if there are gene names in the first column 
    preprocessed <- preprocess_dataset_meta_gennames(dataset)
    data_clean <- preprocessed$data_withoutmeta
    meta_data <- preprocessed$meta_data

    relevant_gene_names <- get_gene_names_for_pathways(chosen_pathways,con)

    filtered <- extract_relevant_genes(relevant_gene_names, data_clean)


    if (nrow(filtered) == 0) {
        stop("No genes found which correspond to the chosen pathway ")
    }

    gene_names <- filtered[ ,1]
    entrez_ids <- get_chosen_IDs_from_database(con, filtered[, 1])
    filtered[, 1] <- entrez_ids
    list_unused <- analyze_pathways_coverage(chosen_pathways, filtered,con)
    unused_matrix <- list_unused$matrix_unused
    unused_ids <- list_unused$missing_ids

    colnames(filtered)[1] <- "Entrez_ID"
    
  }
  
  # rename duplicate values if ther are any
  filtered <- rename_duplikate_genes(filtered)
  
  # set rownames as entrez Ids and remove the column
  rownames(filtered) <- filtered$Entrez_ID
  filtered$Entrez_ID <- NULL
  
  # gene vector equals the row names now, not the first column anymore 
  gene_vector <- rownames(filtered)
  
  
  #return a named list 
  return(list(
    filtered_dataset = filtered,
    meta_data        = meta_data,
    gene_vector      = gene_vector,
    gene_names       = gene_names,
    matrix_unused    = unused_matrix,
    ids_unused       = unused_ids
  ))
}








