library(RSQLite)
library(DBI)

# ============================================================
# HILFSFUNKTIONEN
# ============================================================

##################################################################################
# function to preprocess the original dataset
##################################################################################
# it splits off the last row as labels
# it generates random meta data for Gender and Age
# it renames the first column as Entrez_ID which equals the name in the database
# it changes the Datatype of the ID to integer

# Input Values:
# Dataset which was read by read csv and Header = True ===> important!!
# Return value:
# Named list with meta data and pure data 

preprocess_dataset <- function(data) {
  
  data_labels_index <- nrow(data)
  data_labels <- data[data_labels_index, ]
  data_withoutmeta <- data[-data_labels_index, ]
  
  # Random metadata rows
  meta_age <- sample(18:100, ncol(data), replace = TRUE)
  meta_age[1] <- "Meta_Samples"
  meta_gender <- sample(c("M", "F", "D"), ncol(data), replace = TRUE)
  meta_gender[1]<- "Meta_Gender"
  meta_data <- rbind(data_labels, meta_age, meta_gender)
  
  # First column: Entrez_ID and integer
  data_withoutmeta[[1]] <- as.integer(data_withoutmeta[[1]])
  colnames(data_withoutmeta)[1] <- "Entrez_ID"
  
  # return named list 
  return(list(
    data_withoutmeta = data_withoutmeta,
    meta_data = meta_data
  ))
}



#########################################################
#Preprocess dataset with meta data and ID as first column 
#########################################################
# it searches the index of the row Meta_labels
# and get all indices from that index till the last row of the dataset
# it renames the first column as Entrez_ID which equals the name in the database
# it changes the Datatype of the ID to integer

# Input Values:
# Dataset which was read by read csv and Header = True ===> important!!
# Return value:
# Named list with meta data and pure data 


preprocess_dataset_meta <- function(data) {
  
  data_labels_index <- which(data[, 1] == "Meta_labels")
  meta_indices <- data_labels_index : nrow(data)
  
  #dataframe with meta information as row name 
  data_meta <- data[meta_indices, ]
  rownames(data_meta) <- data_meta[, 1]
  data_meta <- data_meta[, -1]
  
  data_withoutmeta <- data[-meta_indices, ]
  
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


preprocess_dataset_meta_gennames <- function(data,con) {
  
  data_labels_index <- which(dat[, 1] == "Meta_labels")
  meta_indices <- data_labels_index : nrow(data)
  
  #dataframe with meta information as row name 
  data_meta <- data[meta_indices, ]
  rownames(data_meta) <- data_meta[, 1]
  data_meta <- data_meta[, -1]
  
  data_withoutmeta <- data[-meta_indices, ]
  
  entrez_ids <- get_chosen_IDs_from_database(con,data_withoutmeta[[1]])
  data_withoutmeta[ ,1] <- entrez_ids
  colnames(data_withoutmeta)[1] <- "Entrez_ID"
  
  # return named list 
  return(list(
    data_withoutmeta = data_withoutmeta,
    meta_data = data_meta
  ))
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
  
  # preserve input order
  result <- result[match(entrez_ids, result$Entrez_ID), ]
  
  return(result$Genname)
}

######################################################################
# matches the extracted Gene names with their ID in the Database
######################################################################

# Input values:
# Database connection object, vector of gene names
# Return: 
# character vector of entrez Ids

get_chosen_IDs_from_database <- function(con, gene_names) {
  
  platzhalter <- paste(rep("?", length(gene_names)), collapse = ",")
  query       <- paste0("SELECT Genname, Entrez_ID FROM Gene WHERE Genname IN (", platzhalter, ")")
  result      <- dbGetQuery(con, query, params = as.list(gene_names))
  
  # Reihenfolge erzwingen
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

######################################################################
# returns all entrez ids which belong to the chosen pathway(s)
######################################################################
# unique gen IDs will be returned 
# Input values:
# database connection object
# a character vector of chosen pathways in the GUI
# Return: 
# integer vector of unique entrez IDs ==> no duplicates 

get_genes_for_pathways <- function(chosen_pathways, con) {
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
# filters the original dataset and only shows the genes which were
######################################################################
# previously selected 
# Input Value: 
# Integer vector of previously selected Entrez IDs
# originally processed dataframe without meta data but which adjusted first column 
# Return: 
# filtered dataset
extract_relevant_genes <- function(extracted_genes, original_data) {
  
  extracted_dataset <- original_data[original_data$Entrez_ID %in% extracted_genes, ]
  
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
  
  duplicate_values <- unique(extracted_dataset$Entrez_ID[duplicated(extracted_dataset$Entrez_ID)])
  
  if (length(duplicate_values) > 0) {
    extracted_dataset$Entrez_ID <- as.character(extracted_dataset$Entrez_ID)
    
    for (dup_id in duplicate_values) {
      positions <- which(extracted_dataset$Entrez_ID == as.character(dup_id))
      for (i in seq_along(positions)) {
        extracted_dataset$Entrez_ID[positions[i]] <- paste0(extracted_dataset$Entrez_ID[positions[i]], "_", i)
      }
    }
  }
  
  return(extracted_dataset)
}


# ============================================================
# HAUPTFUNKTION 
# ============================================================

#mthe function needs the chosen pathways from the GUI selection as well as the type of dataset
# furthermore it needs the original dataset the connection object for the database
# named list is returned: filtered dataset, metadata, gene vector, gene names 


run_data_integration <- function(dataset, chosen_pathways, con, dataset_type) {
  
  
  if (dataset_type == "Entrez ID"){
    
    preprocessed <- preprocess_dataset_meta(dataset)
    data_clean <- preprocessed$data_withoutmeta
    meta_data <- preprocessed$meta_data
  }
  
  else {
    
    preprocessed <- preprocess_dataset_meta_gennames(dataset,con)
    data_clean <- preprocessed$data_withoutmeta
    meta_data <- preprocessed$meta_data
    
  }
  
  # Ids which correspond to the chosen pathway(s)
  relevant_ids <- get_genes_for_pathways(chosen_pathways, con)
  
  
  
  #filtered dataset
  filtered <- extract_relevant_genes(relevant_ids, data_clean)
  
  #if there are no matches in the dataset
  if (nrow(filtered) == 0) {
    stop("No genes found which correspond to the chosen pathway ")
  }
  
  # gen names which correspond to the Ids
  gene_names <- get_chosen_gennames_from_database(con, filtered$Entrez_ID)
  
  # rename duplicate values if thera are any
  filtered <- rename_duplikate_genes(filtered)
  
  # set rownames as entrez Ids and remove the column
  rownames(filtered) <- filtered$Entrez_ID
  filtered$Entrez_ID <- NULL
  
  # gene vector equals the row names now     
  gene_vector <- rownames(filtered)
  
  
  #return a named list 
  return(list(
    filtered_dataset = filtered,
    meta_data        = meta_data,
    gene_vector      = gene_vector,
    gene_names       = gene_names
  ))
}


# ============================================================
# BEISPIELAUFRUF 
# ============================================================
#
con     <- dbConnect(RSQLite::SQLite(), "GeneDatabase.sqlite")
dataset <- read.csv("/Users/alisa/Desktop/Bimi6/R_Projekt_Tests/TCGA_kidney_unnormalized_meta.csv", header = TRUE)

pathway_names <- get_pathwaynames_from_database(con = con)
result  <- run_data_integration(
  dataset          = dataset,
  chosen_pathways  = c("Fatty acid metabolism"),
  con              = con,
  dataset_type     = "Entrez ID"
)

meta <- result$meta_data
gene_vec <- result$gene_vector
filtered_data <- result$filtered_dataset
gene_names <- result$gene_names


