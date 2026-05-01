library(RSQLite)
library(DBI)

# ============================================================
# HILFSFUNKTIONEN
# ============================================================

##################################################################################
# function to preprocess the original dataset
# it splits off the last row as labels
# it generates random meta data for Gender and Age
# it renames the first column as Entrez_ID which equals the name in the database
# it changes the Datatype of the ID to integer
##################################################################################
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


# matches the extracted Gene IDs with their name in the Database
# Input values:
# Database connection object, vector of entrez_ids
# Return: 
# character vector of gene names
get_chosen_gennames_from_database <- function(con, entrez_ids) {

  platzhalter <- paste(rep("?", length(entrez_ids)), collapse = ",")
  query       <- paste0("SELECT Genname FROM Gene WHERE Entrez_ID IN (", platzhalter, ")")
  gennames    <- dbGetQuery(con, query, params = as.list(entrez_ids))

  return(gennames[[1]])
}


# returns all pathway names which are stored in the database
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




# returns all entrez ids which belong to the chosen pathway(s)
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

# filters the original dataset and only shows the genes which were
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


# it handles duplicate Entrez_IDs by appending a numeric suffix 
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

run_data_integration <- function(dataset, chosen_pathways, con) {

  # preprocess data
  preprocessed <- preprocess_dataset(dataset)
  data_clean <- preprocessed$data_withoutmeta
  meta_data <- preprocessed$meta_data


  # Ids which correspond to the chosen pathway(s)
  relevant_ids <- get_genes_for_pathways(chosen_pathways, con)

  # gen names which correspond to the Ids
  gene_names <- get_chosen_gennames_from_database(con, relevant_ids)

  #filtered dataset
  filtered <- extract_relevant_genes(relevant_ids, data_clean)

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
dataset <- read.csv("data/TCGA_kidney_unnormalized.csv", header = TRUE)

pathway_names <- get_pathwaynames_from_database(con = con)
result  <- run_data_integration(
  dataset          = dataset,
  chosen_pathways  = c("Fatty acid metabolism", "Biosynthesis of nucleotide sugars"),
  con              = con
 )


