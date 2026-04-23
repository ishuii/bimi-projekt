library(RSQLite)
library(DBI)

con <- dbConnect(RSQLite::SQLite(), "GeneDatabase.sqlite")

#---------------------------------------
#function to preprocess original dataset
#----------------------------------------
dataset <- read.csv("data/TCGA_kidney_unnormalized.csv")

#Dataset needs header = True when reading csv 
#Genes cannot be originally used as rownames as they can appear more than once
preprocess_dataset <- function(data) {

    data_labels_index <- nrow(data)
    data_labels <- data[data_labels_index, ]

    data_withoutlabels <- data[-data_labels_index, ]


    #rename first column as Entrez_ID 
    #== corresponds to the name of the column in the database
    colnames(data_withoutlabels)[1] <- "Entrez_ID"

    return(list(data_withoutlabels, data_labels))

}

result <- preprocess_dataset(dataset)


dataset_processed <- result[[1]]
dataset_processed_labels <- result[[2]]




#--------------------------------------------------------------------
#function to get a specific column==> especially for pathway name use 
#--------------------------------------------------------------------
get_column_from_database <- function(con, tablename, columname) {

  query <- paste0("SELECT ", columname, " FROM ", tablename)

  columns <- dbGetQuery(con, query)

  return(columns)

}

#Use for GUI
columns <- get_column_from_database(con,"Pathway", "Name")
pathway_name <- columns$Name

#------------------------------------------------
#Function for one chosen pathway
#get all genes which correspond to the pathway
#-------------------------------------------------
get_genes_for_chosen_pathway <- function(userChoice,con){

query <- "SELECT l.Entrez_ID, p.Name FROM Pathway AS p, Lookup_Gene_Pathway AS l 
          WHERE p.Pathway_ID = l.Pathway_ID 
          AND p.Name = ?"

result <- dbGetQuery(conn = con, query, params= userChoice)

return(result)


}

entrez_ids <- get_genes_for_chosen_pathway("Metabolic pathways", con = con)


#----------------------------------------------------
#Function extract relevant Genes from original dataset
#-----------------------------------------------------

extract_relevant_genes <- function(extracted_genes, original_data){


extracted_dataset <- original_data[original_data$Entrez_ID %in% extracted_genes$Entrez_ID, ]

return(extracted_dataset)

}

extracted_dataset <- extract_relevant_genes(entrez_ids, dataset_processed)

