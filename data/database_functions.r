library(RSQLite)
library(DBI)

#Connetion object
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

#--------------------------------
#Test function preprocess_dataset
#-------------------------------

result <- preprocess_dataset(dataset)

dataset_processed <- result[[1]]
dataset_processed_labels <- result[[2]]


#--------------------------------------------------------------------
#function to get a specific column==> especially for pathway name use 
#--------------------------------------------------------------------
get_column_from_database <- function(con, tablename, columname) {

  query <- paste0("SELECT ", columname, " FROM ", tablename, " Order by ",columname, " ASC" )

  columns <- dbGetQuery(con, query)

  return(columns)

}
#-------------
#Test function 
#Use for GUI
#-------------
columns <- get_column_from_database(con,"Pathway", "Name")
pathway_name <- columns$Name

#------------------------------------------------
#Function for one chosen pathway
#get all genes which correspond to the pathway
#-------------------------------------------------
get_genes_for_pathway <- function(userChoice,con){

query <- "SELECT l.Entrez_ID, p.Name FROM Pathway AS p, Lookup_Gene_Pathway AS l 
          WHERE p.Pathway_ID = l.Pathway_ID 
          AND p.Name = ?"

result <- dbGetQuery(conn = con, query, params= userChoice)

return(result$Entrez_ID)


}

#-------------
#test function 
#-------------
entrez_ids <- get_genes_for_pathway("Fatty acid metabolism", con = con)


#------------------------------------------------
#Function for more than one chosen pathway
#get all genes which correspond to the pathways
#-------------------------------------------------
get_genes_for_pathways <- function(vector_userchoices,con){

resultvec <- c()

for (i in 1:length(vector_userchoices)){
    query <- "SELECT l.Entrez_ID, p.Name FROM Pathway AS p, Lookup_Gene_Pathway AS l 
            WHERE p.Pathway_ID = l.Pathway_ID 
            AND p.Name = ?"

    result <- dbGetQuery(conn = con, query, params= vector_userchoices[i])

    resultvec <- c(resultvec,result$Entrez_ID)



}

return(unique(resultvec))

}

#---------------
#test function 
#-----------------

userinput <- c("Fatty acid metabolism", "Biosynthesis of nucleotide sugars")
entrez_ids_2 <- get_genes_for_pathways(userinput, con)




#----------------------------------------------------
#Function extract relevant Genes from original dataset
#-----------------------------------------------------

extract_relevant_genes <- function(extracted_genes, original_data){


extracted_dataset <- original_data[original_data$Entrez_ID %in% extracted_genes, ]

return(extracted_dataset)

}

#--------------
#test function 
#-------------

extracted_dataset <- extract_relevant_genes(entrez_ids_2, dataset_processed)

#aggregate if there are duplicates 
#final_exctracted_dataset <- aggregate(. ~ Entrez_ID, data = extracted_dataset, FUN = mean)