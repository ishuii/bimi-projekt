

#Benannte Liste 
#Index vor wo was steht 
#- Gene Vektor 
#- Metadaten, liste von Vektoren, Labels 
#- Datensatz gefiltert


#Auswahl ob Genname oder Entrez ID
#Funktion schreiben für Genname in Entrez ID 

#alles in eine Hauptfunktion auslagern!!!! die Aufrufe der Funktionen sollen da rein 
#Meta_ Daten alles andere als die Labels 
#Nur Datensatz mit Entrez ID !!festlegen
#doppelte Werte behandlen 


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
#generates random Meta Values: Gender and Age 
preprocess_dataset <- function(data) {

  data_labels_index <- nrow(data)
  data_labels <- data[data_labels_index, ]

  data_withoutlabels <- data[-data_labels_index, ]

  #Random Values for Meta Data 
  meta_age <- sample(18:100, ncol(data), replace = TRUE)
  meta_age[1]<- "Meta_Samples"
  meta_gender <- sample(c("M", "F", "D"), ncol(data), replace = TRUE)
  meta_gender[1]<- "Meta_Gender"
  meta_data <- rbind(data_labels, meta_age, meta_gender)



  #rename first column as Entrez_ID and change datatype to integer
  data_withoutlabels[[1]] <- as.integer(data_withoutlabels[[1]]) 

  #Entrez_ID Column Name == corresponds to the name of the column in the database
  colnames(data_withoutlabels)[1] <- "Entrez_ID"

  return(list(data_withoutlabels, meta_data))

}


get_chosen_gennames_from_database <- function(con,entrez_ids) {

  platzhalter <- paste(rep("?", length(entrez_ids)), collapse = ",")

  query <- paste0("SELECT Genname FROM Gene WHERE Entrez_ID IN (", platzhalter, ")")
  gennames <- dbGetQuery(con, query, params = entrez_ids)

  return(gennames[[1]])

}


#return Pathway Names as a String vector ==> grants that the pathway selection in the GUI has the same names 
get_pathwaynames_from_database <- function(con) {

  query <- "SELECT Name from Pathway"
  pathways <- dbGetQuery(con, query)

  return(pathways[[1]])

}


#------------------------------------------------
#Function for Only one chosen pathway
#get all genes which correspond to the pathway
#returns a vector of the chosen Genese 
#-------------------------------------------------
get_genes_for_chosen_pathway <- function(userChoice,con){

query <- "SELECT l.Entrez_ID, p.Name FROM Pathway AS p, Lookup_Gene_Pathway AS l 
          WHERE p.Pathway_ID = l.Pathway_ID 
          AND p.Name = ?"

result <- dbGetQuery(conn = con, query, params= userChoice)

return(result$Entrez_ID)


}


#------------------------------------------------
#Function for more than one chosen pathway
#get all genes which correspond to the pathways
#input is a vector of all chosen pathways
#-------------------------------------------------
get_genes_for_pathways <- function(vector_userchoices,con){

resultvec <- c()

for (i in seq_along(vector_userchoices)){
      query <- "SELECT l.Entrez_ID, p.Name FROM Pathway AS p, Lookup_Gene_Pathway AS l 
              WHERE p.Pathway_ID = l.Pathway_ID 
              AND p.Name = ?"

result <- dbGetQuery(conn = con, query, params= vector_userchoices[i])
resultvec <- c(resultvec,result$Entrez_ID)

  
}

  return(unique(resultvec))

}


#----------------------------------------------------
#Function extract relevant Genes from original dataset
#-----------------------------------------------------

#Spaltenvektor ID zurückgeben

extract_relevant_genes <- function(extracted_genes, original_data){


extracted_dataset <- original_data[original_data$Entrez_ID %in% extracted_genes, ]



return(extracted_dataset)



}

rename_duplikate_genes <- function(extracted_dataset){

#genvec <- sample(1:40,30,replace = TRUE)

duplicate_values <- unique(extracted_dataset$Entrez_ID[duplicated(extracted_dataset$Entrez_ID)])

if (length(duplicate_values) > 0) {

  for(i in duplicate_values){

    #Position der doppelten 
    position <- which( extracted_dataset$Entrez_ID == i )

    for(i in seq_along(position)){

      extracted_dataset$Entrez_ID[position[i]] <- paste0(extracted_dataset$Entrez_ID[position[i]], "_", i)

    }

  }

}
  return(extracted_dataset)
}