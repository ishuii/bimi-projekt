

source("data/database_functions_v2.r")
################################################
# function which inserts meta data in a dataset 
################################################


# input: 
# it requires a csv dataset and the path where it will be stored
create_meta_csv <- function(dataset,outputpath){

zeilen_index <- which(dataset[, 1] == "labels")
dataset[zeilen_index, 1] <- "Meta_labels"

gender <- sample(c("M", "F", "D"), size = ncol(dataset), replace = TRUE)
gender[1] <- "Meta_gender"
age <- sample(18:90, size = ncol(dataset), replace = TRUE)
age[1] <- "Meta_age"

dataset <- rbind(dataset, gender, age)

write.csv(dataset, outputpath,row.names = FALSE)

}

#input: 
create_dataset_gennames_meta_csv <- function(dataset, con, outputpath) {
  
  # Index der Labels-Zeile finden
  row_index <- grep("labels", dataset[, 1], ignore.case = FALSE, perl = FALSE)
  
  # Datenteil ohne Meta
  dataset_without_meta <- dataset[-row_index, ]
  data_index <- row_index - 1
  
  # Zufällige Gennamen aus DB einfügen
  genes <- sample(get_all_genes_from_database(con = con), size = data_index, replace = TRUE)
  dataset_without_meta[, 1] <- genes
  
  # Labels-Zeile direkt aus Originaldatensatz holen und umbenennen
  labels_row <- dataset[row_index, ]
  labels_row[1, 1] <- "Meta_labels"        # ← erste Zelle umbenennen
  
  # Meta-Zeilen erstellen
  gender <- sample(c("M", "F", "D"), size = ncol(dataset), replace = TRUE)
  gender[1] <- "Meta_gender"
  
  age <- sample(18:90, size = ncol(dataset), replace = TRUE)
  age[1] <- "Meta_age"
  
  # Alles zusammenfügen: Gendaten + Labels + Gender + Age
  dataset_final <- rbind(dataset_without_meta, labels_row, gender, age)
  
  write.csv(dataset_final, outputpath, row.names = FALSE)
}


##################
#Beispielaufruf
#################
con     <- dbConnect(RSQLite::SQLite(), "GeneDatabase.sqlite")

dataset <- read.csv("data/TCGA_kidney_unnormalized.csv", header = TRUE)

create_dataset_gennames_meta_csv(dataset,con, "/Users/alisa/Desktop/Bimi6/R_Projekt_Tests/TCGA_kidney_gene_names.csv")

#create_meta_csv(dataset = dataset, "data/TCGA_kidney_unnormalized_TOP10_meta.csv")






