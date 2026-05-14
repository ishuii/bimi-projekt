

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
# it requires a dataset with Entrez IDs and just labels 
create_dataset_gennames_meta_csv <- function(dataset, con) {

#find index where the word labels occurs
row_index <- grep("labels", dataset[, 1], ignore.case = FALSE, perl = FALSE)
# split dataset 
dataset_without_meta <- dataset[-row_index, ]
#save id vector 
ids <- dataset_without_meta[ ,1]

#transform ids in corresponding gene names and replace them in de dataset
genes <- get_chosen_gennames_from_database(con, ids)
dataset_without_meta[ ,1] <- genes
rbind(dataset_without_meta, dataset[row_index,])

#modify data with meta 
dataset_without_meta[row_index, 1] <- "Meta_labels"
gender <- sample(c("M", "F", "D"), size = ncol(dataset), replace = TRUE)
gender[1] <- "Meta_gender"
age <- sample(18:90, size = ncol(dataset), replace = TRUE)
age[1] <- "Meta_age"

dataset <- rbind(dataset_without_meta, gender, age)

return (dataset)
    
}


##################
#Beispielaufruf
#################
con     <- dbConnect(RSQLite::SQLite(), "GeneDatabase.sqlite")

dataset <- read.csv("data/TCGA_kidney_unnormalized_TOP10.csv", header = TRUE)

dataset_gene_meta <- create_dataset_gennames_meta_csv(dataset, con)

#create_meta_csv(dataset = dataset, "data/TCGA_kidney_unnormalized_TOP10_meta.csv")

#meta_csv <- read.csv( "data/TCGA_kidney_unnormalized_TOP10_meta.csv", header = TRUE)

#Werte





