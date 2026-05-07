
################################################
# function which inserts meta data in a dataset 
################################################


# input: 
# it requires a csv dataset and the path where it will be saved
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


##################
#Beispielaufruf
#################

dataset <- read.csv("data/TCGA_kidney_unnormalized_TOP10.csv", header = TRUE)

#create_meta_csv(dataset = dataset, "data/TCGA_kidney_unnormalized_TOP10_meta.csv")

#meta_csv <- read.csv( "data/TCGA_kidney_unnormalized_TOP10_meta.csv", header = TRUE)

#Werte



