

#all pathways with name for human genome 
#read delim in order to read files which are separated by tabulator 
pathways <- read.delim("https://rest.kegg.jp/list/pathway/hsa", header = FALSE)

#genes with entrez ID for a specific pathway hsa01100
#data structure = dataframe
genes_pathway <- read.delim("https://rest.kegg.jp/link/hsa/hsa01100", header = FALSE)

#rename columns
names(genes_pathway) <- c("hsa01100", "Gene ID")

#remove "path in column hsa"
genes_pathway$hsa01100 <- gsub("path:", "", genes_pathway$hsa01100)

#does gene hsa.... exist in dataframe 
result <- genes_pathway[genes_pathway[, 2] == "hsa:1000000", ]


#Genom data hsa:2997
#http get request 
# ==> Character vector
genom_data <- readLines("https://rest.kegg.jp/get/hsa:2997")

gene_name <- genom_data[3]











#read data from csv 
data_kidney <- read.csv("/Users/alisa/Desktop/Bimi6/R/R_Projekt/bimi-projekt/data/TCGA_kidney_unnormalized_TOP10.csv", row.names = 1)

gene_data <- data_kidney[1:10, ]
labels_data <- data_kidney[nrow(data_kidney), ]

# gene IDS are columns not rows
# rows are samples of the patients

transposed_gene_data <- t(gene_data)
transposed_labels_data <- t(labels_data)

print(transposed_labels_data)

# what do the abbreviations mean fex TCGA.MH.A55Z.01A.11R.A26U.07 ???
#returns a vector ???? are we allowed to use it?
d_matrix <- dist(transposed_gene_data)








