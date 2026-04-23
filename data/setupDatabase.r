
#Install Packages if not installed, otherwise just load them 
if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")


library(RSQLite)
library(DBI)
#Library for Gene Annotations
library("org.Hs.eg.db")



#----------------------------------------
#Creating the Database with empty tables
#----------------------------------------

#Create Database if not already existing regarding ERM
#creating a con object which leads to the corresponding Database 
con <- dbConnect(RSQLite::SQLite(), "GeneDatabase.sqlite")

dbExecute(con, "
  CREATE TABLE if not exists Gene (
    Entrez_ID INTEGER PRIMARY KEY,
    Genname VARCHAR(255),
    Symbol VARCHAR(100)
  )
")

# Table Pathway
dbExecute(con, "
  CREATE TABLE if not exists Pathway (
    Pathway_ID varchar(100) PRIMARY KEY,
    Name VARCHAR(255)
  )
")


# Table N:M connecting Gene and Pathway by ID
dbExecute(con, "
  CREATE TABLE if not exists Lookup_Gene_Pathway (
    Lookup_ID INTEGER PRIMARY KEY AUTOINCREMENT,
    Pathway_ID INTEGER,
    Entrez_ID INTEGER,
    FOREIGN KEY (Pathway_ID) REFERENCES Pathway (Pathway_ID),
    FOREIGN KEY (Entrez_ID) REFERENCES Gene (Entrez_ID)
  )
")


#-------------------------------
#Filling the Database 
#--------------------------------

#---------------------
#Pathway
#---------------------
#Read CSV Data from REST API "https://rest.kegg.jp/list/pathway/hsa"
#Header = False, otherwise the first Dataset will be displayed as column name

pathway_names <- read.csv2("data/pathway_names.csv", header = FALSE)
colnames(pathway_names) <- c("Pathway_ID", "Name")

#Remove information Homo Sapiens, as we only use Human Genes 
pathway_names$Name <- gsub(" - Homo sapiens \\(human\\)", "", pathway_names$Name)


#---------------------
#Pathway and Gene
#---------------------
#Loading CSV Data from "https://rest.kegg.jp/link/pathway/hsa"
gene_pathways <- read.csv2("data/Gene_Pathway.csv", header= FALSE)
colnames(gene_pathways) <- c("Entrez_ID", "Pathway_ID")

# remove hsa: and path: in columns 
gene_pathways$Entrez_ID <- gsub("hsa:", "", gene_pathways$Entrez_ID)
gene_pathways$Pathway_ID <- gsub("path:", "", gene_pathways$Pathway_ID)



#--------------
#Gene
#--------------
#get all the unique genes which are present in the pathways
gene_id <- unique(gene_pathways$Entrez_ID)

gen_id_name_symbol <- AnnotationDbi::select(org.Hs.eg.db, 
                                   keys = gene_id, 
                                   columns = c("GENENAME","SYMBOL"), 
                                   keytype = "ENTREZID")

#Change datatype from character to integer
gen_id_name_symbol$ENTREZID <- as.integer(gen_id_name_symbol$ENTREZID)
colnames(gen_id_name_symbol) <- c("Entrez_ID", "Genname", "Symbol")

#Insert data into database
dbWriteTable(con,"Lookup_Gene_Pathway", gene_pathways,append = TRUE)
dbWriteTable(con,"Gene", gen_id_name_symbol,append = TRUE)
dbWriteTable(con, "Pathway", pathway_names, append = TRUE)


#eventuell case insensitive???