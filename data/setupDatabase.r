
#Install Packages if not installed, otherwise just load them 
if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")
library("org.Hs.eg.db")

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
pathway_names <- read.csv("pathway_names.csv", header = FALSE)
colnames(pathway_names) <- c("Pathway_ID", "Name")

#Spalten anpassen, Info Homo Sapiens entfernen 
pathway_names$Name <- gsub(" - Homo sapiens \\(human\\)", "", pathway_names$Name)


#man kann es so erstellen aber ERM Modell schwierig 
dbWriteTable(con, "Pathway", pathway_names, append = TRUE)



#---------------------
#Pathway und Gene
#---------------------


#Loading CSV Data with 
gene_pathways <- read.csv2("Gene_Pathway.csv", header= FALSE)
colnames(gene_pathways) <- c("Entrez_ID", "Pathway_ID")

#path und hsa entfernen aus den Spalten 
gene_pathways$Entrez_ID <- gsub("hsa:", "", gene_pathways$Entrez_ID)
gene_pathways$Pathway_ID <- gsub("path:", "", gene_pathways$Pathway_ID)

dbWriteTable(con,"Lookup_Gene_Pathway", gene_pathways,append = TRUE)





#--------------
#Gene
#--------------
#Gene, die in den Pathways vorkommen, nicht doppelt 
gene_IDs <- unique(gene_pathways$Entrez_ID)

gen_ID_Name_Symbol <- AnnotationDbi::select(org.Hs.eg.db, 
                                   keys = gene_IDs, 
                                   columns = c("GENENAME","SYMBOL"), 
                                   keytype = "ENTREZID")

gen_ID_Name_Symbol$ENTREZID <- as.integer(gen_ID_Name_Symbol$ENTREZID)
colnames(gen_ID_Name_Symbol) <- c("Entrez_ID", "Genname", "Symbol")

dbWriteTable(con,"gene", gen_ID_Name_Symbol,append = TRUE)