
source("data/database_functions_v2.r")

# Test dataset with duplicate genes

df_duplicates <- data.frame(
  Entrez_ID = c("30", "51", "51", "37", "1956", "672", "18", "8310", "21","37", "Meta_labels"),
  Sample_01 = c(14.2, 11.5, 8.9, 14.1, 10.3, 11.6, 5, 6, 7, 101,1),
  Sample_02 = c(13.8, 11.2, 9.1, 13.9, 10.5, 11.4, 1, 2, 3, 100,1),
  stringsAsFactors = FALSE
)


# Test dataset with more Meta Data 

df_duplicates2 <- data.frame(
  Entrez_ID = c("30", "51", "51", "37", "1956", "672", "18", "8310", "21","37", "Meta_labels", "Meta_gender", "Meta_age", "Meta_lol"),
  Sample_01 = c(14.2, 11.5, 8.9, 14.1, 10.3, 11.6, 5, 6, 7, 101,1,"M", 90, "XD"),
  Sample_02 = c(13.8, 11.2, 9.1, 13.9, 10.5, 11.4, 1, 2, 3, 100,1, "F", 33, "ROFL"),
  stringsAsFactors = FALSE
)



# Test dataset with empty values 

df_duplicates3 <- data.frame(
  Entrez_ID = c("30", "51", "51", "37", "1956", "672", "18", "8310", "21","37", "Meta_labels", "Meta_gender", "Meta_age", "Meta_lol"),
  Sample_01 = c(14.2, " ", 8.9, 14.1, "", 11.6, 5, 6, 7, 101,1,"M", 90, "XD"),
  Sample_02 = c(13.8, 11.2, 9.1, 13.9, 10.5, 11.4, 1, 2, 3, 100,1, "F", 33, "ROFL"),
  stringsAsFactors = FALSE
)


#Test Dataset with gene names 

df_duplicates4 <- data.frame(
  Gene_Name = c("acyl-CoA synthetase long chain family member 6", "carbonyl reductase 4", 
                "carnitine palmitoyltransferase 1A", "carnitine palmitoyltransferase 1B", 
                "carnitine palmitoyltransferase 1C", "carnitine palmitoyltransferase 2", 
                "enoyl-CoA hydratase, short chain 1", "enoyl-CoA hydratase and 3-hydroxyacyl CoA dehydrogenase", 
                "ELOVL fatty acid elongase 1", "ELOVL fatty acid elongase 2", 
                "ELOVL fatty acid elongase 3", "ELOVL fatty acid elongase 4", 
                "ELOVL fatty acid elongase 5", "ELOVL fatty acid elongase 6", 
                "ELOVL fatty acid elongase 7", "fatty acid desaturase 1", "fatty acid desaturase 2", "ELOVL fatty acid elongase 7",
                "Meta_labels", "Meta_gender"),
  
  Sample_01 = c(14.2, " ", 8.9, 14.1, 1,1, 11.6, 5, 6, 7, 101, 12.5, 11.5, 16, 120, 1.2, 1.5, 1000, 1, "M"),
  
  Sample_02 = c(13.8, 11.2, 9.1, 13.9, 1,10.5, 11.4, 1, 2, 3, 100, 14.2, " ", 8.9, 14.1, "", 11.6, 5, 1, "F"),
  
  stringsAsFactors = FALSE
)



# Test run 

con     <- dbConnect(RSQLite::SQLite(), "GeneDatabase.sqlite")
pathway_names <- get_pathwaynames_from_database(con = con)

result  <- run_data_integration(
  dataset          = df_duplicates4,
  chosen_pathways  = c("Fatty acid metabolism"),
  con              = con,
  dataset_type     = "Gene"
)


meta <- result$meta_data
gene_vec <- result$gene_vector
filtered_data <- result$filtered_dataset
gene_names <- result$gene_names

