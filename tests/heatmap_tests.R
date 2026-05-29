#Heatmap with real data 
#I use ggplot for visualisation its easier for me an dominik to combine our code with ggplot 

library(ggplot2) 
library(reshape2) 
library(viridis) 
library(RColorBrewer) 

source("R/clustering/normalization_methods.R") 
source("R/visualization/heatmap.R")

# List of colours 
viridis <- viridis::viridis(100) 
RdYlBu <- brewer.pal(11, "RdYlBu") 
RdBu <- brewer.pal(11, "RdBu") 
PRGn <- brewer.pal(11, "PRGn") 


#load right csv data 

df <- read.csv( "data/TCGA_kidney_unnormalized_TOP10_meta.csv", 
                row.names = 1, 
                check.names = FALSE ) 

#get meta data 

metadata_df <- data.frame( Patient = colnames(df), 
                           meta_gender = as.character(df["Meta_gender", ]), 
                           meta_age = as.numeric(df["Meta_age", ]) ) 

rownames(metadata_df) <- metadata_df$Patient 

#remove unnecesarry rows (as i remember we just need gender, age and Expression) 

df <- df[ !(rownames(df) %in% c( "Gender:", "Age:", "Labels" )), ] 

#normalization 

df_normalized <- normalization(df, 1)

#remove labels 

df_normalized <- df_normalized[ rownames(df_normalized) != "labels", ] 

#heatmap fields

heatmap_fields <- create_heatmap_field_data( data_matrix = df_normalized, 
                                             metadata_df = metadata_df) 

# Keep only needed columns 

heatmap_fields <- heatmap_fields[, c( "Expression:", "Gender:", "Age:" )] 

################################################## 
# VERSION 1 - not all Patients 
################################################## 

df_na <- df_normalized[, 1:250] 

# Gene order 
gene_order_na <- 1:nrow(df_na) 
# cluster patients

hc_na <- hclust( dist(t(df_na)), 
                 method = "average" ) 

patient_order_na <- hc_na$order 

#show heatmap Version 1 
generate_heatmap( data_matrix = df_na, 
                  gene_order = gene_order_na, 
                  patient_order = patient_order_na ) 

#pdf with x axis 
p_pdf <- generate_heatmap( df_na, 
                           gene_order_na, 
                           patient_order_na, 
                           show_x_axis = TRUE ) 

ggsave( "heatmap_na.pdf", plot = p_pdf, width = 25, height = 16 ) 

################################################## 
# VERSION 2 - all Patiens
################################################## 

gene_order_all <- 1:nrow(df_normalized) 

# Cluster all
hc_all <- hclust( dist(t(df_normalized)), 
                  method = "average" ) 

patient_order_all <- hc_all$order 
#show heatmap Verson 2 

generate_heatmap( data_matrix = df_normalized, 
                  gene_order = gene_order_all, 
                  patient_order = patient_order_all ) 

#split pdf in 4, bigger then DIN A4 was still not enough (250 is just for test) 

patients_per_page <- 250 

pdf( "heatmap_split_4pages.pdf", 
     width = 25, 
     height = 16 ) 

for (i in seq(1, ncol(df_normalized), 
              by = patients_per_page)) { 
  
  end_index <- min( i + patients_per_page - 1, ncol(df_normalized) ) 
  
  current_patients <- patient_order_all[i:end_index] 
  
  p_split <- generate_heatmap( data_matrix = df_normalized, 
                               gene_order = gene_order_all, 
                               patient_order = current_patients, 
                               show_x_axis = TRUE ) 
  
  print(p_split) 
  
  } 

dev.off()
