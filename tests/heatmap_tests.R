#Heatmap with real data 
#I use ggplot for visualisation its easier for me an dominik to combine our code with ggplot 

library(ggplot2) 
library(reshape2) 
library(viridis) 
library(RColorBrewer) 

source("R/clustering/normalization_methods.R") 

# List of colours 
viridis <- viridis::viridis(100) 
RdYlBu <- brewer.pal(11, "RdYlBu") 
RdBu <- brewer.pal(11, "RdBu") 
PRGn <- brewer.pal(11, "PRGn") 

#heatmap function 
generate_heatmap <- function(data_matrix, gene_order, patient_order, show_x_axis = FALSE) { 

# Sort rows and columns 
sorted_matrix <- data_matrix[gene_order, patient_order] 

# Convert matrix to long format 
df_plot <- melt(sorted_matrix) colnames(df_plot) <- c("Gene", "Patient", "Expression") 

# Correct display order 
df_plot$Gene <- factor( df_plot$Gene, levels = rev(rownames(sorted_matrix)) ) 
df_plot$Patient <- factor( df_plot$Patient, levels = colnames(sorted_matrix) ) 

# Heatmap 
p <- ggplot( 
  df_plot, 
  aes( x = Patient, y = Gene, fill = Expression ) ) +

  geom_tile(color = "grey85") + 
  
  scale_fill_gradientn( colours = RdYlBu, 
                        name = "Expression", 
                        limits = range(df_plot$Expression, na.rm = TRUE) ) + 
  
  labs( title = "Gene Expression Heatmap", 
            x = "Patients", 
            y = "Genes" ) + 
  
  theme_minimal() + 
  
  theme( axis.text.x = element_text( angle = 90, 
                                     vjust = 1, 
                                     hjust = 1, 
                                     size = 4 ), 
         axis.ticks.x = element_line() ) + 
  
  guides( fill = guide_colorbar( barwidth = 3.1, 
                                 barheight = 8, 
                                 title.position = "top", 
                                 title.hjust = 0.5 ) ) 

#hide x axis 

if (!show_x_axis) { 
  p <- p + theme(
    axis.text.x = element_blank(), axis.ticks.x = element_blank() )
} 

return(p) 

} 

#add fields for meta data and hover function

create_heatmap_field_data <- function(data_matrix, metadata_df, cluster_vector) { 
  
  field_data <- melt(data_matrix) 
  
  colnames(field_data) <- c( "Gene", "Patient", "Expression:" ) 
  
  field_data$`Gender:` <- metadata_df[field_data$Patient, "meta_gender" ] 
  field_data$`Age:` <- metadata_df[ field_data$Patient, "meta_age" ] 
  
  return(field_data) 
  
  } 

#load right csv data 

df <- read.csv( "C:/Users/Huawei/Desktop/Projekt LL/bimi-projekt-dev/data/TCGA_kidney_unnormalized_TOP10_meta.csv", 
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

df_normalized <- normalize_log_zscore(df) 

#remove labels 

df_normalized <- df_normalized[ rownames(df_normalized) != "labels", ] 

#heatmap fields 

heatmap_fields <- create_heatmap_field_data( data_matrix = df_normalized, 
                                             metadata_df = metadata_df, 
                                             cluster_vector = cluster_vector ) 

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
