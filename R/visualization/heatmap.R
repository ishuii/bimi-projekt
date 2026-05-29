#Heatmap 

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
generate_heatmap <- function(data_matrix,
                             gene_order,
                             patient_order,
                             show_x_axis = FALSE) {
  
  # Sort rows and columns
  sorted_matrix <- data_matrix[gene_order, patient_order]
  
  # Convert matrix to long format
  df_plot <- melt(sorted_matrix)
  colnames(df_plot) <- c("Gene", "Patient", "Expression")
  
  # Correct display order
  df_plot$Gene <- factor(
    df_plot$Gene,
    levels = rev(rownames(sorted_matrix))
  )
  
  df_plot$Patient <- factor(
    df_plot$Patient,
    levels = colnames(sorted_matrix)
  )
  
  # Heatmap
  p <- ggplot(
    df_plot,
    aes(
      x = Patient,
      y = Gene,
      fill = Expression
    )
  ) +
    
    geom_tile(color = "grey85") +
    
    scale_fill_gradientn(
      colours = RdYlBu,
      name = "Expression",
      limits = range(df_plot$Expression, na.rm = TRUE)
    ) +
    
    labs(
      title = "Gene Expression Heatmap",
      x = "Patients",
      y = "Genes"
    ) +
    
    theme_minimal() +
    
    theme(
      axis.text.x = element_text(
        angle = 90,
        vjust = 1,
        hjust = 1,
        size = 4
      ),
      axis.ticks.x = element_line()
    ) +
    
    guides(
      fill = guide_colorbar(
        barwidth = 3.1,
        barheight = 8,
        title.position = "top",
        title.hjust = 0.5
      )
    )
  
  #hide x axis 
  if (!show_x_axis) {
    p <- p + theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  }
  
  return(p)
}

#add fields for meta data and hover function
create_heatmap_field_data <- function(data_matrix,
                                      metadata_df) {
  
  field_data <- melt(data_matrix)
  
  colnames(field_data) <- c(
    "Gene",
    "Patient",
    "Expression:"
  )
  
  field_data$`Gender:` <- metadata_df[
    field_data$Patient,
    "meta_gender"
  ]
  
  field_data$`Age:` <- metadata_df[
    field_data$Patient,
    "meta_age"
  ]
  
  return(field_data)
}

#clustering
get_patient_order <- function(df_normalized, method = "average") {
  
  hc <- hclust(dist(t(df_normalized)), method = method)
  return(hc$order)
}

#Save as PDF with x axis 

heatmap_pdf <- function(df_normalized,
                        patient_order,
                        gene_order = NULL,
                        file = "Heatmap.pdf",
                        pages = 4,
                        width = 25,
                        height = 16,
                        show_x_axis = TRUE) {
  
  # default gene order
  if (is.null(gene_order)) {
    gene_order <- 1:nrow(df_normalized)
  }
  
  # split patients into equal chunks
  n_patients <- length(patient_order)
  patients_per_page <- ceiling(n_patients / pages)
  
  pdf(file, width = width, height = height)
  
  for (i in seq(1, n_patients, by = patients_per_page)) {
    
    end_index <- min(i + patients_per_page - 1, n_patients)
    
    current_patients <- patient_order[i:end_index]
    
    p <- generate_heatmap(
      data_matrix   = df_normalized,
      gene_order    = gene_order,
      patient_order = current_patients,
      show_x_axis   = show_x_axis
    )
    
    print(p)
  }
  
  dev.off()
}