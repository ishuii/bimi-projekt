#Heatmap 

library(ggplot2)
library(reshape2)
library(viridis)
library(RColorBrewer)
library(stringr)
library(plotly)


#heatmap function 
generate_heatmap <- function(data_matrix,
                             gene_order,
                             patient_order,
                             gene_names = NULL,
                             palette = NULL,
                             show_x_axis = FALSE
                             ) {
  
  # Sort rows and columns
  sorted_matrix <- data_matrix[gene_order, patient_order]
  
  # Convert matrix to long format
  df_plot <- melt(data_matrix)
  colnames(df_plot) <- c("Gene", "Patient", "Expression")
  
  # Correct display order
  
  display_gene_names <- make.unique(
    gene_names[gene_order]
  )
  
  df_plot$Gene <- factor(
    display_gene_names,
    levels = rev(display_gene_names)
  )
  
  df_plot$Patient <- factor(
    df_plot$Patient,
    levels = colnames(data_matrix)
  )
  
  if (!is.null(palette)) {
    
    heat_colors <- switch(
      palette,
      "viridis" = viridis::viridis(100),
      "RdYlBu"  = brewer.pal(11,"RdYlBu"),
      "RdBu"    = brewer.pal(11,"RdBu"),
      "PRGn"    = brewer.pal(11,"PRGn"),
      {
        warning("Unknown palette -> using viridis")
        viridis::viridis(100)
      }
    )
    
  } else {
    
    heat_colors <- viridis::viridis(100)
    
  }
  
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
      colours = heat_colors,
      name = "Expression",
      limits = range(df_plot$Expression, na.rm = TRUE)
    ) +
    
    scale_y_discrete(position = "right") +
    
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


extract_metadata_names <- function(metaDaten_gefiltert) {
  
  meta_rows <- grep(
    "^Meta_",
    rownames(metaDaten_gefiltert),
    value = TRUE
  )
  
  str_remove(
    meta_rows,
    "^Meta_"
  )
}

#add fields for meta data and hover function
create_heatmap_field_data <- function(data_matrix,
                                      metaDaten_gefiltert) {
  
  field_data <- melt(data_matrix)
  
  colnames(field_data) <- c(
    "Gene",
    "Patient",
    "Expression"
  )
  
  meta_rows <- grep(
    "^Meta_",
    rownames(metaDaten_gefiltert),
    value = TRUE
  )
  
  for (meta_name in meta_rows) {
    
    column_name <- stringr::str_remove(
      meta_name,
      "^Meta_"
    )
    
    meta_values <- as.character(
      metaDaten_gefiltert[meta_name, ]
    )
    
    names(meta_values) <- colnames(metaDaten_gefiltert)
    
    field_data[[column_name]] <-
      meta_values[
        as.character(field_data$Patient)
      ]
  }
  
  field_data
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

generate_heatmap_plotly <- function(
    data_matrix,
    gene_order,
    patient_order,
    gene_names = NULL,
    palette = NULL,
    show_x_axis = FALSE
){
  
  heatmap <- generate_heatmap(
    data_matrix   = data_matrix,
    gene_order    = gene_order,
    patient_order = patient_order,
    gene_names    = gene_names,
    palette       = palette,
    show_x_axis   = show_x_axis
  )
  
  heatmap_plotly <- ggplotly(
    heatmap,
    tooltip = c(
      "Gene",
      "Patient",
      "Expression"
    ),
    dynamicTicks = FALSE
  )
  
  built <- plotly_build(heatmap_plotly)
  
  built$x$layout$xaxis$fixedrange <- TRUE
  built$x$layout$yaxis$fixedrange <- TRUE
  
  heatmap_plotly <- layout(
    built,
    margin = list(
      l = 5,
      r = 60,
      t = 30,
      b = 5
    ),
    xaxis = list(
      fixedrange = TRUE
    ),
    yaxis = list(
      fixedrange = TRUE,
      side = "right"
    )
  )
  
  return(heatmap_plotly)
}