library(patchwork)
library(ggplot2)

#source("R/visualization/heatmap_final.R")
#source("R/visualization/final_dendrogram.R")

grafikpanel <- function(
    heatmap_plot,
    patient_dendro,
    gene_dendro
) {
  
  # ==========================================================
  # DENDROGRAMME BEREINIGEN
  # ==========================================================
  
  patient_dendro <- patient_dendro +
    theme(
      axis.title.x = element_blank(),
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank()
    )
  
  gene_dendro <- gene_dendro +
    coord_flip() +
    scale_y_reverse()+
    theme(
      axis.title.x = element_blank(),
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank()
    )
  # ==========================================================
  # HEATMAP BEREINIGEN
  # ==========================================================
  
  heatmap_plot <- heatmap_plot +
    theme(
      plot.title = element_blank(),
      axis.title = element_blank(),
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank()
    )
  
  # ==========================================================
  # PATCHWORK
  # ==========================================================
  
  final_plot <-
    
    (
      patchwork::plot_spacer() +
        patient_dendro
    ) /
    
    (
      gene_dendro +
        heatmap_plot
    ) +
    
    plot_layout(
      widths  = c(1.5, 6),
      heights = c(1.5, 6)
    )
  
  return(final_plot)
}