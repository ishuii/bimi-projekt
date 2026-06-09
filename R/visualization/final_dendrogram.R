#####===========================================================================
# This script is the final skript for generating the dendrogram
#
# FUNCTIONS INCLUDED:
# - match_classes
# - build_tree_V2()
# - get_order_vector
# - draw_segments_V3() : Recursively collects coordinates and cluster IDs
# - plot_dendro_V3()   : Renders the final colored ggplot2 object
#####===========================================================================
# ============================================================
# SOURCES
# ============================================================
source("R/visualization/final_tree_functions.R")
source("R/visualization/final_dendrogram_functions.R")


#####===========================================================================
#                         GENERATE_DENDRO
#
# Main function — orchestrates the full dendrogram pipeline.
# Segment drawing and plotting are delegated to final_dendrogram_functions.R
#####===========================================================================

generate_dendro <- function(cluster_result, tree_result, order_vector,
                            title="", names_vector=NULL, class_labels=NULL) {
  
  # ============================================================
  # VARIABLEN
  # ============================================================
  
  cluster_height <- cluster_result$matched_at
  
  # ============================================================
  # PALETTE BAUEN
  # Wenn class_labels vorhanden: Standardfarben, rainbow als Fallback
  # Wenn class_labels = NULL:    alles schwarz (wird in plot_dendro behandelt)
  # ============================================================
  
  built_palette <- NULL
  if (!is.null(class_labels)) {
    detected_classes <- unique(class_labels)
    detected_classes <- detected_classes[!is.na(detected_classes) & detected_classes != ""]
    default_colors   <- c("cyan", "orange","purple", "green", "pink", "yellow", "blue", "red")
    colors           <- if (length(detected_classes) <= length(default_colors)) default_colors[1:length(detected_classes)] else rainbow(n)
    built_palette    <- c(setNames(colors, detected_classes), "Default" = "gray")
  }
  
  # ============================================================
  # SEGMENTE SAMMELN
  # ============================================================
  
  coords <- calculate_coords(tree_result, order_vector, cluster_height)
  
  show_legend    <- !is.null(class_labels)
  
  draw_result <- draw_segments(
    node_coords  = coords,
    tree         = tree_result,
    order        = order_vector,
    height       = cluster_height,
    class_labels = class_labels
  )
  
  # ============================================================
  # PLOTTEN
  # ============================================================
  
  final_plot <- plot_dendro(
    draw_result  = draw_result,
    max_height   = max(cluster_height),
    title        = title,
    palette      = built_palette,
    names_vector = names_vector,
    show_legend  = show_legend
  )
  
  print(final_plot)
  return(final_plot)
}

