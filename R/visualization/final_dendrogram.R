####===========================================================================
# This is the main entry point for the dendrogram visualization.
#
# - generate_dendro() : runs the full pipeline and returns a ggplot object
#
# Dependencies (loaded automatically via source):
#   - final_tree_functions.R       : build_tree(), get_order_vector()
#   - final_dendrogram_functions.R : calculate_coords(), draw_segments(), plot_dendro()
#####===========================================================================

source("R/visualization/final_tree_functions.R")
source("R/visualization/final_dendrogram_functions.R")

#####===========================================================================
#                         GENERATE_DENDRO
#
# The main function — takes clustering results and produces a dendrogram plot.
# If class_labels are provided, branches and leaf names are colored by class
# and a legend is shown. If not, everything is drawn in black.
#####===========================================================================

generate_dendro <- function(cluster_result, tree_result, order_vector,
                            title="", names_vector=NULL, class_labels=NULL, palette = NULL) {

    # extracting merge heights from cluster result — needed for coordinate calculation
    cluster_height <- cluster_result$matched_at
    
    # deriving legend visibility from whether class labels were provided
    show_legend <- !is.null(class_labels)
    
    # building color palette from the detected classes
    
    color_vector = NULL
    # if no class labels given, the black fallback is handled in plot_dendro
    if (!is.null(class_labels)){
      
      detected_classes <- unique(class_labels)
      detected_classes <- detected_classes[!is.na(detected_classes) & detected_classes != ""]
      n <- length(detected_classes)
      default_colors   <- c("cyan", "orange", "purple", "green", "pink", "yellow", "blue", "red")
      
      # if palette is given => get color vectors
      
      if(!is.null(palette)){
      colors <- switch(
        palette,"viridis" = viridis::viridis(n,end = 0.8),
        "RdYlBu"  = brewer.pal(n, "RdYlBu"),
        "RdBu"    = brewer.pal(n, "RdBu"),
        "PRGn"    = brewer.pal(n, "PRGn"),
        {
          # if given String doesn't match => warning + use default_colors
          warning(paste("Unbekannte Palette:", palette, "-> verwende Standardfarben"))
          default_colors[1:n]
          }
      )
      }
      
      # if no palette is given => fallback using default colors
      else {
        
        # fallback if more classlabels are detected, then available default colors
        if (n <= length(default_colors)) {
          colors <- default_colors[1:n]
        }
        else
          colors <- rainbow(length(detected_classes))
      }
      color_vector <- c(setNames(colors, detected_classes), "Default" = "black") 
    }
    
    # calculating root coordinates and collecting all segments and leaf metadata
    coords <- calculate_coords(tree_result, order_vector, cluster_height)
    
    draw_result <- draw_segments(
      node_coords  = coords,
      tree         = tree_result,
      order        = order_vector,
      height       = cluster_height,
      class_labels = class_labels
    )
    
    # handing everything to plot_dendro and returning the ggplot object
    final_plot <- plot_dendro(
      draw_result  = draw_result,
      max_height   = max(cluster_height),
      title        = title,
      palette      = color_vector,
      names_vector = names_vector,
      show_legend  = show_legend
    )
    
    return(final_plot)
  }