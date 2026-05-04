# Heatmap

# Mock: Color palettes provided by Johanna/GUI Team
color_palette_registry <- list(
  "standard" = c("blue", "white", "red"),
  "Viridis"  = c("#440154", "#21908C", "#FDE725"),
  "Magma"    = c("#000004", "#51127C", "#B63679")
)

# None of these are real values, used for testing purposes only


generate_heatmap <- function(data_matrix, order_vector, palette_selection = "standard") { # Required parameters for heatmap
  
  if (!(palette_selection %in% names(color_palette_registry))) { # To be programmed by GUI Team
    stop("Palette not found! Choose: ", paste(names(color_palette_registry), collapse = ", ")) 
  }
  
  selected_palette <- color_palette_registry[[palette_selection]] # To be programmed by GUI Team
  
  data_sorted <- data_matrix[order_vector, ] # Sorted based on order_vector
  data_plot <- t(data_sorted) # Transpose
  data_plot <- data_plot[, rev(1:ncol(data_plot))] # Reversed to read from top to bottom instead of bottom to top
  
  
  # Visualization of the heatmap
  layout(matrix(c(1, 2), nrow = 1), widths = c(4, 1))
  
  par(mar = c(7, 5, 4, 2)) 
  color_function <- colorRampPalette(selected_palette)(100)
  
  x_coordinates <- 1:nrow(data_plot)
  y_coordinates <- 1:ncol(data_plot)
  
  image(x = x_coordinates, y = y_coordinates, z = data_plot, 
        col = color_function, axes = FALSE, xlab = "", ylab = "",
        main = paste("Heatmap"))
  
  axis(1, at = x_coordinates, labels = colnames(data_matrix), las = 2)
  axis(2, at = y_coordinates, labels = rev(rownames(data_sorted)), las = 1)
  
  # Visualization of the legend
  par(mar = c(7, 1, 4, 3))
  legend_values <- seq(min(data_matrix), max(data_matrix), length.out = 100)
  image(x = 1, y = legend_values, z = matrix(legend_values, nrow = 1), 
        col = color_function, axes = FALSE, xlab = "", ylab = "")
  axis(4, las = 1)
  mtext("Expression", side = 3, line = 1, cex = 0.8)
  
  layout(1)
}


generate_heatmap(expr_matrix_sorted, order_vector, "Viridis")



 

