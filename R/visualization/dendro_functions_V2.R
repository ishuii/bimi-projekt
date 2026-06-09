library(ggplot2)


draw_segments_V2 <- function(coords, tree, labels, height, order, names) {
  
  # Basecase: leerer Baum
  if (is.null(tree)) return(list(segments = NULL, labels = NULL))
  
  # Basecase: Blatt
  if (!is.null(tree$id)) {
    return(list(
      segments = NULL,
      labels   = data.frame(
        x     = which(tree$id == order),
        y     = -0.8,
        label = names[tree$id]
      )
    ))
  }
  
  # Elternknoten
  left_coords  <- calculate_coords(order, height, tree$left)
  right_coords <- calculate_coords(order, height, tree$right)
  
  segments_df <- data.frame(
    x0 = c(left_coords$x,  left_coords$x,  right_coords$x),
    y0 = c(coords$y,       coords$y,       coords$y),
    x1 = c(right_coords$x, left_coords$x, right_coords$x),
    y1 = c(coords$y,       left_coords$y, right_coords$y)
  )
  
  left_result <- draw_segments_V2(
    left_coords,
    tree$left,
    labels,
    height,
    order,
    names
  )
  
  right_result <- draw_segments_V2(
    right_coords,
    tree$right,
    labels,
    height,
    order,
    names
  )
  
  return(list(
    segments = rbind(segments_df, left_result$segments, right_result$segments),
    labels   = rbind(left_result$labels, right_result$labels)
  ))
}


plot_dendro_V2 <- function(coords, tree, order, height, labels, names, title=""){
  
  result <- draw_segments_V2(
    coords,
    tree,
    labels,
    height,
    order,
    names)
  
  plot <- ggplot() +
          geom_segment(data = result$segments,
                 aes(x=x0, y=y0, xend=x1, yend=y1)) +
    
          geom_text(data = result$labels,
              aes(x=x, y=y, label=label),
              angle=90) +
    
          ylim(c(-1.5, max(height))) +
          labs(y = "Distanz", x = "") +
          theme(panel.grid = element_blank()) +
          theme_classic()+
          ggtitle(title) 
  
  print(plot)
}