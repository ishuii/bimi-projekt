#####===========================================================================
# This script extends Version 2 (V2) by implementing dynamic tree and label
# coloring based on provided class/cluster labels.
#
# FUNCTIONS INCLUDED:
# - draw_segments_V3() : Recursively collects coordinates and cluster IDs
# - plot_dendro_V3()   : Renders the final colored ggplot2 object
#####===========================================================================

library(ggplot2)

#####===========================================================================
#                         DRAW_SEGMENTS_V3

## This function extends the V2 layout by mapping original dataset categories 
# (class labels) to the tree structure. This allows ggplot2 to color branches 
# and leaf texts based on known data classes.
#####===========================================================================

draw_segments_V3 <- function(coords, tree, class_labels, height, order, names) { #changing labels to class_labels
  
  ##### LOGICAL STOPPING POINTS (BASE CASES) ###################################
  
  ## 1. Basecase: If the tree or branch is empty, there is nothing to collect.--
  # Return empty slots
  if (is.null(tree$left) && is.null(tree$right)) {
    
    # get the class_label of the leaf
    raw_class <- if (!is.null(class_labels)) as.character(class_labels[tree$id]) else "Default"
    
    # SICHERHEIT: Falls das Label in den Metadaten NA ist, fangen wir es ab
    leaf_class <- if (is.na(raw_class) || length(raw_class) == 0) "Default" else raw_class
    
    return(list(
      segments = NULL,
      labels   = data.frame(
        x     = which(order == tree$id), # Korrektur: Findet die X-Position des Namens im geordneten Vektor
        y     = 0,
        label = tree$id,                 # Korrektur: Da tree$id schon der Name ist, nutzen wir ihn direkt!
        
        # add a category to store the given class
        class = leaf_class
      ),
      current_class = leaf_class # Korrektur: Wird separat auf der Hauptebene zurückgegeben
    ))
  }
  
  #### PARENT NODE LOGIC (VOR DIE REKURSION GEZOGEN) ###########################
  #For an internal node, we need to know where its left and right children sit
  # so we can draw the connector lines. We use our existing calculator function for this
  
  left_coords  <- calculate_coords(order, height, tree$left)
  right_coords <- calculate_coords(order, height, tree$right)
  
  #### RECURSIVE STEP ##########################################################
  
  # To get the rest of the tree, we delegate the tracking to the left and right children
  # They will independently loop through their own sub-branches and return their maps
  left_result  <- draw_segments_V3(left_coords, tree$left, class_labels, height, order, names)
  right_result <- draw_segments_V3(right_coords, tree$right, class_labels, height, order, names)
  
  # Wir vergleichen die hochgereichten Klassen der beiden Kinder
  # SICHERHEIT: isTRUE() verhindert den Absturz, falls ein Kind unerwartet NA liefert
  vergleich_ist_gleich <- left_result$current_class == right_result$current_class
  
  parent_class <- if (!is.null(left_result$current_class) && 
                      !is.null(right_result$current_class) && 
                      isTRUE(vergleich_ist_gleich)) {
    left_result$current_class  # Beide Kinder sind gleich -> Kategorie wandert hoch
  } else {
    "Default"                  # Ungleiche Kinder -> Ast wird neutral (schwarz)
  }
  
  # Instead of calling segments(), we build a local data frame with 3 rows.
  # Row 1: horizontal bar -> left and right chil
  # Row 2: vertical bar -> down to the left child's height
  # Row 3: vertical bar -> down to the right child's height
  segments_df <- data.frame(
    x0 = c(left_coords$x,  left_coords$x,  right_coords$x),
    y0 = c(coords$y,       coords$y,       coords$y),
    x1 = c(right_coords$x, left_coords$x, right_coords$x),
    y1 = c(coords$y,       left_coords$y, right_coords$y),
    
    class = rep(parent_class, 3)
  )
  
  #### DATA AGGREGATION TO FINAL RESULT #########################################
  # This builds a single, complete dataset as we move back up to the root.
  return(list(
    segments      = rbind(segments_df, left_result$segments, right_result$segments),
    labels        = rbind(left_result$labels, right_result$labels),
    current_class = parent_class 
  ))
}

#####===========================================================================
#                         PLOT_DENDRO_V2
#
# THOUGHT PROCESS:
# This is the master function. It acts as the container. It triggers the 
# coordinate-collector and feeds the final, massive data frames into ggplot's geoms.
#####===========================================================================

plot_dendro_V3 <- function(draw_result, height, title="", palette=NULL) {
  
  # 1. Tabellen entpacken
  segments_df <- draw_result$segments
  labels_df   <- draw_result$labels
  
  # 2. Basis-Plot (Standardmäßig komplett SCHWARZ)
  plot <- ggplot() +
    geom_segment(data = segments_df, aes(x=x0, y=y0, xend=x1, yend=y1)) +
    geom_text(data = labels_df, aes(x=x, y=y, label=label), angle=90, hjust=1.1) +
    ylim(c(-1.5, max(height))) +
    labs(y = "Distanz", x = "") +
    theme_classic() + 
    theme(panel.grid = element_blank()) + 
    ggtitle(title) 
  
  # 3. Nur wenn eine Palette übergeben wurde, färben wir den Plot dynamisch ein
  if(!is.null(palette)) {
    
    # Hier holen wir uns die existierenden Klassen (falls wir sie für Checks brauchen)
    all_classes <- unique(c(segments_df$class, labels_df$class))
    detected_classes <- all_classes[all_classes != "Default"]
    
    # Sicherheitsnetz: Wir zwingen "Default"-Äste dazu, schwarz zu bleiben
    if (!"Default" %in% names(palette)) {
      palette["Default"] <- "black"
    }
    
    # Wir überschreiben den Plot mit der Farblogik und hängen die Palette an
    plot <- ggplot() +
      geom_segment(data = segments_df, aes(x=x0, y=y0, xend=x1, yend=y1, color=class)) +
      geom_text(data = labels_df, aes(x=x, y=y, label=label, color=class), angle=90, hjust=1.1) +
      ylim(c(-1.5, max(height))) +
      labs(y = "Distanz", x = "") +
      theme_classic() + 
      theme(panel.grid = element_blank()) + 
      ggtitle(title) +
      scale_color_manual(values = palette)
  }
  
  # 4. Plot ausgeben
  print(plot)
}