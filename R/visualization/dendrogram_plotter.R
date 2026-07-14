###===========================================================================
# This is the main entry point for the dendrogram rendering engines.
#
# - get_color()            : generates a named color vector based on class labels and a palette
# - plot_dendro_ggplot() : takes pre-calculated data and renders a ggplot2 object
# - plot_dendro_plotly() : takes pre-calculated data and renders a plotly object
#
# Dependencies (loaded automatically via source):
#    - generate_dendro_data() from your data script
#####===========================================================================

####===========================================================================
#                               GET_COLOR
#
# Generates a named color vector mapping each unique class to a specific hex code.
# Dynamically samples from viridis, RColorBrewer, or a custom default fallback list.
# Returns a translation vector that guarantees "Default" maps to black.
#####===========================================================================
get_color <- function(class_labels, palette) {
  
  # check input validity; return default black early if vectors are missing
  if (is.null(class_labels) || is.null(palette)) {
    return(c("Default" = "black"))
  }
  
  # extract unique groups and clean the vector by removing empty strings, NAs AND "Default"
  detected_classes <- unique(class_labels)
  # OPTIMIERUNG: Hier filtern wir "Default" direkt aus den Paletten-Klassen heraus!
  detected_classes <- detected_classes[!is.na(detected_classes) & detected_classes != "" & detected_classes != "Default"]
  n <- length(detected_classes)
  
  # if no classes remain after cleaning, fall back to default black mapping
  if (n == 0) {
    return(c("Default" = "black"))
  }
  
  # define standard hex colors as a backup if no library palette is selected
  default_colors <- c(
    "#0000FF", "#FF6C00", "#005300", "#A100FA", "#B2DF8A",
    "#FFD300", "#0096FF", "#9B4D00", "#00FFD2", "#FDBF6F",
    "#7B8100", "#960000", "#00646B", "#D60072", "#00FF00",
    "#FF0000", "#540066", "#00A278", "#000094", "#FFC0CB"
  )
  
  # if palette is given => get color vectors
  if (!is.null(palette)) {
    colors <- switch(
      palette,
      "viridis" = viridis::viridis(n, end = 0.8),
      "RdYlBu"  = {
        full <- RColorBrewer::brewer.pal(11, "RdYlBu")
        full <- full[-c(5, 6, 7)]                 
        full[round(seq(1, length(full), length.out = n))]
      },
      "RdBu"    = {
        full <- RColorBrewer::brewer.pal(11, "RdBu")
        full <- full[-c(5, 6, 7)]                 
        full[round(seq(1, length(full), length.out = n))]
      },
      "PRGn"    = {
        full <- RColorBrewer::brewer.pal(11, "PRGn")
        full <- full[-c(5, 6, 7)]                 
        full[round(seq(1, length(full), length.out = n))]
      },
      {
        warning(paste("Unbekannte Palette:", palette, "-> verwende Standardfarben"))
        default_colors[1:n]
      }
    )
  } else {
    colors <- default_colors[1:n]
  }
  
  # build the final translation vector and explicitly bind "Default" to black
  color_vector <- c(setNames(colors, detected_classes), "Default" = "black")
  
  return(color_vector)
}
#####===========================================================================
#                         PLOT_DENDRO_GGPLOT
#
# Akzeptiert jetzt das 'dendro_data' Objekt, behält das originale Layout 
# aber zu 100% exakt bei.
#####===========================================================================
#####===========================================================================
#                         PLOT_DENDRO_GGPLOT
#####===========================================================================
plot_dendro_ggplot <- function(dendro_data, title="", names_vector=NULL, palette_name="RdBu", show_legend=FALSE, show_x_axis=TRUE, show_y_axis=TRUE) {
  
  segments_df <- dendro_data$draw_result$segments
  labels_df   <- dendro_data$draw_result$labels
  max_height  <- dendro_data$max_height
  
  # Palette dynamic generation at runtime
  all_classes <- c(segments_df$class, labels_df$class)
  palette     <- get_color(all_classes, palette_name)
  
  # AUTOMATISCHE LEGENDEN-PRÜFUNG:
  # Wir schauen, welche eindeutigen Klassen existieren (ohne "Default")
  unique_classes <- unique(all_classes)
  has_real_classes <- any(unique_classes != "Default") && length(unique_classes) > 0
  
  labels_df$label <- if (!is.null(names_vector)) {
    names_vector[labels_df$id]
  } else {
    as.character(labels_df$id)
  }
  
  # HIER RECHTSCHREIB-FEHLER KORRIGIERT & HOCH-DYNAMISCHE TEXTLÄNGE:
  # Wir ermitteln die maximale Anzahl an Buchstaben der Labels
  max_char_len <- max(nchar(labels_df$label), na.rm = TRUE)
  
  # Set start of labels slightly below 0 (always 1% of the tree height)
  labels_df$y <- - (max_height * 0.01)
  
  # FESTES Y-LIMIT FÜR DEN BAUM:
  y_min <- - (max_height * 0.05)
  
  # Beautiful rounded breaks for the distance axis
  y_breaks <- pretty(c(0, max_height), n = 5)
  final_max_y <- max(max(y_breaks), max_height)
  
  # ELEMENT COUNT & DYNAMIC SIZING FOR CONTENT ONLY:
  n_elements <- nrow(labels_df)
  
  # FESTE SCHRIFTGRÖSSEN FÜR DIE ACHSEN 
  axis_title_size <- 12  
  axis_text_size  <- 10  
  
  # Dynamic Line Width (Ast-Dicke schrumpft weiterhin bei vielen Daten)
  dynamic_line_width <- max(0.15, min(0.7, 0.7 - ((n_elements - 50) * 0.001)))
  
  # Generate the plot
  plot <- ggplot() +
    geom_segment(
      data      = segments_df, 
      aes(x=x0, y=y0, xend=x1, yend=y1, color=class), 
      linewidth = dynamic_line_width
    ) +
    
    # Hier filtern wir "Default" aus der Legende, falls andere Klassen existieren
    scale_color_manual(
      values = palette, 
      breaks = names(palette)[names(palette) != "Default"]
    ) +
    
    scale_y_continuous(breaks = y_breaks) +
    
    # Zoom cleanly and PREVENT clipping of long labels
    coord_cartesian(ylim = c(y_min, final_max_y), expand = FALSE, clip = "off") +
    
    labs(y = "Distance", x = "") +
    ggtitle(title) +
    
    theme_classic() +
    theme(
      panel.grid   = element_blank(),
      axis.line    = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x  = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_text(size = axis_title_size),
      axis.text.y  = element_text(size = axis_text_size),
      plot.title   = element_text(size = axis_title_size + 2, face = "bold", hjust = 0.5),
      
      # DYNAMISCHER UNTERER RAND:
      # Kurze Namen brauchen nur ~60pt, extrem lange Namen bekommen bis zu 250pt Platz!
      plot.margin  = margin(15, 15, max(60, max_char_len * 4.5), 15, "pt")
    )
  
  if (show_x_axis) {
    # Leaf labels still scale so they don't overlap
    font_size <- max(0.6, min(5.5, 220 / n_elements))
    
    plot <- plot + geom_text(
      data        = labels_df, 
      aes(x = x, y = y, label = label, color = class), 
      angle       = 90, 
      hjust       = 1, 
      vjust       = 0.5, 
      size        = font_size,    
      show.legend = FALSE # Verhindert das hässliche "a" in der Legende!
    )
  }
  
  # ENTSCHEIDUNG: Wann zeigen wir die Legende?
  if (show_legend && has_real_classes) {
    plot <- plot + theme(legend.position = "right")
  } else {
    plot <- plot + theme(legend.position = "none")
  }
  
  if (!show_y_axis) {
    plot <- plot + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
  }
  
  return(plot)
}
#####===========================================================================
#                         PLOT_DENDRO_PLOTLY
#####===========================================================================
plot_dendro_plotly <- function(
    dendro_data, 
    side = "top",         # "top" (oben) oder "left" (links)
    names_vector = NULL,  # Die echten Namen für die Achsenbeschriftung
    palette_name = "RdBu", # Palette dynamic parameter introduction
    show_legend = FALSE,
    show_x_axis = TRUE,   # Kontrolliert die Namen unten
    show_y_axis = TRUE    # Kontrolliert die Distanz-Skala
) {
  
  # 1. Daten und Variablen entpacken
  segments_df <- dendro_data$draw_result$segments
  labels_df   <- dendro_data$draw_result$labels
  max_height  <- dendro_data$max_height
  elements    <- nrow(labels_df)
  
  if (is.null(segments_df) || nrow(segments_df) == 0) {
    return(plotly::plot_ly())
  }
  
  # Palette dynamic generation at runtime
  all_classes <- c(segments_df$class, labels_df$class)
  palette     <- get_color(all_classes, palette_name)
  
  # 2. Native Plotly-Fläche initialisieren
  plot <- plotly::plot_ly()
  
  # 3. Äste zeichnen mit dynamischem Hover-Text
  unique_classes <- unique(segments_df$class)
  for (cl in unique_classes) {
    cl_segs  <- segments_df[segments_df$class == cl, ]
    cl_color <- if (cl %in% names(palette)) palette[cl] else "black"
    
    if (nrow(cl_segs) == 0) next
    
    if (side == "top") {
      x_coords <- as.vector(t(cbind(cl_segs$x0, cl_segs$x1, NA)))
      y_coords <- as.vector(t(cbind(cl_segs$y0, cl_segs$y1, NA)))
    } else {
      x_coords <- as.vector(t(cbind(-cl_segs$y0, -cl_segs$y1, NA)))
      y_coords <- as.vector(t(cbind(cl_segs$x0, cl_segs$x1, NA)))
    }
    
    # DYNAMISCHER HOVER-TEXT FÜR ÄSTE: Keine Gruppe bei "Default"
    hover_texts <- if (cl == "Default") {
      paste0("Distance: <b>", round(y_coords, 3), "</b>")
    } else {
      paste0("Group: <b>", cl, "</b><br>Distance: <b>", round(y_coords, 3), "</b>")
    }
    
    plot <- plot %>% plotly::add_lines(
      x = x_coords, y = y_coords,
      line = list(color = cl_color, width = 1.5),
      name = as.character(cl),
      legendgroup = as.character(cl),
      showlegend = (show_legend && cl != "Default"),
      hoverinfo = "text",
      text = hover_texts
    )
    
    # --- 4. ANNOTATIONS & HOVER AN DEN ENDPUNKTEN (OHNE AUSDÜNNUNG) ---
    cl_labels <- labels_df[labels_df$class == cl, ]
    
    if (nrow(cl_labels) > 0) {
      all_names <- if (!is.null(names_vector)) names_vector[cl_labels$id] else as.character(cl_labels$id)
      
      # Hover-Netz (Nutzt alle Patienten der Klasse)
      leaf_hover_text <- if (cl == "Default") {
        paste0("Name: <b>", all_names, "</b>")
      } else {
        paste0("Name: <b>", all_names, "</b><br>Group: <b>", cl, "</b>")
      }
      
      if (side == "top") {
        plot <- plot %>% plotly::add_trace(
          type = "scatter", mode = "markers",
          x = cl_labels$x, y = 0,
          marker = list(size = 4, color = cl_color, opacity = 0), 
          hoverinfo = "text", text = leaf_hover_text, showlegend = FALSE
        )
      } else {
        plot <- plot %>% plotly::add_trace(
          type = "scatter", mode = "markers",
          x = 0, y = cl_labels$x,
          marker = list(size = 4, color = cl_color, opacity = 0),
          hoverinfo = "text", text = leaf_hover_text, showlegend = FALSE
        )
      }
      
      # ------------------------------------------------------------------------
      # Text-Achsenbeschriftung: Zeigt JEDEN Namen ohne Ausnahme an
      # ------------------------------------------------------------------------
      if (show_x_axis) {
        
        # Berechnet die Schriftgröße weiterhin dynamisch basierend auf der Gesamtanzahl,
        # damit es bei vielen Elementen zumindest versucht, lesbar zu bleiben.
        dynamic_size <- max(6.5, min(12, 13 - (elements / 20)))
        
        # Kosmetik: Wenn der Datensatz klein ist (< 100 Elemente), fetten wir den Text
        display_names <- all_names
        if (elements < 100) {
          display_names <- paste0("<b>", display_names, "</b>")
        }
        
        if (side == "top") {
          plot <- plot %>% plotly::add_annotations(
            x = cl_labels$x, y = 0,
            xref = "data", yref = "paper",
            text = display_names,
            showarrow = FALSE, textangle = -90,          
            xanchor = "center", yanchor = "top",          
            font = list(color = cl_color, size = dynamic_size),
            hoverinfo = "none"
          )
        } else {
          plot <- plot %>% plotly::add_annotations(
            x = 0, y = cl_labels$x,
            xref = "paper", yref = "data",
            text = display_names,
            showarrow = FALSE, textangle = 0,            
            xanchor = "right", yanchor = "middle",
            font = list(color = cl_color, size = dynamic_size),
            hoverinfo = "none"
          )
        }
      }
    }
  }
  
  # ============================================================================
  # 5. GRANULIERTE ACHSEN-SEGMENTIERUNG (nticks = 18)
  # ============================================================================
  max_char_len <- if (!is.null(names_vector)) max(nchar(as.character(names_vector)), na.rm = TRUE) else 10
  dynamic_margin <- max(80, max_char_len * 5.0)
  
  clean_axis <- list(
    showgrid = FALSE, showline = FALSE, zeroline = FALSE, 
    showticklabels = FALSE, ticks = "", title = "", fixedrange = FALSE
  )
  
  xaxis_config <- clean_axis
  yaxis_config <- clean_axis
  
  if (side == "top") {
    xaxis_config$range <- c(0.5, elements + 0.5)
    yaxis_config$range <- c(0, max_height * 1.05)
    
    if (show_y_axis) {
      yaxis_config$showticklabels <- TRUE
      yaxis_config$nticks         <- 18 
      
      plot <- plot %>% plotly::add_annotations(
        x = -0.06, y = 0.5, xref = "paper", yref = "paper",
        text = "Distance", showarrow = FALSE, textangle = -90,
        font = list(size = 12, color = "black"), hoverinfo = "none"
      )
    }
  } else {
    yaxis_config$range <- c(0.5, elements + 0.5)
    xaxis_config$range <- c(-max_height * 1.05, 0)
    
    if (show_y_axis) {
      xaxis_config$showticklabels <- TRUE
      xaxis_config$nticks         <- 18
      
      plot <- plot %>% plotly::add_annotations(
        x = 0.5, y = -0.06, xref = "paper", yref = "paper",
        text = "Distance", showarrow = FALSE, textangle = 0,
        font = list(size = 12, color = "black"), hoverinfo = "none"
      )
    }
  }
  
  # ============================================================================
  # 6. LAYOUT GENERIEREN
  # ============================================================================
  plot <- plot %>% 
    plotly::layout(
      xaxis         = xaxis_config, 
      yaxis         = yaxis_config,
      plot_bgcolor  = "white",  
      paper_bgcolor = "white",
      showlegend    = show_legend,
      margin        = list(
        b = if (show_x_axis && side == "top") dynamic_margin else 40,
        l = if (show_x_axis && side == "left") dynamic_margin else 75,
        r = 40,
        t = 40
      )
    ) %>% 
    plotly::config(scrollZoom = TRUE)
  
  return(plot)
}