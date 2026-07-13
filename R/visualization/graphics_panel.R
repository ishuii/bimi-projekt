library(plotly)

grafikpanel <- function(
    gene_dendro_data, patient_dendro_data,
    gene_order, patient_order, data_matrix, metaDaten_gefiltert,
    gene_names = NULL, patient_names = NULL, palette_name = "PRGn"
) {
  
  # Fetch mapping fields and filter out key columns for metadata looping
  heatmap_fields <- create_heatmap_field_data(data_matrix, metaDaten_gefiltert)
  metadata_columns <- setdiff(colnames(heatmap_fields), c("Gene", "Patient", "Expression"))
  
  total_genes    <- length(gene_order)
  total_patients <- length(patient_order)
  patient_range  <- c(0.5, total_patients + 0.5)
  gene_range     <- c(0.5, total_genes + 0.5)
  
  # ===========================================================================
  # INTEGRIERTER AUFRUF: Generierung des Heatmap-Plots direkt in der Funktion
  # ===========================================================================
  heatmap_plot <- generate_heatmap_plotly(
    data_matrix   = data_matrix,
    gene_order    = gene_order,
    patient_order = patient_order,
    gene_names    = gene_names,
    palette       = palette_name,  
    show_x_axis   = TRUE
  )
  
  heatmap_object <- plotly_build(heatmap_plot)
  heatmap_xaxis  <- heatmap_object$x$layout$xaxis
  heatmap_yaxis  <- heatmap_object$x$layout$yaxis
  
  final_panel <- plot_ly()
  
  # Step 1: Render Heatmap mit VEKTORISIERTER Hover-Erstellung (High Performance)
  for (data_trace in heatmap_object$x$data) {
    if (!identical(data_trace$type, "heatmap")) next
    
    # Labels sicher auslesen
    gene_labels    <- if (!is.null(heatmap_yaxis$ticktext)) heatmap_yaxis$ticktext else data_trace$y
    patient_labels <- if (!is.null(heatmap_xaxis$ticktext)) heatmap_xaxis$ticktext else data_trace$x
    
    # Vektorisierte Matrix-Generierung mit originalen Variablen
    matrix_hover_text <- outer(
      gene_labels, patient_labels, 
      function(g, p) paste0("<b>Gene:</b> ", g, "<br><b>Patient:</b> ", p)
    )
    
    matrix_hover_text <- matrix(
      paste0(matrix_hover_text, "<br><b>Expression:</b> ", round(data_trace$z, 4)), 
      nrow = total_genes
    )
    
    # Metadaten-Zusatz ebenfalls vektorisiert einbetten, falls vorhanden
    if (length(metadata_columns) > 0) {
      ordered_gene_ids    <- rownames(data_matrix)[gene_order[total_genes - (1:total_genes) + 1]]
      ordered_patient_ids <- colnames(data_matrix)[patient_order]
      
      meta_lookup <- data.frame(
        Key = paste0(heatmap_fields$Gene, "_", heatmap_fields$Patient),
        Strings = apply(heatmap_fields[, metadata_columns, drop=FALSE], 1, function(r) {
          paste0("<br><b>", metadata_columns, ":</b> ", r, collapse="")
        }),
        stringsAsFactors = FALSE
      )
      
      grid_keys <- outer(ordered_gene_ids, ordered_patient_ids, function(g, p) paste0(g, "_", p))
      match_idx <- match(grid_keys, meta_lookup$Key)
      
      meta_matrix <- matrix("", nrow = total_genes, ncol = total_patients)
      meta_matrix[!is.na(match_idx)] <- meta_lookup$Strings[match_idx[!is.na(match_idx)]]
      matrix_hover_text <- matrix(paste0(matrix_hover_text, meta_matrix), nrow = total_genes)
    }
    
    final_panel <- add_trace(
      final_panel, x = data_trace$x, y = data_trace$y, z = data_trace$z, type = "heatmap",
      colorscale = data_trace$colorscale, showscale = TRUE, showlegend = FALSE,
      colorbar = list(title = "Expression", x = 1.01, xanchor = "left", len = 0.35, y = 0.1, yanchor = "bottom"),
      text = matrix_hover_text, hoverinfo = "text", xaxis = "x", yaxis = "y"
    )
  }
  # ===========================================================================
  # Step 2: Render Patient Dendrogram branches (Live via optimiertem get_color)
  # ===========================================================================
  patient_segments <- patient_dendro_data$draw_result$segments
  patient_labels   <- patient_dendro_data$draw_result$labels
  
  if (!is.null(patient_segments) && nrow(patient_segments) > 0) {
    patient_segments <- na.omit(patient_segments)
    patient_palette  <- get_color(patient_segments$class, palette_name)
    
    for (current_class in unique(patient_segments$class)) {
      class_segments <- patient_segments[patient_segments$class == current_class, ]
      if (nrow(class_segments) == 0) next
      
      class_color <- if (current_class %in% names(patient_palette)) patient_palette[current_class] else "black"
      
      x_coords <- as.vector(t(cbind(class_segments$x0, class_segments$x1, NA)))
      y_coords <- as.vector(t(cbind(class_segments$y0, class_segments$y1, NA)))
      
      branch_hover <- if (current_class == "Default") {
        paste0("Distance: <b>", round(y_coords, 3), "</b>")
      } else {
        paste0("Group: <b>", current_class, "</b><br>Distance: <b>", round(y_coords, 3), "</b>")
      }
      
      # OPTIMIERUNG: width von 2 auf 1 gesetzt für dünnere, sauberere Linien
      final_panel <- add_trace(
        final_panel, x = x_coords, y = y_coords, type = "scatter", mode = "lines", connectgaps = FALSE,
        line = list(color = class_color, width = 1), name = as.character(current_class), 
        legendgroup = as.character(current_class), showlegend = (current_class != "Default"),
        text = branch_hover, hoverinfo = "text", xaxis = "x2", yaxis = "y2"
      )
    }
    
    if (!is.null(patient_labels) && nrow(patient_labels) > 0) {
      real_names <- if (!is.null(patient_names)) patient_names[patient_labels$id] else colnames(data_matrix)[patient_labels$id]
      leaf_hover <- ifelse(
        patient_labels$class == "Default", 
        paste0("Name: <b>", real_names, "</b>"), 
        paste0("Name: <b>", real_names, "</b><br>Group: <b>", patient_labels$class, "</b>")
      )
      patient_colors <- if (!is.null(patient_palette)) patient_palette[patient_labels$class] else "black"
      patient_colors[is.na(patient_colors)] <- "black"
      
      final_panel <- add_trace(
        final_panel, x = patient_labels$x, y = 0, type = "scatter", mode = "markers",
        marker = list(size = 8, color = patient_colors, opacity = 0), showlegend = FALSE, 
        text = leaf_hover, hoverinfo = "text", xaxis = "x2", yaxis = "y2"
      )
    }
  }
  
  # ===========================================================================
  # Step 3: Render Gene Dendrogram branches (Erzwungen: IMMER SCHWARZ)
  # ===========================================================================
  gene_segments <- gene_dendro_data$draw_result$segments
  gene_labels   <- gene_dendro_data$draw_result$labels
  
  if (!is.null(gene_segments) && nrow(gene_segments) > 0) {
    gene_segments <- na.omit(gene_segments)
    
    for (current_class in unique(gene_segments$class)) {
      class_segments <- gene_segments[gene_segments$class == current_class, ]
      if (nrow(class_segments) == 0) next
      
      x_coords <- as.vector(t(cbind(-class_segments$y0, -class_segments$y1, NA)))
      y_coords <- as.vector(t(cbind(class_segments$x0, class_segments$x1, NA)))
      
      # OPTIMIERUNG: width von 2 auf 1 gesetzt für dünnere, sauberere Linien
      final_panel <- add_trace(
        final_panel, x = x_coords, y = y_coords, type = "scatter", mode = "lines", connectgaps = FALSE,
        line = list(color = "black", width = 1), text = paste0("Distance: <b>", round(abs(x_coords), 3), "</b>"), 
        hoverinfo = "text", showlegend = FALSE, xaxis = "x3", yaxis = "y3"
      )
    }
    
    if (!is.null(gene_labels) && nrow(gene_labels) > 0) {
      plotly_y_positions <- total_genes - gene_labels$x + 1
      available_names <- if (!is.null(heatmap_yaxis$ticktext)) heatmap_yaxis$ticktext else heatmap_object$x$data[[1]]$y
      real_names <- available_names[plotly_y_positions]
      
      final_panel <- add_trace(
        final_panel, x = 0, y = plotly_y_positions, type = "scatter", mode = "markers",
        marker = list(size = 12, color = "black", opacity = 0), showlegend = FALSE, 
        text = paste0("Name: <b>", real_names, "</b>"), hoverinfo = "text", xaxis = "x3", yaxis = "y3"
      )
    }
  }
  # ===========================================================================
  # Step 4: Layout & Dynamische Schriftgrößen (Optimierte Skalierung & Abstände)
  # ===========================================================================
  # ERHÖHTE BASISGRÖSSE: Startet bei kleinen Datensätzen deutlich größer (bis zu 18pt)
  # und fällt selbst bei vielen Patienten nie unter 8pt.
  dynamic_xaxis_size <- max(8, min(18, 700 / total_patients))
  
  final_panel <- layout(
    final_panel, dragmode = "zoom", hovermode = "closest",
    margin = list(l = 25, r = 80, t = 25, b = 120),
    legend = list(orientation = "v", x = 1.01, xanchor = "left", y = 0.5, yanchor = "middle", title = list(text = "<b>Klassen</b>", font = list(size = 11))),
    
    xaxis  = list(domain = c(0.25, 0.88), range = patient_range, type = "linear", tickmode = "array", tickvals = heatmap_xaxis$tickvals, ticktext = heatmap_xaxis$ticktext, tickangle = -90, tickfont = list(size = dynamic_xaxis_size), showticklabels = TRUE, showgrid = FALSE, zeroline = FALSE),
    xaxis2 = list(domain = c(0.25, 0.88), range = patient_range, matches = "x", showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),
    xaxis3 = list(domain = c(0, 0.25), range = c(-gene_dendro_data$max_height, 0), showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),
    
    # Auch die Genbeschriftung (Y-Achse) profitiert hier von einem leichten Boost (von 6 auf 8pt)
    yaxis  = list(domain = c(0, 0.82), range = gene_range, side = "right", type = "linear", tickmode = "array", tickvals = heatmap_yaxis$tickvals, ticktext = heatmap_yaxis$ticktext, tickfont = list(size = 8), showticklabels = TRUE, showgrid = FALSE, zeroline = FALSE, automargin = TRUE),
    yaxis2 = list(domain = c(0.82, 1.00), range = c(0, patient_dendro_data$max_height * 1.05), showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),
    yaxis3 = list(domain = c(0, 0.82), range = gene_range, matches = "y", type = "linear", showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE)
  )
  
  return(final_panel)
}