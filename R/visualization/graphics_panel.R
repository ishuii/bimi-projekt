library(plotly)

grafikpanel <- function(
    heatmap_plot,
    gene_dendro_data,
    patient_dendro_data,
    gene_order,
    patient_order,
    data_matrix,
    metaDaten_gefiltert,
    gene_names = NULL,
    patient_names = NULL
){
  
  # --------------------------------------------------------------------------
  # 1) Vorbereitung & Vektorberechnung (Massiver Performance-Gewinn)
  # --------------------------------------------------------------------------
  heatmap_fields <- create_heatmap_field_data(
    data_matrix = data_matrix,
    metaDaten_gefiltert = metaDaten_gefiltert
  )
  meta_cols <- setdiff(colnames(heatmap_fields), c("Gene", "Patient", "Expression"))
  
  n_genes    <- length(gene_order)
  n_patients <- length(patient_order)
  
  gene_range    <- c(0.5, n_genes + 0.5)
  patient_range <- c(0.5, n_patients + 0.5)
  
  patient_max_height <- patient_dendro_data$max_height
  gene_max_height    <- gene_dendro_data$max_height
  
  heatmap_built <- plotly_build(heatmap_plot)
  heatmap_xaxis <- heatmap_built$x$layout$xaxis
  heatmap_yaxis <- heatmap_built$x$layout$yaxis
  
  final <- plot_ly()
  
  # --------------------------------------------------------------------------
  # 2) Heatmap-Trace (Schneller, matrixbasierter Hover-Text)
  # --------------------------------------------------------------------------
  for (trace in heatmap_built$x$data) {
    if (identical(trace$type, "heatmap")) {
      
      n_rows <- length(trace$y)
      n_cols <- length(trace$x)
      custom_text <- matrix("", nrow = n_rows, ncol = n_cols)
      
      gene_labels    <- if (!is.null(heatmap_yaxis$ticktext)) heatmap_yaxis$ticktext else trace$y
      patient_labels <- if (!is.null(heatmap_xaxis$ticktext)) heatmap_xaxis$ticktext else trace$x
      
      for (i in 1:n_rows) {
        actual_gene_idx <- gene_order[n_rows - i + 1]
        raw_gene_id     <- rownames(data_matrix)[actual_gene_idx]
        display_gene    <- as.character(gene_labels[i])
        gene_sub        <- heatmap_fields[as.character(heatmap_fields$Gene) == raw_gene_id, ]
        
        for (j in 1:n_cols) {
          actual_pat_idx  <- patient_order[j]
          raw_patient_id  <- colnames(data_matrix)[actual_pat_idx]
          display_patient <- as.character(patient_labels[j])
          val_expression  <- trace$z[i, j]
          
          hover_text <- paste0(
            "<b>Gene:</b> ", display_gene, "<br>",
            "<b>Patient:</b> ", display_patient, "<br>",
            "<b>Expression:</b> ", round(val_expression, 4)
          )
          
          row_data <- gene_sub[as.character(gene_sub$Patient) == raw_patient_id, ]
          if (nrow(row_data) > 0 && length(meta_cols) > 0) {
            for (m in meta_cols) {
              hover_text <- paste0(hover_text, "<br><b>", m, ":</b> ", row_data[[m]])
            }
          }
          custom_text[i, j] <- hover_text
        }
      }
      
      final <- add_trace(
        final, x = trace$x, y = trace$y, z = trace$z, type = "heatmap",
        colorscale = trace$colorscale,
        showscale = TRUE,
        showlegend = FALSE,
        colorbar = list(
          title = "Expression",
          x = 1.01,              # Perfekt an den rechten Rand geschmiegt
          xanchor = "left",      
          len = 0.35,
          y = 0.1,
          yanchor = "bottom"
        ),
        text = custom_text, hoverinfo = "text", xaxis = "x", yaxis = "y"
      )
    }
    # HINWEIS: Der else-Zweig wurde gelöscht. ggplotly-Geisterpunkte werden ignoriert.
  }
  
  # --------------------------------------------------------------------------
  # 3) Patienten-Dendrogramm (Komplett Vektorisiert - kein "trace 1" mehr!)
  # --------------------------------------------------------------------------
  p_seg <- patient_dendro_data$draw_result$segments
  p_pal <- patient_dendro_data$color_vector
  
  # Sofortiger Filter gegen ungültige Datenreihen vorab
  p_seg <- p_seg[!is.na(p_seg$x0) & !is.na(p_seg$x1) & !is.na(p_seg$y0) & !is.na(p_seg$y1), ]
  p_seg <- p_seg[!is.na(p_seg$class) & p_seg$class != "", ]
  
  # Globale Zuordnung der Patientennamen (Vektor-Geschwindigkeit)
  leaf_positions <- p_seg$x1
  leaf_positions[leaf_positions < 1 | leaf_positions > n_patients] <- NA
  actual_indices <- patient_order[leaf_positions]
  display_names  <- if (!is.null(patient_names)) patient_names[actual_indices] else colnames(data_matrix)[actual_indices]
  
  # Nur noch 1 Trace pro Klasse (statt tausender Traces). Verhindert Abstürze.
  for (cl in unique(p_seg$class)) {
    sub_seg <- p_seg[p_seg$class == cl, ]
    if (nrow(sub_seg) == 0) next
    
    cl_color <- if (cl %in% names(p_pal)) p_pal[cl] else "black"
    
    x_coords <- as.vector(t(cbind(sub_seg$x0, sub_seg$x1, NA)))
    y_coords <- as.vector(t(cbind(sub_seg$y0, sub_seg$y1, NA)))
    
    sub_names <- display_names[p_seg$class == cl]
    hover_texts <- as.vector(t(cbind(sub_names, sub_names, NA)))
    hover_texts[is.na(hover_texts)] <- ""
    
    final <- add_trace(
      final, x = x_coords, y = y_coords, type = "scatter", mode = "lines", connectgaps = FALSE,
      line = list(color = cl_color, width = 1.5),
      name = as.character(cl),
      legendgroup = as.character(cl),
      showlegend = (cl != "Default"),
      text = hover_texts, hoverinfo = "text",
      hoverlabel = list(bgcolor = cl_color, bordercolor = cl_color, font = list(color = "white")),
      xaxis = "x2", yaxis = "y2"
    )
  }
  
  # --------------------------------------------------------------------------
  # 4) Gen-Dendrogramm (Ebenfalls effizient komprimiert)
  # --------------------------------------------------------------------------
  g_seg <- gene_dendro_data$draw_result$segments
  g_pal <- gene_dendro_data$color_vector
  
  g_seg <- g_seg[!is.na(g_seg$x0) & !is.na(g_seg$x1) & !is.na(g_seg$y0) & !is.na(g_seg$y1), ]
  
  for (cl in unique(g_seg$class)) {
    sub_seg <- g_seg[g_seg$class == cl, ]
    if (nrow(sub_seg) == 0) next
    cl_color <- if (cl %in% names(g_pal)) g_pal[cl] else "black"
    
    x_coords <- as.vector(t(cbind(-sub_seg$y0, -sub_seg$y1, NA)))
    y_coords <- as.vector(t(cbind(sub_seg$x0, sub_seg$x1, NA)))
    
    final <- add_trace(
      final, x = x_coords, y = y_coords, type = "scatter", mode = "lines", connectgaps = FALSE,
      line = list(color = cl_color, width = 1.5), hoverinfo = "none", showlegend = FALSE,
      xaxis = "x3", yaxis = "y3"
    )
  }
  
  # --------------------------------------------------------------------------
  # 5) Layout: Voll mitskalierend & ohne Platzverschwendung rechts
  # --------------------------------------------------------------------------
  final <- layout(
    final, dragmode = "zoom", hovermode = "closest",
    
    # X-Achsen: Heatmap stoppt exakt bei 0.82. Das gibt den Genen bis 1.0 genug Platz.
    xaxis = list(
      domain = c(0.18, 0.82), range = patient_range, type = "linear",
      tickmode = "array", tickvals = heatmap_xaxis$tickvals, ticktext = heatmap_xaxis$ticktext,
      tickangle = -90, showticklabels = TRUE, showgrid = FALSE, zeroline = FALSE
    ),
    xaxis2 = list(
      domain = c(0.18, 0.82), range = patient_range, matches = "x", 
      showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE
    ),
    xaxis3 = list(
      domain = c(0, 0.18), range = c(-gene_max_height, 0), 
      showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE
    ),
    
    # Y-Achsen
    yaxis = list(
      domain = c(0, 0.85), range = gene_range, side = "right", type = "linear",
      tickmode = "array", tickvals = heatmap_yaxis$tickvals, ticktext = heatmap_yaxis$ticktext,
      tickfont = list(size = 7), 
      showticklabels = TRUE, showgrid = FALSE, zeroline = FALSE,
      automargin = TRUE  # Passt die Lücke dynamisch an die Länge der Genbeschriftung an!
    ),
    yaxis2 = list(
      domain = c(0.85, 1.00), range = c(0, patient_max_height * 1.05), 
      showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE
    ),
    yaxis3 = list(
      domain = c(0, 0.85), range = gene_range, matches = "y", type = "linear", 
      showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE
    ),
    
    # Rechten Rand schmal halten (nur 80px Puffer), da die Legenden am rechten Außenrand kleben
    margin = list(l = 5, r = 80, t = 25, b = 120),
    
    # Klassen-Legende sitzt sauber rechtsbündig über der Colorbar
    legend = list(
      orientation = "v",
      x = 1.01,
      xanchor = "left",
      y = 0.9,
      yanchor = "top",
      title = list(text = "<b>Klassen</b>", font = list(size = 11))
    )
  )
  
  return(final)
}