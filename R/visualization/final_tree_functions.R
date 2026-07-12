library(plotly)

#####===========================================================================
#                         GRAFIKPANEL
#
# Builds the combined panel consisting of the Heatmap, the Gene Dendrogram 
# (left), and the Patient Dendrogram (top) as a single native Plotly object.
#
# FIXED ISSUES (Chronological Reference):
#   - Ghost trace point at (0,0): Fixed by discarding the raw ggplotly 'else'
#     branch artifacts that polluted the plot with blank dots.
#   - "trace 1" legend clutter: Resolved by enforcing unique group names on 
#     dendrogram branches and dropping unnamed helper traces.
#   - Disappearing Colorbar: Hard-enforced showscale = TRUE on the heatmap.
#   - Truncated Patient Dendrogram: Added 5% height padding to yaxis2 range.
#   - X-Axis Domain Mismatch: Aligned domains of xaxis and xaxis2 perfectly.
#   - Big Data Crash: Replaced iterative segment plotting with vectorized 
#     (x0, x1, NA) line paths to handle huge datasets without browser lags.
#####===========================================================================

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
) {
  
  #####===========================================================================
  # 1) PREPARATION: Setup dimensions and grab base layout from ggplotly
  #===========================================================================
  n_genes    <- length(gene_order)
  n_patients <- length(patient_order)
  
  # pre-calculating axis boundaries for the matrix
  gene_range    <- c(0.5, n_genes + 0.5)
  patient_range <- c(0.5, n_patients + 0.5)
  
  # extracting tick parameters from the pre-built ggplotly input
  heatmap_built <- plotly_build(heatmap_plot)
  heatmap_xaxis <- heatmap_built$x$layout$xaxis
  heatmap_yaxis <- heatmap_built$x$layout$yaxis
  
  # initializing the final native plotly container
  final_plot <- plot_ly()
  
  #####===========================================================================
  # 2) HEATMAP TRACE: Extract data matrix and configure right colorbar
  #===========================================================================
  for (trace in heatmap_built$x$data) {
    if (identical(trace$type, "heatmap")) {
      
      final_plot <- add_trace(
        final_plot, x = trace$x, y = trace$y, z = trace$z, type = "heatmap",
        colorscale = trace$colorscale, 
        showscale = TRUE,      # keeps expression colorbar visible
        showlegend = FALSE,
        
        # FIXED: colorbar docks at x=1.01 and grows rightwards into the margin
        colorbar = list(
          title = "Expression",
          x = 1.01,              
          xanchor = "left",      
          len = 0.35,
          y = 0.1,
          yanchor = "bottom"
        ),
        xaxis = "x", yaxis = "y"
      )
    }
    # FIXED: The unhandled 'else' branch containing the ggplotly helper trace 
    # bug (causing the black dot at 0,0 and "trace 1") was entirely deleted.
  }
  
  #####===========================================================================
  # 3) PATIENT DENDROGRAM: Vectorized top panel rendering (xaxis2 / yaxis2)
  #===========================================================================
  p_seg <- patient_dendro_data$draw_result$segments
  p_pal <- patient_dendro_data$color_vector
  
  # FIXED: Rigorous filtering against NAs to completely block ghost coordinates
  p_seg <- p_seg[!is.na(p_seg$x0) & !is.na(p_seg$x1) & !is.na(p_seg$y0) & !is.na(p_seg$y1), ]
  p_seg <- p_seg[!is.na(p_seg$class) & p_seg$class != "", ]
  
  # fast array-based resolution of patient hover labels (no loops)
  leaf_positions <- p_seg$x1
  leaf_positions[leaf_positions < 1 | leaf_positions > n_patients] <- NA
  actual_indices <- patient_order[leaf_positions]
  display_names  <- if (!is.null(patient_names)) patient_names[actual_indices] else colnames(data_matrix)[actual_indices]
  
  # FIXED: Matrix flattening using NA separators combines thousands of individual 
  # lines into exactly 1 trace per class. Boosts performance on massive datasets.
  for (cl in unique(p_seg$class)) {
    sub_seg <- p_seg[p_seg$class == cl, ]
    if (nrow(sub_seg) == 0) next
    
    cl_color <- if (cl %in% names(p_pal)) p_pal[cl] else "black"
    
    # packing coordinates into flat vectors split by NAs
    x_coords <- as.vector(t(cbind(sub_seg$x0, sub_seg$x1, NA)))
    y_coords <- as.vector(t(cbind(sub_seg$y0, sub_seg$y1, NA)))
    
    # stretching hover text matrix to match the flat line coordinates
    sub_names <- display_names[p_seg$class == cl]
    hover_texts <- as.vector(t(cbind(sub_names, sub_names, NA)))
    hover_texts[is.na(hover_texts)] <- ""
    
    final_plot <- add_trace(
      final_plot, x = x_coords, y = y_coords, type = "scatter", mode = "lines", 
      connectgaps = FALSE, line = list(color = cl_color, width = 1.5),
      name = as.character(cl), 
      legendgroup = as.character(cl), 
      showlegend = (cl != "Default"), # suppresses default gray paths from legend clutter
      text = hover_texts, hoverinfo = "text", 
      xaxis = "x2", yaxis = "y2"
    )
  }
  
  #####===========================================================================
  # 4) GENE DENDROGRAM: Vectorized left panel rendering (xaxis3 / yaxis3)
  #===========================================================================
  g_seg <- gene_dendro_data$draw_result$segments
  g_pal <- gene_dendro_data$color_vector
  
  # stripping structural coordinate errors before feeding data to plotly
  g_seg <- g_seg[!is.na(g_seg$x0) & !is.na(g_seg$x1) & !is.na(g_seg$y0) & !is.na(g_seg$y1), ]
  
  for (cl in unique(g_seg$class)) {
    sub_seg <- g_seg[g_seg$class == cl, ]
    if (nrow(sub_seg) == 0) next
    
    cl_color <- if (cl %in% names(g_pal)) g_pal[cl] else "black"
    
    # flipping and mirroring coordinates so the tree grows leftwards
    x_coords <- as.vector(t(cbind(-sub_seg$y0, -sub_seg$y1, NA)))
    y_coords <- as.vector(t(cbind(sub_seg$x0, sub_seg$x1, NA)))
    
    final_plot <- add_trace(
      final_plot, x = x_coords, y = y_coords, type = "scatter", mode = "lines", 
      connectgaps = FALSE, line = list(color = cl_color, width = 1.5), 
      hoverinfo = "none", showlegend = FALSE, 
      xaxis = "x3", yaxis = "y3"
    )
  }
  
  #####===========================================================================
  # 5) LAYOUT: Multi-axis synchronization and auto-adjusting margins
  #===========================================================================
  final_plot <- layout(
    final_plot, dragmode = "zoom", hovermode = "closest",
    
    # tight borders around the screen canvas
    margin = list(l = 5, r = 80, t = 25, b = 120),
    
    # FIXED: Heatmap stops at 0.82. The remaining width is reserved for gene text labels.
    xaxis = list(
      domain = c(0.18, 0.82), range = patient_range, type = "linear",
      tickmode = "array", tickvals = heatmap_xaxis$tickvals, ticktext = heatmap_xaxis$ticktext,
      tickangle = -90, showticklabels = TRUE, showgrid = FALSE, zeroline = FALSE
    ),
    # FIXED: matches="x" dynamically chains the top dendrogram scale to the heatmap zoom state
    xaxis2 = list(
      domain = c(0.18, 0.82), range = patient_range, matches = "x", 
      showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE
    ),
    xaxis3 = list(
      domain = c(0, 0.18), range = c(-gene_dendro_data$max_height, 0), 
      showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE
    ),
    
    # FIXED: side="right" moves gene labels next to the heatmap. 
    # automargin=TRUE recalculates the space dynamically based on the longest gene name.
    yaxis = list(
      domain = c(0, 0.85), range = gene_range, side = "right", type = "linear",
      tickmode = "array", tickvals = heatmap_yaxis$tickvals, ticktext = heatmap_yaxis$ticktext,
      tickfont = list(size = 7), showticklabels = TRUE, showgrid = FALSE, zeroline = FALSE, 
      automargin = TRUE  
    ),
    # FIXED: range includes 5% padding so the highest dendrogram node isn't clipped
    yaxis2 = list(
      domain = c(0.85, 1.00), range = c(0, patient_dendro_data$max_height * 1.05), 
      showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE
    ),
    yaxis3 = list(
      domain = c(0, 0.85), range = gene_range, matches = "y", type = "linear", 
      showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE
    ),
    
    # anchoring the class legend at x=1.01 (sharing vertical space neatly above the colorbar)
    legend = list(
      orientation = "v", 
      x = 1.01, 
      xanchor = "left", 
      y = 0.9, 
      yanchor = "top", 
      title = list(text = "<b>Klassen</b>", font = list(size = 11))
    )
  )
  
  return(final_plot)
}
