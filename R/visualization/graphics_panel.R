#####===========================================================================
# This script contains the primary layout panel function for the interactive
# visualization dashboard
#
# - grafikpanel() : generates a unified interactive plotly panel combining the
#                   expression heatmap with co-aligned gene and patient dendrograms
#####===========================================================================


#####===========================================================================
#                             GRAFIKPANEL
#
# Creates a unified, interactive plot panel combining an expression heatmap
# with co-aligned gene (vertical) and patient (horizontal) hierarchical
# clustering dendrograms
#####===========================================================================

grafikpanel <- function(
    gene_dendro_data, patient_dendro_data,
    gene_order, patient_order, data_matrix, metaDaten_gefiltert,
    gene_names = NULL, patient_names = NULL, palette_name = "PRGn"
) {
  
  # Gene/Patient/Expression are structural columns not metadata 
  # => everything else in heatmap_fields is a Meta_ column meant for the hover text below
  heatmap_fields <- create_heatmap_field_data(data_matrix, metaDaten_gefiltert)
  metadata_columns <- setdiff(colnames(heatmap_fields), c("Gene", "Patient", "Expression"))
  
  # range is needed to set the axis bounds for both the heatmap (xaxis/yaxis)
  # and the matching dendrogram (xaxis2/yaxis3) => same range on both keeps
  # them visually aligned
  total_genes    <- length(gene_order)
  total_patients <- length(patient_order)
  patient_range  <- c(0.5, total_patients + 0.5)
  gene_range     <- c(0.5, total_genes + 0.5)
  
  ##### DYNAMIC SIZING CALCULATIONS #############################################
  
  # Dynamically scale branch thickness based on dataset size (matches ggplot behavior)
  dynamic_patient_line_width <- max(0.5, min(1.3, 2.0 - ((total_patients - 50) * 0.005)))
  dynamic_gene_line_width    <- max(0.5, min(1.3, 2.0 - ((total_genes - 50) * 0.005)))
  
  # ===========================================================================
  # CALL Plotly Heatmap => generate_heatmap_plotly()
  # ===========================================================================
  
  heatmap_plot <- generate_heatmap_plotly(
    data_matrix   = data_matrix,
    gene_order    = gene_order,
    patient_order = patient_order,
    gene_names    = gene_names,
    palette       = palette_name,  
    show_x_axis   = TRUE
  )
  
  # extracting resolved axis info from the build => needed again further
  # down for the hover labels and for aligning the dendrogram axes to
  # the heatmap in the final layout
  heatmap_object <- plotly_build(heatmap_plot)
  heatmap_xaxis  <- heatmap_object$x$layout$xaxis
  heatmap_yaxis  <- heatmap_object$x$layout$yaxis
  
  # building empty plotly object
  final_panel <- plot_ly()
  
  ##### HEATMAP HOVER #############################################
  
  # walk through every trace, skip anything that
  # isn't the heatmap itself, then build the full gene x patient hover text
  # grid in one vectorized pass (not cell by cell) and attach it to that trace => for...
  
  for (data_trace in heatmap_object$x$data) {            # data infos of the heatmap => heatmap_object$x$data
    
    # filter extra traces, that are not needed
    if (!identical(data_trace$type, "heatmap")) next
    
    # prefer the resolved tick labels (real gene/patient names); fall back to
    # the raw trace coordinates if ggplotly didn't attach ticktext for some reason
    gene_labels    <- if (!is.null(heatmap_yaxis$ticktext)) heatmap_yaxis$ticktext else data_trace$y
    patient_labels <- if (!is.null(heatmap_xaxis$ticktext)) heatmap_xaxis$ticktext else data_trace$x
    
    # using outer() for a single vectorized call across every (gene, patient)
    # pair instead of looping row by row 
    # paste0() is vectorized, so outer()
    # can hand it both label vectors at once and build the whole grid in one go
    matrix_hover_text <- outer(
      gene_labels, patient_labels, 
      function(g, p) paste0("<b>Gene:</b> ", g, "<br><b>Patient:</b> ", p)
    )
    
    # paste0() flattens the matrix into a vector => reshape back into
    # gene x patient form right after
    matrix_hover_text <- matrix(
      paste0(matrix_hover_text, "<br><b>Expression:</b> ", round(data_trace$z, 4)),
      nrow = total_genes
    )
    
    # build metadata hover text if there actually is metadata 
    if (length(metadata_columns) > 0) {
      
      # gene/patient identifiers in the exact order the heatmap draws them,
      # needed to build lookup ids that match the heatmap's cell layout
      ordered_gene_ids    <- rownames(data_matrix)[gene_order[1:total_genes]]
      ordered_patient_ids <- colnames(data_matrix)[patient_order]
      
      # building a lookup table: one row per (gene, patient) combination from the
      # unsorted raw data, pairing a combined "gene_patient" key with its
      # already-formatted metadata text
      meta_lookup <- data.frame(
        
        # key composition: combine gene + patient into a single string so a plain
        # string match can stand in for matching on two columns at once
        Key = paste0(heatmap_fields$Gene, "_", heatmap_fields$Patient),
        Strings = Reduce(
          paste0,
          lapply(metadata_columns, function(metadata_column) {
            paste0("<br><b>", metadata_column, ":</b> ", heatmap_fields[[metadata_column]])
          })
        ),
        stringsAsFactors = FALSE
      )
      
      # same key format as above, but built in heatmap cell order this time --
      # this is the "query" side matched against meta_lookup$Key
      grid_keys <- outer(ordered_gene_ids, ordered_patient_ids, function(g, p) paste0(g, "_", p))
      match_indices <- match(grid_keys, meta_lookup$Key)
      
      # blank grid in heatmap shape, filled in only where a match was actually found
      meta_matrix <- matrix("", nrow = total_genes, ncol = total_patients)
      meta_matrix[!is.na(match_indices)] <- meta_lookup$Strings[match_indices[!is.na(match_indices)]]
      
      # append the metadata text onto what's already there (gene/patient/expression)
      matrix_hover_text <- matrix(paste0(matrix_hover_text, meta_matrix), nrow = total_genes)
    }
    
    # adding trace to the plotly object
    final_panel <- add_trace(
      final_panel, x = data_trace$x, y = data_trace$y, z = data_trace$z, type = "heatmap",
      colorscale = data_trace$colorscale, showscale = TRUE, showlegend = FALSE,
      zmin = data_trace$zmin, zmax = data_trace$zmax,
      colorbar = list(title = "Expression", x = 1.01, xanchor = "left", len = 0.35, y = 0.1, yanchor = "bottom"),
      text = matrix_hover_text, hoverinfo = "text", xaxis = "x", yaxis = "y"
    )
  }
  
  #####===========================================================================
  # BUILDING PATIENT DENDROGRAM => Recycling Code from Standalone Dendro
  #####===========================================================================
  
  patient_segments <- patient_dendro_data$draw_result$segments
  patient_labels   <- patient_dendro_data$draw_result$labels
  
  if (!is.null(patient_segments) && nrow(patient_segments) > 0) {
    
    # Clean empty values and generate dynamic colors for group branches
    patient_segments <- na.omit(patient_segments)
    patient_palette  <- get_color(patient_segments$class, palette_name)
    
    for (current_class in unique(patient_segments$class)) {
      class_segments <- patient_segments[patient_segments$class == current_class, ]
      if (nrow(class_segments) == 0) next
      
      class_color <- if (current_class %in% names(patient_palette)) patient_palette[current_class] else "black"
      
      # REUSED STANDALONE LOGIC: Lift pen using NA values for separate branches
      x_coordinates <- as.vector(t(cbind(class_segments$x0, class_segments$x1, NA)))
      y_coordinates <- as.vector(t(cbind(class_segments$y0, class_segments$y1, NA)))
      
      # Clean tooltips by hiding class label for "Default" branches
      branch_hover <- if (current_class == "Default") {
        paste0("Distance: <b>", round(y_coordinates, 3), "</b>")
      } else {
        paste0("Group: <b>", current_class, "</b><br>Distance: <b>", round(y_coordinates, 3), "</b>")
      }
      
      # Draw individual branches on patient axis with dynamic line width
      final_panel <- add_trace(
        final_panel, x = x_coordinates, y = y_coordinates, type = "scatter", mode = "lines", connectgaps = FALSE,
        line = list(color = class_color, width = dynamic_patient_line_width), name = as.character(current_class), 
        legendgroup = as.character(current_class), showlegend = (current_class != "Default"),
        text = branch_hover, hoverinfo = "text", xaxis = "x2", yaxis = "y2"
      )
    }
    
    ##### DENDROGRAM HOVER #############################################
    
    if (!is.null(patient_labels) && nrow(patient_labels) > 0) {
      
      # Resolve display names using custom mapping vector, or fall back 
      # to original matrix column names mapped via the integer label identifier
      resolved_names <- if (!is.null(patient_names)) {
        patient_names[patient_labels$id]
      } else {
        colnames(data_matrix)[patient_labels$id]
      }
      
      # Construct leaf hover text omitting group name for "Default" class
      # to prevent visual clutter in unassigned/uncolored branches
      leaf_hover <- ifelse(
        patient_labels$class == "Default", 
        paste0("Name: <b>", resolved_names, "</b>"), 
        paste0("Name: <b>", resolved_names, "</b><br>Group: <b>", patient_labels$class, "</b>")
      )
      
      # Map patient group classes to their respective hex colors, defaulting
      # to solid black for uncolored branches
      patient_colors <- if (!is.null(patient_palette)) {
        patient_palette[patient_labels$class]
      } else {
        "black"
      }
      patient_colors[is.na(patient_colors)] <- "black"
      
      # REUSED STANDALONE LOGIC: Draw invisible hover target markers (opacity=0)
      # This creates a generous click/hover zone at the dendrogram base 
      # without adding visual clutter to the rendering canvas
      final_panel <- add_trace(
        final_panel, x = patient_labels$x, y = 0, type = "scatter", mode = "markers",
        marker = list(size = 8, color = patient_colors, opacity = 0), showlegend = FALSE, 
        text = leaf_hover, hoverinfo = "text", xaxis = "x2", yaxis = "y2"
      )
    }
  } # <-- Hier wurde die schließende Klammer korrigiert (beendet "if (!is.null(patient_segments)...)")
  
  #####===========================================================================
  # BUILDING GENE DENDROGRAM
  #####===========================================================================
  
  gene_segments <- gene_dendro_data$draw_result$segments
  gene_labels   <- gene_dendro_data$draw_result$labels
  
  if (!is.null(gene_segments) && nrow(gene_segments) > 0) {
    gene_segments <- na.omit(gene_segments)
    
    for (current_class in unique(gene_segments$class)) {
      class_segments <- gene_segments[gene_segments$class == current_class, ]
      if (nrow(class_segments) == 0) next
      
      # REUSED STANDALONE LOGIC: Flip coordinates to draw horizontal branches
      x_coordinates <- as.vector(t(cbind(-class_segments$y0, -class_segments$y1, NA)))
      y_coordinates <- as.vector(t(cbind(class_segments$x0, class_segments$x1, NA)))
      
      # Draw horizontal branches on gene axis with dynamic line width
      final_panel <- add_trace(
        final_panel, x = x_coordinates, y = y_coordinates, type = "scatter", mode = "lines", connectgaps = FALSE,
        line = list(color = "black", width = dynamic_gene_line_width), text = paste0("Distance: <b>", round(abs(x_coordinates), 3), "</b>"), 
        hoverinfo = "text", showlegend = FALSE, xaxis = "x3", yaxis = "y3"
      )
    }
    
    if (!is.null(gene_labels) && nrow(gene_labels) > 0) {
      plotly_y_positions <- gene_labels$x
      resolved_names <- gene_names[gene_labels$id]
      
      # REUSED STANDALONE LOGIC: Draw invisible hover target markers (opacity=0)
      final_panel <- add_trace(
        final_panel, x = 0, y = plotly_y_positions, type = "scatter", mode = "markers",
        marker = list(size = 8, color = "black", opacity = 0), showlegend = FALSE, 
        text = paste0("Name: <b>", resolved_names, "</b>"), hoverinfo = "text", xaxis = "x3", yaxis = "y3"
      )
    }
  }
  
  #####===========================================================================
  # PANEL LAYOUT ALIGNMENT
  #####===========================================================================
  
  # Dynamically calculate label size to keep patient axis readable
  dynamic_xaxis_size <- max(8, min(18, 700 / total_patients))
  
  final_panel <- layout(
    final_panel, dragmode = "zoom", hovermode = "closest",
    
    margin = list(l = 25, r = 80, t = 50, b = 120),
    legend = list(orientation = "v", x = 1.01, xanchor = "left", y = 0.5, yanchor = "middle", title = list(text = "<b>Klassen</b>", font = list(size = 11))),
    
    # Configure shared coordinate domains to visually bind heatmap and dendrogram axes
    xaxis  = list(domain = c(0.25, 0.88), range = patient_range, type = "linear", tickmode = "array", tickvals = heatmap_xaxis$tickvals, ticktext = heatmap_xaxis$ticktext, tickangle = -90, tickfont = list(size = dynamic_xaxis_size), showticklabels = TRUE, showgrid = FALSE, zeroline = FALSE),
    xaxis2 = list(domain = c(0.25, 0.88), range = patient_range, matches = "x", showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),
    xaxis3 = list(domain = c(0, 0.25), range = c(-gene_dendro_data$max_height, 0), showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),
    
    yaxis  = list(domain = c(0, 0.78), range = gene_range, side = "right", type = "linear", tickmode = "array", tickvals = heatmap_yaxis$tickvals, ticktext = heatmap_yaxis$ticktext, tickfont = list(size = 8), showticklabels = TRUE, showgrid = FALSE, zeroline = FALSE, automargin = TRUE),
    yaxis2 = list(domain = c(0.78, 1.00), range = c(0, patient_dendro_data$max_height * 1.05), showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),
    yaxis3 = list(domain = c(0, 0.78), range = gene_range, matches = "y", type = "linear", showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE)
  )
  
  final_panel <- config(final_panel, scrollZoom = TRUE)
  
  return(final_panel)
}