library(plotly)

grafikpanel <- function(
    heatmap_plot,
    patient_dendro,
    gene_dendro,
    gene_order,
    patient_order
){
  

#Plots bauen 
  heatmap_built <- plotly_build(heatmap_plot)
  heatmap_xaxis <- heatmap_built$x$layout$xaxis
  heatmap_yaxis <- heatmap_built$x$layout$yaxis
  
  patient_built <- plotly_build(patient_dendro)
  gene_built    <- plotly_build(gene_dendro)
  
  
  final <- plotly_empty()

#Trace Heatmap
  
  for(trace in heatmap_built$x$data){
    
    final <- add_trace(
      final,
      x = trace$x,
      y = trace$y,
      z = trace$z,
      type = trace$type,
      colorscale = trace$colorscale,
      showscale = trace$showscale,
      text = trace$text,
      hoverinfo = trace$hoverinfo,
      xaxis = "x",
      yaxis = "y"
    )
    
  }

#trace patient dendro 
  
  for(trace in patient_built$x$data){
    
    final <- add_trace(
      final,
      x = trace$x,
      y = trace$y,
      type = trace$type,
      mode = trace$mode,
      line = trace$line,
      text = trace$text,
      hoverinfo = trace$hoverinfo,
      xaxis = "x2",
      yaxis = "y2"
    )
    
  }

#trace gene dendro 

  
  for(trace in gene_built$x$data){
    
    final <- add_trace(
      final,
      x = -trace$y,
      y = trace$x,
      type = trace$type,
      mode = trace$mode,
      line = trace$line,
      xaxis = "x3",
      yaxis = "y3"
    )
    
  }
  
#gemeinsame Koordinaten 
  
  n_genes    <- length(gene_order)
  n_patients <- length(patient_order)
  
  
  gene_range <- c(
    0.5,
    n_genes + 0.5
  )
  
  patient_range <- c(
    0.5,
    n_patients + 0.5
  )

#Zusammensetzung 
  
  final <- layout(
    final,
    dragmode = "zoom",
    #heatmap
    xaxis = modifyList(
      heatmap_xaxis,
      list(
        domain = c(0.20,1),
        range = patient_range,
        fixedrange = FALSE,
        showticklabels = TRUE,
        tickangle = -90
      )
    ),
    
    yaxis = modifyList(
      heatmap_yaxis,
      list(
        domain = c(0,0.80),
        range = gene_range,
        fixedrange = FALSE,
        side = "right",
        showticklabels = TRUE
      )
    ),
    #Patient Dendro
    xaxis2 = list(
      domain = c(0.20,1),
      range = patient_range,
      fixedrange = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE,
      zeroline = FALSE
    ),
    
    yaxis2 = list(
      domain = c(0.80,1),
      fixedrange = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE,
      zeroline = FALSE
    ),
    #Gene Dendro
    xaxis3 = list(
      domain = c(0,0.20),
      showticklabels = FALSE
    ),
    
    yaxis3 = list(
      domain = c(0,0.80),
      showticklabels = TRUE,
      autorange = "reversed"
    ),
#Lücken zwischen Dendros und Heatmap entfernen   
    margin = list(
      l = 0,
      r = 0,
      t = 0,
      b = 0,
      pad = 0
    )
    
  )
  
  
  return(final)
}


