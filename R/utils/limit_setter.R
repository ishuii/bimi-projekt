limit_setter <- function(df_expression, normalization, clip = 3) {
  
  df_expression <- df_expression[is.finite(df_expression)]
  
  if (normalization == 3) {
    return(list(palette = viridis::viridis(100), limits = range(df_expression), midpoint = NULL))
  }
  
  # Diverging scales (all centered methods)
  m <- max(abs(df_expression), na.rm = TRUE)
  
  # Optional clipping
  if (!is.null(clip))
    m <- min(m, clip)
  
  list(palette = colorRampPalette(rev(RColorBrewer::brewer.pal(11, "RdBu")))(100), limits = c(-m, m), midpoint = 0)
}