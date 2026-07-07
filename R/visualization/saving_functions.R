export_dendro_pdf <- function(plot, filename = "dendro", filepath = getwd(), landscape = TRUE) {
  
  ## --- 1. SET STANDARD DINA4 DIMENSIONS ---
  if (landscape) {
    width  <- 297  # mm (A4 Landscape)
    height <- 210  # mm
  } else {
    width  <- 210  # mm (A4 Portrait)
    height <- 297  # mm
  }
  
  ## --- 2. CLEAN UP LINES FOR THE PDF ---
  ## To fix the "lines are too thick" problem without changing your main script,
  ## we look for the segment layer and set a finer linewidth (e.g., 0.35).
  for (i in seq_along(plot$layers)) {
    if (inherits(plot$layers[[i]]$geom, "GeomSegment")) {
      plot$layers[[i]]$aes_params$linewidth <- 0.15
    }
  }
  
  ## --- 3. CONSTRUCT FULL FILE PATH ---
  full_path <- file.path(filepath, paste0(filename, ".pdf"))
  
  ## --- 4. SAVE VIA GGSIZE (GOLD STANDARD) ---
  ## This is the safest way in ggplot2 to generate a single-page vector PDF.
  ggsave(
    filename = full_path,
    plot     = plot,
    width    = width,
    height   = height,
    units    = "mm",
    device   = "pdf"
  )
}