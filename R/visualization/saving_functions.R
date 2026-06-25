### This script contains functions for saving dendrogram as pdf and svg
library(svglite)
library(ggplot2)

export_dendro_pdf <- function(plot, filename = "dendro", filepath = getwd(), landscape = TRUE, show_names = TRUE) {
  if (landscape) {
    width <- 297; height <- 210
  } else {
    width <- 210; height <- 297
  }

  if (!show_names) {
    plot$layers <- Filter(function(l) !inherits(l$geom, "GeomText"), plot$layers)
  }

  full_path <- file.path(filepath, paste0(filename, ".pdf"))

  ggsave(full_path, plot = plot,
         width = width, height = height, units = "mm",
         device = "pdf")
}

generate_dendro_svg <- function(plot, show_names = TRUE) {

  if (!show_names) {
    plot$layers <- Filter(function(l) !inherits(l$geom, "GeomText"), plot$layers)
  }

  svg_string <- svgstring()
  print(plot)
  dev.off()

  return(svg_string())
}