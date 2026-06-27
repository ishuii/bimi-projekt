pdf_check <- function(produced_pdfs) {
  files <- list.files(produced_pdfs, pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE)
  
  if (length(files) == 0) {
    return("")
  }
  
  info <- file.info(files)
  paste(files, info$mtime, info$size, collapse = "|")
}

pdf_value <- function(produced_pdfs) {
  files <- list.files( produced_pdfs, pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE)
  
  if (length(files) == 0) {
    return(NULL)
  }
  
  info <- file.info(files)
  files <- files[!is.na(info$size) & info$size > 0]
  
  if (length(files) == 0) {
    return(NULL)
  }
  
  info <- file.info(files)
  files[order(info$mtime, decreasing = TRUE)][1]
}


pdf_content <- function(file, watched_pdf, daten_aktuell, input, clust_config) {
  
  report_pdf <- tempfile(fileext = ".pdf")
  daten_report <- daten_aktuell()
  
  pdf(report_pdf, width = 8.27, height = 11.69)
  
  on.exit({try(dev.off(), silent = TRUE)}, add = TRUE)
  
  # page 1
  y <- 0.95
  new_page("ClusterIt Analyse-Report")
  
  section_title("Übersicht")
  
  key_value("Dateiname", if (!is.null(input$Datei_csv)) input$Datei_csv$name else "Keine CSV hochgeladen")
  key_value("Erstellt am", format(Sys.time(), "%d.%m.%Y um %H:%M Uhr"))
  key_value("Report-Typ", "Clusteranalyse")
  
  y <- y - 0.02
  section_title("Datensatz")
  
  if (!is.null(daten_report)) {
    first_col_name <- names(daten_report)[1]
    
    metric_row(list(
      list(title = "Zeilen", value = as.character(nrow(daten_report)), subtitle = "aktueller Datensatz", fill = "#FFFFFF"),
      list(title = "Spalten",value = as.character(ncol(daten_report)), subtitle = "aktueller Datensatz", fill = "#FFFFFF"),
      list(title = "Geschützte Spalte", value = first_col_name, subtitle = "erste Spalte", fill = "#FFFFFF")
    ))
  } else {
    wrapped_text("Noch keine CSV-Datei hochgeladen.")
  }
  
  # page 2
  new_page("Analyse-Parameter")
  
  section_title("Gewählte Einstellungen")
  
  key_value("Clusterverfahren", clust_config$method)
  key_value("Distanzmatrix", clust_config$distance)
  key_value("Normalisierung", clust_config$normalisation)
  key_value("Farbpalette", input$farbpaletten)
  
  if (!is.null(clust_config$distance) && clust_config$distance == "Minkowski-Distanz") {
    key_value("Minkowski p", input$param_paramtab)
  }
  
  if (!is.null(clust_config$method) && clust_config$method == "Custom-Linkage") {
    y <- y - 0.02
    section_title("Custom-Linkage Parameter")
    
    key_value("alpha_a", clust_config$alpha_a)
    key_value("alpha_b", clust_config$alpha_b)
    key_value("beta", clust_config$beta)
    key_value("gamma", clust_config$gamma)
  }
  
  if (!is.null(input$pathways) && length(input$pathways) > 0) {
    y <- y - 0.02
    list_items("Ausgewählte Pathways", input$pathways, max_items = 20)
  }
  
  dev.off()
  
  external_pdf <- watched_pdf()
  qpdf::pdf_combine(input = c(report_pdf, external_pdf), output = file)
}


# Design Hilfsfunktionen
safe_len <- function(x) {
  if (is.null(x)) return(0)
  length(x)
}

safe_value <- function(x, fallback = "-") {
  if (is.null(x) || length(x) == 0) return(fallback)
  x <- x[1]
  if (is.na(x)) return(fallback)
  as.character(x)
}

new_page <- function(title = "ClusterIt Report") {
  plot.new()
  par(mar = c(0, 0, 0, 0))
  
  #Backround
  rect(0, 0, 1, 1, col = "#F7F8FA", border = NA)
  
  #Head
  rect(0, 0.90, 1, 1, col = "#2C3E50", border = NA)
  text(0.05, 0.955, title, adj = 0, cex = 1.6, font = 2, col = "white")
  text(
    0.95,
    0.955,
    format(Sys.time(), "%d.%m.%Y %H:%M"),
    adj = 1,
    cex = 0.8,
    col = "white"
  )
  
  #Feetzeile
  rect(0, 0, 1, 0.035, col = "#2C3E50", border = NA)
  text(0.05, 0.018, "ClusterIt", adj = 0, cex = 0.7, col = "white")
  text(0.95, 0.018, "Automatisch generierter Analyse-Report", adj = 1, cex = 0.7, col = "white")
  
  y <<- 0.86
}

section_title <- function(label) {
  if (y < 0.13) new_page()
  
  rect(0.05, y - 0.045, 0.95, y + 0.012, col = "#FBEEB9", border = "#E2D28A")
  text(0.07, y - 0.018, label, adj = 0, cex = 1.05, font = 2, col = "#1F2933")
  
  y <<- y - 0.075
}

key_value <- function(label, value) {
  if (y < 0.08) new_page()
  
  text(0.07, y, paste0(label, ":"), adj = 0, cex = 0.82, font = 2, col = "#34495E")
  text(0.42, y, safe_value(value), adj = 0, cex = 0.82, col = "#111111")
  
  y <<- y - 0.035
}

wrapped_text <- function(text_value, x = 0.07, width = 95, cex = 0.78, col = "#333333") {
  lines <- unlist(strwrap(as.character(text_value), width = width))
  
  for (line in lines) {
    if (y < 0.08) new_page()
    text(x, y, line, adj = 0, cex = cex, col = col)
    y <<- y - 0.032
  }
}

metric_card <- function(x1, x2, title, value, subtitle = "", fill = "#FFFFFF") {
  rect(x1, y - 0.105, x2, y, col = fill, border = "#D5D8DC")
  text(x1 + 0.015, y - 0.028, title, adj = 0, cex = 0.72, font = 2, col = "#34495E")
  text(x1 + 0.015, y - 0.065, safe_value(value), adj = 0, cex = 1.25, font = 2, col = "#111111")
  
  if (!is.null(subtitle) && subtitle != "") {
    text(x1 + 0.015, y - 0.09, subtitle, adj = 0, cex = 0.58, col = "#626567")
  }
}


metric_row <- function(cards) {
  if (y < 0.18) new_page()
  
  gap <- 0.015
  start_x <- 0.05
  total_width <- 0.90
  card_width <- (total_width - gap * (length(cards) - 1)) / length(cards)
  
  for (i in seq_along(cards)) {
    x1 <- start_x + (i - 1) * (card_width + gap)
    x2 <- x1 + card_width
    
    metric_card(
      x1 = x1,
      x2 = x2,
      title = cards[[i]]$title,
      value = cards[[i]]$value,
      subtitle = cards[[i]]$subtitle,
      fill = cards[[i]]$fill
    )
  }
  
  y <<- y - 0.14
}

list_items <- function(title, values, max_items = 12) {
  if (is.null(values) || length(values) == 0) return(NULL)
  
  if (y < 0.12) new_page()
  
  text(0.07, y, title, adj = 0, cex = 0.85, font = 2, col = "#34495E")
  y <<- y - 0.035
  
  show_values <- head(values, max_items)
  
  for (v in show_values) {
    if (y < 0.08) new_page()
    wrapped_text(paste0("• ", v), x = 0.09, width = 85, cex = 0.72)
  }
  
  if (length(values) > max_items) {
    wrapped_text(
      paste0("... weitere ", length(values) - max_items, " Einträge nicht angezeigt."),
      x = 0.09,
      width = 85,
      cex = 0.7,
      col = "#666666"
    )
  }
  
  y <<- y - 0.015
}
