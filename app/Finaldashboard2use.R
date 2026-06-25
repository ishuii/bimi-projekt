
library(shiny)
library(dipsaus)
library(shinydashboard)
library(jsonlite)
library(devtools)
library(distRcpp)
library(shinyFeedback)
library(shinyjs)
library(bslib)
library(bsicons)
library(shinyBS)
library(RSQLite)
library(DBI)
library(qpdf)

source("R/clustering/single_linkage.R")
source("R/clustering/average_linkage.R")
source("R/clustering/complete_linkage.R")
source("R/clustering/normalization_methods.R")
source("data/database_functions_v4.r")
source("R/clustering/hierarchical_clustering.R")
source("R/visualization/final_dendrogram.R")



if(interactive()){
  
  
  ui <- dashboardPage(
    dashboardHeader(title = "GenexCluster"),
    
    dashboardSidebar(
      width = 350,
      sidebarMenu(id = "tabs",
                  menuItem("Startseite", tabName = "Startseite", icon = icon("home")),
                  menuItem("Datei Hochladen", icon = icon("upload"), tabName = "datei_hochladen"),
                  menuItem("Parametern Wählen", icon = icon("sliders"), tabName = "parameter"),
                  menuItem("Heatmap", tabName = "heatmap"),
                  
                  conditionalPanel(
                    condition = 'input.tabs == "heatmap"',
                    
                    div(
                      title = "Cluster Einstellungen",
                      width = 6,
                      solidHeader = TRUE,
                      status = "warning",
                      class = "heatmap-controls",
                      id = "heatmap",
                      
                      selectInput(inputId = "clusterverfahren_sidebar", label = "Clusterverfahren auswählen", 
                                  choices = c("Single-Linkage", "Average-Linkage", "Complete-Linkage", "Custom-Linkage")),
                      
                      
                      selectInput(inputId = "normalisierung_sidebar", label = "Normalisierungs Verfahren auswählen", 
                                  choices = c("normalize_log_zscore", "normalize_log_only", "normalize_log_median_centering", "normalize_log_mad")),
                      
                      
                      selectInput(inputId="distanzmatrix_sidebar", label = "Distanz Matrix auswählen", 
                                  choices = c("Euklidische Distanz", "Manhattan-Distanz", "Minkowski-Distanz", "Canberra-Distanz", "Pearson-Distanz", "Winkeldistanz (Angular Seperation)")),
                      
                      conditionalPanel(condition = "input.distanzmatrix_sidebar == 'Minkowski-Distanz'",
                                       numericInput(inputId = "param_heatmap", label = "Parameter p eingeben", value = 1),
                                       textOutput("result")),
                      conditionalPanel(
                        condition = "input.clusterverfahren_sidebar == 'Custom-Linkage'",
                        numericInput("alpha_a", "Alpha a", value=0.5),
                        numericInput("alpha_b", "Alpha b", value = 0.5),
                        numericInput("beta", "Beta", value=0),
                        numericInput("gamma", "Gamma", value=0)
                      ),
                      
                      radioButtons(inputId = "farbpaletten_sidebar", label = "Farbpalette für Heatmaps auswählen", 
                                   choiceNames = list(
                                     
                                     tagList(
                                       "RdYlBu",
                                       
                                       tags$span(
                                         class = "badge bg-info", # Creates the blue box style from your image
                                         style = "cursor: pointer; padding: 3px 6px; font-weight: bold;",
                                         `data-toggle` = "popover",
                                         `data-html` = "true",    # Allows text inside to wrap cleanly
                                         title = "Standard",      # Bold title of the popover
                                         `data-content` = "Farben: Rot, Gelb, Blau", # Subtext
                                         "?"
                                       )
                                     ), 
                                     
                                     tagList(
                                       "Viridis",
                                       
                                       tags$span(
                                         class = "badge bg-info",
                                         style = "cursor: pointer; padding: 3px 6px; font-weight: bold;",
                                         `data-toggle` = "popover",
                                         `data-html` = "true",
                                         title = "Viridis",
                                         `data-content` = "Farben: Lila, Grün, Gelb",
                                         "?"
                                       )
                                     ), 
                                     
                                     tagList(
                                       "RdBu",
                                       
                                       tags$span(
                                         class = "badge bg-info",
                                         style = "cursor: pointer; padding: 3px 6px; font-weight: bold;",
                                         `data-toggle` = "popover",
                                         `data-html` = "true",
                                         title = "Magma",
                                         `data-content` = "Farben: Rot, Blau",
                                         "?"
                                       )
                                     ),
                                     
                                     tagList(
                                       "PRGn",
                                       
                                       tags$span(
                                         class = "badge bg-info",
                                         style = "cursor: pointer; padding: 3px 6px; font-weight: bold;",
                                         `data-toggle` = "popover",
                                         `data-html` = "true",
                                         title = "Magma",
                                         `data-content` = "Farben: Lila, Grün",
                                         "?"
                                       )
                                     )
                                     
                                   ), 
                                   choiceValues = list("RdYlBu", "Viridis", "RdBu","PRGn")
                      ),
                      
                    ),
                    
                  )
                  
                  
      ),
      br(),
      div(
        style = "padding: 10px;",
        downloadButton("download_pdf", "PDF exportieren")
        
      )),
    
    dashboardBody(
      useShinyFeedback(),
      useShinyjs(),
      
      tags$head(
        
        tags$style(HTML("
             .main-header {position:fixed; width:100%;}
             .content-wrapper{padding-top: 50px !important;}            ")),
        
        
        tags$style(HTML("
      /* Main header */
      .main-header .logo {
        background-color: #ECECEC !important;
        color: #000000 !important;
      }
      
      .main-header .logo:hover {
      background-color: #ECECEC !important;
      color: black !important;
    }

      .main-header .navbar {
        background-color: #ECECEC !important;
      }

      /* Sidebar */
      .main-sidebar {
        background-color: #ECECEC !important;
      }
      
      /* All sidebar text */
    .sidebar-menu > li > a {
      color: black !important;
    }
     
     /* Active menu item */
    .sidebar-menu > li.active > a {
      background-color: #ECECEC !important;
      color: black !important;
    }
      
       /* Treeview arrows/icons */
    .sidebar-menu li a .fa,
    .sidebar-menu li a .glyphicon {
      color: black !important;
    }
    
    .custom-box .box-header{
    background-color: #FBEEB9 !important;
    }
    
    .custom-box .box-title{
    color: black !important;
    }
    
    .cluster-box .box-header{
    background-color:  #FBEEB9 !important;
    }
    
    .cluster-box .box-title{
    color: black !important;
    }
    
    .preset-box .box-header{
    background-color:  #FBEEB9 !important;
    }
    
    .preset-box .box-title{
    color: black !important;
    }
    
    /* Overrides SUCCESS box header */
    .box.box-success > .box-header {
      background-color: #FBEEB9 !important;
      color: black !important;
      border-bottom: none;
    }
    
    /* Overrides PRIMARY box header */
    .box.box-primary > .box-header {
      background-color: #FBEEB9 !important;
      color: black !important;
      border-bottom: none;
    }
    
    #changes text in sidebar to black
    .heatmap-controls label {
    color: black !important;
    }
    
    .heatmap-controls .control-label {
    color: black !important;
    }
    
    .heatmap-controls .radio-label {
    color: black !important;
    }
    
    #heatmap .radio label{
    color: black !important;
    }
    
    .heatmap-controls h1,
    .heatmap-controls h2,
    .heatmap-controls h3,
    .heatmap-controls h4,
    .heatmap-controls h5,
    .heatmap-controls h6 {
    color: black !important;
    }
    
    "))
      ),
      
      tabItems(
        
        tabItem(tabName = "Startseite",
                h2("Wilkommen zum Dashboard für Cluster Analyse"),
                
                actionButton('nextpage', 'Datei Hochladen')
                
        ),
        
        tabItem(tabName = "datei_hochladen",
                h2("CSV Datei hochladen"),
                
                fancyFileInput("Datei_csv", "CSV Datei hochladen", accept = ".csv"),
                
                fluidRow(
                  box(
                    width = 12,
                    h4("NA-Fehlerbehandlung"),
                    verbatimTextOutput("na_info"),
                    
                    uiOutput("na_column_choices"),
                    uiOutput("na_row_choice"),
                    
                    fluidRow(
                      column(
                        width = 6,
                        actionButton(
                          inputId = "auto_na_drop_columns",
                          label = "Für alle: NA-Spalten entfernen",
                          class = "btn-danger",
                          width = "100%"
                        )
                      ),
                      column(
                        width = 6,
                        actionButton(
                          inputId = "auto_na_mean",
                          label = "Für alle: Mittelwert berechnen",
                          class = "btn-primary",
                          width = "100%"
                        )
                      )
                    ),
                    
                    br(),
                    
                    actionButton(
                      inputId = "apply_na_handling",
                      label = "Manuelle NA-Behandlung anwenden",
                      class = "btn-warning"
                    )
                  )
                ),
                
                tableOutput("coverage_table"),
                
                fluidRow(
                  
                  box(
                    title = "Datensatz Parametern einstellen",
                    width = 12,
                    class = "custom-box",
                    
                    selectizeInput(
                      "pathways",
                      "Pathways auswählen",
                      
                      choices = NULL,
                      
                      multiple = TRUE
                    )
                    
                  )
                ),
                
                actionButton('confirm_button', "Weiter mit diesem Pathways"),
                
                actionButton('switchtab', 'Parametern Wählen'),
                
        ),
        
        tabItem(tabName = "parameter",
                h2("Bitte Parametern benötigt zur Cluster Analyse, auswählen"),
                
                fluidRow(
                  
                  box(
                    title = "Cluster Einstellungen",
                    width = 12,
                    solidHeader = TRUE,
                    status = "success",
                    class = "cluster-box",
                    
                    selectInput(inputId = "clusterverfahren", label = "Clusterverfahren auswählen", 
                                choices = c("Single-Linkage", "Average-Linkage", "Complete-Linkage", "Custom-Linkage")),
                    
                    conditionalPanel(
                      condition = "input.clusterverfahren == 'Custom-Linkage'",
                      numericInput("alpha_a", "Alpha a", value=0.5),
                      numericInput("alpha_b", "Alpha b", value = 0.5),
                      numericInput("beta", "Beta", value=0),
                      numericInput("gamma", "Gamma", value=0)
                    ),
                    
                    
                    
                    radioButtons(inputId = "farbpaletten", label = "Farbpalette für Heatmaps auswählen", 
                                 choiceNames = list(
                                   
                                   tagList(
                                     "RdYlBu",
                                     
                                     tags$span(
                                       class = "badge bg-info", # Creates the blue box style from your image
                                       style = "cursor: pointer; padding: 3px 6px; font-weight: bold;",
                                       `data-toggle` = "popover",
                                       `data-html` = "true",    # Allows text inside to wrap cleanly
                                       title = "Standard",      # Bold title of the popover
                                       `data-content` = "Farben: Rot, Gelb, Blau", # Subtext
                                       "?"
                                     )
                                   ), 
                                   
                                   tagList(
                                     "Viridis",
                                     
                                     tags$span(
                                       class = "badge bg-info",
                                       style = "cursor: pointer; padding: 3px 6px; font-weight: bold;",
                                       `data-toggle` = "popover",
                                       `data-html` = "true",
                                       title = "Viridis",
                                       `data-content` = "Farben: Lila, Grün, Gelb",
                                       "?"
                                     )
                                   ), 
                                   
                                   tagList(
                                     "RdBu",
                                     
                                     tags$span(
                                       class = "badge bg-info",
                                       style = "cursor: pointer; padding: 3px 6px; font-weight: bold;",
                                       `data-toggle` = "popover",
                                       `data-html` = "true",
                                       title = "Magma",
                                       `data-content` = "Farben: Rot, Blau",
                                       "?"
                                     )
                                   ),
                                   
                                   tagList(
                                     "PRGn",
                                     
                                     tags$span(
                                       class = "badge bg-info",
                                       style = "cursor: pointer; padding: 3px 6px; font-weight: bold;",
                                       `data-toggle` = "popover",
                                       `data-html` = "true",
                                       title = "Magma",
                                       `data-content` = "Farben: Lila, Grün",
                                       "?"
                                     )
                                   )
                                   
                                 ), 
                                 choiceValues = list("RdYlBu", "Viridis", "RdBu","PRGn")
                    ),
                    
                    
                    selectInput(inputId = "normalisierung", label = "Normalisierungs Verfahren auswählen", 
                                choices = c("normalize_log_zscore", "normalize_log_only", "normalize_log_median_centering", "normalize_log_mad")),
                    
                    
                    selectInput(inputId="distanzmatrix", label = "Distanz Matrix auswählen", 
                                choices = c("Euklidische Distanz", "Manhattan-Distanz", "Minkowski-Distanz", "Canberra-Distanz", "Pearson-Distanz", "Winkeldistanz (Angular Seperation)")),
                    
                    conditionalPanel(condition = "input.distanzmatrix == 'Minkowski-Distanz'",
                                     numericInput(inputId = "param_paramtab", label = "Parameter p eingeben", value = 1),
                                     textOutput("result")),
                    
                  ),
                  
                ),
                
                fluidRow(
                  box(
                    title = "Preset speichern/laden",
                    width = 12,
                    solidHeader = TRUE,
                    status = "primary",
                    class = "preset-box",
                    
                    textInput("preset_name", "Name des Presets"),
                    actionButton("save_preset", "Preset speichern"),
                    br(), br(),
                    selectInput("preset_datei", "Preset auswählen", choices = NULL),
                    actionButton("load_preset", "Preset laden")
                  )
                ),
                
                disabled(actionButton("run", "Run Cluster Analyse", class = "btn-successful")),
                
        ),
        
        
        tabItem(tabName = "heatmap",
                h2("Heatmap"),
                plotOutput("HeatmapPlot"),
                verbatimTextOutput("debug_matrix"),
                
                
                tags$script(HTML('
          $(document).ready(function(){
            $("body").popover({ 
              selector: "[data-toggle=popover]",
              trigger: "hover click", // Opens on hover OR click
              container: "body"       // Fixes layout breaking issues
            });
          });
        ')),
                
                
                
                textOutput("selection_feedback"),
                
                
                actionButton('back', 'zurück zum Parametern wählen')
        )
        
      )       
      
    )
  )
  
  
  
  server <- function(input, output, session) {
    
    
    con <- dbConnect(RSQLite::SQLite(), "GeneDatabase.sqlite")
    
    dbExecute(con, "
  CREATE TABLE if not exists Gene (
    Entrez_ID INTEGER PRIMARY KEY,
    Genname VARCHAR(255),
    Symbol VARCHAR(100)
  )
")
    
    
    
    
    cluster_result <- reactiveVal(NULL)
    
    d_mat_result <- reactiveVal(NULL)
    
    pathway_list <- reactiveVal()
    
    coverage_result <- reactiveVal(NULL)
    
    dendrogram_result <- reactiveVal(NULL)
    
    output$Beispieltext <- renderText({
      paste("Deine Datei:", input$x)
    })
    preset_values <- reactiveVal(list()) #Create reactive variable list
    options(shiny.maxRequestSize = 10 * 1024^3)
    # CSV IMPORT BACKEND ---------------------------------------------------------
    daten_original <- reactiveVal(NULL)
    daten_aktuell  <- reactiveVal(NULL)
    na_infos       <- reactiveVal(NULL)
    
    # Speichert die dynamischen Input-IDs für die manuelle Spaltenauswahl
    na_column_map <- reactiveVal(NULL)
    
    
    # Hilfsfunktion: NA-Status analysieren --------------------------------------
    analyze_na_status <- function(df,
                                  bereits_bereinigt = FALSE,
                                  entfernte_all_na_spalten = character(0),
                                  entfernte_user_spalten = character(0),
                                  imputierte_spalten = character(0),
                                  entfernte_zeilen = integer(0)) {
      
      req(ncol(df) >= 1)
      
      # Erste Spalte wird immer geschützt und nicht für NA-Entscheidungen genutzt
      id_col <- names(df)[1]
      data_cols <- names(df)[-1]
      
      if (length(data_cols) == 0) {
        return(list(
          id_col = id_col,
          na_gesamt = 0,
          zeilen_mit_na = 0,
          zeilen_gesamt = nrow(df),
          spalten_gesamt = ncol(df),
          na_pro_spalte = setNames(integer(0), character(0)),
          spalten_mit_na = character(0),
          spalten_mit_na_counts = integer(0),
          rows_over_50_na = integer(0),
          rows_over_50_na_names = character(0),
          meta_rows = integer(0),
          bereits_bereinigt = bereits_bereinigt,
          entfernte_all_na_spalten = entfernte_all_na_spalten,
          entfernte_user_spalten = entfernte_user_spalten,
          imputierte_spalten = imputierte_spalten,
          entfernte_zeilen = entfernte_zeilen
        ))
      }
      
      df_data <- df[, data_cols, drop = FALSE]
      
      na_pro_spalte <- colSums(is.na(df_data))
      spalten_mit_na <- names(na_pro_spalte)[na_pro_spalte > 0]
      
      row_na_ratio <- rowMeans(is.na(df_data))
      
      first_col_values <- as.character(df[[id_col]])
      first_col_values[is.na(first_col_values)] <- ""
      
      # meta_-Zeilen schützen
      meta_rows <- which(grepl("^meta_", first_col_values))
      
      # Zeilen mit mehr als 50% NA suchen
      rows_over_50_na <- which(row_na_ratio > 0.5)
      
      # meta_-Zeilen werden nicht zum Entfernen vorgeschlagen
      rows_over_50_na_without_meta <- setdiff(rows_over_50_na, meta_rows)
      
      list(
        id_col = id_col,
        na_gesamt = sum(is.na(df_data)),
        zeilen_mit_na = sum(rowSums(is.na(df_data)) > 0),
        zeilen_gesamt = nrow(df),
        spalten_gesamt = ncol(df),
        na_pro_spalte = na_pro_spalte,
        spalten_mit_na = spalten_mit_na,
        spalten_mit_na_counts = na_pro_spalte[spalten_mit_na],
        rows_over_50_na = rows_over_50_na_without_meta,
        rows_over_50_na_names = first_col_values[rows_over_50_na_without_meta],
        meta_rows = meta_rows,
        bereits_bereinigt = bereits_bereinigt,
        entfernte_all_na_spalten = entfernte_all_na_spalten,
        entfernte_user_spalten = entfernte_user_spalten,
        imputierte_spalten = imputierte_spalten,
        entfernte_zeilen = entfernte_zeilen
      )
    }
    replace_na_with_row_mean <- function(df, target_cols = NULL) {
      
      req(ncol(df) >= 1)
      
      
      data_cols <- names(df)[-1] #Save first coloium
      
      if (length(data_cols) == 0) {
        return(list(
          df = df,
          imputierte_spalten = character(0)
        ))
      }
      
      
      numeric_cols <- data_cols[sapply(df[, data_cols, drop = FALSE], is.numeric)] #only numeric values for mean
      
      if (length(numeric_cols) == 0) {
        return(list(
          df = df,
          imputierte_spalten = character(0)
        ))
      }
      
      # Falls keine Zielspalten übergeben wurden: alle numerischen Spalten behandeln
      if (is.null(target_cols)) {
        target_cols <- numeric_cols
      }
      
      # Zielspalten auf vorhandene numerische Spalten begrenzen
      target_cols <- intersect(target_cols, numeric_cols)
      
      if (length(target_cols) == 0) {
        return(list(
          df = df,
          imputierte_spalten = character(0)
        ))
      }
      
      # Matrix für schnelle Zeilenmittelwerte
      numeric_matrix <- as.matrix(df[, numeric_cols, drop = FALSE])
      
      # Zeilenmittelwert je Zeile berechnen
      row_means <- rowMeans(numeric_matrix, na.rm = TRUE)
      
      # Falls eine Zeile nur NA-Werte hat, entsteht NaN
      row_means[is.nan(row_means)] <- NA
      
      imputierte_spalten <- character(0)
      
      for (col in target_cols) {
        
        na_idx <- which(is.na(df[[col]]) & !is.na(row_means))
        
        if (length(na_idx) > 0) {
          df[[col]][na_idx] <- row_means[na_idx]
          imputierte_spalten <- c(imputierte_spalten, col)
        }
      }
      
      list(
        df = df,
        imputierte_spalten = unique(imputierte_spalten)
      )
    }
    
    # Hilfsfunktion: dynamische Spaltenauswahl aktualisieren --------------------
    refresh_na_column_map <- function(info) {
      cols_with_na <- info$spalten_mit_na
      
      if (length(cols_with_na) > 0) {
        na_column_map(
          data.frame(
            input_id = paste0("na_col_action_", seq_along(cols_with_na)),
            colname = cols_with_na,
            stringsAsFactors = FALSE
          )
        )
      } else {
        na_column_map(NULL)
      }
    }
    
    
    # CSV hochladen -------------------------------------------------------------
    observeEvent(input$Datei_csv, {
      
      req(input$Datei_csv)
      
      df <- read.csv(
        input$Datei_csv$datapath,
        header = TRUE,
        stringsAsFactors = FALSE,
        na.strings = c("", " ", "NA", "NaN", "NULL", "N/A", "n/a")
      )
      
      df[df == ""] <- NA
      
      req(ncol(df) >= 1)
      
      # Erste Spalte wird immer geschützt
      data_cols <- names(df)[-1]
      entfernte_all_na_spalten <- character(0)
      
      if (length(data_cols) > 0) {
        df_data <- df[, data_cols, drop = FALSE]
        
        # Spalten, die komplett aus NA bestehen, automatisch entfernen
        all_na_cols <- names(df_data)[colSums(!is.na(df_data)) == 0]
        
        if (length(all_na_cols) > 0) {
          df <- df[, !names(df) %in% all_na_cols, drop = FALSE]
          entfernte_all_na_spalten <- all_na_cols
        }
      }
      
      daten_original(df)
      daten_aktuell(df)
      
      info <- analyze_na_status(
        df,
        bereits_bereinigt = FALSE,
        entfernte_all_na_spalten = entfernte_all_na_spalten
      )
      
      na_infos(info)
      refresh_na_column_map(info)
    })
    
    
    # NA-Info ausgeben ----------------------------------------------------------
    output$na_info <- renderPrint({
      
      info <- na_infos()
      
      if (is.null(info)) {
        cat("Noch keine CSV-Datei hochgeladen.")
        return(invisible(NULL))
      }
      
      cat("Erste Spalte wird immer ausgelassen/geschützt:", info$id_col, "\n\n")
      cat("Anzahl aller NA-Werte ohne erste Spalte:", info$na_gesamt, "\n")
      cat("Zeilen mit mindestens einem NA-Wert:", info$zeilen_mit_na, "\n")
      cat("Spalten mit mindestens einem NA-Wert:", length(info$spalten_mit_na), "\n")
      cat("Zeilen gesamt:", info$zeilen_gesamt, "\n")
      cat("Spalten gesamt:", info$spalten_gesamt, "\n\n")
      
      if (length(info$entfernte_all_na_spalten) > 0) {
        cat("Automatisch entfernte Spalten, die komplett aus NA bestanden:\n")
        print(info$entfernte_all_na_spalten)
        cat("\n")
      }
      
      if (length(info$spalten_mit_na_counts) > 0) {
        cat("Spalten mit NA-Werten:\n")
        print(info$spalten_mit_na_counts)
        cat("\n")
      } else {
        cat("Keine Spalten mit NA-Werten vorhanden.\n\n")
      }
      
      if (length(info$rows_over_50_na) > 0) {
        cat("Zeilen mit mehr als 50% NA-Werten, ohne geschützte meta_-Zeilen:\n")
        print(data.frame(
          zeilennummer = info$rows_over_50_na,
          name = info$rows_over_50_na_names
        ))
        cat("\n")
      } else {
        cat("Keine Zeilen mit mehr als 50% NA-Werten gefunden.\n\n")
      }
      
      if (length(info$meta_rows) > 0) {
        cat("Geschützte meta_-Zeilen:", length(info$meta_rows), "\n")
        cat("Diese werden nicht automatisch entfernt.\n\n")
      }
      
      if (isTRUE(info$bereits_bereinigt)) {
        cat("Status: NA-Behandlung wurde angewendet.\n\n")
        
        if (length(info$imputierte_spalten) > 0) {
          cat("Spalten, bei denen NA durch Mittelwert ersetzt wurde:\n")
          print(info$imputierte_spalten)
          cat("\n")
        }
        
        if (length(info$entfernte_user_spalten) > 0) {
          cat("Vom User entfernte Spalten:\n")
          print(info$entfernte_user_spalten)
          cat("\n")
        }
        
        if (length(info$entfernte_zeilen) > 0) {
          cat("Entfernte Zeilen:\n")
          print(info$entfernte_zeilen)
          cat("\n")
        }
      } else {
        cat("Status: Datei wurde geprüft. Noch keine User-Entscheidung angewendet.\n")
      }
      
      invisible(NULL)
    })
    
    
    # UI für manuelle Spalten mit NA-Werten ------------------------------------
    output$na_column_choices <- renderUI({
      
      info <- na_infos()
      col_map <- na_column_map()
      
      req(info)
      
      if (is.null(col_map) || nrow(col_map) == 0) {
        return(tags$p("Keine Spalten mit NA-Werten vorhanden."))
      }
      
      tagList(
        tags$hr(),
        tags$h4("Entscheidung für Spalten mit NA-Werten"),
        
        tags$div(
          style = "
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(320px, 1fr));
            gap: 15px;
            align-items: start;
          ",
          
          lapply(seq_len(nrow(col_map)), function(i) {
            
            col <- col_map$colname[i]
            input_id <- col_map$input_id[i]
            na_count <- info$spalten_mit_na_counts[[col]]
            
            is_num <- is.numeric(daten_aktuell()[[col]])
            
            choices <- if (is_num) {
              c(
                "NA durch Zeilen-Mittelwert ersetzen" = "mean",
                "Ganze Spalte entfernen" = "drop",
                "Spalte unverändert behalten" = "keep"
              )
            } else {
              c(
                "Ganze Spalte entfernen" = "drop",
                "Spalte unverändert behalten" = "keep"
              )
            }
            
            selected_choice <- if (is_num) "mean" else "keep"
            
            tags$div(
              style = "
                border: 1px solid #ddd;
                border-radius: 8px;
                padding: 12px;
                background-color: #fafafa;
                word-break: break-word;
              ",
              
              radioButtons(
                inputId = input_id,
                label = paste0(col, " — ", na_count, " NA-Wert(e)"),
                choices = choices,
                selected = selected_choice
              )
            )
          })
        )
      )
    })
    
    
    # UI für manuelle Zeilenentscheidung ---------------------------------------
    output$na_row_choice <- renderUI({
      
      info <- na_infos()
      req(info)
      
      if (length(info$rows_over_50_na) == 0) {
        return(NULL)
      }
      
      tagList(
        tags$hr(),
        tags$h4("Entscheidung für Zeilen mit mehr als 50% NA-Werten"),
        
        radioButtons(
          inputId = "row_na_action",
          label = paste0(
            length(info$rows_over_50_na),
            " Zeile(n) haben mehr als 50% NA-Werte. Was soll passieren?"
          ),
          choices = c(
            "Zeilen behalten" = "keep",
            "Zeilen entfernen, aber meta_-Zeilen behalten" = "drop"
          ),
          selected = "keep"
        )
      )
    })
    
    
    # Manuelle NA-Behandlung anwenden ------------------------------------------
    observeEvent(input$apply_na_handling, {
      
      req(daten_aktuell())
      
      df <- daten_aktuell()
      col_map <- na_column_map()
      
      entfernte_user_spalten <- character(0)
      imputierte_spalten <- character(0)
      entfernte_zeilen <- integer(0)
      
      # Spaltenentscheidungen anwenden
      if (!is.null(col_map) && nrow(col_map) > 0) {
        
        for (i in seq_len(nrow(col_map))) {
          
          col <- col_map$colname[i]
          input_id <- col_map$input_id[i]
          action <- input[[input_id]]
          
          if (is.null(action)) next
          if (!col %in% names(df)) next
          
          if (action == "drop") {   # User Input Delete whole Patient
            df[[col]] <- NULL
            entfernte_user_spalten <- c(entfernte_user_spalten, col)
          } else if (action == "mean") {
            
            if (is.numeric(df[[col]])) {
              
              row_mean_result <- replace_na_with_row_mean(
                df = df,
                target_cols = col
              )
              
              df <- row_mean_result$df
              
              imputierte_spalten <- c(
                imputierte_spalten,
                row_mean_result$imputierte_spalten
              )
            }
          }
          }
        }
      
      
      # Zeilenentscheidung anwenden
      req(ncol(df) >= 1)  # check if data greater 1
      
      id_col <- names(df)[1]
      data_cols <- names(df)[-1]
      
      if (length(data_cols) > 0) {
        df_data <- df[, data_cols, drop = FALSE]
        row_na_ratio <- rowMeans(is.na(df_data))
        
        first_col_values <- as.character(df[[id_col]]) # change first column to text 
        first_col_values[is.na(first_col_values)] <- "" # if name = na change / bugfix
        
        meta_rows <- which(grepl("^meta_", first_col_values)) # Search for _meta in names
        rows_over_50_na <- which(row_na_ratio > 0.5)
        
        
        removable_rows <- setdiff(rows_over_50_na, meta_rows) # saves not removles rows
        
        if (!is.null(input$row_na_action) && input$row_na_action == "drop") { # check user choice
          if (length(removable_rows) > 0) {
            df <- df[-removable_rows, , drop = FALSE] # removes meta and first row from dataframe
            entfernte_zeilen <- removable_rows
          }
        }
      }
      
      daten_aktuell(df)
      
      info <- analyze_na_status(
        df,
        bereits_bereinigt = TRUE,
        entfernte_user_spalten = entfernte_user_spalten,
        imputierte_spalten = imputierte_spalten,
        entfernte_zeilen = entfernte_zeilen
      )
      
      na_infos(info)
      refresh_na_column_map(info)
    })
    
    
    
    observeEvent(input$auto_na_drop_columns, {   # Quick function for user to drop for all
      
      req(daten_aktuell())
      
      df <- daten_aktuell()
      req(ncol(df) >= 1)
      
      data_cols <- names(df)[-1] # Igonore first Coloum
      
      entfernte_user_spalten <- character(0)
      imputierte_spalten <- character(0)
      entfernte_zeilen <- integer(0)
      
      if (length(data_cols) > 0) {
        df_data <- df[, data_cols, drop = FALSE]
        
        # Alle Spalten mit mindestens einem NA entfernen
        na_cols <- names(df_data)[colSums(is.na(df_data)) > 0]
        
        if (length(na_cols) > 0) {
          df <- df[, !names(df) %in% na_cols, drop = FALSE]
          entfernte_user_spalten <- na_cols
        }
      }
      
      daten_aktuell(df)
      
      info <- analyze_na_status(
        df,
        bereits_bereinigt = TRUE,
        entfernte_user_spalten = entfernte_user_spalten,
        imputierte_spalten = imputierte_spalten,
        entfernte_zeilen = entfernte_zeilen
      )
      
      na_infos(info)
      refresh_na_column_map(info)
      
      showNotification(
        paste0(length(entfernte_user_spalten), " Spalte(n) mit NA-Werten entfernt."),
        type = "message"
      )
    })
    
    
    # Schnellfunktion 2: Für alle Mittelwert berechnen --------------------------
    observeEvent(input$auto_na_mean, {
      
      req(daten_aktuell())
      
      df <- daten_aktuell()
      
      req(ncol(df) >= 1)
      
      row_mean_result <- replace_na_with_row_mean(df)
      
      df <- row_mean_result$df
      imputierte_spalten <- row_mean_result$imputierte_spalten
      
      entfernte_user_spalten <- character(0)
      entfernte_zeilen <- integer(0)
      
      daten_aktuell(df)
      
      info <- analyze_na_status(
        df,
        bereits_bereinigt = TRUE,
        entfernte_user_spalten = entfernte_user_spalten,
        imputierte_spalten = imputierte_spalten,
        entfernte_zeilen = entfernte_zeilen
      )
      
      na_infos(info)
      
      cols_with_na <- info$spalten_mit_na
      
      if (length(cols_with_na) > 0) {
        na_column_map(
          data.frame(
            input_id = paste0("na_col_action_", seq_along(cols_with_na)),
            colname = cols_with_na,
            stringsAsFactors = FALSE
          )
        )
      } else {
        na_column_map(NULL)
      }
      
      showNotification(
        paste0(length(imputierte_spalten), " Spalte(n) mit Zeilen-Mittelwert behandelt."),
        type = "message"
      )
    })
    
    
    daten <- reactive({
      req(daten_aktuell())
      daten_aktuell()
    })
    
    pdf_watch_folder <- "C:/Users/andre/Documents/bimi projekt/bimi-projekt" # ANGEPASST AUF PDF VON DENDRO UND SO
    
    if (!dir.exists(pdf_watch_folder)) {
      dir.create(pdf_watch_folder, recursive = TRUE)
    }
    
    watched_pdf <- reactivePoll(
      intervalMillis = 2000,
      session = session,
      
      checkFunc = function() {
        files <- list.files(
          pdf_watch_folder,
          pattern = "\\.pdf$",
          full.names = TRUE,
          ignore.case = TRUE
        )
        
        if (length(files) == 0) {
          return("")
        }
        
        info <- file.info(files)
        
        paste(
          files,
          info$mtime,
          info$size,
          collapse = "|"
        )
      },
      
      valueFunc = function() {
        files <- list.files(
          pdf_watch_folder,
          pattern = "\\.pdf$",
          full.names = TRUE,
          ignore.case = TRUE
        )
        
        if (length(files) == 0) {
          return(NULL)
        }
        
        info <- file.info(files)
        
        # Nur fertige PDFs mit Dateigröße > 0 verwenden
        files <- files[!is.na(info$size) & info$size > 0]
        
        if (length(files) == 0) {
          return(NULL)
        }
        
        info <- file.info(files)
        
        # Neueste PDF nehmen
        files[order(info$mtime, decreasing = TRUE)][1]
      }
    )
    
    

    output$download_pdf <- downloadHandler(
      
      filename = function() {
        paste0("Freitags_Report_", Sys.Date(), ".pdf")
      },
      
      contentType = "application/pdf",
      
      content = function(file) {
        
        report_pdf <- tempfile(fileext = ".pdf")
        
        info <- na_infos()
        daten_report <- daten_aktuell()
        
        pdf(report_pdf, width = 8.27, height = 11.69)
        
        on.exit({
          try(dev.off(), silent = TRUE)
        }, add = TRUE)
        
        
        # Design-Hilfsfunktionen
        
        y <- 0.95
        
        safe_len <- function(x) {
          if (is.null(x)) return(0)
          length(x)
        }
        
        safe_value <- function(x, fallback = "-") {
          if (is.null(x) || length(x) == 0 || is.na(x)) return(fallback)
          as.character(x)
        }
        
        new_page <- function(title = "PDF DOKUMENT TEST") {
          plot.new()
          par(mar = c(0, 0, 0, 0))
          
          # Hintergrund
          rect(0, 0, 1, 1, col = "#F7F8FA", border = NA)
          
          # Kopfzeile
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
          
          # Footer
          rect(0, 0, 1, 0.035, col = "#2C3E50", border = NA)
          text(0.05, 0.018, "ProjektCluster", adj = 0, cex = 0.7, col = "white")
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
          text(x1 + 0.015, y - 0.065, value, adj = 0, cex = 1.25, font = 2, col = "#111111")
          
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
        
        # -------------------------------------------------------------------------
        # Seite 1: Titel und Übersicht
        # -------------------------------------------------------------------------
        
        new_page("Projekt Analyse-Report")
        
        section_title("Übersicht")
        
        key_value("Dateiname", if (!is.null(input$Datei_csv)) input$Datei_csv$name else "Keine CSV hochgeladen")
        key_value("Erstellt am", format(Sys.time(), "%d.%m.%Y um %H:%M Uhr"))
        key_value("Report-Typ", "Clusteranalyse mit NA-Fehlerbehandlung")
        
        y <- y - 0.02
        
        section_title("Datensatz")
        
        if (!is.null(daten_report)) {
          
          metric_row(list(
            list(
              title = "Zeilen",
              value = as.character(nrow(daten_report)),
              subtitle = "aktueller Datensatz",
              fill = "#FFFFFF"
            ),
            list(
              title = "Spalten",
              value = as.character(ncol(daten_report)),
              subtitle = "aktueller Datensatz",
              fill = "#FFFFFF"
            ),
            
          ))
          
        } else {
          
          wrapped_text("Noch keine CSV-Datei hochgeladen.")
        } 
        
        
        # Seite 2: Parameter
        
        new_page("Analyse-Parameter")
        
        section_title("Gewählte Einstellungen")
        
        key_value("Clusterverfahren", input$clusterverfahren)
        key_value("Distanzmatrix", input$distanzmatrix)
        key_value("Normalisierung", input$normalisierung)
        key_value("Farbpalette", input$farbpaletten)
        
        if (!is.null(input$distanzmatrix) &&
            input$distanzmatrix == "Minkowski-Distanz") {
          key_value("Minkowski p", input$param_paramtab)
        }
        
        if (!is.null(input$clusterverfahren) &&
            input$clusterverfahren == "Custom-Linkage") {
          
          y <- y - 0.02
          section_title("Custom-Linkage Parameter")
          
          key_value("alpha_a", input$alpha_a)
          key_value("alpha_b", input$alpha_b)
          key_value("beta", input$beta)
          key_value("gamma", input$gamma)
        }
        
        if (!is.null(input$pathways) && length(input$pathways) > 0) {
          y <- y - 0.02
          list_items("Ausgewählte Pathways", input$pathways, max_items = 20)
        }
        
        dev.off()
        # PDF merge function
        
        external_pdf <- watched_pdf()
        
        if (!is.null(external_pdf) && file.exists(external_pdf)) {
          
          tryCatch(
            {
              qpdf::pdf_combine(
                input = c(report_pdf, external_pdf),
                output = file
              )
            },
            error = function(e) {
              file.copy(report_pdf, file, overwrite = TRUE)
              warning("PDF konnte nicht zusammengefügt werden: ", e$message)
            }
          )
          
        } else {
          
          file.copy(report_pdf, file, overwrite = TRUE)
        }
      }
    )
    
    refresh_presets <- function() {
      
      if (!dir.exists("presets")) {
        dir.create("presets")
      }
      
      dateien <- list.files(
        path = "presets",
        pattern = "\\.json$",
        full.names = TRUE
      )
      
      choices <- c(
        "Bitte Preset auswählen" = "",
        setNames(dateien, basename(dateien))
      )
      
      updateSelectInput(
        session,
        "preset_datei",
        choices = choices,
        selected = ""
      )
    }
    
    # Beim Start Preset-Liste laden
    refresh_presets()
    
    
    observeEvent(input$save_preset, {
      
      req(input$preset_name)
      
      if (!dir.exists("presets")) {
        dir.create("presets")
      }
      
      preset <- list(
        clusterverfahren = input$clusterverfahren,
        normalisierung = input$normalisierung,
        distanzmatrix = input$distanzmatrix,
        farbpaletten = input$farbpaletten,
        
        # Minkowski-Parameter
        param_paramtab = input$param_paramtab,
        
        # Custom-Linkage-Parameter
        alpha_a = input$alpha_a,
        alpha_b = input$alpha_b,
        beta = input$beta,
        gamma = input$gamma,
        
        # Pathways
        pathways = input$pathways,
        
        # optional, falls du später wieder anzahlcluster einbaust
        anzahlcluster = if (!is.null(input$anzahlcluster)) input$anzahlcluster else NULL,
        
        gespeichert_am = as.character(Sys.time())
      )
      
      # Dateiname sicher machen
      safe_name <- gsub("[^A-Za-z0-9_\\-]", "_", input$preset_name)
      
      pfad <- file.path("presets", paste0(safe_name, ".json"))
      
      jsonlite::write_json(
        preset,
        path = pfad,
        auto_unbox = TRUE,
        pretty = TRUE
      )
      
      showNotification(
        paste("Preset gespeichert unter:", pfad),
        type = "message"
      )
      
      refresh_presets()
    })
    
    
    observeEvent(input$load_preset, {
      
      req(input$preset_datei)
      req(nzchar(input$preset_datei))
      
      preset <- jsonlite::fromJSON(
        input$preset_datei,
        simplifyVector = FALSE
      )
      
      if (!is.null(preset$clusterverfahren)) {
        updateSelectInput(
          session,
          "clusterverfahren",
          selected = preset$clusterverfahren
        )
      }
      
      if (!is.null(preset$normalisierung)) {
        updateSelectInput(
          session,
          "normalisierung",
          selected = preset$normalisierung
        )
      }
      
      if (!is.null(preset$distanzmatrix)) {
        updateSelectInput(
          session,
          "distanzmatrix",
          selected = preset$distanzmatrix
        )
      }
      
      if (!is.null(preset$farbpaletten)) {
        updateRadioButtons(
          session,
          "farbpaletten",
          selected = preset$farbpaletten
        )
      }
      
      if (!is.null(preset$param_paramtab)) {
        updateNumericInput(
          session,
          "param_paramtab",
          value = preset$param_paramtab
        )
      }
      
      if (!is.null(preset$alpha_a)) {
        updateNumericInput(
          session,
          "alpha_a",
          value = preset$alpha_a
        )
      }
      
      if (!is.null(preset$alpha_b)) {
        updateNumericInput(
          session,
          "alpha_b",
          value = preset$alpha_b
        )
      }
      
      if (!is.null(preset$beta)) {
        updateNumericInput(
          session,
          "beta",
          value = preset$beta
        )
      }
      
      if (!is.null(preset$gamma)) {
        updateNumericInput(
          session,
          "gamma",
          value = preset$gamma
        )
      }
      
      if (!is.null(preset$pathways)) {
        
        selected_pathways <- unlist(preset$pathways)
        
        if (!is.null(pathway_list())) {
          updateSelectizeInput(
            session,
            "pathways",
            choices = pathway_list(),
            selected = selected_pathways,
            server = TRUE
          )
        } else {
          updateSelectizeInput(
            session,
            "pathways",
            selected = selected_pathways,
            server = TRUE
          )
        }
      }
      
      if (!is.null(preset$anzahlcluster) && "anzahlcluster" %in% names(input)) {
        updateNumericInput(
          session,
          "anzahlcluster",
          value = preset$anzahlcluster
        )
      }
      
      showNotification(
        paste("Preset geladen:", basename(input$preset_datei)),
        type = "message"
      )
    })
    
    
    observeEvent(input$nextpage, {
      updateTabItems(session, "tabs", selected = "datei_hochladen")
    })
    
    
    observeEvent(input$switchtab, {
      updateTabItems(session, "tabs", selected = "parameter")
    })
    
    run_analysis <- function(){    
      
      req(daten())
      req(input$distanzmatrix)
      req(input$clusterverfahren)
      
      #calls the updated data
      data <- daten()
      
      #erste Spalte immer aus der Clusteranalyse ausschließen
      data <- data[, -1, drop = FALSE]
      
      #keep numeric only
      data <- data[sapply(data, is.numeric)]
      
      #prevents empty numeric matrix crash
      req(ncol(data) > 0)
      
      #placeholder normalization
      df_normalized <- data
      
      #transpose
      data_t <- t(df_normalized)
      
      #user's distance matrix choice
      method <- switch (input$distanzmatrix,
                        "Euklidische Distanz" = "euclidean",
                        "Manhattan-Distanz" = "manhattan",
                        "Minkowski-Distanz" = "minkowski",
                        "Canberra-Distanz" = "canberra",
                        "Pearson-Distanz" = "pearson",
                        "Winkeldistanz (Angular Seperation)" = "angular"
      )
      
      req(method != "")
      
      #calling distanz matrix function
      d_mat <- dist_cpp(data_t, method = method)
        
      d_mat_result(d_mat)
      
      #user's cluster choices selected
      if(input$clusterverfahren == "Single-Linkage"){
        result <- single_linkage(d_mat)
      }
      
      if(input$clusterverfahren == "Average-Linkage"){
        result <- average_linkage(d_mat)
      }
      
      if(input$clusterverfahren == "Complete-Linkage"){
        result <- complete_linkage(d_mat)
      }
      
      #store the results
      cluster_result(result)
      
      
      if(input$clusterverfahren == "Custom-Linkage"){
        custom_params <- list(
          alpha_a = input$alpha_a,
          alpha_b = input$alpha_b,
          beta = input$beta,
          gamma = input$gamma
        )
        
        result_custom <- hierarchical_clustering(
          d_mat,
          method = "custom",
          custom_params = custom_params
        )
      }else{
        method_name <- switch (input$clusterverfahren,
                               "Single-Linkage" = "single",
                               "Average-Linkage" = "average",
                               "Complete-Linkage" = "complete"
        )
        
        result_method <- hierarchical_clustering(
          d_mat,
          method = method_name
        )
      }
      
      if(input$clusterverfahren_sidebar == "Custom-Linkage"){
        custom_params <- list(
          alpha_a = input$alpha_a,
          alpha_b = input$alpha_b,
          beta = input$beta,
          gamma = input$gamma
        )
        
        result_custom <- hierarchical_clustering(
          d_mat,
          method = "custom",
          custom_params = custom_params
        )
      }else{
        method_name <- switch (input$clusterverfahren_sidebar,
                               "Single-Linkage" = "single",
                               "Average-Linkage" = "average",
                               "Complete-Linkage" = "complete"
        )
        
        result_method <- hierarchical_clustering(
          d_mat,
          method = method_name
        )
      }
      
      dendro <- generate_dendro(result)
      
      
      updateTabItems(session, "tabs", selected = "heatmap")
     
      print(dim(data))
      print(method) 
    }
    
    observeEvent(input$run, {
      
      req(inputs_valid())
      
      if(input$distanzmatrix == "Minkowski-Distanz" &&
         input$param_paramtab == 1){
        
        showModal(
          modalDialog(
            title = "Warnung",
            "hier wird mit Manhattan-Distanz statt Minkowski-Distanz berechnet. Möchten Sie fortfahren?",
            
            footer = tagList(
              modalButton("Abbrechen"),
              
              actionButton("confirm_run", "Ja")
            )
          )
        )
      } else if(input$distanzmatrix == "Minkowski-Distanz" &&
                input$param_paramtab == 2){
        showModal(
          modalDialog(
            title = "Warnung",
            "hier wird mit Euklidische Distanz statt Minkowski-Distanz berechnet. Möchten Sie fortfahren?",
            
            footer = tagList(
              modalButton("Nein"),
              
              actionButton("confirm_run", "Ja")
            )
          )
        )
      }else{
        run_analysis()
      }
    })
    
    observeEvent(input$confirm_run, {
      
      removeModal()
      
      run_analysis()
    })
  
    
    output$debug_matrix <- renderPrint({
      
      cat("Distanz Matrix: ", input$distanzmatrix, "\n")
      cat("Cluster Methode: ", input$clusterverfahren, "\n")
      
      req(d_mat_result())
      
      print(d_mat_result())
      
    })
    
    output$HeatmapPlot <- renderPlot({
      req(d_mat_result())
      print(dim(d_mat_result())) #Debug
      generate_heatmap(d_mat_result())
      
    })
    
    observe({
      
      if(input$distanzmatrix != "Minkowski-Distanz"){
        shinyFeedback::hideFeedback("param_paramtab")
        return()
      }
      
      val <- input$param_paramtab
      msg <- NULL
      
      #error message: p has to be a number
      if(is.null(val)||is.na(val)){                                         #error message: p has to be a number
        msg <- "Bitte eine Zahl eingeben"
      }else if(val <= 0){                                                  #if p<0, error msg: p has to be greater than 0
        msg <- "Falsche eingabe: bitte ein Zahl größer als 0 eingeben"
      }else if(val>10000){
        msg <- "Maximale eingabe Zahl ist 10000"
      }else if(val %% 1 !=0){
        msg <- "Falsche eingabe: bitte ein Integer eingeben"
      }
      shinyFeedback::feedbackDanger(
        "param_paramtab",
        !is.null(msg),
        msg
      )
      
    })
    
    observe({
      
      if(input$distanzmatrix_sidebar != "Minkowski-Distanz"){
        shinyFeedback::hideFeedback("param_heatmap")
        return()
      }
      
      val <- input$param_heatmap
      msg <- NULL
      
      #error message: p has to be a number
      if(is.null(val)||is.na(val)){                                         #error message: p has to be a number
        msg <- "Bitte eine Zahl eingeben"
      }else if(val <= 0){                                                  #if p<0, error msg: p has to be greater than 0
        msg <- "Falsche eingabe: bitte ein Zahl größer als 0 eingeben"
      }else if(val>10000){
        msg <- "Maximale eingabe Zahl ist 10000"
      }else if(val %% 1 !=0){
        msg <- "Falsche eingabe: bitte ein Integer eingeben"
      }
      shinyFeedback::feedbackDanger(
        "param_heatmap",
        !is.null(msg),
        msg
      )
      
    })
    
    observeEvent(input$back, {
      updateTabItems(session, "tabs", selected = "parameter")
    })  
    
    
    observe({
      req(con)
      
      pw <- get_pathwaynames_from_database(con)
      
      pathway_list(pw)
    })
    
    
    observe({
      req(pathway_list())
      
      updateSelectizeInput(
        session,
        "pathways",
        choices = pathway_list(),
        server=TRUE
      )
    })
    
    observeEvent(input$switchtab, {
      
      selected_pathways <- input$pathways
      
      print(selected_pathways)
      
    })
    
    observeEvent(input$back, {
      
      showModal(
        modalDialog(
          title = "Warnung",
          "Das Zurückkehren zu den Parametern löscht die aktuelle Heatmap. Möchten Sie fortfahren",
          
          footer = tagList(
            modalButton("Ja"),
            
            actionButton("confirm_run", "Nein")
          )
        )
      )
    })
    
    observeEvent(input$confirm_run,{
      
      removeModal()
    })
    
    
    inputs_valid <- reactive({
      
      req(input$clusterverfahren)
      req(input$normalisierung)
      req(input$distanzmatrix)
      
      req(!is.null(input$farbpaletten))
      
      
      mink_valid <- TRUE
      
      if(input$distanzmatrix == "Minkowski-Distanz"){
        mink_valid <- !is.null(input$param_paramtab) &&
          !is.na(input$param_paramtab) &&
          input$param_paramtab > 0 &&
          input$param_paramtab <= 10000 &&
          input$param_paramtab == as.integer(input$param_paramtab) 
      }
      
      
        TRUE && mink_valid
    })
    
    observe({
      
      if(isTRUE(inputs_valid())){
        shinyjs::enable("run")
      }else{
        shinyjs::disable("run")
      }
    })
    
    observeEvent(input$run, {
      print("RUN CLICKED")
    })
    
    
    observeEvent(input$analyze_pathways, {
      cov_matrix <- analyze_pathways_coverage(
        daten(),
        input$pathways
      )
      
      coverage_result(cov_matrix)
    })
    
    output$coverage_table <- renderTable({
      
      req(coverage_result())
      
      as.data.frame(coverage_result())
    }, rownames = TRUE)
    
    observeEvent(input$confirm_button, {
      
      showModal(
        modalDialog(
          title = "Warnung",
          "Einige Gene in den ausgewählten Pathways wurden entfernt. Möchten Sie trotzdem mit den ausgewählten Pathways fortfahren?",
          
          footer = tagList(
            modalButton("Andere Pathways auswählen"),
            
            actionButton("continue_analysis", "Ja")
          )
        )
      )
    })
    
    observeEvent(input$continue_analysis,{
      
      removeModal()
      
      updateTabItems(
        session,
        "tabs",
        selected = "paramter"
      )
    })

   
  }  
  shinyApp(ui, server)
}

