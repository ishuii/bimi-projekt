
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
library(plotly)

source("R/clustering/normalization_methods.R")
source("data/database_functions_v4.r")
source("R/clustering/hierarchical_clustering.R")
source("R/visualization/final_dendrogram.R")



if(interactive()){
  
  
  ui <- dashboardPage(
    dashboardHeader(title = "ClusterIt!"),
    
    dashboardSidebar(
      width = 350,
      sidebarMenu(id = "tabs",
                  menuItem("Startseite", tabName = "Startseite", icon = icon("home")),
                  menuItem("Datei Hochladen", icon = icon("upload"), tabName = "datei_hochladen"),
                  menuItem("Parametern Wählen", icon = icon("sliders"), tabName = "parameter"),
                  menuItem("Visualisierung", tabName = "heatmap"),
                  
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
        background-color: #D1D1D1 !important;
        color: #000000 !important;
      }
      
      .main-header .logo:hover {
      background-color: #FFFFFF !important;
      color: black !important;
    }

      .main-header .navbar {
        background-color: #D1D1D1 !important;
      }

      /* Sidebar */
      .main-sidebar {
        background-color: #D1D1D1 !important;
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
      background-color: #FEFAEC !important;
      color: black !important;
      border-bottom: none;
    }
    
    /* Overrides PRIMARY box header */
    .box.box-primary > .box-header {
      background-color: #FEFAEC !important;
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
                    
                    actionButton(
                      inputId = "drop_na",
                      label = "NA-Spalten entfernen",
                      class = "btn-warning"
                    ),
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
                h2("Visualisierung"),
                
                plotlyOutput("patientDendrogram"),
                plotlyOutput("geneDendrogram"),
                
                
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
                
                
                actionButton('back', 'zurück zum Parametern wählen'),
                conditionalPanel(condition = "input.distanzmatrix == 'Minkowski-Distanz'",
                )
        )
        
      )       
      
    )
  )
  
  
  # ------------------------------------------------------------------------------------------------
  server <- function(input, output, session) {
    
    cluster_result <- reactiveVal(NULL)
    
    cluster_bundle <- reactiveVal(NULL)
    
    d_mat_result <- reactiveVal(NULL)
    
    pathway_list <- reactiveVal()
    
    coverage_result <- reactiveVal(NULL)
    
    tree_patient <- reactiveVal(NULL)
    order_patient <- reactiveVal(NULL)
    cluster_patient <- reactiveVal(NULL)
    
    tree_gene <- reactiveVal(NULL)
    order_gene <- reactiveVal(NULL)
    cluster_gene <- reactiveVal(NULL)
    
    clust_config <- reactiveValues(
      method = "Single-Linkage",
      normalisation = "normalize_log_zscore",
      distance = "Euklidische Distanz",
      alpha_a = 0.5,
      alpha_b = 0.5,
      beta = 0,
      gamma = 0,
      minkowski_p = 1
    )
    
    output$Beispieltext <- renderText({
      paste("Deine Datei:", input$x)
    })
    preset_values <- reactiveVal(list()) #Create reactive variable list
    options(shiny.maxRequestSize = 300 * 1024^2)
    # CSV IMPORT BACKEND
    daten_original <- reactiveVal(NULL)
    
    daten_aktuell <- reactiveVal(NULL)
    
    na_infos <- reactiveVal(NULL)
    
    observeEvent(input$Datei_csv, {
      
      req(input$Datei_csv)
      
      df <- read.table(
        input$Datei_csv$datapath,
        header = TRUE,
        stringsAsFactors = FALSE,
        sep = ",",
        dec = ".",
        quote = "\"",
        na.strings = c("", " ", "NA", "NaN", "NULL", "N/A"),
        colClasses = NA
      )
      #      df[df == ""] <- NA
      
      df <- as.data.frame(lapply(df, function(col){
        converted <- suppressWarnings(as.numeric(as.character(col)))
        na_before <- sum(is.na(col))
        na_after <- sum(is.na(converted))
        if(na_after <= na_before + 0.5 * length(col)) converted else col
      }))
      
      na_gesamt <- sum(is.na(df))
      
      
      zeilen_mit_na <- !complete.cases(df)
      
      
      anzahl_zeilen_mit_na <- sum(zeilen_mit_na)
      
      
      na_pro_spalte <- colSums(is.na(df))
      
      daten_original(df)
      daten_aktuell(df)
      
      
      na_infos(list(
        na_gesamt = na_gesamt,
        zeilen_mit_na = anzahl_zeilen_mit_na,
        zeilen_gesamt = nrow(df),
        spalten_gesamt = ncol(df),
        na_pro_spalte = na_pro_spalte,
        bereits_bereinigt = FALSE
      ))
      
      
    })
    
    output$na_info <- renderPrint({
      
      info <- na_infos()
      
      if (is.null(info)) {
        cat("Noch keine CSV-Datei hochgeladen.")
        return(invisible(NULL))
      }
      
      cat("Anzahl aller NA-Werte:", info$na_gesamt, "\n")
      cat("Zeilen mit mindestens einem NA-Wert:", info$zeilen_mit_na, "\n")
      cat("Spalten mit mindestens einem NA-Wert:", sum(info$na_pro_spalte > 0), "\n")
      cat("Zeilen gesamt:", info$zeilen_gesamt, "\n")
      cat("Spalten gesamt:", info$spalten_gesamt, "\n\n")
      
      
      if (isTRUE(info$bereits_bereinigt)) {
        cat("\nStatus: NA-Spalten wurden entfernt.\n")
        cat("Entfernte Spalten:", info$entfernte_spalten, "\n")
        
        if (length(info$entfernte_spalten_namen) > 0) {
          cat("Entfernte Spaltennamen:\n")
          print(info$entfernte_spalten_namen)
        }
      } else {
        cat("\nStatus: Datei wurde geprüft. Es wurde noch nichts gelöscht.\n")
      }
      
      invisible(NULL)
    })
    
    observeEvent(input$drop_na, {
      
      req(daten_aktuell())
      
      df <- daten_aktuell()
      
      # Colum with at least one NA
      spalten_mit_na <- colSums(is.na(df)) > 0
      
      # save name of removed colum
      entfernte_spalten_namen <- names(df)[spalten_mit_na]
      
      #Remove coloum
      df_clean <- df[, !spalten_mit_na, drop = FALSE]
      
      #Number of removed columns
      entfernte_spalten <- sum(spalten_mit_na)
      
      #save clean data
      daten_aktuell(df_clean)
      
      # refresh na_infos
      na_infos(list(
        na_gesamt = sum(is.na(df_clean)),
        spalten_mit_na = sum(colSums(is.na(df_clean)) > 0),
        zeilen_mit_na = sum(!complete.cases(df_clean)),
        zeilen_gesamt = nrow(df_clean),
        spalten_gesamt = ncol(df_clean),
        na_pro_spalte = colSums(is.na(df_clean)),
        bereits_bereinigt = TRUE,
        entfernte_spalten = entfernte_spalten,
        entfernte_spalten_namen = entfernte_spalten_namen
      ))
    })
    daten <- reactive({
      req(daten_aktuell())
      daten_aktuell()
    })
    output$download_pdf <- downloadHandler(
      
      filename = function() {
        paste0("cluster_report_", Sys.Date(), ".pdf")
      },
      
      contentType = "application/pdf",
      
      content = function(file) {
        
        info <- na_infos()
        daten <- daten_aktuell()
        
        pdf(file, width = 8.27, height = 11.69)  # A4 ungefähr
        
        plot.new()
        par(mar = c(1, 1, 1, 1))
        
        y <- 0.95
        
        text(0.05, y, "Cluster Analyse Report", adj = 0, cex = 1.6, font = 2)
        y <- y - 0.08
        
        if (!is.null(input$Datei_csv)) {
          text(0.05, y, paste("Dateiname:", input$Datei_csv$name), adj = 0, cex = 1)
          y <- y - 0.05
        }
        
        text(0.05, y, paste("Erstellt am:", Sys.Date()), adj = 0, cex = 1)
        y <- y - 0.08
        
        text(0.05, y, "NA-Fehlerbehandlung", adj = 0, cex = 1.3, font = 2)
        y <- y - 0.06
        
        if (is.null(info)) {
          
          text(0.05, y, "Noch keine CSV-Datei hochgeladen.", adj = 0, cex = 1)
          
        } else {
          
          text(0.05, y, paste("Anzahl aller NA-Werte:", info$na_gesamt), adj = 0, cex = 1)
          y <- y - 0.045
          
          text(0.05, y, paste("Zeilen mit mindestens einem NA-Wert:", info$zeilen_mit_na), adj = 0, cex = 1)
          y <- y - 0.045
          
          text(0.05, y, paste("Zeilen gesamt:", info$zeilen_gesamt), adj = 0, cex = 1)
          y <- y - 0.045
          
          text(0.05, y, paste("Spalten gesamt:", info$spalten_gesamt), adj = 0, cex = 1)
          y <- y - 0.06
          
          if (isTRUE(info$bereits_bereinigt)) {
            text(0.05, y, "Status: NA-Spalten wurden entfernt.", adj = 0, cex = 1)
            y <- y - 0.045
            
            if (!is.null(info$entfernte_spalten)) {
              text(0.05, y, paste("Entfernte Spalten:", info$entfernte_spalten), adj = 0, cex = 1)
              y <- y - 0.045
            }
          } else {
            text(0.05, y, "Status: Datei wurde geprüft. Es wurde noch nichts gelöscht.", adj = 0, cex = 1)
            y <- y - 0.045
          }
        }
        
        y <- y - 0.06
        
        text(0.05, y, "Gewählte Parameter", adj = 0, cex = 1.3, font = 2)
        y <- y - 0.06
        
        text(0.05, y, paste("Clusterverfahren:", input$clusterverfahren), adj = 0, cex = 1)
        y <- y - 0.045
        
        text(0.05, y, paste("Distanzmatrix:", input$distanzmatrix), adj = 0, cex = 1)
        y <- y - 0.045
        
        text(0.05, y, paste("Normalisierung:", input$normalisierung), adj = 0, cex = 1)
        y <- y - 0.045
        
        text(0.05, y, paste("Anzahl Cluster:", input$anzahlcluster), adj = 0, cex = 1)
        
        if (!is.null(daten)) {
          
          plot.new()
          par(mar = c(1, 1, 1, 1))
          
          text(0.05, 0.95, "Datensatz-Vorschau", adj = 0, cex = 1.4, font = 2)
          
          preview <- head(daten[, seq_len(min(5, ncol(daten))), drop = FALSE], 10)
          preview_text <- capture.output(print(preview))
          
          y <- 0.88
          
          for (line in preview_text) {
            text(0.05, y, line, adj = 0, cex = 0.75, family = "mono")
            y <- y - 0.04
          }
        }
        
        dev.off()
      }
    )
    
    observeEvent(input$anzahlcluster, {   # Save User Choice Cluster
      tmp <- preset_values()
      tmp$anzahlcluster <- input$anzahlcluster
      preset_values(tmp)
    })  
    
    observeEvent(input$clusterverfahren, { #Save User Choice Clusterfunction
      tmp <- preset_values()
      tmp$clusterverfahren <- input$clusterverfahren
      preset_values(tmp)
    })  
    
    observeEvent(input$distanzmatrix, { #Save User Choice distance
      tmp <- preset_values()
      tmp$distanzmatrix <- input$distanzmatrix
      preset_values(tmp)
      
    })
    
    observeEvent(input$normalisierung, { #Save User Choice Normalisierung
      tmp <- preset_values()
      tmp$normalisierung <- input$normalisierung
      preset_values(tmp)
    })
    observeEvent(input$farbpaletten, { #Save User Choice Color
      tmp <- preset_values()
      tmp$farbpaletten <- input$farbpaletten
      preset_values(tmp)
    })
    
    
    refresh_presets <- function() { # Refresh Preset Dropdown
      if (!dir.exists("presets")) {
        dir.create("presets")
      }
      
      dateien <- list.files(
        path = "presets",
        pattern = "\\.json$",
        full.names = TRUE
      )
      
      updateSelectInput(
        session,
        "preset_datei",
        choices = setNames(dateien, basename(dateien))
      )
    }
    
    
    observeEvent(input$load_preset, { #Load Preset after user choice
      req(input$preset_datei)
      
      preset <- jsonlite::fromJSON(input$preset_datei)
      
      if (!is.null(preset$anzahlcluster)) {
        updateNumericInput(session, "anzahlcluster", value = preset$anzahlcluster)
      }
      
      if (!is.null(preset$clusterverfahren)) {
        updateSelectInput(session, "clusterverfahren", selected = preset$clusterverfahren)
      }
      
      if (!is.null(preset$normalisierung)) {
        updateSelectInput(session, "normalisierung", selected = preset$normalisierung)
      }
      
      if (!is.null(preset$farbpaletten)) {
        updateRadioButtons(session, "farbpaletten", selected = preset$farbpaletten)
      }
      
      if (!is.null(preset$distanzmatrix)) {
        updateSelectInput(session, "distanzmatrix", selected = preset$distanzmatrix)
      }
    })
    
    
    observeEvent(input$nextpage, {
      updateTabItems(session, "tabs", selected = "datei_hochladen")
    })
    
    
    observeEvent(input$switchtab, {
      updateTabItems(session, "tabs", selected = "parameter")
    })
    
    observeEvent(input$clusterverfahren, {
      clust_config$method <- input$clusterverfahren
    })
    
    observeEvent(input$distanzmatrix, {
      clust_config$distance <- input$distanzmatrix
    })
    
    observeEvent(input$normalisierung, {
      clust_config$normalisation <- input$normalisierung
    })
    
    
    observeEvent(input$clusterverfahren_sidebar, {
      clust_config$method <- input$clusterverfahren_sidebar
    })
    
    observeEvent(input$distanzmatrix_sidebar, {
      clust_config$distance <- input$distanzmatrix_sidebar
    })
    
    observeEvent(input$normalisierung_sidebar, {
      clust_config$normalisation <- input$normalisierung_sidebar
    })
    
    observeEvent(input$alpha_a, {
      clust_config$alpha_a <- input$alpha_a
    })
    
    observeEvent(input$alpha_b, {
      clust_config$alpha_b <- input$alpha_b
    })
    
    observeEvent(input$beta, {
      clust_config$beta <- input$beta
    })
    
    observeEvent(input$gamma, {
      clust_config$gamma <- input$gamma
    })
    
    
    observe({
      updateSelectInput(session, "clusterverfahren",
                        selected = clust_config$method)
      
      updateSelectInput(session, "clusterverfahren_sidebar",
                        selected = clust_config$method)
      
      updateSelectInput(session, "distanzmatrix",
                        selected = clust_config$distance)
      
      updateSelectInput(session, "distanzmatrix_sidebar",
                        selected = clust_config$distance)
      
      updateSelectInput(session, "normalisierung",
                        selected = clust_config$normalisation)
      
      updateSelectInput(session, "normalisierung_sidebar",
                        selected = clust_config$normalisation)
    })
    
    run_analysis <- function(){   
      
      cat("Analysis started\n")
      
      tryCatch({
        
        req(daten())
        req(clust_config$distance)
        req(clust_config$method)
        req(clust_config$normalisation)
        
        #calls the updated data
        data <- daten()
        cat("data dims:", nrow(data), ncol(data), "\n")
        
        data <- daten()
        cat("data dims:", nrow(data), ncol(data), "\n")
        cat("column names:", paste(names(data), collapse=", "), "\n")  # <- ADD THIS
        cat("column types:", paste(sapply(data, class), collapse=", "), "\n")  # <- AND THIS
        
        #filters rows by selected pathways
        selected_pathways <- input$pathways
        if(!is.null(selected_pathways)&&length(selected_pathways)>0){
          gene_ids <- get_geneIDS_for_pathways(chosen_pathways = selected_pathways,con=con)
          cat("Gene IDs found:", length(gene_ids), "\n")
          data <- extract_relevant_genes(extracted_genes = gene_ids, original_data = data)
          cat("Rows after pathways filter:", nrow(data), "\n")
        }
        
        #keep numeric only
        data <- data[sapply(data, is.numeric)]
        cat("numeric cols:", ncol(data), "\n")
        
        #prevents empty numeric matrix crash
        req(ncol(data) > 0)
        cat("ncol check OK\n")
        
        #placeholder normalization
        df_normalized <- switch(clust_config$normalisation,
                                "normalize_log_zscore" = normalization(data, 1),
                                "normalize_log_only" = normalization(data,2), 
                                "normalize_log_median_centering" = normalization(data, 3), 
                                "normalize_log_mad" = normalization(data,4),
                                data
        )
        
        cat("normalisation OK, dims:", nrow(df_normalized), ncol(df_normalized), "\n")
        
        method <- switch(clust_config$distance,
                         "Euklidische Distanz" = "euclidean",
                         "Manhattan-Distanz" = "manhattan",
                         "Minkowski-Distanz" = "minkowski",
                         "Canberra-Distanz" = "canberra",
                         "Pearson-Distanz" = "pearson",
                         "Winkeldistanz (Angular Seperation)" = "angular"
        )
        
        cat("distance method String:", method, "\n")
        
        
        method_name <- switch (clust_config$method,
                               "Single-Linkage" = "single",
                               "Average-Linkage" = "average",
                               "Complete-Linkage" = "complete",
                               "Custom-Linkage" = "custom"
        )
        
        cat("cluster method string:", method_name, "\n")
        
        custom_params <- if(method_name == "custom"){
          list(
            alpha_a = clust_config$alpha_a,
            alpha_b = clust_config$alpha_b,
            beta = clust_config$beta,
            gamma = clust_config$gamma
          )
        } else NULL
        
        
        #---------------------Patient data ------------------------
        data_pat <- t(df_normalized)
        
        #calling distanz matrix function
        d_pat <- dist_cpp(data_pat, method = method)
        
        cluster_pat <- hierarchical_clustering(
          d_pat,
          method = method_name,
          custom_params = custom_params
        )
        
        tree_pat <- build_tree(cluster_pat)
        order_pat <- get_order_vector(tree_pat)
        
        
        #------------------Gene data ------------------------------
        
        data_gene <- df_normalized
        
        d_gene <- dist_cpp(data_gene, method = method)
        
        cluster_genes <- hierarchical_clustering(
          d_gene,
          method = method_name,
          custom_params = custom_params
        )
        
        tree_genes <- build_tree(cluster_genes)
        order_genes <- get_order_vector(tree_genes)
        
        #---------------------------------------------------------
        
        
        cluster_patient(cluster_pat)
        tree_patient(tree_pat)
        order_patient(order_pat)
        
        cluster_gene(cluster_genes)
        tree_gene(tree_genes)
        order_gene(order_genes)
        
        shinyjs::delay(100, {
          updateTabItems(session, "tabs", selected = "heatmap")
        })
        
        cat("tab switch triggered\n")
        
      }, error = function(e){
        
        cat("\n=== ERROR after step above ===\n")
        cat("Message:", conditionMessage(e), "\n")
        print(traceback())
        cat("====================\n")
      })
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
    
    
    
    observeEvent(input$save_preset, { #Save Preset in Json
      req(input$preset_name)
      if (!dir.exists("presets")) {
        dir.create("presets")
      }
      pfad <- file.path("presets", paste0(input$preset_name, ".json"))
      jsonlite::write_json(
        preset_values(),
        path = pfad,
        auto_unbox = TRUE,
        pretty = TRUE
      )
      showNotification(paste("Preset gespeichert unter:", pfad), type = "message")
      refresh_presets()
    })
    
    
    
    output$debug_matrix <- renderPrint({
      
      cat("Distanz Matrix: ", input$distanzmatrix, "\n")
      cat("Cluster Methode: ", input$clusterverfahren, "\n")
      
      req(d_mat_result())
      
      print(d_mat_result())
      
    })
    
    #    output$HeatmapPlot <- renderPlot({
    #      req(d_mat_result())
    #      print(dim(d_mat_result())) #Debug
    #      generate_heatmap(d_mat_result())
    
    #    })
    
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
    
    con <- dbConnect(RSQLite::SQLite(), "GeneDatabase.sqlite")
    
    
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
        selected = "parameter"
      )
    })
    
    
    output$patientDendrogram <- renderPlotly({
      
      req(cluster_patient())
      req(tree_patient())
      req(order_patient())
      
      p <- generate_dendro(cluster_patient(), tree_patient(), order_patient(), title = "Patienten", names_vector = NULL, class_labels = NULL)
      
      ggplotly(p) 
    })
    
    
    output$geneDendrogram <- renderPlotly({
      
      req(cluster_gene())
      req(tree_gene())
      req(order_gene())
      
      p <- generate_dendro(cluster_gene(), tree_gene(), order_gene(), title = "Gene", names_vector = NULL, class_labels = NULL)
      
      ggplotly(p) 
    })
    
    
    
  }  
  shinyApp(ui, server)
}

