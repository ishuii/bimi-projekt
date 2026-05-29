options(shiny.maxRequestSize = 150 * 1024^2)  # 150 MB


library(shiny)
library(dipsaus)
library(shinydashboard)
library(jsonlite)
library(devtools)
library(distRcpp)
library(shinyFeedback)
library(shinyjs)
library(rmarkdown)
library(knitr)
library(bslib)
library(bsicons)
library(shinyBS)
library(RSQLite)
library(DBI)

source("R/clustering/single_linkage.R")
source("R/clustering/average_linkage.R")
source("R/clustering/complete_linkage.R")
source("tests/heatmap_tests.R")
source("R/clustering/normalization_methods.R")
source("data/database_functions_v2.r")



if(interactive()){
  

ui <- dashboardPage(
    dashboardHeader(title = "ClusterIt"),
    
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
            status = "primary",
            
            selectInput(inputId = "clusterverfahren", label = "Clusterverfahren auswählen", 
                        choices = c("Single-Linkage", "Average-Linkage", "Complete-Linkage")),
            
            
            selectInput(inputId = "normalisierung", label = "Normalisierungs Verfahren auswählen", 
                        choices = c("Standard", "Normalize Log Only", "Correlation-based Normalization", "Logarithm with MAD")),
            
            
            selectInput(inputId="distanzmatrix", label = "Distanz Matrix auswählen", 
                        choices = c("Euklidische Distanz", "Manhattan-Distanz", "Minkowski-Distanz", "Canberra-Distanz", "Pearson-Distanz", "Winkeldistanz (Angular Seperation)")),
            
            conditionalPanel(condition = "input.distanzmatrix == 'Minkowski-Distanz'",
                             useShinyFeedback(),
                             numericInput(inputId = "param", label = "Parameter p eingeben", value = 1),
                             textOutput("result")),
            
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
            
          ),
          
        )
        
        
        ),
      br(),
      div(
        style = "padding: 10px;",
        downloadButton("download_pdf", "PDF exportieren")
      
      )),
    
    dashboardBody(
      
      tags$head(
        
        tags$style(HTML("
             .main-header {position:fixed; width:100%;}
             .content-wrapper{padding-top; 50px !important;}            ")),
      
        
        tags$style(HTML("
      /* Main header */
      .main-header .logo {
        background-color: #ECECEC !important;
        color: #ECECEC !important;
      }

      .main-header .navbar {
        background-color: #ECECEC !important;
      }

      /* Sidebar */
      .main-sidebar {
        background-color: #ECECEC !important;
      }

      /* Sidebar menu hover */
      .sidebar-menu > li:hover > a {
        background-color: #000000 !important;
      }

      /* Active tab */
      .sidebar-menu > li.active > a {
        background-color: #000000 !important;
        color: black !important;
      }
    "))
      ),
      
      useShinyjs(),
      
      tags$style(HTML("
        .form-control{
          font-size: 16px;
        }
        
        label{
        font-size: 16px;
        font-weight:600;
        }
      ")),
      
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

                fluidRow(
                  
                  box(
                    title = "Datensatz Parametern einstellen",
                    width = 12,
                    status = "success",
                    
                    selectizeInput(
                      "pathways",
                      "Pathways auswählen",
                      
                      choices = NULL,
                      
                      multiple = TRUE
                    )
                
                  )
                ),
                
                actionButton('switchtab', 'Parametern Wählen'),
                
        ),
        
        tabItem(tabName = "parameter",
                h2("Bitte Parametern benötigt zur Cluster Analyse, auswählen"),
                
                fluidRow(
                  
                  box(
                    title = "Cluster Einstellungen",
                    width = 12,
                    solidHeader = TRUE,
                    status = "primary",
                    
                    selectInput(inputId = "clusterverfahren", label = "Clusterverfahren auswählen", 
                                choices = c("Single-Linkage", "Average-Linkage", "Complete-Linkage")),
                    
            
                    
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
                                choices = c("Standard", "Normalize Log Only", "Correlation-based Normalization", "Logarithm with MAD")),
                    
                    
                    selectInput(inputId="distanzmatrix", label = "Distanz Matrix auswählen", 
                                choices = c("Euklidische Distanz", "Manhattan-Distanz", "Minkowski-Distanz", "Canberra-Distanz", "Pearson-Distanz", "Winkeldistanz (Angular Seperation)")),
                    
                   conditionalPanel(condition = "input.distanzmatrix == 'Minkowski-Distanz'",
                                    useShinyFeedback(),
                                    numericInput(inputId = "param", label = "Parameter p eingeben", value = 1),
                                    textOutput("result")),
                   
                  ),
        
                ),
                
                fluidRow(
                  box(
                    title = "Preset speichern/laden",
                    width = 12,
                    status = "warning",
                    solidHeader = TRUE,
                    
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
                
                
                actionButton('back', 'zurück zum Parametern wählen'),
                conditionalPanel(condition = "input.distanzmatrix == 'Minkowski-Distanz'",
                                 useShinyFeedback())
        )
        
      )       

    )
 )

  

server <- function(input, output, session) {
  

  cluster_result <- reactiveVal(NULL)
  
  d_mat_result <- reactiveVal(NULL)
  
  pathway_list <- reactiveVal()
  
  output$Beispieltext <- renderText({
    paste("Deine Datei:", input$x)
  })
  preset_values <- reactiveVal(list()) #Create reactive variable list
  options(shiny.maxRequestSize = 1024 * 1024^2)
  # CSV IMPORT BACKEND
  daten_original <- reactiveVal(NULL)
  
  daten_aktuell <- reactiveVal(NULL)
  
  na_infos <- reactiveVal(NULL)
  
  observeEvent(input$Datei_csv, {
    
    req(input$Datei_csv)
    
    df <- read.csv(
      input$Datei_csv$datapath,
      header = TRUE,
      stringsAsFactors = FALSE,
      na.strings = c("", " ", "NA", "NaN", "NULL", "N/A")
    )
    df[df == ""] <- NA
    
   
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
  
  run_analysis <- function(){    
    
    #calls the updated data
    data <- daten()
    
    #keep numeric only
    data <- data[sapply(data, is.numeric)]
    
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
    
    
    
    updateTabItems(session, "tabs", selected = "heatmap")
    
  }
  
  observeEvent(input$run, {
    if(input$distanzmatrix == "Minkowski-Distanz" &&
      input$param == 1){
      
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
      input$param == 2){
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
  
  observeEvent(input$confirm_run,{
    
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
  
  output$HeatmapPlot <- renderPlot({
    req(d_mat_result())
    
    generate_heatmap(d_mat_result())

  })
  
  observe({
    
    val <- input$param
    
    #error message: p has to be a number
    shinyFeedback::feedbackDanger(
      "param",
      is.null(val) || is.na(val),
      "Bitte eine Zahl eingeben"
    )
    
    #if p<0, error msg: p has to be greater than 0
    shinyFeedback::feedbackDanger(
      "param",
      !is.null(val) && !is.na(val) && val <= 0,
      "Falsche eingabe: bitte ein Zahl größer als 0 eingeben"
    )
    
    shinyFeedback::feedbackDanger(
      "param",
      !is.null(val) && !is.na(val) && val > 10000,
      "Maximale eingabe Zahl ist 10000"
    )
      
    shinyFeedback::feedbackDanger(
      "param",
      !is.null(val) && !is.na(val) && val %% 1 != 0,
      "Falsche eingabe: bitte ein Integer eingeben"
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
    req_cluster <- !is.null(input$clusterverfahren) && input$clusterverfahren != "" #ensures that some option is chosen from cluster methods dropdown
    
    req_farb <- length(input$farbpaletten) >0
    
    req_norm <- !is.null(input$normalisierung) && input$normalisierung != ""
    
    req_dist <- !is.null(input$distanzmatrix) && input$distanzmatrix != ""
    
    
    mink_valid <- TRUE
    
    if(input$distanzmatrix == "Minkowski-Distanz"){
      mink_valid <- !is.null(input$param) &&
        !is.na(input$param) &&
        input$param > 0 &&
        input$param <= 10000 &&
        input$param %% 1 == 0
    }
    
    req_cluster &&
      req_norm &&
      req_dist &&
      req_farb &&
      mink_valid
  })
  
  observe({
    
    if(inputs_valid()){
      shinyjs::enable("run")
    }else{
      shinyjs::disable("run")
    }
  })
  session$onFlushed(function() {
    refresh_presets()
  }, once = TRUE)
  
  
  
}  
shinyApp(ui, server)
}
  

