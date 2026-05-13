
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

source("R/clustering/single_linkage.R")
source("R/clustering/average_linkage.R")
source("R/clustering/complete_linkage.R")
source("Heatmap_Funktion.R")
source("R/clustering/normalization_methods.R")




if(interactive()){
  
ui <- dashboardPage(
    dashboardHeader(title = "ClusterIt"),
    
    dashboardSidebar(
      sidebarMenu(id = "tabs",
        menuItem("Startseite", tabName = "Startseite", icon = icon("home")),
        menuItem("Datei Hochladen", icon = icon("upload"), tabName = "datei_hochladen"),
        menuItem("Parametern Wählen", icon = icon("sliders"), tabName = "parameter"),
        menuItem("Heatmap", tabName = "heatmap")
        
        
        ),
      br(),
      div(
        style = "padding: 10px;",
        downloadButton("download_pdf", "PDF exportieren")
      
      )),
    
    dashboardBody(
      
      tags$head(
        tags$style(HTML("
      /* Main header */
      .main-header .logo {
        background-color: #FBEEB9 !important;
        color: #000000 !important;
      }

      .main-header .navbar {
        background-color: #FBEEB9 !important;
      }

      /* Sidebar */
      .main-sidebar {
        background-color: #D1D1D1 !important;
      }

      /* Sidebar menu hover */
      .sidebar-menu > li:hover > a {
        background-color: #000000 !important;
      }

      /* Active tab */
      .sidebar-menu > li.active > a {
        background-color: #e8d98f !important;
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
                       title = "Auswertung",
                       width = 12,
                       tableOutput("Beispieltext"),
                       verbatimTextOutput("Spalten")
                       )
                     ),

                fluidRow(
                  
                  box(
                    title = "Datensatz Parametern einstellen",
                    width = 12,
                    status = "success",
                    
                    checkboxGroupInput(inputId = "pathways", label = "Pathways Auswählen",
                                       choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                                       selected = 1, width = '400px', inline = FALSE),
                    
                    checkboxGroupInput(inputId = "gene", label = "Gene Auswählen",
                                       choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                                       selected = 1, width = '400px', inline = FALSE)
                
                  )
                ),
                
                actionButton('switchtab', 'Parametern Wählen'),
                
        ),
        
        tabItem(tabName = "parameter",
                h2("Bitte Parametern benötigt zur Cluster Analyse, auswählen"),
                
                fluidRow(
                  
                  box(
                    title = "Cluster Einstellungen",
                    width = 6,
                    solidHeader = TRUE,
                    status = "primary",
                    
                    selectInput(inputId = "clusterverfahren", label = "Clusterverfahren auswählen", 
                                choices = c("Single-Linkage", "Average-Linkage", "Complete-Linkage")),
                    
                    
                    numericInput(inputId = "anzahlcluster", label = "Anzahl von Clustern eingeben",
                                 value = 2, min = 1, max = 10),
                    
                    
                    selectInput(inputId = "normalisierung", label = "Normalisierungs Verfahren auswählen", 
                                choices = c("a", "b", "c")),
                    
                    
                    selectInput(inputId="distanzmatrix", label = "Distanz Matrix auswählen", 
                                choices = c("Euklidische Distanz", "Manhattan-Distanz", "Minkowski-Distanz", "Canberra-Distanz", "Pearson-Distanz", "Winkeldistanz (Angular Seperation)")),
                    
                   conditionalPanel(condition = "input.distanzmatrix == 'Minkowski-Distanz'",
                                    useShinyFeedback(),
                                    numericInput(inputId = "param", label = "Parameter p eingeben", value = 1),
                                    textOutput("result")),
                   
                  ),
                  
                  box(
                    title = "Datensatz Einstellungen",
                    width = 6,
                    status = "primary",
                    solidHeader = TRUE,
                    
                    checkboxGroupInput(inputId = "variable", label = "Variablen Auswählen",
                                       choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                                       selected = 1, width = '400px', inline = FALSE),
                    
                    
                    radioButtons(inputId = "gewebe", label = "Gewebe art auswählen", 
                                 choices = list(
                                   "Gesunde Gewebe" = 1,
                                   "Ungesunde Gewebe" = 2
                                 )
                    ),
                    
                    checkboxGroupInput(inputId = "koerper", label = "Körperteile Auswählen",
                                       choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                                       selected = 1, width = '100px', inline = FALSE),
                    
                  )
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
                
                actionButton('run', 'Run Cluster Analyse'),
                
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
                
                textOutput("selection_feedback"),
                
                
                actionButton('back', 'zurück zum Parametern wählen')
        )
        
      )       

    )
 )

  

server <- function(input, output, session) {

  cluster_result <- reactiveVal(NULL)
  
  d_mat_result <- reactiveVal(NULL)
  
  output$Beispieltext <- renderText({
    paste("Deine Datei:", input$x)
  })
  preset_values <- reactiveVal(list()) #Create reactive variable list
  options(shiny.maxRequestSize = 100 * 1024^2)
  # CSV IMPORT BACKEND
  daten <- reactive({      # for dynamic processing
    req(input$Datei_csv)   # upload check
    read.csv(input$Datei_csv$datapath)
    
  })
  
  #CSV PROCESSING
  output$Spalten <- renderText({
    paste0("Der Datensatz hat ",nrow(daten())-1, " Messwerte", "\n",
           "Der Datensatz hat ",ncol(daten()), " Samples")
  })
  
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
    
    req(input$param)
    
    #error message: p has to be a number
    if(is.na(input$param)){
      feedbackDanger("param", TRUE, "Bitte eine Zahl eingeben")
      
    #if p<0, error msg: p has to be greater than 0
    }else if (input$param <= 0){
      feedbackDanger("param", TRUE, "Falsche eingabe: bitte ein Zahl größer als 0 eingeben")
      
    #if p is not an integer  
    }else if (input$param %% 1 != 0){
      feedbackDanger("param", TRUE, "Falsche eingabe: bitte ein Integer eingeben")
      
      
    }else if (input$param > 10000){
      feedbackDanger("param", TRUE, "Maximale eingabe Zahl ist 10000")
      
    }else{
      feedbackDanger("param", FALSE)
    }
  })
  
  observeEvent(input$back, {
    updateTabItems(session, "tabs", selected = "parameter")
  })  
  
  
}  
shinyApp(ui, server)
}
  

