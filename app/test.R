
library(shiny)
library(dipsaus)
library(shinydashboard)
library(jsonlite)
library(devtools)
library(distRcpp)
library(shinyFeedback)
library(shinyjs)

source("R/clustering/single_linkage.R")
source("R/clustering/average_linkage.R")
source("R/clustering/complete_linkage.R")
source("Heatmap_Funktion.R")
source("R/clustering/normalization_methods.R")




if(interactive()){
  
ui <- dashboardPage(skin = "red",
    dashboardHeader(title = "ClusterIt"),
    
    dashboardSidebar(
      sidebarMenu(id = "tabs",
        menuItem("Startseite", tabName = "Startseite", icon = icon("home")),
        menuItem("Datei Hochladen", icon = icon("upload"), tabName = "datei_hochladen"),
        menuItem("Parametern Wählen", icon = icon("sliders"), tabName = "parameter"),
        menuItem("Heatmap", tabName = "heatmap")
        
        
        )
      ),
    
    dashboardBody(
      
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
                    

                    radioButtons(inputId = "farbpaletten", label = "Farbpalette für Heatmaps auswählen", 
                                 choices = list(
                                   "Option 1" = 1,
                                   "Option 2" = 2,
                                   "Option 3" = 3
                                 )
                    ),
                    
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
                verbatimTextOutput("debug_matrix")
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
    if(input$param == 1){
      
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
    } else if(input$param == 2){
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
  
}  
shinyApp(ui, server)
}
  

