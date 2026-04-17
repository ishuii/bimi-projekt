
library(shiny)
library(dipsaus)
library(shinydashboard)


if(interactive()){
  
main <- dashboardPage(skin = "yellow",
    dashboardHeader(title = "ClusterIt"),
    
    dashboardSidebar(
      sidebarMenu(id = "tabs",
        menuItem("Startseite", tabName = "Startseite", icon = icon("home")),
        menuItem("Datei Hochladen", icon = icon("upload"), tabName = "datei_hochladen"),
        menuItem("Parametern Wählen", icon = icon("sliders"), tabName = "parameter"),
        menuItem("Heatmap", icon = icon("bar"), tabName = "heatmap")
        
        
        )
      ),
    
    dashboardBody(
      
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
                    
                    
                    selectInput(inputId = "distanzmatrix", label = "Distanz Matrix auswählen", 
                                choices = c("Euklidische distanz", "b", "c"))
                    
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
                
                actionButton('run', 'Run Cluster Analyse'),
                
                ),
        
        
        tabItem(tabName = "heatmap",
                h2("Heatmap"))
      )
    )
 )
  

server <- function(input, output, session) {
  output$Beispieltext <- renderText({
    paste("Deine Datei:", input$x)
  })
  
  
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
  
  observeEvent(input$nextpage, {
    updateTabItems(session, "tabs", selected = "datei_hochladen")
  })
  
  
  observeEvent(input$switchtab, {
   updateTabItems(session, "tabs", selected = "parameter")
  })
  
  observeEvent(input$run, {
    updateTabItems(session, "tabs", selected = "heatmap")
  })
  
}
  
shinyApp(main, server)
}
  



?valueBox
