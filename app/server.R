# Server-Logik
library(shiny)
options(shiny.maxRequestSize = 100 * 1024^2) # Erhöhe Uploadlimit
#Server ---------------------
server<- function(input, output) {
  output$Beispieltext <- renderText({
    paste("Deine Zahl:", input$x)
  })
  
  
  # CSV IMPORT BACKEND
  daten <- reactive({      #Für dynamische Verarbeitung
    req(input$Datei_csv) # Check hochgeladen
    read.csv(input$Datei_csv$datapath)
    
  })
  #CSV DATENVERARBEITUNG
  
  output$Spalten <- renderText({
    paste0("Der Datensatz hat ",nrow(daten())-1, " Messwerte", "\n",
   "Der Datensatz hat ",ncol(daten()), " Samples")
    })
  
}

  
  
  
  
  

