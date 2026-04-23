# Server logic

library(shiny)

options(shiny.maxRequestSize = 100 * 1024^2) # increase upload limit

#Server ---------------------
server <- function(input, output) {
  output$Beispieltext <- renderText({
    paste("Deine Zahl:", input$x)
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
  
}

  
  
  
  
  

