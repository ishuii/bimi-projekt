# R-file for the GUI
library(shiny)
# User Interface -------------

ui <- fluidPage(
  titlePanel = "Bimi Projekt",
  sidebarLayout(
    mainPanel(
      textOutput("Bespieltext"),
      verbatimTextOutput("Spalten")
    ),
    
    fileInput("Datei_csv", "Datei hochladen", accept = c(".csv"))
    
  )
  
)