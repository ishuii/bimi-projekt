# R-file for the GUI
library(shiny)
# User Interface -------------

ui <- fluidPage(
  title = "Bimi Projekt",
  sliderInput("x", "Zahl", 1, 100, 50),
  sidebarLayout(
    mainPanel(
      textOutput("Bespieltext"),
      textOutput("Spalten")
    ),
    
    fileInput("Datei_csv", "Datei hochladen", accept = c(".csv"))
    
  )
  
)