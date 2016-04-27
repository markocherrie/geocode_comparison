# geocoding shiny
library(shiny)


shinyUI(bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  headerPanel("Comparison of Geocoding Providers"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                textInput("str", label = h3("Address"), value = "Enter address..."),
                actionButton("goButton", "Geocode!")
  ))
)  