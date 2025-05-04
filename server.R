library(shiny)
library(leaflet)
library(shinythemes)
shinyServer(function(input, output, session) {
  
  output$island_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -66.1, lat = 18.4, zoom = 8) %>%
      addMarkers(lng = -66.1, lat = 18.4, popup = "San Juan, Puerto Rico")
  })
  
  output$demo_text <- renderText({
    paste("Demographic data for the year", input$year, "will be shown here.")
  })
})
