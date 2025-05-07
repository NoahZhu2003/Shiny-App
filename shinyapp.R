library(shiny)
library(bslib)
library(leaflet)

ui <- page_fluid(
  
  titlePanel("Puerto Rico Island"),
  
  navset_card_pill(
    nav_panel("General Description", 
              h3("Overview"),
              p("Puerto Rico is a U.S. territory located in the Caribbean."),
              p("It has a rich culture and history, and a population of about 3.2 million people.")
    ),
    
    nav_panel("Map & Key Facts",
              h3("Puerto Rico Map"),
              leafletOutput("island_map"),
              br(),
              p("This map shows the location of San Juan, Puerto Rico.")
    ),
    
    nav_panel("Demographics", 
              h3("Demographic Data (Sample Placeholder)"),
              p("Demographic information will go here.")
    ),
    
    nav_panel("Hydrology", 
              h3("Hydrology (Sample Placeholder)"),
              p("Hydrological data will go here.")
    ),
    
    nav_menu("Other links",
             nav_panel("Citation", 
                       h4("References"),
                       p("Data sourced from World Bank, US Census, and CIA Factbook.")
             ),
             "----",
             "Description:",
             nav_item(
               a("Shiny", href = "https://shiny.posit.co", target = "_blank")
             )
    ),
    id = "tab"
  )
)

server <- function(input, output, session) {
  
  output$island_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -66.1, lat = 18.4, zoom = 8) %>%
      addMarkers(lng = -66.1, lat = 18.4, popup = "San Juan, Puerto Rico")
  })
  
}

shinyApp(ui = ui, server = server)
