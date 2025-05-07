library(shiny)
library(bslib)
library(leaflet)

ui <- page_fluid(
  
  titlePanel("Puerto Rico Island"),
  
  navset_card_pill(
    nav_panel("General Description", 
              h3("Overview"),
              p("Puerto Rico is an unincorporated territory of the United States located in the northeastern Caribbean Sea.It lies east of the Dominican Republic and west of the U.S. Virgin Islands, situated between the Atlantic Ocean and the Caribbean Sea."),
              img(src = "puerto_rico.jpg.avif", width = "80%"),
              h4("Land Area"),
              p("Puerto Rico covers approximately 9,104 square kilometers (3,515 square miles), about the size of the U.S. state of Connecticut."),
              
              h4("Climate and Temperature Zone"),
              p("The island has a tropical rainforest climate with an average annual temperature of 26°C (78°F)."),
              p("It remains warm year-round with minimal seasonal variation. Hurricane season lasts from June through November."),
              
              h4("Terrain and Nature"),
              p("Puerto Rico features a mountainous interior dominated by the Cordillera Central, with coastal lowlands and beaches along the edges."),
              p("It sits near the Caribbean Plate and occasionally experiences seismic activity."),
              
              h4("Capital and Cities"),
              p("The capital and largest city is San Juan. Other major cities include Ponce, Mayagüez, and Bayamón."),
              
              h4("Population and Language"),
              p("Puerto Rico has a population of approximately 3.2 million people. Spanish and English are both official languages, with Spanish being more widely spoken."),
              
              h4("Currency and Political Status"),
              p("The island uses the U.S. Dollar (USD). As a U.S. territory, its residents are U.S. citizens but cannot vote in presidential elections."),
              p("Puerto Rico has its own constitution and exercises local self-governance.")
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
