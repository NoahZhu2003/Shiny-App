library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

#data analysis
raw_data <- read_csv("Data/population.csv", skip = 4)

pr_pop <- raw_data %>%
  filter(`Country Name` == "Puerto Rico",
         `Indicator Name` == "Population, total")

pr_pop_long <- pr_pop %>%
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "year",
    values_to = "population"
  ) %>%
  mutate(
    year = as.integer(year),
    population = as.numeric(population)
  ) %>%
  filter(!is.na(population))



#shiny
ui <- page_fluid(
  
  titlePanel("Puerto Rico Island"),
  
  navset_card_pill(
    nav_panel("General Description", 
              h2(strong("Overview")),
              p("Puerto Rico is an unincorporated territory of the United States located in the northeastern Caribbean Sea.It lies east of the Dominican Republic and west of the U.S. Virgin Islands, situated between the Atlantic Ocean and the Caribbean Sea."),
              h4("Land Area"),
              p("Puerto Rico covers approximately 9,104 square kilometers (3,515 square miles), about the size of the U.S. state of Connecticut."),
              
              tags$div(
                style = "text-align: center; margin: 20px 0;",
                img(src = "puerto_rico.jpg.avif", width = "80%", style = "border-radius: 12px; box-shadow: 0 4px 8px rgba(0,0,0,0.2);")
              ),
              tags$p(style = "text-align: center; font-style: italic; color: #555;", 
                     "Colorful homes in San Juan, Puerto Rico"),
              
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
              h3("Demographic Data"),
              p("Demographic information will go here."),
              sliderInput("year", "Select Year:",
                          min = min(pr_pop_long$year),
                          max = max(pr_pop_long$year),
                          value = max(pr_pop_long$year),
                          step = 1),
              
              plotOutput("population_plot", height = "300px", width = "50%")
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
  
  
  output$population_plot <- renderPlot({
    selected_year <- input$year
    
    full_data <- pr_pop_long
    
    current_point <- pr_pop_long %>% filter(year == selected_year)
    
    ggplot(full_data, aes(x = year, y = population)) +
      geom_line(color = "skyblue", size = 1.2) +
      geom_point(data = current_point, aes(x = year, y = population), 
                 color = "darkred", size = 4) +
      labs(title = paste("Puerto Rico Population:", selected_year),
           x = "Year", y = "Population") +
      theme_minimal(base_size = 10) +
      theme(
        panel.grid.major = element_line(color = "grey"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        axis.title = element_text(face = "bold")
      )

  })
  
}

shinyApp(ui = ui, server = server)
