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


race_data <- data.frame(
  year = c(1960, 1970, 1980, 1990, 2000, 2010, 2020),
  white = c(79.7, 80.5, 80.5, 80.5, 80.5, 75.8, 17.1),
  non_white = c(20.3, 19.5, 19.5, 19.5, 19.5, 24.2, 82.9)
)


raw_temp <- read_csv("Data/temp.csv")
temp_df <- raw_temp %>%
  select(DATE, TAVG) %>%            
  filter(!is.na(TAVG)) %>%          
  separate(DATE, into = c("year", "month"), sep = "-") %>%  
  mutate(
    year = as.integer(year),
    month = as.integer(month),
    temperature = as.numeric(TAVG)
  ) %>%
  select(year, month, temperature)

discharge_df <- read_csv("Data/cleaned_discharge.csv") %>%
  mutate(
    discharge = as.numeric(discharge),
    year = as.integer(year),
    month = as.integer(month)
  ) %>%
  filter(!is.na(year), !is.na(month))

gage_height_df <- read_csv("Data/cleaned_gage_height.csv") %>%
  mutate(
    gage_height = as.numeric(gage_height),
    year = as.integer(year),
    month = as.integer(month)
  ) %>%
  filter(!is.na(year), !is.na(month))









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

    
    
    
    
    
    
        
    nav_panel("Map",
              h3("Puerto Rico Map"),
              leafletOutput("island_map"),
              br(),
              p("This map shows the location of San Juan, Puerto Rico.")
    ),
    
    
    
    nav_panel("Demographics", 
              h3("Demographic Data"),
              sliderInput("year", "Select Year:",
                          min = 1960,
                          max = 2023,
                          value = 2023,
                          step = 1),
              plotOutput("population_plot", height = "300px"),
              
              fluidRow(
                column(6, plotOutput("race_pie", height = "300px")),
                column(6, tableOutput("race_table"),"Race Composition by Decade (%)",
                       tableOutput("race_table"),)
              ),
              p(h6("The decrease in the White population is primarily due to changes in how the race question was asked and how people responded — not due to population decline. (U.S. Census Bureau, 2021)"))
              
              
    ),
    
    
    
    
    
    
    nav_panel("Hydrology", 
              h3("Hydrology"),
              plotOutput("temp_plot"),
              sliderInput("shared_year", "Select Year:",
                          min = 1970,
                          max = 2023,
                          value = 2023,
                          step = 1),
              fluidRow(
                column(6,
                       plotOutput("gage_plot", height = "300px")
                ),
                column(6,
                       plotOutput("discharge_plot", height = "300px")
                )
              )

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

  #map
  output$island_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -66.1, lat = 18.4, zoom = 8) %>%
      addMarkers(lng = -66.1, lat = 18.4, popup = "San Juan, Puerto Rico")
  })
  
  #population plot
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
  
  
  
  #racial plot
  
  output$race_table <- renderTable({
    race_data
  }, digits = 1, bordered = TRUE, striped = TRUE, spacing = "m", align = "c")
  
  
  output$race_pie <- renderPlot({
    
    # make 196x to be 1960
    decade_year <- floor(input$year / 10) * 10
    
    if (any(race_data$year == decade_year)) {
      race_slice <- race_data[race_data$year == decade_year, ]
      
      pie_df <- data.frame(
        category = c("White", "Non-White"),
        percentage = c(race_slice$white, race_slice$non_white)
      )
      
      ggplot(pie_df, aes(x = "", y = percentage, fill = category)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y") +
        labs(title = paste("Race Breakdown (approx.):", decade_year)) +
        theme_void(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5),
          legend.title = element_blank(),
          legend.position = "right"
        ) +
        scale_fill_manual(values = c("White" = "skyblue", "Non-White" = "darkorange"))
    }
  })
  
  
  #temp plot
  output$temp_plot <- renderPlot({
    req(temp_df)
    
    selected_year <- input$shared_year
    filtered <- temp_df %>% filter(year == selected_year)
    
    ggplot(filtered, aes(x = month, y = temperature)) +
      geom_line(color = "tomato", size = 1.2) +
      geom_point(color = "black", size = 3) +
      scale_x_continuous(breaks = 1:12) +
      scale_y_continuous(limits = c(20, 32)) +
      labs(title = paste("Average Monthly Temperature in", selected_year),
           x = "Month", y = "Temperature (°C)") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold")
      )
  })
  
  #discharge and gage height
  output$gage_plot <- renderPlot({
    selected <- gage_height_df %>% filter(year == input$shared_year)
    
    ggplot(selected, aes(x = factor(month), y = gage_height)) +
      geom_col(fill = "steelblue") +
      labs(title = paste("Average Gage Height -", input$shared_year),
           x = "Month", y = "Gage Height") +
      theme_minimal()
  })
  
  
  output$discharge_plot <- renderPlot({
    selected <- discharge_df %>% filter(year == input$shared_year)
    
    ggplot(selected, aes(x = factor(month), y = discharge)) +
      geom_col(fill = "darkorange") +
      labs(title = paste("Average Discharge -", input$shared_year),
           x = "Month", y = "Discharge") +
      theme_minimal()
  })
  

}
  


shinyApp(ui = ui, server = server)

