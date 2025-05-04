library(shiny)
library(leaflet)
library(shinythemes)
shinyUI(fluidPage(
  
  titlePanel("Puerto Rico Island Dashboard"),
  
  tabsetPanel(
    tabPanel("General Description",
             h4("This is a summary about Puerto Rico.")
    ),
    
    tabPanel("Map & Key Facts",
             leafletOutput("island_map")
    ),
    
    tabPanel("Demographics",
             # 在 Demographics 页直接放 slider 和占位文字
             sidebarLayout(
               sidebarPanel(
                 sliderInput("year", "Select Year:",
                             min = 2000, max = 2024, value = 2012, sep = "")
               ),
               mainPanel(
                 h4(textOutput("demo_text"))
               )
             )
    ),
    
    tabPanel("Hydrology",
             h4("Hydrological data visualization will go here.")
    ),
    
    tabPanel("SWOT Analysis",
             h4("Strengths, Weaknesses, Opportunities, Threats... (placeholder)")
    )
  )
))

