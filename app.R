# Group 5: Justin Han, Wendy Liang, Rachel Paresa, Alec Raines, Denny Wang
# GEOG458
# Final Project: Seattle's 911 Calls

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(shinythemes)
library(sf)

source("./scripts/build_map.R")
source("./scripts/hood_chart.R")
source("./scripts/type_chart.R")

# fire_stat_df <- read.csv("./data/Fire_Stations.csv", stringsAsFactors = FALSE)
# colnames(fire_stat_df)[1] <- "lng"
# colnames(fire_stat_df)[2] <- "lat"
# hospital_df <- read.csv("./data/Hospitals.csv", stringsAsFactors = FALSE)
# colnames(hospital_df)[1] <- "lng"
# colnames(hospital_df)[2] <- "lat"
# seattle_18 <- st_read("./data/2018_Fire_Calls_Seattle/2018_Fire_Calls_Seattle.shp")
# seattle_17 <- st_read("./data/2017_Fire_Calls_Seattle/2017_Fire_Calls_Seattle.shp")

# seattle <- st_read("../data/seattlefc/seattlefc.shp")
# types_response <- seattle %>% group_by(Type) %>% summarize(count = n())

ui <- navbarPage(
  theme = shinytheme("yeti"),
  "Seattle's 911 Calls",
  # Create a tab panel for your map
  tabPanel(
    "About",
    titlePanel("Project Description/Background"),
    # Create sidebar layout
    sidebarLayout(
      # Side panel for controls
      sidebarPanel(
        width = 3,
        h4("GEOG458: Advanced Digital Geographies"),
        h5("Group 5: Justin Han, Wendy Liang, Rachel Paresa, Alec Raines, Denny Wang")
      ),
      mainPanel(
        p("Insert paragraph about project description/background.")
      )
    )
  ),
  
  # Create a tab panel for your map
  tabPanel(
    "Map",
    titlePanel("Location of Emergency Services"),
    # Create sidebar layout
    sidebarLayout(
      # Side panel for controls
      sidebarPanel(
        width = 3,
        # Input to select variable to map
        checkboxGroupInput("service", 
                           label = "Emergency Service to Display",
                           choices = list("Hospitals" = "h", "Fire Stations" = "fs"),
                           selected = list("h", "fs")
        ),
        sliderInput("months",
                    label = "Month of 911 Calls to Display",
                    min = 1,
                    max = 12,
                    value = c(1, 12)
        ),
        radioButtons("year",
                     label = "Year of 911 Calls to Display",
                     choices = list("2017" = 17, "2018" = 18),
                     selected = 17
        )
      ),
      
      # Main panel: display plotly map
      mainPanel(
        leafletOutput("map", width = "100%", height = 800)
      )
    )
  ),
  
  # Create a tabPanel to show groupings by neighborhoods
  tabPanel(
    "Neighborhoods",
    # Add a titlePanel to your tab
    titlePanel("Number of 911 Calls by Neighborhoods"),
    # Create a sidebar layout for this tab (page)
    sidebarLayout(
      # Create a sidebarPanel for your controls
      sidebarPanel(
        width = 1,
        checkboxGroupInput("hood_year",
                           label = "Year of 911 Calls to Display",
                           choices = list("2017" = 17, "2018" = 18),
                           selected = 17
        )
      ),
      # Create a main panel, in which you should display your plotly Scatter plot
      mainPanel(
        plotlyOutput("hood", width = "130%", height = 750)
      )
    )
  ),
  
  # Create a tabPanel to show groupings by type of response needed
  tabPanel(
    "Response Types",
    # Add a titlePanel to your tab
    titlePanel("Number of 911 Calls by Types of Response Needed"),
    # Create a sidebar layout for this tab (page)
    sidebarLayout(
      # Create a sidebarPanel for your controls
      sidebarPanel(
        width = 1,
        checkboxGroupInput("type_year",
                           label = "Year of 911 Calls to Display",
                           choices = list("2017" = 17, "2018" = 18),
                           selected = 17
        )
      ),
      # Create a main panel, in which you should display your plotly Scatter plot
      mainPanel(
        plotlyOutput("type", width = "130%", height = 750)
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$map <- renderLeaflet(
    return(services_map(input$service, input$months, input$year))
  )
  output$hood <- renderPlotly(
    return(hood_chart(input$hood_year))
  )
  output$type <- renderPlotly(
    return(type_chart(input$type_year))
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

