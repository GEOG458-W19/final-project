# Group 5: Justin Han, Wendy Liang, Rachel Paresa, Alec Raines, Denny Wang
# GEOG458
# Final Project: Seattle's 911 Calls

# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(shinythemes)
library(sf)
library(DT)

# Source necessary file scripts
source("./scripts/build_map.R")
source("./scripts/hood_chart.R")
source("./scripts/type_chart.R")
source("./scripts/build_demo.R")
source("./scripts/build_boxplot.R")

# Read in initial data files for the ui/server to run appropriately
types <- read.csv("./data/types-done.csv", stringsAsFactors = FALSE)
seattle_18 <- st_read("./data/2018_Fire_Calls_Seattle/2018_Fire_Calls_Seattle.shp", stringsAsFactors = FALSE)
seattle_17 <- st_read("./data/2017_Fire_Calls_Seattle/2017_Fire_Calls_Seattle.shp", stringsAsFactors = FALSE)
hoods <- seattle_18 %>% group_by(S_HOOD) %>% summarize(n = n())

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
        tabsetPanel(
          tabPanel("Project Desciption", 
                   p(" "),
                   p("For our final project, we are analyzing the relationship between the type and quantity of acted upon 911 calls within neighborhoods in Seattle 
                      and its demographics. Our research question being, Is there a correlation between the type of 911 calls and demographics in an area? 
                      The topic at hand is very relevant as we can use this data to predict hot spots of different types of incidences. For example, 
                      if we are able to locate areas that have observed more 911 calls which require advice life care services and medical attention, 
                      the city can address the need and create more ambulance staging areas to improve the efficiency of caring for the residents in 
                      those areas. Another example is if an area has observed higher rates of fire calls, the city can see where they can more strategically 
                      put the fire departments. The importance of the research question is posed to analyze not only the commonly recognized demographics of 
                      race, but also demographics such as household income level."),
                   p("The area we have chosen for analysis is the city of Seattle, the reason being that all of us are familiar with the area and that it is 
                      a large city with varying urban and suburban elements. With the large amount and diversity of jobs in Seattle, it is reasonable to 
                      consider Seattle is diverse in its demographics. Thus we were curious to analyze the various 911 call types and its relationship to 
                      the demographics of various neighborhoods in Seattle."),
                   p("Another reason we are focusing on the type of 911 calls acted upon and demographics is because there is a vast amount of existing literature 
                      on crime, the police, and neighborhood demographics. Various studies showed that “[n]eighborhoods with high crime rates were [] more 
                      likely to be subjects of coercive police response[s]” (Sun). But an “under-researched factor [was] neighborhood age composition” (Sun). 
                      Thus for our final project we will also be considering age as a demographic."),
                   p("The existing literature brings to light that certain demographics may be more apt to call 911. A study published by the American College 
                      of Emergency Physicians in 2014, concluded that “[t]he fear of becoming involved because of the distrust of law enforcement, a result of 
                      possible undocumented status or outstanding warrants, was a common barrier that precluded [the studied] high-risk neighborhood residents 
                      from calling 911” (Sasson). This is an important problem in the data that we used, as it may not be a fully accurate representative of the 
                      needs of various communities."),
                   p("Another issue with 911 call data we identified was the potential of having false 911 calls. As “each false call must be tracked down 
                      by police”, our group decided to focus on actually acted on 911 calls and a good majority were by the fire department (The Canadian Press)."),
                   p("The data around the incident locations of fire department related 911 calls, location of fire departments, and location of hospitals 
                      comes from the City of Seattle. The format of the data was a CSV file, which updated in real time, with data such as addresses, response 
                      type of 911 call, and time. The most common response types were aid responses and medic responses."),
                   p(strong("Aid responses:"), "medical response requiring EMTs (any Seattle firefighters) who are Basic Life Support (BLS) qualified. 
                      Aid units are ambulances staffed with Firefighter/EMTs."),
                   p(strong("Medic responses:"), "medical response requiring Paramedics who are Advanced Life Support (ALS) qualified. 
                      Medic units are ambulances staffed with Firefighter/Paramedics."),
                   p("Thus in this project, we are hoping to delve deeper into the type and quantity of acted upon 911 calls 
                      and analyze if there is a correlation to neighborhood demographics. ")
                ),
        tabPanel("Citations",
                  p(" "),
                  p("The Canadian Press. \"Alberta Mounties Wasting Time, Resources on Hundreds of False 911 Calls.\"", em("The Canadian Press"), "(Toronto), 2009."),
                  p("Sasson, Comilla, Jason S. Haukoos, Leila Ben-Youssef, Lorenzo Ramirez, Sheana Bull, Brian Eigel, David J. Magid, and Ricardo Padilla.
                    \"Barriers to calling 911 and learning and performing cardiopulmonary resuscitation for residents of primarily Latino, high-risk 
                    neighborhoods in Denver, Colorado.\"", em("Annals of emergency medicine"), "65, no. 5 (2015): 545-552."),
                  p("Sun, Ivan Y., Brian K. Payne, and Yuning Wu. \"The impact of situational factors, officer characteristics, and neighborhood context 
                    on police behavior: A multilevel analysis.\"", em("Journal of criminal justice"), "36, no. 1 (2008): 22-32."))
        )
      )
    )
  ),
  
  # Create a tab panel for metadata
  tabPanel(
    "Metadata",
    titlePanel("Response Types "),
    DT::dataTableOutput("table")
  ),
  
  # Create a tab panel for your map
  tabPanel(
    "Emergency Services Map",
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
                    min = 1, max = 12,
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
        leafletOutput("map", width = "100%", height = 700),
        plotlyOutput("boxplot", width = "100%", height = 300),
        plotlyOutput("boxplot", height = 300)
      )
    )
  ),
  
  # Create a tab panel for your map
  tabPanel(
    "Demographics Map",
    titlePanel("Demographics of Seattle"),
    # Create sidebar layout
    sidebarLayout(
      # Side panel for controls
      sidebarPanel(
        width = 3,
        # Input to select variable to map
        radioButtons("year",
                     label = "Year of 911 Calls to Display",
                     choices = list("2017" = 17, "2018" = 18),
                     selected = 17
        )
      ),

      # Main panel: display plotly map
      mainPanel(
        leafletOutput("demo", width = "100%", height = 800)
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
        width = 2,
        checkboxGroupInput("hood_year",
                           label = "Year of 911 Calls to Display",
                           choices = list("2017" = 17, "2018" = 18),
                           selected = 17
        ),
        selectInput("neighborhood",
                           label = "Neighborhood to Compare Call Increases",
                           choices = as.list(hoods$S_HOOD),
                           selected = "University District"
        )
      ),
      # Create a main panel, in which you should display your plotly Scatter plot
      mainPanel(
        plotlyOutput("hood", width = "120%", height = 750)
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
  output$boxplot <- renderPlotly(
    return(distance_boxplot(input$service, input$year, input$months))
  )
  output$demo <- renderLeaflet(
    return(demo_map(input$year))
  )
  output$hood <- renderPlotly(
    return(hood_chart(input$hood_year, input$neighborhood))
  )
  output$type <- renderPlotly(
    return(type_chart(input$type_year))
  )
  output$table <- DT::renderDataTable(
    types, 
    options = list(
      autoWidth = TRUE, 
      columnDefs = list(list(width = '100px')),
      lengthMenu = c(10, 25, 50, 100, 116)
      )
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
