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
source("./scripts/build_boxplot.R")
source("./scripts/build_demo.R")
source("./scripts/hood_chart.R")
source("./scripts/type_chart.R")

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
                   p("For our final project, we are analyzing the relationship between the type and quantity of acted upon 911 calls within neighborhoods in Seattle and its demographics in terms of population density. Our research question being, Is there a correlation between the type of 911 calls and population density in an area? The topic at hand is very relevant as we can use this data to predict hot spots of different types of incidences. For example, if we are able to locate areas that have observed more 911 calls which require advice life care services and medical attention, the city can address the need and create more ambulance staging areas to improve the efficiency of caring for the residents in those areas. Another example is if an area has observed higher rates of fire calls, the city can see where they can more strategically put the fire departments. The importance of the research question is posed to analyze demographics such as population density."),
                   p("The area we have chosen for analysis is the city of Seattle, the reason being that all of us are familiar with the area and that it is a large city with varying urban and suburban elements. With the large amount and diversity of jobs in Seattle, it is reasonable to consider Seattle is diverse in its demographics. Thus we were curious to analyze the various 911 call types and its relationship to the population density of various neighborhoods in Seattle."),
                   p("Another reason we are focusing on the type of 911 calls and demographics is because there is a vast amount of existing literature on crime, the police, and neighborhood demographics. Various studies showed that "[n]eighborhoods with high crime rates were [] more likely to be subjects of coercive police response[s]" (Sun). Our project will be focusing on population density within the census blocks, but a problem with this data is that such neighborhoods observe higher foot traffic and not necessarily more residents."),
                   p("The existing literature brings to light that certain demographics may be more apt to call 911. A study published by the American College of Emergency Physicians in 2014, concluded that "[t]he fear of becoming involved because of the distrust of law enforcement, a result of possible undocumented status or outstanding warrants, was a common barrier that precluded [the studied] high-risk neighborhood residents from calling 911" (Sasson). This is an important problem in the data that we used, as it may not be a fully accurate representative of the needs of various communities."),
                   p("Another issue with 911 call data we identified was the potential of having false 911 calls. As "each false call must be tracked down by police", our group decided to focus on actually acted on 911 calls and a good majority were by the fire department (The Canadian Press)."),
                   p("The data around the incident locations of fire department related 911 calls, location of fire departments, and location of hospitals comes from the City of Seattle. The format of the data was a CSV file, which updated in real time, with data such as addresses, response type of 911 call, and time. The most common response types were aid responses and medic responses."),
                   p(strong("Aid responses:"), "medical response requiring EMTs (any Seattle firefighters) who are Basic Life Support (BLS) qualified. 
                      Aid units are ambulances staffed with Firefighter/EMTs."),
                   p(strong("Medic responses:"), "medical response requiring Paramedics who are Advanced Life Support (ALS) qualified. 
                      Medic units are ambulances staffed with Firefighter/Paramedics."),
                   p("The demographic data comes from the Census, more specifically the 2010 census population & housing count, as a shapefile. Some of the issues associated with Census Data include race categorization, microdata and statistical confidentiality, as well as interpolated values. Another issue we ran into was that we were hoping to compare multiple years of data, in terms of the population density, but the data had not been collected yet for the 2020 census thus we focused on the population demographics for 2010. We had also looked into using the 5 year American Housing Survey population estimates, but due to insufficient data many records were void or unaccounted for."),
                   p("Thus in this project, we are hoping to delve deeper into the type and quantity of acted upon 911 calls and analyze if there is a correlation to neighborhood demographics. 
")
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
        ),
        p("Overview: 
This map is intended to depict the relationship between the 911 call response locations and the distance to local hospitals and fire stations. Being able to quantify the distance to the nearest hospital or fire station from various call locations will help us see the availability of assistance. For future references, emergency services can compare the average call distances and determine if there needs to be improvement in quantity and location of future emergency services as well as using it as a source to reflect on response times given the average call distances.
          "),
        p("Analysis:
From this analysis of the distances, we have observed a shorter distance from call locations to the nearest fire station compared to hospitals. One possible explanation might be due to the larger quantity of fire stations than hospitals. Another observation was that 90% of neighborhoods had an increase of 911 calls from 2017 to 2018. The remaining 10% had a decrease. 
          ")
      ),
      
      # Main panel: display leaflet map
      mainPanel(
        leafletOutput("map", width = "100%", height = 700),
        plotlyOutput("boxplot", width = "100%", height = 300)
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
        ),
        p("Overview: 
This map describes the relationship of previous analyses but with population density accounted for. The importance of this map is to show the relationship between population density and the number of 911 call locations. 
"),
        p('Analysis: 
This map plots out the calls for "Medic Response", "Trans to AMR", and "Auto Fire Alarm" call types as different colored points. The population density is also mapped based on different census tracts using data from the 2010 census. The calls seems to clustered around higher population density areas like in the downtown area and university district whereas in areas with lower population density like in the industrial district, there are a lot less emergency calls. But there are areas with higher population density but lower number of calls to areas with lower population densities so it is not a very clear correlation. As for the specific call types, there seems to be a lot more fire alarm calls in the downtown area and university district due to higher density of buildings/population. There seems to be no connection between medic response/trans to AMF and population density since the calls seem to be scattered throughout Seattle.
          ')
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
        ),
        p("The most common 911 response types were aid response, medic response, and transfer to American Medical Response. This bar graph brings to light that most of the common acted upon 911 calls are health related calls, calls that need to be addressed as soon as possible. ")
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
        ),
        p("We identified the neighborhoods with the most 911 call response locations being: Belltown, Pioneer Square, and Central Business District. The importance of this graph is to highlight the neighborhoods that have observed higher rates of 911 calls and thus arguably have a higher need. ")
      ),
      # Create a main panel, in which you should display your plotly Scatter plot
      mainPanel(
        plotlyOutput("type", width = "130%", height = 750)
      )
    )
  ),
  tabPanel(
    "Conclusion",
    titlePanel("Conclusion and Sources"),
    p("Conclusion: 
A common issue that we ran into while conducting this analysis was that we had used 911 calls in correlation to population density. This proved to be difficult, as population density is not necessarily representative of total foot traffic in an area. Areas that have observed lower rates of population density may not necessarily mean that the overall foot traffic in that area was low. Where locations such as Downtown with low population density may actually have a high rate of foot traffic because of the workforce in that area. Thus we believe further analysis will need to be done with 911 calls in relation to foot traffic data. 
        "),
    p("Location of hospitals/police stations/fire stations"),
    p("Hospital https://data.seattle.gov/Public-Safety/Hospitals/b39b-ku23"),
    p("Fire Station https://data.seattle.gov/Land-Base/Fire-Stations/mmqu-77bv"),
    p("Police Stations https://data.seattle.gov/Public-Safety/Police-Stations/grvu-i7fa"),
    p("Call data https://data.seattle.gov/Public-Safety/Seattle-Real-Time-Fire-911-Calls/kzjm-xkqj"),
    p("Demographics https://www.census.gov/geo/maps-data/data/tiger-data.html")
  )
)

# Define server logic required to have user interaction and dynamic charts
server <- function(input, output) {
  output$map <- renderLeaflet(
    return(services_map(input$service, input$months, input$year))
  )
  output$boxplot <- renderPlotly(
    return(distance_boxplot(input$service, input$year, input$months))
  )
  output$demo <- renderLeaflet(
    return(build_demo(input$year))
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
