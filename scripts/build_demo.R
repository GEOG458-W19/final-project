library(dplyr)
library(leaflet)
library(sp)
library(rgdal)
library(sf)

# Reads csv file
calls <- readOGR("./data/2018_Fire_Calls_Seattle/2018_Fire_Calls_Seattle.shp",
                 layer = "2018_Fire_Calls_Seattle", GDAL1_integer64_policy = TRUE)
calls_df <- st_read("./data/2018_Fire_Calls_Seattle/2018_Fire_Calls_Seattle.shp")
pop <- st_read("./data/population_data/population.shp")
# Read in the shapefiles for each year
seattle_18 <- st_read("./data/2018_Fire_Calls_Seattle/2018_Fire_Calls_Seattle.shp", stringsAsFactors = F)
seattle_17 <- st_read("./data/2017_Fire_Calls_Seattle/2017_Fire_Calls_Seattle.shp", stringsAsFactors = F)
# Returns a leaflet of the demographics and location calls based upon the year given
#demo_map <- function(year) {
  # # Determines which year of data set to display
  # yr_df <- ""
  # if (year == 18) {
  #   yr_df <- seattle_18
  # } else {
  #   yr_df <- seattle_17
  # }
  
  # Popup label for the call locations
  # yr_label <- paste(
  #   paste0("Date: ", yr_df$Month, "/", yr_df$Year),
  #   paste("Address:", yr_df$Address),
  #   paste("Type:", yr_df$Type),
  #   sep = "<br/>"
  # )
  
  # Creates base map with 911 calls
  # p <- leaflet(yr_df) %>% 
  #   addProviderTiles(providers$CartoDB.Positron) %>% 
  #   setView(lng = -122.32945, lat = 47.60357, zoom = 12) %>% 
  #   addCircleMarkers(
  #     lat = ~yr_df$Latitude,
  #     lng = ~yr_df$Longitude,
  #     clusterOptions = markerClusterOptions(),
  #     color = "browns",
  #     fillOpacity = 0.5,
  #     popup = ~yr_label
  #   ) %>% 
  #   addLegend("bottomright", 
  #             colors = list("black"), 
  #             labels = list("Call Locations"),
  #             title = "Emergency Calls and Demographics",
  #             opacity = 1
  #   )
  # creates map
  build_demo <- function(year){
    # Determines which year of data set to display
    yr_df <- ""
    if (year == 18) {
      yr_df <- seattle_18
    } else {
      yr_df <- seattle_17 
    } 
    #counts the number of times each call occurs and selects the top 10. add desc() to arrange to go form most calls to lowest
    pop_grouped <- pop %>% group_by(TRACTCE10) %>% summarize(pop=sum(POP10)) %>% arrange(pop)
    top_10_call_types <- yr_df %>% group_by(Type) %>% summarize(total_numb_calls=n()) %>% arrange(desc(total_numb_calls)) %>% head(10) 
    call_top_10_df <- filter(yr_df, Type %in% c("Medic Response","Trans to AMR", "Auto Fire Alarm"))
    #assigns different colors to different categories according to domain. unique() returns 
    #list of unique values in the calls shp in the type column
    colors <- c("#AD1E1E","#F18932","#FFD916","#C7F715","#070406","#6CCCE2","#0F3ECA","#390FCC","#8E01EE","#F5008B")
    palette_fn <- colorFactor(topo.colors(3), call_top_10_df$Type)
    palette_pop <- colorNumeric("Blues", pop_grouped$pop)
    leaflet(data = call_top_10_df) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addTiles() %>% 
      setView(lng = -122.32945, lat = 47.60357, zoom = 12) %>%
      #adds population density polygons
      addPolygons(
        data=pop_grouped,
        fillColor=~palette_pop(pop),
        color="black",
        fillOpacity=1
      ) %>%
      addLegend("bottomright", pal=palette_pop,values=~pop_grouped$pop,title="Population Density")%>%
      #adds call types markers
      addCircleMarkers(
        radius=2,
        fillColor = ~palette_fn(Type),
        color = "black",
        fillOpacity = 1
      ) %>%
      addLegend("bottomright",pal=palette_fn,values=~Type,
                title = "Call Types", opacity=1) 
    
  }
  
  return(p)
}
