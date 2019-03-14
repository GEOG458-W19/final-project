library(dplyr)
library(leaflet)
library(sp)
library(rgdal)
library(sf)

# Read in the shapefiles for each year and population
seattle_18 <- st_read("./data/2018_Fire_Calls_Seattle/2018_Fire_Calls_Seattle.shp", stringsAsFactors = F)
seattle_17 <- st_read("./data/2017_Fire_Calls_Seattle/2017_Fire_Calls_Seattle.shp", stringsAsFactors = F)
pop <- st_read("./data/population_data_shp/population_data/population.shp", stringsAsFactors = F)


# Returns a leaflet of the demographics and location calls based upon the year given
build_demo <- function(year){
  # Determines which year of data set to display
  yr_df <- ""
  if (year == 18) {
    yr_df <- seattle_18
  } else {
    yr_df <- seattle_17 
  } 
  
  # Counts the number of times each call occurs and selects the top 10 and then
  # adds desc() to arrange to go form most calls to lowest
  pop_grouped <- pop %>% group_by(TRACTCE10) %>% summarize(pop=sum(POP10)) %>% arrange(pop)
  top_10_call_types <- yr_df %>% group_by(Type) %>% summarize(total_numb_calls = n()) %>% 
    arrange(desc(total_numb_calls)) %>% head(10) 
  call_top_10_df <- filter(yr_df, Type %in% c("Medic Response","Trans to AMR", "Auto Fire Alarm"))
  
  # Assigns different colors to different categories according to domain. unique() returns 
  # List of unique values in the calls shp in the type column
  colors <- c("#AD1E1E","#F18932","#FFD916","#C7F715","#070406","#6CCCE2","#0F3ECA","#390FCC","#8E01EE","#F5008B")
  palette_fn <- colorFactor(topo.colors(3), call_top_10_df$Type)
  palette_pop <- colorNumeric("Blues", pop_grouped$pop)
  leaflet(data = call_top_10_df) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>% 
    setView(lng = -122.32945, lat = 47.60357, zoom = 12) %>%
    # Adds population density polygons
    addPolygons(
      data = pop_grouped,
      fillColor =~palette_pop(pop),
      color = "black",
      fillOpacity = 1
    ) %>%
    addLegend("bottomright", pal = palette_pop, values = ~pop_grouped$pop, title = "Population Density") %>%
    # Adds call types markers
    addCircleMarkers(
      radius = 2,
      fillColor = ~palette_fn(Type),
      color = "black",
      fillOpacity = 1
    ) %>%
    addLegend("bottomright", pal = palette_fn, values = ~Type,
              title = "Call Types", opacity = 1) 
    
}
