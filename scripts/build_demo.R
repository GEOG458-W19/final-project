library(dplyr)
library(leaflet)

seattle_18 <- st_read("./data/2018_Fire_Calls_Seattle/2018_Fire_Calls_Seattle.shp")
seattle_17 <- st_read("./data/2017_Fire_Calls_Seattle/2017_Fire_Calls_Seattle.shp")

demo_map <- function(year) {
  # Determines which year of data set to display
  yr_df <- ""
  if (year == 18) {
    yr_df <- seattle_18
  } else {
    yr_df <- seattle_17
  }
  
  # Popup label for the call locations
  yr_label <- paste(
    paste0("Date: ", yr_df$Month, "/", yr_df$Year),
    paste("Address:", yr_df$Address),
    paste("Type:", yr_df$Type),
    sep = "<br/>"
  )
  
  # Creates base map with 911 calls
  p <- leaflet(yr_df) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>% 
    setView(lng = -122.32945, lat = 47.60357, zoom = 12) %>% 
    addCircleMarkers(
      lat = ~yr_df$Latitude,
      lng = ~yr_df$Longitude,
      clusterOptions = markerClusterOptions(),
      color = "browns",
      fillOpacity = 0.5,
      popup = ~yr_label
    ) %>% 
    addLegend("bottomright", 
              colors = list("black"), 
              labels = list("Call Locations"),
              title = "Emergency Calls and Demographics",
              opacity = 1
    )
  
  return(p)
}