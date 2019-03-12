library(dplyr)
library(leaflet)

fire_stat_df <- read.csv("./data/Fire_Stations.csv", stringsAsFactors = FALSE)
colnames(fire_stat_df)[1] <- "lng"
colnames(fire_stat_df)[2] <- "lat"
hospital_df <- read.csv("./data/Hospitals.csv", stringsAsFactors = FALSE)
colnames(hospital_df)[1] <- "lng"
colnames(hospital_df)[2] <- "lat"
# seattle <- st_read("./data/seattlefc/seattlefc.shp") %>% top_n(1000)
seattle_18 <- st_read("./data/2018_Fire_Calls_Seattle/2018_Fire_Calls_Seattle.shp")
seattle_17 <- st_read("./data/2017_Fire_Calls_Seattle/2017_Fire_Calls_Seattle.shp")

hsptl_label <- paste(
  hospital_df$FACILITY,
  hospital_df$ADDRESS,
  sep = "<br/>"
)

sfd_label <- paste(
  fire_stat_df$STNID,
  fire_stat_df$ADDRESS,
  sep = "<br/>"
)

services_map <- function(services, months, year) {
  # Determines which year of data set to display
  yr_df <- ""
  if (year == 18) {
    yr_df <- seattle_18 %>% filter(Month >= months[1], Month <= months[2])
  } else {
    yr_df <- seattle_17 %>% filter(Month >= months[1], Month <= months[2])
  }
  
  # Popup label for the call locations
  yr_label <- paste(
    paste0("Date: ", yr_df$Month, "/", yr_df$Year),
    paste("Address:", yr_df$Address),
    paste("Type:", yr_df$Type),
    sep = "<br/>"
  )

  # Creates base map with 911 calls
  p <- leaflet(fire_stat_df) %>% 
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
              colors = list("black", "red", "blue"), 
              labels = list("Call Locations", "Seattle Fire Dept. (SFD)", "Hospitals"),
              title = "Emergency Calls and Services",
              opacity = 1
    )
  
  # Determines if the map should display hospitals, fire stations, or both
  if (is.element("h", services)) {
      p <- p %>% addCircles(
                    lat = ~hospital_df$lat,
                    lng = ~hospital_df$lng,
                    color = "blue",
                    fillOpacity = 1,
                    popup = ~hsptl_label
                  )
  }
  
  if (is.element("fs", services)) {
      p <- p %>% addCircles(
                    lat = ~fire_stat_df$lat,
                    lng = ~fire_stat_df$lng,
                    color = "red",
                    fillOpacity = 1,
                    popup = ~sfd_label
                  )
  }
  
  return(p)
}