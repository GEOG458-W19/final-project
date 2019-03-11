installed.packages("leaflet")
library(leaflet)
library(sp)
library(rgdal)
library(dplyr)
library(sf)
#reads csv file
#pop_data <- read.csv("./Population_Density_2017.csv", stringsAsFactors = FALSE)
#reads shapefile
calls <- readOGR("./final-project-master/data/2018_Fire_Calls_Seattle/2018_Fire_Calls_Seattle.shp",
                  layer = "2018_Fire_Calls_Seattle", GDAL1_integer64_policy = TRUE)
calls_df <-st_read("./final-project-master/data/2018_Fire_Calls_Seattle/2018_Fire_Calls_Seattle.shp")
#selects rows in shapefile for column "Type" that are == to "text"
aid_response_select <- subset(calls, Type %in% "Natural Gas Odor")
#counts the number of times each call occurs and selects the top 20
top_20_call_types <- calls_df %>% group_by(Type) %>% summarize(total_numb_calls=n()) %>% arrange(desc(total_numb_calls)) %>% head(2) 
call_top_20_df <- filter(calls_df, Type==unique(top_20_call_types$Type))
#assigns different colors to different categories according to domain. unique() returns 
#list of unique values in the calls shp in the type column
palette_fn <- colorFactor(palette = "Dark2", domain = call_top_20_df[["Type"]])

map <- leaflet(call_top_20_df) %>% 
  addTiles() %>% 
  setView(lng = -122.32945, lat = 47.60357, zoom = 12) %>%
  addCircleMarkers(
    radius=0.1,
    color = ~palette_fn(call_top_20_df[["Type"]])
  )

map

call_types
unique(calls$Type)
