library(sf)

# Reads in the python manipulated and combined shapefiles for the extra column of nearest emergency service mapped to the call
fc2017 <- st_read("./data/FC2017_FS_HOS/FC2017_FS_HOS.shp", stringsAsFactors = FALSE)
fc2018 <- st_read("./data/FC2018_FS_HOS/FC2018_FS_HOS.shp", stringsAsFactors = FALSE)

# Returns a boxplot of each call location's nearest hospital and/or fire station
distance_boxplot <- function(services, year, months) {
  # Determines which year of data set to display
  yr_df <- ""
  if (year == 18) {
    yr_df <- fc2018 %>% filter(Month >= months[1], Month <= months[2])
  } else {
    yr_df <- fc2017 %>% filter(Month >= months[1], Month <= months[2])
  }

  # Create/Manipulate dataframe of distances of emergency services to use for boxplot
  df <- list("Distance" = c(yr_df$FS_Distanc, yr_df$Hos_Distan), 
              "Emergency_Service" = c(rep("Fire Station", nrow(yr_df)), rep("Hospital", nrow(yr_df)))) %>% 
        as.data.frame(stringsAsFactors = F)
    
  
  # Determines if the boxplot should display hospitals, fire stations, or both
  if (length(services) == 1) {
    if (is.element("h", services)) {
      df <- df %>% filter(Emergency_Service == "Hospital")
    }
    if (is.element("fs", services)) {
      df <- df %>% filter(Emergency_Service == "Fire Station")
    }
  } else if (length(services) == 0) {
    # If no emergency service was selected, do not make a chart
    return()
  }
  
  # Create a boxplot of the filtered data set
  g <- ggplot(df, aes(Emergency_Service, Distance)) + geom_boxplot(varwidth = T, fill = "black") + coord_flip()
  g <- g %>% ggplotly() %>% layout(title = paste0("Distance from Calls to Nearest Emergency Service"), 
                                   margin = list(b = 150, l = 100, t = 50),
                                   yaxis = list(title = "Emergency Service"))
  
  return(g)
}


