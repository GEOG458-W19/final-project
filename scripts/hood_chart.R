library(dplyr)

seattle_18 <- st_read("./data/2018_Fire_Calls_Seattle/2018_Fire_Calls_Seattle.shp")
seattle_17 <- st_read("./data/2017_Fire_Calls_Seattle/2017_Fire_Calls_Seattle.shp")

hood_chart <- function(year) {
  p <- ggplot() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title = element_text(vjust=1))
  
  # Determines which year of data set to display
  if (is.element(18, year)) {
    seattle_18 <- seattle_18 %>% group_by(S_HOOD) %>% summarise(Count = n())
    colnames(seattle_18)[1] <- "Neighborhood"
    p <- p + geom_point(seattle_18, mapping = aes(x = Neighborhood, y = Count, group = 1, colour = "2018")) +
                geom_line(seattle_18, mapping = aes(x = Neighborhood, y = Count, group = 1, colour = "2018"))
  }
  
  if (is.element(17, year)){
    seattle_17 <- seattle_17 %>% group_by(S_HOOD) %>% summarise(Count = n())
    colnames(seattle_17)[1] <- "Neighborhood"
    p <- p + geom_point(seattle_17, mapping = aes(x = Neighborhood, y = Count, group = 1, colour = "2017")) +
                geom_line(seattle_17, mapping = aes(x = Neighborhood, y = Count, group = 1, colour = "2017"))
  }

  p <- ggplotly(p, tooltip = c('x', 'y')) %>%
        layout(title = paste0("911 Call Locations by Neighborhood"),
               margin=list(b = 150, l = 100, t = 50))
  return(p)
}