library(dplyr)

seattle_18 <- st_read("./data/2018_Fire_Calls_Seattle/2018_Fire_Calls_Seattle.shp")
seattle_17 <- st_read("./data/2017_Fire_Calls_Seattle/2017_Fire_Calls_Seattle.shp")

type_chart <- function(year) {
  p <- ggplot() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_text(vjust=1))
  
  # Determines which year of data set to display
  if (is.element(18, year)) {
    seattle_18 <- seattle_18 %>% group_by(Type) %>% summarise(Count = n())
    p <- p + geom_bar(seattle_18, mapping = aes(x = Type, y = Count, fill = "2018"), 
                      stat = "identity", position = "dodge") + guides(fill = guide_legend(title = "Year"))
  }
  
  if (is.element(17, year)){
    seattle_17 <- seattle_17 %>% group_by(Type) %>% summarise(Count = n())
    p <- p + geom_bar(seattle_17, mapping = aes(x = Type, y = Count, fill = "2017"), 
                      stat = "identity", position = "dodge") + guides(fill = guide_legend(title = "Year"))
  }
  
  p <- ggplotly(p, tooltip = c('x', 'y')) %>%
    layout(title = paste0("911 Call Locations by Type"),
           margin=list(b = 150, l = 100, t = 50))
  return(p)
}