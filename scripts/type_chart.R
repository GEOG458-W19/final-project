library(dplyr)

seattle_18 <- st_read("./data/2018_Fire_Calls_Seattle/2018_Fire_Calls_Seattle.shp")
seattle_17 <- st_read("./data/2017_Fire_Calls_Seattle/2017_Fire_Calls_Seattle.shp")

type_chart <- function(year) {
  p <- ggplot() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_text(vjust=1),
          legend.position = c(1, 1))
  
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
           margin=list(b = 150, l = 100, t = 50),
           legend = list(x = 1, y = 1))
  return(p)
}

##################statistical analysis#####################
#total number of calls for each type
type_calls <-seattle_18 %>% group_by(Type) %>% summarize(total_numb_calls=n()) %>% arrange(desc(total_numb_calls))
#calculates average number of calls per call type. 
average_numb_calls_type <- nrow(seattle_18)/(length(unique(seattle_18$Type)))
#max number of calls per neighborhood
max_calls_type <- type_calls[which.max(type_calls$total_numb_calls),]
#min number of calls per neighborhood
min_calls_type <-type_calls[which.min(type_calls$total_numb_calls),]
#range
range_calls_type <- max_calls_type$total_numb_calls - min_calls_type$total_numb_calls
#median
median_calls_type <- median(type_calls$total_numb_calls)
#calculates top 10 call types with the most calls.
top_10_call_types <- seattle_18 %>% group_by(Type) %>% summarize(total_numb_calls=n()) %>% arrange(desc(total_numb_calls)) %>% head(10) %>% select(Type, total_numb_calls)
