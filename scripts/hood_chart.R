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
    p <- p + geom_bar(seattle_18, mapping = aes(x = Neighborhood, y = Count, fill = "2018"), 
                      stat = "identity", position = "dodge") + guides(fill = guide_legend(title = "Year"))
  }
  
  if (is.element(17, year)){
    seattle_17 <- seattle_17 %>% group_by(S_HOOD) %>% summarise(Count = n())
    colnames(seattle_17)[1] <- "Neighborhood"
    p <- p + geom_bar(seattle_17, mapping = aes(x = Neighborhood, y = Count, fill = "2017"), 
                      stat = "identity", position = "dodge") + guides(fill = guide_legend(title = "Year"))
  }

  p <- ggplotly(p, tooltip = c('x', 'y')) %>%
        layout(title = paste0("911 Call Locations by Neighborhood"),
               margin=list(b = 150, l = 100, t = 50))
  return(p)
}




#####################statistical analysis#######################
pop_data <- read.csv("./Population_Density_2017.csv", stringsAsFactors = FALSE)
#finds total number of calls in each neighborhood
neighborhood_calls <-seattle_18 %>% group_by(S_HOOD) %>% summarize(total_numb_calls=n()) %>% arrange(desc(total_numb_calls))
#calculates average number of calls per neighborhood. 
average_numb_calls <- nrow(seattle_18)/(length(unique(seattle_18$S_HOOD)))
#max number of calls per neighborhood
max_calls_neighborhood <- neighborhood_calls[which.max(neighborhood_calls$total_numb_calls),]
#min number of calls per neighborhood
min_calls_neighborhood <-neighborhood_calls[which.min(neighborhood_calls$total_numb_calls),]
#range
range_calls_neighborhood <- max_calls_neighborhood$total_numb_calls - min_calls_neighborhood$total_numb_calls
#median
median_calls_neighborhood <- median(neighborhood_calls$total_numb_calls)
#calculates top 10 neighborhoods with the most calls.
top_10_neighborhoods <- neighborhood_calls %>% head(10) %>% select(S_HOOD, total_numb_calls)

###########correlation between population ##########
#calculates difference in the number of calls in 2018 to 2017
diff_numb_calls <- nrow(seattle_18) - nrow(seattle_17)
#calculates the percent change in the number of calls from 2017 to 2018
calls_percent_change <- (diff_numb_calls/nrow(seattle_17)) * 100
#calculates difference in total pop from 2017 to 2016
pop_change <- sum(pop_data$Estimated.Total.Population.2017)-sum(pop_data$Estimated.Total.Population.2016)
#calculates percent change in total population from 2016 to 2017
pop_percent_change <- (pop_change/sum(pop_data$Estimated.Total.Population.2016))*100
