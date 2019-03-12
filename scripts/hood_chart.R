library(dplyr)

seattle_18 <- st_read("./data/2018_Fire_Calls_Seattle/2018_Fire_Calls_Seattle.shp")
seattle_17 <- st_read("./data/2017_Fire_Calls_Seattle/2017_Fire_Calls_Seattle.shp")

h17 <- seattle_17 %>% group_by(S_HOOD) %>% summarise(c17 = n())
h18 <- seattle_18 %>% group_by(S_HOOD) %>% summarise(c18 = n())
hood_names <- h17$S_HOOD
h17_count <- h17$c17
h18_count <- h18$c18
hood <- data.frame(hood_names, h17_count, h18_count) %>% 
        mutate(increase_count = h18_count - h17_count) %>% 
        mutate(total_count = h18_count + h17_count) %>% 
        mutate(increase_perc = increase_count / total_count)
# i_portion <- hood %>% filter(increase_perc > 0) %>% select(increase_count) %>% sum()
# d_portion <- hood %>% filter(increase_perc < 0) %>% select(increase_count) %>% sum()
i_val <- hood %>% filter(increase_perc > 0) %>% nrow()
i_val <- round(i_val / sum(i_val) * 100, digits = 2)
d_val <- hood %>% filter(increase_perc > 0) %>% nrow()
d_val <- round(d_val / sum(d_val) * 100, digits = 2)
hood <- hood %>% mutate(pie_increase = ifelse(increase_perc > 0, increase_perc * i_val, abs(increase_perc * d_val)))

test <- pie(hood$pie_increase, labels = hood$hood_names)

count <- c((hood %>% filter(increase_perc > 0) %>% nrow()), (hood %>% filter(increase_perc < 0) %>% nrow()))
increased <- c("Yes", "No")
pct <- round(count / sum(count) * 100)
lbls <- paste(increased, pct)
lbls <- paste0(lbls, "%")
pie <- pie(count, labels = lbls, main = "Neighborhoods that Increased in 911 Calls from 2017 to 2018")



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