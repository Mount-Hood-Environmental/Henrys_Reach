# Load Packages and Data ------
library(tidyverse)
library(sf)
library(mapview)
library(leaflet)
litz_locs <- read_csv("data/Litz_Locations.csv")
pittag_data <- read_csv("data/0ll_cleaned_010122_050122.csv")


#Create Plot of Litz Cord Locations 
leaflet(litz_locs) %>%
  addTiles() %>%
  #addProviderTiles('Esri.WorldGrayCanvas') %>%
  addCircles(lng = ~Longitude, lat = ~Latitude, label = ~Reader,
             radius = 5,
             color = "red",
             labelOptions = labelOptions(noHide = TRUE, offset=c(0,0), textOnly = TRUE,
                                         textsize = "10px",)) 

#Plot detentions by month -----

#Create new column called month and define the levels from Jan-Apr
#to be used in "facet_wrap" for plots
pittag_data$month <- format(pittag_data$min_det, format = "%b")
pittag_data$month <- factor(pittag_data$month, levels = c("Jan","Feb","Mar","Apr"))

#Create two data frames to split up Nodes 1-3 & Nodes 4-18
pittag_data_nodes_1_3 <- pittag_data %>%
  filter(node %in% c(1,2,3))
pittag_data_nodes_4_18 <- pittag_data %>%
  filter(node %in% c(4:18))

#Plot Detection for nodes 1-3
ggplot(pittag_data_nodes_1_3, aes(x=node)) +
  geom_bar() + 
  facet_wrap(~month, scales = "free")+ 
  labs(title = "Detections for Nodes 1 - 3", y = "Number of Detections") + 
  scale_x_discrete(limit = c("Node 1", "Node 2", "Node 3"))

#Plot Detection for nodes 4-18
ggplot(pittag_data_nodes_4_18, aes(x=node)) +
  geom_bar() + 
  facet_wrap(~month, scales = "free") +
  labs(title = "Detections for Nodes 4 - 18", y = "Number of Detections", 
               x= "Node Number") + 
  scale_x_continuous(limits= c(4,18),breaks = seq(4,18, by = 1))
  
  
#Plot Detection by Node ----
 ggplot(pittag_data, aes(x=month)) +
    geom_bar() + 
    facet_wrap(~node, scales = "free") +
    labs(title = "Detections for all sites Jan-Apr", y = "Number of Detections", 
         x= "Month") +
    scale_x_discrete(limit = c("Jan","Feb","Mar","Apr"))
                   
#Daily Detection ---------------------

pittag_data_month <- pittag_data %>% 
  mutate(SC = ifelse(node %in% 1:2,1,  # Create a column that combines nodes into
              ifelse(node %in% 4:5,2,  # A single side channel (e.g. node 1&2 = SC 1)
              ifelse(node %in% 6:7,3,
              ifelse(node %in% 8:9,4,
              ifelse(node %in% 10:11,5,
              ifelse(node %in% 12:13,6,0))))))) %>%
  filter(SC != 0)
pittag_data_month$day <- format(pittag_data_month$min_det, format = "%d")
pittag_data_month$day <- type.convert(pittag_data_month$day)


 ggplot(pittag_data_month, aes(x=day, fill = as.factor(SC))) +
  geom_bar() + 
  facet_wrap(~month, scales = "free") +
  labs(title = "Daily Detections/Month", x="Day of the Month",
       y = "Number of detections", fill = "Side Channel" ) +
  scale_x_continuous(limits= c(1,31),breaks = seq(1,31, by = 1))
 
 
  
  

 


 



