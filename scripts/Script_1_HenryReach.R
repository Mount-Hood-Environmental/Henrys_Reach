# Load Packages and Data ------
library(tidyverse)
library(sf)
library(mapview)
library(leaflet)
litz_locs <- read_csv("data/Litz_Locations.csv")
pittag_data <- read_csv("data/0ll_cleaned_010122_050122.csv")


#Create Plot of Litz Cord Locations 
leaflet(litz_locs) %>%
  addProviderTiles('Esri.WorldGrayCanvas') %>%
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
                   
    
  

 


 



