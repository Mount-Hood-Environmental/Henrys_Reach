# Load Packages and Data ------
library(tidyverse)
library(lubridate)
library(sf)
library(mapview)
library(leaflet)
library(leafem)
library(abind)
library(stars)
library(scales)
library(tiff)
library(raster)
library(magrittr)
litz_locs <- read_csv("Data/Litz_Locations.csv")
pittag_data <- read_csv("Data/0LL_cleaned_nov_may")
ortho <- aggregate((terra::rast('Data/ortho_reduced/Henrys_reduced.tif') %>% raster::brick()), fact = 10)


#Create Plot of Litz Cord Locations 
litz_locs_sc <- litz_locs %>% mutate(Side_Channel = c("SRC 1", "NA", "SRC 2", "HRSC 1", "NA", 
                                      "HRSC 2", "NA","HRSC 3", "NA",
                                      "HRSC 4", "NA", "HRSC 5", "NA", "HRSC 6", 
                                      "NA", "NA", "HRSC 7", "HRSC 8")) %>%
                filter(Side_Channel != "NA")



leaflet(litz_locs_sc) %>%
  addProviderTiles('Esri.WorldImagery') %>%
  addRasterRGB(ortho) %>%
    setView(lng = -113.627, lat = 44.887, zoom = 13.45) %>%
  addCircles(lng = ~Longitude, lat = ~Latitude, label = ~Side_Channel,
             radius = 5,
             color = "red",
             labelOptions = labelOptions(noHide = TRUE, offset=c(0,0), textOnly = TRUE,
                                         textsize = "10px",
              style = list(
              "color" = "white"  
              ))) 




#Plot detentions by month -----

#Create new column called month and define the levels from Jan-Apr
#to be used in "facet_wrap" for plots
pittag_data$month <- format(pittag_data$min_det, format = "%b")
pittag_data$month <- factor(pittag_data$month, levels = c("Nov","Dec","Jan","Feb","Mar","Apr"))

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
start <- as.Date("2022-04-01") 
end   <- as.Date("2022-04-14")

pittag_data_month <- pittag_data %>% 
  mutate(SC = ifelse(node %in% 1:2,   "SRSC 1",  # Create a column that combines nodes into
              ifelse(node ==   3,     "SRSC 2",       
              ifelse(node %in% 4:5,   "HRSC 1",  # A single side channel (e.g. node 1&2 = SCSC 1)
              ifelse(node %in% 6:7,   "HRSC 2",
              ifelse(node %in% 8:9,   "HRSC 3",
              ifelse(node %in% 10:11, "HRSC 4",
              ifelse(node %in% 12:13, "HRSC 5",
              ifelse(node %in% 14:16, "HRSC 6",
              ifelse(node ==   17,    "HRSC 7", 
              ifelse(node ==   18,    "HRSC 8",0))))))))))) %>%
  mutate(project = ifelse(SC %in% c("SRSC 1", "SRSC 2"), "SRSC","HRSC")) %>%
  filter(between(as.Date(min_det),start,end) 
         , project == "HRSC")





 ggplot(pittag_data_month, aes(x=as.Date(min_det), fill = as.factor(SC))) +
  geom_bar(position = "dodge", color = "black") + 
  labs(title = "Daily Detections/Month", x="Day of the Month",
       y = "Number of detections", fill = "Side Channel" ) +
  scale_x_date(date_breaks = "1 day", labels = date_format("%d"),
               limits = c(start,end))
 

#Daily Entrance into two S.C. complexes in HR site ---- 
  
pit_tag_data_unfilterd <- read_csv("~/Desktop/Github/Henrys_reach/0LL_cleaned_nov_may")

SCC_daily_pit <- pit_tag_data_unfilterd %>% 
  filter(node %in% c(4:18)) %>% #Filter out data from "SRSC" project
  
  mutate(Side_Channel = ifelse(node %in% 4:5,   "HRSC 1",  #combine nodes into side channels
                               ifelse(node %in% 6:7,   "HRSC 2",  # e.g. node 4&5 correspond to a single                
                                      ifelse(node %in% 8:9,   "HRSC 3",  # Side_Channel "HRSC_1"              
                                             ifelse(node %in% 10:11, "HRSC 4",
                                                    ifelse(node %in% 12:13, "HRSC 5", 
                                                           ifelse(node %in% 14:18, "exit", 0)))))),
         
         Complex =      ifelse(Side_Channel %in% c("HRSC 1","HRSC 2"), "Complex 1",  #An Individual enters a Complex based off
                               ifelse(Side_Channel %in% c("HRSC 3", "HRSC 4", "HRSC 5"),    #which Side_Channel entrance is used
                                      "Complex 2","exit")))

 ggplot(SCC_daily_pit, aes(Side_Complex)) + 
      geom_bar()  
    
  
 
#Plot Fish Paths  -----

pull_tagcode <- SCC_daily_pit %>% pull(tag_code)
unique_tag_code <- unique(pull_tagcode) 
select_indv <- unique_tag_code[23]
fish_path <- filter(SCC_daily_pit,tag_code == select_indv)

ggplot(SCC_daily_pit, aes(x=min_det,y=Side_Channel,color=tag_code,group=tag_code))+
  geom_point()+
  geom_line()+
  theme(legend.position = "none")




