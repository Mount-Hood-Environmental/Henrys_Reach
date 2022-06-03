
# Authors: Bridger Bertram
# Purpose: Create Plots before adding to Shiny App
# Created: May 06 
# Last Modified: June 3

#load packages & Data / Modify Data Structure -----
library(tidyverse)
library(sf)
library(mapview)
library(leaflet)
library(leafem)
library(scales)
library(tiff)
library(raster)

litz_locs <- read_csv("Data/Litz_Locations.csv")
pittag_data_raw <- read_csv("Data/0LL_cleaned_nov_may")
ortho <- aggregate((terra::rast('Data/ortho_reduced/Henrys_reduced.tif') %>%
                   raster::brick()), fact = 3)
                   ortho[ortho == 0] <- NA
Lemhi_200_sf <- st_read("Data/shapefiles/Lemhi_200_rch/Lemhi_200.shp")

channel_complex <- pittag_data_raw %>% 
  mutate(SC = ifelse(node %in% 1:2,   "SRSC 1",
              ifelse(node ==     3,   "SRSC 2",
              ifelse(node %in% 4:5,   "HRSC 1",
              ifelse(node %in% 6:7,   "HRSC 2",
              ifelse(node %in% 8:9,   "HRSC 3",
              ifelse(node %in% 10:11, "HRSC 4",
              ifelse(node %in% 12:13, "HRSC 5",
              ifelse(node %in% 14:16, "HRSC 6",
              ifelse(node ==   17,    "HRSC 7",
              ifelse(node ==   18,    "HRSC 8",0))))))))))) %>%
  #Create another column that combines side channels into complexes.         
  mutate(Complex = ifelse(SC %in% c("HRSC 1", "HRSC 2"), "Upper HRSC",
                   ifelse(SC %in% c("SRSC 1", "SRSC 2"),  "SRSC" , "Lower HRSC")))

#Duration Plot HRSC ----

#Each loop runs through every unique individual that enters into their respective complex
#and calculates the total time spent in that complex. Data are saved in "total_time_1" & "total_time_2" 
#for Upper HRSC and Lower HRSC respectively. 

#loop for complex 1
total_time_1 <- c()
for(i in unique(filter(channel_complex, Complex == "Upper HRSC")$tag_code)) {
  
  total_time_1 <- c(total_time_1, 
                    max((channel_complex %>% filter(Complex == "Upper HRSC", tag_code == i))$max_det) - 
                    min((channel_complex %>% filter(Complex == "Upper HRSC", tag_code == i))$min_det))
}

#loop for complex 2
total_time_2 <- c()
for(j in unique(filter(channel_complex, Complex == "Lower HRSC")$tag_code)) {
  
  total_time_2 <- c(total_time_2, 
                    max((channel_complex %>% filter(Complex == "Lower HRSC", tag_code == j))$max_det) - 
                    min((channel_complex %>% filter(Complex == "Lower HRSC", tag_code == j))$min_det))
  
}

#Create ggplot that shows time spent at each complex.

 ggplot(
   
       tibble(.rows = length(c(total_time_1,total_time_2))) %>%
       mutate(tot_time = c(total_time_1,total_time_2),
              complex_vec = c( c(rep("Complex 1",times = length(total_time_1))), 
              c(rep("Complex 2",times = length(total_time_2))))),
              aes(x=tot_time,color=complex_vec)) + 
   
  geom_histogram(bins = 10, fill="white", alpha=0.5, position="identity") +
  geom_vline(aes(xintercept=mean(tot_time)), linetype="dashed") +
  xlab("Days Spent in Project Site")+ 
  ylab("Number of Individuals") 
 
#Duration Plot SRSC ----
 
 total_time_3 <- c()
 for(k in unique(filter(channel_complex, Complex == "SRSC")$tag_code)) {
   
   total_time_3 <- c(total_time_3, 
                     max((channel_complex %>% filter(Complex == "SRSC", tag_code == k))$max_det) - 
                     min((channel_complex %>% filter(Complex == "SRSC", tag_code == k))$min_det))
 } 
 
 ggplot(
   
   tibble(.rows = length(c(total_time_3))) %>%
     mutate(tot_time = c(total_time_3),
            complex_vec = c( rep("SRSC",times = length(total_time_3 )))),
     aes(x=tot_time)) + 
   
   geom_histogram(bins = 10, alpha=0.5, color = "red") +
   geom_vline(aes(xintercept=mean(tot_time)), linetype="dashed") +
   xlab("Days Spent in Project Site")+ 
   ylab("Number of Individuals")

# Leaflet Plot ----
leaflet(litz_locs %>% mutate(Side_Channel = 
               c("SRSC 1",  "NA"  , "SRSC 2", "HRSC 1",  "NA"   , 
                 "HRSC 2",  "NA"  , "HRSC 3",  "NA"   , "HRSC 4", 
                  "NA"   ,"HRSC 5",  "NA"   , "HRSC 6",  "NA"   , 
                  "NA"   ,"HRSC 7", "HRSC 8")) %>%
                filter(Side_Channel != "NA") %>%
                mutate(complex = c("SRSC","SRSC",rep("Upper HRSC",2),rep("Lower HRSC",6)))%>%
                mutate(Color = c("red","red","green","green",rep("blue",6)))) %>%
   addProviderTiles('Esri.WorldImagery') %>%
   addRasterRGB(ortho , na.color = "transparent",
                r = 1, 
                g = 2, 
                b = 3,
                domain = 3) %>%
   setView(lng = -113.627, lat = 44.887, zoom = 13.45) %>%
   addCircles(lng = ~Longitude, lat = ~Latitude, label = ~Side_Channel,
              radius = 5,
              color =~Color, 
              labelOptions = labelOptions(noHide = TRUE, offset=c(0,0), textOnly = TRUE,
                                          textsize = "10px",
                                          style = list("color" = "white" ))) %>%
   addLegend(labels = ~unique(complex),color = ~unique(Color))
 
# Shape File Plot ----
 
 ggplot() + 
   geom_sf(data = Lemhi_200_sf) + 
   ggtitle("Lemhi River") + 
   coord_sf()






