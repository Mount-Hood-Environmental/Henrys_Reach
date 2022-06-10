
# Authors: Bridger Bertram
# Purpose: Create Plots before adding to Shiny App
# Created: May 06 
# Last Modified: June 6

#load packages & Data / Modify Data Structures -----
library(tidyverse)
library(sf)
library(mapview)
library(leaflet)
library(leafem)
library(scales)
library(tiff)
library(raster)
library(dataRetrieval)
library(padr)

#Litz Cord Locations 
litz_locs <- read_csv("Data/Litz_Locations.csv")
#Pit Tag Data from Litz Cords 
pittag_data_raw <- read_csv("Data/0LL_cleaned_nov_may")
#Orthomosaic of Project Site
ortho <- aggregate((terra::rast('Data/ortho_reduced/Henrys_reduced.tif') %>%
                   raster::brick()), fact = 10)
                   ortho[ortho == 0] <- NA
#Lemhi River Basin Shape File 
Lemhi_200_sf <- st_transform(st_read("Data/shapefiles/Lemhi_200_rch/Lemhi_200.shp"), '+proj=longlat +datum=WGS84') 

#Lemhi River Discharge data from USGS (Lemhi River Nr Lemhi, ID)
Lemhi_discharge_raw <- readNWISuv(siteNumbers = "13305000", parameterCd = "00060",startDate = "2022-03-01",endDate = "2022-05-18" )
colnames(Lemhi_discharge_raw)[4]<-"cfs"
Lemhi_discharge <- Lemhi_discharge_raw %>%
                   dplyr::select(dateTime,cfs) %>%
                   mutate(day = as.Date(dateTime, format = "%d"))%>%
                   aggregate(cfs~day,mean)  

  
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
   
   tibble(.rows = length(total_time_3)) %>%
     mutate(tot_time = total_time_3),
     aes(x=tot_time)) + 
   
   geom_histogram(bins = 10, alpha=0.5, color = "red") +
   geom_vline(aes(xintercept=mean(tot_time)), linetype="dashed") +
   xlab("Days Spent in Project Site") + 
   ylab("Number of Individuals")

# Leaflet Plot ----
 
 click_stats <- paste(  "Mar 01 - Apr 18" ,
                          "<br/>" ,
                          "Detections =",
                          sum(filter(channel_complex,between
                            (as.Date(min_det),as.Date("2022-03-01"),as.Date("2022-05-18")))$SC == "HRSC 3"))
 
leaflet(litz_locs %>% mutate(Side_Channel = 
               c("SRSC 1",  "NA"  , "SRSC 2", "HRSC 1",  "NA"   , 
                 "HRSC 2",  "NA"  , "HRSC 3",  "NA"   , "HRSC 4", 
                  "NA"   ,"HRSC 5",  "NA"   , "HRSC 6",  "NA"   , 
                  "NA"   ,"HRSC 7", "HRSC 8")) %>%
                filter(Side_Channel != "NA") %>%
                mutate(complex = c("SRSC","SRSC",rep("Upper HRSC",2),rep("Lower HRSC",6)))%>%
                mutate(Color = c("red","red","green","green",rep("blue",6)))) %>%
   addProviderTiles('Esri.WorldImagery') %>%
   addRasterRGB(ortho , na.color = "transparent",r = 1,g = 2, b = 3, domain = 3) %>%
   setView(lng = -113.627, lat = 44.899, zoom = 15.5) %>%
   addCircles(lng = ~Longitude, lat = ~Latitude, label = ~Side_Channel,
              radius = 5,
              color =~Color, 
              popup = ~click_stats,
              labelOptions = labelOptions(noHide = TRUE, offset=c(0,0), textOnly = TRUE,
                                          textsize = "10px",
                                          style = list("color" = "white" ))) %>%
   leaflet::addLegend(labels = ~unique(complex),color = ~unique(Color))
 
# Shape File Leaflet ----
leaflet(    litz_locs %>% mutate(Side_Channel = 
                                 c("SRSC 1",  "NA"  , "SRSC 2", "HRSC 1",  "NA"   , 
                                   "HRSC 2",  "NA"  , "HRSC 3",  "NA"   , "HRSC 4", 
                                   "NA"   ,"HRSC 5",  "NA"   , "HRSC 6",  "NA"   , 
                                   "NA"   ,"HRSC 7", "HRSC 8")) %>%
            filter(Side_Channel != "NA") %>%
            mutate(complex = c("SRSC","NA","Upper HRSC","NA",rep("NA",5),"Lower HRSC"))%>%
            filter(complex != "NA")) %>%
   
setView(lng = -113.627, lat = 44.887, zoom = 13.45) %>% 
addProviderTiles(providers$CartoDB.Positron) %>%
addPolygons(data = Lemhi_200_sf) %>%
addCircles(lng = ~Longitude, lat = ~Latitude, label = ~complex,
             radius = 10,
             color = 'red',
             labelOptions = labelOptions(noHide = TRUE, offset=c(0,0), textOnly = TRUE,
                                         textsize = "10px"))
 
 
#Daily Detection Plots ----
 
 ggplot( filter(channel_complex,Complex == "Lower HRSC") , aes(x=as.Date(min_det), fill = as.factor(SC))) +
   geom_bar(color = "black") + 
   labs(title = "Daily Detections : Lower HRSC", x="Date",
        y = "Number of detections", fill = "Side Channel") +
   scale_x_date(date_breaks = "1 day" , labels = date_format("%m/%d/%y"),
                limits = c(as.Date("2022-01-01"),as.Date("2022-05-18")))
 
#Weekly Detection Plots (facets) ----
 
 ggplot( channel_complex %>% 
           filter(Complex == "Lower HRSC") %>% 
           mutate(min_det_week = cut.POSIXt(min_det,"week")),
           aes(x=as.Date(min_det_week))) +
   facet_wrap(~SC, scales = "free") + 
   geom_bar(color = "black") + 
   labs(title = "Weekly Detections : Lower HRSC", x="Date",
        y = "Number of detections", fill = "Side Channel" ) +
   scale_x_date(date_breaks = "1 week" , labels = date_format("%m/%d"),
                limits = c(as.Date("2022-01-01"),as.Date("2022-05-18")))

 ggplot( channel_complex %>% 
           filter(Complex == "Upper HRSC") %>% 
           mutate(min_det_week = cut.POSIXt(min_det,"week")),
         aes(x=as.Date(min_det_week))) +
   facet_wrap(~SC, scales = "free") + 
   geom_bar(color = "black") + 
   labs(title = "Weekly Detections : Lower HRSC", x="Date",
        y = "Number of detections", fill = "Side Channel" ) +
   scale_x_date(date_breaks = "1 week" , labels = date_format("%m/%d"),
                limits = c(as.Date("2022-01-01"),as.Date("2022-05-018")))

#Weekly Detection Plots (stacked bars) ----

 ggplot( channel_complex %>% 
           filter(Complex == "Lower HRSC") %>% 
           mutate(min_det_week = cut.POSIXt(min_det,"week")),
         aes(x=as.Date(min_det_week), fill = as.factor(SC))) +
   geom_bar(color = "black") + 
   labs(title = "Weekly Detections : Lower HRSC", x="Date",
        y = "Number of detections", fill = "Side Channel" ) +
   scale_x_date(date_breaks = "1 week" , labels = date_format("%m/%d"),
                limits = c(as.Date("2022-01-01"),as.Date("2022-05-18")))

# Lemhi CFS and Daily Dets ---- 

 #   daily_dets <- as.data.frame(table(
 #     pittag_data_raw %>%
 #       mutate(day = as.Date(min_det, format = "%d")) %>% 
 #       dplyr::select(day) %>% 
 #       filter(between(day, as.Date("2022-03-01"),as.Date("2022-05-18")))
 #   )) 
 # daily_dets[1] <- as.Date(daily_dets$day) 
 # 
 # daily_dets_complete <- daily_dets %>% pad %>%
 #   fill_by_value(Freq)
 # 
 # ggplot(daily_dets, aes( x=day , y=Freq ))+ 
 #  geom_line() 
 # 
 # ggplot(Lemhi_discharge, aes(x = day, y=cfs)) + 
 #   geom_line()
 

 
 
 
 
 