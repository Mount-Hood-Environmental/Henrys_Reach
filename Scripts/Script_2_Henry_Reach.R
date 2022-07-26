
# Authors: Bridger Bertram
# Purpose: Create Plots before adding to Shiny App
# Created: May 06 
# Last Modified: June 14

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
library(here)
library(leafpop)
library(glue)
library(leaflegend)
library(matrixStats)
library(plotly)
library(zoo)

setwd(here())
#Litz Cord Locations 
litz_locs <- read_csv("Data/Litz_Locations.csv")
#Pit Tag Data from Litz Cords 
pittag_data_raw <- read_csv("Data/0LL_cleaned_nov_may")
#Orthomosaic of Project Site
ortho <- aggregate((terra::rast('Data/ortho_reduced/Henrys_reduced.tif') %>%
                   raster::brick()), fact = 10)
                   ortho[ortho == 0] <- NA
                   
ortho_spring <- aggregate((terra::rast('Data/ortho_reduced/Henrys_reduced_spring_22.tif') %>%
                   raster::brick()), fact = 10)
                   ortho_spring[ortho_spring == 0] <- NA
                   
#Lemhi River Basin Shape File 
Lemhi_200_sf <- st_transform(st_read("Data/shapefiles/Lemhi_200_rch/Lemhi_200.shp"), '+proj=longlat +datum=WGS84') 
HW_shp_lemhi <- st_transform(st_read("Data/shapefiles/Henry_HighW.gpkg"), '+proj=longlat +datum=WGS84') 
LW_shp_lemhi <- st_transform(st_read("Data/shapefiles/Henry_LowW.gpkg"), '+proj=longlat +datum=WGS84') 

#Lemhi River Discharge data from USGS (Lemhi River Nr Lemhi, ID)
Lemhi_discharge_raw <- readNWISuv(siteNumbers = "13305000", parameterCd = "00060",startDate = "2022-03-01",endDate = "2022-05-18" )
colnames(Lemhi_discharge_raw)[4]<-"cfs"
Lemhi_discharge <- Lemhi_discharge_raw %>%
                   dplyr::select(dateTime,cfs) %>%
                   mutate(day = as.Date(dateTime, format = "%d"))%>%
                   aggregate(cfs~day,mean)  

USGS_Stream_Data <- read_csv("Data/LemL5_Flow_WY.csv") 

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
 
 leaflet_popup_graphs  <- channel_complex %>%
   filter(between(as.Date(channel_complex$min_det),as.Date("2022-05-17"),as.Date("2022-05-18"))) %>%
   group_by(SC) %>%
   nest() %>% 
   filter(!SC %in% c("SRSC 1", "SRSC 2")) %>%
  
   mutate(ggs = purrr :: map2(
     data, SC,
     
     ~ ggplot(data = .x %>% 
                group_by(date = as.Date(min_det), SC ) %>% 
                summarise(n=n()) %>% 
                filter(!SC %in% c("SRSC 1","SRSC 2")), 
                aes( x = date , y = cumsum(n))) + 
       
       ggtitle(glue("Henry's Reach Side Channel {.y}")) +
       geom_line() + geom_point()+
       labs( x = "Date", y = "Cumulative Detections") + 
       scale_x_date(date_breaks = "1 month" , labels = date_format("%b %d")) + 
       theme(axis.text = element_text(size=14),
             axis.title = element_text(size = 16, face = "bold"),
             plot.caption = element_text(hjust = 0, size = 14),
             title = element_text(size = 16, face = "bold"))
   )) %>%
   slice(match(c("HRSC 1","HRSC 2","HRSC 3","HRSC 4",
                 "HRSC 5","HRSC 6","HRSC 7","HRSC 8"),SC)) 
   
 if ( length(leaflet_popup_graphs$SC) < 8) {
   leaflet_popup_graphs[nrow(leaflet_popup_graphs) + 1, ] <- c(SC = NA ,data = NA ,ggs = NA )}
 
 if ( length(leaflet_popup_graphs$SC) < 8) {
   leaflet_popup_graphs[nrow(leaflet_popup_graphs) + 1, ] <- c(SC = NA ,data = NA ,ggs = NA )}
 
 if ( length(leaflet_popup_graphs$SC) < 8) {
   leaflet_popup_graphs[nrow(leaflet_popup_graphs) + 1, ] <- c(SC = NA ,data = NA ,ggs = NA )}

 
    
 leaflet_plot_data <-litz_locs %>%
   mutate(Side_Channel = 
           c("NA"   ,  "NA"  ,  "NA"   , "HRSC 1",  "NA"   , 
            "HRSC 2",  "NA"  , "HRSC 3",  "NA"   , "HRSC 4", 
             "NA"   ,"HRSC 5",  "NA"   , "HRSC 6",  "NA"   , 
             "NA"   ,"HRSC 7", "HRSC 8")) %>%
   filter(Side_Channel != "NA" ) %>%
   mutate(complex = c(rep("Upper HRSC",2),rep("Lower HRSC",6)))%>%
   mutate(Color = c("cyan","cyan",rep("coral",6))) %>%
   mutate(plots_id = leaflet_popup_graphs$SC ) %>%
   mutate(plots = leaflet_popup_graphs$ggs )


   
  leaf_plot <- leaflet(leaflet_plot_data) %>%
   addProviderTiles('Esri.WorldImagery') %>%
   addRasterRGB(ortho_spring , na.color = "transparent",r = 1,g = 2, b = 3, domain = 3) %>%
   setView(lng = -113.627, lat = 44.8995, zoom = 17)


if (any(is.na(leaflet_plot_data) == TRUE)) {
  leaf_plot %>%
  addCircles(data = leaflet_plot_data,
             lng = ~Longitude, lat = ~Latitude,
             radius = 2,
             color =~Color,
             opacity = 1,
             fillOpacity = 1,
             label = ~Side_Channel,
             labelOptions = labelOptions(noHide = TRUE,
                                         direction = "bottom",
                                         textsize = "12px",
                                         style = list("color" = "black" )))
} else {
  leaf_plot %>%
    addCircles(data = leaflet_plot_data,
               lng = ~Longitude, lat = ~Latitude,
               radius = 2,
               color =~Color,
               opacity = 1,
               fillOpacity = 1,
               label = ~Side_Channel,
               labelOptions = labelOptions(noHide = TRUE,
                                           direction = "bottom",
                                           textsize = "12px",
                                           style = list("color" = "black" )),
               
               popup = popupGraph(leaflet_plot_data$plots,
                                  width = 550,
                                  height = 250))
}


 
# Shape File Leaflet ----

 leaflet() %>%
   addProviderTiles('Esri.WorldImagery') %>%
   addRasterRGB(ortho, na.color = "transparent",r = 1,g = 2, b = 3, domain = 3) %>%
   addPolygons(data = LW_shp_lemhi %>% mutate(color = c("red", "blue", rep("red",4), "blue")),
               color = ~color ) %>%
   setView(lng = -113.627, lat = 44.8995, zoom = 17) %>%
   addCircles(data = litz_locs,
              lng = ~Longitude, lat = ~Latitude,
              radius = 2,
              color ="red",
              opacity = 1,
              fillOpacity = 1)
              
 
#Daily Detection Plots ----
 
 x_axis_by_day <- scale_x_date(date_breaks = "1 week" , labels = date_format("%b %d"))
 
 ggplot( channel_complex %>% 
           filter(Complex == "Lower HRSC",
                  between(as.Date(min_det),as.Date("2022-04-20"),as.Date("2022-05-18"))), 
         
         aes(x=as.Date(min_det), fill = as.factor(SC))) +
   geom_bar(color = "black") + 
   x_axis_by_day + 
   #scale_x_date(date_breaks = "1 week" , labels = date_format("%b %d")) + 
   labs(title = "Daily Detections : Lower HRSC", x="Date",
        y = "Number of detections", fill = "Side Channel") +
   theme(axis.text = element_text(size=12),
         axis.title = element_text(size = 14, face = "bold"),
         title = element_text(size = 14, face = "bold"))
 
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

# Lemhi CFS ---- 

 USGS_Stream_Data_filtered <- USGS_Stream_Data%>% 
   mutate(date = USGS_Stream_Data$...1) %>%
   filter(!date %in% c("Min","Q1","Median","Q3","Max")) %>%
   mutate(real_date = as.Date(date,format = "%b-%d")) %>%
   dplyr::select(real_date,Min,Q1,Median,Q3,Max) %>%
   gather(key = "stat", value = "cfs", -real_date)
 
 USGS_Stream_Data_filtered_2 <- USGS_Stream_Data%>% 
   mutate(date_2 = USGS_Stream_Data$...1) %>%
   filter(!date_2 %in% c("Min","Q1","Median","Q3","Max")) %>%
   mutate(real_date_2 = as.Date(date_2,format = "%b-%d")) %>%
   dplyr::select('1996',real_date_2) %>%
   gather(key = "stat_2", value = "cfs_2", -real_date_2) 
 
 USGS_Stream_Data_filtered <- na.locf(USGS_Stream_Data_filtered)
 USGS_Stream_Data_filtered_2 <- na.locf(USGS_Stream_Data_filtered_2)
 
 USGS_Stream_Data_filtered$stat <- factor( USGS_Stream_Data_filtered$stat, 
    levels = c("Max", "Q3", "Median", "Q1", "Min"))
 

 gg_cfs <- ggplot() +
   geom_line(data = USGS_Stream_Data_filtered, aes(x = real_date, y = cfs, color = stat)) +
    geom_ribbon(aes(  x   = USGS_Stream_Data_filtered%>%filter(stat=="Q1")  %>% pull(real_date),
                      ymin = USGS_Stream_Data_filtered%>%filter(stat=="Q1")  %>% pull(cfs),
                      ymax = USGS_Stream_Data_filtered%>%filter(stat=="Q3")  %>% pull(cfs)), 
                fill = "cyan", alpha = .3) + 
    geom_ribbon(aes(  x   = USGS_Stream_Data_filtered%>%filter(stat=="Q1")  %>% pull(real_date),
                      ymin = USGS_Stream_Data_filtered%>%filter(stat=="Q3")  %>% pull(cfs),
                      ymax = USGS_Stream_Data_filtered%>%filter(stat=="Max") %>% pull(cfs)), 
                fill = "coral", alpha = .3) +
    geom_ribbon(aes(  x   = USGS_Stream_Data_filtered%>%filter(stat=="Q1")  %>% pull(real_date),
                      ymin = USGS_Stream_Data_filtered%>%filter(stat=="Min") %>% pull(cfs),
                      ymax = USGS_Stream_Data_filtered%>%filter(stat=="Q1")  %>% pull(cfs)), 
                fill = "coral", alpha = .3) +
   scale_color_manual(values = c("coral","cyan","black","cyan","coral"), 
                      labels = c("Max", "75th Quartile", "Median ('79-'22)", "25th Quartile", "Min"))+
   scale_x_date(date_breaks = "1 month" , labels = date_format("%b %d")) +
   labs(x = "Date", y = "Discharge (cfs)", title = "Lemhi River - L5 (USGS #13305310)") +
   theme_minimal() +
   theme(axis.text = element_text(size=16),
         legend.text = element_text(size = 16),
         legend.key.size = unit(1.75, 'cm'),
         legend.title = element_blank(),
         axis.title = element_text(size = 16, face = "bold"),
         plot.caption = element_text(hjust = 0, size = 14),
         title = element_text(size = 16, face = "bold"),
         plot.title = element_text(hjust = 0.5))
 
   gg_cfs <- gg_cfs + 
   new_scale_color() +
   geom_line(data = USGS_Stream_Data_filtered_2,
             aes(x=real_date_2, y=cfs_2, color=stat_2), size = 1.5)
  
 gg_cfs
 
#Plot_ly cfs---- 
input <- c('1996','2021') 

 USGS_plotly_data <- USGS_Stream_Data%>% 
   mutate(date = USGS_Stream_Data$...1) %>%
   filter(!date %in% c("Min","Q1","Median","Q3","Max")) %>%
   mutate(real_date = as.Date(date,format = "%b-%d")) %>%
   dplyr::select(real_date,Min,Q1,Median,Q3,Max,input) %>%
   arrange(real_date)
 
 USGS_plotly_data<- na.locf(USGS_plotly_data)
 

 
 ploty_cfs <- plot_ly(data = USGS_plotly_data, x = ~real_date)
 for(i in 1:length(input)){
   ploty_cfs <- add_trace(ploty_cfs, y = pull(USGS_plotly_data,input[i]), type = 'scatter', mode = 'lines', name = input[i]) }
 
 ploty_cfs %>%
   add_ribbons(ymin = ~Min, ymax = ~Q1 , fillcolor = "coral", opacity = .5, line = list(color = 'rgba(0, 0, 0, 0)'),
               showlegend = FALSE) %>%
   add_ribbons(ymin = ~Q1 , ymax = ~Q3 , fillcolor = "cyan" , opacity = .5, line = list(color = 'rgba(0, 0, 0, 0)'),
               showlegend = FALSE)  %>%
   add_ribbons(ymin = ~Q3 , ymax = ~Max, fillcolor = "coral", opacity = .5, line = list(color = 'rgba(0, 0, 0, 0)'),
               showlegend = FALSE) %>%
   add_trace(y = ~Min,    mode = 'lines', type = 'scatter', name = 'Min',
             line = list(color = 'coral')) %>%
   add_trace(y = ~Q1,     mode = 'lines', type = 'scatter', name = 'Q1',
             line = list(color = 'cyan')) %>%
   add_trace(y = ~Median, mode = 'lines', type = 'scatter', name = 'Median',
             line = list(color = 'black')) %>%
   add_trace(y = ~Q3,     mode = 'lines', type = 'scatter', name = 'Q3',
             line = list(color = 'cyan')) %>%
   add_trace(y = ~Max,    mode = 'lines', type = 'scatter', name = 'Max',
             line = list(color = 'coral')) 

  USGS_plotly_data_2 <- USGS_Stream_Data%>% 
    mutate(date = USGS_Stream_Data$...1) %>%
    filter(!date %in% c("Min","Q1","Median","Q3","Max")) %>%
    mutate(real_date = as.Date(date,format = "%b-%d")) %>%
    dplyr::select(real_date_2,"1996") %>%
    arrange(real_date)
 
 