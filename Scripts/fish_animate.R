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
library(ggplot2)
library(gganimate)
library(rgeos)
library(gifski)


LW_shp_lemhi <- st_transform(st_read("Data/shapefiles/Henry_LowW.gpkg"), '+proj=longlat +datum=WGS84') 
pittag_data_raw <- read_csv("Data/0LL_cleaned_nov_may")
litz_locs <- read_csv("Data/Litz_Locations.csv")

animate_fish <- pittag_data_raw %>% 
  filter(!node %in% c(1,2,3)) %>%
  mutate(date= as.Date(min_det)) %>%
  filter(between(as.Date(min_det),as.Date("2022-02-01"),as.Date("2022-03-01"))) %>%
  mutate(SC = ifelse(node %in% 4:5,   "HRSC 1",
              ifelse(node %in% 6:7,   "HRSC 2",
              ifelse(node %in% 8:9,   "HRSC 3",
              ifelse(node %in% 10:11, "HRSC 4",
              ifelse(node %in% 12:13, "HRSC 5",
              ifelse(node %in% 14:16, "HRSC 6",
              ifelse(node ==   17,    "HRSC 7",
              ifelse(node ==   18,    "HRSC 8",0))))))))) %>%
  mutate(Complex = ifelse(SC %in% c("HRSC 1", "HRSC 2"), "Upper HRSC","Lower HRSC")) %>%
  mutate(lng = ifelse(Complex == "Upper HRSC", -113.6256, -113.6267  )) %>%
  mutate(lat = ifelse(Complex == "Upper HRSC", 44.8975,44.9)) %>%
  group_by(tag_code) %>% arrange(min_det) 


p <- ggplot() +
 geom_polygon( aes( x = LW_shp_lemhi[[4]][[1]][[1]][[1]][,1], y = LW_shp_lemhi[[4]][[1]][[1]][[1]][,2] ), color = "red" , fill = "red" , alpha = .5) +
 geom_polygon( aes( x = LW_shp_lemhi[[4]][[2]][[1]][[1]][,1], y = LW_shp_lemhi[[4]][[2]][[1]][[1]][,2] ), color = "blue", fill = "blue", alpha = .5) +
 geom_polygon( aes( x = LW_shp_lemhi[[4]][[3]][[1]][[1]][,1], y = LW_shp_lemhi[[4]][[3]][[1]][[1]][,2] ), color = "red" , fill = "red" , alpha = .5) +
 geom_polygon( aes( x = LW_shp_lemhi[[4]][[4]][[1]][[1]][,1], y = LW_shp_lemhi[[4]][[4]][[1]][[1]][,2] ), color = "red" , fill = "red" , alpha = .5) +
 geom_polygon( aes( x = LW_shp_lemhi[[4]][[5]][[1]][[1]][,1], y = LW_shp_lemhi[[4]][[5]][[1]][[1]][,2] ), color = "red" , fill = "red" , alpha = .5) +
 geom_polygon( aes( x = LW_shp_lemhi[[4]][[6]][[1]][[1]][,1], y = LW_shp_lemhi[[4]][[6]][[1]][[1]][,2] ), color = "red" , fill = "red" , alpha = .5) +
 geom_polygon( aes( x = LW_shp_lemhi[[4]][[7]][[1]][[1]][,1], y = LW_shp_lemhi[[4]][[7]][[1]][[1]][,2] ), color = "blue", fill = "blue", alpha = .5) +
 geom_point(data = animate_fish, aes(x=lng, y=lat, color = tag_code), size = 3, 
            position = position_jitter(h=0.0001,w=0.0001)) +
 transition_time(min_det,) +
 labs(title = "Date : {frame_time}", x="Longitude", y="Latitude")


animate(p, renderer = gifski_renderer())


id_vec <- c(rep("fish 1", 10), rep("fish_2", 10),rep("fish_3", 10))
date <- c(rep(seq.Date(as.Date("2022-01-01"),as.Date("2022-01-10"),"day"),3))
Complex_1 <- c(sample(1:2, 10, replace = TRUE),sample(1:2, 10, replace = TRUE),sample(1:2, 10, replace = TRUE))



fish_move <- data.frame(id_vec, date, Complex_1)
fish_move <- fish_move %>% 
  mutate(lng = ifelse(Complex_1 == 1, -113.6256, -113.6267  )) %>%
  mutate(lat = ifelse(Complex_1 == 1,  44.8975 , 44.9)) %>% 
  arrange(date)
  

g <- ggplot() +
  geom_polygon( aes( x = LW_shp_lemhi[[4]][[1]][[1]][[1]][,1], y = LW_shp_lemhi[[4]][[1]][[1]][[1]][,2] ), color = "red" , fill = "red" , alpha = .5) +
  geom_polygon( aes( x = LW_shp_lemhi[[4]][[2]][[1]][[1]][,1], y = LW_shp_lemhi[[4]][[2]][[1]][[1]][,2] ), color = "blue", fill = "blue", alpha = .5) +
  geom_polygon( aes( x = LW_shp_lemhi[[4]][[3]][[1]][[1]][,1], y = LW_shp_lemhi[[4]][[3]][[1]][[1]][,2] ), color = "red" , fill = "red" , alpha = .5) +
  geom_polygon( aes( x = LW_shp_lemhi[[4]][[4]][[1]][[1]][,1], y = LW_shp_lemhi[[4]][[4]][[1]][[1]][,2] ), color = "red" , fill = "red" , alpha = .5) +
  geom_polygon( aes( x = LW_shp_lemhi[[4]][[5]][[1]][[1]][,1], y = LW_shp_lemhi[[4]][[5]][[1]][[1]][,2] ), color = "red" , fill = "red" , alpha = .5) +
  geom_polygon( aes( x = LW_shp_lemhi[[4]][[6]][[1]][[1]][,1], y = LW_shp_lemhi[[4]][[6]][[1]][[1]][,2] ), color = "red" , fill = "red" , alpha = .5) +
  geom_polygon( aes( x = LW_shp_lemhi[[4]][[7]][[1]][[1]][,1], y = LW_shp_lemhi[[4]][[7]][[1]][[1]][,2] ), color = "blue", fill = "blue", alpha = .5) +
  geom_point(data = fish_move, aes(x=lng, y=lat, color = id_vec), size = 3,
             position = position_jitter(h=0.0001,w=0.0001)) +
  transition_time(date) +
  labs(title = "Date : {frame_time}", x="Longitude", y="Latitude")
 
animate(g, renderer = gifski_renderer())



  
         
         
         
         
         


