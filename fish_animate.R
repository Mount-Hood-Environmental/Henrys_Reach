

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


id_vec <- c("fish_1", "fish_1", "fish_2", "fish_2")
date <- as.Date(c("2022-01-01","2022-01-02","2022-01-01","2022-01-02"))
log <- c(-113.6256, -113.6267,-113.6258,-113.6269)
lat <- c(44.8975 , 44.8985,44.8974,44.8985)
fish_move <- data.frame(id_vec , date,  lat, log)

p <- ggplot() +
 geom_polygon( aes( x = LW_shp_lemhi[[4]][[1]][[1]][[1]][,1], y = LW_shp_lemhi[[4]][[1]][[1]][[1]][,2] ), color = "red" , fill = "red" , alpha = .5) +
 geom_polygon( aes( x = LW_shp_lemhi[[4]][[2]][[1]][[1]][,1], y = LW_shp_lemhi[[4]][[2]][[1]][[1]][,2] ), color = "blue", fill = "blue", alpha = .5) +
 geom_polygon( aes( x = LW_shp_lemhi[[4]][[3]][[1]][[1]][,1], y = LW_shp_lemhi[[4]][[3]][[1]][[1]][,2] ), color = "red" , fill = "red" , alpha = .5) +
 geom_polygon( aes( x = LW_shp_lemhi[[4]][[4]][[1]][[1]][,1], y = LW_shp_lemhi[[4]][[4]][[1]][[1]][,2] ), color = "red" , fill = "red" , alpha = .5) +
 geom_polygon( aes( x = LW_shp_lemhi[[4]][[5]][[1]][[1]][,1], y = LW_shp_lemhi[[4]][[5]][[1]][[1]][,2] ), color = "red" , fill = "red" , alpha = .5) +
 geom_polygon( aes( x = LW_shp_lemhi[[4]][[6]][[1]][[1]][,1], y = LW_shp_lemhi[[4]][[6]][[1]][[1]][,2] ), color = "red" , fill = "red" , alpha = .5) +
 geom_polygon( aes( x = LW_shp_lemhi[[4]][[7]][[1]][[1]][,1], y = LW_shp_lemhi[[4]][[7]][[1]][[1]][,2] ), color = "blue", fill = "blue", alpha = .5) +
 geom_point(data = fish_move, aes(x=log, y=lat)) +
 transition_states(date) +
 labs(x="Longitude", y="Latitude")
 
animate(p, renderer = gifski_renderer())


animate_fish <- pittag_data_raw %>% 
  filter(!node %in% c(1,2,3)) %>%
  
         
         
         
         
         


