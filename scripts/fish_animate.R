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
library(plotly)

LW_shp_lemhi <- st_transform(st_read("Data/shapefiles/Henry_LowW.gpkg"), '+proj=longlat +datum=WGS84')
HW_shp_lemhi <- st_transform(st_read("Data/shapefiles/Henry_HighW.gpkg"), '+proj=longlat +datum=WGS84')
Fish_Move_Mock <- read_csv("Data/Fish_Move_Mock_Data.csv")
Fish_Move_Mock_Adv <- read_csv("Data/Fish_Move_Mock_Data_Adv.csv")



Fish_Move_Mock_Adv <- Fish_Move_Mock_Adv %>%
  mutate(new_date = as.Date(Date,"%m/%d/%Y")) %>%
  mutate(Frame = as.numeric(format(new_date, "%d")))%>%
  mutate(lng = ifelse(Location == "Lem 1 Release", median(HW_shp_lemhi[[4]][[1]][[1]][[1]][,1]),
               ifelse(Location == "Lower HRSC",    median(HW_shp_lemhi[[4]][[2]][[1]][[1]][,1]+.0005),
               ifelse(Location == "Upper HRSC",    median(HW_shp_lemhi[[4]][[3]][[1]][[1]][,1]),
               ifelse(Location == "Lemhi 3",       median(HW_shp_lemhi[[4]][[4]][[1]][[1]][,1]),
               ifelse(Location == "Lemhi 5",       median(HW_shp_lemhi[[4]][[5]][[1]][[1]][,1]),
               ifelse(Location == "Lemhi 4",       median(HW_shp_lemhi[[4]][[6]][[1]][[1]][,1]-.0001),
               ifelse(Location == "Lemhi 2",       median(HW_shp_lemhi[[4]][[7]][[1]][[1]][,1]),0)))))))) %>%
  
  mutate(lat = ifelse(Location == "Lem 1 Release", median(HW_shp_lemhi[[4]][[1]][[1]][[1]][,2]),
               ifelse(Location == "Lower HRSC",    median(HW_shp_lemhi[[4]][[2]][[1]][[1]][,2]-.0001),
               ifelse(Location == "Upper HRSC",    median(HW_shp_lemhi[[4]][[3]][[1]][[1]][,2]-.0005),
               ifelse(Location == "Lemhi 3",       median(HW_shp_lemhi[[4]][[4]][[1]][[1]][,2]),
               ifelse(Location == "Lemhi 5",       median(HW_shp_lemhi[[4]][[5]][[1]][[1]][,2]),
               ifelse(Location == "Lemhi 4",       median(HW_shp_lemhi[[4]][[6]][[1]][[1]][,2]-.0001),
               ifelse(Location == "Lemhi 2",       median(HW_shp_lemhi[[4]][[7]][[1]][[1]][,2]),0)))))))) %>% 
  select(new_date,Tag,Location,lng,lat,Frame) %>%
  complete(Tag,new_date) %>%
  filter(between(new_date,as.Date("2022-05-03"),as.Date("2022-05-09")))

K <- ggplot() +
  geom_polygon( aes( x = HW_shp_lemhi[[4]][[1]][[1]][[1]][,1], y = HW_shp_lemhi[[4]][[1]][[1]][[1]][,2], text = "Lemhi 1"),
                color = "red"  , fill = "red"  , alpha = .5) +
  geom_polygon( aes( x = HW_shp_lemhi[[4]][[2]][[1]][[1]][,1], y = HW_shp_lemhi[[4]][[2]][[1]][[1]][,2], text = "Lower Heny's Reach Side Channel" ),
                color = "blue" , fill = "blue" , alpha = .5) +
  geom_polygon( aes( x = HW_shp_lemhi[[4]][[3]][[1]][[1]][,1], y = HW_shp_lemhi[[4]][[3]][[1]][[1]][,2], text = "Upper Henr's Reach Side Channel "),
                color = "blue" , fill = "blue" , alpha = .5) +
  geom_polygon( aes( x = HW_shp_lemhi[[4]][[4]][[1]][[1]][,1], y = HW_shp_lemhi[[4]][[4]][[1]][[1]][,2], text = "Lemhi 3"),
                color = "red"  , fill = "red"  , alpha = .5) +
  geom_polygon( aes( x = HW_shp_lemhi[[4]][[5]][[1]][[1]][,1], y = HW_shp_lemhi[[4]][[5]][[1]][[1]][,2], text = "Lemhi 5"),
                color = "red"  , fill = "red"  , alpha = .5) +
  geom_polygon( aes( x = HW_shp_lemhi[[4]][[6]][[1]][[1]][,1], y = HW_shp_lemhi[[4]][[6]][[1]][[1]][,2], text = "Lemhi 4"),
                color = "red"  , fill = "red"  , alpha = .5) +
  geom_polygon( aes( x = HW_shp_lemhi[[4]][[7]][[1]][[1]][,1], y = HW_shp_lemhi[[4]][[7]][[1]][[1]][,2], text = "Lemhi 2"),
                color = "red"  , fill = "red"  , alpha = .5) +
  geom_point(data = Fish_Move_Mock_Adv, 
             aes(x=lng, y=lat, color=Tag, frame = format(new_date, "%b %d")),
             size = 3, position = position_jitter(h=0.0002,w=0.0002)) + 
  labs(x="Longitude",y="Latitude")+
  theme(legend.position = "none")


   ggplotly(K) %>% 
   animation_opts(frame = 2000, 
                  transition = 1500, 
                  easing = 'linear') %>% 
   animation_slider( currentvalue = list(prefix = "Date : " , 
                     font = list(color = "black")))
 


    

 
   




 

  
         
       
         
         
         


