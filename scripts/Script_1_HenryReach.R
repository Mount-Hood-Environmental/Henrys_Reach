# Load Packages and Data ------

library(tidyverse)
library(sf)
library(mapview)
library(leaflet)
litz_locs <- read_csv("data/Litz_Locations.csv")
pittag_data <- read_csv("data/0ll_cleaned_010122_050122.csv")


#Create Plot of Litz Cord Locations 
leaflet(litz_locs) %>%
  addProviderTiles('Esri.WorldImagery') %>%
  addCircles(lng = ~Longitude, lat = ~Latitude, label = ~Reader,
             radius = 15,
             color = "red",
             labelOptions = labelOptions(noHide = TRUE, offset=c(0,0), textOnly = TRUE,
                                         textsize = "15px",)) 


 



