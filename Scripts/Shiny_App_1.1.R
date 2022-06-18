# Authors: Bridger Bertram
# Purpose:  Pit Tag Data visualization of Henry's Reach Project. Lemhi River, Idaho
# Created: May 15
# Last Modified: June 14

# Load Packages and Data ------
library(tidyverse)
library(sf)
library(mapview)
library(leaflet)
library(leafem)
library(raster)
library(shiny)
library(shinythemes)
library(scales)
library(here)

setwd(here())
litz_locs <- read_csv("Data/Litz_Locations.csv")
pittag_data_raw <- read_csv("Data/0LL_cleaned_nov_may")
ortho <- aggregate((terra::rast('Data/ortho_reduced/Henrys_reduced.tif') %>%
         raster::brick()), fact = 3)
         ortho[ortho == 0] <- NA

# Modify Data structure. Create new column that combines "nodes" in side channels "SC".
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

# ui ----
ui <- fluidPage(theme = shinytheme("spacelab"),
  titlePanel("Henry's Reach"),
   fluidRow(
      column(7,
       tabsetPanel(
         tabPanel("Site Detection", 
                       dateRangeInput('daterange','Select Date Range',
                              start = "2022-03-01",
                              end   = "2022-05-18",
                              min   = as.Date(min(pittag_data_raw$min_det)),
                              max   = as.Date(max(pittag_data_raw$min_det))),
                        fluidRow(
                          column(width = 6, selectInput('Complex','Side Channel Complex',choices = c("Lower HRSC","Upper HRSC"))),
                          column(width = 6,selectInput('time_frame','Detections by...',choices = c("day","week"), selected = "week"))
                                ),  #fluidRow (select inputs)
                        plotOutput('bar_graph')),
         tabPanel("in Development",
                  title = "Fish Movement"
                  ), #tabPanel "fish movement"
              ), #tabsetPanel (main)
             ), #column
 column(width  = 5, 
        offset = 0, 
        radioButtons('ortho_choice', 'Orthomosaic', 
                     choices = c('Display', 'Remove'),
                     inline = TRUE), 
        leafletOutput('SC_map', height = "85vh"))
   
  ), #fluidRow
) #fluidPage

server <- function(input,output,session){

#Detection Data ----    
  output$bar_graph <- renderPlot({
    
    if (input$daterange[2]-input$daterange[1]<=10) {
      x_axis_args <- scale_x_date(date_breaks = "1 day" , labels = date_format("%b %d"))
    } else if (input$daterange[2]-input$daterange[1]<=75) {
      x_axis_args <- scale_x_date(date_breaks = "1 week" , labels = date_format("%b %d"))
    }else{
      x_axis_args <- scale_x_date(date_breaks = "1 month" , labels = date_format("%b %d"))
    }
    
    ggplot(channel_complex %>%
           filter(between(as.Date(min_det),input$daterange[1],input$daterange[2]),
           Complex == input$Complex) %>%
           mutate(min_det_scaled = cut.POSIXt(min_det,input$time_frame)),
           aes(x=as.Date(min_det_scaled), fill = as.factor(SC))) +
      geom_bar(color = "black") +
      x_axis_args + 
      labs(x="Date", y = "Number of detections", fill = "Side Channel",
           caption = "Caption Text") +
      theme(axis.text = element_text(size=14),
            legend.text = element_text(size = 14),
            axis.title = element_text(size = 16, face = "bold"),
            plot.caption = element_text(hjust = 0, size = 14),
            title = element_text(size = 16, face = "bold")) + 
      scale_fill_manual(values = c("#3300CC", "#E69F00", "#56B4E9",
                                   "#F0E442", "#0072B2", "#D55E00"))
  })

# Fish Movement ----
  
   #In Development
  
# Leaflet Map---- 
  output$SC_map <- renderLeaflet({
    
   leaflet_plot_data <- litz_locs %>% mutate(Side_Channel =
                           c("NA",  "NA"  , "NA", "HRSC 1",  "NA"   ,
                             "HRSC 2",  "NA"  , "HRSC 3",  "NA"   , "HRSC 4",
                             "NA"   ,"HRSC 5",  "NA"   , "HRSC 6",  "NA"   ,
                             "NA"   ,"HRSC 7", "HRSC 8")) %>%
      filter(Side_Channel != "NA") %>%
      mutate(complex = c(rep("Upper HRSC",2),rep("Lower HRSC",6)))%>%
      #mutate(Color = c("cyan","cyan",rep("coral",6))) %>%
      mutate(Color = c("#3300CC", "#E69F00", "#3300CC", "#E69F00",
                       "#56B4E9", "#F0E442", "#0072B2", "#D55E00")) %>%
      mutate(sum_col = channel_complex %>%
               filter(!SC %in% c("SRSC 1","SRSC 2")) %>%
               filter(between(as.Date(min_det),input$daterange[1],input$daterange[2])) %>%
               count(SC) %>%
               complete(SC = c('HRSC 1','HRSC 2', 'HRSC 3', 'HRSC 4',
                               'HRSC 5','HRSC 6', 'HRSC 7', 'HRSC 8'),
                        fill = list(n = 0)) %>%
               pull(n))
  
 
  
  if (input$ortho_choice == "Remove") {

    leaflet(leaflet_plot_data) %>%
      addProviderTiles('Esri.WorldImagery') %>%
      setView(lng = -113.627, lat = 44.899, zoom = 15.5) %>%
      addCircles(data = leaflet_plot_data %>% filter(complex == input$Complex),
                 lng = ~Longitude, lat = ~Latitude,
                 radius = 5,
                 color =~Color,
                 opacity = 1,
                 fillOpacity = 1,
                 popup = ~paste(  Side_Channel,
                                  "<br/>",
                                  format(input$daterange[1],"%b %d"), "-" ,format(input$daterange[2],"%b %d"),
                                  "<br/>" ,
                                  "Detection =", sum_col ))

    }else{

    leaflet(leaflet_plot_data) %>%
      addProviderTiles('Esri.WorldImagery') %>%
      addRasterRGB(ortho , na.color = "transparent", r = 1,  g = 2,  b = 3, domain = 3)%>%
      setView(lng = -113.627, lat = 44.899, zoom = 15.5) %>%
      addCircles(data = leaflet_plot_data %>% filter(complex == input$Complex),
                 lng = ~Longitude, lat = ~Latitude,
                 radius = 5,
                 color =~Color,
                 opacity = 1,
                 fillOpacity = 1,
                 popup = ~paste(  Side_Channel,
                                  "<br/>",
                                  format(input$daterange[1],"%b %d"), "-" ,format(input$daterange[2],"%b %d"),
                                  "<br/>" ,
                                  "Detection =", sum_col ))

    }

  })
  
}
shinyApp(ui=ui,server=server)
