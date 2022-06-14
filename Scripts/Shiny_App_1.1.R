# Authors: Bridger Bertram
# Purpose:  Pit Tag Data visualization of Henry's Reach Project. Lemhi River, Idaho
# Created: May 15
# Last Modified: June 3

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

setwd("~/Desktop/GitHub/Henrys_reach")
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
                              start = "2022-04-20",
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
      
    ggplot(channel_complex %>%
           filter(between(as.Date(min_det),input$daterange[1],input$daterange[2]), 
           Complex == input$Complex) %>% 
           mutate(min_det_scaled = cut.POSIXt(min_det,input$time_frame)), 
           aes(x=as.Date(min_det_scaled), fill = as.factor(SC))) +
      geom_bar(color = "black") + 
      scale_x_date(date_breaks = "1 day" , labels = date_format("%b %d")) +
      labs(x="Date", y = "Number of detections", fill = "Side Channel" ) +
      theme(axis.text = element_text(size=12),
            axis.title = element_text(size = 14, face = "bold"),
            title = element_text(size = 14, face = "bold"))
                 
    
    } else if (input$daterange[2]-input$daterange[1]<=75) {

    ggplot(channel_complex %>%
           filter(between(as.Date(min_det),input$daterange[1],input$daterange[2]),
           Complex == input$Complex) %>%
           mutate(min_det_scaled = cut.POSIXt(min_det,input$time_frame)),
           aes(x=as.Date(min_det_scaled), fill = as.factor(SC))) +
      geom_bar(color = "black") +
      scale_x_date(date_breaks = "1 week" , labels = date_format("%b %d")) +
      labs( x="Date",y = "Number of detections", fill = "Side Channel" ) +
      theme(axis.text = element_text(size=12),
            axis.title = element_text(size = 14, face = "bold"),
            title = element_text(size = 14, face = "bold"))
      
                  
      
    }else{
      
      ggplot(channel_complex %>%
               filter(between(as.Date(min_det),input$daterange[1],input$daterange[2]),
                      Complex == input$Complex) %>%
               mutate(min_det_scaled = cut.POSIXt(min_det,input$time_frame)),
             aes(x=as.Date(min_det_scaled), fill = as.factor(SC))) +
        geom_bar(color = "black") +
        scale_x_date(date_breaks = "1 month" , labels = date_format("%b %d")) +
        labs( x="Date",y = "Number of detections", fill = "Side Channel" ) +
        theme(axis.text = element_text(size=12),
              axis.title = element_text(size = 14, face = "bold"),
              title = element_text(size = 14, face = "bold"))
                     
       
    }
  })

# Fish Movement ----
  
   #In Development
  
# Leaflet Map---- 
  output$SC_map <- renderLeaflet({
    
    if (input$ortho_choice == 'Remove'){

      leaflet(litz_locs %>% mutate(Side_Channel =
                                     c("NA",  "NA"  , "NA", "HRSC 1",  "NA"   ,
                                       "HRSC 2",  "NA"  , "HRSC 3",  "NA"   , "HRSC 4",
                                       "NA"   ,"HRSC 5",  "NA"   , "HRSC 6",  "NA"   ,
                                       "NA"   ,"HRSC 7", "HRSC 8")) %>%
                filter(Side_Channel != "NA") %>%
                mutate(complex = c(rep("Upper HRSC",2),rep("Lower HRSC",6)))%>%
                mutate(Color = c("green","green",rep("blue",6))) %>%
                mutate(sum_col = channel_complex %>% 
                                    filter(!SC %in% c("SRSC 1","SRSC 2")) %>%
                                    filter(between(as.Date(min_det),input$daterange[1],input$daterange[2])) %>% 
                                    count(SC) %>% 
                                    complete(SC = c('HRSC 1','HRSC 2', 'HRSC 3', 'HRSC 4',
                                                    'HRSC 5','HRSC 6', 'HRSC 7', 'HRSC 8'), 
                                             fill = list(n = 0)) %>%
                                    pull(n))) %>%
        
        addProviderTiles('Esri.WorldImagery') %>%
        setView(lng = -113.627, lat = 44.899, zoom = 15.5) %>%
        addCircles(lng = ~Longitude, lat = ~Latitude, label = ~Side_Channel,
                   radius = 5,
                   color =~Color,
                   popup =~paste(    format(input$daterange[1],"%b %d"), "-" ,format(input$daterange[2],"%b %d") ,
                                     "<br/>" ,
                                     "Detections =", sum_col ),
                   labelOptions = labelOptions(noHide = TRUE, offset=c(0,0), textOnly = TRUE,
                                               textsize = "12px",
                                               style = list("color" = "white" ))) %>%
        leaflet::addLegend(labels = ~unique(complex),color = ~unique(Color))

    }else{

    leaflet(litz_locs %>% mutate(Side_Channel =
                                   c("NA",  "NA"  , "NA", "HRSC 1",  "NA"   ,
                                     "HRSC 2",  "NA"  , "HRSC 3",  "NA"   , "HRSC 4",
                                      "NA"   ,"HRSC 5",  "NA"   , "HRSC 6",  "NA"   ,
                                      "NA"   ,"HRSC 7", "HRSC 8")) %>%
              filter(Side_Channel != "NA") %>%
              mutate(complex = c(rep("Upper HRSC",2),rep("Lower HRSC",6)))%>%
              mutate(Color = c("green","green",rep("blue",6))) %>%
              mutate(sum_col = channel_complex %>% 
                                filter(!SC %in% c("SRSC 1","SRSC 2")) %>%
                                filter(between(as.Date(min_det),input$daterange[1],input$daterange[2])) %>% 
                                count(SC) %>%
                                complete(SC = c('HRSC 1','HRSC 2', 'HRSC 3', 'HRSC 4',
                                                'HRSC 5','HRSC 6', 'HRSC 7', 'HRSC 8'), 
                                         fill = list(n = 0)) %>%
                                pull(n))) %>%
        
      addProviderTiles('Esri.WorldImagery') %>%
      addRasterRGB(ortho , na.color = "transparent", r = 1,  g = 2,  b = 3, domain = 3)%>%
      setView(lng = -113.627, lat = 44.899, zoom = 15.5) %>%
      addCircles(lng = ~Longitude, lat = ~Latitude, label = ~Side_Channel,
                 radius = 5,
                 color =~Color,
                 popup = ~paste(  format(input$daterange[1],"%b %d"), "-" ,format(input$daterange[2],"%b %d"),
                                  "<br/>" ,
                                  "Detection =", sum_col ),
                 labelOptions = labelOptions(noHide = TRUE, offset=c(0,0), textOnly = TRUE,
                                             textsize = "12px",
                                             style = list("color" = "black" ))) %>%
      leaflet::addLegend(labels = ~unique(complex),color = ~unique(Color))
    }

  })
  
}
shinyApp(ui=ui,server=server)
