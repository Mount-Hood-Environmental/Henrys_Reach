# Authors: Bridger Bertram
# Purpose:  Pit Tag Data visualization of Henry's Reach Project. Lemhi River, Idaho
# Created: May 15
# Last Modified: May 30 

# Load Packages and Data ------
library(tidyverse)
library(sf)
library(mapview)
library(leaflet)
library(shiny)
library(shinythemes)
library(scales)

setwd("~/Desktop/GitHub/Henrys_reach")
litz_locs <- read_csv("Data/Litz_Locations.csv")
pittag_data_raw <- read_csv("Data/0LL_cleaned_nov_may")

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
ui <- fluidPage(theme = shinytheme("yeti"),
  titlePanel("Henry's Reach Detection Data"),
   fluidRow(
    column(7,
     tabsetPanel(
      tabPanel("HRSC", 
      
       tabsetPanel(type = "tabs",
              
              tabPanel("Daily Dections", 
                               selectInput('Complex','Side Channel Complex',choices = c("Lower HRSC","Upper HRSC")),
                               plotOutput('bar_graph')), 
              tabPanel("Duration",plotOutput('Duration_Plot'))
                
  )
 ),
 tabPanel("SRSC",plotOutput('SRSC_bar_plot'))
 ),
),
 column(5,
        dateRangeInput('daterange','Select Date Range', 
                       start = "2022-03-25",
                       end   = "2022-04-05",
                       min = as.Date(min(pittag_data_raw$min_det)),
                       max = as.Date(max(pittag_data_raw$min_det))),
        leafletOutput('SC_map')
  )
 )
)


server <- function(input,output,session){

#Henry's Reach Daily Detentions ------   
  # This plot describes the daily detentions at each side channel. Data is filtered 
  # based on the date range input. The X-axis is modified with the "if" statement to
  # improve readablity. 
  output$bar_graph <- renderPlot({
    
    channel_complex_daily_plot <- channel_complex %>%
    filter(node %in% c(4:18)) %>% #Filter out data from "SRSC" project
    filter(between(as.Date(min_det),input$daterange[1],input$daterange[2]) 
           , Complex == input$Complex) 
    
    if (input$daterange[2]-input$daterange[1]<=10) { 
    ggplot(channel_complex_daily_plot, aes(x=as.Date(min_det), fill = as.factor(SC))) +
      geom_bar(color = "black") + 
      labs(title = "Daily Detections", x="Date",
           y = "Number of detections", fill = "Side Channel" ) +
      scale_x_date(date_breaks = "1 day" , labels = date_format("%m/%d/%y"),
                   limits = c(input$daterange[1],input$daterange[2]))
    }else{
    ggplot(channel_complex_daily_plot, aes(x=as.Date(min_det), fill = as.factor(SC))) +
      geom_bar(color = "black") + 
      labs(title = "Daily Detections", x="Date",
            y = "Number of detections", fill = "Side Channel" ) +
      scale_x_date(date_breaks = "1 week" , labels = date_format("%m/%d/%y"),
                     limits = c(input$daterange[1],input$daterange[2]))
    }
  })

#Duration Plot -----  
  output$Duration_Plot <- renderPlot({
  
    cc_1 <- channel_complex %>% filter(Complex == "Upper HRSC")
    cc_2 <- channel_complex %>% filter(Complex == "Lower HRSC")  

#Each loop runs through every unique individual that enters into their respective complex
#and calculates the total time spent in that complex. Data are saved in "total_time_1" & "total_time_2" 
#for Complex 1 and Complex 2 respectively.   
    
    #loop for complex 1
    fish_1 <- unique(cc_1$tag_code)
    total_time_1 <- c()
    tag_id_1 <- c()
    for(i in fish_1) {
      track_fish_1 <- cc_1 %>% filter(tag_code == i)
      total_time_1 <- c(total_time_1, max(track_fish_1$max_det) - min(track_fish_1$min_det))
      tag_id_1 <- c(tag_id_1,i)
    }
    
    #loop for complex 2
    fish_2 <- unique(cc_2$tag_code)
    total_time_2 <- c()
    tag_id_2 <- c()
    for(j in fish_2) {
      track_fish_2 <- cc_2 %>% filter(tag_code == j)
      total_time_2 <- c(total_time_2, max(track_fish_2$max_det) - min(track_fish_2$min_det))
      tag_id_2 <- c(tag_id_2,j)
    }

# Create data frame that combines data from loops to be used for ggplot.         
    plot_time <- tibble(.rows = length(c(tag_id_1,tag_id_2))) %>%
      mutate(
        tag_id = c(tag_id_1,tag_id_2),
        tot_time = c(total_time_1,total_time_2),
        complex_vec = c( c(rep("Complex 1",times = length(total_time_1))), 
                         c(rep("Complex 2",times = length(total_time_2)))))

#Plot the time spent by juvenile Chinook in each complex.         
    ggplot(plot_time, aes(x=tot_time,color=complex_vec)) + 
      geom_histogram(bins = 10, fill="white", alpha=0.5, position="identity") +
      geom_vline(data=plot_time, aes(xintercept=mean(tot_time)),
                 linetype="dashed") +
      xlab("Days Spent in Project Site")+ 
      ylab("Number of Individuals") 
  })
 
# SRSC Daily Detections  ----  
  output$SRSC_bar_plot <- renderPlot({
    
    channel_complex_SRSC <- channel_complex %>%
      filter(node %in% c(1:3)) %>% #Filter out data from "HRSC" project
      filter(between(as.Date(min_det),input$daterange[1],input$daterange[2])) 
    
    if (input$daterange[2]-input$daterange[1]<=10) { 
      ggplot(channel_complex_SRSC, aes(x=as.Date(min_det), fill = as.factor(SC))) +
        geom_bar(color = "black") + 
        labs(title = "Daily Detections", x="Date",
             y = "Number of detections", fill = "Side Channel" ) +
        scale_x_date(date_breaks = "1 day" , labels = date_format("%m/%d/%y"),
                     limits = c(input$daterange[1],input$daterange[2]))
    }else{
      ggplot(channel_complex_SRSC, aes(x=as.Date(min_det), fill = as.factor(SC))) +
        geom_bar(color = "black") + 
        labs(title = "Daily Detections", x="Date",
             y = "Number of detections", fill = "Side Channel" ) +
        scale_x_date(date_breaks = "1 week" , labels = date_format("%m/%d/%y"),
                     limits = c(input$daterange[1],input$daterange[2]))
    }
 })
  
# Leaflet Map of Project Site ---- 
  output$SC_map <- renderLeaflet({
    
    litz_locs_sc <- litz_locs %>% mutate(Side_Channel = c("SRC 1", "NA", "SRC 2", "HRSC 1", 
                                                          "NA", "HRSC 2", "NA","HRSC 3", "NA",
                                                          "HRSC 4", "NA", "HRSC 5", "NA", "HRSC 6", 
                                                          "NA", "NA", "HRSC 7", "HRSC 8")) %>%
      filter(Side_Channel != "NA")
    
      leaflet(litz_locs_sc) %>%
        addProviderTiles('Esri.WorldImagery') %>%
        setView(lng = -113.627, lat = 44.887, zoom = 13.45) %>%
        addCircles(lng = ~Longitude, lat = ~Latitude, label = ~Side_Channel,
                   radius = 5,
                   color = "red",
                   labelOptions = labelOptions(noHide = TRUE, offset=c(0,0), textOnly = TRUE,
                                               textsize = "10px",
                   style = list (
                   "color"="white")))
     
  })
}
shinyApp(ui=ui,server=server)
