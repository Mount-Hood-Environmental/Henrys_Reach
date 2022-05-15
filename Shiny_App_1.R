# Load Packages and Data ------
library(tidyverse)
library(sf)
library(mapview)
library(leaflet)
library(shiny)

litz_locs <- read_csv("data/Litz_Locations.csv")
pittag_data <- read_csv("data/0ll_cleaned_010122_050122.csv")

ui <- fluidPage(
  titlePanel("Henry's Reach Detection Data"),
  fluidRow(
  column(7,
  sliderInput('daterange','Select Date Range', 
              min = as.Date(min(pittag_data$min_det)),
              max = as.Date(max(pittag_data$min_det)),
              value = c(as.Date("2022-01-01"),as.Date("2022-01-20"))),
  plotOutput('bar_graph')
  ),
  
  column(5,
    selectInput('project','Enter Project',choices = c("HRSC","SRSC")),
    leafletOutput('SC_map')
  )
 )
)

server <- function(input,output,session){
  
  output$bar_graph <- renderPlot({
    
    pittag_data_month <- pittag_data %>% 
      mutate(SC = ifelse(node %in% 1:2,   "SRSC 1",  # Create a column that combines nodes into
                  ifelse(node ==   3,     "SRSC 2",  # A single side channel (e.g. node 1&2 = SCSC 1)     
                  ifelse(node %in% 4:5,   "HRSC 1",  
                  ifelse(node %in% 6:7,   "HRSC 2",
                  ifelse(node %in% 8:9,   "HRSC 3",
                  ifelse(node %in% 10:11, "HRSC 4",
                  ifelse(node %in% 12:13, "HRSC 5",
                  ifelse(node %in% 14:16, "HRSC 6",
                  ifelse(node ==   17,    "HRSC 7", 
                  ifelse(node ==   18,    "HRSC 8",0))))))))))) %>%
      mutate(project = ifelse(SC %in% c("SRSC 1", "SRSC 2"), "SRSC","HRSC")) %>%
      filter(between(as.Date(min_det),input$daterange[1],input$daterange[2]) 
             , project == input$project) 
    
    ggplot(pittag_data_month, aes(x=as.Date(min_det), fill = as.factor(SC))) +
      geom_bar() + 
      labs(title = "Daily Detections/Month", x="Day of the Month",
           y = "Number of detections", fill = "Side Channel" ) +
      scale_x_date(date_breaks = "1 day", labels = date_format("%d"),
                   limits = c(input$daterange[1],input$daterange[2]))
  })
 
  output$SC_map <- renderLeaflet({
    
    litz_locs_sc <- litz_locs %>% mutate(Side_Channel = c("SRC 1", "NA", "SRC 2", "HRSC 1", 
                                                          "NA", "HRSC 2", "NA","HRSC 3", "NA",
                                                          "HRSC 4", "NA", "HRSC 5", "NA", "HRSC 6", 
                                                          "NA", "NA", "HRSC 7", "HRSC 8")) %>%
      filter(Side_Channel != "NA")
    
    leaflet(litz_locs_sc) %>%
      addTiles() %>%
      addCircles(lng = ~Longitude, lat = ~Latitude, label = ~Side_Channel,
                 radius = 5,
                 color = "red",
                 labelOptions = labelOptions(noHide = TRUE, offset=c(0,0), textOnly = TRUE,
                                             textsize = "10px",)) 
  })
}
shinyApp(ui=ui,server=server)
