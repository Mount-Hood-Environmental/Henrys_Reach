# Load Packages and Data ------
library(tidyverse)
library(lubridate)
library(sf)
library(mapview)
library(leaflet)
library(shiny)

litz_locs <- read_csv("data/Litz_Locations.csv")
pittag_data <- read_csv("data/0ll_cleaned_010122_050122.csv")

pittag_data$month <- format(pittag_data$min_det, format = "%b")
pittag_data$month <- factor(pittag_data$month, levels = c("Jan","Feb","Mar","Apr"))
pittag_data$day <- format(pittag_data$min_det, format = "%d")
pittag_data$day <- type.convert(pittag_data$day)

ui <- fluidPage(
  titlePanel("Henry's Reach Detection Data"),
  fluidRow(
  column(7,
  selectInput('month','Enter Month', choices = c("Jan","Feb","Mar","Apr")),
  plotOutput('bar_graph')
  ),
  
  column(5,
    leafletOutput('SC_map')
  )
 )
)

server <- function(input,output,session){
  
  output$bar_graph <- renderPlot({
    
    pittag_data_month <- pittag_data %>% filter(month == input$month) %>%
      mutate(SC = ifelse(node %in% 1:2,1,  # Create a column that combines nodes into
                  ifelse(node %in% 4:5,2,  # A single side channel (e.g. node 1&2 = SC 1)
                  ifelse(node %in% 6:7,3,
                  ifelse(node %in% 8:9,4,
                  ifelse(node %in% 10:11,5,
                  ifelse(node %in% 12:13,6,0))))))) %>%
      filter(SC != 0)
    
    ggplot(pittag_data_month, aes(x=day, fill = as.factor(SC))) +
      geom_bar() + 
      labs(title = "Daily Detections/Month", x="Day of the Month",
           y = "Number of detections", fill = "Side Channel" ) +
      scale_x_continuous(limits= c(1,31),breaks = seq(1,31, by = 1)) 
  })
 
  output$SC_map <- renderLeaflet({
    
    leaflet(litz_locs) %>%
      addTiles() %>%
      addCircles(lng = ~Longitude, lat = ~Latitude, label = ~Reader,
                 radius = 5,
                 color = "red",
                 labelOptions = labelOptions(noHide = TRUE, offset=c(0,0), textOnly = TRUE,
                                             textsize = "10px",)) 
  })
}
shinyApp(ui=ui,server=server)
