# Authors: Bridger Bertram
# Purpose:  Pit Tag Data visualization of Henry's Reach Project. Lemhi River, Idaho
# Created: May 15
# Last Modified: June 24

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
library(glue)
library(leafpop)


setwd(here())
litz_locs <- read_csv("Data/Litz_Locations.csv")
pittag_data_raw <- read_csv("Data/0LL_cleaned_nov_may")

res_of_ortho <- 10

ortho_fall <- aggregate((terra::rast('Data/ortho_reduced/Henrys_reduced.tif') %>%
         raster::brick()), fact = res_of_ortho)
         ortho_fall[ortho_fall == 0] <- NA
         
ortho_spring <- aggregate((terra::rast('Data/ortho_reduced/Henrys_reduced_spring_22.tif') %>%
        raster::brick()), fact = res_of_ortho)
        ortho_spring[ortho_spring == 0] <- NA
        
 HW_shp_lemhi <- st_transform(st_read("Data/shapefiles/Henry_HighW.gpkg"), '+proj=longlat +datum=WGS84') 
 LW_shp_lemhi <- st_transform(st_read("Data/shapefiles/Henry_LowW.gpkg"), '+proj=longlat +datum=WGS84')                 

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
  mutate(Complex = ifelse(SC %in% c("HRSC 1", "HRSC 2"), "Upper HRSC",
                   ifelse(SC %in% c("SRSC 1", "SRSC 2"),  "SRSC" , "Lower HRSC")))

# ui ----
ui <- fluidPage(theme = shinytheme("spacelab"),
  titlePanel("Henry's Reach"),

       tabsetPanel(
         tabPanel("Site Detection", 
           fluidRow(
            column(width = 7,        
                       dateRangeInput('daterange','Select Date Range',
                              start = "2022-01-01",
                              end   = "2022-05-18",
                              min   = as.Date(min(pittag_data_raw$min_det)),
                              max   = as.Date(max(pittag_data_raw$min_det))),
                        fluidRow(
                          column(width = 4, "Upper Henry's Reach" ,
                                 checkboxGroupInput('Complex_1',label = NULL,choices = c("HRSC 1","HRSC 2"), 
                                                    selected = c("HRSC 1","HRSC 2"))),
                          
                          column(width = 4, "Lower Henry's Reach",
                                 
                              fluidRow(column(6,
                                           checkboxGroupInput('Complex_2_1', label = NULL, choices = c("HRSC 3","HRSC 4","HRSC 5"), 
                                            selected =  c("HRSC 3","HRSC 4","HRSC 5"))),
                                        column(6,
                                           checkboxGroupInput('Complex_2_2',label = NULL, choices = c("HRSC 6","HRSC 7","HRSC 8"), 
                                              selected = c("HRSC 6","HRSC 7","HRSC 8"))))),
                          
                          
                                                               
                          column(width = 4, selectInput('time_frame','Detections by...',choices = c("day","week"), selected = "week"))
                                ),  #fluidRow (select inputs)
                        plotOutput('bar_graph', height = "50vh")),

            column(width  = 5, 
                   offset = 0, 
                   radioButtons('ortho_choice', 'Orthomosaic', 
                     choices = c('Fall', 'Spring'),
                     inline = TRUE), 
                   leafletOutput('SC_map', height = "70vh"))
                   ), #fluidRow
                  ), #tabPannel "Site Detection"
 
 
 tabPanel(title = "Fish Movement", 
    fluidRow(
      column(8, 
        radioButtons('season_choice', label = NULL, 
                     choices = c('Fall', 'Spring'), 
                     inline = TRUE),     
        leafletOutput('shp_map', height = "80vh")     
      ),
      column(4, 
             dateRangeInput('daterange_fish_move','Select Date Range',
                            start = "2022-01-01",
                            end   = "2022-05-18",
                            min   = as.Date(min(pittag_data_raw$min_det)),
                            max   = as.Date(max(pittag_data_raw$min_det))), 
             uiOutput("dateslider_fish_move")
             )
    )    
   ), #tabPanel "fish movement"
  ), #tabsetPanel (main)
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
  
    if (length(c(input$Complex_1,input$Complex_2_1,input$Complex_2_2 )) == 8) {
      color_choices <- c("#3300CC", "#E69F00", "#FF0000", "#00FFFF","#56B4E9", "#F0E442", "#0072B2", "#D55E00")
    } else if (length(c(input$Complex_1,input$Complex_2_1,input$Complex_2_2 )) == 7) { 
      color_choices <- c("#3300CC", "#E69F00", "#FF0000", "#00FFFF","#56B4E9", "#F0E442", "#0072B2")
    } else if (length(c(input$Complex_1,input$Complex_2_1,input$Complex_2_2 )) == 6) { 
      color_choices <- c("#3300CC", "#E69F00", "#FF0000", "#00FFFF","#56B4E9", "#F0E442")
    } else if (length(c(input$Complex_1,input$Complex_2_1,input$Complex_2_2 )) == 5) { 
      color_choices <- c("#3300CC", "#E69F00", "#FF0000", "#00FFFF","#56B4E9")
    } else if (length(c(input$Complex_1,input$Complex_2_1,input$Complex_2_2 )) == 4) { 
      color_choices <- c("#3300CC", "#E69F00", "#FF0000", "#00FFFF") 
    } else if (length(c(input$Complex_1,input$Complex_2_1,input$Complex_2_2 )) == 3) { 
      color_choices <- c("#3300CC", "#E69F00", "#FF0000") 
    } else if (length(c(input$Complex_1,input$Complex_2_1,input$Complex_2_2 )) == 2) { 
      color_choices <- c("#3300CC", "#E69F00") 
    } else { color_choices <- c("00FFFF") }
      
    ggplot(channel_complex %>%
           filter(channel_complex$SC %in% c(input$Complex_1,input$Complex_2_1,input$Complex_2_2 )) %>% 
           filter(between(as.Date(min_det),input$daterange[1],input$daterange[2])) %>%
           mutate(min_det_scaled = cut.POSIXt(min_det,input$time_frame)),
           aes(x=as.Date(min_det_scaled), fill = as.factor(SC))) +
      geom_bar(color = "black") +
      x_axis_args + 
      labs(x="Date", y = "Number of detections", fill = "Side Channel",
           caption = "Caption Text") +
      theme(axis.text = element_text(size=14),
            legend.text = element_text(size = 14),
            legend.key.size = unit(1.75, 'cm'),
            axis.title = element_text(size = 16, face = "bold"),
            plot.caption = element_text(hjust = 0, size = 14),
            title = element_text(size = 16, face = "bold")) + 
      scale_fill_manual(values = color_choices)
  })

# Leaflet Map---- 
  output$SC_map <- renderLeaflet({
    
    if (input$daterange[2]-input$daterange[1]<=10) {
      x_axis_args_leaf <- scale_x_date(date_breaks = "1 day" , labels = date_format("%b %d"))
    } else if (input$daterange[2]-input$daterange[1]<=75) {
      x_axis_args_leaf <- scale_x_date(date_breaks = "1 week" , labels = date_format("%b %d"))
    }else{
      x_axis_args_leaf <- scale_x_date(date_breaks = "1 month" , labels = date_format("%b %d"))
    }

    if (length(c(input$Complex_1,input$Complex_2_1,input$Complex_2_2 )) == 8) {
      color_choices_leaf <- c("#3300CC", "#E69F00", "#FF0000", "#00FFFF","#56B4E9", "#F0E442", "#0072B2", "#D55E00")
    } else if (length(c(input$Complex_1,input$Complex_2_1,input$Complex_2_2 )) == 7) { 
      color_choices_leaf <- c("#3300CC", "#E69F00", "#FF0000", "#00FFFF","#56B4E9", "#F0E442", "#0072B2")
    } else if (length(c(input$Complex_1,input$Complex_2_1,input$Complex_2_2 )) == 6) { 
      color_choices_leaf <- c("#3300CC", "#E69F00", "#FF0000", "#00FFFF","#56B4E9", "#F0E442")
    } else if (length(c(input$Complex_1,input$Complex_2_1,input$Complex_2_2 )) == 5) { 
      color_choices_leaf <- c("#3300CC", "#E69F00", "#FF0000", "#00FFFF","#56B4E9")
    } else if (length(c(input$Complex_1,input$Complex_2_1,input$Complex_2_2 )) == 4) { 
      color_choices_leaf <- c("#3300CC", "#E69F00", "#FF0000", "#00FFFF") 
    } else if (length(c(input$Complex_1,input$Complex_2_1,input$Complex_2_2 )) == 3) { 
      color_choices_leaf <- c("#3300CC", "#E69F00", "#FF0000") 
    } else if (length(c(input$Complex_1,input$Complex_2_1,input$Complex_2_2 )) == 2) { 
      color_choices_leaf <- c("#3300CC", "#E69F00") 
    } else { color_choices_leaf <- c("00FFFF") }
    
    leaflet_popup_graphs <- channel_complex %>%
      filter(between(as.Date(min_det),input$daterange[1],input$daterange[2])) %>%
      group_by(SC) %>%
      nest() %>% 
      filter(!SC %in% c("SRSC 1", "SRSC 2")) %>%
      
      mutate(ggs = purrr :: map2(
        data, SC,
        
        ~ ggplot(data = .x %>% group_by(date = as.Date(min_det), SC ) %>% 
                   summarise(n=n()) %>% 
                   filter(!SC %in% c("SRSC 1","SRSC 2")), 
                 aes( x = date , y = cumsum(n))) + 
          
          ggtitle(glue("Henry's Reach Side Channel {.y}")) +
          geom_line(size = 1) + geom_point(color = "cyan", size = 2)+
          labs( x = "Date", y = "Cumulative Detections") + 
          x_axis_args_leaf + 
          theme(axis.text = element_text(size=14),
                axis.title = element_text(size = 16, face = "bold"),
                plot.caption = element_text(hjust = 0, size = 14),
                title = element_text(size = 16, face = "bold")))) %>%
      slice(match(c("HRSC 1","HRSC 2","HRSC 3","HRSC 4",
                    "HRSC 5","HRSC 6","HRSC 7","HRSC 8"),SC))
    
    if ( length(leaflet_popup_graphs$SC) < 8 ){
      leaflet_popup_graphs[nrow(leaflet_popup_graphs) + 1,] <- c(SC = NA ,data = NA ,ggs = NA )}
    if ( length(leaflet_popup_graphs$SC) < 8 ){
      leaflet_popup_graphs[nrow(leaflet_popup_graphs) + 1,] <- c(SC = NA ,data = NA ,ggs = NA )}
    if ( length(leaflet_popup_graphs$SC) < 8 ){
      leaflet_popup_graphs[nrow(leaflet_popup_graphs) + 1,] <- c(SC = NA ,data = NA ,ggs = NA )}
    if ( length(leaflet_popup_graphs$SC) < 8 ){
      leaflet_popup_graphs[nrow(leaflet_popup_graphs) + 1,] <- c(SC = NA ,data = NA ,ggs = NA )}
    if ( length(leaflet_popup_graphs$SC) < 8 ){
      leaflet_popup_graphs[nrow(leaflet_popup_graphs) + 1,] <- c(SC = NA ,data = NA ,ggs = NA )}
    if ( length(leaflet_popup_graphs$SC) < 8 ){
      leaflet_popup_graphs[nrow(leaflet_popup_graphs) + 1,] <- c(SC = NA ,data = NA ,ggs = NA )}
    
   leaflet_plot_data <- litz_locs %>% mutate(Side_Channel =
                           c("NA",  "NA"  , "NA", "HRSC 1",  "NA"   ,
                             "HRSC 2",  "NA"  , "HRSC 3",  "NA"   , "HRSC 4",
                             "NA"   ,"HRSC 5",  "NA"   , "HRSC 6",  "NA"   ,
                             "NA"   ,"HRSC 7", "HRSC 8")) %>%
      filter(Side_Channel != "NA") %>%
      mutate(complex = c(rep("Upper HRSC",2),rep("Lower HRSC",6)))%>%
      mutate(plots_id = leaflet_popup_graphs$SC ) %>%
      mutate(plots = leaflet_popup_graphs$ggs )


  leaf_plot <- leaflet(leaflet_plot_data) %>%
      addProviderTiles('Esri.WorldImagery',
       options = providerTileOptions(maxNativeZoom=19,maxZoom=100)) %>%
       setView(lng = -113.627, lat = 44.8982, zoom = 17)
 

  
   if (input$ortho_choice == "Fall" && any(is.na(leaflet_plot_data) == TRUE)) {
     
     leaf_plot %>%
     addRasterRGB(ortho_fall , na.color = "transparent", r = 1,  g = 2,  b = 3, domain = 3) %>%
       addCircles(data = leaflet_plot_data %>% filter(Side_Channel %in% c(input$Complex_1,input$Complex_2_1,input$Complex_2_2)),
                  lng = ~Longitude, lat = ~Latitude,
                  radius = 2,
                  color = color_choices_leaf,
                  opacity = 1,
                  fillOpacity = 1,
                  label = ~Side_Channel,
                  labelOptions = labelOptions(noHide = TRUE,
                                              direction = "bottom",
                                              textsize = "12px",
                                              style = list("color" = "black" )))
     
   } else if (input$ortho_choice == "Fall" && any(is.na(leaflet_plot_data) == FALSE)) {
     
     leaf_plot %>% 
     addRasterRGB(ortho_fall , na.color = "transparent", r = 1,  g = 2,  b = 3, domain = 3) %>%
       addCircles(data = leaflet_plot_data %>% filter(Side_Channel %in% c(input$Complex_1,input$Complex_2_1,input$Complex_2_2)),
                  lng = ~Longitude, lat = ~Latitude,
                  radius = 2,
                  color = color_choices_leaf,
                  opacity = 1,
                  fillOpacity = 1,
                  label = ~Side_Channel,
                  labelOptions = labelOptions(noHide = TRUE,
                                              direction = "bottom",
                                              textsize = "12px",
                                              style = list("color" = "black" )),
                  popup =  popupGraph(filter(leaflet_plot_data,Side_Channel %in% c(input$Complex_1,input$Complex_2_1,input$Complex_2_2))$plots, 
                                      width = 550, 
                                      height = 250)) 
     
   } else if (input$ortho_choice == "Spring" && any(is.na(leaflet_plot_data) == TRUE)) {
       
     leaf_plot %>%
       addRasterRGB( ortho_spring , na.color = "transparent", r = 1,  g = 2,  b = 3, domain = 3) %>%
       addCircles(data = leaflet_plot_data %>% filter(Side_Channel %in% c(input$Complex_1,input$Complex_2_1,input$Complex_2_2)),
                  lng = ~Longitude, lat = ~Latitude,
                  radius = 2,
                  color = color_choices_leaf,
                  opacity = 1,
                  fillOpacity = 1,
                  label = ~Side_Channel,
                  labelOptions = labelOptions(noHide = TRUE,
                                              direction = "bottom",
                                              textsize = "12px",
                                              style = list("color" = "black" )))
     
   } else {
       
     leaf_plot %>% 
       addRasterRGB(ortho_spring , na.color = "transparent", r = 1,  g = 2,  b = 3, domain = 3) %>%
       addCircles(data = leaflet_plot_data %>% filter(Side_Channel %in% c(input$Complex_1,input$Complex_2_1,input$Complex_2_2)),
                  lng = ~Longitude, lat = ~Latitude,
                  radius = 2,
                  color = color_choices_leaf,
                  opacity = 1,
                  fillOpacity = 1,
                  label = ~Side_Channel,
                  labelOptions = labelOptions(noHide = TRUE,
                                              direction = "bottom",
                                              textsize = "12px",
                                              style = list("color" = "black" )),
                  popup =  popupGraph(filter(leaflet_plot_data,Side_Channel %in% c(input$Complex_1,input$Complex_2_1,input$Complex_2_2))$plots, 
                                      width = 550, 
                                      height = 250)) 
     
     }
  
  })
  
  # Fish Movement ----
  output$shp_map <- renderLeaflet({
    
     shp_leaf <- leaflet() %>%
      addProviderTiles('Esri.WorldImagery',
          options = providerTileOptions(maxNativeZoom=19,maxZoom=100)) %>%
      setView(lng = -113.627, lat = 44.8995, zoom = 16)       

     if (input$season_choice == "Fall") {
       shp_leaf <- shp_leaf %>%
       addRasterRGB(ortho_fall , na.color = "transparent", r = 1,  g = 2,  b = 3, domain = 3) %>%
       addPolygons(data = LW_shp_lemhi %>% mutate(color = c("red", "blue", rep("red",4), "blue")),
                   color = ~color ) 

     } else {
      shp_leaf <- shp_leaf %>%
         addRasterRGB(ortho_spring , na.color = "transparent", r = 1,  g = 2,  b = 3, domain = 3) %>%
         addPolygons(data = HW_shp_lemhi %>% mutate(color = c("red", "blue", "blue",rep("red",4))),
                     color = ~color )
     }
     
    shp_leaf %>% 
    addCircles(data = litz_locs,
               lng = ~Longitude, lat = ~Latitude,
               radius = 2,
               color = "red",
               opacity = 1,
               fillOpacity = 1) 
    

     
  })
 
  output$dateslider_fish_move <- renderUI(
    
    sliderInput("Dateslider",
                "Date:",
                min = input$daterange_fish_move[1], 
                max = input$daterange_fish_move[2], 
                value=as.Date ("2022-01-01"),timeFormat="%Y-%m-%d")
    )
  
} #Closing Bracket for Server
  
shinyApp(ui=ui,server=server)
