# Authors: Bridger Bertram
# Purpose:  Pit Tag Data visualization of Henry's Reach Project. Lemhi River, Idaho
# Created: May 15
# Last Modified: July 9th

# Load Packages and Data ------
library(tidyverse)
library(sf)
library(mapview)
library(leaflet)
library(leafem)
library(raster)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(scales)
library(here)
library(glue)
library(leafpop)
library(plotly) 
library(shinycssloaders) 
library(gganimate)
library(ggnewscale)
library(zoo)
library(randomcoloR)

setwd(here())
litz_locs        <- read_csv("Data/Litz_Locations.csv")
pittag_data_raw  <- read_csv("Data/0LL_cleaned_nov_may")
USGS_Stream_Data <- read_csv("Data/LemL5_Flow_WY.csv")
Fish_Move_Mock_Adv <- read_csv("Data/Fish_Move_Mock_Data_Adv.csv")


#Low Resolution Ortho
ortho_resolution <- 10
#High Resolution Ortho 
#ortho_resolution <- 3

ortho_fall <- aggregate((terra::rast('Data/ortho_reduced/Henrys_reduced.tif') %>%
         raster::brick()), fact = ortho_resolution)
         ortho_fall[ortho_fall == 0] <- NA
         
ortho_spring <- aggregate((terra::rast('Data/ortho_reduced/Henrys_reduced_spring_22.tif') %>%
        raster::brick()), fact = ortho_resolution)
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
  titlePanel( 
    fluidRow(column(width = 8, "Henry's Reach"), 
             column(width = 2, tags$a(tags$img(src = "MHE_logo.jpg", height = 80, width = 190, align = "right"),  href="https://mthoodenvironmental.com/")),
             column(width = 2, tags$a(tags$img(src = "Biomark_logo.png", height = 80, width = 180), href = "https://www.biomark.com/")))
            ), #Title Panel
   
       tabsetPanel(
         tabPanel("Site Detection", 
           fluidRow(
            column(width = 7,
                   fluidRow(
                     column(6,
                       dateRangeInput('daterange','Select Date Range',
                              start = "2022-01-01",
                              end   = "2022-05-18",
                              min   = as.Date(min(pittag_data_raw$min_det)),
                              max   = as.Date(max(pittag_data_raw$min_det)))),
                     column(width = 6, selectInput('time_frame','Detections by...',choices = c("day","week"), selected = "week"))),
                   
                        fluidRow(
                          column(width = 6, 
                                 pickerInput('Complex_1',choices = c("HRSC 1","HRSC 2"), 
                                              selected = c("HRSC 1","HRSC 2"), multiple = TRUE, 
                                              options = pickerOptions(actionsBox = TRUE, 
                                                                      selectedTextFormat = 'static', 
                                                                      noneSelectedText = "Upper Henry's Reach"))
                          ),
                          
                          column(width = 6, 
                                 pickerInput('Complex_2',choices = c("HRSC 3","HRSC 4","HRSC 5",
                                                                     "HRSC 6","HRSC 7","HRSC 8"), 
                                             selected = c("HRSC 3","HRSC 4","HRSC 5",
                                                          "HRSC 6","HRSC 7","HRSC 8"),
                                             multiple = TRUE, 
                                             options = pickerOptions(actionsBox = TRUE,
                                                                     selectedTextFormat = 'static', 
                                                                     noneSelectedText = "Lower Henry's Reach"))),
                                ),  #fluidRow (select inputs)
                        plotOutput('bar_graph', height = "70vh") %>% 
                         withSpinner(color="#0dc5c1")
                   
                   ),

            column(width  = 5, 
                   offset = 0, 
                   radioButtons('ortho_choice', 'Orthomosaic', 
                     choices = c('Fall', 'Spring'),
                     inline = TRUE), 
                   leafletOutput('SC_map', height = "70vh") %>%
                   withSpinner(color="#0dc5c1"))
                   ), #fluidRow
                  ), #tabPannel "Site Detection"
 
 
 tabPanel(title = "Fish Movement", 
         
    dateRangeInput('daterange_fish_move','Select Date Range',
                     start = "2022-01-01",
                     end   = "2022-05-18",
                     min   = as.Date(min(pittag_data_raw$min_det)),
                     max   = as.Date(max(pittag_data_raw$min_det))),
   
        plotlyOutput('Move_Fish', height = "70vh", width = "70%") %>%
          withSpinner(color="#0dc5c1")  
       # dataTableOutput("poly_tables")
   ), #tabPanel "fish movement"
 
 tabPanel(title = "Lemhi River Discharge", 
    pickerInput("year", label = NULL, choices = c(as.character(seq(2022,1979,-1))), multiple = TRUE, 
                options = pickerOptions(actionsBox = TRUE, 
                                        selectedTextFormat = 'static', 
                                        noneSelectedText = "Select Years")),
    plotlyOutput("Lemhi_Discharge_Plot", height = "75vh") %>%
      withSpinner(color="#0dc5c1")
    ), #tabPanel "Lemhi River Discharge" 
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
  
    if (length(c(input$Complex_1,input$Complex_2)) == 8) {
      color_choices <- c("#3300CC", "#E69F00", "#FF0000", "#00FFFF","#56B4E9", "#F0E442", "#0072B2", "#D55E00")
    } else if (length(c(input$Complex_1,input$Complex_2)) == 7) { 
      color_choices <- c("#3300CC", "#E69F00", "#FF0000", "#00FFFF","#56B4E9", "#F0E442", "#0072B2")
    } else if (length(c(input$Complex_1,input$Complex_2)) == 6) { 
      color_choices <- c("#3300CC", "#E69F00", "#FF0000", "#00FFFF","#56B4E9", "#F0E442")
    } else if (length(c(input$Complex_1,input$Complex_2)) == 5) { 
      color_choices <- c("#3300CC", "#E69F00", "#FF0000", "#00FFFF","#56B4E9")
    } else if (length(c(input$Complex_1,input$Complex_2)) == 4) { 
      color_choices <- c("#3300CC", "#E69F00", "#FF0000", "#00FFFF") 
    } else if (length(c(input$Complex_1,input$Complex_2)) == 3) { 
      color_choices <- c("#3300CC", "#E69F00", "#FF0000") 
    } else if (length(c(input$Complex_1,input$Complex_2)) == 2) { 
      color_choices <- c("#3300CC", "#E69F00") 
    } else { color_choices <- c("00FFFF") }
      
    ggplot(channel_complex %>%
           filter(channel_complex$SC %in% c(input$Complex_1,input$Complex_2)) %>% 
           filter(between(as.Date(min_det),input$daterange[1],input$daterange[2])) %>%
           mutate(min_det_scaled = cut.POSIXt(min_det,input$time_frame)),
           aes(x=as.Date(min_det_scaled), fill = as.factor(SC))) +
      geom_bar(color = "black") +
      x_axis_args + 
      labs(x="Date", y = "Number of detections", fill = "Side Channel",
           caption = "Caption Text") +
      theme_minimal() +
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

    if (length(c(input$Complex_1,input$Complex_2)) == 8) {
      color_choices_leaf <- c("#3300CC", "#E69F00", "#FF0000", "#00FFFF","#56B4E9", "#F0E442", "#0072B2", "#D55E00")
    } else if (length(c(input$Complex_1,input$Complex_2)) == 7) { 
      color_choices_leaf <- c("#3300CC", "#E69F00", "#FF0000", "#00FFFF","#56B4E9", "#F0E442", "#0072B2")
    } else if (length(c(input$Complex_1,input$Complex_2)) == 6) { 
      color_choices_leaf <- c("#3300CC", "#E69F00", "#FF0000", "#00FFFF","#56B4E9", "#F0E442")
    } else if (length(c(input$Complex_1,input$Complex_2)) == 5) { 
      color_choices_leaf <- c("#3300CC", "#E69F00", "#FF0000", "#00FFFF","#56B4E9")
    } else if (length(c(input$Complex_1,input$Complex_2)) == 4) { 
      color_choices_leaf <- c("#3300CC", "#E69F00", "#FF0000", "#00FFFF") 
    } else if (length(c(input$Complex_1,input$Complex_2)) == 3) { 
      color_choices_leaf <- c("#3300CC", "#E69F00", "#FF0000") 
    } else if (length(c(input$Complex_1,input$Complex_2)) == 2) { 
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
          theme_dark() +
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
       setView(lng = -113.627, lat = 44.8985, zoom = 17)
 

  
   if (input$ortho_choice == "Fall" && any(is.na(leaflet_plot_data) == TRUE)) {
     
     leaf_plot %>%
     addRasterRGB(ortho_fall , na.color = "transparent", r = 1,  g = 2,  b = 3, domain = 3) %>%
       addCircles(data = leaflet_plot_data %>% filter(Side_Channel %in% c(input$Complex_1,input$Complex_2)),
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
       addCircles(data = leaflet_plot_data %>% filter(Side_Channel %in% c(input$Complex_1,input$Complex_2)),
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
                  popup =  popupGraph(filter(leaflet_plot_data,Side_Channel %in% c(input$Complex_1,input$Complex_2))$plots, 
                                      width = 550, 
                                      height = 250)) 
     
   } else if (input$ortho_choice == "Spring" && any(is.na(leaflet_plot_data) == TRUE)) {
       
     leaf_plot %>%
       addRasterRGB( ortho_spring , na.color = "transparent", r = 1,  g = 2,  b = 3, domain = 3) %>%
       addCircles(data = leaflet_plot_data %>% filter(Side_Channel %in% c(input$Complex_1,input$Complex_2)),
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
       addCircles(data = leaflet_plot_data %>% filter(Side_Channel %in% c(input$Complex_1,input$Complex_2)),
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
                  popup =  popupGraph(filter(leaflet_plot_data,Side_Channel %in% c(input$Complex_1,input$Complex_2))$plots, 
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
                   color = ~color,
                   layerId = ~Reach) 

     } else {
      shp_leaf <- shp_leaf %>%
         addRasterRGB(ortho_spring , na.color = "transparent", r = 1,  g = 2,  b = 3, domain = 3) %>%
         addPolygons(data = HW_shp_lemhi %>% mutate(color = c("red", "blue", "blue",rep("red",4))),
                     color = ~color,
                     layerId = ~Reach)
     }
     
    shp_leaf %>% 
    addCircles(data = litz_locs,
               lng = ~Longitude, lat = ~Latitude,
               radius = 2,
               color = "red",
               opacity = 1,
               fillOpacity = 1) 
     
  }) #Closing Brackets for output$shp_map
  
  
#Click input table -----  
  observeEvent(input$shp_map_shape_click, { 
   
    output$poly_tables <- renderDataTable({
     
      p <- input$shp_map_shape_click 

      if(p$id == "Lem1_Release"){ 
        table_leaf <- c("Main Channel", "Lem 1 realease", "red")
        table_leaf <- as.data.frame(table_leaf)
        table_leaf
        
      }else if (p$id == "Upper_HRSC"){
        table_leaf <- c("Side Channel", "Upper HRSC", "blue")
        table_leaf <- as.data.frame(table_leaf)
        table_leaf  
        
      }else if (p$id == "Lower_HRSC"){
        table_leaf <- c("Side Channel", "Lower HRSC", "blue")
        table_leaf <- as.data.frame(table_leaf)
        table_leaf  
        
      }else if (p$id == "Lem3"){
        table_leaf <- c("Main Channel", "Lemhi 3", "red")
        table_leaf <- as.data.frame(table_leaf)
        table_leaf  
        
      }else if (p$id == "Lem4"){
        table_leaf <- c("Main Channel", "Lemhi 4", "red")
        table_leaf <- as.data.frame(table_leaf)
        table_leaf  
        
      }else if (p$id == "Lem5"){
        table_leaf <- c("Main Channel", "Lemhi 5", "red")
        table_leaf <- as.data.frame(table_leaf)
        table_leaf 
        
      }else{
        table_leaf <- c("Main Channel", "Lemhi 2", "red")
        table_leaf <- as.data.frame(table_leaf)
        table_leaf } 
      
    }) #Closing brackets for output$poly_tables  
  }) #closing brackets for observeEvent 
  
#Lemhi River Discharge Plot ----
  output$Lemhi_Discharge_Plot <- renderPlotly({
    
    USGS_plotly_data <- USGS_Stream_Data%>% 
      mutate(date = USGS_Stream_Data$...1) %>%
      filter(!date %in% c("Min","Q1","Median","Q3","Max")) %>%
      mutate(real_date = as.Date(date,format = "%b-%d")) %>%
      dplyr::select(real_date,Min,Q1,Median,Q3,Max,input$year) %>%
      arrange(real_date)
    
    USGS_plotly_data<- na.locf(USGS_plotly_data)
    line_pal <- randomColor(100, luminosity="bright")
    if (is.null(input$year) == FALSE) {
      ploty_cfs <- plot_ly(data = USGS_plotly_data, x = ~real_date)
      for(i in 1:length(input$year)){
        
        ploty_cfs <- add_trace(ploty_cfs, y = pull(USGS_plotly_data,input$year[i]),
                     type = 'scatter',
                     mode = 'lines', 
                     name = input$year[i], 
                     line = list(color = line_pal[i], 
                            width = 4),
                     hovertemplate = paste('%{x|%b,%d}  <i>cfs</i>: %{y}')) 
        
      }}else{ploty_cfs <- plot_ly(data = USGS_plotly_data, x = ~real_date)}
  
    ploty_cfs %>%
      add_ribbons(ymin = ~Min, ymax = ~Q1 , fillcolor = "coral", opacity = .2, line = list(color = 'rgba(0, 0, 0, 0)'),
                  showlegend = FALSE, hoverinfo = 'none') %>%
      add_ribbons(ymin = ~Q1 , ymax = ~Q3 , fillcolor = "cyan" , opacity = .2, line = list(color = 'rgba(0, 0, 0, 0)'),
                  showlegend = FALSE, hoverinfo = 'none')  %>%
      add_ribbons(ymin = ~Q3 , ymax = ~Max, fillcolor = "coral", opacity = .2, line = list(color = 'rgba(0, 0, 0, 0)'),
                  showlegend = FALSE, hoverinfo = 'none') %>%
      add_trace(y = ~Min,    mode = 'lines', type = 'scatter', name = 'Min',
                line = list(color = 'coral', width = 2),
                hovertemplate = paste('%{x|%b,%d}  <i>cfs</i>: %{y}')) %>%
      add_trace(y = ~Q1,     mode = 'lines', type = 'scatter', name = '25th Quartile',
                line = list(color = 'cyan',  width = 2),
                hovertemplate = paste('%{x|%b,%d}  <i>cfs</i>: %{y}')) %>%
      add_trace(y = ~Median, mode = 'lines', type = 'scatter', name = "Median ('79-'22)",
                line = list(color = 'black', width = 2),
                hovertemplate = paste('%{x|%b,%d}  <i>cfs</i>: %{y}')) %>%
      add_trace(y = ~Q3,     mode = 'lines', type = 'scatter', name = '75th Quartile',
                line = list(color = 'cyan',  width = 2),
                hovertemplate = paste('%{x|%b,%d}  <i>cfs</i>: %{y}')) %>%
      add_trace(y = ~Max,    mode = 'lines', type = 'scatter', name = 'Max',
                line = list(color = 'coral', width = 2), 
                hovertemplate = paste('%{x|%b,%d}  <i>cfs</i>: %{y}') ) %>%
      layout(title = "Lemhi River - L5 (USGS #13305310)", 
             xaxis = list(title = list( text = "Date", size = 25)), 
             yaxis = list(title = list( text = "Discharge (cfs)", size = 25)))
  })
  
#GG Fish Move -------  
output$Move_Fish <- renderPlotly({
  
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
    complete(Tag,Frame)
  
  K <- ggplot() +
    geom_polygon( aes( x = HW_shp_lemhi[[4]][[1]][[1]][[1]][,1], y = HW_shp_lemhi[[4]][[1]][[1]][[1]][,2] ), color = "red"  , fill = "red"  , alpha = .5) +
    geom_polygon( aes( x = HW_shp_lemhi[[4]][[2]][[1]][[1]][,1], y = HW_shp_lemhi[[4]][[2]][[1]][[1]][,2] ), color = "blue" , fill = "blue" , alpha = .5) +
    geom_polygon( aes( x = HW_shp_lemhi[[4]][[3]][[1]][[1]][,1], y = HW_shp_lemhi[[4]][[3]][[1]][[1]][,2] ), color = "blue" , fill = "blue" , alpha = .5) +
    geom_polygon( aes( x = HW_shp_lemhi[[4]][[4]][[1]][[1]][,1], y = HW_shp_lemhi[[4]][[4]][[1]][[1]][,2] ), color = "red"  , fill = "red"  , alpha = .5) +
    geom_polygon( aes( x = HW_shp_lemhi[[4]][[5]][[1]][[1]][,1], y = HW_shp_lemhi[[4]][[5]][[1]][[1]][,2] ), color = "red"  , fill = "red"  , alpha = .5) +
    geom_polygon( aes( x = HW_shp_lemhi[[4]][[6]][[1]][[1]][,1], y = HW_shp_lemhi[[4]][[6]][[1]][[1]][,2] ), color = "red"  , fill = "red"  , alpha = .5) +
    geom_polygon( aes( x = HW_shp_lemhi[[4]][[7]][[1]][[1]][,1], y = HW_shp_lemhi[[4]][[7]][[1]][[1]][,2] ), color = "red"  , fill = "red"  , alpha = .5) +
    geom_point(data = Fish_Move_Mock_Adv, aes(x=lng, y=lat, color=Tag, frame = Frame), size = 3,
               position = position_jitter(h=0.0001,w=0.0001)) + 
    labs(x="Longitude",y="Latitude")+
    theme(legend.position = "none")
  
  
  ggplotly(K) %>% 
    animation_opts(1000) %>%
    animation_slider(
      currentvalue = list(prefix = "Day"))
  
  
})
  
} #Closing Bracket for Server
  
shinyApp(ui=ui,server=server)
