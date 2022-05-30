
# Authors: Bridger Bertram
# Purpose:  Fill in
# Created: Fill in
# Last Modified: Fill in
# Notes:

#load packages and data -----
library(tidyverse)
library(sf)
library(mapview)
library(leaflet)
library(scales)

litz_locs <- read_csv("Data/Litz_Locations.csv")
pittag_data_raw <- read_csv("Data/0LL_cleaned_nov_may")

#Add Side_Channel and Complex columns ----
channel_complex <- pittag_data_raw %>% 
  filter(node %in% c(4:18)) %>% #Filter out data from "SRSC" project
  
  mutate(Side_Channel = ifelse(node %in% 4:5,   "HRSC 1",  #combine nodes into side channels
                        ifelse(node %in% 6:7,   "HRSC 2",  # e.g. node 4&5 correspond to a single                
                        ifelse(node %in% 8:9,   "HRSC 3",  # Side_Channel "HRSC_1"              
                        ifelse(node %in% 10:11, "HRSC 4",
                        ifelse(node %in% 12:13, "HRSC 5", 
                        ifelse(node %in% 14:18, "exit", 0)))))),
         
         Complex =      ifelse(Side_Channel %in% c("HRSC 1","HRSC 2"), "Complex 1",  #An Individual enters a Complex based off
                        ifelse(Side_Channel %in% c("HRSC 3", "HRSC 4", "HRSC 5"),    #which Side_Channel entrance is used
                                                            "Complex 2","exit")))    #if and indv. goes through an exit channel
                                                                                     #"exit" is returned in column "Complex"
#Create New Data Frame with Columns tag_code,complex,total_time ---- 

#Filter "channel_complex" for each complex called cc_1 & cc_2
cc_1 <- channel_complex %>% filter(Complex %in% c("exit","Complex 1")) # If you wanted, you could throw this into the loops below to cut down stuff in environment.
cc_2 <- channel_complex %>% filter(Complex == "Complex 2")

#Each loop runs through every unique individual that enters into their respective complex
#and calculates the total time spent in that complex. Data are saved in "total_time_1" & "total_time_2" 
#for Complex 1 and Complex 2 respectively. 

#loop for complex 1
fish_1 <- unique(cc_1$tag_code)
total_time_1 <- c()
tag_id_1 <- c()
for(i in fish_1) {
track_fish_1 <- channel_complex %>% filter(tag_code == i)
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

#Create data frame and also calculate average time spent at project site.
plot_time <- tibble(.rows = length(c(tag_id_1,tag_id_2))) %>%
  mutate(
    tag_id = c(tag_id_1,tag_id_2),
    tot_time = c(total_time_1,total_time_2),
    complex_vec = c( c(rep("Complex 1",times = length(total_time_1))), 
                     c(rep("Complex 2",times = length(total_time_2)))))

#Create ggplot that shows time spent at each complex.----

 ggplot(plot_time, aes(x=tot_time,color=complex_vec)) + 
   geom_histogram(bins = 10, fill="white", alpha=0.5, position="identity") +
   geom_vline(data=plot_time, aes(xintercept=mean(tot_time)),
              linetype="dashed") +
  xlab("Days Spent in Project Site")+ 
  ylab("Number of Individuals") 


 





