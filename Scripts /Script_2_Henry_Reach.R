#load packages and data -----
library(tidyverse)
library(lubridate)
library(sf)
library(mapview)
library(leaflet)
library(scales)

litz_locs <- read_csv("Litz_Locations.csv")
pittag_data_raw <- read_csv("~/Desktop/Github/Henrys_reach/0LL_cleaned_nov_may")

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
cc_1 <- channel_complex %>% filter(Complex %in% c("exit","Complex 1"))
cc_2 <- channel_complex %>% filter(Complex == "Complex 2")

#Each loop runs through every unique individual that enters into their respective complex
#and calculates the total time spent in that complex. Data are saved in "total_time_1" & "total_time_2" 
#for Complex 1 and Complex 2.

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


#Compile all vectors to create data frame. Note that the order in which 
#vectors are combined matters.
complex_1_vec <- c(rep("Complex 1",times = length(total_time_1)))
complex_2_vec <- c(rep("Complex 2",times = length(total_time_2)))
tot_time_vec <- c(total_time_1,total_time_2)
tag_id_vec   <- c(tag_id_1,tag_id_2)
complex_vec  <- c(complex_1_vec,complex_2_vec)

#Create data frame and also calculate average time spent at project site.
plot_time <- data.frame(tag_id_vec,complex_vec,tot_time_vec)
mean_1 <- mean(plot_time$tot_time_vec)

#Create ggplot that shows time spent at each complex.----

 ggplot(plot_time, aes(x=tot_time_vec,color=complex_vec)) + 
   geom_histogram(bins = 10, fill="white", alpha=0.5, position="identity") +
   geom_vline(data=plot_time, aes(xintercept=mean_1),
              linetype="dashed") +
  xlab("Days Spent in Project Site")+ 
  ylab("Number of Individuals") 


 





