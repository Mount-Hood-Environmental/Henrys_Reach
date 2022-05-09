# Load Packages ------
library(tidyverse)

#load DATA 

tag_data <- read_csv("data/0LL_tag_010122_050122.csv")

tag_data$date <- as.Date(tag_data$detected)
#tag_data$time <- as.POSIXct(tag_data$detected,format = "%H:%M")
tag_data$detected <- as.POSIXct(tag_data$detected, format = "%Y-%m-%d %H:%M:%S")


tag_data <- tag_data %>%
  distinct(detected, .keep_all = TRUE)

#Track a randomly selected Fish ----
fish_rand <- tag_data %>% 
  distinct(tag,.keep_all = TRUE) %>%
  sample_n(100) %>%
  pull(tag)

fish_track <- tag_data %>%
  filter(tag %in% fish_rand) 

ggplot(fish_track, 
  aes(x=detected,y=reader,group=tag,color=tag)) + 
  geom_point()+
  geom_line()+ 
  theme(legend.position = "none")
 
#Site visit frequency ----
pop_sites <- tag_data %>%
  distinct(tag,reader,.keep_all = TRUE)
ggplot(pop_sites, aes(x=reader)) +
 geom_bar()

pop_sites_month <- pop_sites%>%
  filter(reader == c("01","02"))
  
ggplot(pop_sites_month, aes(x=date))+
 geom_histogram() + 
 facet_wrap(~reader) 

ggplot(pop_sites_month, aes(x=date , color=reader)) + 
  geom_histogram(position="identity", alpha=0.1,bins = 40)

  

  








  
  
