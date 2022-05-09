# Author: Nick Porter
# Purpose: Create a cleaned up version of Pit data for Lemhi Litz cords downloaded from Biologic
# Created: 10/27/21
# Last Modified: 1/21/22

#-----------------------------------------------------------------

rm(list = ls())
# load a couple packages

#to install PITcleanr (if you don't have it)
#remotes::install_github("KevinSee/PITcleanr")
require(tidyverse)
require(lubridate)
require(esquisse)
library(janitor)
library(PITcleanr)

# read in the raw data, and rename some columns
raw_data = read.csv("C:~Desktop/Henry_Reach/data/0LL_tag_010122_050122.csv") %>% # your file pathway
  rename(`Tag Code` = tag,
         `Event Date Time Value` = detected,
         `Event Site Code Value` = reader,
         `Antenna ID` = antenna) %>%
  # you'll need to add this column
  mutate(`Event Type Name` = "Observation") %>%
  janitor::clean_names()
  
# compress the data
comp_df = compress(raw_data,
                   ignore_event_vs_release = T)

comp_df

write.csv(comp_df, "C:/Users/bolde/Desktop/0LL_cleaned_010122_050122.csv")

# Bridger, ignore the stuff below here for now


#----------------------------------------------
# for directionality, you'll need a parent-child table
# fish move from parent to child
# you can hand-code this however you want
parent_child <- tibble(parent = c("16","17","18","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15")) %>%
  mutate(child = lead(parent)) %>%
  filter(!is.na(child))

# this adds direction to each observation (forward/backward/etc.)
direct_df = addDirection(comp_df,
                         parent_child)
direct_df

glimpse(parent_child)