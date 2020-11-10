#load library
library(dplyr)
library(tidyverse)
library(readr)
library(readxl)
library(janitor)


# WEBSITE: https://public.emdat.be/data
# International Disaster database

######################################

#TODO: Basic information about this dataset
# For a disaster to be entered into the database 
# at least one of the following criteria must be fulfilled:

## Ten (10) or more people reported killed
## Hundred (100) or more people reported affected
## Declaration of a state of emergency
## Call for international assistance

# Country: Country (or countries) in which the disaster has occurred.
# Disaster group: natural disasters, and technological disasters, complex disasters (e.g. famine)
# Disaster type: Description of the disaster based on a pre-defined classification.
# Date: When the disaster occurred. Month/Day/Year.
# Death: Number of people who lost their life because the event happened.
# Missing: The number of people who re missing / presumed dead (official figure when available).
# Total deaths: Sum of death and missing.
# Injured: People suffering from physical injuries, trauma or an illness
# Homeless: Number of people whose house is destroyed or heavily damaged
# and therefore need shelter after an event.
# Affected: People requiring immediate assistance during a period of emergency, 
# i.e. requiring basic survival needs such as food, water, shelter, sanitation 
# and immediate medical assistance.
# Total affected: Sum of injured, homeless, and affected.
# Estimated Damage: The amount of damage to property, crops, and livestock. 
# The value of estimated damage is given in US$ (‘000). For each disaster, 
# the registered figure corresponds to the damage value at the moment of the event
# , i.e. the figures are shown true to the year of the event.


#####################################

# read in the csv file
my_path <- "C:/Users/Yesuel Kim/Documents/Git/Blog-HealthAndJusticeLeague"
data <- read_excel(paste0(my_path,"/data/naturaldisasters.xlsx"), 
                   col_types = c("skip", "text", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "skip", "text", "text", "skip", 
                                 "skip", "skip", "skip", "numeric", 
                                 "numeric", "text", "text", "text", 
                                 "text", "text", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric"))

data <- data %>% clean_names()

data %>% filter(latitude == "-17.82")

names(data)

#remove unnecessary variables
data1 <- data %>%
  select(-c(seq, disaster_subtype, disaster_subsubtype, event_name, 
            entry_criteria, origin, associated_dis, aid_contribution, local_time,
            river_basin)) %>%
  rename(countrycode = iso, lat = latitude, long=longitude)


# convert latitude and longitude into numeric types
# negative latitude if in the Southern hemisphere
# negative longitude if in the Western hemisphere
data2 <- data1 %>%
  separate(col = lat, into = c("latnum", "latdir"), sep = " ", remove = TRUE) %>%
  separate(col = long, into = c("longnum", "longdir"), sep = " ", remove = TRUE) %>%
  mutate(latnum = as.numeric(latnum),
         longnum = as.numeric(longnum)) %>%
  mutate(latnumneg = latnum * -1, longnumneg = longnum * -1) %>%
  select(-c(26, 27, 28)) %>%
  rowwise() %>%
  mutate(latitude = case_when(
    (latnum >= 0 && latdir == "S") ~ latnumneg,
    TRUE ~ latnum
  )) %>%
  rowwise() %>%
  mutate(longitude = case_when(
    (longnum >= 0 && longdir == "W") ~ longnumneg,
    TRUE ~ longnum
  ))

# available disaster types
# unique(data2$disaster_type)
# [1] "Drought"            
# [2] "Earthquake"         
# [3] "Volcanic activity"  
# [4] "Mass movement (dry)"
# [5] "Storm"              
# [6] "Flood"              
# [7] "Epidemic"           
# [8] "Landslide"          
# [9] "Wildfire"           
# [10] "Extreme temperature"
# [11] "Fog"                
# [12] "Insect infestation" 
# [13] "Impact"             
# [14] "Animal accident" 

relevant <- c("Drought", "Storm", "Flood", "Landslide", 
              "Wildfire", "Extreme temperature")

datafinal <- data2 %>%
  select(c(1:10), c(15:25), -c(disaster_group, disaster_subgroup, starts_with("start_"), starts_with("end_"))) %>%
  filter(disaster_type %in% relevant)

datafinal_yearsum <- datafinal %>%
  group_by(year, country, countrycode, disaster_type) %>%
  summarize(deaths = sum(total_deaths, na.rm = TRUE), 
            injured = sum(no_injured, na.rm = TRUE),
            affected = sum(no_affected, na.rm = TRUE),
            homeless = sum(no_homeless, na.rm = TRUE),
            occurrence = n())

datafinal_all <- datafinal_yearsum %>%
  group_by(year, country, countrycode) %>%
  summarize(deaths = sum(deaths, na.rm = TRUE), 
            injured = sum(injured, na.rm = TRUE),
            affected = sum(affected, na.rm = TRUE),
            homeless = sum(homeless, na.rm = TRUE),
            occurrence = sum(occurrence)) %>%
  mutate(disaster_type = "All") %>%
  select(c(1:3), disaster_type, everything()) %>%
  bind_rows(datafinal_yearsum) %>%
  filter(year >= 1980) %>%
  mutate(total = deaths + injured + affected + homeless)


datafinal_summary <- datafinal_yearsum %>%
  pivot_longer(cols = c(deaths, injured, affected, homeless, occurrence),
               names_to = "category",
               values_to = "value") %>%
  pivot_wider(id_cols = c(year, country, countrycode),
              names_from = c(disaster_type, category),
              values_from = value,
              values_fill = 0) %>%
  select(year, country, countrycode, ends_with("occurrence"), 
         ends_with("deaths"), everything()) 

datafinal_summary$totaldeaths <- rowSums(datafinal_summary[, 10:15], na.rm=TRUE)
datafinal_summary$totalaffected <- rowSums(datafinal_summary[, 16:33], na.rm = TRUE)


datafinal_summary <- datafinal_summary %>%
  select(-c(10:33))

write_csv(x = datafinal, 
          path = paste0(my_path,"/data/wrangled_natdisasters.csv"))

write_csv(x = datafinal_all, 
          path =  paste0(my_path,"/data/wrangled_natdisasters_byyear.csv"))

write_csv(x = datafinal_summary, 
          path =  paste0(my_path,"/data/wrangled_natdisasters_briefsum.csv"))

# To also merge latitude and longitude data,
# library(maps)
# world_map <- map_data(map = "world2", region = ".")
