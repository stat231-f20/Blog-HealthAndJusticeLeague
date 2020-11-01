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



write_csv(x = data2, 
          path = paste0(my_path,"/data/wrangled_natdisasters.csv"))
  
