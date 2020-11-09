
#load library
library(dplyr)
library(tidyverse)
library(readr)
library(readxl)
library(janitor)

my_path <- "C:/Users/seshu/Documents/RStudio/projects/git/Blog-HealthAndJusticeLeague"
origclimatedisaster_data <- read_excel(paste0(my_path,"/data/naturaldisasters.xlsx"), 
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

irrelevant <- c("Earthquake", "Epidemic", "Landslide"
                , "Insect infestation", "Mass movement (dry)")

countrymort_data <- read_csv(paste0(my_path,"/data/wrangled_infmatmortline.csv"))

distinctcountries <- unique(countrymort_data$Country)

reducedclimatedisaster_data <- origclimatedisaster_data %>% 
  clean_names() %>%
  select(c(1, 5:6, 10)) %>%
  filter(c(country %in% distinctcountries)
         , year %in% c(1980:2018)
         , !(disaster_type %in% irrelevant))
  
