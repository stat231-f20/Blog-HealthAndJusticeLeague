
#load library
library(dplyr)
library(tidyverse)
library(readr)
library(readxl)
library(janitor)


my_path <- "C:/Users/seshu/Documents/RStudio/projects/git/Blog-HealthAndJusticeLeague"
climatedisaster_data <- read_csv(paste0(my_path,"/data/wrangled_natdisasters_byyear.csv"))

countrymort_data <- read_csv(paste0(my_path,"/data/wrangled_infmatmortline.csv"))

distinctcountrycode <- unique(countrymort_data$COU)

reducedclimatedisaster_data <- climatedisaster_data %>% 
  select(c(1:4, 9)) %>%
  filter(c(countrycode %in% distinctcountrycode)
         , year %in% c(1980:2018))

write_csv(reducedclimatedisaster_data, paste0(my_path,"/data/wrangled_climateq3.csv"))
          
          
