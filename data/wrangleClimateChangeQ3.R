
#load library
library(dplyr)
library(tidyverse)
library(readr)
library(readxl)
library(janitor)


my_path <- "C:/Users/seshu/Documents/RStudio/projects/git/Blog-HealthAndJusticeLeague"

climatedisaster_data <- read_csv(paste0(my_path,"/data/wrangled_natdisasters_byyear.csv"))

climatedisaster_data$country <- gsub("Czech Republic (the)", "Czech Republic", climatedisaster_data$country)
climatedisaster_data$country <- gsub("Korea (the Republic of)", "Korea", climatedisaster_data$country)
climatedisaster_data$country <- gsub("Netherlands (the)", "Netherlands", climatedisaster_data$country)
climatedisaster_data$country <- gsub("Slovakia", "Slovakia Republic", climatedisaster_data$country)
climatedisaster_data$country <- gsub("United Kingdom of Great Britain and Northern Ireland (the)", "United Kingdom", climatedisaster_data$country)
climatedisaster_data$country <- gsub("United States of America (the)", "United States", climatedisaster_data$country)


countrymort_data <- read_csv(paste0(my_path,"/data/wrangled_infmatmortline.csv"))

distinctcountrycode <- unique(countrymort_data$COU)

reducedclimatedisaster_data <- climatedisaster_data %>% 
  select(c(1:4, 9)) %>%
  filter(c(countrycode %in% distinctcountrycode)
         , year %in% c(1980:2018)) %>%
  pivot_wider(id_cols = c(year, country, countrycode)
              , names_from = disaster_type
              , values_from = occurrence)

write_csv(reducedclimatedisaster_data, paste0(my_path,"/data/wrangled_climateq3.csv"))
          
          
