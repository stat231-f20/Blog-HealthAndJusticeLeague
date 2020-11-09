
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

reducedclimatedisaster_data <- origclimatedisaster_data %>% 
  clean_names() %>%
  select(c(1, 5:6, 10)) %>%
  filter(year %in% c(1980:2018))
