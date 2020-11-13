library(tidyverse)
library(dplyr)
library(shiny)
library(shinythemes)
library(countrycode)
library(leaflet)
library(maps)
library(readr)
library(ggplot2)
library(lubridate)

my_path <- "C:/Users/Yesuel Kim/Documents/Git/Blog-HealthAndJusticeLeague"

co2 <- read_csv(paste0(my_path, "/data/co_bycountrycsv"))
gtemp <- read_csv(paste0(my_path, "/data/globaltemperature_long.csv"))
  
ggplot(data = gtemp_long, aes(x = as.POSIXct(period, origin = "1970-01-01"), y = temp)) +
  geom_line() +
  geom_smooth() +
  scale_x_datetime(name = "Time")
