#wrangling done by Mythili


#loading packages
library(readr)
library(dplyr)
library(janitor)
library(tidyverse)
library(maps)



#reading in .csv files for country-wise infant and maternal mortality data

path_in <- "C:/Users/seshu/Documents/RStudio/projects/git/Blog-HealthAndJusticeLeague/data"
origINFANT <- read_csv(paste0(path_in,"/infantmortalityorig.csv"))
origMATERN <- read_csv(paste0(path_in,"/maternalmortalityorig.csv"))



#selecting necessary columns

world_map <- map_data(map = "world", region = ".") %>%
  select(c(5, 1:3))



#informatively naming columns; joining infant mortality data w/ lat & long data

reducedcolumnsINFANT <- origINFANT %>%
  select(c(5:6, 8:9)) %>%
  rename(deaths_per_1000_live_births = 'Value') %>%
  right_join(world_map, by = c("Country" = "region"))



#rearranging columns so latitude and longitude are next to country

reducedcolumnsINFANT <- reducedcolumnsINFANT[c(1:2, 5:7, 3:4)]



#informatively naming columns; joining maternal mortality data w/ lat & long data

reducedcolumnsMATERN <- origMATERN %>%
  select(c(5:6, 8:9)) %>%
  rename(deaths_per_100000_live_births = 'Value') %>%
  right_join(world_map, by = c("Country" = "region"))



#rearranging columns so latitude and longitude are next to country

reducedcolumnsMATERN <- reducedcolumnsMATERN[c(1:2, 5:7, 3:4)]



#writing out infant/maternal mortality data frames to .csv files

path_out <- "C:/Users/seshu/Documents/RStudio/projects/git/Blog-HealthAndJusticeLeague/data"

write_csv(x = reducedcolumnsINFANT, 
          path = paste0(path_out,"/wrangled_infantmortality.csv"))

write_csv(x = reducedcolumnsMATERN, 
          path = paste0(path_out,"/wrangled_maternalmortality.csv"))

