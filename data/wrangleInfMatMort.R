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



#informatively naming columns; joining infant mortality data w/ lat & long data

reducedcolumnsINFANT <- origINFANT %>%
  select(c(5:6, 8:9)) %>%
  rename(deaths_per_1000_live_births = 'Value')



#informatively naming columns; joining maternal mortality data w/ lat & long data

reducedcolumnsMATERN <- origMATERN %>%
  select(c(5:6, 8:9)) %>%
  rename(deaths_per_100000_live_births = 'Value')



#combining tables for clustering
infmatcombined <- reducedcolumnsINFANT %>%
  right_join(reducedcolumnsMATERN, by = c("COU", "Country", "Year"))



#writing out infant/maternal mortality data frames to .csv files

path_out <- "C:/Users/seshu/Documents/RStudio/projects/git/Blog-HealthAndJusticeLeague/data"

write_csv(x = infmatcombined, 
          path = paste0(path_out,"/wrangled_infmatmortality.csv"))

