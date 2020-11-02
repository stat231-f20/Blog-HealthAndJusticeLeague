

#loading packages
library(readr)
library(dplyr)
library(janitor)
library(tidyverse)

path_in <- "C:/Users/seshu/Documents/RStudio/projects/git/Blog-HealthAndJusticeLeague/data"

origINFANT <- read_csv(paste0(path_in,"/infantmortalityorig.csv"))
origMATERN <- read_csv(paste0(path_in,"/maternalmortalityorig.csv"))

reducedcolumnsINFANT <- origINFANT %>%
  select(c(4:6, 8:9)) %>%
  rename(deaths_per_1000_live_births = 'Value') %>%
  mutate(percentmortinf = (deaths_per_1000_live_births/1000)*100)

reducedcolumnsMATERN <- origMATERN %>%
  select(c(4:6, 8:9)) %>%
  rename(deaths_per_100000_live_births = 'Value') %>%
  mutate(percentmortmat = (deaths_per_100000_live_births/100000)*100)