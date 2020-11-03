

#loading packages
library(readr)
library(dplyr)
library(janitor)
library(tidyverse)
library(maps)

path_in <- "C:/Users/seshu/Documents/RStudio/projects/git/Blog-HealthAndJusticeLeague/data"

origINFANT <- read_csv(paste0(path_in,"/infantmortalityorig.csv"))
origMATERN <- read_csv(paste0(path_in,"/maternalmortalityorig.csv"))

world_map <- map_data(map = "world", region = ".") %>%
  select(c(5, 1:2))

reducedcolumnsINFANT <- origINFANT %>%
  select(c(4:6, 8:9)) %>%
  rename(deaths_per_1000_live_births = 'Value') %>%
  right_join(world_map, by = c("Country" = "region"))

reducedcolumnsMATERN <- origMATERN %>%
  select(c(4:6, 8:9)) %>%
  rename(deaths_per_100000_live_births = 'Value') %>%
  right_join(world_map, by = c("Country" = "region"))


ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white") +
  # remove background color and ticks
  theme_void()  +
  # make aspect ratio fixed
  coord_fixed(ratio = 1.3) 