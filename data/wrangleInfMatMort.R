#wrangling done by Mythili


#PLANS:
####line graph in shiny that allows user to select countries and select either infant or maternal mortality
###########the infant or maternal mortality will be graphed over time

####k-means cluster that shows inf vs. mat mortality in 1980s vs 2010s
###########normalize values (subtract mean, divide by standard deviation or something)
###########will show developing vs. developed countries + how climate change affects them

####line graph in shiny that allows user to select countries
###########will show extreme temps/whatever affects agricultural societies over time
###########note all the inf/mat morts improve, 
#################but maybe their values are still worse in certain countries vs. others when compared to temp

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
  rename(deaths_per_1000_live_births = 'Value') %>%
  mutate(decade = `Year` - `Year` %% 10)



#informatively naming columns; joining maternal mortality data w/ lat & long data

reducedcolumnsMATERN <- origMATERN %>%
  select(c(5:6, 8:9)) %>%
  rename(deaths_per_100000_live_births = 'Value') %>%
  mutate(decade = `Year` - `Year` %% 10)


#combining tables for clustering
infmatcluster <- reducedcolumnsINFANT %>%
  right_join(reducedcolumnsMATERN, by = c("COU", "Country", "Year", "decade")) %>%
  na.omit() %>%
  group_by(COU, Country, decade) %>%
  summarize(avginfmort = mean(deaths_per_1000_live_births)
            , avmatmort = mean(deaths_per_100000_live_births)) %>%
  arrange(decade)

cluster1980s <- infmatcombined %>%
  filter(decade %in% 1980)

cluster2010s <- infmatcombined %>%
  filter(decade %in% 2010)

#combined tables for line graphs
infmatline <- reducedcolumnsINFANT %>%
  right_join(reducedcolumnsMATERN, by = c("COU", "Country", "Year", "decade")) %>%
  na.omit() %>%
  mutate(NetChange_Infant = case_when(Country == lag(Country) ~ deaths_per_1000_live_births - lag(deaths_per_1000_live_births)
                                    , Country != lag(Country) ~ NA_real_)
         , NetChange_Maternal = case_when(Country == lag(Country) ~ deaths_per_100000_live_births - lag(deaths_per_100000_live_births)
                                        , Country != lag(Country) ~ NA_real_)) %>%
  select(c(1:3, 7:8))


#writing out infant/maternal mortality data frames to .csv files

path_out <- "C:/Users/seshu/Documents/RStudio/projects/git/Blog-HealthAndJusticeLeague/data"

write_csv(x = cluster1980s, 
          path = paste0(path_out,"/wrangled_infmatmortcluster1980s.csv"))

write_csv(x = cluster2010s, 
          path = paste0(path_out,"/wrangled_infmatmortcluster2010s.csv"))

write_csv(x = infmatline, 
          path = paste0(path_out,"/wrangled_infmatmortline.csv"))

