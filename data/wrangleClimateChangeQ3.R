
#load library
library(dplyr)
library(tidyverse)
library(readr)
library(readxl)
library(janitor)

#This library is new addition
install.packages("countrycode")
library(countrycode)
# This library has a function countrycode() that can convert long country names 
# into one of many different expressions 
# (country codes, German country name, short English name, etc.)
# help(countrycode)

my_path <- "C:/Users/seshu/Documents/RStudio/projects/git/Blog-HealthAndJusticeLeague"

climatedisaster_data <- read_csv(paste0(my_path,"/data/wrangled_natdisasters_byyear.csv"))

names(climatedisaster_data)

# We will add the 3-character official country code into the dataset
climatedis2 <- climatedisaster_data %>%
  mutate(isocode = countrycode(climatedisaster_data$country,
                               # Change long english country name into
                               origin = "country.name", 
                               # 3-character official country code
                               destination = "iso3c"))

# You will get the error saying:
# Some values were not matched unambiguously: Azores Islands, Canary Is, 
# Czechoslovakia, Netherlands Antilles, Serbia Montenegro, Yemen Arab Rep, 
# Yemen P Dem Rep, Yugoslavia
unique((climatedis2 %>% filter(is.na(isocode)))$country)

# Hardcoding using case_when() is the only way to solve this problem for 
# these 6 regions
climatedis_fin <- climatedis2 %>%
  mutate(isocode_matched = case_when(
    country == "Czechoslovakia" ~ "CZE",
    # It actually turns out that 5 regions except for Czechoslovakia are not
    # even present in the mortality data, so we do not need to match isocode
    # for those 5 regions anyways
    TRUE ~ isocode)) %>%
  select(-isocode) %>%
  rename(isocode = isocode_matched)

# Note that the official iso code is different from the country code
# in the original dataset for some countries (e.g. Russia - Soviet Union)
View(climatedis_fin %>%
       filter(isocode != countrycode) %>%
       select(isocode, countrycode, country))

# We will do the same for the mortality data
countrymort_data <- read_csv(paste0(my_path,"/data/wrangled_infmatmortline.csv"))

mortality2 <- countrymort_data %>%
  mutate(isocode = countrycode(countrymort_data$Country, origin = "country.name",
                               destination = "iso3c"))

# Now we have the same standardized official country code in both datasets.
# Therefore, I will try to filter the climate disasters data based on
# the isocode present in mortality dataset
# This way we don't have to deal with USA/United States/United States of America problem

isocode_mortality <- unique(mortality2$isocode)

reducedclimatedis <- climatedis_fin %>% 
  select(year, country, isocode, disaster_type, occurrence) %>%
  filter(isocode %in% isocode_mortality
         , year %in% c(2010:2018)) %>%
  pivot_wider(id_cols = c(year, country, isocode)
              , names_from = disaster_type
              , values_from = occurrence)

write_csv(reducedclimatedis, paste0(my_path,"/data/wrangled_climateq3.csv"))
write_csv(mortality2, paste0(my_path,"/data/wrangled_finalinfmatmortline.csv"))

# I commented out your work so you can recover them whenever you want to
######
# 
# #load library
# library(dplyr)
# library(tidyverse)
# library(readr)
# library(readxl)
# library(janitor)
# 
# 
# my_path <- "C:/Users/seshu/Documents/RStudio/projects/git/Blog-HealthAndJusticeLeague"
# 
# climatedisaster_data <- read_csv(paste0(my_path,"/data/wrangled_natdisasters_byyear.csv"))
# 
# climatedisaster_data$country <- gsub("Czech Republic (the)", "Czech Republic", climatedisaster_data$country)
# climatedisaster_data$country <- gsub("Korea (the Republic of)", "Korea", climatedisaster_data$country)
# climatedisaster_data$country <- gsub("Netherlands (the)", "Netherlands", climatedisaster_data$country)
# climatedisaster_data$country <- gsub("Slovakia", "Slovakia Republic", climatedisaster_data$country)
# climatedisaster_data$country <- gsub("United Kingdom of Great Britain and Northern Ireland (the)", "United Kingdom", climatedisaster_data$country)
# climatedisaster_data$country <- gsub("United States of America (the)", "United States", climatedisaster_data$country)
# 
# 
# countrymort_data <- read_csv(paste0(my_path,"/data/wrangled_infmatmortline.csv"))
# 
# distinctcountrycode <- unique(countrymort_data$COU)
# 
# reducedclimatedisaster_data <- climatedisaster_data %>% 
#   select(c(1:4, 9)) %>%
#   filter(c(countrycode %in% distinctcountrycode)
#          , year %in% c(1980:2018)) %>%
#   pivot_wider(id_cols = c(year, country, countrycode)
#               , names_from = disaster_type
#               , values_from = occurrence)
# 
# write_csv(reducedclimatedisaster_data, paste0(my_path,"/data/wrangled_climateq3.csv"))
          
######
