
#load library
library(dplyr)
library(tidyverse)
library(readr)
library(readxl)
library(janitor)

#FIXME: This library is new addition
library(countrycode)
# This library has a function countrycode() that can convert long country names 
# into one of many different expressions 
# (country codes, German country name, short English name, etc.)
# help(countrycode)

my_path <- "C:/Users/Yesuel Kim/Documents/Git/Blog-HealthAndJusticeLeague"

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
         , year %in% c(1980:2018)) %>%
  pivot_wider(id_cols = c(year, country, isocode)
              , names_from = disaster_type
              , values_from = occurrence)

write_csv(reducedclimatedisaster_data, paste0(my_path,"/data/wrangled_climateq3.csv"))


