library(readr)
library(tidyverse)
library(dplyr)
library(janitor)
library(countrycode)

########
# Info #
########
# 	Average global mean temperature anomalies in degrees Celsius 
# relative to a base period. GISTEMP base period: 1951-1980. 
# GCAG base period: 20th century average


# source: https://datahub.io/core/co2-fossil-by-nation#resource-fossil-fuel-co2-emissions-by-nation
# Variables
# total: Total carbon emissions from fossil fuel consumption and cement production 
# (million metric tons of C)
# per_capita: Per capita carbon emissions (metric tons of carbon; after 1949 only)
########
data("iso3166")

my_path <- "C:/Users/Yesuel Kim/Documents/Git/Blog-HealthAndJusticeLeague"

gtemp <- read_csv(paste0(my_path, "/data/Global_temp.csv"))

temp1980 <- gtemp %>%
  filter(Year >= 1980) %>%
  select(c((1:13)))

temp_fin <- temp1980 %>%
  mutate_if(is.character, funs(as.numeric(.)))

temp_finlong <- temp_fin %>%
  pivot_longer(cols = c(2:13),
               names_to = "Month",
               values_to = "temp") %>%
  janitor::clean_names() %>%
  mutate(per = paste0(year, month)) %>%
  mutate(period = lubridate::ymd(per, truncated = 2)) %>%
  select(period, temp)

co2 <- read_csv(paste0(my_path, "/data/fossil-fuel-co2-emissions-by-nation.csv"))

co2_1980 <- co2 %>%
  filter(Year >= 1980) %>%
  clean_names() %>%
  select(year, country, total, per_capita) %>%
  mutate(country2 = tolower(country))

co2_fin <- co2_1980 %>%
  mutate(country = case_when(
    country2 == "france (including monaco)" ~ "france",
    country2 == "italy (including san marino)" ~ "italy",
    country2 == "st. kitts-nevis-anguilla" ~ "st. kitts-nevis",
    TRUE ~ country2)) %>%
  select(-country2) %>%
  mutate(isocode = countrycode(country,origin = "country.name", 
                               destination = "iso3c"))

write_csv(co2_fin, paste0(my_path, "/data/co2_bycountry.csv"))
write_csv(temp_fin, paste0(my_path, "/data/globaltemperature.csv"))
write_csv(temp_finlong, paste0(my_path, "/data/globaltemperature_long.csv"))



