library(tidyverse)
library(readr)
library(dplyr)

#this file contains a collection of additional wrangling I had to do
#for creating leaflets and other visualizations
path_bella <- "C:/Users/Bella/Desktop/git/Blog-HealthAndJusticeLeague/data"

data_healthimp <- read_csv(paste0(path_bella,"/wrangled_HEimpacts.csv"))
data_air <- read_csv(paste0(path_bella, "/wrangled_PM25popexp.csv"))

#more wrangling smH
##renaming column variable name to convert to string
data_air <- data_air%>%
  rename(Y1990 = "1990",
         Y2017 = "2017")

data_air$change <- data_air$Y1990 - data_air$Y2017

##need to match the country name
data_air[16, 2] <- "South Korea"
data_air[24, 2] <- "Slovakia"
data_air[28, 2] <- "UK"
data_air[29, 2] <- "USA"
data_air[61, 2] <- "China"
data_air[64, 2] <- "Republic of Congo"
data_air[66, 2] <- "Ivory Coast"
data_air[69, 2] <- "North Korea"
data_air[103, 2] <- "Laos"
data_air[111, 2] <- "Macedonia"
data_air[162, 2] <- "Syria"

##further wrangling the health impact data
data_healthimp_wide <- data_healthimp%>%
  pivot_wider(values_from = value,
              names_from = c(risk, measure),
              names_glue = "{risk}.{measure}")
data_healthimp_wide <- data_healthimp_wide[(data_healthimp_wide$age == "all"), ]
data_healthimp_wide <- data_healthimp_wide[, -5]

###ambient PM2.5 pdeathsper1M only
data_pdeaths_amb <- data_healthimp_wide[, c(1, 2, 3, 4, 6)]
data_pdeaths_amb <- data_pdeaths_amb%>%
  pivot_wider(values_from = `ambientpm2.5.pdeathsper1m`,
              names_from = sex, 
              names_glue = "{sex}")
data_pdeaths_amb$pdeathsper1m <- data_pdeaths_amb$`F` + data_pdeaths_amb$M

data_pdeaths_amb <- data_pdeaths_amb%>%
  select(-c(`F`, M))%>%
  pivot_wider(values_from = `pdeathsper1m`,
              names_from = year,
              names_glue = "Y{year}")

###need to match the data AGAIN SMHHHHHHHHHHHHH
data_pdeaths_amb[163, 2] <- "South Korea"
data_pdeaths_amb[164, 2] <- "Slovakia"
data_pdeaths_amb[181, 2] <- "UK"
data_pdeaths_amb[195, 2] <- "USA"
data_pdeaths_amb[41, 2] <- "China"
data_pdeaths_amb[21, 2] <- "Republic of Congo"
data_pdeaths_amb[45, 2] <- "Ivory Coast"
data_pdeaths_amb[48, 2] <- "North Korea"
data_pdeaths_amb[103, 2] <- "Laos"
data_pdeaths_amb[104, 2] <- "Macedonia"
data_pdeaths_amb[170, 2] <- "Syria"

###DALY only
data_DALY <- data_healthimp_wide[, c(1, 2, 3, 4, 5)]
data_DALY <- data_DALY%>%
  pivot_wider(values_from = `ambientpm2.5.dalyper1k`,
              names_from = sex, 
              names_glue = "{sex}")
data_DALY$DALY <- data_DALY$`F` + data_DALY$M

data_DALY <- data_DALY%>%
  select(-c(`F`, M))%>%
  pivot_wider(values_from = `DALY`,
              names_from = year,
              names_glue = "Y{year}")

data_DALY$change <- data_DALY$Y1990 - data_DALY$Y2017

###need to match the data AGAIN SMHHHHHHHHHHHHH
data_DALY[163, 2] <- "South Korea"
data_DALY[164, 2] <- "Slovakia"
data_DALY[181, 2] <- "UK"
data_DALY[195, 2] <- "USA"
data_DALY[41, 2] <- "China"
data_DALY[21, 2] <- "Republic of Congo"
data_DALY[45, 2] <- "Ivory Coast"
data_DALY[48, 2] <- "North Korea"
data_DALY[103, 2] <- "Laos"
data_DALY[104, 2] <- "Macedonia"
data_DALY[170, 2] <- "Syria"

data_DALYonly <- data_healthimp_wide[, c(1, 2, 3, 4, 5)]
data_DALYonly <- data_DALYonly%>%
  pivot_wider(values_from = `ambientpm2.5.dalyper1k`,
              names_from = sex,
              names_glue = "{sex}")
data_DALYonly$DALY <- data_DALYonly$`F` + data_DALYonly$M

data_DALYonly <- data_DALYonly%>%
  select(-c(`F`, M))
data_DALYonly <- data_DALYonly[data_DALYonly$year == 1990 | 
                                 data_DALYonly$year == 1995| 
                                 data_DALYonly$year == 2000| data_DALYonly$year == 2005|
                                 data_DALYonly$year == 2010| data_DALYonly$year == 2011| 
                                 data_DALYonly$year == 2012| data_DALYonly$year == 2013| 
                                 data_DALYonly$year == 2014| data_DALYonly$year == 2015| 
                                 data_DALYonly$year == 2016| data_DALYonly$year == 2017, ]

path_bella <- "C:/Users/Bella/Desktop/git/Blog-HealthAndJusticeLeague/data"
data_PM25 <- read_csv(paste0(path_bella,
                             "/Exposure_PM25_air.csv"))

#taking out unnecessary column varaibles
data_PM25 <- data_PM25%>%
  select(COU, Country, Variable, Year, Value)%>%
  rename(country = Country,
         year = Year)

#separate out the variables
data_PM25_wide <- data_PM25%>%
  pivot_wider(values_from = Value,
              names_from = Variable,
              names_glue = "{Variable}")

data_joined <- left_join(data_DALYonly, data_PM25_wide, by = c("country", "year"), copy = TRUE)%>%
  select(-c(COU, cou))%>%
  rename(PM25exposure = `Mean population exposure to PM2.5`)

data_joined <- data_joined[, -c(5, 6, 7, 8)]

data_joined <- data_joined%>%
  filter(is.na(DALY) == FALSE)%>%
  filter(is.na(PM25exposure) == FALSE)



write_csv(x = data_air, 
          path = paste0(path_bella,"/wrangled_air_final.csv"))
write_csv(x = data_DALY, 
          path = paste0(path_bella,"/wrangled_DALY_final.csv"))
write_csv(x = data_joined, 
          path = paste0(path_bella,"/joined_data_PM25_DALY.csv"))


