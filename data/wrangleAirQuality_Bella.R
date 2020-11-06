#loading packages
library(tidyverse)
library(readr)
library(dplyr)

#**please look at the bottom of codes for detailed info on the original and wrangled data**

#importing original data
path_bella <- "C:/Users/Bella/Desktop/git/Blog-HealthAndJusticeLeague/data"
data_PM25 <- read_csv(paste0(path_bella,
                             "/Exposure_PM25_air.csv"))

#taking out unnecessary column varaibles
data_PM25 <- data_PM25%>%
  select(COU, Country, Variable, Year, Value)

#separate out the variables
data_PM25_wide <- data_PM25%>%
  pivot_wider(values_from = Value,
              names_from = Variable,
              names_glue = "{Variable}")

#exclude all variables but mean population exposure to PM2.5
data_PM25_popexp <- data_PM25_wide[, -c(5, 6, 7 ,8)]
data_PM25_popexp <- data_PM25_popexp%>%
  pivot_wider(values_from = `Mean population exposure to PM2.5`,
              names_from = Year,
              names_glue = "{Year}")

#exclude all variables but the 10 µg/m3
data_PM25_10 <- data_PM25_wide[, -c(4, 6, 7 ,8)]
data_PM25_10 <- data_PM25_10%>%
  pivot_wider(values_from = `Percentage of population exposed to more than 10 micrograms/m3`,
              names_from = Year,
              names_glue = "{Year}")

#exclude all variables but the 15 µg/m3
data_PM25_15 <- data_PM25_wide[, -c(4, 5, 7 ,8)]
data_PM25_15 <- data_PM25_15%>%
  pivot_wider(values_from = `Percentage of population exposed to more than 15 micrograms/m3`,
              names_from = Year,
              names_glue = "{Year}")

#exclude all variables but the 25 µg/m3
data_PM25_25 <- data_PM25_wide[, -c(4, 5, 6 ,8)]
data_PM25_25 <- data_PM25_25%>%
  pivot_wider(values_from = `Percentage of population exposed to more than 25 micrograms/m3`,
              names_from = Year,
              names_glue = "{Year}")

#exclude all variables but the 35 µg/m3
data_PM25_35 <- data_PM25_wide[, -c(4, 5, 6 ,7)]
data_PM25_35 <- data_PM25_35%>%
  pivot_wider(values_from = `Percentage of population exposed to more than 35 micrograms/m3`,
              names_from = Year,
              names_glue = "{Year}")

#final datasets to be used (look below for detailed descriptions):
PM25_all_var <- data_PM25_Wide
PM25_pop_exposure <- data_PM25_popexp
PM25_exposure10 <- data_PM25_10
PM25_exposure15 <- data_PM25_15
PM25_exposure25 <- data_PM25_25
PM25_exposure35 <- data_PM25_35

write_csv(x = PM25_pop_exposure, 
          path = paste0(path_bella,"/wrangled_PM25popexp.csv"))

#------------------------------F--Y--I-----------------------------------------

#Source of original data: https://stats.oecd.org/index.aspx?queryid=72722

# I. Descriptons of final datasets to be used in further coding:

## "PM25_all_var" contains data for the following variables (by country): 
### 1) Mean population exposure to PM2.5 (µg/m3)
### 2) Percentage of population exposed to more than 10 µg/m3
### 3) Percentage of population exposed to more than 15 µg/m3
### 4) Percentage of population exposed to more than 25 µg/m3
### 5) Percentage of population exposed to more than 35 µg/m3

## "PM25_pop_exposures" contains data for all countries but for only variable 1) (by year)
## "PM25_exposure10" contains data for all countries but for only variable 2) (by year)
## "PM25_exposure15" contains data for all countries but for only variable 3) (by year)
## "PM25_exposure25" contains data for all countries but for only variable 4) (by year)
## "PM25_exposure35" contains data for all countries but for only variable 5) (by year)

# II. Descriptions of variables being used:

## 1) Mean population exposure to outdoor PM2.5 (µg/m3): calculated as the mean annual
## outdoor PM2.5 concentration weighted by population living in the relevant
## area, that is, the concentration level, expressed in µg/m3,
## to which a typical resident is exposed throughout a year.

### This is often the preferred indicator, for two reasons: 
### 1) as a continuous variable, it allows capturing even minor changes in exposures, and
### 2) it allows summarizing exposure of the entire population of a country. 

## However, the indicators listed below might be easier to communicate. 

## Percentage of population exposed to more than 10 µg/m3: the proportion of
## people living in areas with annual concentrations exceeding the WHO Air
## Quality Guideline (AQG) value of 10 micrograms per cubic meter.

## Percentage of population exposed to more than 15/25/35 µg/m3: the proportion of
## people living in areas with annual concentrations exceeding the WHO Interim
## target-3/2/1 value of 15/25/35 micrograms per cubic meter.

#------------------------------------------------------------------------------------------