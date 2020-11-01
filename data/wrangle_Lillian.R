#load library
library(dplyr)
library(tidyverse)
library(readr)
library(janitor)


# WEBSITE: https://stats.oecd.org/Index.aspx?
# select theme: Environment - Environmental risks and health

######################################

#TODO: Basic information about this dataset
# This dataset is on the various health and economic impacts of air pollution, 
# more specifically pm2.5 in ambient and residential air.
# There are 4 different outcome variables
# 1. DALYs, per thousand inhabitants
# 2. Premature deaths, per million inhabitants
# 3. Welfare cost of premature deaths, in 2010 USD per capita (unit: 2010USD)
# 4. Welfare cost of premature deaths in % GDP equivalent (unit: %)

#####################################

# read in the csv file
my_path <- "C:/Users/Yesuel Kim/Documents/Git/Blog-HealthAndJusticeLeague"
data <- read_csv(paste0(my_path, "/data/Health_Economic_impacts.csv"))

# see how the dataset looks
glimpse(data)
# These variables are redundant
nrow(data %>% filter(YEA != Year))
nrow(data %>% filter(tolower(SEX) != tolower(Sex)))
unique(data$AGE)
unique(data$Age)
unique(data$RISK)
unique(data$Risk)
unique(data$`Unit Code`)
unique(data$Unit)
unique(data$VAR)
unique(data$Variable)

# These columns are not needed
unique(data$`PowerCode Code`)
unique(data$PowerCode)

# remove unnecessary/redundant variables
data1 <- data %>%
  select(-c(AGE, YEA, SEX, Risk, `Unit Code`, `PowerCode Code`, 
            PowerCode, `Flag Codes`, Flags, VAR, `Reference Period Code`,
            `Reference Period`))

# more intuitive variable names and levels

data2 <- data1 %>%
  mutate(measure = case_when(
    Variable == "DALYs, per thousand inhabitants" ~ "dalyper1k",
    Variable == "Premature deaths, per million inhabitants" ~ "pdeathsper1m",
    Variable == "Welfare cost of premature deaths, USD per capita" ~ "2010wcostpercapita",
    Variable == "Welfare cost of premature deaths, % GDP equivalent" ~ "wcostgdppercent"
  )) %>%
  mutate(risk = case_when(
    RISK == "PM_2_5_OUT" ~ "ambientpm2.5",
    RISK == "PM_2_5_IN" ~ "residpm2.5"
  )) %>%
  mutate(sex = case_when(
    Sex == "Female" ~ "F",
    Sex == "Male" ~ "M"
  )) %>%
  mutate(age = case_when(
    Age == "ALL" ~ "all",
    Age == "Less than 15 years old" ~ "_15",
    Age == "15 to 64 years old" ~ "15_64",
    Age == "More than 64 years old" ~ "64_"
  )) %>%
  select(-c(Variable, RISK, Unit, Sex, Age)) %>%
  # column names to lower cases
  clean_names() %>%
  # rearrange columns
  select(year, cou, country, measure, value, risk, sex, age)

write_csv(x = data2, 
          path = paste0(my_path,"/data/wrangled_HEimpacts.csv"))
  
