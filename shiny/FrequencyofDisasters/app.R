
library(tidyverse)
library(leaflet)
library(maps)
library(readr)
library(dplyr)
library(janitor)
library(ggplot2)
library(plotly)
library(shiny)
library(shinythemes)
library(countrycode)
library(viridis)

######
# Coding done by Lillian
# This shiny app was published from my personal repo
# For some reason, the absolute path below worked for running the app
# but produced an error for publishing the app
# For publishing, we had to copy the dataset into the folder where
# app.R file was located, and use read_csv("DATASET.csv") with a relative path.
# If you want to reproduce the analysis AND re-publish the app, do the same, 
# i.e. copy the dataset into the same folder and use read_csv("wrangled_natdisasters_byyear.csv")
######

my_path <- "C:/Users/Yesuel Kim/Documents/Git/Blog-HealthAndJusticeLeague/data"

# calling in the datasets

# natural disaster dataset
natdis <- read_csv(paste0(my_path,"/wrangled_natdisasters_byyear.csv")) %>%
  filter(disaster_type != "All") %>%
  select(year, disaster_type, occurrence) %>%
  group_by(year, disaster_type) %>%
  summarize(total_occur = sum(occurrence))

# HTML Text: background info to display on the side
text <- sprintf("<br/><strong>%s</strong>%s<br/><br/>%s<br/>%s<br/>%s<br/>%s<br/>%s<br/><br/>",
                "Source: EM-DAT",
                a(href="https://www.emdat.be/", " (The International Disaster Database)"),
                "This app only shows the natural disasters that fulfill the following criteria:",
                "a) 10 or more reported killed; or",
                "b) 100 or more reported affected; or",
                "c) A state of emergency declared; or",
                "d) International assistance requested")%>% 
  lapply(htmltools::HTML)



# for checkboxGroupInput choices
relevant <- unique(natdis$disaster_type)


shinyApp(
  
  ui = fluidPage(theme = shinytheme("cerulean"), 
                 titlePanel("Frequency of Impactful Natural Disasters"),
                 
                 fluidRow(
                   column(offset = 1, width = 11,
                          # disaster type
                          checkboxGroupInput(inputId = "disaster",
                                             label = "Select disaster(s)",
                                             choices = relevant,
                                             selected = relevant,
                                             inline = TRUE))
                 ),
                 
                 fluidRow(
                   column(offset = 1, width = 10, 
                          plotlyOutput("freqplot")
                   )
                 ),
                 
                 fluidRow(
                   column(offset = 1, width = 11, text)
                 )
  ),
  
  server = function(input, output) {
    # natural disasters of a given type in a given year
    natdis_react <- reactive({
      natdis %>%
        filter(disaster_type %in% input$disaster)
    })
    
    
    output$freqplot <- renderPlotly({
      ggplot(data = natdis_react()) +
        geom_area(aes(x = year, y = total_occur, fill = disaster_type),
                  position = "stack") + 
        theme_classic() + 
        labs(x = "Year", y = "Total occurrence of Disasters") + 
        scale_fill_viridis_d(name = "Disaster Type")
      
    })
    
  },
  
  options = list(height = 500)
)
