library(leaflet)
library(maps)
library(readr)
library(dplyr)
library(tidyverse)
library(janitor)
library(countrycode)
library(shiny)
library(shinythemes)
library(plotly)


######
# Coding done by Lillian
# This shiny app was published from my personal repo
# For some reason, the absolute path below worked for running the app
# but produced an error for publishing the app
# For publishing, we had to copy the dataset into the folder where
# app.R file was located, and use read_csv("DATASET.csv") with a relative path.
# If you want to reproduce the analysis AND re-publish the app, do the same, 
# i.e. copy the dataset into the same folder and use read_csv("/globaltemperature_all.csv")
######

# calling in the datasets
my_path <- "C:/Users/Yesuel Kim/Documents/Git/Blog-HealthAndJusticeLeague/data"

gtemp <- read_csv(paste0(my_path, "/globaltemperature_all.csv")) %>%
  mutate(year = as.numeric(substr(period, 1,4)))

h <- ggplot(data = gtemp,
            aes(x = as.POSIXct(period, origin = "1970-01-01"), y = temp)) +
  geom_line(size = 0.2) +
  geom_hline(aes(yintercept = 0), color = "red", size = 0.2, linetype = 2) +
  geom_smooth(alpha = 0.5, size = 1, linetype = 1) +
  theme_classic() +
  theme(text = element_text(size = 15))+
  labs(title = "Compared to 20th century average temperature") +
  scale_x_datetime(name = "Years") +
  scale_y_continuous(name = "Global temperature (celsius)")

ui <- fluidPage(theme = shinytheme("cerulean"), 
                titlePanel("Change in Global Temperature"),
                fluidRow(
                  column(offset = 1, width = 11,
                         sliderInput(inputId = "tempYear",
                                     label = "Select the year",
                                     min = 1880,
                                     max = 2020,
                                     value = 2000,
                                     sep = "",
                                     ticks = TRUE,
                                     width = "90%"))
                ),
                fluidRow(
                  column(offset = 1, width = 11,
                         plotOutput("tempPlot", width = "90%")))
                )



server <- function(input, output){
  
  gtemp_react <- reactive({
    
    sample <- gtemp %>%
      filter(year == input$tempYear) %>%
      arrange(period)
    
    ## to highlight region data
    data.frame(start=sample[1,1],
               end=sample[nrow(sample), 1])
    
  })
  
  output$tempPlot <- renderPlot({
    
    h +
      geom_rect(inherit.aes = FALSE,
                aes(xmin=as.POSIXct(gtemp_react()[1,1]), 
                    xmax=as.POSIXct(gtemp_react()[1,2]), 
                    ymin=-1.0, ymax=1.5), 
                fill="yellow", alpha=0.01)
    
  })

}

shinyApp(ui = ui, server = server)




