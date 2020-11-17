

#coding done by Mythili

library(shinythemes)
library(shiny)
library(tidyverse)
library(janitor)
library(lubridate)
library(shinyWidgets)

## this absolute path and read_csv() line are needed to run the app.R file individually in our repo
path_in <- "C:/Users/seshu/Documents/RStudio/projects/git/Blog-HealthAndJusticeLeague/data"
infmatmortdata <- read_csv(paste0(path_in,"/wrangled_finalinfmatmortline.csv"))

## the following read_csv() with a relative path was needed to publish the app
## note: I published the app in my personal repo, but I kept the line here for reference
## I embedded the published shiny app link into our blog post

#infmatmortdata <- read_csv("wrangled_finalinfmatmortline.csv")

# natdisastdata <- read_csv("wrangled_climateq3.csv") %>%
#   rename("Extreme_temperature" = "Extreme temperature")

## minute data wrangling
infmatmortdata2 <- infmatmortdata %>%
  filter(Year %in% c(2010:2018))

natdisastdata <- read_csv(paste0(path_in,"/wrangled_climateq3.csv")) %>%
  rename("Extreme_temperature" = "Extreme temperature")

#creating country and variable choices for user to look at in shiny
country_choices <- (infmatmortdata2 %>%
                      count(isocode))$isocode

mort_choices_values = names(infmatmortdata2)[4:5]
mort_choices_names <- c("Net Change in Infant Mortality", "Net Change in Maternal Mortality")
names(mort_choices_values) <- mort_choices_names

disastfreq_choices_values = names(natdisastdata)[4:10]
disastfreq_choices_names <- c("All Disasters", "Storms", "Wildfires"
                              , "Floods", "Droughts", "Landslides"
                              , "Extreme Temperature Events")
names(disastfreq_choices_values) <- disastfreq_choices_names

# ui 
ui <- fluidPage(
  
  h1("Climate Disaster Effects on Infant and Maternal Mortality"),
  
  tabsetPanel(
    
    tabPanel("Countrywise Natural Disaster Frequency, Infant and Maternal Mortality", 
             sidebarLayout(
               sidebarPanel(
                 
                 h4("Explore different variables to view how the frequency of 
         climate disasters and infant/maternal mortality are related!"),
                 
                 selectizeInput(inputId = "Countries"
                                , label = "Choose two countries to display data for: "
                                , choices = country_choices
                                , selected = c("MEX", "CHE")
                                , multiple = TRUE
                                , options = list(maxItems = 2)
                                
                 ),
                 
                 selectInput(inputId = "mort"
                             , label = "Choose a predictor variable of interest:"
                             , choices = mort_choices_values
                             , selected = "NetChange_Infant"
                 ),
                 
                 selectInput(inputId = "disastfreq"
                             , label = "Choose a predictor variable of interest:"
                             , choices = disastfreq_choices_values
                             , selected = "All")
                 
               ),
               
               mainPanel(
                 
                 plotOutput(outputId = "bar1")
                 , plotOutput(outputId = "bar2")
                 
               )
             )
    ), 
    #so the user knows which three-letter abbreviation refers to which country
    tabPanel("Country Reference Codes",
             
             mainPanel(
               fluidRow(
                 column(width = 12, offset = 1, style = "padding: 10px",
                        dataTableOutput(output = "countryrefs"))
                 
               )
             )
    )
  )
  
)


# server
server <- function(input,output){
  
  use_data1_q3 <- reactive({
    data1 <- filter(infmatmortdata2, isocode %in% input$Countries)
  })
  
  use_data2_q3 <- reactive({
    data2 <- filter(natdisastdata, isocode %in% input$Countries)
  })
  
  
  output$bar1 <- renderPlot({
    ggplot(data = use_data1_q3(), aes_string(x = "Year", y = input$mort)) +
      geom_bar(position = "dodge", stat = "identity", aes_string(fill = "isocode")) +
      labs(x = "Year", y = "Net Change in Mortality Per Month"
           , title = "Infant and Maternal Mortality Worldwide from 1980-2018") + 
      theme_bw() + 
      theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)
            , axis.text.x = element_text(size = 15, hjust = .5, vjust = .5)
            , axis.text.y = element_text(size = 15, hjust = .5, vjust = .5)
            , text = element_text(size=15)) + 
      geom_hline(yintercept = 0, color = "black", size = 0.5) + 
      facet_wrap(~ isocode, ncol = 1)
    
  })
  
  output$bar2 <- renderPlot({
    ggplot(data = use_data2_q3(), aes_string(x = "year", y = input$disastfreq)) +
      geom_bar(position = "dodge", stat = "identity", aes_string(fill = "isocode")) +
      labs(x = "Year", y = "Frequency of Chosen Natural Disaster"
           , title = "Countrywise Natural Disaster Frequency (1980-2018)") + 
      theme_bw() + 
      theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)
            , axis.text.x = element_text(size = 15, hjust = .5, vjust = .5)
            , axis.text.y = element_text(size = 15, hjust = .5, vjust = .5)
            , text = element_text(size=15)) + 
      facet_wrap(~ isocode, ncol = 1)
    
  })
  
  output$countryrefs <- renderDataTable(unique(infmatmortdata[, c(6, 2)]))
  
}


# call to shinyApp
shinyApp(ui = ui, server = server)


