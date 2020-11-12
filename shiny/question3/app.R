

#coding done by Mythili

library(shinythemes)
library(shiny)
library(tidyverse)
library(janitor)
library(lubridate)

path_in <- "C:/Users/seshu/Documents/RStudio/projects/git/Blog-HealthAndJusticeLeague/data"

infmatmortdata <- read_csv(paste0(path_in,"/wrangled_infmatmortline.csv"))
natdisastdata <- read_csv(paste0(path_in,"/wrangled_climateq3.csv"))

country_choices <- (infmatmortdata %>%
                      count(isocode))$isocode
country_choice_names <- unique(infmatmortdata$isocode)
names(country_choices) <- country_choice_names

mort_choices_values = names(infmatmortdata)[4:5]
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
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Explore different variables to view how the frequency of 
         climate disasters and infant/maternal mortality are related!"),
      
      selectizeInput(inputId = "Countries"
                     , label = "Choose two countries to display data for: "
                     , choices = country_choices
                     , selected = NULL
                     , multiple = TRUE
                     , options = list(maxItems = 2)

      ),
      
      selectInput(inputId = "mort"
                  , label = "Choose a predictor variable of interest:"
                  , choices = mort_choices_values
                  , selected = infmatmortdata$NetChange_Infant
      ),
      
      selectInput(inputId = "disastfreq"
                  , label = "Choose a predictor variable of interest:"
                  , choices = mort_choices_values
                  , selected = infmatmortdata$NetChange_Infant)
      
    ),
    
    
    
    mainPanel(
      
      plotOutput(outputId = "bar1")
      , plotOutput(OutputId = "bar2")
      
    )
  )
)


# server
server <- function(input,output){
  
  use_data1_q3 <- reactive({
    data1 <- filter(infmatmortdata, isocode %in% input$Countries)
  })
  

  output$bar1 <- renderPlot({
    ggplot(data = use_data1_q3(), aes_string(x = "Year", y = input$mort)) +
      geom_bar(position = "dodge", stat = "identity", aes_string(fill = "Country")) +
      labs(x = "Year", y = "Net Change in Mortality Per Month"
           , title = "Infant and Maternal Mortality Worldwide from 1980-2018") +
      theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) + 
      facet_wrap(~ isocode, ncol = 1)

  })
  
  output$bar2 <- renderPlot({
    ggplot(data = use_data1_q3(), aes_string(x = "Year", y = input$disastfreq)) +
      geom_bar(position = "dodge", stat = "identity", aes_string(fill = "isocode")) +
      labs(x = "Year", y = "Frequency of Chosen Natural Disaster"
           , title = "Natural Disaster Occurrences Worldwide Between 1980 and 2018") +
      theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) + 
      facet_wrap(~ Country, ncol = 1)
    
  })
  
}


# call to shinyApp
shinyApp(ui = ui, server = server)


