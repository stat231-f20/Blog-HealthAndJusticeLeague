

#coding done by Mythili

library(shinythemes)
library(shiny)
library(tidyverse)
library(janitor)
library(lubridate)

path_in <- "C:/Users/seshu/Documents/RStudio/projects/git/Blog-HealthAndJusticeLeague/data"

infmatmortdata <- read_csv(paste0(path_in,"/wrangled_infmatmortline.csv"))

country_choices <- (infmatmortdata %>%
                      count(Country))$Country
country_choice_names <- unique(infmatmortdata$Country)
names(country_choices) <- country_choice_names

mort_choices_values = names(infmatmortdata)[4:5]
mort_choices_names <- c("Net Change in Infant Mortality", "Net Change in Maternal Mortality")
names(mort_choices) <- mort_choices_names


# ui 
ui <- fluidPage(
  
  h1("Climate Disaster Effects on Infant and Maternal Mortality"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Choose countries to view how the frequency of 
         climate disasters and infant/maternal mortality are related!"),
      
      checkboxGroupInput(inputId = "Countries"
                         , label = "Choose countries to display data for: "
                         , choices = country_choices
                         , selected = NULL
                         , inline = TRUE),
      selectInput(inputId = "y"
                  , label = "Choose a predictor variable of interest:"
                  , choices = mort_choices_values
                  , selected = infmatmortdata$NetChange_Infant)
    ),
    
    mainPanel(
      
      plotOutput(outputId = "bar1")
      
    )
  )
)


# server
server <- function(input,output){
  
  use_data1_q3 <- reactive({
    data1 <- filter(infmatmortdata, Country %in% input$Countries)
  })
  

  output$bar1 <- renderPlot({
    ggplot(data = use_data1_q3(), aes(x = Year, y = input$y)) +
      geom_bar(position = "dodge", stat = "identity", aes(fill = input$Countries)) +
      labs(x = "YEar", y = "Net Change in Mortality Per Month"
           , title = "Infant and Maternal Mortality Worldwide from 1980-2018") +
      theme(legend.position="bottom", plot.title = element_text(hjust = 0.5))
      # scale_fill_discrete(name="Condition Recorded"
      #                     , breaks=c("monthlydeathincrease", "monthlynetincrease")
      #                     , labels=c("Deaths", "Cases"))

  })
  
}


# call to shinyApp
shinyApp(ui = ui, server = server)


