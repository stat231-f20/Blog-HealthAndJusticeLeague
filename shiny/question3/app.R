
library(shinythemes)
library(shiny)
library(tidyverse)
library(janitor)
library(lubridate)

path_in <- "C:/Users/seshu/Documents/RStudio/projects/git/Blog-HealthAndJusticeLeague/data"

infmatmortdata <- read_csv(paste0(path_in,"/wrangled_infmatmortline.csv"))

country_choices <- (infmatmortdata%>%
                    count(country))$State
state_choice_names <- c("Arizona", "California", "Colorado", "District of Columbia", "Delaware", "Florida", "Georgia", "Illinois", "Maryland", "Texas", "Virginia")
names(state_choices) <- state_choice_names


# ui 
ui <- fluidPage(
  
  h1("Covid Case Growth and New Voter Registration Pre- and During the Pandemic"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Choose a state, then click Plot to see how New Voter 
         Registration, COVID Cases, and COVID-Related Deaths have changed in 2020!"),
      selectInput(inputId = "state"
                  , label = "Choose a State of Interest:"
                  , choices = state_choices
                  , selected = "AZ"), 
      actionButton("go", "Plot", icon("fas fa-ambulance"), 
                   style="color: #fff; background-color: grey; border-color: grey"),
    ),
    
    mainPanel(
      
      plotOutput(outputId = "bar1")
      , plotOutput(outputId = "bar2")
      
    )
  )
)


# server
server <- function(input,output){
  
  use_data_tab1 <- eventReactive(input$go, {
    filter(coviddata4, State %in% input$state)
  })
  
  use_data2_tab1 <- eventReactive(input$go, {
    filter(votedata2, State %in% input$state)
  })
  
  output$bar1 <- renderPlot({
    ggplot(data = use_data_tab1(), aes(x = month, y = nCases)) +
      geom_bar(position = "dodge", stat = "identity", aes(fill = Casetype)) +
      labs(x = "Month", y = "Net Increase in Instances Per Month"
           , title = "Covid Case and Death Growth in 2020") + 
      theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) + 
      scale_fill_discrete(name="Condition Recorded"
                          , breaks=c("monthlydeathincrease", "monthlynetincrease")
                          , labels=c("Deaths", "Cases"))
    
  })
  
  
  output$bar2 <- renderPlot({
    ggplot(data = use_data2_tab1(), aes(x = month, y = newvoters)) + 
      geom_bar(stat = "identity", fill = "darkgrey") + 
      labs(x = "Month", y = "Number of Newly Registered Voters"
           , title = "Voter Registration in 2020") + 
      scale_x_continuous(breaks = c(4,5,6,7)) + 
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  
}


# call to shinyApp
shinyApp(ui = ui, server = server)


