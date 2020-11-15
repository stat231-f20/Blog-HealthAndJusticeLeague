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

# calling in the datasets
my_path <- "C:/Users/Yesuel Kim/Documents/Git/Blog-HealthAndJusticeLeague/data"
# natural disaster dataset
natdis <- read_csv(paste0(my_path, "/wrangled_natdisasters_byyear.csv")) %>%
  filter(disaster_type != "All")
#explicitly call maps:: because map() is masked by purrr packagef
natdis_leaflet <- maps::map("world", fill = TRUE, plot = FALSE, wrap=c(-180,180)) 
# This is the dataset containing 
# the official country code, official country name, 
# and country name used in maps package
data(iso3166)

# a3 is iso3c country code
list_countries <- iso3166 %>%
  select(a3, ISOname, mapname)

# HTML Text: background info to display on the side
text <- sprintf("<strong>%s</strong>%s<br/><br/>%s<br/>%s<br/>%s<br/>%s<br/>%s<br/><br/>",
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

ui <- fluidPage(theme = shinytheme("cerulean"), 
  titlePanel("Impacts of Natural Disasters"),
  # Instead of sidebar, change the layout?
  fluidRow(
    column(width = 6, style = "padding: 20px",
      # disaster type
      checkboxGroupInput(inputId = "disaster",
                         label = "Select disaster(s)",
                         choices = relevant,
                         selected = relevant,
                         inline = TRUE)),
    column(width = 6,
      # year
      sliderInput(inputId = "year",
                  label = "Select the year",
                  min = 1980,
                  max = 2020,
                  value = 2000,
                  sep = "",
                  ticks = TRUE,
                  width = "90%"))),
    fluidRow(
      column(width = 5,
             column(offset = 1, width = 11, text)
            ),
      column(width = 7,
           leafletOutput("natdisplot"))
  )
)



server <- function(input, output){
  
  # natural disasters of a given type in a given year
  natdis_react <- reactive({
    natdis %>%
      filter(disaster_type %in% input$disaster) %>%
      filter(year == input$year) %>%
      # Below operation is because the country names used in maps package and 
      # the country names in natdis data differ
      right_join(list_countries, by = c("countrycode" = "a3"))
    
  })
  
  
  a <- reactive({
    # country name
    natdis_leaflet$country <- str_extract(natdis_leaflet$names, "[^:]+")
    
    # how many disasters occurred in a given year?
    # Use mapname to fill in the corresponding values in the freq vector
    natdis_leaflet$freq <- natdis_react()$occurrence[match(natdis_leaflet$country,
                                                           natdis_react()$mapname)]
    
    natdis_leaflet$deaths <- natdis_react()$deaths[match(natdis_leaflet$country,
                                                         natdis_react()$mapname)]
    
    
    natdis_leaflet$injured <- natdis_react()$injured[match(natdis_leaflet$country,
                                                           natdis_react()$mapname)]
    
    
    natdis_leaflet$homeless <- natdis_react()$homeless[match(natdis_leaflet$country,
                                                             natdis_react()$mapname)]
    
    
    natdis_leaflet$affected <- natdis_react()$affected[match(natdis_leaflet$country,
                                                             natdis_react()$mapname)]
    
    # deaths + injured + homeless + affected
    natdis_leaflet$total <- natdis_react()$total[match(natdis_leaflet$country,
                                                       natdis_react()$mapname)]
    
    natdis_leaflet
  })
  
  bins <- c(0, 1000, 5000, 10000, 50000, 100000, 500000, 1000000, 5000000, Inf)
  pal_natdis <- reactive({
    colorBin("YlOrRd", domain = a()$total, bins = bins)
  })
  
  
  
  output$natdisplot <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(data = a()) %>%
      addTiles() %>%
      setView(80, 0, zoom = 1)
      
  })
  
  # separating it into observe section reduces the delay/lag 
  # when variable selection changes because the background map is not being
  # drawn all over again!
  observe({
    
    leafletProxy("natdisplot", data = a()) %>%
      # reference: https://stackoverflow.com/questions/48953149/dynamic-color-fill-for-polygon-using-leaflet-in-shiny-not-working
      addPolygons(fillColor = ~pal_natdis()(total),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.5,
                  popup = paste0("region: ", a()$country, "<br>",
                                 "occurred: ", a()$freq, " time(s)", "<br>",
                                 "deaths: ", a()$deaths, "<br>", 
                                 "injured: ", a()$injured, "<br>",
                                 "affected: ", a()$affected, "<br>",
                                 "homeless: ", a()$homeless, "<br>"),
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE)) %>%
    addLegend(pal = pal_natdis(), values = ~total, opacity = 0.7, title = NULL,
              position = "bottomright")
    
    
  })
  
  
}

shinyApp(ui = ui, server = server)

