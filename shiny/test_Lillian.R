library(tidyverse)
library(dplyr)
library(shiny)
library(shinythemes)
library(countrycode)
library(leaflet)
library(maps)
library(readr)
library(ggplot2)
library(lubridate)

my_path <- "C:/Users/Yesuel Kim/Documents/Git/Blog-HealthAndJusticeLeague/data"

co2 <- read_csv(paste0(my_path, "/co2_bycountry.csv"))

data("iso3166")

co2_leaflet <- maps::map("world", fill = TRUE, plot = FALSE, wrap=c(-180,180))

shinyApp(
  
  ui = fluidPage(theme = shinytheme("cerulean"), 
                 titlePanel("Carbon Emission by Countries"),
                 fluidRow(
                   column(offset = 1, width = 11,
                          sliderInput(inputId = "co2Year",
                                      label = "Select the year",
                                      min = 1970,
                                      max = 2014,
                                      value = 1980,
                                      sep = "",
                                      ticks = TRUE,
                                      width = "90%"))
                 ),
                 fluidRow(
                   column(offset = 1, width = 11,
                          leafletOutput("co2plot", width = "90%", height = "400px")))
  ),
  
  server = function(input, output) {
    
    co2_react <- reactive({
      
      co2 %>% 
        filter(year == input$co2Year) %>%
        right_join(iso3166, by = c("isocode" = "a3")) %>%
        select(-sovereignty)
      
    }) 
    
    b <- reactive({
      
      co2_leaflet$country <- str_extract(co2_leaflet$names, "[^:]+")
      
      co2_leaflet$total <- co2_react()$total[match(co2_leaflet$country,
                                                 co2_react()$mapname)]
      
      co2_leaflet$per_capita <- co2_react()$per_capita[match(co2_leaflet$country,
                                                           co2_react()$mapname)]
      co2_leaflet
      
    })
    
    bins <- c(0, 0.4, 0.8, 1.2, 1.6, 2.0, 2.4, 2.8, 3.2, Inf)
    
    pal_co2 <- reactive({
      colorBin("YlOrRd", domain = b()$per_capita, bins = bins)
    })
    
    
    output$co2plot <- renderLeaflet({
      
      leaflet(data = b()) %>%
        addTiles() %>%
        setView(80, 0, zoom = 1)
      
    })
    
    observe({
      
      leafletProxy("co2plot", data = b()) %>%
        # reference: https://stackoverflow.com/questions/48953149/dynamic-color-fill-for-polygon-using-leaflet-in-shiny-not-working
        addPolygons(weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.5,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    fillColor = ~pal_co2()(per_capita),
                    popup = paste0("region: ", b()$country, "<br>",
                                   "total emission: ",  b()$total, " million ton", "<br>",
                                   "per capita emission: ",  b()$per_capita, " million ton", "<br>")) %>%
        addLegend(pal = pal_co2(), values = ~per_capita, opacity = 0.7, title = NULL,
                  position = "bottomright")
    })
    
  },
  options = list(height = 500)
)








