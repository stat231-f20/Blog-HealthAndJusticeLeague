#FIXME: Thank you so much for taking a look at this code!

library(leaflet)
library(maps)
library(readr)
library(dplyr)
library(tidyverse)
library(janitor)
library(countrycode)
library(shiny)

# calling in the datasets
my_path <- "C:/Users/kcorreia/Dropbox (Amherst College)/Teaching/Fall 2020/Stat231/git/Blog-HealthAndJusticeLeague"
# natural disaster dataset
natdis <- read_csv(paste0(my_path, "/data/wrangled_natdisasters_byyear.csv"))
#explicitly call maps:: because map() is masked by purrr packagef
natdis_leaflet <- maps::map("world", fill = TRUE, plot = FALSE, wrap=c(-180,180)) 
# This is the dataset containing 
# the official country code, official country name, 
# and country name used in maps package
data(iso3166)

#####
# ignore this part
# natdis_leaflet$country <- str_extract(natdis_leaflet$names, "[^:]+")
# natdis_leaflet$code <- natdis_leaflet$country %>% countrycode(origin = "country.name", destination = "iso3c")
# natdis_leaflet$freq <- natdis$occurrence[match(natdis_leaflet$code, natdis_react$countrycode)]
# natdis_leaflet$deaths <- natdis$deaths[match(natdis_leaflet$code, natdis_react$countrycode)]
# natdis_leaflet$injured <- natdis$injured[match(natdis_leaflet$code, natdis_react$countrycode)]
# natdis_leaflet$homeless <- natdis$homeless[match(natdis_leaflet$code, natdis_react$countrycode)]
# natdis_leaflet$total <- natdis$total[match(natdis_leaflet$code, natdis_react$countrycode)]
######

# a3 is iso3c country code
list_countries <- iso3166 %>%
  select(a3, ISOname, mapname)

######
#ignore this part
# check if the country code for important/significant countries ended up missing
# world_missing <- world_code %>%
#   filter(is.na(code))

# The missing country/regions are: 
# Ascension Island, Azores, Barbuda, Bonaire, Canary Islands, 
# Chagos Archipelago, Grenadines, Heard Island, Kosovo, Madeira Islands, 
# Micronesia, Saba, Saint Martin, Siachen Glacier, Sint Eustatius, 
# Virgin Islands
######

# for selectInput drop-down choices(types of natural disasters)
relevant <- c("All", "Drought","Storm", "Flood", "Landslide", 
              "Wildfire", "Extreme temperature")
names(relevant) <- relevant

ui <- fluidPage(#theme = "bootstrap.min.css", 
  #tags$style("#state_select {border-color: #52B097;}"),
  titlePanel("Impacts of Natural Disasters"),
  sidebarLayout(
    sidebarPanel(
      # year
      sliderInput(inputId = "year",
                  label = "Select the year",
                  min = 1980,
                  max = 2020,
                  value = 2000,
                  sep = "",
                  ticks = TRUE),
      # disaster type
      selectInput(inputId = "disaster",
                  label = "Select the disaster type",
                  choices = relevant,
                  selected = "All",
                  selectize = FALSE)),
    
    mainPanel(leafletOutput("natdisplot"))
  )
)



server <- function(input, output){
  
  # natural disasters of a given type in a given year
  natdis_react <- reactive({
    natdis %>%
      filter(disaster_type == input$disaster) %>%
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
  
  # will consider making this a logarithmic scale(?) instead of arbitrary numbers
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
      setView(0, 0, zoom = 1) %>%
      # reference: https://stackoverflow.com/questions/48953149/dynamic-color-fill-for-polygon-using-leaflet-in-shiny-not-working
      addPolygons(fillColor = ~pal_natdis()(total),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.5,
                  popup = paste0("country: ", a()$country, "<br>",
                                 "occurred:", a()$freq, "<br>",
                                 "deaths: ", a()$deaths, "<br>", 
                                 "injured: ", a()$injured, "<br>",
                                 "affected: ", a()$affected, "<br>",
                                 "homeless: ", a()$homeless, "<br>"),
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE))
  })
  
  #######
  # before separating background and polygons
  # output$natdisplot <- renderLeaflet(
  #   leaflet(data = natdis_leaflet()) %>%
  #     addTiles() %>%
  #     setView(0, 0, zoom = 1)%>%
  #     addPolygons(fillColor = ~pal_natdis(total),
  #                 weight = 2,
  #                 opacity = 1,
  #                 color = "white",
  #                 dashArray = "3",
  #                 fillOpacity = 0.7,
  #                 popup = paste0("country: ", natdis_leaflet()$country, "<br>",
  #                                "occurred:", natdis_leaflet()$occurrence, "<br>",
  #                                "deaths: ", natdis_leaflet()$deaths, "<br>", #newline
  #                                "injured: ", natdis_leaflet()$injured, "<br>",
  #                                "affected: ", natdis_leaflet()$affected, "<br>",
  #                                "homeless: ", natdis_leaflet()$homeless, "<br>",
  #                                )) %>%
  #     addLegend(pal = pal_natdis, values = ~total, opacity = 0.7, title = NULL,
  #               position = "bottomright")
  # )
  ######
  
  
}

shinyApp(ui = ui, server = server)

