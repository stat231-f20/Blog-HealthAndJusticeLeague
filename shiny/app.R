library(leaflet)
library(maps)
library(readr)
library(dplyr)
library(tidyverse)
library(janitor)
library(countrycode)
library(shiny)

# calling in the datasets
my_path <- "C:/Users/Yesuel Kim/Documents/Git/Blog-HealthAndJusticeLeague"
world_map <- map_data(map = "world", region = ".")
natdis0 <- read_csv(paste0(my_path, "/data/wrangled_natdisasters_byyear.csv"))

natdis <- natdis0 %>%
  filter(year >= 1980) %>%
  mutate(total = deaths + injured + affected + homeless)

#The list of countries
list_countries <- world_map %>%
  group_by(region) %>%
  summarize() %>%
  mutate(code = countrycode(region, origin = "country.name", destination = "iso3c", warn = TRUE))


# check if the country code for important/significant countries ended up missing
# world_missing <- world_code %>%
#   filter(is.na(code))

# The missing country/regions are: 
# Ascension Island, Azores, Barbuda, Bonaire, Canary Islands, 
# Chagos Archipelago, Grenadines, Heard Island, Kosovo, Madeira Islands, 
# Micronesia, Saba, Saint Martin, Siachen Glacier, Sint Eustatius, 
# Virgin Islands

# join world_code dataset with natdis2 dataset by country code (1950-)
# for selectInput drop-down choices
relevant <- c("All", "Drought","Storm", "Flood", "Landslide", 
              "Wildfire", "Extreme temperature")
names(relevant) <- relevant

ui <- fluidPage(theme = "bootstrap.min.css", 
                tags$style("#state_select {border-color: #52B097;}"),
                titlePanel("Impacts of Natural Disasters"),
                sidebarLayout(
                  sidebarPanel(
                    sliderInput(inputId = "year",
                                label = "Select the year",
                                min = 1980,
                                max = 2020,
                                value = 2000,
                                ticks = FALSE),
                    selectInput(inputId = "disaster",
                                label = "Select the disaster type",
                                choices = relevant,
                                selected = "All",
                                selectize = FALSE)),
                  
                  mainPanel(leafletOutput("natdisplot"))))

                          

server <- function(input, output){
  
  natdis_react <- reactive(
    natdis %>%
      filter(disaster_type == input$disaster) %>%
      filter(year == input$year) %>%
      right_join(list_countries, by = c("countrycode" = "code")) %>%
      arrange(region)
  )
  
  natdis_leaflet <- reactive(
    #explicitly call maps:: because map() is masked by purrr package
    maps::map("world", fill = TRUE, plot = FALSE)
  )
  
  #FIXME: TRYING TO SOLVE SHINY+LEAFLET INTEGRATION PROBLEM (ADDING ELEMENTS)
  # natdis_leaflet()$country <- reactive(
  #   str_extract(natdis_leaflet()$names, "[^:]+")
  # )
  
  # natdis_leaflet()$freq <- reactive(
  #   natdis_react()$occurrence[match(natdis_leaflet()$country, 
  #                                                        natdis_react()$region)]
  #   )
  # 
  # natdis_leaflet()$deaths <- reactive(
  #   natdis_react()$deaths[match(natdis_leaflet()$country, 
  #                                                      natdis_react()$region)]
  # )
  # 
  # natdis_leaflet()$injured <- reactive(
  #   natdis_react()$injured[match(natdis_leaflet()$country, 
  #                                                        natdis_react()$region)]
  # )
  # 
  # natdis_leaflet()$homeless <- reactive(
  #   natdis_react()$homeless[match(natdis_leaflet()$country, 
  #                                                        natdis_react()$region)]
  # )
  # 
  # natdis_leaflet()$affected <- reactive(
  #   natdis_react()$affected[match(natdis_leaflet()$country, 
  #                                                        natdis_react()$region)]
  # )
  # 
  # natdis_leaflet()$total <- reactive(
  #   natdis_react()$total[match(natdis_leaflet()$country, 
  #                                                            natdis_react()$region)]
  # )
  
  
  pal_natdis <- reactive(
    colorBin("YlOrRd", domain = natdis_leaflet()$total)
    )

  output$natdisplot <- renderLeaflet(
    leaflet(data = natdis_leaflet()) %>%
      addTiles() %>%
      setView(0, 0, zoom = 1)%>%
      addPolygons(fillColor = ~pal_natdis(total),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  popup = paste0("country: ", natdis_leaflet()$country, "<br>",
                                 "occurred:", natdis_leaflet()$occurrence, "<br>",
                                 "deaths: ", natdis_leaflet()$deaths, "<br>", #newline
                                 "injured: ", natdis_leaflet()$injured, "<br>",
                                 "affected: ", natdis_leaflet()$affected, "<br>",
                                 "homeless: ", natdis_leaflet()$homeless, "<br>",
                                 )) %>%
      addLegend(pal = pal_natdis, values = ~total, opacity = 0.7, title = NULL,
                position = "bottomright")
  )
  
  
  
}

shinyApp(ui = ui, server = server)

