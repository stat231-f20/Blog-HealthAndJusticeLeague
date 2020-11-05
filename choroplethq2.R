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
natdis <- read_csv(paste0(my_path, "/data/wrangled_natdisasters_byyear.csv"))

# match country name in world_map dataset with country dataset
world_code <- world_map %>%
  mutate(code = countrycode(region, origin = "country.name", destination = "iso3c",
              warn = TRUE))


# check if the country code for important/significant countries ended up missing
world_missing <- world_code %>%
  filter(is.na(code))

# The missing country/regions are: 
# Ascension Island, Azores, Barbuda, Bonaire, Canary Islands, 
# Chagos Archipelago, Grenadines, Heard Island, Kosovo, Madeira Islands, 
# Micronesia, Saba, Saint Martin, Siachen Glacier, Sint Eustatius, 
# Virgin Islands

# join world_code dataset with natdis2 dataset by country code (1950-)

world_natdis <- world_code %>%
  left_join(natdis_2000, by = c("code" = "countrycode"))

natdis_leaflet <- map(database = "world", fill = TRUE, plot = TRUE)

class(natdis_leaflet)
natdis_leaflet$names <- world_natdis$country
natdis_leaflet$deaths <- world_natdis$deaths
natdis_leaflet$injured <- world_natdis$injured
natdis_leaflet$affected <- world_natdis$affected
natdis_leaflet$homeless <- world_natdis$homeless

pal_natdis <- colorBin("YlOrRd", domain = natdis_leaflet$deaths)

labels_natdis <- sprintf(
  "%s<br>deaths: <strong>%s</strong><br/>",
  natdis_leaflet$names, natdis_leaflet$deaths) %>% 
  lapply(htmltools::HTML)

relevant <- c("Drought","Storm", "Flood", "Landslide", "Wildfire", "Extreme temperature")
names(relevant) <- relevant


natdis_leaflet <- map("world", fill = TRUE, plot = FALSE)

ui <- fluidPage(theme = "bootstrap.min.css", 
                tags$style("#state_select {border-color: #52B097;}"),
                titlePanel("Impacts of Natural Disasters"),

                           sidebarLayout(
                             sidebarPanel(
                               sliderInput(inputId = "year",
                                                  label = "Select the year",
                                                  min = 1900,
                                           max = 2020,
                                           value = 2000,
                                           ticks = TRUE),
                               selectInput(inputId = "disaster",
                                           label = "Select the disaster type",
                                           choices = relevant,
                                           selected = "Drought",
                                           selectize = FALSE),
                             
                             mainPanel(LeafletOutput("natdismap")))))

server <- function(input, output){
  
  natdis_react <- reactive(
    natdis %>%
      filter(disaster_type == input$disaster) %>%
      filter(year == input$year) %>%
      right_join(world_code, by = c("countrycode" = "code"))
  )
  
  natdis_leaflet <- reactive(
    natdis_leaflet %>%
      mutate()
  )
  
  
  pal <- colorBin("YlOrRd", domain = natdis_leaflet$)
  
  
  
  output$natdisplot <- renderLeaflet(
    leaflet(data = natdis_react)%>%
      addTiles() %>%
      setView(0, 0, zoom = 1)%>%
      addPolygons(fillColor = ~pal_natdis(deaths),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  popup = paste0("country: ", usa_states_leaflet$names, "<br>",
                                 "Number of schools reporting:",
                                 round(usa_states_leaflet$n_schools,0),
                                 "<br>", #newline
                                 "Number of schools planning for in-person learning: ",
                                 round(usa_states_leaflet$n_inperson,0), 
                                 "<br>", #newline
                                 "Proportion planning for in-person learning: ",
                                 round(usa_states_leaflet$prop_inperson,2)))%>%
      addLegend(pal = pal_natdis, values = ~deaths, opacity = 0.7, title = NULL,
                position = "bottomright")
  )
  
  
  
}

shinyApp(ui = ui, server = server)

