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

my_path <- "C:/Users/Yesuel Kim/Documents/Git/Blog-HealthAndJusticeLeague"

co2 <- read_csv(paste0(my_path, "/data/co2_bycountry.csv"))
gtemp <- read_csv(paste0(my_path, "/data/globaltemperature_long.csv"))

data("iso3166")

co2_leaflet <- maps::map("world", fill = TRUE, plot = FALSE, wrap=c(-180,180))

co2_react <- co2 %>%
  filter(year == 1980) %>%
  right_join(iso3166, by = c("isocode" = "a3")) %>%
  select(-sovereignty)

co2_leaflet$country <- str_extract(co2_leaflet$names, "[^:]+")
co2_leaflet$total <- co2_react$total[match(co2_leaflet$country,
                                                   co2_react$mapname)]
co2_leaflet$per_capita <- co2_react$per_capita[match(co2_leaflet$country,
                                           co2_react$mapname)]
bins <- c(0, 0.4, 0.8, 1.2, 1.6, 2.0, 2.4, 2.8, 3.2, Inf)
pal_co2 <- colorBin("YlOrRd", domain = co2_leaflet$per_capita, bins = bins)

leaflet(data = co2_leaflet) %>%
  addTiles() %>%
  setView(0, 0, zoom = 1) %>%
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
              fillColor = ~pal_co2(per_capita),
              popup = paste0("region: ", co2_leaflet$country, "<br>",
                             "total emission: ", co2_leaflet$total, " million ton", "<br>",
                             "per capita emission: ", co2_leaflet$per_capita, " million ton", "<br>")) %>%
  addLegend(pal = pal_co2, values = ~per_capita, opacity = 0.7, title = NULL,
            position = "bottomright")



#plotOutput("tempPlot")

# h <- ggplot(data = gtemp, 
#             aes(x = as.POSIXct(period, origin = "1970-01-01"), y = temp)) +
#   geom_line(size = 0.2) +
#   geom_smooth(alpha = 0.5, size = 1) +
#   theme_minimal() +
#   labs(title = "Global temperature (1970-2020)",
#        subtitle = "Compared to 20th century average temperature") +
#   scale_x_datetime(name = "Time") + 
#   scale_y_continuous(name = "Global temperature (celsius)")

# gtemp_react <- reactive({
#   
#   sample <- gtemp %>%
#     filter(year==input$year) %>%
#     arrange(period)
#   
#   ## to highlight region data
#   data.frame(start=sample[1,1],
#              end=sample[nrow(sample), 1])
#   
# })

# output$tempPlot <- renderPlot({
#   
#   h +
#     geom_rect(inherit.aes = FALSE,
#               aes(xmin=as.POSIXct(gtemp_react()[1,1]), 
#                   xmax=as.POSIXct(gtemp_react()[1,2]), 
#                   ymin=-0.25, ymax=1.25), 
#               fill="yellow", alpha=0.005)
# 
#   })
# 
# 

# gtemp <- read_csv(paste0(my_path, "/data/globaltemperature_long.csv")) %>%
#   mutate(year = as.numeric(substr(period, 1,4)))

