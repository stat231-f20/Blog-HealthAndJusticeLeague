natdis_react <-
  natdis %>%
    filter(disaster_type == "All") %>%
    filter(year == 2002) %>%
    right_join(list_countries, by = c("countrycode" = "code")) %>%
    arrange(region)


natdis_leaflet <- 
  #explicitly call maps:: because map() is masked by purrr package
  maps::map("world", fill = TRUE, plot = FALSE)


natdis_leaflet$country <- 
  str_extract(natdis_leaflet$names, "[^:]+")
