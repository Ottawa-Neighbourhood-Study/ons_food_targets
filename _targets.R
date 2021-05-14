#_targets.R
library(targets)
options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("tidyverse",
                            "httr",
                            "rvest",
                            "sf",
                            "leaflet",
                            "jsonlite",
                            "onsr",
                            #"RSelenium",  # needed only for scraping
                            #"wdman",
                            "htmltools"))
source("R/scrape_grocers.R")
source("R/parse_grocers.R")

list(
  # tar_target(
  #   gmap_api_key,
  #     readr::read_file("../chris_google_api/chris_google_api_key.csv")
  # ),
  # 
  # tar_target(
  #   grocers_large,
  #   scrape_and_parse_large_grocers(gmap_api_key) %>%
  #     write_csv("data/grocers_large_all.csv") 
  # ),
  tar_target(
    grocers_large_data,
    "data/grocers_large_ottawa.csv",
    format = "file"
  ),
  tar_target(
    yelp_grocers_data,
    "data/yelp_grocery.csv",
    format = "file"
  ),
  
  
  tar_target(
    grocers_large,
    read_csv(grocers_large_data) %>%
      mutate(size = "large")
  ),
  
  # load the yelp grocers, geocode with ottawa's service, filter out any with
  # missing latitudes (these are in quebec), filter out any names of large 
  # grocers either from the bigdataset or a specified list
  tar_target( 
    grocers_yelp,
    read_csv(yelp_grocers_data) %>%
      onsr::geocode_ottawa(var = "formattedAddress") %>%
      select(-longitude, -latitude) %>%
      filter(!toupper(name) %in% grocers_large$name & !name %in% grocers_large$name) %>%
      filter(!stringr::str_detect(name, "Your Independent Grocer|No Frills|Sobeys|Farmboy|T&T")) %>%
      drop_na(lat) %>%
      rename(notes = categories,
             X=lng,
             Y=lat,
             address = formattedAddress) %>%
      select(-rating, -priceRange) %>%
      mutate(address2 = "",
             size = "small",
             update_date= Sys.Date()) %>%
      write_csv("data/grocers_yelp_ottawa.csv")
  ),
  
  tar_target(grocers_all,
             bind_rows(grocers_large, grocers_yelp) %>%
               write_csv("data/grocers_all.csv")),
  
  ##### SOME MAPS FOR TESTING
  
  tar_target(
    grocers_large_ott_map,
    leaflet(rename(grocers_large, lat = Y, lon = X)) %>% addTiles() %>% addMarkers(label = ~purrr::map(paste0("<b>",name,"</b><br>",address), htmltools::HTML)) 
  ),
  tar_target(
    grocers_yelp_ott_map,
    leaflet(rename(grocers_yelp, lat = Y, lon = X)) %>% addTiles() %>% addMarkers(label = ~purrr::map(paste0("<b>",name,"</b><br>",address), htmltools::HTML)) 
  )
  
)