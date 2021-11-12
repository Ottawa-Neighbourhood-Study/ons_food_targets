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
  tar_target(
    gmap_api_key,
      readr::read_file("../chris_google_api/chris_google_api_key.csv")
  ),
  # 
  # tar_target(
  #   grocers_large,
  #   scrape_and_parse_large_grocers(gmap_api_key) %>%
  #     write_csv("data/grocers_large_all.csv") 
  # ),
  tar_target(
    grocers_large_data,
    "data/grocers/grocers_large_ottawa.csv",
    format = "file"
  ),
  tar_target(
    yelp_grocers_data,
    "data/grocers/yelp_grocery.csv",
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
      rename(note = categories,
             X=lng,
             Y=lat,
             address = formattedAddress) %>%
      select(-rating, -priceRange) %>%
      mutate(address2 = "",
             size = "small",
             update_date= Sys.Date()) %>%
      write_csv("data/grocers/grocers_yelp_ottawa.csv")
  ),
  
  tar_target(grocers_all_scraped,
             bind_rows(grocers_large, grocers_yelp) %>%
               write_csv("data/grocers/grocers_all.csv")),
  
  # load the new data from students
  tar_target(
    grocers_student_additions,
    
    readxl::read_xlsx("data/grocers/Grocery Store Additions - Sheet1.xlsx") %>%
      select(name = Name, chain, address) %>% 
      mutate(phone = stringr::str_extract(address, "\\(\\d\\d\\d\\) \\d\\d\\d-\\d\\d\\d\\d"),
             address = stringr::str_remove_all(address, "\\(\\d\\d\\d\\) \\d\\d\\d-\\d\\d\\d\\d"),
             address = if_else(name == "Shoppers", paste0(address, ", Ottawa"), address)) %>%
      onsr::geocode_gmap(address, api_key = gmap_api_key, verbose = TRUE)
  ),
  
  # put the data together
  
  tar_target(
    grocers_all,
    grocers_student_additions %>%
      rename(X = lng, Y = lat) %>%
      bind_rows(grocers_all_scraped) %>%
      write_csv(paste0("data/grocers/grocers_", Sys.Date()))
  ),
  
  
  # combine restaurants into existing data
  tar_target(
    foodspace,
    {
    test <-   read_csv("data/combined/foodspace_2021-08-26c.csv", col_types = cols(.default = "c")) %>%
        mutate(dataset = "new", update_date = "2021-08-26") %>%
        bind_rows(read_csv("data/restaurants/consolidated/restaurants_2021-11-11.csv", col_types = cols(.default = "c")) ) %>%
     select(-rowid) %>%
      rowid_to_column() %>%
        write_csv(sprintf("data/combined/foodspace_%s.csv", Sys.Date()))
    }
  )
  
  
  ##### SOME MAPS FOR TESTING
  
  # tar_target(
  #   grocers_large_ott_map,
  #   leaflet(rename(grocers_large, lat = Y, lon = X)) %>% addTiles() %>% addMarkers(label = ~purrr::map(paste0("<b>",name,"</b><br>",address), htmltools::HTML)) 
  # ),
  # tar_target(
  #   grocers_yelp_ott_map,
  #   leaflet(rename(grocers_yelp, lat = Y, lon = X)) %>% addTiles() %>% addMarkers(label = ~purrr::map(paste0("<b>",name,"</b><br>",address), htmltools::HTML)) 
  # )
  
)