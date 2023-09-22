#_targets.R
library(targets)
library(tidyverse)
library(tidygeocoder)
library(rvest)
library(sf)
library(leaflet)
library(jsonlite)
library(htmltools)
options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("tidyverse",
                            "httr",
                            "rvest",
                            "sf",
                            "leaflet",
                            "jsonlite",
                            #    "onsr",
                            #"RSelenium",  # needed only for scraping
                            #"wdman",
                            "htmltools"))
source("R/scrape_grocers.R")
source("R/parse_grocers.R")

source (".ignorethis") # google maps api key, don't sync on github

list(
  
  # tar_target(
  #   gmap_api_key,
  #     readr::read_file("../chris_google_api/chris_google_api_key.csv")
  # ),
  # 
  targets::tar_target(
    ons_shp, neighbourhoodstudy::ons_gen3_shp
  ),
  
  targets::tar_target(
    grocers_large,
    scrape_and_parse_large_grocers(gmap_api_key)),
  
  targets::tar_target(
    save_results,
    readr::write_csv(grocers_large, paste0("outputs/grocers_large_", Sys.Date(),".csv"))
  ),
  
  targets::tar_target(
    grocers_large_ottawa,
    geo_filter_grocers(grocers_large, ons_shp = ons_shp)
  ),
  
  targets::tar_target(
    save_results_ottawa,
    {
      readr::write_csv(sf::st_drop_geometry(grocers_large_ottawa), paste0("outputs/grocers_large_ottawa-", Sys.Date(),".csv"))
      sf::write_sf(grocers_large_ottawa, paste0("outputs/grocers_large_ottawa-", Sys.Date(),".shp"))
    }
  )
  
)