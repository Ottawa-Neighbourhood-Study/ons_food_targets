library(tidyverse)
library(osmdata)
library(onsr)
library(sf)
library(leaflet)


# load the files for circle k, quickie, and everything on yelp
ck <- read_csv("data/convenience/circle_k.csv") %>%
  filter(distance < 50) %>%
  mutate(address = sprintf("%s, %s, %s", address, city, country),
         address = stringr::str_replace_all(address, ",,", ",")) %>%
  select(name = display_brand,
         address,
         note = services)

qk <- read_csv("data/convenience/quickie.csv") %>%
  mutate(address = sprintf("%s, %s, %s", address, address2, postal_code)) %>%
  select(name, address, phone)

yp <- read_csv("data/convenience/yelp_convenience.csv") %>%
  rename(note = categories) %>%
  filter(!tolower(name) %in% tolower(unique(ck$name)),
         !stringr::str_detect(name, "Quickie the"),
         !stringr::str_detect(name, "Mac’s"))


convenience <- bind_rows(ck, yp, qk)


# re-geocode them

api_key <- read_file("../chris_google_api/chris_google_api_key.csv")

convenience <- onsr::geocode_gmap(data = convenience,
                                  var = address,
                                  api_key = api_key,
                                  verbose = TRUE)

convenience %>%
  write_csv("data/convenience/convenience_geo.csv")

# eliminate many duplicates automatically
con_dedup <- convenience %>%
  filter(!stringr::str_detect(name, "Quickie Convenience|Quickie Stores")) %>%
  filter(!stringr::str_detect(address, "QC|Gatineau|GATINEAU")) %>%
#  filter(!stringr::str_detect(name, "Quickie the")) %>%
  group_by(lat, lng) %>%
  mutate(num = n()) %>%
  arrange(lat, lng, -num) %>%
  filter(!(name == "Ultramar" & num == 2)) %>%
  filter(!(stringr::str_detect(name, "Esso") & num == 2)) %>%
  group_by(lat, lng) %>%
  mutate(num2 = n())

# ... but do a few manually
write_csv(con_dedup, "data/convenience/convenience_geo_formanual.csv")

# and load them back, remove the unnecessary columns, and save one last time
con_manual <- read_csv("data/convenience/convenience_geo_postmanual.csv")

con_manual %>%
  select(-num, -num2) %>%
  write_csv(paste0("data/convenience/convenience_", Sys.Date(),".csv"))




######### CIRCLE K

# fetch("https://www.circlek.com/stores_new.php?lat=45.421&lng=-75.69&services=&region=global&page=1", {
#   "headers": {
#     "accept": "*/*",
#     "accept-language": "en-CA,en-GB;q=0.9,en-US;q=0.8,en;q=0.7",
#     "sec-ch-ua": "\" Not;A Brand\";v=\"99\", \"Google Chrome\";v=\"91\", \"Chromium\";v=\"91\"",
#     "sec-ch-ua-mobile": "?0",
#     "sec-fetch-dest": "empty",
#     "sec-fetch-mode": "cors",
#     "sec-fetch-site": "same-origin",
#     "x-requested-with": "XMLHttpRequest",
#     "cookie": "_gid=GA1.2.244581154.1623356509; _gcl_au=1.1.462902689.1623356510; _hjid=e75998d9-17ce-4c78-ad58-4c1a08f6764f; _hjTLDTest=1; _hjFirstSeen=1; _hjAbsoluteSessionInProgress=0; _hjIncludedInSessionSample=0; _fbp=fb.1.1623356509803.292697256; _ga=GA1.2.290832939.1623356509; _ga_4NY463VXHT=GS1.1.1623356509.1.1.1623356608.0"
#   },
#   "referrer": "https://www.circlek.com/store-locator?address=Ottawa,Ontario,Canada&lat=45.421&lng=-75.69",
#   "referrerPolicy": "no-referrer-when-downgrade",
#   "body": null,
#   "method": "GET",
#   "mode": "cors"
# });

library(httr)
library(rvest)

scrape_circlek <- function(max_pages = 25){
  page <- 0
  all_results <- tibble()
  
  for (page in 1:max_pages){ # note if you go up to 1000 it gives you maybe all the stores in the world
    url <- paste0("https://www.circlek.com/stores_new.php?lat=45.421&lng=-75.69&services=&region=global&page=",page)
    
    resp <- httr::GET(url)
    
    stores <- httr::content(resp, type = "text/json", encoding = "UTF-8") %>%
      jsonlite::fromJSON()
    
    stores <- stores$stores %>%
      enframe() %>%
      unnest_wider(value)
    
    all_results <- bind_rows(all_results, stores)
  }
  
  #all_results <- 
  results <- all_results %>%
    mutate(services = purrr::map_chr(services, function(x) stringr::str_flatten(x[[1]], collapse = ", "))) 
  
  return (results)
}



scrape_quickie <- function(write_to_file = FALSE){
  # this is just a static html page we need to parse
  
  url <- "https://quickiestores.com/en/store-locator/"
  
  html <- rvest::read_html(url)  
  
  stores <- rvest::html_nodes(html, css = "p:nth-child(1)") %>%
#    as.character() %>%
    stringr::str_replace_all("<br>", "\\\n") %>%
    stringr::str_remove_all("<.*?>")
    
  # remove first and last, which are website junk
  stores <- stores[c(-1, -length(stores))]
  
  results <- tibble(stores = stores, name = "Quickie") %>%
    mutate(stores = stringr::str_trim(stores)) %>%
    tidyr::separate(col = stores, into = c("address", "address2", "postal_code", "phone"), sep = "\\n")

  if (write_to_file) write_csv(results, "data/convenience/quickie.csv")
  
  results    
}

#test <- scrape_quickie(TRUE)



plot <- df %>%
  filter(division_name == "ontario") %>%
  sf::st_as_sf(coords = c("longitude", "latitude")) %>%
  ggplot() + 
    geom_sf() +
    theme_minimal()

plot <- ggplot(data= tibble(x=1:10, y=(1:10)^2)) + ggiraph::geom_point_interactive(aes(x=x, y=y))
ggiraph::ggiraph(ggobj = plot)

  plotly::ggplotly(tooltip = c("latitude", "longitude"))

df$division_name %>% unique()

###########

library(ggplot2)
library(ggiraph)
data <- mtcars
data$carname <- row.names(data)

gg_point = ggplot(data = data) +
  geom_point_interactive(aes(x = wt, y = qsec, color = disp,
                             tooltip = carname, data_id = carname)) + 
  theme_minimal()

girafe(ggobj = gg_point)

#   #%>%  write_csv("data/convenience/circle_k_global.csv")
# 
# df <- all_results %>% 
#   mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude)) %>% 
#   mutate(country = if_else(country == "Canada", "CA", country)) %>%
#   drop_na(latitude) 
#   
# low <- df %>%
#   filter(country == "CA" & latitude <42) %>% mutate(latitude = as.character(latitude), longitude = as.character(longitude)) #%>%
#   sf::st_as_sf(coords = c("longitude", "latitude")) %>% ggplot() + geom_sf(aes(colour = country)) +
#   theme_minimal() +
#   scale_colour_viridis_d() +
#   labs(title = "Global Circle K Locations",
#        subtitle = "From Circle K 'Find a Store' (n=9600)")
#   
# # 
# # atl <- all_results %>%
#   mutate(lon = as.numeric(longitude)) %>%
#   filter (lon > -50 & lon < -30)
# 
# bind_rows(low, atl) %>%
#   select(-franchise,-op_status, -distance,  -lon, -ccs, -display_brand, -services, -franchise_brand, -url) %>%
#   onsr::print_md_table()
# 
#   leaflet() %>% addTiles() %>%addMarkers()
  
  
  
  
  
  ### OPENSTREETMAPS 
  
  
  # ons_shp <- onsr::get_ons_shp()
  # 
  # query <- osmdata::opq(bbox = sf::st_bbox(ons_shp),nodes_only = TRUE) %>%
  #   osmdata::add_osm_feature("shop", "convenience")
  # 
  # df <- osmdata::osmdata_sf(query)
  # 
  # osmdata::available_features()
  # 
  #   osmdata::available_tags("shop")
  #   
  #   
  # df$osm_points %>%
  #   leaflet() %>%
  #   addTiles() %>%
  #   addMarkers(label = ~ name)
  #     