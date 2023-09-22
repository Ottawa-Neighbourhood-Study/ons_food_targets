## Functions to parse the scraped large grocer data and put them in a standard
# format comparable to the old data set.
# NOTE! These functions all take input from the `scrape_*()` family of functions,
# which are in scrape_grocers.R.


################### METRO

parse_metro <- function(data){
  
  data %>% 
    select(note = name,
           address = address1,
           address2 = address2,
           phone,
           Y = lat,
           X = lon) %>%
    mutate(name = "METRO",
           update_date = Sys.Date(),
           Y = as.numeric(Y),
           X = as.numeric(X))
  
}

################### LOBLAWS AND AFFILIATES

parse_loblaws <- function(data){
  
  # figure out which loblaws subsidiary we're dealing with
  store_name <- case_when(
    data$storeBannerId == "independent" ~ "YIG",
    data$storeBannerId == "loblaw" ~ "LOBLAWS",
    data$storeBannerId == "superstore" ~ "REAL CANADIAN SUPERSTORE",
    data$storeBannerId == "nofrills" ~ "NO FRILLS",
    TRUE ~ "UKNOWN BANNER ID :("
  )

  data %>%
    mutate(address2 = paste0(address.town, ", ",address.region,", ", address.postalCode)) %>%
    select(note = name,
           address = address.line1,
           address2,
           phone = contactNumber,
           Y = geoPoint.latitude,
           X = geoPoint.longitude
           ) %>%
    mutate(name = store_name,
           update_date = Sys.Date())
  
}

################### WALMART

parse_walmart <- function(data){
  
  data %>%
    mutate(address2 = paste0(city,", ",province,", ", postal_code)) %>%
    select(note = displayName,
           address = address1,
           address2 = address2,
           phone = phone,
           Y = lat,
           X = lon) %>%
    mutate(name = "WALMART",
           update_date = Sys.Date())
  
}

################### FOOD BASICS

parse_foodbasics <- function(data, gmap_api_key){
  
  data %>%
    dplyr::mutate(name = "FOOD BASICS",
           address = address,
           address2 = NA,
           phone = phone,
           note = NA,
           update_date = Sys.Date()) |>
    tidygeocoder::geocode(address = "address", lat = "Y", lon = "X", method="google")

}

################### SOBEYS

parse_sobeys <- function(data){
  
  data %>%
    mutate(address2 = paste0(city,", ",province,", ",postal_code)) %>%
    select(note = name,
           address,
           address2,
           phone,
           X = lon,
           Y = lat) %>%
    mutate(name = "SOBEY'S",
           X = as.numeric(X),
           Y = as.numeric(Y),
           update_date = Sys.Date())
  
}

################### FRESHCO

parse_freshco <- function(data){
  
  data %>%
    mutate(note = name,
           name = "FRESHCO",
           update_date = Sys.Date(),
           address2 = paste0(city,", ",province,", ",postal_code),
           lon = as.numeric(lon),
           lat = as.numeric(lat)) %>%
    select(name, 
           address,
           address2,
           phone,
           X = lon,
           Y = lat,
           note,
           update_date)
    
}

################### FOODLAND

parse_foodland <- function(data){

  data %>%
    mutate(note = name,
           name = "FOODLAND",
           lon = as.numeric(lon),
           lat = as.numeric(lat),
           update_date = Sys.Date(),
           address2 = paste0(city,", ",province,", ",postal_code)) %>%
    select(name,
           address,
           address2,
           phone,
           X = lon,
           Y = lat,
           note,
           update_date)
  
}

################### T&T

parse_t_and_t <- function(data){

  data %>%
    mutate(#note = name,
           name = "T&T",
           address2 = NA,
           phone=NA,
           note=NA,
           lat = as.numeric(lat),
           lng = as.numeric(lng),
           update_date = Sys.Date()) %>%
    select(name, 
           address,
           address2,
           phone,
           X = lng,
           Y = lat,
           note,
           update_date)
  
}

###################  FARMBOY

parse_farmboy <- function(data){
  data %>%
    filter(stringr::str_detect(address, "Ottawa")) %>%
    mutate(address1 = stringr::str_extract(address, regex(".*(?=Ottawa)", dotall = TRUE)), address2 = stringr::str_extract(address, regex("Ottawa.*", dotall=TRUE))) %>%
    select(-address, -fax) %>%
    rename(address = address1) %>%
    mutate(address = if_else(stringr::str_detect(address, regex("\\n\\d")), 
                             stringr::str_extract(address, regex("(?<=\\n).*", dotall = TRUE)), 
                             address)) |>
    tidygeocoder::geocode(address = "address", lat = "Y", lon = "X", method="google") |>
    dplyr::mutate(update_date = Sys.Date())
  
}




# use the ONS shapefile to geo filter grocers to within 5km of Ottawa
geo_filter_grocers <- function(grocers_large, ons_shp, buffer_dist_m = 5000){
  # neighbourhoodstudy::ons_gen3_shp
  ons_shp <-  ons_shp |>
    dplyr::filter(ONS_Region == "OTTAWA") |>
    dplyr::select(dplyr::any_of(c("ONS_ID", "ONS_Name", "geometry")))
  
  ons_buffer <- sf::st_union(ons_shp) |>
    sf::st_transform(crs = 32189) |>
    sf::st_buffer(dist = buffer_dist_m) |>
    sf::st_transform(crs="WGS84")
  
  results <- grocers_large |>
    dplyr::filter(!is.na(X)) |>
    sf::st_as_sf(coords=c("X","Y"), crs="WGS84", remove = FALSE) |>
    sf::st_filter(ons_buffer)
  
  return(results)
  
}