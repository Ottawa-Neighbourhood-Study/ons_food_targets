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
    mutate(name = "FOOD BASICS",
           address = address,
           address2 = NA,
           phone = phone,
           Y = NA,
           X = NA,
           note = NA,
           update_date = Sys.Date())%>%
    onsr::geocode_gmap(var = "address",
                       api_key = gmap_api_key,
                       verbose = TRUE) %>%
    mutate(X = lng,
           Y = lat) %>%
    select(-lat, -lng)
   
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
    mutate(note = name,
           name = "T&T",
           address2 = NA,
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
                             address)) %>%
    onsr::geocode_ottawa(var = "address") %>%
    rename(Y = lat, X = lng) %>%
    mutate(update_date = Sys.Date())
  
}


