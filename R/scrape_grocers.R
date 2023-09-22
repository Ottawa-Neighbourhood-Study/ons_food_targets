

################### FARM BOY
scrape_farmboy <- function() {
  message("Starting Farmboy")
  warning("This function to scrape Farm Boy locations is *fragile* because Farm Boy's website is inconsistent and changes. DOUBLE CHECK RESULTS.")
  #url <- "https://www.farmboy.ca/stores/"
  url <- "https://www.farmboy.ca/accessible-store-listing/"
  
  r <- rvest::read_html(url)
  
  stores <- rvest::html_elements(r, ".fb_store_location_wrapper > div")
  
  results <- purrr::map_dfr(stores, function(x) {
    text <- rvest::html_text(x)
    address <- stringr::str_extract(text, "(?<=Address: ).*")
    phone <-  stringr::str_extract(text, "(?<=Phone: ).*")
    fax <-  stringr::str_extract(text, "(?<=Fax: ).*")
    return(dplyr::tibble(name = "Farm Boy", address = address, phone = phone, fax = fax))
  }) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), stringr::str_squish)) |>
    tidygeocoder::geocode(address = "address", lat = "Y", lon = "X", method="google") |>
    dplyr::mutate(update_date = Sys.Date())
  
  message("Done Farmboy")
  return(results)
}



################### FOOD BASICS

scrape_foodbasics <- function(verbose = FALSE){
  message("Starting Food Basics")
  url <- "https://www.foodbasics.ca/find-your-food-basics.en.html"
  
  sess <- rvest::session(url)
  
  # get names of towns you can search
  towns <- sess %>% 
    read_html() %>% 
    rvest::html_elements(css = "option") %>%
    html_text()
  
  # remove the first one, which is "Select a City"
  towns <- towns[2:length(towns)]
  
  # search_town <- "Ottawa"
  verbose <- TRUE
  
  results <- tibble::tibble()
  
  # we'll search for each town they list as an option
  for (search_town in towns){
    if (verbose) message (search_town)
    # set the form values. need to set search mode (town vs cp for code postale) and the town itself
    search_form <- sess %>%
      rvest::html_form() %>%
      pluck(1)  %>%
      rvest::html_form_set(searchMode = "town",
                           town = search_town)
    
    resp <- rvest::html_form_submit(search_form)  
    
    # parse out the store information
    stores <- resp %>% 
      rvest::read_html() %>%
      rvest::html_elements(css = ".store") %>%
      rvest::html_text() %>%
      stringr::str_squish() %>%
      stringr::str_extract("(?<=Food Basics ).*?(?= View Map)")
    
    phones <- stringr::str_extract(stores, "\\d{3}-\\d{3}-\\d{4}")
    addresses <- stringr::str_extract(stores, ".*?(?=\\d{3}-\\d{3}-\\d{4})") %>%
      stringr::str_squish()
    
    result <- tibble::tibble(address = addresses, phone = phones)
    
    results <- bind_rows(results, result)
    
    Sys.sleep(0.2)
  }
  
  results <- tidyr::drop_na(results)
  
  return(results)
}



################### FOOD LAND


scrape_foodland <- function() {
  message("Starting Foodland")
  url <- "https://foodland.ca/store-locator/"
  
  r <- rvest::read_html(url)
  stores <- r %>%
    rvest::html_elements(css = ".brand-foodland-store-location")
  
  # port aux choix doesn't have an address, only a postal code, and is left out
  results <- purrr::map_df(stores, function(store) {
    lons <- lats <- ids <- addresses <- cities <- provinces <- postcodes <- NA
    lons <- rvest::html_attr(store, "data-lng")
    lats <- rvest::html_attr(store, "data-lat")
    ids <- rvest::html_attr(store, "data-id")
    
    tryCatch(   expr = {addresses <- rvest::html_elements(store, css = ".location_address_address_1") %>% rvest::html_text()},
                error = function(e) message(e))
    
    cities <- rvest::html_elements(store, css = ".city") %>% rvest::html_text()
    provinces <- rvest::html_elements(store, css = ".province") %>% rvest::html_text()
    postcodes <- rvest::html_elements(store, css = ".postal_code") %>% rvest::html_text()
    names <- rvest::html_elements(store, css = ".name") %>% rvest::html_text()
    
    phones <- NA
    tryCatch(   expr = { phones <- rvest::html_elements(store, css = ".phone") %>% rvest::html_text() },
                error = function(e) message(e) )
    
    tibble(id = ids,
           name = names,
           address = addresses,
           city = cities,
           province = provinces,
           postal_code = postcodes,
           phone = phones,
           lon = lons,
           lat = lats)   
  })
  
  return (results)
}


################### FRESHCO


scrape_freshco <- function() {
  message("Starting FreshCo")
  
  url <- "https://freshco.com/store-locator/"
  
  r <- rvest::read_html(url)
  stores <- r %>%
    rvest::html_elements(css = ".store-result")
  
  # get all the individual store items
  results <- purrr::map_df(stores, function(store) {
    lons <- lats <- ids <- addresses <- cities <- provinces <- postcodes <- NA
    lons <- rvest::html_attr(store, "data-lng")
    lats <- rvest::html_attr(store, "data-lat")
    ids <- rvest::html_attr(store, "data-id")
    
    addresses <- rvest::html_elements(store, css = ".location_address_address_1") %>% rvest::html_text()
    cities <- rvest::html_elements(store, css = ".city") %>% rvest::html_text()
    provinces <- rvest::html_elements(store, css = ".province") %>% rvest::html_text()
    postcodes <- rvest::html_elements(store, css = ".postal_code") %>% rvest::html_text()
    names <- rvest::html_elements(store, css = ".name") %>% rvest::html_text()
    
    phones <- NA
    tryCatch(   expr = { phones <- rvest::html_elements(store, css = ".phone") %>% rvest::html_text() },
                error = function(e) message(e) )
    
    tibble(id = ids,
           name = names,
           address = addresses,
           city = cities,
           province = provinces,
           postal_code = postcodes,
           phone = phones,
           lon = lons,
           lat = lats)
  })
  
  return (results)
}

################### LOBLAWS AND AFFILIATES- YIG AND SUPERSTORE

# function to scrape loblaws and affiliates
# the api works by passing it a "banner id," i.e. a store brand name
# loblaw and independent are the two I know about right now
scrape_loblaws <- function(banner_ids = c("loblaw", "independent", "superstore", "nofrills")){
  message(paste0("Starting Loblaws Banner ID: ",banner_ids))
  banner_ids <- match.arg(banner_ids)
  
  url <- paste0("https://www.loblaws.ca/api/pickup-locations?bannerIds=",banner_ids)
  
  resp <- httr::GET(url)
  
  
  result <- resp %>%
    httr::content(type = "text/json", encoding = "UTF-8") %>%
    jsonlite::fromJSON() %>%
    tibble::as_tibble()
  
  result <- result %>%
    jsonlite::flatten() %>%
    tibble::as_tibble() %>%
    dplyr::select(-features, -departments)
  return (result)
  
}


################### METRO
# NOTE! Metro uses javascript so we do it with selenium.
# 

scrape_metro <- function() {
  message("Starting Metro:")
  
  # z = rvest::read_html("https://www.metro.ca/en/find-a-grocery?utm_id=12468278_1333253161_58836805612&utm_source=google&utm_medium=cpc&gclid=Cj0KCQjw9rSoBhCiARIsAFOiplne0nnif2tkLZ2fM3AUqN75QdfuOMSi7_htwEA_qRjUAHbdhiTCQlAaAhJ8EALw_wcB&gclsrc=aw.ds")
  url <- "https://www.metro.ca/en/find-a-grocery"

  html <- rvest::read_html(url)  
  stores_html <- rvest::html_elements(html, ".fs--box-shop")
  
  results <- stores_html |>
    purrr::map_dfr(function(x) {
      attrs <- rvest::html_attrs(x)      
        lat <- attrs["data-store-lat"] |> as.numeric()
        lon <- attrs["data-store-lng"] |> as.numeric()
        
        moredata <- rvest::html_elements(x, ".white-wrapper div")
        address1 <- rvest::html_elements(moredata, ".address--street") |>
          rvest::html_attr("data-street")
        
        address2 <- rvest::html_elements(moredata, ".address--city") |>
          rvest::html_attr("data-city")
        
        phone <-  rvest::html_elements(moredata, ".store-phone") |>
          rvest::html_text() |> stringr::str_squish()
        
        result <- dplyr::tibble(name = "Metro", address1 = address1, address2 = address2, phone = phone, lat = lat, lon = lon)
        
      return(result)
    })
  
  return(results)
}



################### SOBEYS

scrape_sobeys <- function() {
  message("starting Sobeys:")
  url <- "https://www.sobeys.com/store-locator/"
  
  resp <- rvest::read_html(url)#httr::GET(url)
  
  stores <- resp %>%
    rvest::html_elements(css = ".store-result")
  
  results <- purrr::map_df(stores, function(store) {
    lons <- lats <- ids <- addresses <- cities <- provinces <- postcodes <- NA
    lons <- rvest::html_attr(store, "data-lng")
    lats <- rvest::html_attr(store, "data-lat")
    ids <- rvest::html_attr(store, "data-id")
    
    tryCatch(   expr = {addresses <- rvest::html_elements(store, css = ".location_address_address_1") %>% rvest::html_text()},
                error = function(e) message(e))
    
    cities <- rvest::html_elements(store, css = ".city") %>% rvest::html_text()
    provinces <- rvest::html_elements(store, css = ".province") %>% rvest::html_text()
    postcodes <- rvest::html_elements(store, css = ".postal_code") %>% rvest::html_text()
    names <- rvest::html_elements(store, css = ".name") %>% rvest::html_text()
    
    phones <- NA
    tryCatch(   expr = { phones <- rvest::html_elements(store, css = ".phone") %>% rvest::html_text() },
                error = function(e) message(e) )
    
    result <- tibble::tibble(id = ids,
                             name = names,
                             address = addresses,
                             city = cities,
                             province = provinces,
                             postal_code = postcodes,
                             phone = phones,
                             lon = lons,
                             lat = lats)  
    
    return(result)
  })
  

  return(results)
}


################### WALMART


scrape_walmart <- function(address = "ottawa on"){
  message("Starting Walmart")
  addr_url <- urltools::url_encode(address)
  url <- paste0("https://www.walmart.ca/en/stores-near-me?addr=",addr_url)
  
  #resp <- httr::GET(url)
  
  html <- rvest::read_html(url)
  
  html_text <- html %>%
    as.character()
  
  json_raw <- stringr::str_extract(html_text, regex("(?<=window.INITIAL_STATE = ).*?(?=</script>)", dotall = TRUE)) %>%
    stringr::str_squish()
  
  json_parsed <- jsonlite::fromJSON(json_raw)
  
  # extract the store information and remove some extra data
  stores <- json_parsed$searchResults$stores %>% 
    as_tibble() %>%
    select(-timeZone, -kiosk, -deleted, -metadata, -market, -todaysTiming, -currentHours, -regularHours, -distance)
  
  # extract store services
  stores <- stores %>%
    mutate(servicesMap = purrr::map_chr(servicesMap, function(x) {
      pluck(x, "service") %>%
        pluck("name") %>%
        stringr::str_flatten(collapse = " | ")
    }))
  
  # extract addresses and lat/lon
  stores <- stores %>%
    mutate(address1 = address$address1,
           city = address$city,
           province = address$state,
           postal_code = address$postalCode,
           lat = geoPoint$latitude,
           lon = geoPoint$longitude) %>%
    select(-address, -geoPoint)
  
  return (stores)
}


################### T&T GROCERY STORES

scrape_t_and_t <- function() {
  message("Starting T&T")
  #url <- "https://www.tntsupermarket.com/rest/V1/xmapi/get-store-list-new?lang=en&address=L3T"
  url <- "https://www.tntsupermarket.com/rest/V2/xmapi/get-stores?postcode=K1H"
  
  resp <- httr::GET(url)
  
  stores <- resp %>%
    httr::content(type = "text/json", encoding = "UTF-8") %>%
    jsonlite::fromJSON() %>%
    pluck("data") %>%  
    as_tibble() |>
    dplyr::mutate(name = "T&T")
  
  return(stores)
}


########## WHOLE FOODS

# add whole foods manually, there's only one
get_whole_foods <- function(){
  message("Starting Whole Foods")
  whole_foods <- tibble::tibble(note = "Lansdowne Park",
                                name = "Whole Foods Market",
                                address = "951 Bank St",
                                address2 = "Ottawa, ON K1S 3W7",
                                phone = "(613) 565-7150",
                                update_date = Sys.Date()) #%>%
    #onsr::geocode_ottawa(var = "address") %>%
   # rename(Y = lat, X = lng)
  
  return(whole_foods)
}

###################  DO IT ALL


scrape_and_parse_large_grocers <- function(gmap_api_key){
  
  ## Food Basics
  foodbasics <- scrape_foodbasics() %>%
    parse_foodbasics(gmap_api_key = gmap_api_key)
  
  
  ### metro
  #warning("metro not working")
  metro <- scrape_metro() %>%  parse_metro()
  
  ### YIG
  yig <- scrape_loblaws(banner_ids = "independent") %>%
    parse_loblaws()
  
  ## Loblaws
  
  loblaws <- scrape_loblaws(banner_ids = "loblaw") %>% 
    parse_loblaws()
  
  ## Walmart
  
  walmart <- scrape_walmart() %>% 
    parse_walmart()
  

  ## Sobeys
  
  sobeys <- scrape_sobeys() %>%
    parse_sobeys()
  
  ## Freshco
  
  freshco <- scrape_freshco() %>%
    parse_freshco()
  
  ## Foodland
  
  foodland <- scrape_foodland() %>%
    parse_foodland()
  
  ## Real Canadian Superstore (Loblaws affiliate)
  
  rcsuperstore <- scrape_loblaws(banner_ids = "superstore") %>%
    parse_loblaws()
  
  ## No Frills (Loblaws affiliate)
  
  nofrills <- scrape_loblaws(banner_ids = "nofrills") %>%
    parse_loblaws()
  
  ## T&T
  tt <- scrape_t_and_t() %>%
    parse_t_and_t()
  
  # FARMBOY:
  
  farmboy <- scrape_farmboy() #%>% parse_farmboy()
  
  whole_foods <- get_whole_foods()
  
  ## GEOCODE FOOD BASICS!!
  # They didn't include lat/lon in their data. Since we want values outside of ottawa,
  # we'll use google maps for geocoding.
  
  
  ## PUT IT ALL TOGETHER, WHAT DO YOU GET?
  
  grocers_large <- bind_rows(metro, yig, loblaws, walmart, rcsuperstore, foodbasics, sobeys, freshco, foodland, nofrills, tt, farmboy, whole_foods) |>
    dplyr::mutate(update_date = Sys.Date())
  
  return(grocers_large)
}


