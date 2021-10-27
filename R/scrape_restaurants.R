
library(tidyverse)
library(rvest)
library(httr)
library(sf)

# scraping restaurants


fastfood_chains <- old_food %>%
  filter(Category2 == "fast food") %>%
  group_by(Name) %>%
  mutate(num = n()) %>%
  select(Name, num) %>%
  arrange(desc(num)) %>%
  distinct() %>% 
  write_csv("ff_temp.csv")


#harveys <- read_csv("data/restaurants/harveys.csv")

# SECOND CUP IS ANNOYING, NOT DONE YET TODO
# 1 for 1 pizza
# prince gourmet
# popeyes (graphql wouldn't work)

get_ontario_fsas <- function() {
  fsa <- read_sf("C:/Users/chris/Documents/large_shapefiles/lfsa000b16a_e/lfsa000b16a_e.shp") %>%
    filter(stringr::str_detect(PRNAME, "Ontario"))
  
  fsa$CFSAUID
}

scrape_secondcup <- function() {
  url <- "https://secondcup.com/find-a-cafe"
  
  sess <- rvest::session(url)
  
  # get the form
  f <- rvest::html_form(sess)[[3]]
  
  all_cafes <- tibble()
  
  search_cities <- c("ottawa on", "nepean on", "kanata on", "barrhaven on",
                     "orleans on")
  search_cities <- get_ontario_fsas()
  
  #search_city <- search_cities[[1]]
  for(search_city in search_cities){
    message(search_city)
    
    f <- rvest::html_form_set(f, "postal_code" = search_city)
    
    sess2 <- rvest::session_submit(sess, f)
    
    cafes_html <- sess2 %>%
      read_html() %>%
      html_elements(".o-location-collection__item")
    
    cafes <- cafes_html %>%
      purrr::map_dfr(function(x) {
        addr <-  html_elements(x, "#postal-code-form , .m-location-features__address .a-link") %>%
          html_text2()
        
        phone <- html_elements(x, ".m-location-features__phone") %>% html_text2()
        
        tibble(name = "Second Cup",
               address = addr,
               phone = phone)  
      })
    
    all_cafes <- bind_rows(all_cafes, cafes)
  }
  
  all_cafes <- distinct(all_cafes)
  
  write_csv(all_cafes, "data/restaurants/second_cup.csv")
  
  return(all_cafes)
}

scrape_muchoburrito <- function() {
  url <- "https://api.momentfeed.com/v1/analytics/api/llp.json?auth_token=GTKDWXDZMLHWYIKP&center=45.421143,-75.690057&coordinates=44.76227747205096,-75.10599131777343,46.073172781358885,-76.28839488222656&multi_account=false&page=1&pageSize=30"
  
  resp <- httr::GET(url) %>%
    httr::content(type = "text/json", encoding = "UTF-8")
  
  mb <- jsonlite::fromJSON(resp) %>%
    jsonlite::flatten() %>%
    as_tibble()
  
  mbs <- mb %>% 
    mutate(address = if_else(store_info.address_extended == "", 
                             sprintf("%s, %s, %s, %s", store_info.address, store_info.locality, store_info.region, store_info.postcode),
                             sprintf("%s, %s, %s, %s, %s", store_info.address, store_info.address_extended, store_info.locality, store_info.region, store_info.postcode))
    ) %>%
    select(name = store_info.name,
           address,
           phone = store_info.phone,
           lat = store_info.latitude,
           lon = store_info.longitude)
  
  write_csv(mbs, "data/restaurants/much_burrito.csv")
  
  return(mbs)
}

scrape_greekonwheels <- function() {
  
  url <- "https://greekonwheels.ca/greek-on-wheels-locations/"
  
  gow_html <- read_html(url)
  
  addrs <- gow_html %>%
    html_elements("#content .clearfix h1") %>%
    html_text() %>%
    stringr::str_replace("\\s\\(", ", ") %>%
    stringr::str_remove("\\)")
  
  phones <- gow_html %>%
    html_elements("#content strong") %>%
    html_text() %>%
    stringr::str_remove_all("Phone: |.* or ")  #.*?(?=\\d+)"
  
  gow <- tibble(name = "Greek on Wheels",
                address = addrs,
                phone = phones)
  
  write_csv(gow, "data/restaurants/greek_on_wheels.csv")
  
  return(gow)
}

scrape_teriyaki_experience <- function() {
  url <- "https://www.teriyakiexperience.com/wp-admin/admin-ajax.php?action=get_google_map_data&query="
  
  resp <- httr::GET(url) %>%
    httr::content(type = "text/json", encoding= "UTF-8")
  
  ty <- jsonlite::fromJSON(resp)
  
  tys <- ty$data$info %>%
    as_tibble()
  
  tys_all <- tys %>%
    jsonlite::flatten() %>%
    as_tibble() %>%
    mutate(prov =  purrr::map_chr(tys$province, function(x) {
      result <- pluck(x, "name")
      if (is.null(result)) result <- ""
      result
    }),
    address = sprintf("%s, %s, %s, %s", street, city, prov, country.name),
    name = "Teriyaki Express",
    phone = stringr::str_squish(contact.phone)) %>%
    bind_cols(ty$data$position) %>%
    select(name, address, phone, lat, lng) 
  
  
  write_csv(tys_all, "data/restaurants/teriyaki_express.csv")
  
  return(tys_all)  
}

scrape_newyorkfries <- function() {
  url <- "https://www.newyorkfries.com/_api/wix-code-public-dispatcher/siteview/wix/data-web.jsw/find.ajax?gridAppId=5f8c34a2-3417-46b2-a854-84727d6752b8&instance=wixcode-pub.f2504f9027cf51bc1ca8e1a1a453918a2fa57c32.eyJpbnN0YW5jZUlkIjoiYTA2NTRkYTktZDE1Yi00M2E2LTg5MjUtMjgwNjE5ZjE4N2VlIiwiaHRtbFNpdGVJZCI6IjMxOGU2YzViLTE5ZWItNGMyNi1iNWVhLWRkYTM4M2E3ZTRmYiIsInVpZCI6bnVsbCwicGVybWlzc2lvbnMiOm51bGwsImlzVGVtcGxhdGUiOmZhbHNlLCJzaWduRGF0ZSI6MTYzNTI2Njk5NTk5NiwiYWlkIjoiMGY5MmVmYTEtMDJjNC00N2ZhLTkxZTktZDdhNTAxNTc1NTA3IiwiYXBwRGVmSWQiOiJDbG91ZFNpdGVFeHRlbnNpb24iLCJpc0FkbWluIjpmYWxzZSwibWV0YVNpdGVJZCI6ImI4YTg3MmExLTMwMDMtNDQ5Yy05MWM0LWU1YzBlZDk2MTFlZCIsImNhY2hlIjpudWxsLCJleHBpcmF0aW9uRGF0ZSI6bnVsbCwicHJlbWl1bUFzc2V0cyI6IkFkc0ZyZWUsU2hvd1dpeFdoaWxlTG9hZGluZyxIYXNEb21haW4iLCJ0ZW5hbnQiOm51bGwsInNpdGVPd25lcklkIjoiYmRmOGI2ODAtYWY4ZC00ODI5LWIxY2YtNDkwY2UzZDJhMDc0IiwiaW5zdGFuY2VUeXBlIjoicHViIiwic2l0ZU1lbWJlcklkIjpudWxsfQ==&viewMode=site"
  
  payload <- '["LocationsCanada",{"$and":[{"international":{"$ne":true}}]},[{"province":"asc"}],0,115,null,null,null]'
  
  resp <- httr::POST(url,
                     body = payload
                     , content_type_json()
  )
  
  result <- resp %>%
    httr::content(type = "application/json", encoding = "UTF-8")
  
  nyf <- result$result$items
  
  nyf_all <- nyf %>%
    purrr::map_dfr(function(x) {
      address <- x$addressFull$formatted
      lat <- x$addressFull$location$latitude
      lon <- x$addressFull$location$longitude
      
      if (is.null(lat)) lat <- NA
      if (is.null(lon)) lon <- NA
      
      tibble(name = "New York Fries",
             address = address,
             lat = lat,
             lon = lon)
    })
  
  write_csv(nyf_all, "data/restaurants/new_york_fries.csv")
  
  return(nyf_all)  
}


scrape_lunch <- function() {
  url <- "https://thinklunch.ca/locations/"
  
  lunch_html <- rvest::read_html(url)
  
  stores <- lunch_html %>%
    rvest::html_elements(".eluid3e4252ed p") %>% html_text()
  
  store_addr <- stringr::str_remove_all(stores , "\\(.*")
  store_phone <- stringr::str_remove_all(stores, ".*(?=\\()")
  
  machine_addr <- lunch_html %>%
    rvest::html_elements(".eluid4845666f p") %>% html_text()
  
  store <- tibble(name = "LUNCH",
                  address = store_addr,
                  phone = store_phone,
                  note = "Restaurant")
  
  machine <- tibble(name = "LUNCH",
                    address = machine_addr,
                    phone = "",
                    note = "Vending Machine")  
  
  # cafeterias too
  lunch_html <- rvest::read_html("https://thinklunch.ca/menu/")
  
  cafs <- lunch_html %>%
    html_elements("#eluiddfe57a1d_3 .dn-heading") %>% html_text()
  
  caf_addr <- stringr::str_remove(cafs, " \\|.*") %>% stringr::str_trim()
  caf_phone <- stringr::str_extract(cafs, "(?<=\\|).*") %>% stringr::str_trim() %>%
    if_else(is.na(.), "", .)
  
  caf <- tibble(name = "LUNCH",
                address = caf_addr,
                phone = caf_phone,
                note = "Cafeteria")
  
  lunch <- bind_rows(store, machine, caf)    
  
  write_csv(lunch, "data/restaurants/lunch.csv")
  
  return(lunch)
  
}

scrape_littlecasesars <- function() {
  url <- "https://api.cloud.littlecaesars.com/bff/api/GetClosestStores"
  
  payload <- '{"address":{"street":"","city":"ottawa on","state":"","zip":""}}'
  
  headers <- c(`:authority` = "api.cloud.littlecaesars.com", `:method` = "POST", 
               `:path` = "/bff/api/GetClosestStores", `:scheme` = "https", accept = "application/json", 
               `accept-encoding` = "gzip, deflate, br", `accept-language` = "en-ca", 
               `cache-control` = "no-cache, max-age=0", `content-length` = "64", 
               `content-type` = "application/json", origin = "https://littlecaesars.ca", 
               referer = "https://littlecaesars.ca/en-ca/order/pickup/stores/search/ottawa%20on/", 
               `sec-ch-ua` = "\"Chromium\";v=\"94\", \"Google Chrome\";v=\"94\", \";Not A Brand\";v=\"99\"", 
               `sec-ch-ua-mobile` = "?0", `sec-ch-ua-platform` = "\"Windows\"", 
               `sec-fetch-dest` = "empty", `sec-fetch-mode` = "cors", `sec-fetch-site` = "cross-site", 
               `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/94.0.4606.81 Safari/537.36")
  
  resp <- httr::POST(
    url = url,
    body = payload,
    content_type_json(),
    add_headers(.headers = headers)
  )
  
  caesars <- resp %>%
    httr::content()
  
  little_caesars <- caesars$stores %>%
    purrr::map_dfr(function(x) {
      
      phone <- x$phone
      lat <- x$latitude
      lon <- x$longitude
      address <- sprintf("%s, %s, %s, %s",
                         x$address$street,
                         x$address$city,
                         x$address$state,
                         x$address$zip)
      tibble(name = "Little Caesar's",
             address = address,
             phone = phone,
             lat = lat,
             lon = lon)
      
    })
  
  write_csv(little_caesars , "data/restaurants/little_caesars.csv")
  
  return(little_caesars)
  
}


scrape_burgerking <- function() {
  
  url <- "https://use1-prod-bk.rbictg.com/graphql"
  
  # need to do a post
  payload <- '[{"operationName":"GetRestaurants","variables":{"input":{"filter":"NEARBY","coordinates":{"userLat":45.4215296,"userLng":-75.69719309999999,"searchRadius":32000},"first":20,"status":"OPEN"}},
  "query":"query GetRestaurants($input: RestaurantsInput) {  restaurants(input: $input) {    pageInfo {      hasNextPage      endCursor      __typename    }    totalCount    nodes {      ...RestaurantNodeFragment      __typename    }    __typename  }}fragment RestaurantNodeFragment on RestaurantNode {  _id  storeId  isAvailable  posVendor  chaseMerchantId  curbsideHours {    ...OperatingHoursFragment    __typename  }  deliveryHours {    ...OperatingHoursFragment    __typename  }  diningRoomHours {    ...OperatingHoursFragment    __typename  }  distanceInMiles  drinkStationType  driveThruHours {    ...OperatingHoursFragment    __typename  }  driveThruLaneType  email  environment  franchiseGroupId  franchiseGroupName  frontCounterClosed  hasBreakfast  hasBurgersForBreakfast  hasCatering  hasCurbside  hasDelivery  hasDineIn  hasDriveThru  hasMobileOrdering  hasParking  hasPlayground  hasTakeOut  hasWifi  hasLoyalty  id  isDarkKitchen  isFavorite  isHalal  isRecent  latitude  longitude  mobileOrderingStatus  name  number  parkingType  phoneNumber  physicalAddress {    address1    address2    city    country    postalCode    stateProvince    stateProvinceShort    __typename  }  playgroundType  pos {    vendor    __typename  }  posRestaurantId  restaurantImage {    asset {      _id      metadata {        lqip        palette {          dominant {            background            foreground            __typename          }          __typename        }        __typename      }      __typename    }    crop {      top      bottom      left      right      __typename    }    hotspot {      height      width      x      y      __typename    }    __typename  }  restaurantPosData {    _id    __typename  }  status  vatNumber  __typename}fragment OperatingHoursFragment on OperatingHours {  friClose  friOpen  monClose  monOpen  satClose  satOpen  sunClose  sunOpen  thrClose  thrOpen  tueClose  tueOpen  wedClose  wedOpen  __typename}"}]'
  
  
  payload <- '[{"operationName":"GetRestaurants","variables":{"input":{"filter":"NEARBY","coordinates":{"userLat":45.4215296,"userLng":-75.69719309999999,"searchRadius":80000},"first":500,"status":"OPEN"}},
"query":"query GetRestaurants($input: RestaurantsInput) {   restaurants(input: $input) {     pageInfo {       hasNextPage       endCursor       __typename     }     totalCount     nodes {       ...RestaurantNodeFragment       __typename     }     __typename   } }  fragment RestaurantNodeFragment on RestaurantNode {   _id   storeId   isAvailable   posVendor   chaseMerchantId   curbsideHours {     ...OperatingHoursFragment     __typename   }   deliveryHours {     ...OperatingHoursFragment     __typename   }   diningRoomHours {     ...OperatingHoursFragment     __typename   }   distanceInMiles   drinkStationType   driveThruHours {     ...OperatingHoursFragment     __typename   }   driveThruLaneType   email   environment   franchiseGroupId   franchiseGroupName   frontCounterClosed   hasBreakfast   hasBurgersForBreakfast   hasCatering   hasCurbside   hasDelivery   hasDineIn   hasDriveThru   hasMobileOrdering   hasParking   hasPlayground   hasTakeOut   hasWifi   id   isDarkKitchen   isFavorite   isRecent   latitude   longitude   mobileOrderingStatus   name   number   parkingType   phoneNumber   physicalAddress {     address1     address2     city     country     postalCode     stateProvince     stateProvinceShort     __typename   }   playgroundType   pos {     vendor     __typename   }   posRestaurantId   restaurantImage {     asset {       _id       metadata {         lqip         palette {           dominant {             background             foreground             __typename           }           __typename         }         __typename       }       __typename     }     crop {       top       bottom       left       right       __typename     }     hotspot {       height       width       x       y       __typename     }     __typename   }   restaurantPosData {     _id     __typename   }   status   vatNumber   __typename }  fragment OperatingHoursFragment on OperatingHours {   friClose   friOpen   monClose   monOpen   satClose   satOpen   sunClose   sunOpen   thrClose   thrOpen   tueClose   tueOpen   wedClose   wedOpen   __typename } "}]'
  
  headers <- c(`:authority` = "use1-prod-bk.rbictg.com", 
               `:method` = "POST", 
               `:path` = "/graphql", 
               `:scheme` = "https", 
               accept = "*/*", 
               `accept-encoding` = "gzip, deflate, br", 
               `accept-language` = "en-CA,en-GB;q=0.9,en-US;q=0.8,en;q=0.7", 
               `content-length` = "2432", `content-type` = "application/json", 
               origin = "https://www.burgerking.ca", `sec-ch-ua` = "\"Chromium\";v=\"94\", \"Google Chrome\";v=\"94\", \";Not A Brand\";v=\"99\"", 
               `sec-ch-ua-mobile` = "?0", `sec-ch-ua-platform` = "\"Windows\"", 
               `sec-fetch-dest` = "empty", `sec-fetch-mode` = "cors", `sec-fetch-site` = "cross-site", 
               `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/94.0.4606.81 Safari/537.36", 
               `x-forter-token` = "050907ea5d264fa18e54c1e94191dd24_1635263350518__UDF43_13ck_tt", 
               `x-session-id` = "9CF4D212-BA52-4E7A-B44A-A2D49CCF0111", `x-ui-language` = "en", 
               `x-ui-region` = "CA", `x-user-datetime` = "2021-10-26T11:49:20-04:00")
  
  resp <- httr::POST(url,
                     body = payload
                     ,content_type_json()
                     #,add_headers(.headers = headers)
  )   %>%
    httr::content()
  
  
  bk <- resp[[1]]$data$restaurants$nodes
  
  bk_all <- bk %>%
    purrr::map_dfr(function(x){
      address <- if_else(x$physicalAddress$address2 == "", 
                         sprintf("%s, %s, %s, %s", 
                                 x$physicalAddress$address1,
                                 x$physicalAddress$city,
                                 x$physicalAddress$stateProvinceShort,
                                 x$physicalAddress$postalCode),
                         sprintf("%s, %s, %s, %s, %s", 
                                 x$physicalAddress$address1,
                                 x$physicalAddress$address2,
                                 x$physicalAddress$city,
                                 x$physicalAddress$stateProvinceShort,
                                 x$physicalAddress$postalCode))
      
      phone <- x$phoneNumber
      lat <- x$latitude
      lon <- x$longitude
      
      
      tibble(name = "Burger King",
             address = address,
             lat = lat,
             lon = lon,
             phone = phone)
      
    })
  
  write_csv(bk_all, "data/restaurants/burger_king.csv")
  
  return(bk_all)
}


# take headers from google dev console and convert to named character vector
parse_headers <- function(raw_headers){
  headers <- raw_headers %>%
    stringr::str_split(pattern = "\\n") %>%
    unlist() %>%
    tibble(raw = .) %>%
    separate(col = raw, into = c("name", "value"), sep = "(?<=\\w): ", extra = "merge")
  
  h <- headers$value
  names(h) <- headers$name
  
  return (h)
}

scrape_wendys <- function() {
  url <- "https://locationservices.wendys.com/LocationServices/rest/nearbyLocations?&lang=en&cntry=CA&sourceCode=ORDER.WENDYS&version=9.1.4&address=ottawa%2C%20on&limit=500&filterSearch=true&hasMobileOrder=true&radius=2000"
  
  resp <- httr::GET(url,
                    add_headers(.headers = h))  
  
  wendys <- resp  %>%
    httr::content(type = "text/json", encoding = "UTF-8") %>%
    jsonlite::fromJSON() %>%
    flatten() %>%
    as_tibble()
  
  wendys <- wendys %>% 
    mutate(address = sprintf("%s, %s, %s, %s", address1, city, state, postal), name = "Wendy's") %>% 
    select(name, address, phone, lat, lon = lng)
  
  write_csv(wendys, "data/restaurants/wendys.csv") 
  
  return(wendys)
}

scrape_kfc <- function() {
  url <- "https://www.kfc.ca/"
  
  # url won't load, don't care, let's just parse the response from chrome dev console
  
  # this works but only if you put it in an empty file and source it
  j <- '{"Status":0,"AjaxResponseMessage":null,"ExceptionDetails":null,"DataObject":{"DeliveryRestaurants":[],"CarryoutRestaurants":[{"RestaurantGroupId":null,"OpenTime":"/Date(-62135596800000)/","CloseTime":"/Date(-62135596800000)/","PrevOpenTime":"/Date(-62135596800000)/","PrevCloseTime":"/Date(-62135596800000)/","CurbsideService":false,"TimeZone":"Eastern Standard Time","OpenDate":"/Date(-62135596800000)/","CloseDate":"/Date(-62135596800000)/","OpenCloseTimes":{"Tuesday":["11:00 AM - 10:00 PM"]},"RestaurantStatus":null,"FranchiseRedirectionUrl":null,"RestaurantRedirectionUrl":null,"CommunicationMethod":0,"PrimaryAddress":"1470-Ottawa-ON@soulfoodsgroup.com","SecondaryAddress":null,"TaxSourceType":0,"POSType":1,"AlohaStoreKey":0,"RestaurantTimings":{"Sun":"11:00 AM - 10:00 PM","Mon":"11:00 AM - 10:00 PM","Tue":"11:00 AM - 10:00 PM","Wed":"11:00 AM - 10:00 PM","Thu":"11:00 AM - 10:00 PM","Fri":"11:00 AM - 10:00 PM","Sat":"11:00 AM - 10:00 PM"},"ScheduledCloseDates":null,"IsRestaurantOpen":false,"IsSamedayButRestNotOpen":false,"IsRestShutdownForNextDay":false,"IsTechnicalShutDown":false,"MinCarryoutOrderingLimit":0,"MinDeliveryOrderingLimit":5,"MaxCarryOutOrderingLimit":500.00,"MaxDeliveryOrderingLimit":500.00,"ImageName":null,"RestaurantNumber":0,"IsCarryoutSupported":true,"IsDeliverySupported":false,"IsDineInSupported":false,"RestaurantServices":null,"HolidayStartEndDateTimes":[],"POSItemId":"170","StoreLocatorId":null,"RestaurantCity":null,"FranchiseName":"Soul Restaurants Canada Inc","IsOnlinePaymentSupported":false,"IsDealConversionSupported":false,"IsSuggestiveUpsellSupported":false,"IsDealSupported":false,"PaymentType":7,"DeliveryCharge":5.00,"FreeDeliveryOrderAmount":0.00,"DefaultDeliveryCharge":5.00,"IsFreeDeliveryActive":false,"DeliveryAmount":0,"RestaurantOrderModeOpenCloseTimes":[{"RestaurantId":"1470","OrderMode":"Carryout","DayOfWeek":"Sunday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1470","OrderMode":"Delivery","DayOfWeek":"Sunday","OpenTime":"11:00 AM","CloseTime":"09:30 PM"},{"RestaurantId":"1470","OrderMode":"Carryout","DayOfWeek":"Monday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1470","OrderMode":"Delivery","DayOfWeek":"Monday","OpenTime":"11:00 AM","CloseTime":"09:30 PM"},{"RestaurantId":"1470","OrderMode":"Carryout","DayOfWeek":"Tuesday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1470","OrderMode":"Delivery","DayOfWeek":"Tuesday","OpenTime":"11:00 AM","CloseTime":"09:30 PM"},{"RestaurantId":"1470","OrderMode":"Carryout","DayOfWeek":"Wednesday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1470","OrderMode":"Delivery","DayOfWeek":"Wednesday","OpenTime":"11:00 AM","CloseTime":"09:30 PM"},{"RestaurantId":"1470","OrderMode":"Carryout","DayOfWeek":"Thursday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1470","OrderMode":"Delivery","DayOfWeek":"Thursday","OpenTime":"11:00 AM","CloseTime":"09:30 PM"},{"RestaurantId":"1470","OrderMode":"Carryout","DayOfWeek":"Friday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1470","OrderMode":"Delivery","DayOfWeek":"Friday","OpenTime":"11:00 AM","CloseTime":"09:30 PM"},{"RestaurantId":"1470","OrderMode":"Carryout","DayOfWeek":"Saturday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1470","OrderMode":"Delivery","DayOfWeek":"Saturday","OpenTime":"11:00 AM","CloseTime":"09:30 PM"}],"IsRestaurantOpenPreviousDay":false,"PaymentMerchantId":"kfc","PaymentAccountId":"1208","PaymentSubAccountId":"1208","PaymentSecret":"Wxue63P5jA","StoreLimitationMsg":"","Is3dSecure":false,"DeliveryPartnerStoreId":"","ProductExclusionList":[],"EmergencyShutdowns":null,"Tips":null,"TaxDetails":[{"TaxCategory":6,"CategoryDescription":"TVH","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":2,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00},{"TaxCategory":8,"CategoryDescription":"TVH sur la livraison","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":2,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00},{"TaxCategory":1,"CategoryDescription":"TPS","TaxAmount":0,"TaxPercentage":5.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":1,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":4.01},{"TaxCategory":6,"CategoryDescription":"TVH","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":1,"SalesCategory":1,"TaxSlabStartValue":4.01,"TaxSlabEndValue":500.00}],"BottleDepositDetails":[{"BottleSizeId":0,"DepositAmount":0.00,"Size":"591ml"},{"BottleSizeId":0,"DepositAmount":0.00,"Size":"2L"},{"BottleSizeId":0,"DepositAmount":0.00,"Size":"355ml"}],"FutureOrderDayLimit":2,"RestClosedFirstOrderTimeDifferenceCarryout":"0","RestClosedLastOrderTimeDifferenceCarryout":"15","RestClosedFirstOrderTimeDifferenceDelivery":"45","RestClosedLastOrderTimeDifferenceDelivery":"0","ShowCalorie":true,"PromiseTime":null,"OrderMode":0,"TenderWithService":[{"OrderMode":1,"Tendertype":3},{"OrderMode":1,"Tendertype":15},{"OrderMode":1,"Tendertype":14}],"PrefferedLanguage":"en-ca","IPAddress":"10.248.114.70:5556","SendtoDeliveryPartnetForCarryoutOrder":false,"SendtoDeliveryPartnetForDeliveryOrder":false,"RestaurantId":"1470","Name":"1470","Description":"","Address":{"Street":"464 Bank Street  ","City":"Ottawa","ZipCode":"K2P 1Z3","Country":"","Latitude":45.4111,"Longitude":-75.6933,"Distance":1.20,"HouseNumber":"","District":"","SubDistrict":"","Comments":""},"Phone":"(613) 714-4480","Email":null,"OnlineOrderingEnabled":true,"IsFranchise":false,"CarryoutQuoteTime":"15","DeliveryQuoteTime":"40","QuadrantId":null,"QuadrantName":null,"OrderAmount":0,"DisplayName":null,"DetailUrlPart1":null,"DetailUrlPart2":null,"Latitude":0,"Longitude":0,"IsOpenNow":false,"POSID":null,"Distance":"1.20","DayHours":[],"EmergencyDownModes":[],"TechnicalDownModes":[],"ServiceModes":[]},{"RestaurantGroupId":null,"OpenTime":"/Date(-62135596800000)/","CloseTime":"/Date(-62135596800000)/","PrevOpenTime":"/Date(-62135596800000)/","PrevCloseTime":"/Date(-62135596800000)/","CurbsideService":false,"TimeZone":"Eastern Standard Time","OpenDate":"/Date(-62135596800000)/","CloseDate":"/Date(-62135596800000)/","OpenCloseTimes":{"Tuesday":["11:00 AM - 06:30 PM"]},"RestaurantStatus":null,"FranchiseRedirectionUrl":null,"RestaurantRedirectionUrl":null,"CommunicationMethod":0,"PrimaryAddress":"1208-Hull-PQ@soulfoodsgroup.com","SecondaryAddress":null,"TaxSourceType":0,"POSType":9,"AlohaStoreKey":0,"RestaurantTimings":{"Sun":"11:00 AM - 06:30 PM","Mon":"11:00 AM - 06:30 PM","Tue":"11:00 AM - 06:30 PM","Wed":"11:00 AM - 06:30 PM","Thu":"11:00 AM - 06:30 PM","Fri":"11:00 AM - 06:30 PM","Sat":"11:00 AM - 06:30 PM"},"ScheduledCloseDates":null,"IsRestaurantOpen":false,"IsSamedayButRestNotOpen":false,"IsRestShutdownForNextDay":false,"IsTechnicalShutDown":false,"MinCarryoutOrderingLimit":0,"MinDeliveryOrderingLimit":5,"MaxCarryOutOrderingLimit":500.00,"MaxDeliveryOrderingLimit":500.00,"ImageName":null,"RestaurantNumber":0,"IsCarryoutSupported":true,"IsDeliverySupported":true,"IsDineInSupported":false,"RestaurantServices":null,"HolidayStartEndDateTimes":[],"POSItemId":"1208","StoreLocatorId":null,"RestaurantCity":null,"FranchiseName":"Soul Restaurants Canada Inc","IsOnlinePaymentSupported":false,"IsDealConversionSupported":false,"IsSuggestiveUpsellSupported":false,"IsDealSupported":false,"PaymentType":7,"DeliveryCharge":5.00,"FreeDeliveryOrderAmount":0.00,"DefaultDeliveryCharge":5.00,"IsFreeDeliveryActive":false,"DeliveryAmount":0,"RestaurantOrderModeOpenCloseTimes":[{"RestaurantId":"1208","OrderMode":"Carryout","DayOfWeek":"Sunday","OpenTime":"11:00 AM","CloseTime":"06:30 PM"},{"RestaurantId":"1208","OrderMode":"Delivery","DayOfWeek":"Sunday","OpenTime":"11:00 AM","CloseTime":"06:30 PM"},{"RestaurantId":"1208","OrderMode":"Carryout","DayOfWeek":"Monday","OpenTime":"11:00 AM","CloseTime":"06:30 PM"},{"RestaurantId":"1208","OrderMode":"Delivery","DayOfWeek":"Monday","OpenTime":"11:00 AM","CloseTime":"06:30 PM"},{"RestaurantId":"1208","OrderMode":"Carryout","DayOfWeek":"Tuesday","OpenTime":"11:00 AM","CloseTime":"06:30 PM"},{"RestaurantId":"1208","OrderMode":"Delivery","DayOfWeek":"Tuesday","OpenTime":"11:00 AM","CloseTime":"06:30 PM"},{"RestaurantId":"1208","OrderMode":"Carryout","DayOfWeek":"Wednesday","OpenTime":"11:00 AM","CloseTime":"06:30 PM"},{"RestaurantId":"1208","OrderMode":"Delivery","DayOfWeek":"Wednesday","OpenTime":"11:00 AM","CloseTime":"06:30 PM"},{"RestaurantId":"1208","OrderMode":"Carryout","DayOfWeek":"Thursday","OpenTime":"11:00 AM","CloseTime":"06:30 PM"},{"RestaurantId":"1208","OrderMode":"Delivery","DayOfWeek":"Thursday","OpenTime":"11:00 AM","CloseTime":"06:30 PM"},{"RestaurantId":"1208","OrderMode":"Carryout","DayOfWeek":"Friday","OpenTime":"11:00 AM","CloseTime":"06:30 PM"},{"RestaurantId":"1208","OrderMode":"Delivery","DayOfWeek":"Friday","OpenTime":"11:00 AM","CloseTime":"06:30 PM"},{"RestaurantId":"1208","OrderMode":"Carryout","DayOfWeek":"Saturday","OpenTime":"11:00 AM","CloseTime":"06:30 PM"},{"RestaurantId":"1208","OrderMode":"Delivery","DayOfWeek":"Saturday","OpenTime":"11:00 AM","CloseTime":"06:30 PM"}],"IsRestaurantOpenPreviousDay":false,"PaymentMerchantId":"kfc","PaymentAccountId":"1208","PaymentSubAccountId":"1208","PaymentSecret":"Wxue63P5jA","StoreLimitationMsg":"","Is3dSecure":false,"DeliveryPartnerStoreId":"","ProductExclusionList":[],"EmergencyShutdowns":null,"Tips":null,"TaxDetails":[{"TaxCategory":1,"CategoryDescription":"TPS","TaxAmount":0,"TaxPercentage":5.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":2,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00},{"TaxCategory":7,"CategoryDescription":"TVP","TaxAmount":0,"TaxPercentage":9.975,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":2,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00},{"TaxCategory":9,"CategoryDescription":"TPS sur la livraison","TaxAmount":0,"TaxPercentage":5.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":2,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00},{"TaxCategory":10,"CategoryDescription":"Delivery Tax PST","TaxAmount":0,"TaxPercentage":9.975,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":2,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00},{"TaxCategory":1,"CategoryDescription":"TPS","TaxAmount":0,"TaxPercentage":5.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":1,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00},{"TaxCategory":7,"CategoryDescription":"TVP","TaxAmount":0,"TaxPercentage":9.975,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":1,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00}],"BottleDepositDetails":[{"BottleSizeId":0,"DepositAmount":0.05,"Size":"591ml"},{"BottleSizeId":0,"DepositAmount":0.05,"Size":"2L"},{"BottleSizeId":0,"DepositAmount":0.05,"Size":"355ml"}],"FutureOrderDayLimit":2,"RestClosedFirstOrderTimeDifferenceCarryout":"0","RestClosedLastOrderTimeDifferenceCarryout":"30","RestClosedFirstOrderTimeDifferenceDelivery":"45","RestClosedLastOrderTimeDifferenceDelivery":"0","ShowCalorie":false,"PromiseTime":null,"OrderMode":0,"TenderWithService":[{"OrderMode":1,"Tendertype":3},{"OrderMode":1,"Tendertype":15},{"OrderMode":1,"Tendertype":14},{"OrderMode":2,"Tendertype":3},{"OrderMode":2,"Tendertype":15},{"OrderMode":2,"Tendertype":14}],"PrefferedLanguage":"fr-ca","IPAddress":"10.248.12.8","SendtoDeliveryPartnetForCarryoutOrder":false,"SendtoDeliveryPartnetForDeliveryOrder":false,"RestaurantId":"1208","Name":"1208","Description":"","Address":{"Street":"347 Boul St-Joseph  ","City":"Hull","ZipCode":"J8Y 3Z3","Country":"","Latitude":45.4422,"Longitude":-75.7336,"Distance":3.66,"HouseNumber":"","District":"","SubDistrict":"","Comments":""},"Phone":"819-777-5544","Email":null,"OnlineOrderingEnabled":true,"IsFranchise":false,"CarryoutQuoteTime":"15","DeliveryQuoteTime":"40","QuadrantId":null,"QuadrantName":null,"OrderAmount":0,"DisplayName":null,"DetailUrlPart1":null,"DetailUrlPart2":null,"Latitude":0,"Longitude":0,"IsOpenNow":false,"POSID":null,"Distance":"3.66","DayHours":[],"EmergencyDownModes":[],"TechnicalDownModes":[],"ServiceModes":[]},{"RestaurantGroupId":null,"OpenTime":"/Date(-62135596800000)/","CloseTime":"/Date(-62135596800000)/","PrevOpenTime":"/Date(-62135596800000)/","PrevCloseTime":"/Date(-62135596800000)/","CurbsideService":false,"TimeZone":"Eastern Standard Time","OpenDate":"/Date(-62135596800000)/","CloseDate":"/Date(-62135596800000)/","OpenCloseTimes":{"Tuesday":["11:00 AM - 10:00 PM"]},"RestaurantStatus":null,"FranchiseRedirectionUrl":null,"RestaurantRedirectionUrl":null,"CommunicationMethod":0,"PrimaryAddress":"1402-Ottawa-ON@soulfoodsgroup.com","SecondaryAddress":null,"TaxSourceType":0,"POSType":1,"AlohaStoreKey":0,"RestaurantTimings":{"Sun":"11:00 AM - 10:00 PM","Mon":"11:00 AM - 10:00 PM","Tue":"11:00 AM - 10:00 PM","Wed":"11:00 AM - 10:00 PM","Thu":"11:00 AM - 10:00 PM","Fri":"11:00 AM - 10:00 PM","Sat":"11:00 AM - 10:00 PM"},"ScheduledCloseDates":null,"IsRestaurantOpen":false,"IsSamedayButRestNotOpen":false,"IsRestShutdownForNextDay":false,"IsTechnicalShutDown":false,"MinCarryoutOrderingLimit":0,"MinDeliveryOrderingLimit":5,"MaxCarryOutOrderingLimit":500.00,"MaxDeliveryOrderingLimit":500.00,"ImageName":null,"RestaurantNumber":0,"IsCarryoutSupported":true,"IsDeliverySupported":true,"IsDineInSupported":false,"RestaurantServices":null,"HolidayStartEndDateTimes":[],"POSItemId":"190","StoreLocatorId":null,"RestaurantCity":null,"FranchiseName":"Soul Restaurants Canada Inc","IsOnlinePaymentSupported":false,"IsDealConversionSupported":false,"IsSuggestiveUpsellSupported":false,"IsDealSupported":false,"PaymentType":7,"DeliveryCharge":5.00,"FreeDeliveryOrderAmount":0.00,"DefaultDeliveryCharge":5.00,"IsFreeDeliveryActive":false,"DeliveryAmount":0,"RestaurantOrderModeOpenCloseTimes":[{"RestaurantId":"1402","OrderMode":"Carryout","DayOfWeek":"Sunday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1402","OrderMode":"Delivery","DayOfWeek":"Sunday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1402","OrderMode":"Carryout","DayOfWeek":"Monday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1402","OrderMode":"Delivery","DayOfWeek":"Monday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1402","OrderMode":"Carryout","DayOfWeek":"Tuesday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1402","OrderMode":"Delivery","DayOfWeek":"Tuesday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1402","OrderMode":"Carryout","DayOfWeek":"Wednesday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1402","OrderMode":"Delivery","DayOfWeek":"Wednesday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1402","OrderMode":"Carryout","DayOfWeek":"Thursday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1402","OrderMode":"Delivery","DayOfWeek":"Thursday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1402","OrderMode":"Carryout","DayOfWeek":"Friday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1402","OrderMode":"Delivery","DayOfWeek":"Friday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1402","OrderMode":"Carryout","DayOfWeek":"Saturday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1402","OrderMode":"Delivery","DayOfWeek":"Saturday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"}],"IsRestaurantOpenPreviousDay":false,"PaymentMerchantId":"kfc","PaymentAccountId":"1208","PaymentSubAccountId":"1208","PaymentSecret":"Wxue63P5jA","StoreLimitationMsg":"","Is3dSecure":false,"DeliveryPartnerStoreId":"","ProductExclusionList":[],"EmergencyShutdowns":null,"Tips":null,"TaxDetails":[{"TaxCategory":6,"CategoryDescription":"TVH","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":2,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00},{"TaxCategory":8,"CategoryDescription":"TVH sur la livraison","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":2,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00},{"TaxCategory":1,"CategoryDescription":"TPS","TaxAmount":0,"TaxPercentage":5.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":1,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":4.01},{"TaxCategory":6,"CategoryDescription":"TVH","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":1,"SalesCategory":1,"TaxSlabStartValue":4.01,"TaxSlabEndValue":500.00}],"BottleDepositDetails":[{"BottleSizeId":0,"DepositAmount":0.00,"Size":"591ml"},{"BottleSizeId":0,"DepositAmount":0.00,"Size":"2L"},{"BottleSizeId":0,"DepositAmount":0.00,"Size":"355ml"}],"FutureOrderDayLimit":2,"RestClosedFirstOrderTimeDifferenceCarryout":"0","RestClosedLastOrderTimeDifferenceCarryout":"15","RestClosedFirstOrderTimeDifferenceDelivery":"45","RestClosedLastOrderTimeDifferenceDelivery":"0","ShowCalorie":true,"PromiseTime":null,"OrderMode":0,"TenderWithService":[{"OrderMode":1,"Tendertype":3},{"OrderMode":1,"Tendertype":15},{"OrderMode":1,"Tendertype":14},{"OrderMode":2,"Tendertype":3},{"OrderMode":2,"Tendertype":15},{"OrderMode":2,"Tendertype":14}],"PrefferedLanguage":"en-ca","IPAddress":"10.248.114.2:5556","SendtoDeliveryPartnetForCarryoutOrder":false,"SendtoDeliveryPartnetForDeliveryOrder":false,"RestaurantId":"1402","Name":"1402","Description":"","Address":{"Street":"932 St. Laurent Blvd  ","City":"Ottawa","ZipCode":"K1K 3B3","Country":"","Latitude":45.4311,"Longitude":-75.6414,"Distance":4.49,"HouseNumber":"","District":"","SubDistrict":"","Comments":""},"Phone":"(613) 745-4466","Email":null,"OnlineOrderingEnabled":true,"IsFranchise":false,"CarryoutQuoteTime":"15","DeliveryQuoteTime":"40","QuadrantId":null,"QuadrantName":null,"OrderAmount":0,"DisplayName":null,"DetailUrlPart1":null,"DetailUrlPart2":null,"Latitude":0,"Longitude":0,"IsOpenNow":false,"POSID":null,"Distance":"4.49","DayHours":[],"EmergencyDownModes":[],"TechnicalDownModes":[],"ServiceModes":[]},{"RestaurantGroupId":null,"OpenTime":"/Date(-62135596800000)/","CloseTime":"/Date(-62135596800000)/","PrevOpenTime":"/Date(-62135596800000)/","PrevCloseTime":"/Date(-62135596800000)/","CurbsideService":false,"TimeZone":"Eastern Standard Time","OpenDate":"/Date(-62135596800000)/","CloseDate":"/Date(-62135596800000)/","OpenCloseTimes":{"Tuesday":["11:00 AM - 10:00 PM"]},"RestaurantStatus":null,"FranchiseRedirectionUrl":null,"RestaurantRedirectionUrl":null,"CommunicationMethod":0,"PrimaryAddress":"1405-Ottawa-ON@soulfoodsgroup.com","SecondaryAddress":null,"TaxSourceType":0,"POSType":1,"AlohaStoreKey":0,"RestaurantTimings":{"Sun":"11:00 AM - 10:00 PM","Mon":"11:00 AM - 10:00 PM","Tue":"11:00 AM - 10:00 PM","Wed":"11:00 AM - 10:00 PM","Thu":"11:00 AM - 10:00 PM","Fri":"11:00 AM - 10:00 PM","Sat":"11:00 AM - 10:00 PM"},"ScheduledCloseDates":null,"IsRestaurantOpen":false,"IsSamedayButRestNotOpen":false,"IsRestShutdownForNextDay":false,"IsTechnicalShutDown":false,"MinCarryoutOrderingLimit":0,"MinDeliveryOrderingLimit":5,"MaxCarryOutOrderingLimit":500.00,"MaxDeliveryOrderingLimit":500.00,"ImageName":null,"RestaurantNumber":0,"IsCarryoutSupported":true,"IsDeliverySupported":true,"IsDineInSupported":false,"RestaurantServices":null,"HolidayStartEndDateTimes":[],"POSItemId":"194","StoreLocatorId":null,"RestaurantCity":null,"FranchiseName":"Soul Restaurants Canada Inc","IsOnlinePaymentSupported":false,"IsDealConversionSupported":false,"IsSuggestiveUpsellSupported":false,"IsDealSupported":false,"PaymentType":7,"DeliveryCharge":5.00,"FreeDeliveryOrderAmount":0.00,"DefaultDeliveryCharge":5.00,"IsFreeDeliveryActive":false,"DeliveryAmount":0,"RestaurantOrderModeOpenCloseTimes":[{"RestaurantId":"1405","OrderMode":"Carryout","DayOfWeek":"Sunday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1405","OrderMode":"Delivery","DayOfWeek":"Sunday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1405","OrderMode":"Carryout","DayOfWeek":"Monday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1405","OrderMode":"Delivery","DayOfWeek":"Monday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1405","OrderMode":"Carryout","DayOfWeek":"Tuesday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1405","OrderMode":"Delivery","DayOfWeek":"Tuesday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1405","OrderMode":"Carryout","DayOfWeek":"Wednesday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1405","OrderMode":"Delivery","DayOfWeek":"Wednesday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1405","OrderMode":"Carryout","DayOfWeek":"Thursday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1405","OrderMode":"Delivery","DayOfWeek":"Thursday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1405","OrderMode":"Carryout","DayOfWeek":"Friday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1405","OrderMode":"Delivery","DayOfWeek":"Friday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1405","OrderMode":"Carryout","DayOfWeek":"Saturday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1405","OrderMode":"Delivery","DayOfWeek":"Saturday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"}],"IsRestaurantOpenPreviousDay":false,"PaymentMerchantId":"kfc","PaymentAccountId":"1208","PaymentSubAccountId":"1208","PaymentSecret":"Wxue63P5jA","StoreLimitationMsg":"","Is3dSecure":false,"DeliveryPartnerStoreId":"","ProductExclusionList":[],"EmergencyShutdowns":null,"Tips":null,"TaxDetails":[{"TaxCategory":6,"CategoryDescription":"TVH","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":2,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00},{"TaxCategory":8,"CategoryDescription":"TVH sur la livraison","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":2,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00},{"TaxCategory":1,"CategoryDescription":"TPS","TaxAmount":0,"TaxPercentage":5.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":1,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":4.01},{"TaxCategory":6,"CategoryDescription":"TVH","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":1,"SalesCategory":1,"TaxSlabStartValue":4.01,"TaxSlabEndValue":500.00}],"BottleDepositDetails":[{"BottleSizeId":0,"DepositAmount":0.00,"Size":"591ml"},{"BottleSizeId":0,"DepositAmount":0.00,"Size":"2L"},{"BottleSizeId":0,"DepositAmount":0.00,"Size":"355ml"}],"FutureOrderDayLimit":2,"RestClosedFirstOrderTimeDifferenceCarryout":"0","RestClosedLastOrderTimeDifferenceCarryout":"15","RestClosedFirstOrderTimeDifferenceDelivery":"45","RestClosedLastOrderTimeDifferenceDelivery":"0","ShowCalorie":true,"PromiseTime":null,"OrderMode":0,"TenderWithService":[{"OrderMode":1,"Tendertype":3},{"OrderMode":1,"Tendertype":15},{"OrderMode":1,"Tendertype":14},{"OrderMode":2,"Tendertype":3},{"OrderMode":2,"Tendertype":15},{"OrderMode":2,"Tendertype":14}],"PrefferedLanguage":"en-ca","IPAddress":"10.248.114.5:5556","SendtoDeliveryPartnetForCarryoutOrder":false,"SendtoDeliveryPartnetForDeliveryOrder":false,"RestaurantId":"1405","Name":"1405","Description":"","Address":{"Street":"2670 Alta Vista Drive  ","City":"Ottawa","ZipCode":"K1V 7T4","Country":"","Latitude":45.3735022,"Longitude":-75.6658696,"Distance":5.87,"HouseNumber":"","District":"","SubDistrict":"","Comments":""},"Phone":"613-733-1222","Email":null,"OnlineOrderingEnabled":true,"IsFranchise":false,"CarryoutQuoteTime":"15","DeliveryQuoteTime":"40","QuadrantId":null,"QuadrantName":null,"OrderAmount":0,"DisplayName":null,"DetailUrlPart1":null,"DetailUrlPart2":null,"Latitude":0,"Longitude":0,"IsOpenNow":false,"POSID":null,"Distance":"5.87","DayHours":[],"EmergencyDownModes":[],"TechnicalDownModes":[],"ServiceModes":[]},{"RestaurantGroupId":null,"OpenTime":"/Date(-62135596800000)/","CloseTime":"/Date(-62135596800000)/","PrevOpenTime":"/Date(-62135596800000)/","PrevCloseTime":"/Date(-62135596800000)/","CurbsideService":false,"TimeZone":"Eastern Standard Time","OpenDate":"/Date(-62135596800000)/","CloseDate":"/Date(-62135596800000)/","OpenCloseTimes":{"Tuesday":["11:00 AM - 08:00 PM"]},"RestaurantStatus":null,"FranchiseRedirectionUrl":null,"RestaurantRedirectionUrl":null,"CommunicationMethod":0,"PrimaryAddress":"1209-Gatineau-PQ@soulfoodsgroup.com","SecondaryAddress":null,"TaxSourceType":0,"POSType":9,"AlohaStoreKey":0,"RestaurantTimings":{"Sun":"11:00 AM - 08:00 PM","Mon":"11:00 AM - 08:00 PM","Tue":"11:00 AM - 08:00 PM","Wed":"11:00 AM - 08:00 PM","Thu":"11:00 AM - 08:00 PM","Fri":"11:00 AM - 08:00 PM","Sat":"11:00 AM - 08:00 PM"},"ScheduledCloseDates":null,"IsRestaurantOpen":false,"IsSamedayButRestNotOpen":false,"IsRestShutdownForNextDay":false,"IsTechnicalShutDown":false,"MinCarryoutOrderingLimit":0,"MinDeliveryOrderingLimit":5,"MaxCarryOutOrderingLimit":500.00,"MaxDeliveryOrderingLimit":500.00,"ImageName":null,"RestaurantNumber":0,"IsCarryoutSupported":true,"IsDeliverySupported":true,"IsDineInSupported":false,"RestaurantServices":null,"HolidayStartEndDateTimes":[],"POSItemId":"1209","StoreLocatorId":null,"RestaurantCity":null,"FranchiseName":"Soul Restaurants Canada Inc","IsOnlinePaymentSupported":false,"IsDealConversionSupported":false,"IsSuggestiveUpsellSupported":false,"IsDealSupported":false,"PaymentType":7,"DeliveryCharge":5.00,"FreeDeliveryOrderAmount":0,"DefaultDeliveryCharge":5.00,"IsFreeDeliveryActive":false,"DeliveryAmount":0,"RestaurantOrderModeOpenCloseTimes":[{"RestaurantId":"1209","OrderMode":"Carryout","DayOfWeek":"Sunday","OpenTime":"11:00 AM","CloseTime":"08:00 PM"},{"RestaurantId":"1209","OrderMode":"Delivery","DayOfWeek":"Sunday","OpenTime":"11:00 AM","CloseTime":"09:30 PM"},{"RestaurantId":"1209","OrderMode":"Carryout","DayOfWeek":"Monday","OpenTime":"11:00 AM","CloseTime":"08:00 PM"},{"RestaurantId":"1209","OrderMode":"Delivery","DayOfWeek":"Monday","OpenTime":"11:00 AM","CloseTime":"09:30 PM"},{"RestaurantId":"1209","OrderMode":"Carryout","DayOfWeek":"Tuesday","OpenTime":"11:00 AM","CloseTime":"08:00 PM"},{"RestaurantId":"1209","OrderMode":"Delivery","DayOfWeek":"Tuesday","OpenTime":"11:00 AM","CloseTime":"09:30 PM"},{"RestaurantId":"1209","OrderMode":"Carryout","DayOfWeek":"Wednesday","OpenTime":"11:00 AM","CloseTime":"08:00 PM"},{"RestaurantId":"1209","OrderMode":"Delivery","DayOfWeek":"Wednesday","OpenTime":"11:00 AM","CloseTime":"09:30 PM"},{"RestaurantId":"1209","OrderMode":"Carryout","DayOfWeek":"Thursday","OpenTime":"11:00 AM","CloseTime":"08:00 PM"},{"RestaurantId":"1209","OrderMode":"Delivery","DayOfWeek":"Thursday","OpenTime":"11:00 AM","CloseTime":"09:30 PM"},{"RestaurantId":"1209","OrderMode":"Carryout","DayOfWeek":"Friday","OpenTime":"11:00 AM","CloseTime":"08:00 PM"},{"RestaurantId":"1209","OrderMode":"Delivery","DayOfWeek":"Friday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1209","OrderMode":"Carryout","DayOfWeek":"Saturday","OpenTime":"11:00 AM","CloseTime":"08:00 PM"},{"RestaurantId":"1209","OrderMode":"Delivery","DayOfWeek":"Saturday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"}],"IsRestaurantOpenPreviousDay":false,"PaymentMerchantId":"kfc","PaymentAccountId":"1208","PaymentSubAccountId":"1208","PaymentSecret":"Wxue63P5jA","StoreLimitationMsg":"","Is3dSecure":false,"DeliveryPartnerStoreId":"","ProductExclusionList":[],"EmergencyShutdowns":null,"Tips":null,"TaxDetails":[{"TaxCategory":1,"CategoryDescription":"TPS","TaxAmount":0,"TaxPercentage":5.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":2,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00},{"TaxCategory":7,"CategoryDescription":"TVP","TaxAmount":0,"TaxPercentage":9.975,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":2,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00},{"TaxCategory":9,"CategoryDescription":"TPS sur la livraison","TaxAmount":0,"TaxPercentage":5.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":2,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00},{"TaxCategory":10,"CategoryDescription":"Delivery Tax PST","TaxAmount":0,"TaxPercentage":9.975,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":2,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00},{"TaxCategory":1,"CategoryDescription":"TPS","TaxAmount":0,"TaxPercentage":5.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":1,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00},{"TaxCategory":7,"CategoryDescription":"TVP","TaxAmount":0,"TaxPercentage":9.975,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":1,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00}],"BottleDepositDetails":[{"BottleSizeId":0,"DepositAmount":0.05,"Size":"591ml"},{"BottleSizeId":0,"DepositAmount":0.05,"Size":"2L"},{"BottleSizeId":0,"DepositAmount":0.05,"Size":"355ml"}],"FutureOrderDayLimit":2,"RestClosedFirstOrderTimeDifferenceCarryout":"0","RestClosedLastOrderTimeDifferenceCarryout":"30","RestClosedFirstOrderTimeDifferenceDelivery":"45","RestClosedLastOrderTimeDifferenceDelivery":"0","ShowCalorie":false,"PromiseTime":null,"OrderMode":0,"TenderWithService":[{"OrderMode":1,"Tendertype":3},{"OrderMode":1,"Tendertype":15},{"OrderMode":1,"Tendertype":14},{"OrderMode":2,"Tendertype":3},{"OrderMode":2,"Tendertype":15},{"OrderMode":2,"Tendertype":14}],"PrefferedLanguage":"fr-ca","IPAddress":"10.248.12.9","SendtoDeliveryPartnetForCarryoutOrder":false,"SendtoDeliveryPartnetForDeliveryOrder":false,"RestaurantId":"1209","Name":"1209","Description":"","Address":{"Street":"258 Rue Notre-Dame  ","City":"Gatineau","ZipCode":"J8P 1K4","Country":"","Latitude":45.4824,"Longitude":-75.6464,"Distance":7.85,"HouseNumber":"","District":"","SubDistrict":"","Comments":""},"Phone":"819-663-8686","Email":null,"OnlineOrderingEnabled":true,"IsFranchise":false,"CarryoutQuoteTime":"15","DeliveryQuoteTime":"40","QuadrantId":null,"QuadrantName":null,"OrderAmount":0,"DisplayName":null,"DetailUrlPart1":null,"DetailUrlPart2":null,"Latitude":0,"Longitude":0,"IsOpenNow":false,"POSID":null,"Distance":"7.85","DayHours":[],"EmergencyDownModes":[],"TechnicalDownModes":[],"ServiceModes":[]},{"RestaurantGroupId":null,"OpenTime":"/Date(-62135596800000)/","CloseTime":"/Date(-62135596800000)/","PrevOpenTime":"/Date(-62135596800000)/","PrevCloseTime":"/Date(-62135596800000)/","CurbsideService":false,"TimeZone":"Eastern Standard Time","OpenDate":"/Date(-62135596800000)/","CloseDate":"/Date(-62135596800000)/","OpenCloseTimes":{"Tuesday":["11:00 AM - 10:00 PM"]},"RestaurantStatus":null,"FranchiseRedirectionUrl":null,"RestaurantRedirectionUrl":null,"CommunicationMethod":0,"PrimaryAddress":"1407-Nepean-ON@soulfoodsgroup.com","SecondaryAddress":null,"TaxSourceType":0,"POSType":1,"AlohaStoreKey":0,"RestaurantTimings":{"Sun":"11:00 AM - 10:00 PM","Mon":"11:00 AM - 10:00 PM","Tue":"11:00 AM - 10:00 PM","Wed":"11:00 AM - 10:00 PM","Thu":"11:00 AM - 10:00 PM","Fri":"11:00 AM - 10:00 PM","Sat":"11:00 AM - 10:00 PM"},"ScheduledCloseDates":null,"IsRestaurantOpen":false,"IsSamedayButRestNotOpen":false,"IsRestShutdownForNextDay":false,"IsTechnicalShutDown":false,"MinCarryoutOrderingLimit":0,"MinDeliveryOrderingLimit":5,"MaxCarryOutOrderingLimit":500.00,"MaxDeliveryOrderingLimit":500.00,"ImageName":null,"RestaurantNumber":0,"IsCarryoutSupported":true,"IsDeliverySupported":true,"IsDineInSupported":false,"RestaurantServices":null,"HolidayStartEndDateTimes":[],"POSItemId":"200","StoreLocatorId":null,"RestaurantCity":null,"FranchiseName":"Soul Restaurants Canada Inc","IsOnlinePaymentSupported":false,"IsDealConversionSupported":false,"IsSuggestiveUpsellSupported":false,"IsDealSupported":false,"PaymentType":7,"DeliveryCharge":5.00,"FreeDeliveryOrderAmount":0.00,"DefaultDeliveryCharge":5.00,"IsFreeDeliveryActive":false,"DeliveryAmount":0,"RestaurantOrderModeOpenCloseTimes":[{"RestaurantId":"1407","OrderMode":"Carryout","DayOfWeek":"Sunday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1407","OrderMode":"Delivery","DayOfWeek":"Sunday","OpenTime":"11:00 AM","CloseTime":"09:30 PM"},{"RestaurantId":"1407","OrderMode":"Carryout","DayOfWeek":"Monday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1407","OrderMode":"Delivery","DayOfWeek":"Monday","OpenTime":"11:00 AM","CloseTime":"09:30 PM"},{"RestaurantId":"1407","OrderMode":"Carryout","DayOfWeek":"Tuesday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1407","OrderMode":"Delivery","DayOfWeek":"Tuesday","OpenTime":"11:00 AM","CloseTime":"09:30 PM"},{"RestaurantId":"1407","OrderMode":"Carryout","DayOfWeek":"Wednesday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1407","OrderMode":"Delivery","DayOfWeek":"Wednesday","OpenTime":"11:00 AM","CloseTime":"09:30 PM"},{"RestaurantId":"1407","OrderMode":"Carryout","DayOfWeek":"Thursday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1407","OrderMode":"Delivery","DayOfWeek":"Thursday","OpenTime":"11:00 AM","CloseTime":"09:30 PM"},{"RestaurantId":"1407","OrderMode":"Carryout","DayOfWeek":"Friday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1407","OrderMode":"Delivery","DayOfWeek":"Friday","OpenTime":"11:00 AM","CloseTime":"09:30 PM"},{"RestaurantId":"1407","OrderMode":"Carryout","DayOfWeek":"Saturday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1407","OrderMode":"Delivery","DayOfWeek":"Saturday","OpenTime":"11:00 AM","CloseTime":"09:30 PM"}],"IsRestaurantOpenPreviousDay":false,"PaymentMerchantId":"kfc","PaymentAccountId":"1208","PaymentSubAccountId":"1208","PaymentSecret":"Wxue63P5jA","StoreLimitationMsg":"","Is3dSecure":false,"DeliveryPartnerStoreId":"","ProductExclusionList":[],"EmergencyShutdowns":null,"Tips":null,"TaxDetails":[{"TaxCategory":6,"CategoryDescription":"TVH","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":2,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00},{"TaxCategory":8,"CategoryDescription":"TVH sur la livraison","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":2,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00},{"TaxCategory":1,"CategoryDescription":"TPS","TaxAmount":0,"TaxPercentage":5.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":1,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":4.01},{"TaxCategory":6,"CategoryDescription":"TVH","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":1,"SalesCategory":1,"TaxSlabStartValue":4.01,"TaxSlabEndValue":500.00}],"BottleDepositDetails":[{"BottleSizeId":0,"DepositAmount":0.00,"Size":"591ml"},{"BottleSizeId":0,"DepositAmount":0.00,"Size":"2L"},{"BottleSizeId":0,"DepositAmount":0.00,"Size":"355ml"}],"FutureOrderDayLimit":2,"RestClosedFirstOrderTimeDifferenceCarryout":"0","RestClosedLastOrderTimeDifferenceCarryout":"15","RestClosedFirstOrderTimeDifferenceDelivery":"45","RestClosedLastOrderTimeDifferenceDelivery":"0","ShowCalorie":true,"PromiseTime":null,"OrderMode":0,"TenderWithService":[{"OrderMode":1,"Tendertype":3},{"OrderMode":1,"Tendertype":15},{"OrderMode":1,"Tendertype":14},{"OrderMode":2,"Tendertype":3},{"OrderMode":2,"Tendertype":15},{"OrderMode":2,"Tendertype":14}],"PrefferedLanguage":"en-ca","IPAddress":"10.248.114.7:5556","SendtoDeliveryPartnetForCarryoutOrder":false,"SendtoDeliveryPartnetForDeliveryOrder":false,"RestaurantId":"1407","Name":"1407","Description":"","Address":{"Street":"1556 Merivale Rd  ","City":"Nepean","ZipCode":"K2G 3J8","Country":"","Latitude":45.3535,"Longitude":-75.7355,"Distance":8.13,"HouseNumber":"","District":"","SubDistrict":"","Comments":""},"Phone":"613-224-2346","Email":null,"OnlineOrderingEnabled":true,"IsFranchise":false,"CarryoutQuoteTime":"15","DeliveryQuoteTime":"40","QuadrantId":null,"QuadrantName":null,"OrderAmount":0,"DisplayName":null,"DetailUrlPart1":null,"DetailUrlPart2":null,"Latitude":0,"Longitude":0,"IsOpenNow":false,"POSID":null,"Distance":"8.13","DayHours":[],"EmergencyDownModes":[],"TechnicalDownModes":[],"ServiceModes":[]},{"RestaurantGroupId":null,"OpenTime":"/Date(-62135596800000)/","CloseTime":"/Date(-62135596800000)/","PrevOpenTime":"/Date(-62135596800000)/","PrevCloseTime":"/Date(-62135596800000)/","CurbsideService":false,"TimeZone":"Eastern Standard Time","OpenDate":"/Date(-62135596800000)/","CloseDate":"/Date(-62135596800000)/","OpenCloseTimes":{"Tuesday":["11:00 AM - 08:00 PM"]},"RestaurantStatus":null,"FranchiseRedirectionUrl":null,"RestaurantRedirectionUrl":null,"CommunicationMethod":0,"PrimaryAddress":"1421-Ottawa-ON@soulfoodsgroup.com","SecondaryAddress":null,"TaxSourceType":0,"POSType":1,"AlohaStoreKey":0,"RestaurantTimings":{"Sun":"11:00 AM - 08:00 PM","Mon":"11:00 AM - 08:00 PM","Tue":"11:00 AM - 08:00 PM","Wed":"11:00 AM - 08:00 PM","Thu":"11:00 AM - 08:00 PM","Fri":"11:00 AM - 08:00 PM","Sat":"11:00 AM - 08:00 PM"},"ScheduledCloseDates":null,"IsRestaurantOpen":false,"IsSamedayButRestNotOpen":false,"IsRestShutdownForNextDay":false,"IsTechnicalShutDown":false,"MinCarryoutOrderingLimit":0,"MinDeliveryOrderingLimit":5,"MaxCarryOutOrderingLimit":500.00,"MaxDeliveryOrderingLimit":500.00,"ImageName":null,"RestaurantNumber":0,"IsCarryoutSupported":true,"IsDeliverySupported":true,"IsDineInSupported":false,"RestaurantServices":null,"HolidayStartEndDateTimes":[],"POSItemId":"160","StoreLocatorId":null,"RestaurantCity":null,"FranchiseName":"Soul Restaurants Canada Inc","IsOnlinePaymentSupported":false,"IsDealConversionSupported":false,"IsSuggestiveUpsellSupported":false,"IsDealSupported":false,"PaymentType":7,"DeliveryCharge":5.00,"FreeDeliveryOrderAmount":0.00,"DefaultDeliveryCharge":5.00,"IsFreeDeliveryActive":false,"DeliveryAmount":0,"RestaurantOrderModeOpenCloseTimes":[{"RestaurantId":"1421","OrderMode":"Carryout","DayOfWeek":"Sunday","OpenTime":"11:00 AM","CloseTime":"08:00 PM"},{"RestaurantId":"1421","OrderMode":"Delivery","DayOfWeek":"Sunday","OpenTime":"11:00 AM","CloseTime":"08:00 PM"},{"RestaurantId":"1421","OrderMode":"Carryout","DayOfWeek":"Monday","OpenTime":"11:00 AM","CloseTime":"08:00 PM"},{"RestaurantId":"1421","OrderMode":"Delivery","DayOfWeek":"Monday","OpenTime":"11:00 AM","CloseTime":"08:00 PM"},{"RestaurantId":"1421","OrderMode":"Carryout","DayOfWeek":"Tuesday","OpenTime":"11:00 AM","CloseTime":"08:00 PM"},{"RestaurantId":"1421","OrderMode":"Delivery","DayOfWeek":"Tuesday","OpenTime":"11:00 AM","CloseTime":"08:00 PM"},{"RestaurantId":"1421","OrderMode":"Carryout","DayOfWeek":"Wednesday","OpenTime":"11:00 AM","CloseTime":"08:00 PM"},{"RestaurantId":"1421","OrderMode":"Delivery","DayOfWeek":"Wednesday","OpenTime":"11:00 AM","CloseTime":"08:00 PM"},{"RestaurantId":"1421","OrderMode":"Carryout","DayOfWeek":"Thursday","OpenTime":"11:00 AM","CloseTime":"08:00 PM"},{"RestaurantId":"1421","OrderMode":"Delivery","DayOfWeek":"Thursday","OpenTime":"11:00 AM","CloseTime":"08:00 PM"},{"RestaurantId":"1421","OrderMode":"Carryout","DayOfWeek":"Friday","OpenTime":"11:00 AM","CloseTime":"08:00 PM"},{"RestaurantId":"1421","OrderMode":"Delivery","DayOfWeek":"Friday","OpenTime":"11:00 AM","CloseTime":"08:00 PM"},{"RestaurantId":"1421","OrderMode":"Carryout","DayOfWeek":"Saturday","OpenTime":"11:00 AM","CloseTime":"08:00 PM"},{"RestaurantId":"1421","OrderMode":"Delivery","DayOfWeek":"Saturday","OpenTime":"11:00 AM","CloseTime":"08:00 PM"}],"IsRestaurantOpenPreviousDay":false,"PaymentMerchantId":"kfc","PaymentAccountId":"1208","PaymentSubAccountId":"1208","PaymentSecret":"Wxue63P5jA","StoreLimitationMsg":"","Is3dSecure":false,"DeliveryPartnerStoreId":"","ProductExclusionList":[],"EmergencyShutdowns":null,"Tips":null,"TaxDetails":[{"TaxCategory":6,"CategoryDescription":"TVH","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":2,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00},{"TaxCategory":1,"CategoryDescription":"TPS","TaxAmount":0,"TaxPercentage":5.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":1,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":4.01},{"TaxCategory":6,"CategoryDescription":"TVH","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":1,"SalesCategory":1,"TaxSlabStartValue":4.01,"TaxSlabEndValue":500.00},{"TaxCategory":8,"CategoryDescription":"TVH sur la livraison","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":2,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00}],"BottleDepositDetails":[{"BottleSizeId":0,"DepositAmount":0.00,"Size":"591ml"},{"BottleSizeId":0,"DepositAmount":0.00,"Size":"2L"},{"BottleSizeId":0,"DepositAmount":0.00,"Size":"355ml"}],"FutureOrderDayLimit":2,"RestClosedFirstOrderTimeDifferenceCarryout":"0","RestClosedLastOrderTimeDifferenceCarryout":"0","RestClosedFirstOrderTimeDifferenceDelivery":"0","RestClosedLastOrderTimeDifferenceDelivery":"0","ShowCalorie":true,"PromiseTime":null,"OrderMode":0,"TenderWithService":[{"OrderMode":1,"Tendertype":3},{"OrderMode":1,"Tendertype":15},{"OrderMode":1,"Tendertype":14},{"OrderMode":2,"Tendertype":3},{"OrderMode":2,"Tendertype":15},{"OrderMode":2,"Tendertype":14}],"PrefferedLanguage":"en-ca","IPAddress":"10.248.114.21:5556","SendtoDeliveryPartnetForCarryoutOrder":false,"SendtoDeliveryPartnetForDeliveryOrder":false,"RestaurantId":"1421","Name":"1421","Description":"","Address":{"Street":"340 Hunt Club Road W  ","City":"Ottawa","ZipCode":"K2E 6T6","Country":"","Latitude":45.3374,"Longitude":-75.7159,"Distance":9.46,"HouseNumber":"","District":"","SubDistrict":"","Comments":""},"Phone":"(613) 714-4476","Email":null,"OnlineOrderingEnabled":true,"IsFranchise":false,"CarryoutQuoteTime":"15","DeliveryQuoteTime":"40","QuadrantId":null,"QuadrantName":null,"OrderAmount":0,"DisplayName":null,"DetailUrlPart1":null,"DetailUrlPart2":null,"Latitude":0,"Longitude":0,"IsOpenNow":false,"POSID":null,"Distance":"9.46","DayHours":[],"EmergencyDownModes":[],"TechnicalDownModes":[],"ServiceModes":[]},{"RestaurantGroupId":null,"OpenTime":"/Date(-62135596800000)/","CloseTime":"/Date(-62135596800000)/","PrevOpenTime":"/Date(-62135596800000)/","PrevCloseTime":"/Date(-62135596800000)/","CurbsideService":false,"TimeZone":"Eastern Standard Time","OpenDate":"/Date(-62135596800000)/","CloseDate":"/Date(-62135596800000)/","OpenCloseTimes":{"Tuesday":["11:00 AM - 10:00 PM"]},"RestaurantStatus":null,"FranchiseRedirectionUrl":null,"RestaurantRedirectionUrl":null,"CommunicationMethod":0,"PrimaryAddress":"1406-Ottawa-ON@soulfoodsgroup.com","SecondaryAddress":null,"TaxSourceType":0,"POSType":1,"AlohaStoreKey":0,"RestaurantTimings":{"Sun":"11:00 AM - 10:00 PM","Mon":"11:00 AM - 10:00 PM","Tue":"11:00 AM - 10:00 PM","Wed":"11:00 AM - 10:00 PM","Thu":"11:00 AM - 10:00 PM","Fri":"11:00 AM - 10:00 PM","Sat":"11:00 AM - 10:00 PM"},"ScheduledCloseDates":null,"IsRestaurantOpen":false,"IsSamedayButRestNotOpen":false,"IsRestShutdownForNextDay":false,"IsTechnicalShutDown":false,"MinCarryoutOrderingLimit":0,"MinDeliveryOrderingLimit":5,"MaxCarryOutOrderingLimit":500.00,"MaxDeliveryOrderingLimit":500.00,"ImageName":null,"RestaurantNumber":0,"IsCarryoutSupported":true,"IsDeliverySupported":true,"IsDineInSupported":false,"RestaurantServices":null,"HolidayStartEndDateTimes":[],"POSItemId":"212","StoreLocatorId":null,"RestaurantCity":null,"FranchiseName":"Soul Restaurants Canada Inc","IsOnlinePaymentSupported":false,"IsDealConversionSupported":false,"IsSuggestiveUpsellSupported":false,"IsDealSupported":false,"PaymentType":7,"DeliveryCharge":5.00,"FreeDeliveryOrderAmount":0.00,"DefaultDeliveryCharge":5.00,"IsFreeDeliveryActive":false,"DeliveryAmount":0,"RestaurantOrderModeOpenCloseTimes":[{"RestaurantId":"1406","OrderMode":"Carryout","DayOfWeek":"Sunday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1406","OrderMode":"Delivery","DayOfWeek":"Sunday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1406","OrderMode":"Carryout","DayOfWeek":"Monday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1406","OrderMode":"Delivery","DayOfWeek":"Monday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1406","OrderMode":"Carryout","DayOfWeek":"Tuesday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1406","OrderMode":"Delivery","DayOfWeek":"Tuesday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1406","OrderMode":"Carryout","DayOfWeek":"Wednesday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1406","OrderMode":"Delivery","DayOfWeek":"Wednesday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1406","OrderMode":"Carryout","DayOfWeek":"Thursday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1406","OrderMode":"Delivery","DayOfWeek":"Thursday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1406","OrderMode":"Carryout","DayOfWeek":"Friday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1406","OrderMode":"Delivery","DayOfWeek":"Friday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1406","OrderMode":"Carryout","DayOfWeek":"Saturday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1406","OrderMode":"Delivery","DayOfWeek":"Saturday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"}],"IsRestaurantOpenPreviousDay":false,"PaymentMerchantId":"kfc","PaymentAccountId":"1208","PaymentSubAccountId":"1208","PaymentSecret":"Wxue63P5jA","StoreLimitationMsg":"","Is3dSecure":false,"DeliveryPartnerStoreId":"","ProductExclusionList":[],"EmergencyShutdowns":null,"Tips":null,"TaxDetails":[{"TaxCategory":6,"CategoryDescription":"TVH","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":2,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00},{"TaxCategory":8,"CategoryDescription":"TVH sur la livraison","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":2,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00},{"TaxCategory":1,"CategoryDescription":"TPS","TaxAmount":0,"TaxPercentage":5.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":1,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":4.01},{"TaxCategory":6,"CategoryDescription":"TVH","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":1,"SalesCategory":1,"TaxSlabStartValue":4.01,"TaxSlabEndValue":500.00}],"BottleDepositDetails":[{"BottleSizeId":0,"DepositAmount":0.00,"Size":"591ml"},{"BottleSizeId":0,"DepositAmount":0.00,"Size":"2L"},{"BottleSizeId":0,"DepositAmount":0.00,"Size":"355ml"}],"FutureOrderDayLimit":2,"RestClosedFirstOrderTimeDifferenceCarryout":"0","RestClosedLastOrderTimeDifferenceCarryout":"15","RestClosedFirstOrderTimeDifferenceDelivery":"45","RestClosedLastOrderTimeDifferenceDelivery":"0","ShowCalorie":true,"PromiseTime":null,"OrderMode":0,"TenderWithService":[{"OrderMode":1,"Tendertype":3},{"OrderMode":1,"Tendertype":15},{"OrderMode":1,"Tendertype":14},{"OrderMode":2,"Tendertype":3},{"OrderMode":2,"Tendertype":15},{"OrderMode":2,"Tendertype":14}],"PrefferedLanguage":"en-ca","IPAddress":"10.248.114.6:5556","SendtoDeliveryPartnetForCarryoutOrder":false,"SendtoDeliveryPartnetForDeliveryOrder":false,"RestaurantId":"1406","Name":"1406","Description":"","Address":{"Street":"2919 Bank St  ","City":"Ottawa","ZipCode":"K1T 1N4","Country":"","Latitude":45.3484,"Longitude":-75.6272,"Distance":9.80,"HouseNumber":"","District":"","SubDistrict":"","Comments":""},"Phone":"613-521-6131","Email":null,"OnlineOrderingEnabled":true,"IsFranchise":false,"CarryoutQuoteTime":"15","DeliveryQuoteTime":"40","QuadrantId":null,"QuadrantName":null,"OrderAmount":0,"DisplayName":null,"DetailUrlPart1":null,"DetailUrlPart2":null,"Latitude":0,"Longitude":0,"IsOpenNow":false,"POSID":null,"Distance":"9.80","DayHours":[],"EmergencyDownModes":[],"TechnicalDownModes":[],"ServiceModes":[]},{"RestaurantGroupId":null,"OpenTime":"/Date(-62135596800000)/","CloseTime":"/Date(-62135596800000)/","PrevOpenTime":"/Date(-62135596800000)/","PrevCloseTime":"/Date(-62135596800000)/","CurbsideService":false,"TimeZone":"Eastern Standard Time","OpenDate":"/Date(-62135596800000)/","CloseDate":"/Date(-62135596800000)/","OpenCloseTimes":{"Tuesday":["11:00 AM - 10:00 PM"]},"RestaurantStatus":null,"FranchiseRedirectionUrl":null,"RestaurantRedirectionUrl":null,"CommunicationMethod":0,"PrimaryAddress":"1420-Nepean-ON@soulfoodsgroup.com","SecondaryAddress":null,"TaxSourceType":0,"POSType":1,"AlohaStoreKey":0,"RestaurantTimings":{"Sun":"11:00 AM - 10:00 PM","Mon":"11:00 AM - 10:00 PM","Tue":"11:00 AM - 10:00 PM","Wed":"11:00 AM - 10:00 PM","Thu":"11:00 AM - 10:00 PM","Fri":"11:00 AM - 10:00 PM","Sat":"11:00 AM - 10:00 PM"},"ScheduledCloseDates":null,"IsRestaurantOpen":false,"IsSamedayButRestNotOpen":false,"IsRestShutdownForNextDay":false,"IsTechnicalShutDown":false,"MinCarryoutOrderingLimit":0,"MinDeliveryOrderingLimit":5,"MaxCarryOutOrderingLimit":500.00,"MaxDeliveryOrderingLimit":500.00,"ImageName":null,"RestaurantNumber":0,"IsCarryoutSupported":true,"IsDeliverySupported":true,"IsDineInSupported":false,"RestaurantServices":null,"HolidayStartEndDateTimes":[],"POSItemId":"80","StoreLocatorId":null,"RestaurantCity":null,"FranchiseName":"Sayan Restaurants Canada Inc.","IsOnlinePaymentSupported":false,"IsDealConversionSupported":false,"IsSuggestiveUpsellSupported":false,"IsDealSupported":false,"PaymentType":7,"DeliveryCharge":5.00,"FreeDeliveryOrderAmount":0.00,"DefaultDeliveryCharge":5.00,"IsFreeDeliveryActive":false,"DeliveryAmount":0,"RestaurantOrderModeOpenCloseTimes":[{"RestaurantId":"1420","OrderMode":"Carryout","DayOfWeek":"Sunday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1420","OrderMode":"Delivery","DayOfWeek":"Sunday","OpenTime":"10:30 AM","CloseTime":"09:30 PM"},{"RestaurantId":"1420","OrderMode":"Carryout","DayOfWeek":"Monday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1420","OrderMode":"Delivery","DayOfWeek":"Monday","OpenTime":"10:30 AM","CloseTime":"09:30 PM"},{"RestaurantId":"1420","OrderMode":"Carryout","DayOfWeek":"Tuesday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1420","OrderMode":"Delivery","DayOfWeek":"Tuesday","OpenTime":"10:30 AM","CloseTime":"09:30 PM"},{"RestaurantId":"1420","OrderMode":"Carryout","DayOfWeek":"Wednesday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1420","OrderMode":"Delivery","DayOfWeek":"Wednesday","OpenTime":"10:30 AM","CloseTime":"09:30 PM"},{"RestaurantId":"1420","OrderMode":"Carryout","DayOfWeek":"Thursday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1420","OrderMode":"Delivery","DayOfWeek":"Thursday","OpenTime":"10:30 AM","CloseTime":"09:30 PM"},{"RestaurantId":"1420","OrderMode":"Carryout","DayOfWeek":"Friday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1420","OrderMode":"Delivery","DayOfWeek":"Friday","OpenTime":"10:30 AM","CloseTime":"09:30 PM"},{"RestaurantId":"1420","OrderMode":"Carryout","DayOfWeek":"Saturday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1420","OrderMode":"Delivery","DayOfWeek":"Saturday","OpenTime":"10:30 AM","CloseTime":"09:30 PM"}],"IsRestaurantOpenPreviousDay":false,"PaymentMerchantId":"kfc","PaymentAccountId":"1208","PaymentSubAccountId":"1208","PaymentSecret":"Wxue63P5jA","StoreLimitationMsg":"","Is3dSecure":false,"DeliveryPartnerStoreId":"","ProductExclusionList":[],"EmergencyShutdowns":null,"Tips":null,"TaxDetails":[{"TaxCategory":6,"CategoryDescription":"TVH","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":2,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00},{"TaxCategory":1,"CategoryDescription":"TPS","TaxAmount":0,"TaxPercentage":5.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":1,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":4.01},{"TaxCategory":6,"CategoryDescription":"TVH","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":1,"SalesCategory":1,"TaxSlabStartValue":4.01,"TaxSlabEndValue":500.00},{"TaxCategory":8,"CategoryDescription":"TVH sur la livraison","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":2,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00}],"BottleDepositDetails":[{"BottleSizeId":0,"DepositAmount":0.00,"Size":"591ml"},{"BottleSizeId":0,"DepositAmount":0.00,"Size":"2L"},{"BottleSizeId":0,"DepositAmount":0.00,"Size":"355ml"}],"FutureOrderDayLimit":2,"RestClosedFirstOrderTimeDifferenceCarryout":"0","RestClosedLastOrderTimeDifferenceCarryout":"15","RestClosedFirstOrderTimeDifferenceDelivery":"45","RestClosedLastOrderTimeDifferenceDelivery":"0","ShowCalorie":true,"PromiseTime":null,"OrderMode":0,"TenderWithService":[{"OrderMode":1,"Tendertype":3},{"OrderMode":1,"Tendertype":15},{"OrderMode":1,"Tendertype":14},{"OrderMode":2,"Tendertype":3},{"OrderMode":2,"Tendertype":15},{"OrderMode":2,"Tendertype":14}],"PrefferedLanguage":"en-ca","IPAddress":"10.248.114.20:5556","SendtoDeliveryPartnetForCarryoutOrder":false,"SendtoDeliveryPartnetForDeliveryOrder":false,"RestaurantId":"1420","Name":"1420","Description":"","Address":{"Street":"2015 Robertson Rd  ","City":"Nepean","ZipCode":"K2H 5Y7","Country":"","Latitude":45.3246,"Longitude":-75.8278,"Distance":14.86,"HouseNumber":"","District":"","SubDistrict":"","Comments":""},"Phone":"(613) 714-4481","Email":null,"OnlineOrderingEnabled":true,"IsFranchise":false,"CarryoutQuoteTime":"15","DeliveryQuoteTime":"40","QuadrantId":null,"QuadrantName":null,"OrderAmount":0,"DisplayName":null,"DetailUrlPart1":null,"DetailUrlPart2":null,"Latitude":0,"Longitude":0,"IsOpenNow":false,"POSID":null,"Distance":"14.86","DayHours":[],"EmergencyDownModes":[],"TechnicalDownModes":[],"ServiceModes":[]},{"RestaurantGroupId":null,"OpenTime":"/Date(-62135596800000)/","CloseTime":"/Date(-62135596800000)/","PrevOpenTime":"/Date(-62135596800000)/","PrevCloseTime":"/Date(-62135596800000)/","CurbsideService":false,"TimeZone":"Eastern Standard Time","OpenDate":"/Date(-62135596800000)/","CloseDate":"/Date(-62135596800000)/","OpenCloseTimes":{"Tuesday":["11:00 AM - 10:00 PM"]},"RestaurantStatus":null,"FranchiseRedirectionUrl":null,"RestaurantRedirectionUrl":null,"CommunicationMethod":0,"PrimaryAddress":"1409-Glouchester-ON@soulfoodsgroup.com","SecondaryAddress":null,"TaxSourceType":0,"POSType":1,"AlohaStoreKey":0,"RestaurantTimings":{"Sun":"11:00 AM - 10:00 PM","Mon":"11:00 AM - 10:00 PM","Tue":"11:00 AM - 10:00 PM","Wed":"11:00 AM - 10:00 PM","Thu":"11:00 AM - 10:00 PM","Fri":"11:00 AM - 10:00 PM","Sat":"11:00 AM - 10:00 PM"},"ScheduledCloseDates":null,"IsRestaurantOpen":false,"IsSamedayButRestNotOpen":false,"IsRestShutdownForNextDay":false,"IsTechnicalShutDown":false,"MinCarryoutOrderingLimit":0,"MinDeliveryOrderingLimit":5,"MaxCarryOutOrderingLimit":500.00,"MaxDeliveryOrderingLimit":500.00,"ImageName":null,"RestaurantNumber":0,"IsCarryoutSupported":true,"IsDeliverySupported":true,"IsDineInSupported":false,"RestaurantServices":null,"HolidayStartEndDateTimes":[],"POSItemId":"198","StoreLocatorId":null,"RestaurantCity":null,"FranchiseName":"Soul Restaurants Canada Inc","IsOnlinePaymentSupported":false,"IsDealConversionSupported":false,"IsSuggestiveUpsellSupported":false,"IsDealSupported":false,"PaymentType":7,"DeliveryCharge":5.00,"FreeDeliveryOrderAmount":0.00,"DefaultDeliveryCharge":5.00,"IsFreeDeliveryActive":false,"DeliveryAmount":0,"RestaurantOrderModeOpenCloseTimes":[{"RestaurantId":"1409","OrderMode":"Carryout","DayOfWeek":"Sunday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1409","OrderMode":"Delivery","DayOfWeek":"Sunday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1409","OrderMode":"Carryout","DayOfWeek":"Monday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1409","OrderMode":"Delivery","DayOfWeek":"Monday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1409","OrderMode":"Carryout","DayOfWeek":"Tuesday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1409","OrderMode":"Delivery","DayOfWeek":"Tuesday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1409","OrderMode":"Carryout","DayOfWeek":"Wednesday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1409","OrderMode":"Delivery","DayOfWeek":"Wednesday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1409","OrderMode":"Carryout","DayOfWeek":"Thursday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1409","OrderMode":"Delivery","DayOfWeek":"Thursday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1409","OrderMode":"Carryout","DayOfWeek":"Friday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1409","OrderMode":"Delivery","DayOfWeek":"Friday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1409","OrderMode":"Carryout","DayOfWeek":"Saturday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1409","OrderMode":"Delivery","DayOfWeek":"Saturday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"}],"IsRestaurantOpenPreviousDay":false,"PaymentMerchantId":"kfc","PaymentAccountId":"1208","PaymentSubAccountId":"1208","PaymentSecret":"Wxue63P5jA","StoreLimitationMsg":"","Is3dSecure":false,"DeliveryPartnerStoreId":"","ProductExclusionList":[],"EmergencyShutdowns":null,"Tips":null,"TaxDetails":[{"TaxCategory":6,"CategoryDescription":"TVH","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":2,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00},{"TaxCategory":8,"CategoryDescription":"TVH sur la livraison","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":2,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00},{"TaxCategory":1,"CategoryDescription":"TPS","TaxAmount":0,"TaxPercentage":5.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":1,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":4.01},{"TaxCategory":6,"CategoryDescription":"TVH","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":1,"SalesCategory":1,"TaxSlabStartValue":4.01,"TaxSlabEndValue":500.00}],"BottleDepositDetails":[{"BottleSizeId":0,"DepositAmount":0.00,"Size":"591ml"},{"BottleSizeId":0,"DepositAmount":0.00,"Size":"2L"},{"BottleSizeId":0,"DepositAmount":0.00,"Size":"355ml"}],"FutureOrderDayLimit":2,"RestClosedFirstOrderTimeDifferenceCarryout":"0","RestClosedLastOrderTimeDifferenceCarryout":"15","RestClosedFirstOrderTimeDifferenceDelivery":"45","RestClosedLastOrderTimeDifferenceDelivery":"0","ShowCalorie":true,"PromiseTime":null,"OrderMode":0,"TenderWithService":[{"OrderMode":1,"Tendertype":3},{"OrderMode":1,"Tendertype":15},{"OrderMode":1,"Tendertype":14},{"OrderMode":2,"Tendertype":3},{"OrderMode":2,"Tendertype":15},{"OrderMode":2,"Tendertype":14}],"PrefferedLanguage":"en-ca","IPAddress":"10.248.114.9:5556","SendtoDeliveryPartnetForCarryoutOrder":false,"SendtoDeliveryPartnetForDeliveryOrder":false,"RestaurantId":"1409","Name":"1409","Description":"","Address":{"Street":"4027 Innes Rd  ","City":"Gloucester","ZipCode":"K1C 1T1","Country":"","Latitude":45.4558366,"Longitude":-75.5076534,"Distance":15.31,"HouseNumber":"","District":"","SubDistrict":"","Comments":""},"Phone":"(613) 830-9586","Email":null,"OnlineOrderingEnabled":true,"IsFranchise":false,"CarryoutQuoteTime":"15","DeliveryQuoteTime":"40","QuadrantId":null,"QuadrantName":null,"OrderAmount":0,"DisplayName":null,"DetailUrlPart1":null,"DetailUrlPart2":null,"Latitude":0,"Longitude":0,"IsOpenNow":false,"POSID":null,"Distance":"15.31","DayHours":[],"EmergencyDownModes":[],"TechnicalDownModes":[],"ServiceModes":[]},{"RestaurantGroupId":null,"OpenTime":"/Date(-62135596800000)/","CloseTime":"/Date(-62135596800000)/","PrevOpenTime":"/Date(-62135596800000)/","PrevCloseTime":"/Date(-62135596800000)/","CurbsideService":false,"TimeZone":"Eastern Standard Time","OpenDate":"/Date(-62135596800000)/","CloseDate":"/Date(-62135596800000)/","OpenCloseTimes":{"Tuesday":["11:00 AM - 10:00 PM"]},"RestaurantStatus":null,"FranchiseRedirectionUrl":null,"RestaurantRedirectionUrl":null,"CommunicationMethod":0,"PrimaryAddress":"1408-Nepean-ON@soulfoodsgroup.com","SecondaryAddress":null,"TaxSourceType":0,"POSType":1,"AlohaStoreKey":0,"RestaurantTimings":{"Sun":"11:00 AM - 10:00 PM","Mon":"11:00 AM - 10:00 PM","Tue":"11:00 AM - 10:00 PM","Wed":"11:00 AM - 10:00 PM","Thu":"11:00 AM - 10:00 PM","Fri":"11:00 AM - 10:00 PM","Sat":"11:00 AM - 10:00 PM"},"ScheduledCloseDates":null,"IsRestaurantOpen":false,"IsSamedayButRestNotOpen":false,"IsRestShutdownForNextDay":false,"IsTechnicalShutDown":false,"MinCarryoutOrderingLimit":0,"MinDeliveryOrderingLimit":5,"MaxCarryOutOrderingLimit":500.00,"MaxDeliveryOrderingLimit":500.00,"ImageName":null,"RestaurantNumber":0,"IsCarryoutSupported":true,"IsDeliverySupported":true,"IsDineInSupported":false,"RestaurantServices":null,"HolidayStartEndDateTimes":[],"POSItemId":"180","StoreLocatorId":null,"RestaurantCity":null,"FranchiseName":"Soul Restaurants Canada Inc","IsOnlinePaymentSupported":false,"IsDealConversionSupported":false,"IsSuggestiveUpsellSupported":false,"IsDealSupported":false,"PaymentType":7,"DeliveryCharge":5.00,"FreeDeliveryOrderAmount":0.00,"DefaultDeliveryCharge":5.00,"IsFreeDeliveryActive":false,"DeliveryAmount":0,"RestaurantOrderModeOpenCloseTimes":[{"RestaurantId":"1408","OrderMode":"Carryout","DayOfWeek":"Sunday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1408","OrderMode":"Delivery","DayOfWeek":"Sunday","OpenTime":"11:00 AM","CloseTime":"09:30 PM"},{"RestaurantId":"1408","OrderMode":"Carryout","DayOfWeek":"Monday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1408","OrderMode":"Delivery","DayOfWeek":"Monday","OpenTime":"11:00 AM","CloseTime":"09:30 PM"},{"RestaurantId":"1408","OrderMode":"Carryout","DayOfWeek":"Tuesday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1408","OrderMode":"Delivery","DayOfWeek":"Tuesday","OpenTime":"11:00 AM","CloseTime":"09:30 PM"},{"RestaurantId":"1408","OrderMode":"Carryout","DayOfWeek":"Wednesday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1408","OrderMode":"Delivery","DayOfWeek":"Wednesday","OpenTime":"11:00 AM","CloseTime":"09:30 PM"},{"RestaurantId":"1408","OrderMode":"Carryout","DayOfWeek":"Thursday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1408","OrderMode":"Delivery","DayOfWeek":"Thursday","OpenTime":"11:00 AM","CloseTime":"09:30 PM"},{"RestaurantId":"1408","OrderMode":"Carryout","DayOfWeek":"Friday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1408","OrderMode":"Delivery","DayOfWeek":"Friday","OpenTime":"11:00 AM","CloseTime":"09:30 PM"},{"RestaurantId":"1408","OrderMode":"Carryout","DayOfWeek":"Saturday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1408","OrderMode":"Delivery","DayOfWeek":"Saturday","OpenTime":"11:00 AM","CloseTime":"09:30 PM"}],"IsRestaurantOpenPreviousDay":false,"PaymentMerchantId":"kfc","PaymentAccountId":"1208","PaymentSubAccountId":"1208","PaymentSecret":"Wxue63P5jA","StoreLimitationMsg":"","Is3dSecure":false,"DeliveryPartnerStoreId":"","ProductExclusionList":[],"EmergencyShutdowns":null,"Tips":null,"TaxDetails":[{"TaxCategory":6,"CategoryDescription":"TVH","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":2,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00},{"TaxCategory":8,"CategoryDescription":"TVH sur la livraison","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":2,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00},{"TaxCategory":1,"CategoryDescription":"TPS","TaxAmount":0,"TaxPercentage":5.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":1,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":4.01},{"TaxCategory":6,"CategoryDescription":"TVH","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":1,"SalesCategory":1,"TaxSlabStartValue":4.01,"TaxSlabEndValue":500.00}],"BottleDepositDetails":[{"BottleSizeId":0,"DepositAmount":0.00,"Size":"591ml"},{"BottleSizeId":0,"DepositAmount":0.00,"Size":"2L"},{"BottleSizeId":0,"DepositAmount":0.00,"Size":"355ml"}],"FutureOrderDayLimit":2,"RestClosedFirstOrderTimeDifferenceCarryout":"0","RestClosedLastOrderTimeDifferenceCarryout":"15","RestClosedFirstOrderTimeDifferenceDelivery":"45","RestClosedLastOrderTimeDifferenceDelivery":"0","ShowCalorie":true,"PromiseTime":null,"OrderMode":0,"TenderWithService":[{"OrderMode":1,"Tendertype":3},{"OrderMode":1,"Tendertype":15},{"OrderMode":1,"Tendertype":14},{"OrderMode":2,"Tendertype":3},{"OrderMode":2,"Tendertype":15},{"OrderMode":2,"Tendertype":14}],"PrefferedLanguage":"en-ca","IPAddress":"10.248.114.8:5556","SendtoDeliveryPartnetForCarryoutOrder":false,"SendtoDeliveryPartnetForDeliveryOrder":false,"RestaurantId":"1408","Name":"1408","Description":"","Address":{"Street":"3780 Fallowfield Road, Unit 724  ","City":"Nepean","ZipCode":"K2J 1A1","Country":"","Latitude":45.2870573,"Longitude":-75.7600472,"Distance":15.74,"HouseNumber":"","District":"","SubDistrict":"","Comments":""},"Phone":"(613) 825-2337","Email":null,"OnlineOrderingEnabled":true,"IsFranchise":false,"CarryoutQuoteTime":"15","DeliveryQuoteTime":"40","QuadrantId":null,"QuadrantName":null,"OrderAmount":0,"DisplayName":null,"DetailUrlPart1":null,"DetailUrlPart2":null,"Latitude":0,"Longitude":0,"IsOpenNow":false,"POSID":null,"Distance":"15.74","DayHours":[],"EmergencyDownModes":[],"TechnicalDownModes":[],"ServiceModes":[]},{"RestaurantGroupId":null,"OpenTime":"/Date(-62135596800000)/","CloseTime":"/Date(-62135596800000)/","PrevOpenTime":"/Date(-62135596800000)/","PrevCloseTime":"/Date(-62135596800000)/","CurbsideService":false,"TimeZone":"Eastern Standard Time","OpenDate":"/Date(-62135596800000)/","CloseDate":"/Date(-62135596800000)/","OpenCloseTimes":{"Tuesday":["11:00 AM - 10:00 PM"]},"RestaurantStatus":null,"FranchiseRedirectionUrl":null,"RestaurantRedirectionUrl":null,"CommunicationMethod":0,"PrimaryAddress":"1422-Ottawa-ON@soulfoodsgroup.com","SecondaryAddress":null,"TaxSourceType":0,"POSType":1,"AlohaStoreKey":0,"RestaurantTimings":{"Sun":"11:00 AM - 10:00 PM","Mon":"11:00 AM - 10:00 PM","Tue":"11:00 AM - 10:00 PM","Wed":"11:00 AM - 10:00 PM","Thu":"11:00 AM - 10:00 PM","Fri":"11:00 AM - 10:00 PM","Sat":"11:00 AM - 10:00 PM"},"ScheduledCloseDates":null,"IsRestaurantOpen":false,"IsSamedayButRestNotOpen":false,"IsRestShutdownForNextDay":false,"IsTechnicalShutDown":false,"MinCarryoutOrderingLimit":0,"MinDeliveryOrderingLimit":5,"MaxCarryOutOrderingLimit":500.00,"MaxDeliveryOrderingLimit":500.00,"ImageName":null,"RestaurantNumber":0,"IsCarryoutSupported":true,"IsDeliverySupported":true,"IsDineInSupported":false,"RestaurantServices":null,"HolidayStartEndDateTimes":[],"POSItemId":"176","StoreLocatorId":null,"RestaurantCity":null,"FranchiseName":"Soul Restaurants Canada Inc","IsOnlinePaymentSupported":false,"IsDealConversionSupported":false,"IsSuggestiveUpsellSupported":false,"IsDealSupported":false,"PaymentType":7,"DeliveryCharge":5.00,"FreeDeliveryOrderAmount":0.00,"DefaultDeliveryCharge":5.00,"IsFreeDeliveryActive":false,"DeliveryAmount":0,"RestaurantOrderModeOpenCloseTimes":[{"RestaurantId":"1422","OrderMode":"Carryout","DayOfWeek":"Sunday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1422","OrderMode":"Delivery","DayOfWeek":"Sunday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1422","OrderMode":"Carryout","DayOfWeek":"Monday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1422","OrderMode":"Delivery","DayOfWeek":"Monday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1422","OrderMode":"Carryout","DayOfWeek":"Tuesday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1422","OrderMode":"Delivery","DayOfWeek":"Tuesday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1422","OrderMode":"Carryout","DayOfWeek":"Wednesday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1422","OrderMode":"Delivery","DayOfWeek":"Wednesday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1422","OrderMode":"Carryout","DayOfWeek":"Thursday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1422","OrderMode":"Delivery","DayOfWeek":"Thursday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1422","OrderMode":"Carryout","DayOfWeek":"Friday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1422","OrderMode":"Delivery","DayOfWeek":"Friday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1422","OrderMode":"Carryout","DayOfWeek":"Saturday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1422","OrderMode":"Delivery","DayOfWeek":"Saturday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"}],"IsRestaurantOpenPreviousDay":false,"PaymentMerchantId":"kfc","PaymentAccountId":"1208","PaymentSubAccountId":"1208","PaymentSecret":"Wxue63P5jA","StoreLimitationMsg":"","Is3dSecure":false,"DeliveryPartnerStoreId":"","ProductExclusionList":[],"EmergencyShutdowns":null,"Tips":null,"TaxDetails":[{"TaxCategory":6,"CategoryDescription":"TVH","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":2,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00},{"TaxCategory":1,"CategoryDescription":"TPS","TaxAmount":0,"TaxPercentage":5.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":1,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":4.01},{"TaxCategory":6,"CategoryDescription":"TVH","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":1,"SalesCategory":1,"TaxSlabStartValue":4.01,"TaxSlabEndValue":500.00},{"TaxCategory":8,"CategoryDescription":"TVH sur la livraison","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":2,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00}],"BottleDepositDetails":[{"BottleSizeId":0,"DepositAmount":0.00,"Size":"591ml"},{"BottleSizeId":0,"DepositAmount":0.00,"Size":"2L"},{"BottleSizeId":0,"DepositAmount":0.00,"Size":"355ml"}],"FutureOrderDayLimit":2,"RestClosedFirstOrderTimeDifferenceCarryout":"0","RestClosedLastOrderTimeDifferenceCarryout":"15","RestClosedFirstOrderTimeDifferenceDelivery":"45","RestClosedLastOrderTimeDifferenceDelivery":"0","ShowCalorie":true,"PromiseTime":null,"OrderMode":0,"TenderWithService":[{"OrderMode":1,"Tendertype":3},{"OrderMode":1,"Tendertype":15},{"OrderMode":1,"Tendertype":14},{"OrderMode":2,"Tendertype":3},{"OrderMode":2,"Tendertype":15},{"OrderMode":2,"Tendertype":14}],"PrefferedLanguage":"en-ca","IPAddress":"10.248.114.22:5556","SendtoDeliveryPartnetForCarryoutOrder":false,"SendtoDeliveryPartnetForDeliveryOrder":false,"RestaurantId":"1422","Name":"1422","Description":"","Address":{"Street":"1375 Trim Road  ","City":"Ottawa","ZipCode":"K4A 0G4","Country":"","Latitude":45.4897,"Longitude":-75.4775,"Distance":18.78,"HouseNumber":"","District":"","SubDistrict":"","Comments":""},"Phone":"(613) 714-4485","Email":null,"OnlineOrderingEnabled":true,"IsFranchise":false,"CarryoutQuoteTime":"15","DeliveryQuoteTime":"40","QuadrantId":null,"QuadrantName":null,"OrderAmount":0,"DisplayName":null,"DetailUrlPart1":null,"DetailUrlPart2":null,"Latitude":0,"Longitude":0,"IsOpenNow":false,"POSID":null,"Distance":"18.78","DayHours":[],"EmergencyDownModes":[],"TechnicalDownModes":[],"ServiceModes":[]},{"RestaurantGroupId":null,"OpenTime":"/Date(-62135596800000)/","CloseTime":"/Date(-62135596800000)/","PrevOpenTime":"/Date(-62135596800000)/","PrevCloseTime":"/Date(-62135596800000)/","CurbsideService":false,"TimeZone":"Eastern Standard Time","OpenDate":"/Date(-62135596800000)/","CloseDate":"/Date(-62135596800000)/","OpenCloseTimes":{"Tuesday":["11:00 AM - 10:00 PM"]},"RestaurantStatus":null,"FranchiseRedirectionUrl":null,"RestaurantRedirectionUrl":null,"CommunicationMethod":0,"PrimaryAddress":"1416-Kanata-ON@soulfoodsgroup.com","SecondaryAddress":null,"TaxSourceType":0,"POSType":1,"AlohaStoreKey":0,"RestaurantTimings":{"Sun":"11:00 AM - 10:00 PM","Mon":"11:00 AM - 10:00 PM","Tue":"11:00 AM - 10:00 PM","Wed":"11:00 AM - 10:00 PM","Thu":"11:00 AM - 10:00 PM","Fri":"11:00 AM - 10:00 PM","Sat":"11:00 AM - 10:00 PM"},"ScheduledCloseDates":null,"IsRestaurantOpen":false,"IsSamedayButRestNotOpen":false,"IsRestShutdownForNextDay":false,"IsTechnicalShutDown":false,"MinCarryoutOrderingLimit":0,"MinDeliveryOrderingLimit":5,"MaxCarryOutOrderingLimit":500.00,"MaxDeliveryOrderingLimit":500.00,"ImageName":null,"RestaurantNumber":0,"IsCarryoutSupported":true,"IsDeliverySupported":true,"IsDineInSupported":false,"RestaurantServices":null,"HolidayStartEndDateTimes":[],"POSItemId":"208","StoreLocatorId":null,"RestaurantCity":null,"FranchiseName":"Soul Restaurants Canada Inc","IsOnlinePaymentSupported":false,"IsDealConversionSupported":false,"IsSuggestiveUpsellSupported":false,"IsDealSupported":false,"PaymentType":7,"DeliveryCharge":5.00,"FreeDeliveryOrderAmount":0.00,"DefaultDeliveryCharge":5.00,"IsFreeDeliveryActive":false,"DeliveryAmount":0,"RestaurantOrderModeOpenCloseTimes":[{"RestaurantId":"1416","OrderMode":"Carryout","DayOfWeek":"Sunday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1416","OrderMode":"Delivery","DayOfWeek":"Sunday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1416","OrderMode":"Carryout","DayOfWeek":"Monday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1416","OrderMode":"Delivery","DayOfWeek":"Monday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1416","OrderMode":"Carryout","DayOfWeek":"Tuesday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1416","OrderMode":"Delivery","DayOfWeek":"Tuesday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1416","OrderMode":"Carryout","DayOfWeek":"Wednesday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1416","OrderMode":"Delivery","DayOfWeek":"Wednesday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1416","OrderMode":"Carryout","DayOfWeek":"Thursday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1416","OrderMode":"Delivery","DayOfWeek":"Thursday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1416","OrderMode":"Carryout","DayOfWeek":"Friday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1416","OrderMode":"Delivery","DayOfWeek":"Friday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1416","OrderMode":"Carryout","DayOfWeek":"Saturday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"},{"RestaurantId":"1416","OrderMode":"Delivery","DayOfWeek":"Saturday","OpenTime":"11:00 AM","CloseTime":"10:00 PM"}],"IsRestaurantOpenPreviousDay":false,"PaymentMerchantId":"kfc","PaymentAccountId":"1208","PaymentSubAccountId":"1208","PaymentSecret":"Wxue63P5jA","StoreLimitationMsg":"","Is3dSecure":false,"DeliveryPartnerStoreId":"","ProductExclusionList":[],"EmergencyShutdowns":null,"Tips":null,"TaxDetails":[{"TaxCategory":6,"CategoryDescription":"TVH","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":2,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00},{"TaxCategory":8,"CategoryDescription":"TVH sur la livraison","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":2,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":500.00},{"TaxCategory":1,"CategoryDescription":"TPS","TaxAmount":0,"TaxPercentage":5.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":1,"SalesCategory":1,"TaxSlabStartValue":0.00,"TaxSlabEndValue":4.01},{"TaxCategory":6,"CategoryDescription":"TVH","TaxAmount":0,"TaxPercentage":13.000,"TaxResponseStatus":0,"TaxResponseStatusMessage":null,"IsTaxInclusiveInPrice":false,"OrderMode":1,"SalesCategory":1,"TaxSlabStartValue":4.01,"TaxSlabEndValue":500.00}],"BottleDepositDetails":[{"BottleSizeId":0,"DepositAmount":0.00,"Size":"591ml"},{"BottleSizeId":0,"DepositAmount":0.00,"Size":"2L"},{"BottleSizeId":0,"DepositAmount":0.00,"Size":"355ml"}],"FutureOrderDayLimit":2,"RestClosedFirstOrderTimeDifferenceCarryout":"0","RestClosedLastOrderTimeDifferenceCarryout":"15","RestClosedFirstOrderTimeDifferenceDelivery":"45","RestClosedLastOrderTimeDifferenceDelivery":"0","ShowCalorie":true,"PromiseTime":null,"OrderMode":0,"TenderWithService":[{"OrderMode":1,"Tendertype":3},{"OrderMode":1,"Tendertype":15},{"OrderMode":1,"Tendertype":14},{"OrderMode":2,"Tendertype":3},{"OrderMode":2,"Tendertype":15},{"OrderMode":2,"Tendertype":14}],"PrefferedLanguage":"en-ca","IPAddress":"10.248.114.16:5556","SendtoDeliveryPartnetForCarryoutOrder":false,"SendtoDeliveryPartnetForDeliveryOrder":false,"RestaurantId":"1416","Name":"1416","Description":"","Address":{"Street":"475 Hazeldean Rd  ","City":"Kanata","ZipCode":"K2L 1V1","Country":"","Latitude":45.2979992,"Longitude":-75.8915584,"Distance":20.50,"HouseNumber":"","District":"","SubDistrict":"","Comments":""},"Phone":"(613) 836-4011","Email":null,"OnlineOrderingEnabled":true,"IsFranchise":false,"CarryoutQuoteTime":"15","DeliveryQuoteTime":"40","QuadrantId":null,"QuadrantName":null,"OrderAmount":0,"DisplayName":null,"DetailUrlPart1":null,"DetailUrlPart2":null,"Latitude":0,"Longitude":0,"IsOpenNow":false,"POSID":null,"Distance":"20.50","DayHours":[],"EmergencyDownModes":[],"TechnicalDownModes":[],"ServiceModes":[]}]}}'
  
  kfc <- jsonlite::fromJSON(j)$DataObject$CarryoutRestaurants %>%
    jsonlite::flatten() %>%
    as_tibble() %>% 
    select(Phone, starts_with("Address")) %>%
    mutate(name = "KFC",
           address = sprintf("%s, %s, %s", Address.Street, Address.City, Address.ZipCode) %>%
             stringr::str_squish() %>%
             stringr::str_replace_all(" ,", ",")) %>%
    select(name,
           address,
           phone = Phone,
           lat = Address.Latitude,
           lon = Address.Longitude)
  
  
  write_csv(kfc, "data/restaurants/kfc.csv")
  
  return(kfc)
}

scrape_dominos <- function() {
  url <- "https://order.dominos.ca/store-locator-international/locate/store?regionCode=CA&latitude=45.4215296&longitude=-75.69719309999999"
  
  resp <- httr::GET(url, 
                    add_headers(.headers = c(
                      "accept" = "application/vnd.com.dominos.ecommerce.store-locator.response+json;version=1.2",
                      "accept-encoding" = "gzip, deflate, br",
                      "accept-language" = "en-CA,en-GB;q=0.9,en-US;q=0.8,en;q=0.7",
                      "cookie" = "_gid=GA1.2.1714415506.1635256830; check=true; _scid=9c859ce5-c074-49b0-8570-333b9da4a64b; _sctr=1|1635220800000; _ga_6EY126Y6XR=GS1.1.1635256831.1.0.1635256831.0; _ga=GA1.1.2027823926.1635256830; _uetsid=14d711a0366511ec8bd74d9606411369; _uetvid=14d717e0366511ecbb69e32930e4db5a; utag_main=v_id:017cbce70936001041985a808e2905072003306a00bd0$_sn:1$_ss:1$_pn:1%3Bexp-session$_st:1635258702615$ses_id:1635256830262%3Bexp-session",
                      "dpz-language" = "en",
                      "dpz-market" = "CANADA",
                      "market" = "CANADA"
                      
                    ))) %>%
    httr::content(type = "text/json", encoding = 'UTF-8')
  
  dominos_carryout <- jsonlite::fromJSON(resp)$Stores %>%
    as_tibble() %>%
    mutate(name = "Domino's Pizza")
  
  dominos <- dominos_carryout %>%
    select(name, address = AddressDescription, phone = Phone)
  
  write_csv(dominos, "data/restaurants/dominos.csv")
  
  return(dominos)
  
}

scrape_dairyqueen <- function() {
  url <- "https://prod-dairyqueen.dotcmscloud.com/api/vtl/locations?country=ca&lat=45.4215296&long=-75.69719309999999"
  
  resp <- httr::GET(url) %>%
    httr::content(type = "text/json", encoding = "UTF-8")
  
  dq <- resp %>%
    jsonlite::fromJSON() %>%
    pluck("locations") %>%
    as_tibble() %>%
    mutate(address = sprintf("%s, %s, %s, %s", address3, city, stateProvince, postalCode),
           name = "Dairy Queen") %>%
    select(name, address, latlong) %>%
    separate(col = latlong, into = c("lat", "lon"), sep = ",")
  
  write_csv(dq, "data/restaurants/dairy_queen.csv")
  
  return(dq)
}

scrape_aw <- function() {
  url <- "https://web.aw.ca/api/locations/"
  
  aw_json <- httr::GET(url) %>%
    httr::content(type = "text/json", encoding = "UTF-8")
  
  aw_data <- jsonlite::fromJSON(aw_json) %>%
    as_tibble()
  
  aw <- aw_data %>%
    mutate(address = if_else(address2 == "",
                             sprintf("%s, %s, %s, %s", address1, city_name, province_code, postal_code),
                             sprintf("%s, %s, %s, %s, %s", address1, address2, city_name, province_code, postal_code)),
           name = "A&W") %>%
    select(name, address, phone = phone_number, lat = latitude, lon = longitude)
  
  write_csv(aw, "data/restaurants/A&W.csv")
  
  return(aw)
}


scrape_thaiexpress <- function() {
  url <- "https://thaiexpress.ca/wp-content/plugins/superstorefinder-wp/ssf-wp-xml.php?wpml_lang=en&t=1635253295641"
  
  resp <- httr::GET(url) %>%
    httr::content(type = "text/xml", encoding = "UTF-8")
  
  thai <- resp %>%
    html_elements("store item") %>%
    purrr::map_dfr(function(x) {
      name <- "Thai Express"
      address <- html_elements(x, "address") %>% html_text()
      phone <- html_elements(x, "telephone") %>% html_text()
      lat <- html_elements(x, "latitude") %>% html_text()
      lon <- html_elements(x, "longitude") %>% html_text()
      
      tibble(name = name,
             address = address,
             phone = phone,
             lat = lat,
             lon = lon)
    })
  
  write_csv(thai, "data/restaurants/thai_express.csv")
  return(thai)
  
}


scrape_milanopizza <- function() {
  url <- "https://order.milanopizzeria.ca/index.php/search/all"
  
  mil_html <- rvest::read_html(url)
  
  restos <- mil_html %>%
    rvest::html_elements(".restaurant")
  
  milanos <- restos %>%
    purrr::map_dfr(function(x){
      name <- "Milano's Pizza"
      address <- rvest::html_element(x, "p") %>%
        html_text()
      phone <- stringr::str_extract(address, "\\(.*") %>%
        stringr::str_squish()
      address <- stringr::str_remove(address, "\\(.*") %>%
        stringr::str_squish()
      
      
      tibble(name = name,
             address = address,
             phone = phone) %>%
        filter(address != "",
               address != "Gatineau Area")
      
    })
  
  write_csv(milanos, "data/restaurants/milanos_pizza.csv")
  return(milanos)
  
}

scrape_bridgehead <- function() {
  url <- "https://www.bridgehead.ca/pages/coffeehouses"
  
  b_html <- rvest::read_html(url)
  
  b_data <- b_html %>%
    as.character() %>%
    stringr::str_extract(regex("(?<=var sites = \\[).*?(?=;)", dotall=TRUE))
  
  bridgehead <- b_data %>%
    stringr::str_split("\\n") %>%
    purrr::map_dfr(function(x) {
      lat = stringr::str_extract(x, "45\\.\\d*")
      lon = stringr::str_extract(x, "-75\\.\\d*")
      address = stringr::str_extract(x, '(?<=target=\\\"_blank\\\">).*?(?=<)')
      phone = stringr::str_extract(x, "(?<=Ph\\. ).*?(?= <)")
      
      tibble(name = "Bridgehead",
             lat = lat,
             lon = lon,
             address = address,
             phone = phone) %>%
        mutate(across(everything(), stringr::str_trim)) %>%
        drop_na(address)
      
    })
  
  write_csv(bridgehead, "data/restaurants/bridgehead.csv")
  
  return(bridgehead)
}


scrape_boosterjuice <- function() {
  url <- "https://www.boosterjuice.com/WebServices/Booster.asmx/StoreLocations"
  
  booster_json <- httr::GET(url) %>%
    httr::content(type = "text/json", encoding = "UTF-8")
  
  booster_data <- booster_json[[1]] %>%
    jsonlite::fromJSON() %>%
    as_tibble()
  
  booster <- booster_data %>%
    mutate(address = if_else(address2 == "", 
                             sprintf("%s, %s, %s, %s", address, city, province, postalCode),
                             sprintf("%s, %s, %s, %s, %s", address, address2, city, province, postalCode)) %>%
             stringr::str_squish(),
           name = "Booster Juice") %>%
    select(name, address, phoneNumber, lat = latitude, lon = longitude)
  
  write_csv(booster, "data/restaurants/booster_juice.csv")
  
  return(booster)
}

scrape_gabrielpizza <- function(){
  
  # IT GIVES JAVASCRIPT DEFINITION NOT JSON
  # so we need to do some super annoying parsing
  
  url <- "https://gabrielpizza.com/wp-admin/admin-ajax.php?action=locations_js&lang=en"
  resp <- httr::GET(url)
  
  gab_js <- httr::content(resp, type = "text/json", encoding = "UTF-8") 
  
  # convert javascript variable definnitions to json
  # (couldn't find a quick way to run js in R)
  gab_json <- gab_js %>%
    stringr::str_remove("var locations =") %>%
    stringr::str_remove(regex("var map.*", dotall=TRUE)) %>%
    #gsub(x = ., pattern = "'", replacement = '"') %>%
    stringr::str_replace_all("(?<!\\w)'(?=\\w)", '"') %>%        # ' not preceded by letter, yes followed by letter
    stringr::str_replace_all("(?<=\\w)'(?!\\w)", '"') %>% # preceded by letter, not followed by letter
    stringr::str_replace_all("title", '"title"') %>%
    stringr::str_replace_all("content :", '"content" :') %>%
    stringr::str_replace_all("lat", '"lat"') %>%
    stringr::str_replace_all("lng", '"lng"') %>%
    stringr::str_replace_all("addr", '"addr"') %>%
    stringr::str_replace_all("city", '"city"') %>%
    stringr::str_replace_all("prov", '"prov"')%>%
    stringr::str_replace_all("pcode", '"pcode"')%>%
    stringr::str_replace_all("notes", '"notes"')%>%
    stringr::str_replace_all("hours", '"hours"')%>%
    stringr::str_replace_all("icon :", '"icon" :')%>%
    stringr::str_replace_all("dinein", '"dinein"')%>%
    stringr::str_replace_all("delivery :", '"delivery" :') %>%
    stringr::str_replace_all(regex(",[:space:]*\\}", dotall=TRUE), "}")  %>%
    stringr::str_replace_all("true", '"true"') %>%
    stringr::str_replace_all("false", '"false"') %>%
    stringr::str_remove_all(";") 
  
  # parse the JSON
  gab_data <- jsonlite::fromJSON(gab_json)
  
  # extract the values from the list
  gabriel <- gab_data %>%
    purrr::map_dfr(function(x) {
      lat = x$lat
      lon = x$lng
      address = sprintf("%s, %s, %s, %s",
                        x$addr,
                        x$city,
                        x$prov,
                        x$pcode)
      dinein = x$dinein
      delivery = x$delivery
      
      tibble(name = "Gabriel's Pizza",
             address = address,
             lat = lat,
             lon = lon,
             dinein = dinein,
             delivery = delivery)
      
    })
  
  # save csv
  write_csv(gabriel, "data/restaurants/gabriels_pizza.csv")
  return(gabriel)
  
}

scrape_pizzapizza <- function(){
  cookie <- 'pp-mw-session=VTJGc2RHVmtYMStUbzAzeHMvRGZoclBtbGh2ZVFhU0RMWEpJemNiQ3Z6NFhjbitnL0lUa1NPcFBPSkVwU1dySFhCNmowTFNQRTI0azRCaEtIbDZzWUorWmIyUWs2THhZcGErbkZQSTkydWc9; AKA_A2=A; _cls_v=6c830111-2b38-43fd-8d7c-c54a6eca8a49; _cls_s=b2134b39-eea5-4139-9494-7020548342df:0; rbzid=73d2SN40vwnU3bjUM1OKeGYm/W7XVZxKP7lt9uYSMTCekFfrICr2owRJgobM/kqpXlOAT3+4CpDUakHRiiKwlRFSFZhM23MHzJmjnLyFxOHuOA8JWmoHrp7IjlsnEZjGC7oWK1irND9Uxjg0TatQJQqF5MO8cS5fHWUQt5irrWsUHFUauQk94Hc5DGF9CvWUlhOu9AtEONhzm+9ynMsGWVYzUtKLW0OfQqRgZNl593S9yOHbhzsgmvOJ4Z15n9SLOhpnOfDZw487TG3s42uXydu33ioPV/cUu6UzBihQSrs=; rbzsessionid=ceeda2671a261750efda8dfae400d0e9; RT="z=1&dm=pizzapizza.ca&si=np20z3xezjq&ss=kv721go4&sl=0&tt=0"; GlobalContactLessDelivery=true; GlobalContactLessPickup=true; isClubBannerShown=true; isDeliveryTabActive=true; ftr_ncd=6; forterToken=189950b9a89d42b6b094e11447f64294_1635190225056__UDF43_9ck; _gcl_au=1.1.1648738651.1635190226; _ga=GA1.2.467176076.1635190226; _gid=GA1.2.1023906708.1635190226; _gat_UA-6939575-8=1; _scid=cbcfbc56-6280-49d3-8361-5bf4f498fcdb; kumulosID=07c3a489-292e-41ac-a0b5-bae38febeae4; _sctr=1|1635134400000; _pin_unauth=dWlkPVlqRTJPR00xWVRFdE1qWmxNaTAwTmpCaExXRTBOREV0WXpGaE5UWTFZMk0wT1Roag; __zlcmid=16jkQqKglwqRQhl; selectedStore=201; userAddress={%22address_components%22:[{%22long_name%22:%22Ottawa%22%2C%22short_name%22:%22Ottawa%22%2C%22types%22:[%22locality%22%2C%22political%22]}%2C{%22long_name%22:%22Ottawa%22%2C%22short_name%22:%22Ottawa%22%2C%22types%22:[%22administrative_area_level_2%22%2C%22political%22]}%2C{%22long_name%22:%22Ontario%22%2C%22short_name%22:%22ON%22%2C%22types%22:[%22administrative_area_level_1%22%2C%22political%22]}%2C{%22long_name%22:%22Canada%22%2C%22short_name%22:%22CA%22%2C%22types%22:[%22country%22%2C%22political%22]}]%2C%22formatted_address%22:%22Ottawa%2C%20ON%2C%20Canada%22%2C%22latitude%22:45.4215296%2C%22longitude%22:-75.69719309999999}; _gac_UA-6939575-8=1.1635190245.CjwKCAjwq9mLBhB2EiwAuYdMtbSsMmDX4t88PaHTLPPOo3HAyUzsTQcZtTbkjQAiMbu9aSsut0d9ABoCqTAQAvD_BwE'
  
  all_pizzapizza <- tibble()
  
  base_url <- "https://www.pizzapizza.ca/ajax/store/api/v1/search/store_locator?latitude=45.4215296&longitude=-75.69719309999999&cursor="
  
  for (i in 0:10){
    message(i)
    url <- paste0(base_url, 10*i)
    test <- httr::GET(url = url,
                      add_headers(.headers = c("cookie" = cookie,
                                               "fortertoken" = '189950b9a89d42b6b094e11447f64294_1635190504985__UDF43_9ck',
                                               "if-none-match"  = 'W/"2702-O3lzCWL/8PhwxXMCLIopBedvLe4"',
                                               "kumulos-install-id" = "07c3a489-292e-41ac-a0b5-bae38febeae4",
                                               "session-token" = "75ad0ed9-1bc0-4fb6-86e2-7e597594c8e4",
                                               "timestamp" = "1635190506344",
                                               "timezone" = "240",
                                               "user-agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/94.0.4606.81 Safari/537.36",
                                               "x-request-id" = "5e3023fd-ba50-43c6-87fc-e243448b9bb4")))
    
    
    #test <- rvest::session("https://www.pizzapizza.ca/restaurant-locator/Ontario/Ottawa?query=Ottawa,%2520ON,%2520Canada")
    
    pizzapizza <- test %>%
      rvest::read_html() %>%
      rvest::html_text() %>%
      jsonlite::fromJSON() %>%
      pluck("stores") %>%
      as_tibble() 
    
    if (nrow(pizzapizza) == 0) break
    
    pizzapizza <- pizzapizza %>%
      mutate(name = "Pizza Pizza",
             address = sprintf("%s, %s, %s, %s", address, city, province, postal_code )) %>%
      select(name, address, lat = latitude, lon = longitude, phone = market_phone_number)
    
    all_pizzapizza <- bind_rows(all_pizzapizza, pizzapizza)
    
  } # end for
  
  write_csv(all_pizzapizza, "data/restaurants/pizzapizza.csv")  
}


scrape_starbucks <- function(){
  
  # manually search a few urls.
  # we get all 62 if we search them all: json says there are 62 around, but only gives 50 at once
  urls <- c("https://www.starbucks.ca/store-locator?map=45.314953,-76.008969,11z", #west
            "https://www.starbucks.ca/store-locator?map=45.379123,-75.68133,10z&place=Ottawa,%20ON,%20Canada", #"ottawa"
            "https://www.starbucks.ca/store-locator?map=45.134434,-75.782018,11z", # south
            "https://www.starbucks.ca/store-locator?map=45.162617,-75.24424,11z", #southeast
            "https://www.starbucks.ca/store-locator?map=45.30302,-75.210878,11z", #east
            "https://www.starbucks.ca/store-locator?map=45.460748,-75.310107,11z" #northeast
  )
  
  all_starbuckses <- tibble()
  
  for (url in urls){
    message(url)
    sb_sess <- rvest::session(url)
    
    sb_test <- sb_sess %>%
      rvest::read_html() %>%
      rvest::html_elements("script") %>%
      purrr::map_chr(as.character) %>% 
      enframe() %>%
      filter(str_detect(value, "BOOTSTRAP")) %>%
      mutate(value = stringr::str_remove(value, "<script>\\n      window.__BOOTSTRAP = "),
             value = stringr::str_remove_all(value, regex("window\\.__INTL_MESSAGES.*", dotall = TRUE))
             #,value = stringr::str_remove_all(value, "</script>")
      )
    
    sb_json <- sb_test$value
    
    sb_parsed <- jsonlite::fromJSON(sb_json)
    
    
    starbuckses <- sb_parsed$previousAction$payload$data$stores%>%
      jsonlite::flatten() %>%
      as_tibble() %>%
      mutate(address = purrr::map_chr(addressLines, stringr::str_flatten, collapse = ", "),
      ) %>%
      select(name = brandName, address,
             lat = coordinates.latitude,
             lon = coordinates.longitude)
    
    all_starbuckses <- bind_rows(all_starbuckses, starbuckses)
  } # end for all urls
  
  all_starbuckses <- distinct(all_starbuckses)
  
  write_csv(all_starbuckses, "data/restaurants/starbucks.csv")
  
  return(all_starbuckses)
}


scrape_mcdonalds <- function(){
  
  # get up to 250 mcdonalds within 100km of ottawa
  results <- httr::GET("https://www.mcdonalds.com/googleappsv2/geolocation?latitude=45.4215296&longitude=-75.69719309999999&radius=100&maxResults=250&country=ca&language=en-ca&showClosed=&hours24Text=Open%2024%20hr")
  
  mcd <- results %>%
    httr::content()
  
  mcdonalds <- mcd[[1]] %>%
    purrr::map_dfr(function(x) {
      lat <- x$geometry$coordinates[[1]]
      lon <- x$geometry$coordinates[[2]]
      
      ad1 <- x$properties$addressLine1
      ad2 <- x$properties$addressLine2
      ad3 <- x$properties$addressLine3
      ad4 <- x$properties$addressLine4
      postcode <- x$properties$postcode
      telephone <- if_else(is.null(x$properties$telephone), "", x$properties$telephone)
      
      if (is.null(ad2)) address <- sprintf("%s, %s, %s, %s", ad1, ad3, postcode, ad4)
      if (!is.null(ad2)) address <- sprintf("%s, %s, %s, %s, %s", ad1, ad2, ad3, postcode, ad4)
      
      
      tibble(name = "McDonald's",
             address = address,
             phone = telephone,
             lat = lat, lon = lon)
    })
  
  write_csv(mcdonalds, "data/restaurants/mcdonalds.csv")
  
  return(mcdonalds)
  
}

scrape_subway <- function(){
  
  # we can only get 50 results at once, so just do a one-page 50-item search
  num_pages <- 1
  page_size <- 50
  
  # set up some cities we'll search for, using the results frmo the API
  searches <- tibble::tribble(
    ~city, ~lat, ~lon,
    "orleans",  "45.4558019", "-75.504733",
    "ottawa",   "45.4215296", "-75.69719309999999",
    "kanata",   "45.3088185", "-75.89868349999999",
    "nepean",   "45.33490459999999", "-75.7241006",
    "barrhaven", "45.2736841", "-75.7372019",
    "kemptville", "45.0163884","-75.64594699999999"
  )
  
  results <- tibble::tibble()
  
  # for each search city
  for (i in 1:nrow(searches)){
    
    message(sprintf("%d/%d",i, nrow(searches)))
    Sys.sleep(1)
    
    lat <- searches$lat[[i]]
    lon <- searches$lon[[i]]
    city <-searches$city[[i]]
    
    #url <- paste0("https://locator-svc.subway.com/v3/GetLocations.ashx?callback=jQuery1111017238309686193065_1630602370805&q=%7B%22InputText%22%3A%22", city, "%2C+ON%2C+Canada%22%2C%22GeoCode%22%3A%7B%22name%22%3A%22Ottawa%22%2C%22Latitude%22%3A",lat,"%2C%22Longitude%22%3A",lon,"%2C%22CountryCode%22%3A%22CA%22%2C%22City%22%3A%22Ottawa%22%7D%2C%22DetectedLocation%22%3A%7B%22Latitude%22%3A0%2C%22Longitude%22%3A0%2C%22Accuracy%22%3A0%7D%2C%22Paging%22%3A%7B%22StartIndex%22%3A",start_index,"%2C%22PageSize%22%3A",page_size,"%7D%2C%22ConsumerParameters%22%3A%7B%22metric%22%3Atrue%2C%22culture%22%3A%22en-CA%22%2C%22country%22%3A%22CA%22%2C%22size%22%3A%22D%22%2C%22template%22%3A%22%22%2C%22rtl%22%3Afalse%2C%22clientId%22%3A%2217%22%2C%22key%22%3A%22SUBWAY_PROD%22%7D%2C%22Filters%22%3A%5B%5D%2C%22LocationType%22%3A2%2C%22behavior%22%3A%22%22%2C%22FavoriteStores%22%3Anull%2C%22RecentStores%22%3Anull%2C%22Stats%22%3A%7B%22abc%22%3A%5B%7B%22N%22%3A%22geo%22%2C%22R%22%3A%22B%22%7D%5D%2C%22src%22%3A%22autocomplete%22%2C%22act%22%3A%22%22%2C%22c%22%3A%22subwayLocator%22%7D%7D&_=1630602370807") %>%
    #paste0("https://locator-svc.subway.com/v3/GetLocations.ashx?callback=jQuery1111017238309686193065_1630602370805&q=%7B%22InputText%22%3A%22Ottawa%2C+ON%2C+Canada%22%2C%22GeoCode%22%3A%7B%22name%22%3A%22Ottawa%22%2C%22Latitude%22%3A45.4215296%2C%22Longitude%22%3A-75.69719309999999%2C%22CountryCode%22%3A%22CA%22%2C%22City%22%3A%22Ottawa%22%7D%2C%22DetectedLocation%22%3A%7B%22Latitude%22%3A0%2C%22Longitude%22%3A0%2C%22Accuracy%22%3A0%7D%2C%22Paging%22%3A%7B%22StartIndex%22%3A",start_index,"%2C%22PageSize%22%3A",page_size,"%7D%2C%22ConsumerParameters%22%3A%7B%22metric%22%3Atrue%2C%22culture%22%3A%22en-CA%22%2C%22country%22%3A%22CA%22%2C%22size%22%3A%22D%22%2C%22template%22%3A%22%22%2C%22rtl%22%3Afalse%2C%22clientId%22%3A%2217%22%2C%22key%22%3A%22SUBWAY_PROD%22%7D%2C%22Filters%22%3A%5B%5D%2C%22LocationType%22%3A2%2C%22behavior%22%3A%22%22%2C%22FavoriteStores%22%3Anull%2C%22RecentStores%22%3Anull%2C%22Stats%22%3A%7B%22abc%22%3A%5B%7B%22N%22%3A%22geo%22%2C%22R%22%3A%22B%22%7D%5D%2C%22src%22%3A%22autocomplete%22%2C%22act%22%3A%22%22%2C%22c%22%3A%22subwayLocator%22%7D%7D&_=1630602370807")
    url <- paste0('{"InputText":"',city,', ON, Canada","GeoCode":{"Latitude":',lat,',"Longitude":',lon,',"CountryCode":"CA"},"DetectedLocation":{"Latitude":0,"Longitude":0,"Accuracy":0},"Paging":{"StartIndex":',start_index,',"PageSize":',page_size,'},"ConsumerParameters":{"metric":true,"culture":"en-CA","country":"CA","size":"M","template":"","rtl":false,"clientId":"17","key":"SUBWAY_PROD"},"Filters":[],"LocationType":2,"behavior":"","FavoriteStores":null,"RecentStores":null,"Stats":{"abc":[{"N":"geo","R":"B"}],"src":"resize","act":"resize","c":"subwayLocator"}}') 
    url <- urltools::url_encode(url) %>%
      paste0("https://locator-svc.subway.com/v3/GetLocations.ashx?callback=jQuery1111017238309686193065_1630602370805&q=", .)
    
    response <- httr::GET(url)
    
    stores <- response %>%
      httr::content(encoding = "UTF-8", type= "text") %>%
      stringr::str_extract("(?<=\\().*(?=\\))") %>%
      jsonlite::fromJSON()
    
    stores <- stores$ResultData %>%
      as_tibble() %>%
      jsonlite::flatten() %>%
      as_tibble() %>%
      mutate(name = "Subway",
             address = sprintf("%s, %s, %s, %s, %s, %s",
                               Address.Address1,
                               Address.Address2,
                               Address.Address3,
                               Address.City,
                               Address.StateProvCode,
                               Address.PostalCode),
             address = stringr::str_remove_all(address," ,"),
             lat = Geo.Latitude,
             lng = Geo.Longitude
             
      ) %>%
      select(name, address, lat, lng)
    
    results <- bind_rows(results, stores) %>%
      distinct()
  }
  
  write_csv(results, "data/restaurants/subway.csv")
  return(results)
}


scrape_tim_hortons <- function(){
  
  # Tim Horton's API uses GraphQL on the back-end
  #https://graphql.org/learn/queries/
  
  url <- "https://use1-prod-th.rbictg.com/graphql"
  
  # got the json payload from Chrome network tools, BUT the json was invalid: had
  # lots of \n newlines that messed it up. search/replaced them to spaces and it goes
  # edited a few variables to return 500 stores within 80km
  payload <- '[{"operationName":"GetRestaurants","variables":{"input":{"filter":"NEARBY","coordinates":{"userLat":45.4215296,"userLng":-75.69719309999999,"searchRadius":80000},"first":500,"status":"OPEN"}},
"query":"query GetRestaurants($input: RestaurantsInput) {   restaurants(input: $input) {     pageInfo {       hasNextPage       endCursor       __typename     }     totalCount     nodes {       ...RestaurantNodeFragment       __typename     }     __typename   } }  fragment RestaurantNodeFragment on RestaurantNode {   _id   storeId   isAvailable   posVendor   chaseMerchantId   curbsideHours {     ...OperatingHoursFragment     __typename   }   deliveryHours {     ...OperatingHoursFragment     __typename   }   diningRoomHours {     ...OperatingHoursFragment     __typename   }   distanceInMiles   drinkStationType   driveThruHours {     ...OperatingHoursFragment     __typename   }   driveThruLaneType   email   environment   franchiseGroupId   franchiseGroupName   frontCounterClosed   hasBreakfast   hasBurgersForBreakfast   hasCatering   hasCurbside   hasDelivery   hasDineIn   hasDriveThru   hasMobileOrdering   hasParking   hasPlayground   hasTakeOut   hasWifi   id   isDarkKitchen   isFavorite   isRecent   latitude   longitude   mobileOrderingStatus   name   number   parkingType   phoneNumber   physicalAddress {     address1     address2     city     country     postalCode     stateProvince     stateProvinceShort     __typename   }   playgroundType   pos {     vendor     __typename   }   posRestaurantId   restaurantImage {     asset {       _id       metadata {         lqip         palette {           dominant {             background             foreground             __typename           }           __typename         }         __typename       }       __typename     }     crop {       top       bottom       left       right       __typename     }     hotspot {       height       width       x       y       __typename     }     __typename   }   restaurantPosData {     _id     __typename   }   status   vatNumber   __typename }  fragment OperatingHoursFragment on OperatingHours {   friClose   friOpen   monClose   monOpen   satClose   satOpen   sunClose   sunOpen   thrClose   thrOpen   tueClose   tueOpen   wedClose   wedOpen   __typename } "}]'
  
  
  #[{"operationName":"GetRestaurants","variables":{"input":{"filter":"NEARBY","coordinates":{"userLat":45.4215296,"userLng":-75.69719309999999,"searchRadius":80000},"first":500,"status":"OPEN"}},"query":"query GetRestaurants($input: RestaurantsInput) {\n  restaurants(input: $input) {\n    pageInfo {\n      hasNextPage\n      endCursor\n      __typename\n    }\n    totalCount\n    nodes {\n      ...RestaurantNodeFragment\n      __typename\n    }\n    __typename\n  }\n}\n\nfragment RestaurantNodeFragment on RestaurantNode {\n  _id\n  storeId\n  isAvailable\n  posVendor\n  chaseMerchantId\n  curbsideHours {\n    ...OperatingHoursFragment\n    __typename\n  }\n  deliveryHours {\n    ...OperatingHoursFragment\n    __typename\n  }\n  diningRoomHours {\n    ...OperatingHoursFragment\n    __typename\n  }\n  distanceInMiles\n  drinkStationType\n  driveThruHours {\n    ...OperatingHoursFragment\n    __typename\n  }\n  driveThruLaneType\n  email\n  environment\n  franchiseGroupId\n  franchiseGroupName\n  frontCounterClosed\n  hasBreakfast\n  hasBurgersForBreakfast\n  hasCatering\n  hasCurbside\n  hasDelivery\n  hasDineIn\n  hasDriveThru\n  hasMobileOrdering\n  hasParking\n  hasPlayground\n  hasTakeOut\n  hasWifi\n  id\n  isDarkKitchen\n  isFavorite\n  isRecent\n  latitude\n  longitude\n  mobileOrderingStatus\n  name\n  number\n  parkingType\n  phoneNumber\n  physicalAddress {\n    address1\n    address2\n    city\n    country\n    postalCode\n    stateProvince\n    stateProvinceShort\n    __typename\n  }\n  playgroundType\n  pos {\n    vendor\n    __typename\n  }\n  posRestaurantId\n  restaurantImage {\n    asset {\n      _id\n      metadata {\n        lqip\n        palette {\n          dominant {\n            background\n            foreground\n            __typename\n          }\n          __typename\n        }\n        __typename\n      }\n      __typename\n    }\n    crop {\n      top\n      bottom\n      left\n      right\n      __typename\n    }\n    hotspot {\n      height\n      width\n      x\n      y\n      __typename\n    }\n    __typename\n  }\n  restaurantPosData {\n    _id\n    __typename\n  }\n  status\n  vatNumber\n  __typename\n}\n\nfragment OperatingHoursFragment on OperatingHours {\n  friClose\n  friOpen\n  monClose\n  monOpen\n  satClose\n  satOpen\n  sunClose\n  sunOpen\n  thrClose\n  thrOpen\n  tueClose\n  tueOpen\n  wedClose\n  wedOpen\n  __typename\n}\n"}]  
  response <- httr::POST(url = url,
                         body = payload
                         ,content_type_json()
  )
  
  r <- response %>%
    httr::content(encoding = "UTF-8")
  #jsonlite::fromJSON()
  
  stores <- tibble::tibble(data = r[[1]]$data$restaurants$nodes)
  
  results <- stores %>%
    mutate(name = "Tim Horton's", #purrr::map_chr(data, pluck("name")),
           phone_number = purrr::map_chr(data, pluck("phoneNumber")),
           address1 = purrr::map_chr(data, pluck, "physicalAddress", "address1"),
           address2 = purrr::map_chr(data, pluck, "physicalAddress", "address2"),
           city = purrr::map_chr(data, pluck, "physicalAddress", "city"),
           province = purrr::map_chr(data, pluck, "physicalAddress", "stateProvinceShort"),
           postal_code = purrr::map_chr(data, pluck, "physicalAddress", "postalCode"),
           lat = purrr::map_chr(data, pluck, "latitude"),
           lng = purrr::map_chr(data, pluck, "longitude"),
           breakfast_burgers = purrr::map_lgl(data, pluck, "hasBurgersForBreakfast")
    ) %>%
    mutate(address = sprintf("%s, %s, %s %s", address1, city, province, postal_code)) %>%
    select(name, address, phone_number, lat, lng)
  
  write_csv(results, "data/restaurants/tim_hortons.csv")
}


scrape_harveys <- function(){
  
  url <- "https://aws-api.harveys.ca/CaraAPI/APIService/getStoreList?from=60.000,-150.000&to=39.000,-50.000&eCommOnly=N"
  
  # may need to get a new cookie from your browser--could this be automated?
  request_headers <- c("cookie" = "ak_bmsc=393CC15AA69DB7E238C37A29F052159E~000000000000000000000000000000~YAAQTQEkF4Chthd7AQAAsKACGAyEnwoHwKz3qXQczQdtK725wDNe0bmGxLhlKie7qan0eAp3epabpVwjZSGKIOjCn86j1N9/Gy+tZtkI1DyTzVEOADJK2qTjjie6lKElRA2jV1iOvdwR7bXvUkXkeeaVQue3zXBTb+K3DV9xnchx1Eh6CKObWM9+t/PNuOF6FV5qOLIJf3T/RMyjp8X96duD4CP2P47laTNJhN61DGn7iTCyJQbqZEST7yqX6asxp5/AvEttRSprqWo5/dlSxVzw/g/2sTIiJBl7if6E0HEqo0it0uZ4MbKktDPzN/yDBSq5FJ+u+0YO34MNr+SMB5vUxT+GgmIo+ZsXapbscjrHj5WAUJD/B1xBqGmk9IBx4bbVyx20l7uinQ==; bm_sz=CB26CCC7BE6FFCB16EF0BD9AB256B000~YAAQTQEkF4Ghthd7AQAAsKACGAzN1bhbZw8SbNOmhtjbRu79pO2udsXafLcEqDC39Y2KAx43sf2TGRnLiM4yDH2tgjq5jsZ2mIF7jKIOGMq2N7Pg623Zt+hl8TcBieuaQIQ7HEy2e+PVx9urYRAqpQLSyxiHIVIajv3bRE37ViqhL07WxP6DGRQDQtcwEwK0vRV9ViehX4F14TS8orowTd2fM3z6eHIQ1mhBmIIvcJlnH8Ja6cCpUuQQ1A3SsKZxEfhMlKgLnKhuFqO5PEhZW0qzf4QauRdhbqx7NdhKVA12Sfg=~3621699~4403506; bm_sv=DBA21C122680AF26D416D00EA87EF0A0~4upkkV6bMdN4+6oaEB8EQ+fWWIkOhDOVDQJt/D9w9EArKTiyVUvB+QTblvR3kVo6oB80lzyfXTIIipoIaBIBwbZjMdHvBTm7vYo+qYTzPZxD7vv/JUV0YevmenZYfL/kWJ5SU38Wor/HqV4nsl0rpkXs0+lMZUTTHnajF+FM6ZY=; _ga=GA1.2.462680228.1628195431; _gid=GA1.2.1853089168.1628195431; _gat_UA-46928317-1=1; optimizelyEndUserId=oeu1628195431087r0.6805542791433585; _fbp=fb.1.1628195431247.672807049; __qca=P0-2143321879-1628195431392; _abck=F30097D97BF464C80DC4DD4A374DAA43~0~YAAQTQEkF7qhthd7AQAAfaQCGAY8q1R8IvvNL8NcpNXLMqvoE3fdQ8E3L1tiRKUwM9Ut+qfEQtaIvoBkq5HrQlH2MDA3T9EZSAnIFy1o48gIGt+5E9IkRDxTceYAWXDLD0nZzCQacRiLQMob4uMOAXnSoPMj2d0sbLfhFsN6Au4hyYX1T2ZOeaxp0BsdA2m8S5tCiC4pV92tg/UQrzYYHA5R2w55tr9s/qV+fhXkvxWFfVZ5AczmN6Df1RkQN5X33jkLBkk+29IJGUIAQIoxwNx9ghEb1Xk3GD/nqYi3SwphirLzdAvmY1cuwBxOH2ZHN51rxJGYNyUBh3Qm1wW6V68bRXVffYyxtUxARLUDwCoRh+xDhsml/826y26oopmFFrO7a4StAN/MKVgOxmMa4MCl4UH2KD/H~-1~||-1||~-1")
  
  resp <- httr::GET(url,
                    httr::add_headers(request_headers,
                                      ":authority" = "aws-api.harveys.ca",
                                      ":method" = "GET",
                                      ":path" = "/CaraAPI/APIService/getStoreList?from=60.000,-150.000&to=39.000,-50.000&eCommOnly=N",
                                      ":scheme" = "https",
                                      "accept" = "application/json, text/javascript, */*; q=0.01",
                                      "accept-encoding"= "gzip, deflate, br",
                                      "accept-language" = "en-CA,en;q=0.9",
                                      "origin" =  "https://www.harveys.ca",
                                      "referer" = "https://www.harveys.ca/",
                                      "sec-ch-ua" = '"Chromium";v="92", " Not A;Brand";v="99", "Google Chrome";v="92"',
                                      "sec-ch-ua-mobile" = "?0",
                                      "sec-fetch-dest"= "empty",
                                      "sec-fetch-mode"= "cors",
                                      "sec-fetch-site"= "same-site"),
                    httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/92.0.4515.131 Safari/537.36"))
  
  data <- rawToChar(resp$content) %>% #httr::content(resp) %>%
    jsonlite::fromJSON() 
  
  
  
  df <- data$response$responseContent$storeModel %>% 
    tibble::as_tibble() %>%
    mutate(address = sprintf("%s %s, %s, %s, %s", streetNumber, street, city, province, postalCode),
           name = "Harvey's") %>%
    select(name, address, phone = phoneNumber)
  
  df %>%
    write_csv("data/restaurants/harveys.csv") %>%
    return()
  
  # df %>%
  #   sf::st_as_sf(coords = c("longitude", "latitude"), crs = "WGS84") %>% ggplot() + geom_sf()
  
  
}
