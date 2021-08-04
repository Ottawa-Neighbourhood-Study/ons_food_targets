## This file has the functions for scraping Circle K and Quickie's locations.

# It also loads the already-scraped convenience stores from Circle K,
# Quickie, and Yelp, does some minimal cleaning to remove duplicates, and saves
# the results in a timestamped file for further processing.

library(tidyverse)
library(osmdata)
library(onsr)
library(sf)
library(leaflet)
library(httr)
library(rvest)


old_food <- read_csv("data/original/ONS Food Environment Mega Data - Food Env-edit.csv")

# extract the old convenience stores, make sure their address includes "Ottawa, ON" if 
# there is no comma in the address (which here means no other city provided)
# old_convenience <- old_food %>%
#   filter(Category2 == "convenience store") %>%
#   select(Name, address, Category3) %>%
#   mutate(address = if_else(stringr::str_detect(address, ","), address,
#                            paste0(address, ", Ottawa, ON"))) %>%
#   onsr::geocode_gmap(address, api_key = api_key, verbose = TRUE)
# 
# con_old <- old_convenience %>%
#   rename(name_old = Name,
#          address_old = address,
#          note_old = Category3)

#write_csv(con_old, "data/convenience/conv_old_geocoded.csv")
con_old <- read_csv("data/convenience/conv_old_geocoded.csv")

con_new <- read_csv("data/convenience/convenience_2021-07-27.csv") %>%
#  con_manual 
#  %>% select(-num, -num2) %>%
  rename(name_new = name,
         address_new = address,
         note_new = note,
         phone_new = phone)

con_join <- full_join(con_old, con_new, by = c("lat", "lng"))

# This function loads the already-scraped results, removes some duplicates, and
# puts them together. The next step is to compare them to the old results from
# the last dataset.
combine_convenience_results <- function() {
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
}


# ADDING NEW ONES

esso <- read_csv("data/convenience/esso.csv")
cdn_tire_gas <- read_csv("data/convenience/cdn_tire_gas.csv")
dollar_tree <- read_csv("data/convenience/dollar_tree.csv")
dollarama <- read_csv("data/convenience/dollarama.csv")
giant_tiger <- read_csv("data/convenience/giant_tiger.csv")
macewen <- read_csv("data/convenience/macewen.csv")
mrgas <- read_csv("data/convenience/mrgas.csv")
petrocan <- read_csv("data/convenience/petrocan.csv")
pioneer <- read_csv("data/convenience/pioneer.csv")
shell <- read_csv("data/convenience/shell.csv")
ultramar <- read_csv("data/convenience/ultramar.csv")


# ULTRAMAR / JOURNIE

scrape_ultramar <- function() {
  url <- "https://www.journie.ca/en-CA/Destinations/Find?Length=100&__RequestVerificationToken=f4EoWvkEsQegLq_BvzIJuWiNIr9vKVXAylUwXprtrA4uP8kaUW7ocCjU8I9_FJ3eSl6imUCQqyUhm05c2os3-InefzU1&UserLatitude=0&UserLongitude=0&g-recaptcha-response=03AGdBq247zgnPPFJZGnBmSJcrGJRrpYMOsLBI4u3KGitwisQI1C_13kk0osXNdb6Ypjp3rQRtmC3E3al2_dYpRbj3CmT-WIdu6YtZQVcRlHYMWsrTxVk6XsEygLl9M9siLnRCBC3vDOHb2Gckz6LwT3k6sm8e0SvVfTJNHecxyar4IhGKr5LanMwJiqVap_vzBvZMJq-VX71MAitIEI7bShSBB8TTyzdyM8GTlF7LasZiKzDSGDi4MwnuYYSU3aWcBiv70NKoAl-Zimaww_ftj2lW3Rhn2otmZ-pQE-imVBr5p5wuz3JHNbyvPJ6gooaLilPNskxJ55duqj_G_kGlCIWrggQFE1bvPuSqlXNZzcSeFvrubQuZIxX4vZPVAj9sgoh23OsowxNzms-8xP3doaJwCUX2rrBn3kojqzNBQM8cKGI7MAi3Jktwb56IfjlkMzyEhIzJPVBh&StreetAddress=Ottawa%2C%20ON%2C%20Canada&Distance=35&TwentyFourHour=false&EarnAtPump=false&EarnAtStore=false&LocationServiceIds=18&X-Requested-With=XMLHttpRequest&_=1628036336401"

  resp <- httr::GET(url)  
  
  df <- httr::content(resp, encoding = "UTF-8")
  
  t <- df %>%
    rvest::html_elements(css = ".station-desc-area") %>% rvest::html_elements(css = ".row:nth-child(1) .col-xs-6:nth-child(1)") %>%
    rvest::html_text2()
  
  phones <- df %>%
    rvest::html_elements(css = ".row:nth-child(1) .col-xs-6+ .col-xs-6") %>%
    rvest::html_text2() %>%
    stringr::str_squish()
  
  ultramar <- tibble(t = t) %>%
    mutate(name = stringr::str_extract(t, ".*(?=\\n)"),
           address = stringr::str_extract(t, "(?<=\\n).*"),
           phone = phones) %>%
    select(-t)
  
  ultramar %>%
    write_csv("data/convenience/ultramar.csv") %>%
    return()
  }


# SHELL GAS STATIONS

scrape_shell <-function() {
  url <- "https://shellgsllocator.geoapp.me/api/v1/locations/nearest_to?lat=45.4&lng=-75.7&autoload=true&travel_mode=driving&avoid_tolls=false&avoid_highways=false&avoid_ferries=false&corridor_radius=50&driving_distances=true&format=json"

  resp <- httr::GET(url)  
  

  
  data <- httr::content(resp, type = "text/json", encoding = "UTF-8") %>%
    jsonlite::fromJSON() %>%
    as_tibble() %>%
    mutate(address = sprintf("%s, %s, %s, %s", address, city, state, postcode),
           name = "Shell") %>%
    select(name, address, phone = telephone, lat, lng)
  
  data %>%
    write_csv("data/convenience/shell.csv") %>%
    return()

  }


# PIONEER GAS STATIONS

scrape_pioneer <- function() {
  url <- "https://www.pioneer.ca/en/find-station/?latitude=&longitude=&locator_q=ottawa%2C+on"
  
  resp <- httr::GET(url)
  
  html_data <- httr::content(resp, encoding = "UTF-8", type = "text/html")
  
  stores <- html_data %>%
    rvest::html_elements(css = ".heading__title--result") 
  
  store_addresses <- stores %>%
    purrr::map(rvest::html_elements, "span") %>%
    purrr::map(rvest::html_text2) %>%
    purrr::map(stringr::str_flatten, collapse = ", ") %>%
    unlist()
  
  pioneer <- tibble(name = "Pioneer Gas",
                    address = store_addresses,
                    phone = NA)
  
  pioneer %>%
    write_csv("data/convenience/pioneer.csv") %>%
    return()
  # lats and lngs are not real values, they're all the same apparently
  # lats <- stores %>% as.character() %>% stringr::str_extract("(?<=latitude\\=).*(?=\\&)")
  # lngs <- stores %>% as.character() %>% stringr::str_extract('(?<=longitude\\=).*(?=")')
  
  #  rvest::html_text()safdafs NOT GETTING THE LINEBREAKS RIGHT
  #   as.character()
  #   rvest::html_elements(css = ".localization__right-col-item-last-section") %>%as.character()
  # 
  # stores %>%
  #   rvest::html_attrs()
  # 
  #   rvest::html_text2()
  #   rvest::html_elements(css = ".hidden") %>%
  #   rvest::html_text2()
  #   rvest::html_elements(css = ".info_window .clearfix") %>%
  #   rvest::html_text2()
  # %>%
  #   jsonlite::fromJSON() %>%
  #   as_tibble()
  # 
}

# PETRO CANADA

scrape_petro_can <- function() {
  url <- "https://www.petro-canada.ca/en/api/petrocanada/locations?fuel&hours&lat=45.3876404&limit=250&lng=-75.64942669999999&place&range=50&service"
  
  resp <- httr::GET(url)

  data <- httr::content(resp, encoding = "UTF-8")
  
  petrocan <- tibble(df = data) %>%
    unnest_wider(col = "df") %>%
    mutate(address = sprintf("%s, %s, %s, %s", AddressLine, PrimaryCity, Subdivision, PostalCode),
           name = "Petro-Canada") %>%
    select(name, 
           address, 
           phone = Phone, 
           lat = Latitude,
           lng= Longitude)
    
  petrocan %>%
    write_csv("data/convenience/petrocan.csv") %>%
    return()
  
  }

# MR GAS

scrape_mr_gas <- function() {
  url <- "https://mrgasltd.ca/wp-admin/admin-ajax.php?action=asl_load_stores&nonce=3c8951fe27&load_all=1&layout=1"

  resp <- httr::GET(url)

  data <- httr::content(resp, type = "text/json", encoding = "UTF-8") %>%
    #rvest::html_text2() %>%
    jsonlite::fromJSON() %>%
    as_tibble()

  mrgas <- data %>%
    mutate(address = sprintf("%s, %s, %s, %s", street, city, state, postal_code),
           name = "Mr. Gas",
           lat = as.numeric(lat),
           lng = as.numeric(lng)) %>%
    select(name, address, phone, lat, lng)
  
  mrgas %>%
    write_csv("data/convenience/mrgas.csv") %>%
    return()
  
  }


# MACEWEN GAS / CONVENIENCE

scrape_macewen <- function() {
  url <- "https://macewen.ca/wp-content/cache/interactive-maps/station-en.json"

  resp <- httr::GET(url)  
  
  data <- httr::content(resp)

  mce <- tibble(mce = data$places)
  df <- mce %>%
    hoist(mce,
          name = "title",
          phone = "phone",
          a1 = list("address", 1),
          a2 = list("address", 2),
          a3 = list("address", 3),
          lat = list("position", 1),
          lng = list("position", 2)
          ) %>%
    mutate(address = sprintf("%s, %s, %s", a1, a2, a3)) %>%
    select(name, address, phone, lat, lng) %>%
    filter(lat > 0)

  df %>%
    write_csv("data/convenience/macewen.csv") %>%
    return()
    
  # mce %>%
  #   unnest_wider(col = mce)
  #   data$places[[1]]$position
  #   
}

## GIANT TIGER

scrape_giant_tiger <- function() {
  url <- "https://www.gianttiger.com/api/commerce/storefront/locationUsageTypes/SP/locations/?startIndex=0&pageSize=50&filter=geo%20near%2845.4215296%2C-75.69719309999999%2C10000000%29&includeAttributeDefinition=true"
  #header_cookie <- "sb-sf-at-prod-s=at=gN8cHDzCAG6q2d5b3OJa6walEqflGY0NTYZj6T8qo3jboTd5JQ4CVh%2BWHbEF%2BPp3afdc98vUVXDIx2dqM5H7mOEO5h1OiWe2bAlF%2FfH1ivZip503zdANvxj3%2BWxZ0QJNDAg3kRelLOyFiHjjyFqNg0%2BDUCI6uGDYzVwmiab%2FFb6Rw4WmpRSTChpQzmY48Kax0sZW85M6y3kO0dB5h%2FZ%2Fa7Ih1RY11Ewgs3f36Su%2BdEefJiO9g9fYlaKnsba6fxtnUTF8ZHJ6D53U%2B4n6h1uQWZXhzaxkgZqPra4q1caU8zG%2FHNuea7U9UFo4MpYmyaqJ5dK0sPgB39eW7AYQZk65ww%3D%3D&dt=2021-08-03T18%3A46%3A50.6432623Z; sb-sf-at-prod=at=gN8cHDzCAG6q2d5b3OJa6walEqflGY0NTYZj6T8qo3jboTd5JQ4CVh%2BWHbEF%2BPp3afdc98vUVXDIx2dqM5H7mOEO5h1OiWe2bAlF%2FfH1ivZip503zdANvxj3%2BWxZ0QJNDAg3kRelLOyFiHjjyFqNg0%2BDUCI6uGDYzVwmiab%2FFb6Rw4WmpRSTChpQzmY48Kax0sZW85M6y3kO0dB5h%2FZ%2Fa7Ih1RY11Ewgs3f36Su%2BdEefJiO9g9fYlaKnsba6fxtnUTF8ZHJ6D53U%2B4n6h1uQWZXhzaxkgZqPra4q1caU8zG%2FHNuea7U9UFo4MpYmyaqJ5dK0sPgB39eW7AYQZk65ww%3D%3D; _mzvr=1_HepdZz4Uywa_GbIwZgUw; _mzvs=nn; ak_bmsc=E8BD910AD886722CB4941271F3E758F2~000000000000000000000000000000~YAAQBhTorNITHOd6AQAAtwBXDQz3oDhACFBU/B6d+hSQ+BR1E6EePK6pTzukYAWdGo+GKw7P+QLXWXPKMVHwMpq8gUpkYcbemvyEf6ig/VZzcrFPeyNWWBZGQEyP+cLE8IG1aURUf4nLFDykqAbFyW5Drv8FDWhulC3LCoFhRe2902asLTA7xU2e45JDHCfDklaiOyCMH4Jpmf9ar5FZXdll+72iPUHD6PHmkDLG8RyRik1efmp+/0FyBEJoHA2ltAXtO2Pyxt9Osz0x+VUk6Cdsyrd9WHMD6KFeRpd8OYv6s1KyCrPEQ+89CJN7mI59+2zXQbVYYwJCiFhS+WiWaACq8r4al1vOvPN4xdB7n/LfbrywHlnZk4UwKCTjRusCFNBBYwbDvkmZ7v/T7Q4=; mt.v=2.1833392231.1628016411580; mt.sc=%7B%22i%22%3A1628016411997%2C%22d%22%3A%5B%5D%7D; _gcl_au=1.1.857893090.1628016412; _ga=GA1.2.1175775704.1628016412; _gid=GA1.2.1920181571.1628016412; _gat_DEPLABS=1; _fbp=fb.1.1628016412335.1826766078; sc.ASP.NET_SESSIONID=n1fg3a5og4wk3uvkceqj41ki; mozucart=%7B%22itemCount%22%3A0%2C%22totalQuantity%22%3A0%2C%22total%22%3A0%2C%22isExpired%22%3Afalse%2C%22hasActiveCart%22%3Atrue%7D; GSID2SAD0Fk5Vxbf=8e786ee2-0bee-4538-b159-46c40c33745f; STSID717056=1baa6574-8a44-4f81-a38d-41de20d32a37; dlCartProductsCategories=%7B%7D; _vuid=1c33cef5-d1b8-4bea-b17a-f08ffd2fe178; my-store-nickname=Ottawa%2C%20Wellington%20St%20W; my-store-code=10; my-store-name=10%20-%20Giant%20Tiger%2C%20Ottawa; my-store-selected=true; my-store-data=%7B%22code%22%3A%2210%22%2C%22name%22%3A%2210%20-%20Giant%20Tiger%2C%20Ottawa%22%2C%22geo%22%3A%7B%22lat%22%3A45.403%2C%22lng%22%3A-75.7262%7D%2C%22address%22%3A%7B%22address1%22%3A%221085%20Wellington%20Street%20West%22%2C%22cityOrTown%22%3A%22Ottawa%22%2C%22stateOrProvince%22%3A%22ON%22%2C%22postalOrZipCode%22%3A%22K1Y%202Y4%22%2C%22countryCode%22%3A%22CA%22%2C%22isValidated%22%3Afalse%7D%2C%22phone%22%3A%22613-722-9589%22%2C%22nickname%22%3A%22Ottawa%2C%20Wellington%20St%20W%22%2C%22curbsidePickUp%22%3Atrue%2C%22schedulingStore%22%3A%22%22%2C%22openToday%22%3A%229%3A00AM-9%3A00PM%22%7D; bm_mi=9628935E88C74467A8F2E0D87F701655~+qEzlPq4EzhBwwa6CTBnLTkPFx8Pa9IHG65qPnMsqeuKmQMoLcqEd6O76gqWvwxuww3+f89i+30koJYbu2+UhCurWTGsvmp2EdFxx4nKlaL52aAUkW5Sp/yGScyESHVb0Jh3CHTmxeNHXIuxwc8ajTT/B6O7+FsGsM4ctj0lXlzNZX7amlSzEjeC7MU9oaNiQ/MMdnOpg6g4W0UqdySbkQrT4GZN3lAzjp91BQieWEuPNhB1Uy7jjVZXfeesP5Q3; _mzvt=6YFRAw_XnkSxce_SDFUZuw; _mzPc=eyJjb3JyZWxhdGlvbklkIjoiNDZiYjZlMTIzZWFiNDM1ZWE3Y2ZiNjlhNTE0ZWYzYTkiLCJpcEFkZHJlc3MiOiIxODQuMTQ2LjEuMjE0IiwiaXNEZWJ1Z01vZGUiOmZhbHNlLCJpc0NyYXdsZXIiOmZhbHNlLCJpc01vYmlsZSI6ZmFsc2UsImlzVGFibGV0IjpmYWxzZSwiaXNEZXNrdG9wIjp0cnVlLCJ2aXNpdCI6eyJ2aXNpdElkIjoiNllGUkF3X1hua1N4Y2VfU0RGVVp1dyIsInZpc2l0b3JJZCI6IjFfSGVwZFp6NFV5d2FfR2JJd1pnVXciLCJpc1RyYWNrZWQiOmZhbHNlLCJpc1VzZXJUcmFja2VkIjpmYWxzZX0sInVzZXIiOnsiaXNBdXRoZW50aWNhdGVkIjpmYWxzZSwidXNlcklkIjoiZjU5OTJjOThkNzJjNGVlZDgyOGVmMjQwNjkzMDY5NGQiLCJmaXJzdE5hbWUiOiIiLCJsYXN0TmFtZSI6IiIsImVtYWlsIjoiIiwiaXNBbm9ueW1vdXMiOnRydWUsImJlaGF2aW9ycyI6WzEwMTRdLCJpc1NhbGVzUmVwIjpmYWxzZX0sInVzZXJQcm9maWxlIjp7InVzZXJJZCI6ImY1OTkyYzk4ZDcyYzRlZWQ4MjhlZjI0MDY5MzA2OTRkIiwiZmlyc3ROYW1lIjoiIiwibGFzdE5hbWUiOiIiLCJlbWFpbEFkZHJlc3MiOiIiLCJ1c2VyTmFtZSI6IiJ9LCJwdXJjaGFzZUxvY2F0aW9uIjp7ImNvZGUiOiIxMCJ9LCJpc0VkaXRNb2RlIjpmYWxzZSwiaXNBZG1pbk1vZGUiOmZhbHNlLCJub3ciOiIyMDIxLTA4LTAzVDE4OjQ3OjIzLjE4ODY1NloiLCJjcmF3bGVySW5mbyI6eyJpc0NyYXdsZXIiOmZhbHNlfSwiY3VycmVuY3lSYXRlSW5mbyI6e319; bm_sv=0F32C269411FAC1F7F9913277D761C36~zHoObQqkf3oNljg4AKynQPHWsqmk+aElUGwC2wqkMvuFAAvFyppmn2S/zMFsSmofBV5duSCcTUlxadPFs+V8/XPvXwFI0x+MqIyE3EkPAw5EbtM6EIRNijdKBnugTzns42JR9IFAevKGbJFUXPxV6nFsg9R1aSiLb3c8GB3FQ/Y="
  #header_cookie <-  "sb-sf-at-prod-s=at=gN8cHDzCAG6q2d5b3OJa6walEqflGY0NTYZj6T8qo3jboTd5JQ4CVh%2BWHbEF%2BPp3afdc98vUVXDIx2dqM5H7mOEO5h1OiWe2bAlF%2FfH1ivZip503zdANvxj3%2BWxZ0QJNDAg3kRelLOyFiHjjyFqNg0%2BDUCI6uGDYzVwmiab%2FFb6Rw4WmpRSTChpQzmY48Kax0sZW85M6y3kO0dB5h%2FZ%2Fa7Ih1RY11Ewgs3f36Su%2BdEefJiO9g9fYlaKnsba6fxtnUTF8ZHJ6D53U%2B4n6h1uQWZXhzaxkgZqPra4q1caU8zG%2FHNuea7U9UFo4MpYmyaqJ5dK0sPgB39eW7AYQZk65ww%3D%3D&dt=2021-08-03T18%3A46%3A50.6432623Z; "
  header_cookie <-  "sb-sf-at-prod-s=at=gN8cHDzCAG6q2d5b3OJa6walEqflGY0NTYZj6T8qo3jboTd5JQ4CVh%2BWHbEF%2BPp3afdc98vUVXDIx2dqM5H7mOEO5h1OiWe2bAlF%2FfH1ivZip503zdANvxj3%2BWxZ0QJNDAg3kRelLOyFiHjjyFqNg0%2BDUCI6uGDYzVwmiab%2FFb6Rw4WmpRSTChpQzmY48Kax0sZW85M6y3kO0dB5h%2FZ%2Fa7Ih1RY11Ewgs3f36Su%2BdEefJiO9g9fYlaKnsba6fxtnUTF8ZHJ6D53U%2B4n6h1uQWZXhzaxkgZqPra4q1caU8zG%2FHNuea7U9UFo4MpYmyaqJ5dK0sPgB39eW7AYQZk65ww%3D%3D&dt=2021-08-03T18%3A46%3A50.6432623Z; sb-sf-at-prod=at=gN8cHDzCAG6q2d5b3OJa6walEqflGY0NTYZj6T8qo3jboTd5JQ4CVh%2BWHbEF%2BPp3afdc98vUVXDIx2dqM5H7mOEO5h1OiWe2bAlF%2FfH1ivZip503zdANvxj3%2BWxZ0QJNDAg3kRelLOyFiHjjyFqNg0%2BDUCI6uGDYzVwmiab%2FFb6Rw4WmpRSTChpQzmY48Kax0sZW85M6y3kO0dB5h%2FZ%2Fa7Ih1RY11Ewgs3f36Su%2BdEefJiO9g9fYlaKnsba6fxtnUTF8ZHJ6D53U%2B4n6h1uQWZXhzaxkgZqPra4q1caU8zG%2FHNuea7U9UFo4MpYmyaqJ5dK0sPgB39eW7AYQZk65ww%3D%3D;"# _mzvr=1_HepdZz4Uywa_GbIwZgUw; _mzvs=nn; ak_bmsc=E8BD910AD886722CB4941271F3E758F2~000000000000000000000000000000~YAAQBhTorNITHOd6AQAAtwBXDQz3oDhACFBU/B6d+hSQ+BR1E6EePK6pTzukYAWdGo+GKw7P+QLXWXPKMVHwMpq8gUpkYcbemvyEf6ig/VZzcrFPeyNWWBZGQEyP+cLE8IG1aURUf4nLFDykqAbFyW5Drv8FDWhulC3LCoFhRe2902asLTA7xU2e45JDHCfDklaiOyCMH4Jpmf9ar5FZXdll+72iPUHD6PHmkDLG8RyRik1efmp+/0FyBEJoHA2ltAXtO2Pyxt9Osz0x+VUk6Cdsyrd9WHMD6KFeRpd8OYv6s1KyCrPEQ+89CJN7mI59+2zXQbVYYwJCiFhS+WiWaACq8r4al1vOvPN4xdB7n/LfbrywHlnZk4UwKCTjRusCFNBBYwbDvkmZ7v/T7Q4=; mt.v=2.1833392231.1628016411580; mt.sc=%7B%22i%22%3A1628016411997%2C%22d%22%3A%5B%5D%7D; _gcl_au=1.1.857893090.1628016412; _ga=GA1.2.1175775704.1628016412; _gid=GA1.2.1920181571.1628016412; _gat_DEPLABS=1; _fbp=fb.1.1628016412335.1826766078; sc.ASP.NET_SESSIONID=n1fg3a5og4wk3uvkceqj41ki; mozucart=%7B%22itemCount%22%3A0%2C%22totalQuantity%22%3A0%2C%22total%22%3A0%2C%22isExpired%22%3Afalse%2C%22hasActiveCart%22%3Atrue%7D; GSID2SAD0Fk5Vxbf=8e786ee2-0bee-4538-b159-46c40c33745f; STSID717056=1baa6574-8a44-4f81-a38d-41de20d32a37; dlCartProductsCategories=%7B%7D; _vuid=1c33cef5-d1b8-4bea-b17a-f08ffd2fe178; my-store-nickname=Ottawa%2C%20Wellington%20St%20W; my-store-code=10; my-store-name=10%20-%20Giant%20Tiger%2C%20Ottawa; my-store-selected=true; my-store-data=%7B%22code%22%3A%2210%22%2C%22name%22%3A%2210%20-%20Giant%20Tiger%2C%20Ottawa%22%2C%22geo%22%3A%7B%22lat%22%3A45.403%2C%22lng%22%3A-75.7262%7D%2C%22address%22%3A%7B%22address1%22%3A%221085%20Wellington%20Street%20West%22%2C%22cityOrTown%22%3A%22Ottawa%22%2C%22stateOrProvince%22%3A%22ON%22%2C%22postalOrZipCode%22%3A%22K1Y%202Y4%22%2C%22countryCode%22%3A%22CA%22%2C%22isValidated%22%3Afalse%7D%2C%22phone%22%3A%22613-722-9589%22%2C%22nickname%22%3A%22Ottawa%2C%20Wellington%20St%20W%22%2C%22curbsidePickUp%22%3Atrue%2C%22schedulingStore%22%3A%22%22%2C%22openToday%22%3A%229%3A00AM-9%3A00PM%22%7D; bm_mi=9628935E88C74467A8F2E0D87F701655~+qEzlPq4EzhBwwa6CTBnLTkPFx8Pa9IHG65qPnMsqeuKmQMoLcqEd6O76gqWvwxuww3+f89i+30koJYbu2+UhCurWTGsvmp2EdFxx4nKlaL52aAUkW5Sp/yGScyESHVb0Jh3CHTmxeNHXIuxwc8ajTT/B6O7+FsGsM4ctj0lXlzNZX7amlSzEjeC7MU9oaNiQ/MMdnOpg6g4W0UqdySbkQrT4GZN3lAzjp91BQieWEuPNhB1Uy7jjVZXfeesP5Q3; _mzvt=6YFRAw_XnkSxce_SDFUZuw; _mzPc=eyJjb3JyZWxhdGlvbklkIjoiNDZiYjZlMTIzZWFiNDM1ZWE3Y2ZiNjlhNTE0ZWYzYTkiLCJpcEFkZHJlc3MiOiIxODQuMTQ2LjEuMjE0IiwiaXNEZWJ1Z01vZGUiOmZhbHNlLCJpc0NyYXdsZXIiOmZhbHNlLCJpc01vYmlsZSI6ZmFsc2UsImlzVGFibGV0IjpmYWxzZSwiaXNEZXNrdG9wIjp0cnVlLCJ2aXNpdCI6eyJ2aXNpdElkIjoiNllGUkF3X1hua1N4Y2VfU0RGVVp1dyIsInZpc2l0b3JJZCI6IjFfSGVwZFp6NFV5d2FfR2JJd1pnVXciLCJpc1RyYWNrZWQiOmZhbHNlLCJpc1VzZXJUcmFja2VkIjpmYWxzZX0sInVzZXIiOnsiaXNBdXRoZW50aWNhdGVkIjpmYWxzZSwidXNlcklkIjoiZjU5OTJjOThkNzJjNGVlZDgyOGVmMjQwNjkzMDY5NGQiLCJmaXJzdE5hbWUiOiIiLCJsYXN0TmFtZSI6IiIsImVtYWlsIjoiIiwiaXNBbm9ueW1vdXMiOnRydWUsImJlaGF2aW9ycyI6WzEwMTRdLCJpc1NhbGVzUmVwIjpmYWxzZX0sInVzZXJQcm9maWxlIjp7InVzZXJJZCI6ImY1OTkyYzk4ZDcyYzRlZWQ4MjhlZjI0MDY5MzA2OTRkIiwiZmlyc3ROYW1lIjoiIiwibGFzdE5hbWUiOiIiLCJlbWFpbEFkZHJlc3MiOiIiLCJ1c2VyTmFtZSI6IiJ9LCJwdXJjaGFzZUxvY2F0aW9uIjp7ImNvZGUiOiIxMCJ9LCJpc0VkaXRNb2RlIjpmYWxzZSwiaXNBZG1pbk1vZGUiOmZhbHNlLCJub3ciOiIyMDIxLTA4LTAzVDE4OjQ3OjIzLjE4ODY1NloiLCJjcmF3bGVySW5mbyI6eyJpc0NyYXdsZXIiOmZhbHNlfSwiY3VycmVuY3lSYXRlSW5mbyI6e319; bm_sv=0F32C269411FAC1F7F9913277D761C36~zHoObQqkf3oNljg4AKynQPHWsqmk+aElUGwC2wqkMvuFAAvFyppmn2S/zMFsSmofBV5duSCcTUlxadPFs+V8/XPvXwFI0x+MqIyE3EkPAw5EbtM6EIRNijdKBnugTzns42JR9IFAevKGbJFUXPxV6nFsg9R1aSiLb3c8GB3FQ/Y="
  
  gt_resp <- httr::GET(url,
                       httr::add_headers(.headers = c("cookie" = header_cookie)),
                       httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/92.0.4515.131 Safari/537.36")
                       )

  gt <- gt_resp %>%
    httr::content(encoding = "UTF-8")
  
  gtt <- gt$items
  
  gttib <- tibble(gt = gt$items)
  
  gt <- gttib %>%
    #unnest_wider(col = gt)
    hoist(gt,
          name = "name", 
          a1 = list("address", 1L),
          a2 = list("address", 2L),
          a3 = list("address", 3L),
          a4 = list("address", 4L),
          phone = "phone",
          geo1 = list("geo", 1L),
          geo2 = list("geo", 2L)
    ) %>%
    # location_type and attributes aren't interesting
          #location_type = "locationTypes",
          #attributes = "attributes") %>%
    #mutate(a = purrr::map_chr(attributes, function(x) unlist(x) %>% as.character() %>% stringr::str_flatten(collapse= ", ")))
  mutate(address = sprintf("%s, %s, %s, %s", a1, a2, a3, a4),
         name = "Giant Tiger") %>%
    select(name, address, phone, lat = geo1, lng = geo2)
    
  
  gt %>%
    write_csv("data/convenience/giant_tiger.csv") %>%
    return()
  
  }


## DOLLARAMA

scrape_dollarama <- function(){
  url <- "https://www.dollarama.com/en-CA/locations/GetDataByCoordinates?longitude=-75.64942669999999&latitude=45.3876404&distance=10&units=kilometers&amenities=&paymentMethods="
  
  req_body= list(
    longitude = -75.64942669999999,
    latitude = 45.3876404,
    distance = 10,
    units = "kilometers"
  )
  dollarama_resp <- httr::POST(url,
             body = req_body,
             encoding = "form",
             httr::add_headers(.headers = c(accept = "json")))
  
  dol_data <- dollarama_resp %>%
    httr::content() %>%
    rvest::html_text2() %>%
    jsonlite::fromJSON()
  
 df <- dol_data$StoreLocations %>%
    as_tibble() %>%
   mutate(phone = ExtraData$Phone,
          address = sprintf("%s, %s, %s, %s",
                            ExtraData$Address$AddressNonStruct_Line1,
                            ExtraData$Address$Locality,
                            ExtraData$Address$Region,
                            ExtraData$Address$PostalCode),
          location = Location$coordinates) %>%
   hoist(location, 
         lat = 2L,
         lng = 1L) %>%
   select(name = Name,
          address,
          phone,
          lat,
          lng)
 
  df %>%
    write_csv("data/convenience/dollarama.csv") %>%
    return()

}


## DOLLAR TREE

scrape_dollar_tree <- function() {
  url <- "https://hosted.where2getit.com/dollartreeca/ajax?&xml_request=%3Crequest%3E%3Cappkey%3E411A7C8E-6139-11E4-96F6-36D9F48ECC77%3C%2Fappkey%3E%3Cformdata+id%3D%22locatorsearch%22%3E%3Cdataview%3Estore_default%3C%2Fdataview%3E%3Climit%3E250%3C%2Flimit%3E%3Cgeolocs%3E%3Cgeoloc%3E%3Caddressline%3Eottawa+on%3C%2Faddressline%3E%3Clongitude%3E-75.69719309999999%3C%2Flongitude%3E%3Clatitude%3E45.4215296%3C%2Flatitude%3E%3Ccountry%3ECA%3C%2Fcountry%3E%3C%2Fgeoloc%3E%3C%2Fgeolocs%3E%3Csearchradius%3E100%3C%2Fsearchradius%3E%3Cradiusuom%3Ekm%3C%2Fradiusuom%3E%3Cwhere%3E%3Ccountry%3E%3Ceq%3ECA%3C%2Feq%3E%3C%2Fcountry%3E%3C%2Fwhere%3E%3C%2Fformdata%3E%3C%2Frequest%3E"
  
  resp <- httr::GET(url)
  
  dt <- httr::content(resp, encoding = "UTF-8", type = "text/xml")
  
  t <- xml2::as_list(dt)
  t <- t$response$collection  
  dt_data <- t %>% enframe() %>%
    select(-name) %>%
    unnest_wider(value) %>%
    mutate(across(everything(), purrr::map, unlist),
           across(everything(), as.character),
           address = sprintf("%s, %s, %s, %s, %s", address1, address2, city, province, postalcode)) %>%
    select(name, address, phone,
           lat = latitude,
           lng = longitude)

  dt_data %>%
    write_csv("data/convenience/dollar_tree.csv") %>%
    return()

}


#### CANADIAN TIRE GAS BARS

scrape_cdn_tire_gas <- function(){
  url <- "https://api-triangle.canadiantire.ca/dss/services/v5/stores?lang=en&radius=100&maxCount=50&includeServicesData=false&lat=45.37804389999999&lng=-75.6693961&storeType=gas"
  
  cdn_tire_headers = c(
    "accept"= "application/json, text/javascript, */*; q=0.01",
    "accept-language"= "en-CA,en-GB;q=0.9,en-US;q=0.8,en;q=0.7",
    "sec-ch-ua"= '\"Chromium\";v=\"92\", \" Not A;Brand\";v=\"99\", \"Google Chrome\";v=\"92\"',
    "sec-ch-ua-mobile"= "?0",
    "sec-fetch-dest"= "empty",
    "sec-fetch-mode"= "cors",
    "sec-fetch-site"= "same-site",
    "service-client"= "ctr/web",
    "x-web-host"= "www.canadiantire.ca",
    "cookie"= "_gcl_au=1.1.754869151.1627068244; RES_TRACKINGID=707753404266999; optimizelyEndUserId=oeu1627068244609r0.41240623344667315; CTC.storeId=0210; _scid=c7eb6f73-8ed9-431a-8cf8-aad3f8cf3260; QuantumMetricUserID=9c5774d87643aaa5723f6b53672836e0; gig_bootstrap_3_yPFp0TgmzQoI1SesoI8MktIItgxzscvLQuILJVcxyJGcHDdt8qizMpw-NFNE8BRJ=gigya_ver4; BVBRANDID=2d070762-eee7-4a40-ae86-a5edac845281; cd_user_id=17ad4d32c060-0b31d88c508f77-6373260-1fa400-17ad4d32c071025; _fbp=fb.1.1627068247123.576002263; _pin_unauth=dWlkPVlqRTJPR00xWVRFdE1qWmxNaTAwTmpCaExXRTBOREV0WXpGaE5UWTFZMk0wT1Roag; _ga_5ET5MN566T=GS1.1.1628010683.1.0.1628010689.0; RES_TRACKINGID_SERVER=707753404266999; QuantumMetricSessionID=fa40a21723711a4c1d440f51b4aee64d; _gid=GA1.2.1835039562.1628010691; _sctr=1|1627963200000; CTC.postalCode=K1V%206B2; at_check=true; ak_bmsc=C77FB2D5464173DA49D68805A29A5582~000000000000000000000000000000~YAAQvRTorL1DXAZ7AQAAHw4gDQwi5WAxCug71Nk0wzNLjhdMZLs5CmFllDfc1C0V1f0SJKhPeDCiyTFbE6iJfCBGJWFp9CgkGFaBAIsARpG0R4+iRV5vSOhofEFDhBMJFA1Ud7moEjJGcv7iZeEfR2M9XvwVg3Mx94JWP8JLv5lpQA0x6sDyy/yb7oSgSQueP7G1+AHqt1V6kKHKSyS/wCk9VqXsdIj2SJvDAiy33difcW1x27ZpJpL+6g44k66IgeRI0IWzXVMUE24gA20YNweAjof5N2Q7mhjtHfuakMjXDdLQx7h2KxHVowzmafZ87bijkZ5G7jyroKn1mMMwxDolNv7fD59mgjR+qkSC/Bdo8xRDjat64IBwDzY58J6Yk/lrrOjNf87YgEN8V855JnkWWeW7vq5+oYUufEsmYI+Edlib41wcclJS3v2rfwocGcrf2dnp8P83oKr04R86m8w3FwYbKJVbfN24WuNmLNIXp2AViLOrmgUvRx4LSgVGE8oore7kfKAdZzk=; AMCV_A6C5776A5245B09C0A490D44%40AdobeOrg=-1124106680%7CMCIDTS%7C18843%7CMCMID%7C78364873170744746582360094040678629079%7CMCAAMLH-1628617610%7C7%7CMCAAMB-1628617610%7CRKhpRz8krg2tLO6pguXWp5olkAcUniQYPHaMWWgdJ3xzPWQmdj0y%7CMCOPTOUT-1628020010s%7CNONE%7CMCAID%7C3064FDE155E565EC-40000D1FE0B701DD%7CvVersion%7C5.2.0; AMCVS_A6C5776A5245B09C0A490D44%40AdobeOrg=1; IR_gbd=canadiantire.ca; BVBRANDSID=42ecc0eb-abc6-4fc8-9c63-fe9b563f186d; bm_sv=DBE61E485D96319FD354B72455AF092C~xZW212OZ3cK2HvYn+A3xLl7N0rHPUOtlLWB3H3++mjugBE6goEXHDlaa0rXTru8tPPBmj1/hPz5wJ7sLA1oCncrgMf87gA1Nok4a5Mo7Xt2hhQFt3sLNsiU3N+cp+AbE2prz9sYK2aEyTw45lzBaS2xEQEKn1tLDNIH8PJAzBiI=; _ga_S6PVRF2FWN=GS1.1.1628010690.2.1.1628012837.0; mbox=PC#ed92824df8c047b09542f41a9f0063be.34_0#1691257638|session#74cd32a052274afca44f8b0d1e8483d1#1628014671; _br_uid_2=uid%3D5279316068865%3Av%3D12.0%3Ats%3D1627068245632%3Ahc%3D4; _uetsid=d8ccda90f47d11ebb6cf15316a5ee01f; _uetvid=8b838540ebeb11eba53bb7adf3cc8199; IR_11779=1628012838571%7C2394960%7C1628012838571%7C%7C; IR_PI=8b8197d1-ebeb-11eb-bea8-4fb84f4235a7%7C1628099238571; _ga=GA1.2.465037289.1627068245"
  )
  
  #t <- rvest::read_html(url)
  resp <- httr::GET(url,
                    httr::add_headers(.headers = cdn_tire_headers),
                    httr::timeout(1),
                    httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/92.0.4515.131 Safari/537.36"))
  
  cdn_json <- resp %>%
    httr::content(type = "text/json", encoding = "UTF-8")
  
  cdn_gas <- jsonlite::fromJSON(cdn_json) %>%
    select(-workingHours) %>%
    as_tibble() %>%
    mutate(address = sprintf("%s, %s, %s, %s", storeAddress1, storeCityName, storeProvince, storePostalCode)) %>%
    mutate(name = "Canadian Tire Gas Bar")  %>%
    select(name, address, phone = storeTelephone, lat = storeLatitude, lng = storeLongitude)

  cdn_gas %>%
    write_csv("data/convenience/cdn_tire_gas.csv") %>%
    return(cdn_gas)
}


######## ESSO CONVENEINCE STORES
# A lot of Esso/Circle K locations don't show up on Circle K's find-a-store.

scrape_esso <- function(){
  
  url <- "https://www.esso.ca/en-CA/api/locator/Locations?Latitude1=44.76542558552975&Latitude2=45.73222783923223&Longitude1=-76.70147912900391&Longitude2=-74.8990346709961&DataSource=RetailGasStations&Country=CA&Customsort=False"
  esso_resp <- httr::GET(url)
  esso_json <- esso_resp %>%
    httr::content(type = "text/json", encoding = "UTF-8")
  
  esso <- esso_json %>%
    jsonlite::fromJSON()
  
  esso <- esso %>%
    as_tibble() %>%
    mutate(address = sprintf("%s, %s, %s", AddressLine1, City, StateProvince)) %>%
    select(name = DisplayName,
           address,
           lat = Latitude,
           lng = Longitude,
           FeaturedItems) %>%
    mutate(convenience_store = purrr::map_lgl(FeaturedItems, function(x) pull(x, "Name") %>% stringr::str_detect("Convenience Store") %>% any())) %>%
    select(-FeaturedItems)
  
  write_csv(esso, "data/convenience/esso.csv")
  return(esso)
}







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



# plot <- df %>%
#   filter(division_name == "ontario") %>%
#   sf::st_as_sf(coords = c("longitude", "latitude")) %>%
#   ggplot() + 
#     geom_sf() +
#     theme_minimal()
# 
# plot <- ggplot(data= tibble(x=1:10, y=(1:10)^2)) + ggiraph::geom_point_interactive(aes(x=x, y=y))
# ggiraph::ggiraph(ggobj = plot)
# 
#   plotly::ggplotly(tooltip = c("latitude", "longitude"))
# 
# df$division_name %>% unique()

# ###########
# 
# library(ggplot2)
# library(ggiraph)
# data <- mtcars
# data$carname <- row.names(data)
# 
# gg_point = ggplot(data = data) +
#   geom_point_interactive(aes(x = wt, y = qsec, color = disp,
#                              tooltip = carname, data_id = carname)) + 
#   theme_minimal()
# 
# girafe(ggobj = gg_point)

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