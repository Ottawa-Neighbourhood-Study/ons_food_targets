library(tidyverse)
library(httr)
library(jsonlite)

###############################################
# Code to download all Zomato restaurants in Ottawa. I thought there were ~2300 but it only gave me 1528.

# GOT THESE FROM A SESSION
headers <- c(
  "method"="GET",
  "authority"="www.zomato.com",
  "scheme"="https",
  "path"="/ottawa/dine-out?offset=20&count=10&zpwa=true",
  #"sec-ch-ua"="`"Google Chrome`";v=`"89`", `"Chromium`";v=`"89`", `";Not\`"A\\Brand`";v=`"99`""
  "x-requested-with"="XMLHttpRequest",
  "sec-ch-ua-mobile"="?1",
  "user-agent"="Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.82 Mobile Safari/537.36",
  "accept"="*/*",
  "sec-fetch-site"="same-origin",
  "sec-fetch-mode"="cors",
  "sec-fetch-dest"="empty",
  "referer"="https://www.zomato.com/",
  "accept-encoding"="gzip, deflate, br",
  "accept-language"="en-CA,en-GB;q=0.9,en-US;q=0.8,en;q=0.7",
  "cookie"="PHPSESSID=1d2b82c59ff771cbb9f992dd19b1d247; csrf=a08bbf66f015c510d0f8a8d2ae0d4967; fbcity=295; fre=0; rd=1380000; zl=en; fbtrack=6529297610f7767f5af6a3cbd67a8c8a; ltv=295; lty=city; locus=%7B%22addressId%22%3A0%2C%22lat%22%3A45.4015274028%2C%22lng%22%3A-75.6941727407%2C%22cityId%22%3A295%2C%22ltv%22%3A295%2C%22lty%22%3A%22city%22%2C%22fetchFromGoogle%22%3Afalse%2C%22dszId%22%3A0%2C%22fen%22%3A%22Ottawa%22%7D; _ga=GA1.2.11841754.1615062974; _gid=GA1.2.798829507.1615062974; _gcl_au=1.1.1907361384.1615062974; _fbp=fb.1.1615062974993.773650579; expab=1; session_id=f6c5f62999695-bb7c-420c-8632-6a7b968bb64d; dpr=2; gsc1=0; _gat_country=1; _gat_city=1; _gat_global=1; cto_bundle=zqSTsl9VQ0d5Sm5aJTJGQUtuUFJ2SUNkUUF5YVNRbW51JTJGMHZuQjhQQmJucnZQaHN0MlkwWHN0TkJsdHdwenExcG9iOERXQ2ZQSTllN01tJTJCbHlaOERnNXNDMzJnR3Z2Rjk3cXZWREMlMkI1Mk11TG9YNkJUVFpnZk5adTBsbGlYUlclMkZnR2pydHBBM1FvMnlJdiUyRnE1ZDZ2OEZKeGNhTnclM0QlM0Q; AWSALBTG=8AFta9uaotO9HpHvn0C8Iyc1pSQKMS/3wy6r8lAXrkXwLeHJRg823v6bNCbFqaOmlMPWyvFOvtPe9bghXtCE275r0g8ZgmF+XaN5if2wCIH5TcRFXz1D5uWl8fNnabDTHgKKUkSW6ak204XY5ChWfHaHS3phubDudmJd8Sqk6gaV; AWSALBTGCORS=8AFta9uaotO9HpHvn0C8Iyc1pSQKMS/3wy6r8lAXrkXwLeHJRg823v6bNCbFqaOmlMPWyvFOvtPe9bghXtCE275r0g8ZgmF+XaN5if2wCIH5TcRFXz1D5uWl8fNnabDTHgKKUkSW6ak204XY5ChWfHaHS3phubDudmJd8Sqk6gaV"

)


pause_secs <- 15
total_num <- 2300
batch_size <- 50
num_batches <- ceiling(total_num / batch_size) 

raw_content <- vector("list", length = num_batches)

for (batch in 3:num_batches){
  message(paste0(batch,"/",num_batches,". Estimated time remaining: ", (num_batches-batch + 1) * pause_secs / 60 , " minutes."))
  offset = (batch - 1) * batch_size
  
  url <- paste0("https://www.zomato.com/ottawa/dine-out?offset=",offset,"&count=",batch_size,"&zpwa=true")
  
  zomato_resp <- httr::GET(url = url,
                           add_headers(headers))
  
  zomato_content <- zomato_resp %>%
    httr::content(type = "text/json", encoding = "UTF-8") %>%
    jsonlite::fromJSON()
  
  # store the raw content for processing
  raw_content[[batch]] <- zomato_content
  
  # wait politely
  Sys.sleep(pause_secs)
}

save(raw_content, file = "data/raw_content.Rda")


############################################################
############## NOW PROCESS THE RAW CONTENT INTO NESTED TIBBLE

nested_content <- tibble(nested = vector("list",length= 46))

for (i in 1:46){
  raw_content[[i-1]]$response$pageData$restaurants
message(i)
# cuisines as nested tibble
cuisines <- raw_content[[i]]$response$pageData$restaurants$cuisines %>% #zomato_content$response$pageData$restaurants$cuisines %>%
  enframe(name = "rowid") %>% 
  unnest(value) %>%
  group_by(rowid) %>%
  select(cuisines = name) %>%
  nest(cuisines = cuisines) %>%
  ungroup() 

# extract and combine all resto info
resto_info <- raw_content[[i]]$response$pageData$restaurants$basic_info %>% #zomato_content$response$pageData$restaurants$basic_info %>%
  as_tibble() %>%
  select(name, 
         restaurant_address,
         subzone_name,
         website,
         serves_veg_flag,
         pure_veg_flag,
         temp_closed_flag,
         is_seating_available,
         phoneData,
         geolocation_data) %>%
  mutate(lat = pluck(geolocation_data, "lat"),
         lon = pluck(geolocation_data, "lon")) %>%
  select(-geolocation_data) %>%
  mutate(phone = pluck(phoneData, "mobile_string")) %>%
  select(-phoneData) %>%
  rowid_to_column() %>%
  left_join(cuisines)


nested_content$nested[[i]] <- resto_info
}

###########################################################
## NOW PUT THE DATA INTO A TIDY FORMAT AND SAVE IT TO CSV
results <- unnest(nested_content, nested)

# put the cuisines into a very lonnnnng format for saving in csv, since list-cols aren't okay for csv
results %>%
  mutate(cuisines = purrr::map(cuisines, purrr::map, function(x) str_replace(tolower(x), pattern = " ", replacement = "_"))) %>%
  unnest(cuisines) %>%
  unnest(cuisines) %>%
  pivot_wider(names_from = cuisines, names_prefix = "cuisine_", values_from = 1, values_fn = length, values_fill = 0) %>%
  write_csv("data/restaurants/zomato_restaurants.csv")
# 
# # testing extracting cookies from GET response for next GET
# cookies <- zomato_resp$cookies %>% as_tibble()
# cookie <- cookies %>%
#   mutate(text = paste0(name,"=",value)) %>%
#   pull(text) %>%
#   str_flatten(collapse = "; ")
# 
# 
# bigone <- list(test_get, zomato_resp)
# 
# 
# 
# 
# 
# 
# 
# # if desired: pivot cuisines to wider format
# resto_info %>%
#   unnest(cuisines) %>%
#   pivot_wider(names_from = cuisines, values_from = 1)

