

# SCRAPING YELP... AGAIN
search_term <- "Convenience%20Stores"

base_url <- paste0("https://www.yelp.ca/search?find_desc=",search_term,"&find_loc=Ottawa%2C%20ON&ns=1&start=")

page_num <- 0
all_urls <- NULL
# GET ALL THE LINKS
for (page_num in 0:23) {
  message (paste0("Page ",page_num+1, "/24"))  
  url <- paste0(base_url, page_num*10)
  
  html <- httr::GET(url)
  
  stores <- html %>%
    httr::content() %>%
    rvest::html_elements(css = ".css-1pxmz4g .css-166la90")
  
  store_urls <- stores %>%
    rvest::html_attr("href")
  
  all_urls <- c(all_urls, store_urls)
  
  Sys.sleep(10)
}

urls <- all_urls %>%
  paste0("https://yelp.ca", .) %>%
  tibble::tibble(url = .) %>%
  write_csv( "data/convenience/yelp_urls.csv")


#### read the files

urls <- readr::read_csv("data/convenience/yelp_urls.csv")

all_stores <- tibble::tibble()
store <- 1

for (store in 111:nrow(urls)){
  message(paste0(store,"/",nrow(urls)))
  store_html <- httr::GET(urls$url[[store]])
  
  test <- store_html %>%
    httr::content(encoding = "UTF-8") %>%
    rvest::html_element(css = ".main-content-wrap") %>%
    as.character() 
  #rvest::html_text2() %>%
  
  t <- test %>%
    stringr::str_extract('(?<=<script type="application/ld\\+json">).*?(?=</script>)')
  
  store_data <- jsonlite::fromJSON(t)
  
  store_name <- store_data$name
  store_phone <- store_data$telephone
  store_address <- store_data$address %>% unlist() %>% stringr::str_flatten(collapse=", ")
  
  result <- tibble::tibble(name = store_name,
                           address = store_address,
                           phone = store_phone)
  
  all_stores <- bind_rows(all_stores, result)

  Sys.sleep(10)  
}

write_csv(all_stores, "data/convenience/yelp_convenience.csv")
