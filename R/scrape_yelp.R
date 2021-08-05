search_term <- "Convenience Stores"
location <- "Ottawa, ON"
location <- "Orleans, ON"

locations <- c("Orleans, ON",
               "Barrhaven, ON",
               "Nepean, ON",
               "Kanata, ON")


for (location in locations) {
  message(location)
  store_results <- scrape_yelp(search_term = "Convenience Stores",
                               location = location)
  
  city <- stringr::str_remove(location, ",.*")
  
  write_csv(store_results,
            paste0("data/convenience/yelp_raw_", city, ".csv"))
  
  message("Done. Pausing for 5 minutes.")
  
  Sys.sleep(300)
}


# now put the files together

yelp_files <- list.files(path="data/convenience/",
                         pattern = "yelp_raw.*") %>%
  paste0("data/convenience/", .)

yelp_convenience <- tibble::tibble()

for (f in yelp_files){
  stores <- read_csv(f)
  yelp_convenience <- bind_rows(yelp_convenience,
                                stores) %>%
    distinct()
}

yelp_convenience <- yelp_convenience %>%
  select(name, address = addressLines,
         categories)

write_csv(yelp_convenience,
          "data/convenience/yelp_convenience.csv")


scrape_yelp <- function(search_term, location, num_pages = 24){
  # SCRAPING YELP... AGAIN

  find_desc <- urltools::url_encode(search_term)
  find_loc <- urltools::url_encode(location)
  
  base_url <- paste0("https://www.yelp.ca/search?find_desc=",find_desc,"&find_loc=",find_loc,"&ns=1&start=")
  
  page_num <- 0
  all_urls <- NULL
  # GET ALL THE LINKS and BASIC STORE DETAILS
  
  store_details <- tibble::tibble()
  
  for (page_num in 0:(num_pages-1)) {
    message (sprintf("Page %s/%s", page_num+1, num_pages)) #paste0("Page ",page_num+1, "/24"))  
    url <- paste0(base_url, page_num*10)
    
    html <- httr::GET(url) %>%
      httr::content()
    
    # do it with more detailed data
    # get the json. we extract all the internal application/json scripts,
    # take the first one,
    # convert to html text,
    # remove leading and trailing junk,
    # and read it as json.
    json_data <-
      html %>%
      #html_elements(css = "[data-hypernova-key]")  %>%
      rvest::html_elements(css = '[type*="application/json"]') %>%
      purrr::pluck(1) %>%
      rvest::html_text() %>%
      stringr::str_remove_all("<!--|-->") %>%
      jsonlite::fromJSON()
    
    # then we root out the useful stuff
    hover_card_data <- json_data$legacyProps$searchAppProps$searchPageProps$searchMapProps$hovercardData 
    
    # then we parse it
    stores <- hover_card_data %>%
      purrr::map_df( function(x) {  enframe(x) %>%
          tidyr::pivot_wider(names_from = name, values_from = value) %>%
          dplyr::mutate(categories = 
                   purrr::map(categories, 
                              function(x) {
                                purrr::pluck(x, "title") %>% 
                                  stringr::str_flatten(collapse = ", ")
                              })
          )}
      ) %>%
      dplyr::mutate(across(everything(), purrr::map, 
                    function(x) {
                      as.character(x) %>%
                        stringr::str_flatten(collapse = ", ")
                    })
      ) %>%
      tidyr::unnest(cols = everything())
    
    store_details <-
      dplyr::bind_rows(store_details, stores)
    
    # do a precautionary pause if we have more pages to scrape
    if (page_num < num_pages - 1) Sys.sleep(10)
  }
  
  return (store_details)
}


# urls <- all_urls %>%
#   paste0("https://yelp.ca", .) %>%
#   tibble::tibble(url = .) %>%
#   write_csv( "data/convenience/yelp_urls.csv")
# 
# 
# #### read the files
# 
# urls <- readr::read_csv("data/convenience/yelp_urls.csv")
# 
# all_stores <- tibble::tibble()
# store <- 1
# 
# for (store in 111:nrow(urls)){
#   message(paste0(store,"/",nrow(urls)))
#   store_html <- httr::GET(urls$url[[store]])
#   
#   test <- store_html %>%
#     httr::content(encoding = "UTF-8") %>%
#     rvest::html_element(css = ".main-content-wrap") %>%
#     as.character() 
#   #rvest::html_text2() %>%
#   
#   t <- test %>%
#     stringr::str_extract('(?<=<script type="application/ld\\+json">).*?(?=</script>)')
#   
#   store_data <- jsonlite::fromJSON(t)
#   
#   store_name <- store_data$name
#   store_phone <- store_data$telephone
#   store_address <- store_data$address %>% unlist() %>% stringr::str_flatten(collapse=", ")
#   
#   result <- tibble::tibble(name = store_name,
#                            address = store_address,
#                            phone = store_phone)
#   
#   all_stores <- bind_rows(all_stores, result)
#  
#   Sys.sleep(10)  
# }
# 
# write_csv(all_stores, "data/convenience/yelp_convenience.csv")
# 
# 
# 
# 
# t <- html %>%
#   as.character() %>%
#   #stringr::str_detect('<script type=') %>%
#   stringr::str_extract_all('(?<=script type\\=\\"application/json\\".*?\\>).*?(?=\\</script)')
# #stringr::str_extract_all('(?<=script type\\=\\"application/ld\\+json\\").*')
# 
# 
# t %>% 
#   stringr::str_detect("json")
# 
# t <-
#   html %>%
#   #  html_elements("script") %>%
#   # html_elements(css = '[type*="json"]') %>%
#   # html_elements(css = "[data-hypernova-key]") %>%
#   html_elements(css = '[type*="json"]') %>%
#   html_text()
# 
# json_data <-
#   html %>%
#   #html_elements(css = "[data-hypernova-key]")  %>%
#   html_elements(css = '[type*="application/json"]') %>%
#   pluck(1) %>%
#   html_text() %>%
#   stringr::str_remove_all("<!--|-->") %>%
#   jsonlite::fromJSON()
#   
#   html_text()
# 
# tt <- t[[2]]
# 
# ttt <- tt %>%
#   stringr::str_remove_all("<!--|-->") %>%
#   jsonlite::fromJSON()
# 
# 
# tttt <- ttt$legacyProps$searchAppProps$searchPageProps$mainContentComponentsListProps$searchResultBusiness %>% as_tibble()
# 
# 
# hover_card_data <- ttt$legacyProps$searchAppProps$searchPageProps$searchMapProps$hovercardData 
# 
# tttt[[1]] %>%
#   enframe() %>%
#   pivot_wider(names_from = name, values_from = value) %>%
#   mutate(across(everything(), purrr::map, function(x) {as.character(x) %>%
#       stringr::str_flatten(collapse = "\\n")})) %>%
#   unnest(cols = everything())
# # 
# # 
# # stores <- tttt %>%
# #   purrr::map_df( function(x) {  enframe(x) %>%
# #       pivot_wider(names_from = name, values_from = value) %>%
# #       mutate(across(where(is.data.frame), function(x) {pull(title) %>% stringr::str_flatten(collapse = ", ")})) %>%
# #       mutate(across(everything(), purrr::map, function(x) {as.character(x) %>%
# #           stringr::str_flatten(collapse = "\\n")})) %>%
# #       unnest(cols = everything())})
# 
# 
# stores <- tttt %>%
#   purrr::map_df( function(x) {  enframe(x) %>%
#       pivot_wider(names_from = name, values_from = value) %>%
#       mutate(categories = 
#                purrr::map(categories, 
#                           function(x) {
#                             purrr::pluck(x, "title") %>% 
#                               stringr::str_flatten(collapse = ", ")
#                           })
#       )}
#   ) %>%
#   mutate(across(everything(), purrr::map, 
#                 function(x) {
#                   as.character(x) %>%
#                     stringr::str_flatten(collapse = ", ")
#                 })
#   ) %>%
#   unnest(cols = everything())
