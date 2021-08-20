# scraping restaurants
harveys <- read_csv("data/restaurants/harveys.csv")


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
