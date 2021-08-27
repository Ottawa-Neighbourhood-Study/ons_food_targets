# functions for  validation dashboard


make_table <- function(data) {
  
  reactable::reactable(data,
                       filterable = FALSE,
                       searchable = TRUE,
                       selection = "multiple",
                       onClick = "select",
                       rowStyle = list(cursor = "pointer"),
                       columns = list(
                         name = reactable::colDef(name = "Name", show = TRUE),
                         address = reactable::colDef(name = "Address", show = TRUE),
                         phone = reactable::colDef(show= FALSE),
                         update_date = reactable::colDef(show = FALSE),
                         lat = reactable::colDef(show = FALSE),
                         lng = reactable::colDef(show = FALSE),
                         note = reactable::colDef(show = FALSE),
                         chain = reactable::colDef(show = FALSE),
                         category2 = reactable::colDef(name = "Category"),
                         category3 = reactable::colDef(name = "Sub-Cat.")
                         
                       ))
}

make_map <- function(data, icon ){
  labs <-  sprintf("<b>%s</b><br>%s<br>%s", data$data()$name, data$data()$category, data$data()$address) %>%
    purrr::map(htmltools::HTML)
  
  leaflet() %>%
    addTiles() %>%
    addAwesomeMarkers(data = data,
                      icon = icon,
                      label = labs)
}

leaflet_food <- function(food){
  
  ons_shp <- onsr::get_ons_shp()
  
  
  
  circle_pal <- function(category){
    colour <- case_when(
      category == "farmer's market" ~ "yellow",
      category == "grocery" ~ "darkgreen",
      category == "convenience" ~ "blue",
      category == "specialty" ~ "red",
      TRUE ~ "grey"
    )
    
  }
  
  popups <- sprintf("<b><h3>%s</b></h3><br>
                    <i>%s: %s</i><br>
                    %s", food$data()$name, food$data()$category2, food$data()$category3, food$data()$address) %>%
    purrr::map(htmltools::HTML)
  
  
  labels <- sprintf("<b>%s</b><br>
                    <i>%s: %s</i><br>
                    %s", food$data()$name, food$data()$category2, food$data()$category3, food$data()$address) %>%
    purrr::map(htmltools::HTML)
  
  
  themap <- leaflet() %>%
    addTiles() %>%
    addPolygons(data = ons_shp,
                weight = 1,
                fill = TRUE,
                fillOpacity = 0.01,
                color = "black",
                group = "ONS Neighbourhood Boundaries",
                label = ~Name) %>%
    leaflet::addLayersControl(overlayGroups = c("ONS Neighbourhood Boundaries"),
                              options = leaflet::layersControlOptions(collapsed = FALSE)  ) %>%
    addCircleMarkers(data = foods,
                     color = ~ circle_pal(category2)
                     #  ,clusterOptions = leaflet::markerClusterOptions()  
                     , popup = popups
                     , label = labels
    )
  
  
  
  return (themap)
  
}

make_dt_table <- function(foods){
  foods %>%
    DT::datatable(
      filter = "top",  # allows filtering on each column
      extensions = c(
        "Buttons",  # add download buttons, etc
        "Scroller"  # for scrolling down the rows rather than pagination
      ),
      rownames = FALSE,  # remove rownames
      style = "bootstrap",
      class = "compact",
      width = "100%",
      editable = list(
        target = 'cell', disable = list(columns = c(0))),
      options = list(
        dom = "Blrtip",  # specify content (search box, etc)
        deferRender = TRUE,
        scrollY = 300,
        scroller = TRUE,
        columnDefs = list(
          list(
            visible = FALSE
            ,targets = c(0, 6, 7)
          )
        ), 
        buttons = list(
          I("colvis"),  # turn columns on and off
          "csv",  # download as .csv
          "excel"  # download as .xlsx
        )
      ),
      colnames = c(
        "Name" = "name",
        "Address" = "address",
        "Category" = "category2" ,
        "Sub-Cat." = "category3" 
        
      )
    )
  
}

