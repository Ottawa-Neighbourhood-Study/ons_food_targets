#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(leaflet)
library(janitor)
library(DT)

files_to_load <- list.files("../data/restaurants")



restos <- purrr::map_dfr(paste0("../data/restaurants/", files_to_load), function(x) {
    readr::read_csv(x) %>%
        mutate(across(everything(), as.character))}) %>%
    select(name, address, lat, lon) %>%
    distinct() %>%
    drop_na(lat, lon) %>%
    mutate(category2 = "Restaurant", category3 = "Restaurant",
           lat = as.numeric(lat),
           lng = as.numeric(lon)) %>%
    select(-lon)

ons_shp <- onsr::get_ons_shp()

food <- read_csv("../data/combined/foodspace_2021-08-26c.csv") %>%
    select(rowid, name, address, category2, category3, Nbhd, lat, lng) %>%
    mutate(Nbhd = if_else(is.na(Nbhd), "(None)", Nbhd)) %>%
    arrange(name) #%>% mutate(in_ott = !is.na(Nbhd))


food <- bind_rows(food, restos)

# consider function
cat2 <- sort(unique(food$category2))
category2_all <- setNames(cat2, janitor::make_clean_names(cat2, case = "title")) %>% 
    c("(All)" = "All", .)

cat3 <- sort(unique(food$category3))
category3_all <- setNames(cat3, janitor::make_clean_names(cat3, case = "title")) %>% 
    c("(All)" = "All", .)

# for setting the choices, the two-valued possibilities
categories <- select(food, category2, category3) %>%
    distinct()

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Food"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("category2",
                        "Category",
                        choices = category2_all,
                        selected = "All"),
            
            selectInput("category3",
                        "Sub-Category",
                        choices = category3_all,
                        selected = "All"),
            
            checkboxInput("ons_map_check",
                          "Show ONS Boundaries",
                          value = TRUE)
            , textInput("search_text",
                      label = "Search:")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Map & Table",
                         leaflet::leafletOutput("foodmap"),
                         DT::dataTableOutput("foodtable")
                         
                ),
                
                tabPanel("Biggest Chains",
                         plotly::plotlyOutput("biggest_chains_plot")),
                
                tabPanel("Sub-Category Proportions",
                         plotly::plotlyOutput("food_plot"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$biggest_chains_plot <- plotly::renderPlotly({
        data <- food_react() %>%
            group_by(name) %>%
            dplyr::summarise(n = n()) %>%
            arrange(desc(n)) %>%
            slice_head(n=15)
        
        forplot <- data  %>%
            ggplot() +
            geom_col(aes(x=reorder(name, n), y = n)) +
            theme_minimal() +
            coord_flip() +
            labs(x=NULL, y=NULL)
        
        plotly::ggplotly(forplot)
        
    })
    
    output$food_plot <- plotly::renderPlotly({
        plotly::ggplotly(ggplot(food) + 
                             geom_bar(aes(x=category2, fill = category3),  position="fill") +
                             theme_minimal() +
                             scale_y_continuous(labels = scales::label_percent()) +
                             labs(x=NULL, y=NULL,
                                  legend = "Sub-Category") +
                             coord_flip() +
                             theme(legend.position = "bottom"))
    })
    
    
    # if category2 is updated, change the choices for category3
    observeEvent(input$category2,
                 {
                     #message(input$category2)
                     
                     if (input$category2 == "All") {
                         new_choices =  c("(All)" = "All")#category3_all
                     } else {
                         new_choices <- categories %>%
                             filter(category2 == input$category2) %>%
                             pull(category3) %>%
                             setNames(janitor::make_clean_names(., case = "title")) %>%
                             sort() %>%
                             c("(All)" = "All", .)
                     }
                     
                     message(new_choices)
                     freezeReactiveValue(input, "category3")
                     updateSelectInput(session,
                                       inputId = "category3",label = "Sub-Category",choices = new_choices, selected = "All")
                     
                 })
    
    
    # set up a reactive value for our list of food items
    food_react <- reactive({
        message("Updating food_react()")
        
        if (input$category2 == "All") {
            message("Cat2 all: all food")
            output <- food
        } else {
            
            if (input$category3 == "All"){
                output <- filter(food, category2 == input$category2)
            } else {
                output <- filter(food, category2 == input$category2 & category3 == input$category3)
            }
        }
        
        output <- filter(output, stringr::str_detect(tolower(name), tolower(input$search_text)))
        
        message("  Done updating food_React()")
        print(output)
        output
    })
    
    # initial map output
    output$foodmap <- leaflet::renderLeaflet({
        food %>%
            leaflet() %>%
            addTiles() %>%
            addPolylines(data = ons_shp, weight = 2,fill = FALSE) %>%
            addMarkers(label = ~name,
                       clusterOptions = markerClusterOptions())
    })
    
    observeEvent(c(food_react()),
                 {
                     leafletProxy("foodmap") %>%
                         clearMarkers() %>%
                         clearMarkerClusters() 
                     
                     # only draw new markers if there's anything to draw
                     if (nrow(food_react()) > 0) {
                         leafletProxy("foodmap")%>%
                         addPolylines(data = ons_shp, weight = 2,fill = FALSE) %>%
                         addMarkers(data = food_react(), label = food_react()$name,
                                    clusterOptions = markerClusterOptions()
                         )
                     }
                 })
    
    output$foodtable <- DT::renderDataTable({
        DT::datatable(select(food_react(),
                             name, address, category2, category3, Nbhd),
                      rownames = FALSE,
                      extensions = "Buttons",
                      options = list(
                          dom = "Btip",
                          buttons = 
                              list('copy', 'print', list(
                                  extend = 'collection',
                                  buttons = c('csv', 'excel', 'pdf'),
                                  text = 'Download'))
                      )
        )
    })
    
    observeEvent(input$ons_map_check,
                 {
                     if (!input$ons_map_check) {
                         leafletProxy("foodmap") %>%
                             clearShapes()
                     } else {
                         leafletProxy("foodmap") %>%
                             addPolylines(data = ons_shp, weight = 2,fill = FALSE) 
                     }
                     
                 })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
