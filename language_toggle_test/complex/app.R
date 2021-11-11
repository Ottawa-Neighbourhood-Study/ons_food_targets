#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shiny.i18n)
library(leaflet)


i18n <- shiny.i18n::Translator$new(translation_csvs_path = "translations")
message(i18n$get_languages())
i18n$set_translation_language("en")




# Define UI for application that draws a histogram
ui <- shinydashboard::dashboardPage(
        
    shinydashboard::dashboardHeader(title = i18n$t("Find a Physician"),
                                    # language selector
                                    tags$li(class = "dropdown",     
                                            tags$li(class = "dropdown", actionLink("lang_change_button", label = textOutput("lang_now_output")))
                                    ),
                                    tags$li(class = "dropdown", shiny.i18n::usei18n(i18n))),
    
    shinydashboard::dashboardSidebar(
        div(i18n$t("Placeholder for sidebar content."))
    ),
    
    shinydashboard::dashboardBody(
        shiny.i18n::usei18n(i18n),
        div(i18n$t("Placeholder for main content.")),
        
        div(leaflet::leafletOutput("ottawa_map"))
    ),
    
    title = "Find a Physician / Trouvez un médecin"
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    lang_values <- reactiveValues(lang_now = "en",
                               lang_button_text = "FR")
    
    # toggle language based on button
    observeEvent(input$lang_change_button, {
        if (lang_values$lang_now == "en"){
            lang_values$lang_now <- "fr"
            lang_values$lang_button_text <- "EN"
        }  else {
            lang_values$lang_now <- "en"
            lang_values$lang_button_text <- "FR"
        }
        message(lang_values$lang_now)
        
        shiny.i18n::update_lang(session, lang_values$lang_now)
    })
    
    output$lang_now_output <- renderText(lang_values$lang_button_text)
    
    # create initial map
    output$ottawa_map <- leaflet::renderLeaflet(
        leaflet() %>%
            addTiles() %>%
            addMarkers(lat = 45.44595221684578, lng = -75.63931179246401, label = i18n$t("Montfort Hospital"))
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
