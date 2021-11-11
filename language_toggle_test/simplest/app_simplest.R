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

i18n <- shiny.i18n::Translator$new(translation_csvs_path = "translations")
message(i18n$get_languages())
i18n$set_translation_language("fr")




# Define UI for application that draws a histogram
ui <- fluidPage(
  i18n$t("Find a Physician")
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
}

# Run the application 
shinyApp(ui = ui, server = server)
