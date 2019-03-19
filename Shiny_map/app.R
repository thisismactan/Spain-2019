library(readr)
library(leaflet)
library(lubridate)
library(shiny)
library(sf)

ui <- fluidPage(
  titlePanel(paste0("Forecast as of ", month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today()))),
  leafletOutput("forecastmap", height = 800)
)

server <- function(input, output) {
  community_shapes <- read_rds("community_shapes.rds")
  
  output$forecastmap <- renderLeaflet({
    leaflet(community_shapes) %>%
      addPolygons(weight = 1, opacity = 1, color = "#666666", fillOpacity = 1, fillColor = ~color, label = ~community_name, popup = ~community_info, 
                  group = "Seats")
  })
}

shinyApp(ui = ui, server = server)