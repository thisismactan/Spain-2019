library(readr)
library(leaflet)
library(lubridate)
library(shiny)
library(sf)

ui <- fluidPage(
  titlePanel(paste0("Forecast as of ", month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today()))),
  leafletOutput("forecastmap", height = 1100)
)

server <- function(input, output) {
  community_shapes <- read_rds("community_shapes.rds")
  province_shapes <- read_rds("province_shapes.rds")
  
  output$forecastmap <- renderLeaflet({
    leaflet() %>%
      addPolygons(data = community_shapes, weight = 1, opacity = 1, color = "#444444", fillOpacity = ~opacity, fillColor = ~color, label = ~community_name, 
                  popup = ~community_info, group = "Seats (community)") %>%
      addPolygons(data = province_shapes, weight = 1, opacity = 1, color = "#444444", fillOpacity = ~opacity, fillColor = ~color, label = ~province_name,
                  popup = ~province_info, group = "Vote (province)") %>%
      addTiles(options = tileOptions(opacity = 0.5, fillOpacity = 0.5)) %>%
      addLayersControl(
        baseGroups = c("Seats (community)", "Vote (province)"),
        position = "topleft",
        options = layersControlOptions(collapsed = FALSE)
      )
  })
}

shinyApp(ui = ui, server = server)