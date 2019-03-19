library(readr)
library(leaflet)
library(lubridate)
library(shiny)
library(sf)

ui <- fluidPage(
  titlePanel(paste0("Simulation results as of ", month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today()))),
  leafletOutput("forecastmap", height = 800)
)

server <- function(input, output) {
  community_shapes <- read_rds("community_shapes.rds")
  province_shapes <- read_rds("province_shapes.rds")
  
  output$forecastmap <- renderLeaflet({
    leaflet() %>%
      addPolygons(data = community_shapes, weight = 1, opacity = 1, color = "#666666", fillOpacity = 1, fillColor = ~color, label = ~community_name, 
                  popup = ~community_info, group = "Seats (community)") %>%
      addPolygons(data = province_shapes, weight = 1, opacity = 1, color = "#666666", fillOpacity = 1, fillColor = ~color, label = ~province_name,
                  popup = ~province_info, group = "Vote (province)") %>%
      addLayersControl(
        baseGroups = c("Seats (community)", "Vote (province)"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = TRUE)
      )
  })
}

shinyApp(ui = ui, server = server)