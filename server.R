library(shiny)
library(leaflet)
library(geojsonio)
library(maps)
library(mapdata)

### MISC FUNCTIONS ----
#source("function1.R")
#source("function2.R")
#source("function3.R")

### SERVER FUNCTIONS ----
server <- function(input, output, session){

  ##### MODULE 1 - DATA CLEANING #####
  sites <- data_long
  output$map  <- renderLeaflet({
    leaflet(sites) %>% 
      addTiles() %>% 
      addCircles(lng = ~Longitude, lat = ~Latitude, weight = 1, radius = ~accident_rate * 30, popup = ~States, fillOpacity = 0.5)
  })
}

