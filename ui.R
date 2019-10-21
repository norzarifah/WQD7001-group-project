library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(dygraphs)
library(leaflet)

### HEADER ----
header <- dashboardHeader(
  title = "OPS SIKAP ANALYSIS"
)

### SIDEBAR ----
sidebar <- dashboardSidebar(
  collapsed = FALSE,
  sidebarMenu(
    menuItem("Overview", tabName = "sidebar_overview", icon = icon("eye-open", lib="glyphicon")),
    menuItem("Tab1", icon = icon("table"), startExpanded = FALSE,
             menuSubItem("MenuTab1.1", icon = icon("eye"), tabName = "sidebar_datablablabla"),
             menuSubItem("MenuTab1.2", icon = icon("eye"), tabName = "sidebar_dataqualityanalysis")),
    menuItem("Tab2", tabname = "sidebar_statisticalanalytics", icon = icon("sort-by-attributes-alt", lib="glyphicon"))
  )
)

### BODY ----
body <- dashboardBody(
  tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
  leafletOutput("map"),
  absolutePanel(top = 10, right = 10,
                style="z-index:500;", # legend over my map (map z = 400)
                tags$h3("map") 
                )
)

### ui ----
ui <- dashboardPage(
  header,
  sidebar,
  body
)

