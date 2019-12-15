library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(dygraphs)
library(leaflet)

#### ui.R ####
### HEADER ----
header <- dashboardHeader(
  title = "Malaysia Road Safety Analysis",
  titleWidth = 350
)

### SIDEBAR ----
sidebar <- dashboardSidebar(
  width = 350,
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Integrator", tabName = "integrator", icon = icon("th"))
  )
)

### BODY ----
body <-  dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
            fluidRow(
              infoBox("Factbox A", 10, width = 3, icon = icon("credit-card"), color = "teal", fill = TRUE),
              infoBox("Factbox B", 20, width = 3, icon = icon("credit-card"), color = "teal", fill = TRUE),
              infoBox("Factbox C", 30, width = 3, icon = icon("credit-card"), color = "teal", fill = TRUE),
              infoBox("Factbox D", 40, width = 3, icon = icon("credit-card"), color = "teal", fill = TRUE),
            ),
            fluidRow(
              box(width = 8,
                  plotlyOutput("asean_map")),
              box(width = 4,
                  plotlyOutput("asean_pop"))
            ),
            fluidRow(
              box(width = 4,
                  plotlyOutput("state_decade")),
              box(width = 4,
                  plotOutput("fatality_age")),
              box(width = 4,
                  plotlyOutput("fatality_user"))
            )
    ),
    
    # Second tab content
    tabItem(tabName = "integrator",
            h2("WHO Map")
    )
  )
)


ui <- fluidPage()

### ui ----
ui <- dashboardPage(skin = "yellow",
                    header,
                    sidebar,
                    body
)

#### server.R ####
### MISC FUNCTIONS ----
source("data_extraction_final.R")
source("data_visualisation_final.R")
#source("function3.R")

### SERVER FUNCTIONS ----
server <- function(input, output, session){
  
  ##### MODULE 1 - DASHBOARD #####
  output$plot1 <- renderPlot({
    death_percent <- group_by(death_by_category, year) %>% mutate(percent = accident_rate/sum(accident_rate)*100)
    hist(death_percent)
  })
  output$asean_map <- renderPlotly({
    asean_map(asean_info)
  })
  output$asean_pop <- renderPlotly({
    asean_pop_bar(new_asean_data)
  })
  output$state_decade <- renderPlotly({
    state_decade_tornado(state_decade_long)
  })
  output$fatality_age <- renderPlot({
    fatalilty_age_heatmap(fatal_age)
  })
  output$fatality_user <- renderPlotly({
    fatalilty_user_bar(accident_data)
  })
}

#### app.R ####
shinyApp(ui = ui, server = server)
