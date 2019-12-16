library(shiny)
library(shinydashboard)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = span("MyRoSe", style = "font-family: Arial, Helvetica, sans-serif; font-weight: bold; font-size: 150%;")),
  dashboardSidebar(sidebarMenu(
    menuItem("ASEAN Dashboard", tabName = "asean_dashboard", icon = icon("globe-asia")),
    menuItem("Malaysia Dashboard", tabName = "malaysia_dashboard", icon = icon("map-marker-alt")),
    menuItem("Steps to Road Safety", tabName = "safety_steps", icon = icon("list"))
  )),
  dashboardBody(
    tags$head(tags$style(HTML(".main-sidebar { font-size: 16px; }"))),
    tabItems(
      # First tab content
      tabItem(tabName = "asean_dashboard",
              fluidRow(
                box(width = 9,
                    background = "light-blue",
                    tags$p(paste("\"", "About 1.3 million people die on the world's roads and 20 - 50 million are injured every year.Road traffic crashes are a major cause of death among all age groups and the leading cause of death for children and young adults. The risk of dying in a road traffic crash is more than 3 times higher in low-income countries than in high-income countries.", "\""),
                           style = "font-size: 120%;"),
                    tags$p(em("WHO Global Status Report on Road Safety 2018"), align = 'right', style = "font-size: 120%;")),
                valueBox(value = tags$p("3rd @ ASEAN", style = "font-size: 140%;"),
                         #paste0("3", tags$sup("rd"), sep = "")
                         subtitle = tags$p("Malaysia: Road Traffic Deaths", style = "font-size: 130%;"),
                         width = 3,
                         icon = icon("car-crash"),
                         color = "yellow")),
              box(title = "Total Vehicle per '000 Population in ASEAN Countries", 
                  status = "warning",
                  width = 6,
                  plotlyOutput("map")),
              box(title = "Population vs. Population Density (per sq.km)",
                  status = "warning",
                  width = 6,
                  plotlyOutput("asean_pop")),
              box(title = "",
                  color = "black",
                  width = 12,
                  height = 150,
                  includeText("include.txt")),
              box(title = "",
                  status = "warning",
                  width = 12,
                  collapsible = TRUE,
                  DT::dataTableOutput("asean_table"))
      ),
      tabItem(tabName = "malaysia_dashboard",
              fluidRow(
                valueBox(value = tags$p("1% | 4%", style = "font-size: 150%;"),
                         subtitle = tags$p("Speed vs. Risk of Death", style = "font-size: 150%;"),
                         width = 4,
                         icon = icon("arrow-circle-up"),
                         color = "yellow"),
                valueBox(value = tags$p("10%", style = "font-size: 150%;"),
                         subtitle = tags$p("Seatbelt by Rear Passengers", style = "font-size: 150%;"),
                         width = 4,
                         icon = icon("user-shield"),
                         color = "yellow"),
                valueBox(value = tags$p("60%", style = "font-size: 150%;"),
                         subtitle = tags$p("Child Restraint vs. Risk of Death", style = "font-size: 150%;"),
                         width = 4,
                         icon = icon("arrow-circle-down"),
                         color = "yellow")
              ),
              box(title = "Road Accident Fatalities by Age Group",
                  width = 7,
                  status = "primary",
                  collapsible = TRUE,
                  solidHeader = TRUE,
                  plotlyOutput("fatality_age")),
              box(title = "Road Accident Fatalities by Types of Vehicle",
                  width = 5,
                  status = "primary", 
                  collapsible = TRUE,
                  solidHeader = TRUE,
                  plotlyOutput("fatality_user")),
              box(title = "Road Accident Fatalities by States (2008 -2017)",
                  width = 12,
                  height = 500,
                  status = "primary",
                  collapsible = TRUE,
                  solidHeader = TRUE,
                  plotlyOutput("state_decade"))
      )#,
      #tabItem(tabName = "safety_steps", includeHTML("1.html"))
    )
  )
)

server <- function(input, output) {
  output$map <- renderPlotly({
    asean_map(asean_info)
  })
  output$asean_pop <- renderPlotly({
    asean_pop_bar(asean_data)
  })
  output$state_decade <- renderPlotly({
    state_decade_tornado(state_decade_long)
  })
  output$fatality_age <- renderPlotly({
    fatalilty_age_heatmap(fatal_age)
  })
  output$fatality_user <- renderPlotly({
    fatalilty_user_bar(accident_data)
  })
  output$asean_table <- DT::renderDataTable({
    datatable(asean_data)
  })
}


shinyApp(ui, server)