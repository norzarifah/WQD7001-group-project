library(shiny)
library(shinydashboard)
library(geojsonio)
library(broom)
library(ggplot2)
library(mapproj)
library(forcats)
library(plyr)
library(dplyr)
library(plotly)
library(RColorBrewer)

asean_data_2017 <- read.csv("asean_data_2017.csv", header=TRUE, sep=",")
fatal_age <- read.csv("fatal_age.csv", header=TRUE, sep=",")
accident_data <- read.csv("accident_data.csv", header=TRUE, sep=",")
state_data <- read.csv("state_data.csv", header=TRUE, sep=",")
Rss <- base64enc::dataURI(file="Rss.png", mime="image/png")
Ulp <- base64enc::dataURI(file="Ulp.png", mime="image/png")

ui <- dashboardPage(
    dashboardHeader(title = span("MyRoSe", style = "font-family: Arial, Helvetica, sans-serif; font-weight: bold; font-size: 150%;")),
    dashboardSidebar(sidebarMenu(
        menuItem("Home", tabName = "asean_dashboard", icon = icon("globe-asia")),
        menuItem("Malaysia Dashboard", tabName = "malaysia_dashboard", icon = icon("map-marker-alt")),
        menuItem("Steps to Road Safety", tabName = "RSS", icon = icon("list"))
    )),
    dashboardBody(
        tags$head(tags$style(HTML(".main-sidebar { font-size: 16px; }"))),
        tabItems(
            # First tab content
            tabItem(tabName = "asean_dashboard",
                    fluidRow(
                        column(width =3,
                               box(title = tags$p("About MyRoSe", style = "font-family: Helvetica; font-weight: bold; font-size: 30px;"),
                                   width = 12,
                                   height = 350,
                                   background = "maroon",
                                   tags$p("MyRoSe (MalaYsia ROad Safety Eye) aims to provide insights and revelation on the road safety situation in Malaysia. The information provided herein are based on data collected from the following resources: ",
                                          style = "font-size: 120%;"),
                                   tags$li("ASEAN Statistical Yearbook 2018", style = " font-size: 14px;"),
                                   tags$li("Malaysia Transport Statistics Book 2018",  style = " font-size: 14px;"),
                                   tags$li("Buku Statistik Keselamatan Jalan Raya",  style = " font-size: 14px;"),
                                   align = "justify"),
                               valueBox(value = tags$p("MYS @ 3rd", style = "font-size: 130%;"),
                                        subtitle = tags$p("Road Traffic Deaths Globally", style = "font-size: 130%;"),
                                        width = 12,
                                        icon = icon("car-crash"),
                                        color = "yellow"),
                               box(width = 12,
                                   height = 350,
                                   background = "purple",
                                   tags$p(tags$em(paste("\"", "About 1.3 million people die on the world's roads and 20 - 50 million are injured every year. Road traffic crashes are a major cause of death among all age groups and the leading cause of death for children and young adults. The risk of dying in a road traffic crash is more than 3 times higher in low-income countries than in high-income countries.", "\""),
                                                  align = 'justify', style = "font-size: 120%;")),
                                   tags$br(),
                                   tags$p(em("WHO Global Status Report on Road Safety 2018"), align = 'right', style = "font-size: 100%;"))
                        ),
                        box(title = tags$b("[1] Number of Vehicle per '000 Population for ASEAN Countries in 2017"), 
                            status = "info",
                            width = 6,
                            height = 450,
                            collapsible = TRUE,
                            solidHeader = TRUE,
                            plotlyOutput("map"),
                            align = "center"),
                        box(tags$p(tags$br(),
                                   tags$b("[1] Left: "), "In 2017, the number of registered vehicles per 1,000 inhabitants for Malaysia was", 
                                   tags$b(tags$em("897 per 1,000 inhabitants.")), "In comparison to the same data collected for the year 2000 (i.e. 456 per 1,000 inhabitants), the number of registered vehicles per 1,000 inhabitants of Malaysia is", 
                                   tags$b(tags$em("growing at an average annual rate of 5.69%.")), 
                                   style = "font-size: 14px;"),
                            tags$br(),
                            tags$p(tags$b("[2] Bottom: "), "Although Malaysia had the second-highest number of registered vehicles per thousand population, the country", 
                                   tags$b(tags$em("ranked at sixth")),  "for the total number of population. ", 
                                   tags$b(tags$em("Are we really owning too many cars?")),"In fact, Malaysia has the third highest rate of car ownership in the world, with ", 
                                   tags$b(tags$em("93% of households owning a car.")), 
                                   style = "font-size: 14px;"),
                            height = 450,
                            status = "info",
                            width = 3,
                            align = "left"),
                        box(title = tags$b("[2] Population vs. Population Density (per sq.km)"),
                            status = "info",
                            width = 9,
                            height = 400,
                            collapsible = TRUE,
                            solidHeader = TRUE,
                            plotlyOutput("asean_pop"),
                            align = "center")
                    )),
            tabItem(tabName = "malaysia_dashboard",
                    fluidRow(
                        valueBox(value = tags$p("RM8.8bil", style = "font-size: 40px;"),
                                 subtitle = tags$p("Est. losses due to road accidents in 2017", style = "font-size: 130%;"),
                                 width = 3,
                                 icon = icon("comments-dollar"),
                                 color = "yellow"),
                        valueBox(value = tags$p("4% | 1%", style = "font-size: 40px;"),
                                 subtitle = tags$p("Increse risk of death pro rata with speed", style = "font-size: 130%;"),
                                 width = 3,
                                 icon = icon("arrow-circle-up"),
                                 color = "yellow"),
                        valueBox(value = tags$p("10%", style = "font-size: 40px;"),
                                 subtitle = tags$p("Rear passengers wearing seat belts", style = "font-size: 130%;"),
                                 width = 3,
                                 icon = icon("user-shield"),
                                 color = "yellow"),
                        valueBox(value = tags$p("60%", style = "font-size: 40px;"),
                                 subtitle = tags$p("Child restraints reduce risk of death", style = "font-size: 130%;"),
                                 width = 3,
                                 icon = icon("arrow-circle-down"),
                                 color = "yellow")
                    ),
                    fluidPage(
                        fluidRow(
                            column(width = 5,
                                   box(tags$p(tags$br(),
                                              tags$b("[1] Bottom: "), "Every year, road crashes in Malaysia are largely attributed to cars ", 
                                              tags$b(tags$em("(i.e. more than 50%)")), "yet, motorcycles are disproportionately accounted for traffic fatalities and injuries.", 
                                              style = "font-size: 14px;"),
                                       #tags$br(),
                                       tags$p(tags$b("[2] Top-right: "), "The number of road fatalities involving adolescents (age group of 16 - 20) were consistently the highest among Malaysians between 2007 and 2017. 
                                              Most fatalities and injuries were sustained on motorcycles where the teenagers were either riding or were pillion riders. This was followed by car collisions.", 
                                              style = "font-size: 14px;"),
                                       #tags$br(),
                                       tags$p(tags$b("[3] Bottom-right: "),
                                              tags$b(tags$em("Selangor")), "shows significant increase in the number of road accidents ",
                                              tags$b(tags$em("by 34.4%")), "over a period of ten years (2009 -2018). The number increase in proportion to urbanization.", 
                                              style = "font-size: 14px;"),
                                       height = 280,
                                       width = 12,
                                       status = "info",
                                       align = "left"),
                                   box(tags$p(tags$b("Above: "),"Number of Road Crashes from 2009 to 2018", align = 'left'),
                                       tags$p(tags$b("Below: "), "Number of Road Crash Fatalities from 2009 to 2018", align = 'left'),
                                       title = tags$b("[1] Road Accidents and Fatalities by Types of Vehicle"),
                                       height = 750,
                                       width = 12,
                                       status = "info",
                                       collapsible = TRUE,
                                       solidHeader = TRUE,
                                       plotlyOutput("fatality_user"),
                                       align = "center")
                            ),
                            column(width = 7,
                                   box(title = tags$b("[2] Road Accident Fatalities by Age Group"),
                                       width = 12,
                                       height = 530,
                                       status = "info",
                                       collapsible = TRUE,
                                       solidHeader = TRUE,
                                       plotlyOutput("fatality_age"),
                                       align = "center"),
                                   box(title = tags$b("[3] Road Accident Fatalities by States  (2009 -2018)"),
                                       width = 12,
                                       height = 500,
                                       status = "info",
                                       collapsible = TRUE,
                                       solidHeader = TRUE,
                                       plotlyOutput("state_decade"),
                                       align = "center"))
                        )
                    )
            ),
            
            tabItem(tabName = "RSS",img(src=Rss), img(src=Ulp))
        )
    )
)

#### server.R ####
### MISC FUNCTIONS ----
source("data_visualisation_final.R")

server <- function(input, output) {
    
    output$map <- renderPlotly({
        asean_map(asean_data_2017)
    })
    output$asean_pop <- renderPlotly({
        asean_pop_bar(asean_data_2017)
    })
    output$state_decade <- renderPlotly({
        state_line(state_data)
    })
    output$fatality_age <- renderPlotly({
        fatalilty_age_heatmap(fatal_age)
    })
    output$fatality_user <- renderPlotly({
        fatalilty_user_bar(accident_data)
    })
    
}

#### app.R ####
shinyApp(ui = ui, server = server)



