#### Objective: Creating plots ------------

## Load the required libraries ------------
library(plotly)
library(geojsonio)
library(broom)
library(ggplot2)
library(mapproj)
library(plyr)
library(dplyr)


## Plot 1: ASEAN map (of population number and population density/ # of motor vehicles) ----
asean_map <- function(asean_data_2017) {
  
  trace1 <- list(
    geo = "geo", 
    type = "choropleth", 
    z = asean_data_2017$total_vehicle, 
    showscale = TRUE, 
    locationmode = "country names", 
    locations = asean_data_2017$country, 
    colorscale = "Portland",
    autocolorscale = FALSE
  )
  
  data <- list(trace1)
  
  layout <- list(
    geo = list(
      scope = 'asia', 
      lonaxis = list(range = c(90, 150)),
      lataxis = list(range = c(-15, 30)), 
      showland = TRUE, 
      landcolor = "rgb(229, 229, 229)", 
      showframe = TRUE, 
      projection = list(type = "Mercator"), 
      resolution = 50, 
      countrycolor = "rgb(255, 255, 255)", 
      coastlinecolor = "rgb(255, 255, 255)", 
      showcoastlines = FALSE),
    legend = list(traceorder = "reversed", x = 0.1, y = 0.9, orientation = 'h', xanchor = "center", yanchor = "middle"),
    autosize=F
  )
  
  plot_ly(width=600, height=350) %>% 
    add_trace(geo=trace1$geo, 
              type=trace1$type, 
              z=trace1$z, 
              showscale=trace1$showscale, 
              locationmode=trace1$locationmode, 
              locations=trace1$locations, 
              colorscale=trace1$colorscale,
              autocolorscale=trace1$autocolorscale) %>%
    layout(geo=layout$geo, 
           title=layout$title, 
           legend=layout$legend,
           margin=list(l = 0, r = 0, b = 0, t = 0, pad = 4))
}

#asean_map(asean_data_2017)

## Plot 2: Mirroring bar chart (of population density per sq.km vs. Total Vehicles per 1000 population) ----
asean_pop_bar <- function(asean_data_2017) {
  
  ay <- list(
    tickfont = list(),
    overlaying = "y",
    side = "right",
    title = "Population density per sq.km",
    range = c(0, 10000)
  )
  plot_ly(asean_data_2017, x = ~country, width=900, height=300) %>%
    add_trace(y = ~pop_mid, name = "Population of ASEAN Countries in '000", type = "bar", 
              yaxis = "y", textposition = "auto", color = I("dark green")) %>%
    add_trace(y = ~pop_density, name = "Population density per sq.km", type = "bar", 
              yaxis = "y2", alpha = 0.5, color = I("green"), width = 0.5) %>%
    layout(xaxis = list(type = 'category', title = "ASEAN Countries"), 
           yaxis2 = ay,
           yaxis = list( title = "Number of population", range = c(0, 300000)),
           legend = list(orientation='h',
                         x = 0.5, y = 1.2,
                         xanchor = "center",
                         yanchor = "top"))
}

#asean_pop_bar(asean_data_2017)
  

## Plot 3: Heatmap (Malaysia: road fatality by age group) ----
fatalilty_age_heatmap <- function(fatal_age) {
  
  fatal_age %>%
    mutate(Age = fct_relevel(Age, "00 - 05", "06 - 10", "11 - 15", "16 - 20", "21 - 25", "26 - 30",
                             "31 - 35", "36 - 40", "41 - 45", "46 - 50", "51 - 55", "56 - 60", 
                             "61 - 65", "66 - 70", "71 - 75", "> 75")) %>%
    mutate(text = paste0("x: ", year, "\n", "y: ", Age, "\n", "Value: ", fatality_count)) %>%
    plot_ly(x =~year, y =~Age, z =~fatality_count, colors =  colorRamp(c("green", "red")), type = "heatmap", 
            text=text, height = 400) %>%
    layout(legend = list(orientation = 'h'), xaxis = list(title = "Year"), yaxis = list(title = "Age Group"))
  
}

#fatalilty_age_heatmap(fatal_age)

## Plot 4: Barchart (Malaysia: road fatality by road user group) ----
fatalilty_user_bar <- function(accident_data) {
  
  p1 <- plot_ly(data=accident_data, x=~year, y=~accident_count, color=~vehicle_category, type = "bar") %>%
    layout(barmode = 'stack', xaxis = list(showticklabels = TRUE))
  p2 <- plot_ly(data=accident_data, x=~year, y=~fatality_count, color=~vehicle_category, type = "bar", 
                showlegend = FALSE, height = 600) %>%
    layout(barmode = 'stack')
  subplot(p1, p2, nrows = 2,  margin = 0.03, heights = c(0.5, 0.5))
  
}

#fatalilty_user_bar(accident_data)
  
## Plot 5: Line chart ----
state_line <- function(state_data) {
  
  mycolors = c(brewer.pal(name="Dark2", n = 8), brewer.pal(name="Paired", n = 6))

  plot_ly(state_data, x = ~year, y = ~JOHOR, name = 'Johor', type = 'scatter', mode = 'lines+markers', 
          height = 400, colors = mycolors) %>%
  add_trace(y = ~KEDAH, name = 'Kedah') %>%
  add_trace(y = ~KELANTAN, name = 'Kelantan') %>%
  add_trace(y = ~MELAKA, name = 'Melaka') %>%
  add_trace(y = ~NEGERI.SEMBILAN, name = 'Negeri Sembilan') %>%
  add_trace(y = ~PAHANG, name = 'Pahang') %>%
  add_trace(y = ~PERAK, name = 'Perak') %>%
  add_trace(y = ~PERLIS, name = 'Perlis') %>%
  add_trace(y = ~PULAU.PINANG, name = 'Pulau Pinang') %>%
  add_trace(y = ~SABAH, name = 'Sabah') %>%
  add_trace(y = ~SARAWAK, name = 'Sarawak') %>%
  add_trace(y = ~SELANGOR, name = 'Selangor') %>%
  add_trace(y = ~TERENGGANU, name = 'Terengganu') %>%
  add_trace(y = ~W.P..KUALA.LUMPUR, name = 'Kuala Lumpur') %>%
  layout(title = "",
         xaxis = list(title = "States in Malaysia"),
         yaxis = list (title = "Number of accidents"))
}

#state_line(state_data)
  
  
  
