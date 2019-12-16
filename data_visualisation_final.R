#### Objective: Creating plots ------------

## Load the required libraries ------------
library(plotly)
library(geojsonio)
library(broom)
library(ggplot2)
library(mapproj)
library(dplyr)
library(plyr)
library(ggthemes)
library(hrbrthemes)

## Plot 1: ASEAN map (of population number and population density/ # of motor vehicles) ----
asean_map <- function(asean_info) {
  
  trace1 <- list(
    geo = "geo", 
    type = "choropleth", 
    z = asean_info$Total_vehicle_2017, 
    showscale = TRUE, 
    locationmode = "country names", 
    locations = asean_info$Country, 
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
  
  plot_ly(width=600, height=400) %>% 
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
# test
# asean_map(asean_info)

## Plot 2: Mirroring bar chart (of population density per sq.km vs. Total Vehicles per 1000 population) ----
asean_pop_bar <- function(asean_data_2017) {
  
  ay <- list(
    tickfont = list(color = "blue"),
    overlaying = "y",
    side = "right",
    title = "second y axis",
    range = c(0, 10000)
  )
  plot_ly(asean_data_2017, x = ~country, width=600, height=400) %>%
    add_trace(y = ~pop_mid, name = "Population of ASEAN Countries in '000", type = "bar", yaxis = "y", textposition = "auto") %>%
    add_trace(y = ~pop_density, name = "Population density per sq.km", type = "bar", yaxis = "y2", alpha = 0.5) %>%
    layout(xaxis = list(type = 'category', title = " "), 
           yaxis2 = ay,
           yaxis = list(range = c(0, 300000)),
           legend = list(traceorder = "reversed", 
                         x = 0.1, y = -0.2,
                         orientation = 'h',
                         xanchor = "center",
                         yanchor = "middle",
                         barmode = 'group'))
}
# test
#asean_pop_bar(asean_data_2017)


## Plot 3: Tornado chart (accidents vs. fatal counts by state over a period of 10 years, 2008-2017) ----
state_decade_transform <- function(state_decade) {
  test <- copy(state_decade)
  #----
  test$accident_decade_perc <- test$accident_decade/sum(test$accident_decade)*100
  test$accident_xmin <- ifelse(test$accident_decade_perc >=0, 0, test$accident_decade_perc)
  test$accident_xmax <- ifelse(test$accident_decade_perc <=0, 0, test$accident_decade_perc)
  test <- test[with(test, order(-accident_xmax)),]
  test$accident_xmid = (test$accident_xmin + test$accident_xmax)/2
  test$accident_ymin <- seq(1,14)
  test$accident_ymax <- seq(2,15)
  test$accident_ymid = (test$accident_ymin + test$accident_ymax)/2
  #----
  test$fatal_decade_perc <- abs(test$fatal_decade)/sum(abs(test$fatal_decade))*100
  test$fatal_decade_check <- ifelse(test$fatal_decade >=0, TRUE, FALSE)
  test <- test %>% mutate(fatal_decade_perc = ifelse(test$fatal_decade_check == TRUE, fatal_decade_perc, -fatal_decade_perc))
  test$fatal_xmin <- ifelse(test$fatal_decade_perc >=0, 0, test$fatal_decade_perc)
  test$fatal_xmax <- ifelse(test$fatal_decade_perc <=0, 0, test$fatal_decade_perc)
  test$fatal_xmid = (test$fatal_xmin + test$fatal_xmax)/2
    test$fatal_ymin <- seq(1,14)
  test$fatal_ymax <- seq(2,15)
  test$fatal_ymin <- test$fatal_ymin + 0.25
  test$fatal_ymax <- test$fatal_ymax - 0.25
  test$fatal_ymid = (test$fatal_ymin + test$fatal_ymax)/2
  state_decade_long <- copy(test)
  return(as.data.frame(state_decade_long))
}

state_decade_tornado <- function(state_decade_long) {
  state_decade_long <- state_decade_transform(state_decade)
  axis_buffer <- 1.1
  xAxis <- list(title = "", tickangle = 0, tickfont = list(size = 10))
  yAxis <- list(side = "left", showgrid = TRUE, zeroline = TRUE, title = "", showticklabels = FALSE,
                range = c(max(state_decade_long$accident_decade) / max(state_decade_long$fatal_decade) * min(state_decade_long$fatal_decade) * axis_buffer, max(state_decade_long$accident_decade) * axis_buffer))
  yAxis2 <- list(side = "right", autotick = FALSE, ticks = "outside", tick0 = 0, dtick = 0.1, 
                 showgrid = FALSE, zeroline = TRUE, overlaying = "y", showticklabels = FALSE,
                 range = c(min(state_decade_long$fatal_decade) * axis_buffer, max(state_decade_long$fatal_decade) * axis_buffer))
  
  plot_ly(data = state_decade_long, x = ~State, width = 1400, height = 400) %>%
    add_trace(data = state_decade_long, x = ~State, y = ~accident_decade, name = 'accident_decade', 
              type = "bar", yaxis = "y", textposition = "top", text = ~accident_decade) %>% 
              # textfont=list(size=12, color="black")
    add_trace(data = state_decade_long, x = ~State, y = ~fatal_decade, name = 'fatal_decade', type = "bar", 
              yaxis = "y2", alpha = 0.5, text =  ~fatal_decade) %>%
    layout(margin = list(r=50, b = 150), xaxis = xAxis, yaxis = yAxis, yaxis2 = yAxis2, showlegend = FALSE)
}
# test:
state_decade_tornado(state_decade_long)


## Plot 4: Heatmap (Malaysia: road fatality by age group) ----
fatalilty_age_heatmap <- function(fatal_age) {
  
  fatal_age <- fatal_age %>%
    mutate(Age = fct_relevel(Age, "0 - 5", "6 - 10", "11 - 15", "16 - 20", "21 - 25", "26 - 30",
                             "31 - 35", "36 - 40", "41 - 45", "46 - 50", "51 - 55", "56 - 60", 
                             "61 - 65", "66 - 70", "71 - 75", "> 75")) %>%
    mutate(text = paste0("x: ", year, "\n", "y: ", Age, "\n", "Value: ", fatality_count))
  
  p <- ggplot(fatal_age, aes(year, Age, fill= fatality_count, text=text)) + 
    geom_tile() +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    scale_fill_viridis_c(option = "B", direction = -1)
  
  ggplotly(p, tooltip="text", width=700, height=400)
}

# test:
fatalilty_age_heatmap(fatal_age)



## Plot 5: Barchart (Malaysia: road fatality by road user group) ----
fatalilty_user_bar <- function(accident_data) {
  
  plot_ly(accident_data, x = ~year, y = ~accident_count, type = 'bar', name = 'accident', marker = list(color = "#5d8402"), 
            hoverinfo = "text", text = ~text1, width=300, height=400) %>%
    add_trace(accident_data, x = ~year, y = ~fatality_count, type = 'bar', name = 'fatality', yaxis = 'y2', 
              marker = list(color = "#BDE0A7"), width = 0.3, alpha = 0.3,
              hoverinfo = "text", text = ~text2) %>%
    layout(title = "",
           xaxis = list(title = "", autotick = FALSE),
           yaxis = list(side = 'left', title = 'Accient count', showgrid = FALSE, zeroline = FALSE),
           yaxis2 = list(side = 'right', overlaying = "y", title = 'Fatality count', showgrid = FALSE, zeroline = FALSE),
           legend = list())
}

# test
#fatalilty_user_bar(accident_data)

asean_data
