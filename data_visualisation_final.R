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

## Plot 1: ASEAN map (of population number and population density/ # of motor vehicles) ----
asean_map <- function(asean_info) {
  
  trace1 <- list(
    geo = "geo", 
    type = "choropleth", 
    z = asean_info$Population_2018, 
    showscale = TRUE, 
    locationmode = "country names", 
    locations = asean_info$Country, 
    colorscale = "Viridis",
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
    title = "Map of Southeast Asia", 
    legend = list(traceorder = "reversed", xanchor = "center", x = 0.5)
  )
  
  plot_ly() %>% 
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
           legend=layout$legend)
}
# test
asean_map(asean_info)

## Plot 2: Mirroring bar chart (of population density per sq.km vs. Total Vehicles per 1000 population) ----
asean_pop_bar <- function(new_asean_data) {
  new_asean_data %>% 
    filter(year == "2017") %>%
    plot_ly(x = ~country, y = ~pop_mid, type = 'bar', name = "Population of ASEAN Countries in '000") %>%
    add_trace(y = ~pop_density, name = "Population density per sq.km") %>%
    layout(yaxis = list(title = 'Count'), barmode = 'overlay', yaxis = FALSE)
}
# test
asean_pop_bar(new_asean_data)

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
  xAxis <- list(title = "", tickangle = 90, tickfont = list(size = 10))
  yAxis <- list(side = "left", showgrid = TRUE, zeroline = TRUE, title = "", 
                range = c(max(state_decade_long$accident_decade) / max(state_decade_long$fatal_decade) * min(state_decade_long$fatal_decade) * axis_buffer, max(state_decade_long$accident_decade) * axis_buffer))
  yAxis2 <- list(side = "right", autotick = FALSE, ticks = "outside", tick0 = 0, dtick = 0.1, 
                 showgrid = FALSE, zeroline = TRUE, overlaying = "y",
                 range = c(min(state_decade_long$fatal_decade) * axis_buffer, max(state_decade_long$fatal_decade) * axis_buffer))
  
  plot_ly(data = state_decade_long, x = ~State) %>%
    add_trace(data = state_decade_long, x = ~State, y = ~accident_decade, name = 'accident_decade', type = "bar", yaxis = "y", textposition = "auto") %>%
    add_trace(data = state_decade_long, x = ~State, y = ~fatal_decade, name = 'fatal_decade', type = "bar", yaxis = "y2", alpha = 0.5) %>%
    layout(margin = list(r=50, b = 150), xaxis = xAxis, yaxis = yAxis, yaxis2 = yAxis2, showlegend = FALSE)
}
# test:
state_decade_tornado(state_decade_long)

## Plot 4: Heatmap (Malaysia: road fatality by age group) ----
fatalilty_age_heatmap <- function(fatal_age) {
  fatal_age %>% 
    ggplot(aes(age_category, year)) +
    geom_raster(aes(fill=fatality_count), alpha = 0.9) +
    scale_x_discrete(limits=unique(fatal_age$age_category)) +
    scale_fill_gradientn(colours=c("#CDDC39","#E91E63")) +
    theme_minimal()
}

# test:
fatalilty_age_heatmap(fatal_age)

## Plot 4: Barchart (Malaysia: road fatality by road user group) ----
fatalilty_user_bar <- function(accident_data) {
  accident_data %>%
    plot_ly(x = ~year, y = ~accident_count, type = 'bar', name = 'accident', marker = list(color = "#5d8402"), 
            hoverinfo = "text", text = ~accident_count) %>%
    add_trace(x = ~year, y = ~fatality_count, type = 'bar', name = 'fatality', yaxis = 'y2', 
              marker = list(color = "#9fff80"), width = 0.5, alpha = 0.3,
              hoverinfo = "text", text = ~fatality_count) %>%
    layout(title = 'To think',
           xaxis = list(title = ""),
           yaxis = list(side = 'left', title = 'Later', showgrid = FALSE, zeroline = FALSE),
           yaxis2 = list(side = 'right', overlaying = "y", title = 'Later2', showgrid = FALSE, zeroline = FALSE))
}

# test
fatalilty_user_bar(accident_data)
