library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(lubridate)
library(DT)
library(highcharter)

server <- function(input, output) {
  options(DT.options = 
            list( autoWidth = T, lengthChange = FALSE, 
              scrollY = "365px", pageLength = 1000,
              dom='t',ordering=F))
  options(highcharter.theme = hc_theme_smpl())
  
  # Leer datos... Mejor en global??? 
  confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv") 
  deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
  recovered <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")
  
  confirmed <- confirmed %>%  
    pivot_longer(-c(1:4), names_to = "date", values_to = "confirmed")
  deaths<- deaths %>%
    pivot_longer(-c(1:4), names_to = "date", values_to = "deaths")
  recovered <- recovered %>%  
    pivot_longer(-c(1:4), names_to = "date", values_to = "recovered")

  data <- confirmed %>%
    left_join(deaths) %>% 
    left_join(recovered) %>% 
    pivot_longer(-c(1:5), names_to = "condition" , values_to = "value")
  
  # eliminar los datos importados para trabajabar sobre `data`
  rm(confirmed, deaths, recovered)
  
  # Vector de los días
  dates <- unique(data$date)
  # Último día de los datos (o ultima actualización)
  last_day <- dates[[length(dates)]]
  
  total_condition <- data %>% 
    filter(date == last_day) %>% 
    group_by(condition) %>% 
    summarise(total = sum(value))
  
  
  ########## RENDER VALUEBOX ###################################################
  
  output$confirmed_value <- renderValueBox({
    total_confirmed <- prettyNum(total_condition[[2]][[1]], 
                                 big.mark = ".", decimal.mark = ",")
    valueBox(
      paste0(total_confirmed), "Total Confirmed", 
      color = "red"
    )
  })
  
  output$deaths_value <- renderValueBox({
    total_death <- prettyNum(total_condition[[2]][[2]],
                             big.mark = ".", decimal.mark = ",")
    valueBox(
      paste0(total_death), "Total Deaths", 
      color = "black"
    )
  })
  
  output$recovered_value <- renderValueBox({
    total_recovered <- prettyNum(total_condition[[2]][[3]],
                                 big.mark = ".", decimal.mark = ",")
    valueBox(
      paste0(total_recovered), "Total Recovered", 
      color = "green"
    )
  })
  
###### RENDER TABLES ####################################
  
  output$table_country_confirmed <- renderDataTable(
    data %>% 
      select(`Country/Region` ,date, condition, value) %>% 
      filter(date == last_day) %>% 
      filter(condition == "confirmed") %>% 
      filter(value > 0) %>% 
      group_by(`Country/Region`) %>% 
      summarise(total_confirmed_by_country = sum(value)) %>% 
      arrange(desc(total_confirmed_by_country)) %>% 
      mutate_if(is.numeric , ~prettyNum(., big.mark = ".", decimal.mark = "," )) %>% 
      unite("Confirmed by Country" , 2:1, sep = " "), 
    selection = 'none',
    rownames = FALSE
  )
  
  output$table_country_death <- renderDataTable(
    data %>% 
      select(`Country/Region` ,date, condition, value) %>% 
      filter(date == last_day) %>% 
      filter(condition == "deaths") %>% 
      filter(value > 0) %>% 
      group_by(`Country/Region`) %>% 
      summarise(total_deaths_by_country = sum(value)) %>% 
      arrange(desc(total_deaths_by_country)) %>% 
      mutate_if(is.numeric , ~prettyNum(., big.mark = ".", decimal.mark = "," )) %>% 
      unite("Deaths by Country", 2:1, sep = " "),
    selection = 'none',
    rownames = FALSE,
    options = list(
      columnDefs = list(list(width = '100%px', targets = "_all"))
    )
  )
  
  output$table_country_recovered <- renderDataTable(
    data %>% 
      select(`Country/Region` ,date, condition, value) %>% 
      filter(date == last_day) %>% 
      filter(condition == "recovered") %>%
      filter(value > 0) %>% 
      group_by(`Country/Region`) %>% 
      summarise(total_recovered_by_country = sum(value)) %>% 
      arrange(desc(total_recovered_by_country)) %>% 
      mutate_if(is.numeric , ~prettyNum(., big.mark = ".", decimal.mark = "," )) %>% 
      unite("Recovered by Country", 2:1, sep = " "), 
    selection = 'none',
    rownames = FALSE
  )
  
############ PLOTLY WORLD ################################


  
 output$world_animated <- renderPlotly({
   g <- list(showframe = FALSE,
             coastlinecolor = toRGB("white"),
             showland = TRUE,
             landcolor = toRGB("gray80"),
             showcountries = TRUE,
             countrycolor = toRGB("white"),
             countrywidth = 0.2,
             projection = list(type = 'Mercator'))
   
    plotly_map <- data %>%   
      mutate(date = mdy(date)) %>% 
      arrange(date) %>%  
      rename(State = `Province/State`, 
             Country= `Country/Region` ,
             lat = Lat,
             lon = Long) %>% 
      mutate(Date = as.character(date)) %>% 
      filter(condition == "confirmed") %>%  # Para plotear solo los confirmados
      mutate(value = ifelse(value == 0 , NA, value)) %>%  # Pasar 0 a NA por plotly plotea los 0's
      plot_geo(marker = list(color = toRGB("red"), # Cambiar colores
                             opacity = 0.5,
                             line = list(color = toRGB("red"),
                                         width = 1.5))) %>%
      add_markers(x = ~lon,
                  y = ~lat,
                  sizes = c(2,max(data$value)/20),
                  size = ~value,
                  frame = ~Date,
                  showlegend = F,
                  type = 'scatter',
                  mode = 'markers',
                  hoverinfo = "text",
                  text = ~paste('Country: ', Country, #Etiquetas 
                                '<br />Confirmed: ', value,
                                "<br />Date: ", date)) %>%
      #  animation_slider(frame= as.character(dias[[1]][[nrow(dias)]])) %>% 
      animation_button(  # esto parece que no sirve...
        x = 1, xanchor = "right", y = 0, yanchor = "bottom"
      ) %>% 
      layout(geo = g) %>%  # Las opciones del mapa!
      animation_opts( easing = "elastic", redraw = FALSE, 
                      transition = 500, mode = "afterall"
      ) # parece que no sirve....
    plotly_map
  })

################## PLOT HIGHCHARTER ###########################################
  
 output$plot1 <- renderHighchart({
   
   data_hc <- data %>%
     mutate(date = mdy(date)) %>% 
     group_by(date, condition) %>% 
     summarise(value = sum(value)) %>% 
     pivot_wider(names_from = condition, values_from = value) %>% 
     ungroup() 
   
   hc <- highchart() %>% 
     hc_add_series(name = "Confirmed", data = data_hc$confirmed) %>% 
     hc_add_series(name = "Deaths", data = data_hc$deaths , color = "black") %>% 
     hc_add_series(name = "Recovered", data= data_hc$recovered , color = "green") %>% 
     hc_xAxis(categories = data_hc$date) %>% 
     hc_tooltip(table = TRUE, sort = TRUE)
   rm(data_hc)
   hc 
 })
 
  output$plot2 <- renderHighchart({
    
    data_hc_2 <- data %>%
      mutate(date = mdy(date)) %>% 
      group_by(date, condition) %>% 
      summarise(value = sum(value)) %>% 
      ungroup() %>% 
      arrange(condition, date) %>% 
      mutate(delta_day = value - lag(value , default = 0)) %>% 
      mutate(delta_day = ifelse(date=="2020-01-22", 0, delta_day)) %>% 
      select(-value) %>% 
      pivot_wider(names_from = condition , values_from = delta_day)
    
    hc2 <- highchart() %>% hc_chart(type = "column") %>% 
      hc_add_series(name = "Confirmed", data = data_hc_2$confirmed) %>% 
      hc_add_series(name = "Deaths", data = data_hc_2$deaths , color = "black") %>% 
      hc_add_series(name = "Recovered", data= data_hc_2$recovered , color = "green") %>% 
      hc_xAxis(categories = data_hc_2$date) %>% 
      hc_tooltip(table = TRUE, sort = TRUE)
    rm(data_hc_2)
    
    hc2
    
  })
  
################## PRINT INFORMATION ##############################
  
  output$day_updated <- renderText({
    last_day2 <- as.Date(last_day , format = "%m/%d/%y")
    last_day2 <- format(last_day2, format = "%B %d %Y")
    paste("Last updated:", last_day2)
  })
 
}