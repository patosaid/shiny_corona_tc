##################### Importar datos de coronavirus ############################
# Datos del repo: https://github.com/CSSEGISandData/COVID-19

library(tidyverse)

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

library(lubridate)

data %>% mutate(date = mdy(date)) %>% 
  pivot_wider( names_from = "condition" , values_from = "value") %>% 
  arrange(date) %>% 
  group_by(`Province/State`, `Country/Region`) %>% 
  summarise(confirmados = max(confirmed),
            muertes = max(deaths),
            recuperados = max(recovered)) %>% 
  arrange(desc(confirmados))



