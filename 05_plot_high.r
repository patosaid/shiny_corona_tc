####################### PLOT with highcharter ##################################

######################## PLOTEAR CURVA VALORES POR FECHA #######################

library(highcharter)
options(highcharter.theme = hc_theme_smpl())

datos <- data(citytemp)

citytemp

highchart() %>% 
  hc_xAxis(categories = citytemp$month) %>% 
  hc_add_series(name = "Tokyo", data = citytemp$tokyo) %>% 
  hc_add_series(name = "London", data = citytemp$london) %>% 
  hc_add_series(name = "Other city",
                data = (citytemp$tokyo + citytemp$london)/2)

data %>%
  mutate(date = mdy(date)) %>% 
  group_by(date, condition) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
#  pivot_wider(names_from = condition , values_from = value) %>% 
  hchart("line", hcaes(x = date, y = value, group = condition))


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

hc 


####################### PLOTEAR EL CAMBIO DIARIO ###############################

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

highchart() %>% hc_chart(type = "column") %>% 
  hc_add_series(name = "Confirmed", data = data_hc_2$confirmed) %>% 
  hc_add_series(name = "Deaths", data = data_hc_2$deaths , color = "black") %>% 
  hc_add_series(name = "Recovered", data= data_hc_2$recovered , color = "green") %>% 
  hc_xAxis(categories = data_hc_2$date) %>% 
  hc_tooltip(table = TRUE, sort = TRUE)

  