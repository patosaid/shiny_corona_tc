# 02 Hacer algunos resúmenes

########## TOTAL CONFIRMADOS ###################################################
# Usando data de 01_import_data.r
# Primera forma de obtener el "total global"

dates <- unique(data$date)
last_day <- dates[[length(dates)]]

data %>% 
  filter(date == last_day) %>% 
  group_by(condition) %>% 
  summarise(total = sum(value))


# Otra alternativa??? no se...

############ TABLA DE PAISES CON MAYOR CANTIDAD DE CONFIRMADOS ################

data %>% 
  select(`Country/Region` ,date, condition, value) %>% 
  filter(date == last_day) %>% 
  filter(condition == "confirmed") %>% 
  group_by(`Country/Region`) %>% 
  summarise(total_confirmed_by_country = sum(value)) %>% 
  arrange(desc(total_confirmed_by_country)) %>% 
  head(15)


######### TABLA DE TOTALES (CONFIRMADOS-MUERTES-RECUPERADOS) POR PAIS ##########

data %>% 
  select(`Country/Region` ,date, condition, value) %>% 
  filter(date == last_day) %>% 
  group_by(`Country/Region`, condition) %>% 
  summarise(total_by_country = sum(value)) %>% 
  replace_na( list( total_by_country =  0)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = condition, values_from = total_by_country) %>% 
  arrange(desc(confirmed))


################### ACTUALMENTE CONFIRMADOS ####################################

# A confirmados se le resta los recuperados y muertes para saber la situacion actual

table <- data %>% 
  select(`Country/Region` ,date, condition, value) %>% 
  filter(date == last_day) %>% 
  group_by(`Country/Region`, condition) %>% 
  summarise(total_by_country = sum(value)) %>% 
  replace_na( list( total_by_country =  0)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = condition, values_from = total_by_country) %>% 
  arrange(desc(confirmed)) %>% 
  mutate(confirmed_now = confirmed - deaths - recovered)

############# TABLA POR DIA, CONFIRMADOS RECUPERADOS Y MUERTES #################

data %>%
  group_by(date, condition) %>% 
  summarise(valor = sum(value))



  




