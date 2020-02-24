# 02 Hacer algunos resúmenes


dates <- unique(data$date)
last_day <- dates[[length(dates)]]

########## TOTAL CONFIRMADOS ###################################################
# Usando data de 01_import_data.r
# Primera forma de obtener el "total global"

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

######### TABLA DE PAISES CON MAYOR CANTIDAD DE CONFIRMADOS UNIDO ##############

data %>% 
  select(`Country/Region` ,date, condition, value) %>% 
  filter(date == last_day) %>% 
  filter(condition == "confirmed") %>% 
  group_by(`Country/Region`) %>% 
  summarise(total_confirmed_by_country = sum(value)) %>% 
  arrange(desc(total_confirmed_by_country)) %>% 
  head(15) %>% 
  unite("Confirmed by Country" , 2:1, sep = " ")


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

data %>% 
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
# confirmados/muertes/recuperados por DIA (acumulado)
data %>%
  mutate(date = mdy(date)) %>% 
  group_by(date, condition) %>% 
  summarise(valor = sum(value))


######### TABLA DE CAMBIO ######################################################
# confirmados/muertes/recuperados por DIA (cambio(delta))
data %>%
  mutate(date = mdy(date)) %>% 
  group_by(date, condition) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  arrange(condition, date) %>% 
  mutate(delta_day = value - lag(value , default = 0)) %>% 
  mutate(delta_day = ifelse(date=="2020-01-22", 0, delta_day))

# QUE MAS??????

# Modelos ...? tidy model? ver Cap. de modelos... seriestemporales?




