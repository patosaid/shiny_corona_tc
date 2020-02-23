############################ TABLAS CON datatable ##############################
# Probar DT para implementar en Shiny...
# Si es muy complicado intentar con kableExtra?

library(DT)
##### FIJAR OPCIONES GLOBALES PARA DT ############################
options(DT.options = 
          list(
      pageLength = 15, autoWidth = TRUE, lengthChange = FALSE,
      dom='t',ordering=F))


########## TOTAL CONFIRMADOS ###################################################
# Usando data de 01_import_data.r
# Primera forma de obtener el "total global"

dates <- unique(data$date)
last_day <- dates[[length(dates)]]

data %>% 
  select(`Country/Region` ,date, condition, value) %>% 
  filter(date == last_day) %>% 
  filter(condition == "confirmed") %>% 
  group_by(`Country/Region`) %>% 
  summarise(total_confirmed_by_country = sum(value)) %>% 
  arrange(desc(total_confirmed_by_country)) %>% 
  slice(c(1:15)) %>% 
  datatable(rownames = FALSE , filter = 'none', 
            options = list(
              columnDefs = list(list(width = '100px', targets = "_all"))), # tamaño de la columna?....
            colnames = c("Country", "Total Confirmed"))



