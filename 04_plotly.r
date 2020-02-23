################ USANDO PLOTLY #################################################

# Falta agregar una opcion para mostrar desde el ultimo día....
# hay autoplay?
# Poner más info en las etiquetas...


library(plotly)
library(lubridate)

# en g algunas opciones del mapa....
g <- list(showframe = FALSE,
          coastlinecolor = toRGB("white"),
          showland = TRUE,
          landcolor = toRGB("gray80"),
          showcountries = TRUE,
          countrycolor = toRGB("white"),
          countrywidth = 0.2,
          projection = list(type = 'Mercator'))

data %>%   
  mutate(date = mdy(date)) %>% 
  arrange(date) %>%  
  rename(State = `Province/State`, 
         Country= `Country/Region` ,
         lat = Lat,
         lon = Long) %>% 
  mutate(Date = as.character(date)) %>% 
  filter(condition == "confirmed") %>%  # Para plotear solo los confirmados
  mutate(value = ifelse(value == 0 , NA, value)) %>%  # Pasar 0 a NA por plotly plotea los 0's
  plot_geo(marker = list(color = toRGB("purple"), # Cambiar colores
                         opacity = 0.5,
                         line = list(color = toRGB("purple"),
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
    animation_opts( easing = "elastic", redraw = FALSE, transition = 500,mode = "afterall"
    ) # parece que no sirve....



