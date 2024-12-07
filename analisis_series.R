library(tidyverse)

data <- read.csv('input/viajes_bici.csv')

# Esto es lo que se hace con cada .csv para obtener viajes diarios:
# -----------------------------------------------------------------
# data <- data_original %>%
#   mutate(fecha_origen = as.Date(fecha_origen)) %>%
#   group_by(fecha_origen, destino) %>%
#   summarise(viajes = n()) %>%
#   rename("fecha" = "fecha_origen") %>%
#   pivot_wider(names_from = destino, values_from = viajes) %>%
#   replace(is.na(.), 0) %>%
#   pivot_longer(!fecha, names_to = "destino", values_to = "viajes")

# Plot
data %>% 
  mutate(fecha = as.POSIXct(fecha)) %>% 
  ggplot(aes(x = fecha, y = viajes, group = destino, color = destino)) + 
  geom_line(linewidth = 0.6) + 
  theme_bw(base_size = 18) + 
  theme(legend.position = "bottom")

# Matriz de correlaciones
data_wide = data %>% 
  pivot_wider(names_from = destino, values_from = viajes)
corrMatrix = cor(data_wide[,2:7])
