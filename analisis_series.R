library(tidyverse)

data <- read.csv('input/viajes_bici.csv')

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
