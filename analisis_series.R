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

# Probar Dickey-Fuller (raices unitarias)
library(tseries)
data %>% 
  filter(destino == "Fac. Ciencias Económicas") %>% 
  pull(viajes) %>% 
  ts(frequency = 7) %>% 
  adf.test(k = floor(12*(length(.)/100)^0.25))

# Probar test HEGY (raices unitarias estacionales)
library(uroot)
ts_med <- data %>% 
  filter(destino == "Fac. de Ingeniería") %>% 
  pull(viajes) %>% 
  ts(frequency = 7)
hegy.test(ts_med, lag.method = "fixed", deterministic = c(1,1,0), maxlag = 22)
# Luego de una diferenciacion estacional...
ts_med_D1 <- diff(ts_med, lag = 7)
hegy.test(ts_med_D1)
