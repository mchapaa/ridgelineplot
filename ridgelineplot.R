# Instalar y cargar las librerías necesarias (descomenta la línea si es necesario)
# install.packages(c("readxl", "dplyr", "lubridate", "ggplot2", "ggridges"))

library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggridges)

# 1. Leer y preparar datos de Valencia
df_val <- read_excel("3 temperaturas valencia.xlsx") %>%
  mutate(
    date = as.Date(date),
    Mes = factor(month(date, label = TRUE, abbr = FALSE), levels = month.name),
    City = "Valencia"
  ) %>%
  filter(year(date) == 2024)

# 2. Leer el archivo de intervalos óptimos
df_opt <- read_excel("naranja.xlsx") %>%
  mutate(Mes = factor(month(as.Date(Fecha), label = TRUE, abbr = FALSE), levels = month.name))

# 3. Filtrar y limpiar datos
df_val <- df_val %>%
  filter(is.finite(tmin), is.finite(tmax))  # Asegura que no hay NAs en tmin y tmax

# 4. Graficar tmin y tmax  
ggplot() +
  # Curva de tmin 
  geom_density_ridges(
    data = df_val,
    aes(x = tmin, y = Mes, fill = "tmin"),
    alpha = 0.6,
    scale = 1,
    rel_min_height = 0.01,
    position = "identity"
  ) +
  # Curva de tmax  
  geom_density_ridges(
    data = df_val,
    aes(x = tmax, y = Mes, fill = "tmax"),
    alpha = 0.6,
    scale = 1,
    rel_min_height = 0.01,
    position = "identity"
  ) +
  # Segmento para el rango óptimo
  geom_segment(
    data = df_opt,
    aes(
      x = temp_min, xend = temp_max,
      y = Mes, yend = Mes,
      color = "Intervalo óptimo"
    ),
    inherit.aes = FALSE,
    size = 1.5
  ) +
  # Líneas verticales discontinuas  
  geom_vline(aes(xintercept = 5, linetype = "Tolerancia"), color = "black", size = 1) +
  geom_vline(aes(xintercept = 36, linetype = "Tolerancia"), color = "black", size = 1) +
  # Escala de colores manual para tmin y tmax
  scale_fill_manual(
    name = "",
    values = c("tmin" = "#1f77b4", "tmax" = "yellow")
  ) +
  
  scale_color_manual(
    name = "",
    values = c("Intervalo óptimo" = "darkorange2")
  ) +
  scale_linetype_manual(
    name = "",
    values = c("Tolerancia" = "dashed"),
    labels = c("Líneas discontinuas: Temp. mínima y máxima de tolerancia")
  ) +
  labs(
    title = "Distribución de Temperaturas Mínimas y Máximas - Valencia 2024",
    subtitle = "y cultivo de la naranja",
    x = "Temperatura (°C)",
    y = "Mes"
  ) +
  theme_ridges() +
  theme(legend.position = "top")
