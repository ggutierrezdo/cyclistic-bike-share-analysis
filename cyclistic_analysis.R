############################################
# Cyclistic Bike-Share Analysis
# Author: Gilberto Gutierrez
############################################

# ================================
# 1. LIBRERÍAS
# ================================
install.packages(c("tidyverse", "janitor", "lubridate"))

library(tidyverse)
library(janitor)
library(lubridate)

Sys.setlocale("LC_TIME", "English")

# ================================
# 2. CARGA DE DATOS (12 MESES)
# ================================
files <- list.files(
  path = "C:/Data_analyst/Cyclistic/",
  pattern = "divvy-tripdata.csv",
  full.names = TRUE
)

all_data <- files %>%
  map_dfr(read_csv)

# ================================
# 3. LIMPIEZA Y TRANSFORMACIÓN
# ================================
bike_data <- all_data %>%
  
  # Seleccionar columnas relevantes
  select(
    ride_id, rideable_type, started_at, ended_at,
    start_station_name, end_station_name,
    member_casual
  ) %>%
  
  # Formato fechas y nuevas variables
  mutate(
    started_at = ymd_hms(started_at),
    ended_at   = ymd_hms(ended_at),
    ride_length = as.numeric(difftime(ended_at, started_at, units = "min")),
    month = month(started_at),
    hour = hour(started_at),
    day_of_week = wday(started_at, label = TRUE)
  ) %>%
  
  # Limpieza general
  clean_names() %>%
  drop_na() %>%
  distinct() %>%
  
  # Eliminar valores inválidos
  filter(started_at < ended_at)

# ================================
# 4. REMOCIÓN DE OUTLIERS
#    (Percentil 2.5% - 97.5%)
# ================================
quantiles <- quantile(
  bike_data$ride_length,
  probs = c(0.025, 0.975)
)

bike_data <- bike_data %>%
  filter(
    ride_length >= quantiles[1],
    ride_length <= quantiles[2]
  )

# ================================
# 5. TABLAS RESUMEN (PARA TABLEAU)
# ================================

# Uso general
summary_user <- bike_data %>%
  group_by(member_casual) %>%
  summarise(
    total_rides = n(),
    avg_ride_length = mean(ride_length),
    median_ride_length = median(ride_length),
    .groups = "drop"
  )

# Top 5 estaciones por usuario
# Crear tabla para estaciones de inicio para miembros
start_station_member <- bike_data %>%
  filter(member_casual == "member") %>%
  group_by(start_station_name) %>%
  summarise(member = n(), .groups = 'drop') %>%
  arrange(desc(member)) %>% 
  top_n(5,member)

# Crear tabla para estaciones de inicio para usuarios casuales
start_station_casual <- bike_data %>%
  filter(member_casual == "casual") %>%
  group_by(start_station_name) %>%
  summarise(casual = n(), .groups = 'drop') %>%
  arrange(desc(casual))%>% 
  top_n(5,casual)

# Crear tabla para estaciones de fin para miembros
end_station_member <- bike_data %>%
  filter(member_casual == "member") %>%
  group_by(end_station_name) %>%
  summarise(member = n(), .groups = 'drop') %>%
  arrange(desc(member)) %>% 
  top_n(5,member)

# Crear tabla para estaciones de fin para usuarios casuales
end_station_casual <- bike_data %>%
  filter(member_casual == "casual") %>%
  group_by(end_station_name) %>%
  summarise(casual = n(), .groups = 'drop') %>%
  arrange(desc(casual))%>% 
  top_n(5,casual)

# Imprimir las tablas
print(start_station_member)
print(start_station_casual)
print(end_station_member)
print(end_station_casual)

# Uso por día de la semana
rides_by_weekday <- bike_data %>%
  group_by(member_casual, day_of_week) %>%
  summarise(
    total_rides = n(),
    avg_ride_length = mean(ride_length),
    .groups = "drop"
  )

# Uso por mes
rides_by_month <- bike_data %>%
  group_by(member_casual, month) %>%
  summarise(
    total_rides = n(),
    avg_ride_length = mean(ride_length),
    .groups = "drop"
  )

# Uso por hora
rides_by_hour <- bike_data %>%
  group_by(member_casual, hour) %>%
  summarise(
    total_rides = n(),
    .groups = "drop"
  )

# Uso por tipo de bicicleta
rides_by_bike <- bike_data %>%
  group_by(member_casual, rideable_type) %>%
  summarise(
    total_rides = n(),
    .groups = "drop"
  )

ggplot(bike_data, aes(x=rideable_type,fill= member_casual)) +
  geom_bar() + 
  ggtitle("Bike type") + xlab("") + ylab("Number of Rides")

# ================================
# 6. EXPORTACIÓN DE DATOS
# ================================
write_csv(bike_data, "bike_data_clean.csv")
write_csv(summary_user, "summary_user.csv")
write_csv(rides_by_weekday, "rides_by_weekday.csv")
write_csv(rides_by_month, "rides_by_month.csv")
write_csv(rides_by_hour, "rides_by_hour.csv")
write_csv(rides_by_bike, "rides_by_bike.csv")

# ================================
# 7. VALIDACIÓN RÁPIDA
# ================================
summary(bike_data$ride_length)
table(bike_data$member_casual)




