# Explore climate stations (CLIMANEVADA) time series for comparison with global models

library(dplyr)
library(lubridate)
library(ggplot2)
library(raster)
library(tidyverse)
library(rgdal)

di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/data/climate/Datos_estaciones/"

# Stations 
all_stations_23030 <- read.table(paste(di, "0. Estaciones_CLIMA.txt", sep = ""), header = TRUE, sep = ";", dec = ",")
all_stations_spdf_23030 <- SpatialPointsDataFrame(coords = all_stations_23030[,c(5,6)], data = all_stations_23030)
plot(all_stations_spdf_23030)
projection(all_stations_spdf_23030) # Not prj defined
CRS.ED50 <- CRS("+init=epsg:23030 +proj=utm +zone=30 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +units=m +no_defs")
proj4string(all_stations_spdf_23030) <- CRS.ED50
projection(all_stations_spdf_23030)

# Load active area
active_3042 <- readOGR(dsn = "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/indata_generation/input_files/", layer = "study_area_active_3042")
projection(active_3042)
plot(active_3042)

# Reproject stations
all_stations_spdf_3042 <- spTransform(all_stations_spdf_23030, projection(active_3042))
projection(all_stations_spdf_23030)
projection(all_stations_spdf_3042)

# Extract stations in active area
stations_aoi <- all_stations_spdf_3042[active_3042, ] # https://www.r-bloggers.com/clipping-spatial-data-in-r/
plot(stations_aoi)
# View(stations_aoi@data)

# Export list of stations
# write.csv(stations_aoi@data, paste(di, "estaciones_en_aoi.csv", sep = ""))

# Available data for stations in aoi
data_aval <- data.frame(file = list.files(path = di, pattern = "*.csv"))
data_aval$INDICATIVO <- c()
for(i in 1:length(data_aval$file)) {
  data_aval$INDICATIVO[i] <- strsplit(as.character(data_aval$file[i]), split = ".csv")[[1]][1]
}
stations_with_data <- data.frame(stations_aoi@data) %>%
  inner_join(data_aval)

stations_without_data <- data.frame(stations_aoi@data) %>%
  full_join(data_aval) %>%
  filter(is.na(file) == TRUE)
# write.csv(stations_without_data, paste(di, "stations_without_data.csv", sep = ""))

# Collect data from stations in aoi

# PRECIPITATION
# PD1: Precipitación total diaria de 7 a 7 (mm)
prec_data <- data.frame()
for(i in 1:length(stations_with_data$INDICATIVO)){
    temp <- read.table(paste(di, stations_with_data$INDICATIVO[i], ".csv", sep = ""), sep = ",", header = TRUE) %>%
      dplyr::filter(VALIDACION == "R",
                    VARIABLE == "PD1") %>% #
      mutate(INDICATIVO = stations_with_data$INDICATIVO[i],
             date = as.Date(FECHA, tryFormats = c("%d/%m/%Y")),
             Year = year(date),
             Month = month(date),
             days_in_month = days_in_month(date)) %>%
      dplyr::select(INDICATIVO, date, HORA, Year, Month, days_in_month, VALOR)
    prec_data <- rbind(prec_data, temp)
}

# Filter
prec_gaps_check <- prec_data %>%
  dplyr::select(INDICATIVO, Year, Month, days_in_month, VALOR) %>%
  group_by(INDICATIVO, Year, Month, days_in_month) %>%
  summarise(observations = n()) %>%
  mutate(VALID = ifelse(observations/days_in_month == 1, TRUE, FALSE)) %>%
  filter(VALID == TRUE)

prec_subset <- left_join(prec_gaps_check, prec_data)
month_prec <- prec_subset %>%
  dplyr::select(INDICATIVO, Year, Month, VALOR) %>%
  group_by(INDICATIVO, Year, Month) %>%
  summarise(month_prec = sum(VALOR))

# Explore
stations <- unique(month_prec$INDICATIVO)
month_prec %>%
  filter(INDICATIVO == stations[127]) %>%
  mutate(date = as.Date(paste(Year, Month, "15", sep = "-"), tryFormats = c("%Y-%m-%d"))) %>%
  ggplot(aes(x = date, y = month_prec)) +
  geom_point()

# Export
# write.table(month_prec, paste("/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/data/climate/calculated_month_prec.txt"), sep = ";")

# Identify number of prec events per month
prec_events <- prec_subset %>%
  dplyr::select(INDICATIVO, Year, Month, VALOR) %>%
  mutate(Prec_event = ifelse(VALOR == 0, 0, 1)) %>%
  # filter(Prec_event == 1) %>%
  dplyr::select(-VALOR) %>%
  group_by(INDICATIVO, Year, Month) %>%
  summarise(Nr_prec_events = sum(Prec_event))

mean(prec_events$Nr_prec_events)
sd(prec_events$Nr_prec_events)
min(prec_events$Nr_prec_events)
max(prec_events$Nr_prec_events)

hist(prec_events$Nr_prec_events)  

plot(prec_events$INDICATIVO  ~ prec_events$Nr_prec_events)

# PI1: Precipitación caída en el período (mm) --> Need to filter and check differently (measurements every 10 minutes)

# Maximum temperature
# TD4: Temperatura máxima diaria (ºC).
tmax_data <- data.frame()
for(i in 1:length(stations_with_data$INDICATIVO)){
  temp <- read.table(paste(di, stations_with_data$INDICATIVO[i], ".csv", sep = ""), sep = ",", header = TRUE) %>%
    dplyr::filter(VARIABLE == "TD4",
                  VALIDACION == "R" | VALIDACION == "E") %>% #
    mutate(INDICATIVO = stations_with_data$INDICATIVO[i],
           date = as.Date(FECHA, tryFormats = c("%d/%m/%Y")),
           Year = year(date),
           Month = month(date),
           days_in_month = days_in_month(date)) %>%
    dplyr::select(INDICATIVO, date, HORA, Year, Month, VALOR, days_in_month)
  tmax_data <- rbind(tmax_data, temp)
}

tmax_gaps_check <- tmax_data %>%
  dplyr::select(INDICATIVO, Year, Month, days_in_month, VALOR) %>%
  group_by(INDICATIVO, Year, Month, days_in_month) %>%
  summarise(observations = n()) %>%
  mutate(VALID = ifelse(observations/days_in_month == 1, TRUE, FALSE)) %>%
  filter(VALID == TRUE)

tmax_subset <- left_join(tmax_gaps_check, tmax_data)
month_tmax <- tmax_subset %>%
  dplyr::select(INDICATIVO, Year, Month, VALOR) %>%
  group_by(INDICATIVO, Year, Month) %>%
  summarise(month_tmax = mean(VALOR))

# Explore
stations <- unique(month_tmax$INDICATIVO)
month_tmax %>%
  filter(INDICATIVO == stations[5]) %>%
  mutate(date = as.Date(paste(Year, Month, "15", sep = "-"), tryFormats = c("%Y-%m-%d"))) %>%
  ggplot(aes(x = date, y = month_tmax)) +
  geom_point()

# Export
# write.table(month_tmax, paste("/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/data/climate/calculated_month_tmax_TD4.txt"), sep = ";")

# Minimum temperature
# TD2: Temperatura mínima diaria (ºC)
tmin_data <- data.frame()
for(i in 1:length(stations_with_data$INDICATIVO)){
  temp <- read.table(paste(di, stations_with_data$INDICATIVO[i], ".csv", sep = ""), sep = ",", header = TRUE) %>%
    dplyr::filter(VARIABLE == "TD2",
                  VALIDACION == "R" | VALIDACION == "E") %>% #
    mutate(INDICATIVO = stations_with_data$INDICATIVO[i],
           date = as.Date(FECHA, tryFormats = c("%d/%m/%Y")),
           Year = year(date),
           Month = month(date),
           days_in_month = days_in_month(date)) %>%
    dplyr::select(INDICATIVO, date, HORA, Year, Month, VALOR, days_in_month)
  tmin_data <- rbind(tmin_data, temp)
}

tmin_gaps_check <- tmin_data %>%
  dplyr::select(INDICATIVO, Year, Month, days_in_month, VALOR) %>%
  group_by(INDICATIVO, Year, Month, days_in_month) %>%
  summarise(observations = n()) %>%
  mutate(VALID = ifelse(observations/days_in_month == 1, TRUE, FALSE)) %>%
  filter(VALID == TRUE)

tmin_subset <- left_join(tmin_gaps_check, tmin_data)
month_tmin <- tmin_subset %>%
  dplyr::select(INDICATIVO, Year, Month, VALOR) %>%
  group_by(INDICATIVO, Year, Month) %>%
  summarise(month_tmin = mean(VALOR))

# Explore
stations <- unique(month_tmin$INDICATIVO)
month_tmin %>%
  filter(INDICATIVO == stations[7]) %>%
  mutate(date = as.Date(paste(Year, Month, "15", sep = "-"), tryFormats = c("%Y-%m-%d"))) %>%
  ggplot(aes(x = date, y = month_tmin)) +
  geom_point()

# Export
# write.table(month_tmin, paste("/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/data/climate/calculated_month_tmin_TD2.txt"), sep = ";")
