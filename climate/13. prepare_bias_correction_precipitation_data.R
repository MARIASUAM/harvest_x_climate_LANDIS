# GENERATE TABLE OF DATA COMPARISON FOR BIAS CORRECTION ON PRECIPITATION DATA

### SETUP ### 
library(raster)
library(dplyr)
library(reshape)
library(tidyverse)
library(lubridate)

di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/"

proj3042 <- CRS(SRS_string="EPSG:3042")

### CLIMATE REGIONS ###
# Load map of climate regions
regions <- raster(paste0(di, "data/climate/clim_regions_20200406.tif")) # EPSG: 3042
crs(regions) <- proj3042

### OBSERVATIONS ####
# Load precipitation data
prec <- read.table(paste(di, "data/climate/prec_data_nofilter.txt", sep = ""), sep = ";")

# Load stations data 
stations <- read.table(paste(di, "data/climate/Datos_estaciones/estaciones_en_aoi.csv", sep = ""), header = TRUE, sep = ",") # EPSG: 3042
stations_spdf <- SpatialPointsDataFrame(coords = stations[,c(6,7)], data = stations)
crs(stations_spdf) <- proj3042
# plot(stations_spdf)

# Extract climate region for each station
stations_regions <- as.matrix(t(raster::extract(regions, stations_spdf, method = 'simple'))) 
stations_regions <- as.data.frame(stations_regions)
colnames(stations_regions) <- stations_spdf@data$INDICATIVO
stations_regions <- melt(stations_regions)
colnames(stations_regions) <- c("INDICATIVO", "clim_region")

nr_stations_per_region <- stations_regions %>%
  dplyr::select(clim_region) %>%
  group_by(clim_region) %>%
  summarise(nr_stations_in_region = n())
  
# Calculate percentage of month observed
prec_quality <- prec %>%
  dplyr::select(INDICATIVO, Year, Month, days_in_month, VALOR) %>%
  group_by(INDICATIVO, Year, Month, days_in_month) %>%
  summarise(observations = n()) %>%
  mutate(Perc_month_observed = observations/days_in_month,
         days_missing = days_in_month - observations) %>%
  left_join(prec) %>%
  mutate(date = as.Date(date, tryFormats = c("%Y-%m-%d")))

# Extract full months without repetitions and calculate monthly precipitation
prec_full <- prec_quality %>%
  filter(Perc_month_observed == 1,
         Year >= 1950, Year <= 2005) %>%
  dplyr::select(INDICATIVO, Year, Month, days_in_month, VALOR) %>%
  group_by(INDICATIVO, Year, Month, days_in_month) %>%
  summarise(OBS_monthly_prec = sum(VALOR)) %>%
  mutate(standard_date = as.Date(paste(Year, Month, "15", sep = "-"),
                                 tryFormats = c("%Y-%m-%d")))

# Extract months with repetitions
prec_repetitions <- prec_quality %>%
  filter(Perc_month_observed > 1,
         Year >= 1950, Year <= 2005)

# Discard repetitions and calculate monthly precipitation
prec_unique_repetitions <- unique(prec_repetitions) %>%
  dplyr::select(INDICATIVO, Year, Month, days_in_month, VALOR) %>%
  group_by(INDICATIVO, Year, Month, days_in_month) %>%
  summarise(OBS_monthly_prec = sum(VALOR)) %>%
  mutate(standard_date = as.Date(paste(Year, Month, "15", sep = "-"),
                                 tryFormats = c("%Y-%m-%d")))

# Extract months with missing values to be filled
# Months which have 5 or more days missing are not considered
whole_series <- data.frame(date = seq(ymd('1950-01-01'),ymd('2005-12-31'), by='days')) %>%
  mutate(Year = year(date), Month = month(date))

prec_missing <- prec_quality %>%
  filter(Perc_month_observed < 1,
         Year >= 1950, Year <= 2005) %>% # Modelled data to compare with only cover this period
  dplyr::select(INDICATIVO, Year, Month, days_missing)
prec_missing <- unique(prec_missing)

months_to_fill <- prec_missing %>%
  dplyr::select(Year, Month)

full_months_to_fill <- months_to_fill %>%
  left_join(whole_series) %>%
  left_join(prec_quality)
  
# Extract specific days with missing values:
days_to_fill <- data.frame()
for (i in seq_along(months_to_fill$INDICATIVO)) {
  temp <- full_months_to_fill %>%
    filter(is.na(VALOR) == TRUE, 
           INDICATIVO == months_to_fill$INDICATIVO[i],
           Year == months_to_fill$Year[i],
           Month == months_to_fill$Month[i])
  days_to_fill <- rbind(days_to_fill, temp)
}

# Fill days with missing values as average among stations within same climate regions
data_to_fill <- days_to_fill %>%
  dplyr::select(INDICATIVO, Year, Month, date) %>%
  left_join(stations_regions)
data_to_fill$FILLED_VALOR <- NA
data_to_fill$VALOR_SD <- NA
data_to_fill$Nr_obs <- NA

filling_data <- prec_quality %>%
  filter(Year >= 1950, Year <= 2005) %>%
  left_join(stations_regions)

# for (i in seq_along(data_to_fill$INDICATIVO)) {
#   temp <- filling_data %>%
#     filter(Year == data_to_fill$Year[i],
#            Month == data_to_fill$Month[i],
#            date == data_to_fill$date[i],
#            clim_region == data_to_fill$clim_region[i])
# 
#   data_to_fill$FILLED_VALOR[i] <- mean(temp$VALOR)
#   data_to_fill$VALOR_SD[i] <- sd(temp$VALOR)
#   data_to_fill$Nr_obs[i] <- length(temp$INDICATIVO)
# }
# write.table(data_to_fill, paste0(di, "data/climate/data_to_fill.txt"), sep = ";")

data_to_fill <- read.table(paste0(di, "data/climate/data_to_fill.txt"), sep = ";") %>%
  mutate(date = as.Date(date))

# Filter out non-acceptable filled values (due to low number of stations, high precipitation, etc)
quality_filter <- data_to_fill %>%
  filter(Nr_obs >= 5,  # At least five stations with data
         FILLED_VALOR <= 2) # Only days with low precipitation are accepted

# Generate full filled months 
filled_months <- quality_filter %>%
  full_join(full_months_to_fill) %>%
  dplyr::select(INDICATIVO, Year, Month, date, FILLED_VALOR, VALOR) %>%
  mutate(Full_VALOR = ifelse(is.na(VALOR), FILLED_VALOR, VALOR))
  
# Calculate monthly precipitation
monthly_prec_filled_months <- filled_months %>%
  dplyr::select(INDICATIVO, Year, Month, Full_VALOR) %>%
  group_by(INDICATIVO, Year, Month) %>%
  summarise(OBS_monthly_prec = sum(Full_VALOR, na.rm = FALSE)) %>%
  mutate(standard_date = as.Date(paste(Year, Month, "15", sep = "-"),
                                 tryFormats = c("%Y-%m-%d"))) %>%
  filter(is.na(OBS_monthly_prec) == FALSE)
  
# Merge full months, fetched months with repetitions and fetched months with missing values
full <- prec_full %>%
  dplyr::select(INDICATIVO, Year, Month, standard_date, OBS_monthly_prec)

fetch_repetitions <- prec_unique_repetitions %>%
  dplyr::select(INDICATIVO, Year, Month, standard_date, OBS_monthly_prec)

fetch_missing <- monthly_prec_filled_months %>%
  dplyr::select(INDICATIVO, Year, Month, standard_date, OBS_monthly_prec)

observed_filled_ts <- rbind(full, fetch_repetitions) 
observed_filled_ts <- rbind(observed_filled_ts, fetch_missing) 

# write.table(observed_filled_ts, paste0(di, "data/climate/filled_observed_prec.txt"), sep = ";")
