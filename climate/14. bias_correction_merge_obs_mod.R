# GENERATE TABLE OF DATA COMPARISON FOR BIAS CORRECTION ON PRECIPITATION DATA:
# MERGE OBSERVATIONS AND MODELLED DATA

### SETUP ### 
library(raster)
library(dplyr)
library(reshape)
library(tidyverse)
library(lubridate)

di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/"
folder_harddisk <- "/Volumes/Data/Climatic_Data/EnviDat/envicloud/chelsa/chelsa_V1/"

proj3042 <- CRS(SRS_string="EPSG:3042")

### CLIMATE REGIONS ###
# Load map of climate regions
regions <- raster(paste0(di, "data/climate/clim_regions_20200406.tif")) # EPSG: 3042
crs(regions) <- proj3042

### OBSERVED AND FILLED DATA ###
obs <- read.table(paste0(di, "data/climate/filled_observed_prec.txt"), sep = ";") %>%
  dplyr::select(INDICATIVO, standard_date, OBS_monthly_prec) %>%
  mutate(standard_date = as.Date(standard_date, tryFormats = c("%Y-%m-%d")))

# Load stations data 
stations <- read.table(paste(di, "data/climate/Datos_estaciones/estaciones_en_aoi.csv", sep = ""), header = TRUE, sep = ",") # EPSG: 3042
stations_spdf <- SpatialPointsDataFrame(coords = stations[,c(6,7)], data = stations)
crs(stations_spdf) <- proj3042

# Extract climate region for each station
stations_regions <- as.matrix(t(raster::extract(regions, stations_spdf, method = 'simple'))) 
stations_regions <- as.data.frame(stations_regions)
colnames(stations_regions) <- stations_spdf@data$INDICATIVO
stations_regions <- melt(stations_regions)
colnames(stations_regions) <- c("INDICATIVO", "clim_region")

### MODELLED DATA ###
# Load stack from models and fetch data
## ACCESS1
my_stack <- stack(paste(folder_harddisk, "pr_ACCESS1-3_historical_1950_2005.nc", sep = ""), varname = "precipitation_flux")
# Extract modelled values 
my_values <- as.matrix(t(raster::extract(my_stack, stations_spdf, method = 'simple'))) 
ACCESS1_df <- as.data.frame(my_values)
colnames(ACCESS1_df) <- stations_spdf@data$INDICATIVO
ACCESS1_df$Layer <- rownames(my_values)
# Fetch data
ACCESS1_modelled <- melt(ACCESS1_df, id.vars = c("Layer"))
colnames(ACCESS1_modelled) <- c("Layer", "INDICATIVO", "ACCESS1_month_prec_mm_s")

## CESM1
my_stack <- stack(paste(folder_harddisk, "pr_CESM1-BGC_historical_1950_2005.nc", sep = ""), varname = "precipitation_flux")
# Extract modelled values 
my_values <- as.matrix(t(raster::extract(my_stack, stations_spdf, method = 'simple'))) 
CESM1_df <- as.data.frame(my_values)
colnames(CESM1_df) <- stations_spdf@data$INDICATIVO
CESM1_df$Layer <- rownames(my_values)
# Fetch data
CESM1_modelled <- melt(CESM1_df, id.vars = c("Layer"))
colnames(CESM1_modelled) <- c("Layer", "INDICATIVO", "CESM1_month_prec_mm_s")

## CMCC-CM
my_stack <- stack(paste(folder_harddisk, "pr_CMCC-CM_historical_1950_2005.nc", sep = ""), varname = "precipitation_flux")
# Extract modelled values 
my_values <- as.matrix(t(raster::extract(my_stack, stations_spdf, method = 'simple'))) 
CMCC_df <- as.data.frame(my_values)
colnames(CMCC_df) <- stations_spdf@data$INDICATIVO
CMCC_df$Layer <- rownames(my_values)
# Fetch data
CMCC_modelled <- melt(CMCC_df, id.vars = c("Layer"))
colnames(CMCC_modelled) <- c("Layer", "INDICATIVO", "CMCC_month_prec_mm_s")

## MIROC5
my_stack <- stack(paste(folder_harddisk, "pr_MIROC5_historical_1950_2005.nc", sep = ""), varname = "precipitation_flux")
# Extract modelled values 
my_values <- as.matrix(t(raster::extract(my_stack, stations_spdf, method = 'simple'))) 
MIROC_df <- as.data.frame(my_values)
colnames(MIROC_df) <- stations_spdf@data$INDICATIVO
MIROC_df$Layer <- rownames(my_values)
# Fetch data
MIROC_modelled <- melt(MIROC_df, id.vars = c("Layer"))
colnames(MIROC_modelled) <- c("Layer", "INDICATIVO", "MIROC_month_prec_mm_s")

# Merge modelled data 
mod_data <- ACCESS1_modelled %>%
  full_join(CMCC_modelled) %>%
  full_join(CESM1_modelled) %>%
  full_join(MIROC_modelled) %>%
  mutate(date0 = str_replace_all(Layer, "X", ""),
         date1 = str_replace_all(date0, "[^[:alnum:]]", "-"),
         standard_date = as.Date(date1, 
                                 tryFormats = c("%Y-%m-%d"))) %>%
  dplyr::select(-Layer, -date0, -date1)

### MERGE AND FETCH TABLE FOR BIAS CORRECTION ###
obs_mod_data <- inner_join(mod_data, obs) %>% # Merge observations and modelled data
  mutate(days_in_month = days_in_month(standard_date),
         Year = year(standard_date),
         Month = month(standard_date)) %>%
  mutate(MOD_ACCESS = ACCESS1_month_prec_mm_s*86400*days_in_month, # Calculate modelled monthly precipitation
         MOD_CESM1 = CESM1_month_prec_mm_s*86400*days_in_month,
         MOD_CMCC = CMCC_month_prec_mm_s*86400*days_in_month,
         MOD_MIROC5 = MIROC_month_prec_mm_s*86400*days_in_month) %>%
  dplyr::select(INDICATIVO, Year, Month, 
                OBS_monthly_prec,
                MOD_ACCESS, MOD_CESM1, MOD_CMCC, MOD_MIROC5) %>%
  left_join(stations_regions) # Add column for climate region

# Calculate average 
avg_region <- obs_mod_data %>%
  dplyr::select(-INDICATIVO) %>%
  group_by(clim_region, Year, Month) %>%
  summarise_all(mean) %>%
  filter(is.na(clim_region) == FALSE)
  
# Export
write.table(avg_region, paste0(di, "data/climate/obs_mod_data.txt"), sep = ";")
