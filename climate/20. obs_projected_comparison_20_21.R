# Comparison climate vs projections for years 2020/2021

library(dplyr)
library(rgdal)
library(raster)
library(reshape)
library(lubridate)
library(ggplot2)

di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/data/climate/Datos_estaciones/"

# Data 2020-2021
data_20_21 <- read.table(paste0(di, "DATOS_CLIMA_2020_2021.txt"), header = TRUE, sep = ";", dec = ",") %>%
  mutate(date = as.Date(FECHA, tryFormats = c("%Y-%m-%d")))
stations_names <- data.frame(INDICATIVO = unique(data_20_21$INDICATIVO))

# Load stations coordinates
my_stations_23030 <- read.table(paste(di, "/0. Estaciones_CLIMA.txt", sep = ""), header = TRUE, sep = ";", dec = ",") %>%
  right_join(stations_names)
my_stations_23030_spdf <- SpatialPointsDataFrame(coords = my_stations_23030[,c(5,6)], data = my_stations_23030)
plot(my_stations_23030_spdf)
projection(my_stations_23030_spdf) # Not prj defined
CRS.ED50 <- CRS("+init=epsg:23030 +proj=utm +zone=30 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +units=m +no_defs")
proj4string(my_stations_23030_spdf) <- CRS.ED50
projection(my_stations_23030_spdf)

# writeOGR(obj = my_stations_23030_spdf, dsn = "/Users/maria.suarez.munoz/Downloads/", layer = 'stations_23030"', driver = 'ESRI Shapefile', encoding = "UTF-8", overwrite_layer = TRUE)

# Load climate regions map
# climate_regions <- raster("/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/indata_generation/output_files/ecoregions/try_6_20200403/20200406_4_4_clustering.tif")
climate_regions <- raster("/Users/maria.suarez.munoz/Downloads/20200406_4_4_clustering.tif")
crs(climate_regions) # EPSG:3042

# Reproject stations
all_stations_spdf_3042 <- spTransform(my_stations_23030_spdf, projection(climate_regions))
projection(my_stations_23030_spdf)
projection(all_stations_spdf_3042)

# Extract climate region for each station values 
my_values <- as.matrix(t(raster::extract(climate_regions, all_stations_spdf_3042, method = 'simple'))) 
df <- as.data.frame(my_values)
colnames(df) <- all_stations_spdf_3042@data$INDICATIVO
df$Layer <- rownames(my_values)

# Fetch data
stations_regions <- melt(df)
colnames(stations_regions) <- c("Station", "Climate_area")
stations_regions$Climate_area <- as.factor(stations_regions$Climate_area)

# Calculate monthly prec, tmax, tmin for 2020 and 2021 from selected stations and add a column with climate area

## Subset precipitation data
prec_data <- data_20_21 %>%
  filter(VARIABLE == "PD1", VALIDACION == "R") %>%
  mutate(Year = year(date),
         Month = month(date),
         days_in_month = days_in_month(date)) %>%
  dplyr::select(INDICATIVO, Year, Month, VALOR, days_in_month)

## Quality check: discard stations-month with data missing
prec_gaps_check <- prec_data %>%
  dplyr::select(INDICATIVO, Year, Month, days_in_month, VALOR) %>%
  group_by(INDICATIVO, Year, Month, days_in_month) %>%
  summarise(observations = n()) %>%
  mutate(VALID = ifelse(observations/days_in_month == 1, TRUE, FALSE)) %>%
  filter(VALID == TRUE)

## Calculate accumulated monthly precipitation
monthly_prec <- prec_gaps_check %>%
  left_join(prec_data) %>%
  dplyr::select(INDICATIVO, Year, Month, VALOR) %>%
  group_by(INDICATIVO, Year, Month) %>%
  summarise(Prec = sum(VALOR)) %>%
  left_join(stations_regions, by = c("INDICATIVO" = "Station")) %>%
  mutate(date = as.Date(paste(Year, Month, "15", sep = "-"), tryFormats = c("%Y-%m-%d"))) %>%
  filter(is.na(Climate_area) == FALSE) %>%
  mutate(Source = "observations")

## Plot
ggplot(monthly_prec, aes(x = date, y = Prec, color = INDICATIVO)) +
  geom_point() +
  geom_line() +
  facet_grid(Climate_area ~ .) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  xlab("Date") +
  ylab("Monthly precipitation")

## Subset tmax data
tmax_data <- data_20_21 %>%
  filter(VARIABLE == "TD4",
         VALIDACION == "R" | VALIDACION == "E") %>%
  mutate(Year = year(date),
         Month = month(date),
         days_in_month = days_in_month(date)) %>%
  dplyr::select(INDICATIVO, Year, Month, VALOR, days_in_month)
  
## Quality check: is there data missing?
tmax_gaps_check <- tmax_data %>%
  dplyr::select(INDICATIVO, Year, Month, days_in_month, VALOR) %>%
  group_by(INDICATIVO, Year, Month, days_in_month) %>%
  summarise(observations = n()) %>%
  mutate(VALID = ifelse(observations/days_in_month == 1, TRUE, FALSE)) %>%
  filter(VALID == TRUE)

## Calculate avg monthly tmax
monthly_tmax <- tmax_gaps_check %>%
  left_join(tmax_data) %>%
  dplyr::select(INDICATIVO, Year, Month, VALOR) %>%
  group_by(INDICATIVO, Year, Month) %>%
  summarise(TMax = mean(VALOR)) %>%
  left_join(stations_regions, by = c("INDICATIVO" = "Station")) %>%
  mutate(date = as.Date(paste(Year, Month, "15", sep = "-"), tryFormats = c("%Y-%m-%d"))) %>%
  filter(is.na(Climate_area) == FALSE) %>%
  mutate(Source = "observations")

## Plot
ggplot(monthly_tmax, aes(x = date, y = TMax, color = INDICATIVO)) +
  geom_point() +
  geom_line() +
  facet_grid(Climate_area ~ .) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  xlab("Date") +
  ylab("Monthly maximum temperature")

## Subset tmin data
tmin_data <- data_20_21 %>%
  filter(VARIABLE == "TD2",
         VALIDACION == "R" | VALIDACION == "E") %>%
  mutate(Year = year(date),
         Month = month(date),
         days_in_month = days_in_month(date)) %>%
  dplyr::select(INDICATIVO, Year, Month, VALOR, days_in_month)

## Quality check: is there data missing?
tmin_gaps_check <- tmin_data %>%
  dplyr::select(INDICATIVO, Year, Month, days_in_month, VALOR) %>%
  group_by(INDICATIVO, Year, Month, days_in_month) %>%
  summarise(observations = n()) %>%
  mutate(VALID = ifelse(observations/days_in_month == 1, TRUE, FALSE)) %>%
  filter(VALID == TRUE)

## Calculate avg monthly tmin
monthly_tmin <- tmin_gaps_check %>%
  left_join(tmin_data) %>%
  dplyr::select(INDICATIVO, Year, Month, VALOR) %>%
  group_by(INDICATIVO, Year, Month) %>%
  summarise(TMin = mean(VALOR)) %>%
  left_join(stations_regions, by = c("INDICATIVO" = "Station")) %>%
  mutate(date = as.Date(paste(Year, Month, "15", sep = "-"), tryFormats = c("%Y-%m-%d"))) %>%
  filter(is.na(Climate_area) == FALSE) %>%
  mutate(Source = "observations")

## Plot
ggplot(monthly_tmin, aes(x = date, y = TMin, color = INDICATIVO)) +
  geom_point() +
  geom_line() +
  facet_grid(Climate_area ~ .) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  xlab("Date") +
  ylab("Monthly minimum temperature")
  
# Load projected climate series
di_proj <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/inputs_climate_adjusted/"
files <- list.files(di_proj, pattern = "*_MIROC5_*")
selected_files <- files[c(1,2,3,4,9,10,11,12,13,14,15,16)]

proj_data <- data.frame()
for(i in 1:length(selected_files)) {
  temp <- read.table(paste0(di_proj, selected_files[i]), header = TRUE) %>%
    mutate(Scenario = strsplit(selected_files[i], split = "_")[[1]][1],
           Model = strsplit(selected_files[i], split = "_")[[1]][2],
           Region = strsplit(selected_files[i], split = "_")[[1]][3],
           Climate_area = strsplit(Region, split = "g")[[1]][2])
  proj_data <- rbind(proj_data, temp)
}

## Subset proj_data and fetch for plotting
proj_data_20_21 <- proj_data %>%
  filter(Year == 2020 | Year == 2021) %>%
  mutate(date = as.Date(paste(Year, Month, "15", sep = "-"), tryFormats = c("%Y-%m-%d")))

proj_prec_20_21 <- proj_data_20_21 %>%
  dplyr::select(Scenario, Climate_area, date, Prec) %>%
  mutate(Source = Scenario)

proj_tmax_20_21 <- proj_data_20_21 %>%
  dplyr::select(Scenario, Climate_area, date, TMax) %>%
  mutate(Source = Scenario)

proj_tmin_20_21 <- proj_data_20_21 %>%
  dplyr::select(Scenario, Climate_area, date, TMin) %>%
  mutate(Source = Scenario)

# Plot Precipitation
subset_prec_obs <- monthly_prec[,-c(1)] %>%
  filter(Climate_area == 2) %>%
  group_by(Year, Month, Climate_area, date, Source) %>%
  summarise(Prec_avg = mean(Prec),
            SD = sd(Prec))

subset_prec_proj <- proj_prec_20_21 %>%
  filter(Climate_area == 2)

cols <- c('#377eb8', #blue
          '#984ea3', #purple
          '#e41a1c',
          '#1b9e77') 

jpeg(file = "/Volumes/GoogleDrive/My Drive/proj_LANDIS/experiments/211129_outputs/prec_comparison_obs_proj_20_21.jpeg", 
     width=6, height=4, units="in", res=300)
ggplot() +
  geom_point(data = subset_prec_obs, aes(x = date, y = Prec_avg, group = Source)) +
  geom_line(data = subset_prec_proj, aes(x = date, y = Prec, color = Source, group = Source)) +
  geom_errorbar(data = subset_prec_obs, 
                aes(x = date, ymin = Prec_avg - SD,
                    ymax = Prec_avg + SD),
                width= .2, position = position_dodge(.05)) +
  # stat_smooth(data = subset_prec_obs, aes(x = date, y = Prec), method = "loess", color = "black", size = 0.5) +
  # facet_grid(Climate_area ~ .) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  scale_color_manual(values = cols) +
  xlab("Date") +
  ylab("Monthly precipitation")
dev.off()

# Plot TMax
subset_tmax_obs <- monthly_tmax %>%
  filter(Climate_area == 2)

subset_tmax_proj <- proj_tmax_20_21 %>%
  filter(Climate_area == 2)

ggplot() +
  geom_point(data = subset_tmax_obs, aes(x = date, y = TMax, color = Source)) +
  geom_line(data = subset_tmax_proj, aes(x = date, y = TMax, color = Source)) +
  facet_grid(Climate_area ~ .) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  xlab("Date") +
  ylab("Maximum temperature")

# Plot TMin
subset_tmin_obs <- monthly_tmin %>%
  filter(Climate_area == 2)

subset_tmin_proj <- proj_tmin_20_21 %>%
  filter(Climate_area == 2)

ggplot() +
  geom_point(data = subset_tmin_obs, aes(x = date, y = TMin, color = Source)) +
  geom_line(data = subset_tmin_proj, aes(x = date, y = TMin, color = Source)) +
  facet_grid(Climate_area ~ .) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  xlab("Date") +
  ylab("Minimum temperature")

