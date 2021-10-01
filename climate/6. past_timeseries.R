## Generate LANDIS climate time series

library(dplyr)
library(ggplot2)
library(lubridate)

di_envidat <- "/Volumes/Data/Climatic_Data/EnviDat/envicloud/chelsa/chelsa_V1/climregions_tables/"
di_inputs <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/data/climate/"

# Load Tmax series
tmax_files <- list.files(di_envidat, pattern = "*_tasmax_historical_*")
tmax_data <- data.frame()
for (i in 1:length(tmax_files)) {
  name <- tmax_files[i]
  model <- strsplit(name, split = "_")[[1]][1]
  period <- strsplit(name, split = "_")[[1]][3]
  temp <- read.csv(paste(di_envidat, tmax_files[i], sep = ""), sep = " ") %>%
    mutate(Model = model, Period = period,
           date = as.Date(paste(Year, Month, "15", sep = "-"), tryFormats = c("%Y-%m-%d")),
           Tmax_reg1_C = tmax_region_1 - 273.15,
           Tmax_reg2_C = tmax_region_2 - 273.15,
           Tmax_reg3_C = tmax_region_3 - 273.15,
           Tmax_reg4_C = tmax_region_4 - 273.15) %>%
    dplyr::select(date, Year, Month, Model, Period, Tmax_reg1_C, Tmax_reg2_C, Tmax_reg3_C, Tmax_reg4_C)
  tmax_data <- rbind(tmax_data, temp)
}

# Load Tmin series
tmin_files <- list.files(di_envidat, pattern = "*_tasmin_historical_*")
tmin_data <- data.frame()
for (i in 1:length(tmin_files)) {
  name <- tmin_files[i]
  model <- strsplit(name, split = "_")[[1]][1]
  period <- strsplit(name, split = "_")[[1]][3]
  temp <- read.csv(paste(di_envidat, tmin_files[i], sep = ""), sep = " ") %>%
    mutate(Model = model, Period = period,
           date = as.Date(paste(Year, Month, "15", sep = "-"), tryFormats = c("%Y-%m-%d")),
           Tmin_reg1_C = tmin_region_1 - 273.15,
           Tmin_reg2_C = tmin_region_2 - 273.15,
           Tmin_reg3_C = tmin_region_3 - 273.15,
           Tmin_reg4_C = tmin_region_4 - 273.15) %>%
    dplyr::select(date, Year, Month, Model, Period, 
                  Tmin_reg1_C, Tmin_reg2_C,
                  Tmin_reg3_C, Tmin_reg4_C)
  tmin_data <- rbind(tmin_data, temp)
}

# Load Prec series
prec_files <- list.files(di_envidat, pattern = "*_pr_historical_*")
prec_data <- data.frame()
for (i in 1:length(prec_files)) {
  name <- prec_files[i]
  model <- strsplit(name, split = "_")[[1]][1]
  period <- strsplit(name, split = "_")[[1]][3]
  temp <- read.csv(paste(di_envidat, prec_files[i], sep = ""), sep = " ") %>%
    mutate(Model = model,
           Period = period,
           date = as.Date(paste(Year, Month, "15", sep = "-"), tryFormats = c("%Y-%m-%d")),
           month_start_date = ymd(paste(Year, Month, "01", sep = "/")) ,
           next_month_start_date = month_start_date + months(1),
           nr_days_in_month = as.integer(as.duration(interval(month_start_date, next_month_start_date)) / 86400),
           Prec_reg1_monthsum = prec_region_1*86400*nr_days_in_month,
           Prec_reg2_monthsum = prec_region_2*86400*nr_days_in_month,
           Prec_reg3_monthsum = prec_region_3*86400*nr_days_in_month,
           Prec_reg4_monthsum = prec_region_4*86400*nr_days_in_month) %>%
    dplyr::select(date, Year, Month, Model, Period, Prec_reg1_monthsum, Prec_reg2_monthsum, Prec_reg3_monthsum, Prec_reg4_monthsum)
  prec_data <- rbind(prec_data, temp)
}

# Merge envidat data
envidat_data <- full_join(tmax_data, tmin_data) %>% 
  full_join(prec_data)

# Generate climate as historical mean
years <- unique(envidat_data$Year)

hist_mean <- envidat_data %>%
  select(-Year, -Period, -date) %>%
  group_by(Month, Model) %>%
  summarise_all(mean)

# Create past series from historical mean
years <- data.frame(Year = c(1800:1949))

past_series <- merge(years, hist_mean)
  length(years$Year)

# Export
write.table(past_series, paste(di_inputs, "past_envidat_series.txt", sep = ""))

