## Generate full LANDIS climate time series for current climate scenario

library(dplyr)
library(ggplot2)
library(lubridate)

di_envidat <- "/Volumes/Data/Climatic_Data/EnviDat/envicloud/chelsa/chelsa_V1/climregions_tables/"
di_inputs <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/data/climate/"
# di_outputs <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/simulations/inputs_climate/"
di_outputs <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/inputs_climate/"

## Create full series for envidat data ##
# Load historical Tmax series
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

# Load historical Tmin series
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
    dplyr::select(date, Year, Month, Model, Period, Tmin_reg1_C, Tmin_reg2_C, Tmin_reg3_C, Tmin_reg4_C)
  tmin_data <- rbind(tmin_data, temp)
}

# Load historical Prec series
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

# Merge historical envidat data
envidat_data <- full_join(tmax_data, tmin_data) %>% 
  full_join(prec_data)  %>%
  select(date, Year, Month, Model, 
         Tmax_reg1_C, Tmax_reg2_C, Tmax_reg3_C, Tmax_reg4_C,
         Tmin_reg1_C, Tmin_reg2_C, Tmin_reg3_C, Tmin_reg4_C,
         Prec_reg1_monthsum, Prec_reg2_monthsum, Prec_reg3_monthsum, Prec_reg4_monthsum)

# Create random series for years 2005-2100
random_series <- data.frame(Sim_year = c(2006:2100),
                            Orig_year = sample(c(1950:2005), length(2006:2100), replace = TRUE)) %>%
  left_join(envidat_data, by = c("Orig_year" = "Year")) %>%
  rename(Year = Sim_year) %>%
  select(-Orig_year) %>%
  select(date, Year, Month, Model, 
         Tmax_reg1_C, Tmax_reg2_C, Tmax_reg3_C, Tmax_reg4_C,
         Tmin_reg1_C, Tmin_reg2_C, Tmin_reg3_C, Tmin_reg4_C,
         Prec_reg1_monthsum, Prec_reg2_monthsum, Prec_reg3_monthsum, Prec_reg4_monthsum)

# Load past series (1800-1949)
past_envidat <- read.table(paste(di_inputs, "past_envidat_series.txt", sep = ""), sep = " ") %>%
  mutate(date = as.Date(paste(Year, Month, "15", sep = "-")),
         Period = "past") %>%
  select(date, Year, Month, Model, 
         Tmax_reg1_C, Tmax_reg2_C, Tmax_reg3_C, Tmax_reg4_C,
         Tmin_reg1_C, Tmin_reg2_C, Tmin_reg3_C, Tmin_reg4_C,
         Prec_reg1_monthsum, Prec_reg2_monthsum, Prec_reg3_monthsum, Prec_reg4_monthsum)

# Merge past, historical and futute envidat series
current_clim_envidat <- rbind(past_envidat, envidat_data) %>%
  rbind(random_series) %>%
  select(-date)

# Load PAR data and merge
par_data <- read.csv(paste(di_inputs, "PAR_timeseries.csv", sep = ""), sep = ",") %>%
  dplyr::select(-X, -date, Year, Month, PAR_reg1, PAR_reg2, PAR_reg3, PAR_reg4)

par_data_MIROC <- par_data %>% mutate(Model = "MIROC5")
par_data_CMCC <- par_data %>% mutate(Model = "CMCC-CM")
par_data_CESM1 <- par_data %>% mutate(Model = "CESM1-BGC")
par_data_ACCESS1 <- par_data %>% mutate(Model = "ACCESS1-3")

full_par_data <- rbind(par_data_MIROC, par_data_CMCC, par_data_CESM1, par_data_ACCESS1)

full_data <- full_join(current_clim_envidat, full_par_data)

# Load CO2 scenario and merge
CO2 <- read.table(paste(di_inputs, "CO2_timeseries.txt", sep = ""), sep = ";") %>%
  select(Year, Month, current) %>%
  rename(CO2 = current)

current_clim_sc <- full_join(full_data, CO2)

# Subset by model 
# current_ACCESS <- current_clim_sc %>% filter(Model == "ACCESS1-3") %>% dplyr::select(-Model) 
# current_CESM1 <- current_clim_sc %>% filter(Model == "CESM1-BGC") %>% dplyr::select(-Model)
# current_CMCC <- current_clim_sc %>% filter(Model == "CMCC-CM") %>% dplyr::select(-Model)
current_MIROC5 <- current_clim_sc %>% filter(Model == "MIROC5") %>% dplyr::select(-Model)

# Subset by region
# current_ACCESS_reg1 <- current_ACCESS %>% dplyr::select(Year, Month, Tmax_reg1_C, Tmin_reg1_C, PAR_reg1, Prec_reg1_monthsum, CO2) %>% rename(TMax = Tmax_reg1_C, TMin = Tmin_reg1_C, PAR = PAR_reg1, Prec = Prec_reg1_monthsum)
# current_CESM1_reg1 <- current_CESM1 %>% dplyr::select(Year, Month, Tmax_reg1_C, Tmin_reg1_C, PAR_reg1, Prec_reg1_monthsum, CO2) %>% rename(TMax = Tmax_reg1_C, TMin = Tmin_reg1_C, PAR = PAR_reg1, Prec = Prec_reg1_monthsum)
# current_CMCC_reg1 <- current_CMCC %>% dplyr::select(Year, Month, Tmax_reg1_C, Tmin_reg1_C, PAR_reg1, Prec_reg1_monthsum, CO2) %>% rename(TMax = Tmax_reg1_C, TMin = Tmin_reg1_C, PAR = PAR_reg1, Prec = Prec_reg1_monthsum)
current_MIROC5_reg1 <- current_MIROC5 %>% dplyr::select(Year, Month, Tmax_reg1_C, Tmin_reg1_C, PAR_reg1, Prec_reg1_monthsum, CO2) %>% rename(TMax = Tmax_reg1_C, TMin = Tmin_reg1_C, PAR = PAR_reg1, Prec = Prec_reg1_monthsum)

# current_ACCESS_reg2 <- current_ACCESS %>% dplyr::select(Year, Month, Tmax_reg2_C, Tmin_reg2_C, PAR_reg2, Prec_reg2_monthsum, CO2) %>% rename(TMax = Tmax_reg2_C, TMin = Tmin_reg2_C, PAR = PAR_reg2, Prec = Prec_reg2_monthsum)
# current_CESM2_reg2 <- current_CESM1 %>% dplyr::select(Year, Month, Tmax_reg2_C, Tmin_reg2_C, PAR_reg2, Prec_reg2_monthsum, CO2) %>% rename(TMax = Tmax_reg2_C, TMin = Tmin_reg2_C, PAR = PAR_reg2, Prec = Prec_reg2_monthsum)
# current_CMCC_reg2 <- current_CMCC %>% dplyr::select(Year, Month, Tmax_reg2_C, Tmin_reg2_C, PAR_reg2, Prec_reg2_monthsum, CO2) %>% rename(TMax = Tmax_reg2_C, TMin = Tmin_reg2_C, PAR = PAR_reg2, Prec = Prec_reg2_monthsum)
current_MIROC5_reg2 <- current_MIROC5 %>% dplyr::select(Year, Month, Tmax_reg2_C, Tmin_reg2_C, PAR_reg2, Prec_reg2_monthsum, CO2) %>% rename(TMax = Tmax_reg2_C, TMin = Tmin_reg2_C, PAR = PAR_reg2, Prec = Prec_reg2_monthsum)

# current_ACCESS_reg3 <- current_ACCESS %>% dplyr::select(Year, Month, Tmax_reg3_C, Tmin_reg3_C, PAR_reg3, Prec_reg3_monthsum, CO2) %>% rename(TMax = Tmax_reg3_C, TMin = Tmin_reg3_C, PAR = PAR_reg3, Prec = Prec_reg3_monthsum)
# current_CESM3_reg3 <- current_CESM1 %>% dplyr::select(Year, Month, Tmax_reg3_C, Tmin_reg3_C, PAR_reg3, Prec_reg3_monthsum, CO2) %>% rename(TMax = Tmax_reg3_C, TMin = Tmin_reg3_C, PAR = PAR_reg3, Prec = Prec_reg3_monthsum)
# current_CMCC_reg3 <- current_CMCC %>% dplyr::select(Year, Month, Tmax_reg3_C, Tmin_reg3_C, PAR_reg3, Prec_reg3_monthsum, CO2) %>% rename(TMax = Tmax_reg3_C, TMin = Tmin_reg3_C, PAR = PAR_reg3, Prec = Prec_reg3_monthsum)
current_MIROC5_reg3 <- current_MIROC5 %>% dplyr::select(Year, Month, Tmax_reg3_C, Tmin_reg3_C, PAR_reg3, Prec_reg3_monthsum, CO2) %>% rename(TMax = Tmax_reg3_C, TMin = Tmin_reg3_C, PAR = PAR_reg3, Prec = Prec_reg3_monthsum)

# current_ACCESS_reg4 <- current_ACCESS %>% dplyr::select(Year, Month, Tmax_reg4_C, Tmin_reg4_C, PAR_reg4, Prec_reg4_monthsum, CO2) %>% rename(TMax = Tmax_reg4_C, TMin = Tmin_reg4_C, PAR = PAR_reg4, Prec = Prec_reg4_monthsum)
# current_CESM4_reg4 <- current_CESM1 %>% dplyr::select(Year, Month, Tmax_reg4_C, Tmin_reg4_C, PAR_reg4, Prec_reg4_monthsum, CO2) %>% rename(TMax = Tmax_reg4_C, TMin = Tmin_reg4_C, PAR = PAR_reg4, Prec = Prec_reg4_monthsum)
# current_CMCC_reg4 <- current_CMCC %>% dplyr::select(Year, Month, Tmax_reg4_C, Tmin_reg4_C, PAR_reg4, Prec_reg4_monthsum, CO2) %>% rename(TMax = Tmax_reg4_C, TMin = Tmin_reg4_C, PAR = PAR_reg4, Prec = Prec_reg4_monthsum)
current_MIROC5_reg4 <- current_MIROC5 %>% dplyr::select(Year, Month, Tmax_reg4_C, Tmin_reg4_C, PAR_reg4, Prec_reg4_monthsum, CO2) %>% rename(TMax = Tmax_reg4_C, TMin = Tmin_reg4_C, PAR = PAR_reg4, Prec = Prec_reg4_monthsum)

# Export tables
# write.table(current_ACCESS_reg1, paste(di_outputs, "current_ACCESS_reg1.txt", sep = ""), row.names = FALSE)
# write.table(current_CESM1_reg1, paste(di_outputs, "current_CESM1_reg1.txt", sep = ""), row.names = FALSE)
# write.table(current_CMCC_reg1, paste(di_outputs, "current_CMCC_reg1.txt", sep = ""), row.names = FALSE)
# write.table(current_MIROC5_reg1, paste(di_outputs, "current_MIROC5_reg1.txt", sep = ""), row.names = FALSE)
# 
# write.table(current_ACCESS_reg2, paste(di_outputs, "current_ACCESS_reg2.txt", sep = ""), row.names = FALSE)
# write.table(current_CESM2_reg2, paste(di_outputs, "current_CESM2_reg2.txt", sep = ""), row.names = FALSE)
# write.table(current_CMCC_reg2, paste(di_outputs, "current_CMCC_reg2.txt", sep = ""), row.names = FALSE)
# write.table(current_MIROC5_reg2, paste(di_outputs, "current_MIROC5_reg2.txt", sep = ""), row.names = FALSE)
# 
# write.table(current_ACCESS_reg3, paste(di_outputs, "current_ACCESS_reg3.txt", sep = ""), row.names = FALSE)
# write.table(current_CESM3_reg3, paste(di_outputs, "current_CESM3_reg3.txt", sep = ""), row.names = FALSE)
# write.table(current_CMCC_reg3, paste(di_outputs, "current_CMCC_reg3.txt", sep = ""), row.names = FALSE)
# write.table(current_MIROC5_reg3, paste(di_outputs, "current_MIROC5_reg3.txt", sep = ""), row.names = FALSE)
# 
# write.table(current_ACCESS_reg4, paste(di_outputs, "current_ACCESS_reg4.txt", sep = ""), row.names = FALSE)
# write.table(current_CESM4_reg4, paste(di_outputs, "current_CESM4_reg4.txt", sep = ""), row.names = FALSE)
# write.table(current_CMCC_reg4, paste(di_outputs, "current_CMCC_reg4.txt", sep = ""), row.names = FALSE)
# write.table(current_MIROC5_reg4, paste(di_outputs, "current_MIROC5_reg4.txt", sep = ""), row.names = FALSE)

write.table(current_MIROC5_reg1, paste(di_outputs, "new_current_MIROC5_reg1.txt", sep = ""), row.names = FALSE)
write.table(current_MIROC5_reg2, paste(di_outputs, "new_current_MIROC5_reg2.txt", sep = ""), row.names = FALSE)
write.table(current_MIROC5_reg3, paste(di_outputs, "new_current_MIROC5_reg3.txt", sep = ""), row.names = FALSE)
write.table(current_MIROC5_reg4, paste(di_outputs, "new_current_MIROC5_reg4.txt", sep = ""), row.names = FALSE)
