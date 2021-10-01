# Generate CO2 time series for LANDIS

library(dplyr)
library(ggplot2)
library(lubridate)

di_inputs <- "/Users/maria.suarez.munoz/Google Drive/Apuntes/Landis_II/archivos_marco_mina/"
di_outputs <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/data/climate/"

# Load Marco's time series and generate individual years
past_years <- data.frame(Period = "1800-1899", New_Year = 1800:1899)

current <- read.table(paste(di_inputs, "24. clim_reg1_current.txt", sep = ""), header = TRUE) %>%
  left_join(past_years, by = c("Year" = "Period")) %>%
  mutate(Year = as.character(Year),
    Final_year = ifelse(is.na(New_Year), Year, New_Year),
    date = as.Date(paste(Final_year, Month, "15", sep = "-"))) %>%
  filter(Year < 2101) %>%
  select(date, Final_year, Month, CO2) %>%
  rename(Year = Final_year,
         current = CO2)

rcp45 <- read.table(paste(di_inputs, "23. clim_reg1_rcp45.txt", sep = ""), header = TRUE) %>%
  left_join(past_years, by = c("Year" = "Period")) %>%
  mutate(Year = as.character(Year),
         Final_year = ifelse(is.na(New_Year), Year, New_Year),
         date = as.Date(paste(Final_year, Month, "15", sep = "-"))) %>%
  filter(Year < 2101) %>%
  select(date, Final_year, Month, CO2) %>%
  rename(Year = Final_year,
         rcp45 = CO2)

rcp85 <- read.table(paste(di_inputs, "25. clim_reg1_rcp85.txt", sep = ""), header = TRUE) %>%
  left_join(past_years, by = c("Year" = "Period")) %>%
  mutate(Year = as.character(Year),
         Final_year = ifelse(is.na(New_Year), Year, New_Year),
         date = as.Date(paste(Final_year, Month, "15", sep = "-"))) %>%
  filter(Year < 2101) %>%
  select(date, Final_year, Month, CO2) %>%
  rename(Year = Final_year,
         rcp85 = CO2)

# Plot
ggplot() +
  geom_line(data = current, aes(x = date, y = current, color = "current")) +
  geom_line(data = rcp45, aes(x = date, y = rcp45, color = "rcp45")) +
  geom_line(data = rcp85, aes(x = date, y = rcp85, color = "rcp85"))

# Merge timeseries
CO2_ts <- full_join(current, rcp45) %>%
  full_join(rcp85)

# Export
write.table(CO2_ts, paste(di_outputs, "CO2_timeseries.txt", sep = ""), sep = ";")
