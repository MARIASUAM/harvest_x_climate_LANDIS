# Climate figure for manuscript

# Data used: MIROC5 model with biascorrected precipitation data for climate region 3 (where most pine plantations fall)

## SETUP
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(reshape2)

di_inputs <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/inputs_climate_adjusted/"
di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/211129_outputs/"

cols <- c('#377eb8', #blue
          '#984ea3', #purple
          '#e41a1c') #red

scenario.labs <- data.frame(scenario_original = c("current", "rcp45", "rcp85"),
                            scenario = c("Current", "RCP4.5", "RCP8.5"))

lines <- c("solid", "dotdash", "dashed")
lines <- c("Current" = "solid", "RCP4.5" = "dotdash", "RCP8.5" = "dashed")

# Load data and fetch labels
files <- list.files(di_inputs, pattern = "*_MIROC5_reg3")
files <- files[c(1, 3, 4)]
series <- data.frame()
for(i in 1:length(files)) {
  temp <- read.table(paste(di_inputs, files[i], sep = ""), sep = " ", header = TRUE) %>%
    mutate(scenario = strsplit(files[i], split = "_")[[1]][1], 
           Tavg_C = (TMin + TMax)/2)
  series <- rbind(series, temp)
}

series <- series %>%
  rename(scenario_original = scenario) %>%
  left_join(scenario.labs) %>%
  select(-scenario_original)

# Calculate annual means for PAR, CO2 and temperaures
annual_means <- series %>%
  select(scenario, Year, PAR, CO2, Tavg_C, TMin, TMax) %>%
  group_by(scenario, Year) %>%
  summarise_all(mean) %>%
  melt(id.vars = c("scenario", "Year"))

# Extract mean temperature
avg_temp <- annual_means %>%
  filter(variable == "Tavg_C")

# Calculate annual precipitation (sum)
annual_sums <- series %>%
  select(scenario, Year, Prec) %>%
  group_by(scenario, Year) %>%
  summarise_all(sum) %>%
  melt(id.vars = c("scenario", "Year"))

# Plot - 2 axis
coef <- 120
jpeg(file = paste(di, "MIROC5_reg3_temp_prec.jpeg", sep = ""), width=6, height=4, units="in", res=300)
ggplot(data = avg_temp, aes(color = scenario)) +
  # Temperature
  geom_point(data = avg_temp, aes(x = Year, y = value), size = 0.5, alpha = 0.5) +
  geom_line(data = avg_temp, aes(x = Year, y = value), size = 0.3, linetype = "longdash") +
  stat_smooth(data = avg_temp, aes(x = Year, y = value), method = "loess") +
  # Precipitation
  geom_point(data = annual_sums, aes(x = Year, y = value/coef), size = 0.5, alpha = 0.5) +
  geom_line(data = annual_sums, aes(x = Year, y = value/coef), size = 0.3, linetype = "dotted") +
  stat_smooth(data = annual_sums, aes(x = Year, y = value/coef), method = "loess") +
  scale_y_continuous(
    name = "Average temperature (Celsius °)",
    sec.axis = sec_axis(~.*coef, name="Total precipitation (mm)")
  ) +
  xlim(2005, 2100) +
  theme_classic() +
  # ggtitle("Region 3") +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines)
dev.off()

# Plot - 1 axis, 2 pannels
par(mfrow= c(1,2))
jpeg(file = paste(di, "MIROC5_reg3_temp_prec_two_pannels.jpeg", sep = ""), width = 6, height = 8, units="in", res=300)
# Temperature
temp <- ggplot(data = avg_temp, aes(color = scenario)) +
  geom_point(data = avg_temp, aes(x = Year, y = value), size = 0.5, alpha = 0.5) +
  geom_line(data = avg_temp, aes(x = Year, y = value), size = 0.3, linetype = "longdash") +
  stat_smooth(data = avg_temp, aes(x = Year, y = value), method = "loess") +
  xlim(2005, 2100) +
  theme_classic() +
  scale_y_continuous(name = "Average temperature (°C)") +
  # ggtitle("Region 3") +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines)

# Precipitation
prec <- ggplot(data = avg_temp, aes(color = scenario)) +
  geom_point(data = annual_sums, aes(x = Year, y = value), size = 0.5, alpha = 0.5) +
  geom_line(data = annual_sums, aes(x = Year, y = value), size = 0.3, linetype = "dotted") +
  stat_smooth(data = annual_sums, aes(x = Year, y = value), method = "loess") +
  scale_y_continuous(name = "Precipitation (mm)") +
  xlim(2005, 2100) +
  theme_classic() +
  # ggtitle("Region 3") +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines)
ggarrange(temp, prec,labels = c("A", "B"), ncol = 1, nrow = 2)
dev.off()

# One-month plot
jpeg(file = paste(di, "MIROC5_reg3_temp_february.jpeg", sep = ""), width = 6, height = 6, units="in", res=300)
series %>%
  filter(Month == 2) %>%
  ggplot(aes(color = scenario)) +
  geom_point(aes(x = Year, y = Tavg_C), size = 0.5, alpha = 0.5) +
  geom_line(aes(x = Year, y = Tavg_C), size = 0.3, linetype = "longdash") +
  stat_smooth(aes(x = Year, y = Tavg_C), method = "loess") +
  xlim(2005, 2100) +
  theme_classic() +
  scale_y_continuous(name = "Average temperature (°C)") +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines)
dev.off()

# CESM model
# Load data and fetch labels
files <- list.files(di_inputs, pattern = "*_CESM1_reg3")
files <- files[c(1, 3, 4)]
series <- data.frame()
for(i in 1:length(files)) {
  temp <- read.table(paste(di_inputs, files[i], sep = ""), sep = " ", header = TRUE) %>%
    mutate(scenario = strsplit(files[i], split = "_")[[1]][1], 
           Tavg_C = (TMin + TMax)/2)
  series <- rbind(series, temp)
}

series <- series %>%
  rename(scenario_original = scenario) %>%
  left_join(scenario.labs) %>%
  select(-scenario_original)

# Calculate annual means for PAR, CO2 and temperatures
annual_means <- series %>%
  select(scenario, Year, PAR, CO2, Tavg_C, TMin, TMax) %>%
  group_by(scenario, Year) %>%
  summarise_all(mean) %>%
  melt(id.vars = c("scenario", "Year"))

# Extract mean temperature
avg_temp <- annual_means %>%
  filter(variable == "Tavg_C")

# Calculate annual precipitation (sum)
annual_sums <- series %>%
  select(scenario, Year, Prec) %>%
  group_by(scenario, Year) %>%
  summarise_all(sum) %>%
  melt(id.vars = c("scenario", "Year"))

# Plot
coef <- 100
jpeg(file = paste(di, "CESM1_reg3_temp_prec.jpeg", sep = ""), width=6, height=4, units="in", res=300)
ggplot(data = avg_temp, aes(color = scenario)) +
  # Temperature
  geom_point(data = avg_temp, aes(x = Year, y = value), size = 0.5, alpha = 0.5) +
  geom_line(data = avg_temp, aes(x = Year, y = value), size = 0.3, linetype = "longdash") +
  stat_smooth(data = avg_temp, aes(x = Year, y = value), method = "loess") +
  # Precipitation
  geom_point(data = annual_sums, aes(x = Year, y = value/coef), size = 0.5, alpha = 0.5) +
  geom_line(data = annual_sums, aes(x = Year, y = value/coef), size = 0.3, linetype = "dotted") +
  stat_smooth(data = annual_sums, aes(x = Year, y = value/coef), method = "loess") +
  scale_y_continuous(
    name = "Average temperature (Celsius °)",
    sec.axis = sec_axis(~.*coef, name="Total precipitation (mm)")
  ) +
  xlim(2005, 2100) +
  theme_classic() +
  ggtitle("CESM1 - Region 3") +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines)
dev.off()

# CMCC model
# Load data and fetch labels
files <- list.files(di_inputs, pattern = "*_CMCC_reg3")
files <- files[c(1, 3, 4)]
series <- data.frame()
for(i in 1:length(files)) {
  temp <- read.table(paste(di_inputs, files[i], sep = ""), sep = " ", header = TRUE) %>%
    mutate(scenario = strsplit(files[i], split = "_")[[1]][1], 
           Tavg_C = (TMin + TMax)/2)
  series <- rbind(series, temp)
}

series <- series %>%
  rename(scenario_original = scenario) %>%
  left_join(scenario.labs) %>%
  select(-scenario_original)

# Calculate annual means for PAR, CO2 and temperaures
annual_means <- series %>%
  select(scenario, Year, PAR, CO2, Tavg_C, TMin, TMax) %>%
  group_by(scenario, Year) %>%
  summarise_all(mean) %>%
  melt(id.vars = c("scenario", "Year"))

# Extract mean temperature
avg_temp <- annual_means %>%
  filter(variable == "Tavg_C")

# Calculate annual precipitation (sum)
annual_sums <- series %>%
  select(scenario, Year, Prec) %>%
  group_by(scenario, Year) %>%
  summarise_all(sum) %>%
  melt(id.vars = c("scenario", "Year"))

# Plot
coef <- 100
jpeg(file = paste(di, "CMCC_reg3_temp_prec.jpeg", sep = ""), width=6, height=4, units="in", res=300)
ggplot(data = avg_temp, aes(color = scenario)) +
  # Temperature
  geom_point(data = avg_temp, aes(x = Year, y = value), size = 0.5, alpha = 0.5) +
  geom_line(data = avg_temp, aes(x = Year, y = value), size = 0.3, linetype = "longdash") +
  stat_smooth(data = avg_temp, aes(x = Year, y = value), method = "loess") +
  # Precipitation
  geom_point(data = annual_sums, aes(x = Year, y = value/coef), size = 0.5, alpha = 0.5) +
  geom_line(data = annual_sums, aes(x = Year, y = value/coef), size = 0.3, linetype = "dotted") +
  stat_smooth(data = annual_sums, aes(x = Year, y = value/coef), method = "loess") +
  scale_y_continuous(
    name = "Average temperature (Celsius °)",
    sec.axis = sec_axis(~.*coef, name="Total precipitation (mm)")
  ) +
  xlim(2005, 2100) +
  theme_classic() +
  ggtitle("CMCC - Region 3") +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines)
dev.off()

# ACCESS model
# Load data and fetch labels
files <- list.files(di_inputs, pattern = "*_ACCESS_reg3")
files <- files[c(1, 3, 4)]
series <- data.frame()
for(i in 1:length(files)) {
  temp <- read.table(paste(di_inputs, files[i], sep = ""), sep = " ", header = TRUE) %>%
    mutate(scenario = strsplit(files[i], split = "_")[[1]][1], 
           Tavg_C = (TMin + TMax)/2)
  series <- rbind(series, temp)
}

series <- series %>%
  rename(scenario_original = scenario) %>%
  left_join(scenario.labs) %>%
  select(-scenario_original)

# Calculate annual means for PAR, CO2 and temperaures
annual_means <- series %>%
  select(scenario, Year, PAR, CO2, Tavg_C, TMin, TMax) %>%
  group_by(scenario, Year) %>%
  summarise_all(mean) %>%
  melt(id.vars = c("scenario", "Year"))

# Extract mean temperature
avg_temp <- annual_means %>%
  filter(variable == "Tavg_C")

# Calculate annual precipitation (sum)
annual_sums <- series %>%
  select(scenario, Year, Prec) %>%
  group_by(scenario, Year) %>%
  summarise_all(sum) %>%
  melt(id.vars = c("scenario", "Year"))

# Plot
coef <- 100
jpeg(file = paste(di, "ACCESS_reg3_temp_prec.jpeg", sep = ""), width=6, height=4, units="in", res=300)
ggplot(data = avg_temp, aes(color = scenario)) +
  # Temperature
  geom_point(data = avg_temp, aes(x = Year, y = value), size = 0.5, alpha = 0.5) +
  geom_line(data = avg_temp, aes(x = Year, y = value), size = 0.3, linetype = "longdash") +
  stat_smooth(data = avg_temp, aes(x = Year, y = value), method = "loess") +
  # Precipitation
  geom_point(data = annual_sums, aes(x = Year, y = value/coef), size = 0.5, alpha = 0.5) +
  geom_line(data = annual_sums, aes(x = Year, y = value/coef), size = 0.3, linetype = "dotted") +
  stat_smooth(data = annual_sums, aes(x = Year, y = value/coef), method = "loess") +
  scale_y_continuous(
    name = "Average temperature (Celsius °)",
    sec.axis = sec_axis(~.*coef, name="Total precipitation (mm)")
  ) +
  xlim(2005, 2100) +
  theme_classic() +
  ggtitle("ACCESS - Region 3") +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines)
dev.off()
