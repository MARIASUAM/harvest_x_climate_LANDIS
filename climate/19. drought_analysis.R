# Drought analysis among models

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
## MIROC5
files_MIROC <- list.files(di_inputs, pattern = "*_MIROC5_reg3")
files_MIROC <- files_MIROC[c(1, 3, 4)]
series_MIROC <- data.frame()
for(i in 1:length(files_MIROC)) {
  temp <- read.table(paste(di_inputs, files_MIROC[i], sep = ""), sep = " ", header = TRUE) %>%
    mutate(scenario = strsplit(files_MIROC[i], split = "_")[[1]][1])
  series_MIROC <- rbind(series_MIROC, temp)
}

series_MIROC <- series_MIROC %>%
  rename(scenario_original = scenario) %>%
  left_join(scenario.labs) %>%
  select(-scenario_original) %>%
  mutate(Model = "MIROC")

# Calculate annual precipitation (sum)
annual_sums_MIROC <- series_MIROC %>%
  select(scenario, Year, Prec, Model) %>%
  group_by(scenario, Year, Model) %>%
  summarise_all(sum) %>%
  melt(id.vars = c("scenario", "Year", "Model"))

## CESM
files_CESM1 <- list.files(di_inputs, pattern = "*_CESM1_reg3")
files_CESM1 <- files_CESM1[c(1, 3, 4)]
series_CESM1 <- data.frame()
for(i in 1:length(files_CESM1)) {
  temp <- read.table(paste(di_inputs, files_CESM1[i], sep = ""), sep = " ", header = TRUE) %>%
    mutate(scenario = strsplit(files_CESM1[i], split = "_")[[1]][1])
  series_CESM1 <- rbind(series_CESM1, temp)
}

series_CESM1 <- series_CESM1 %>%
  rename(scenario_original = scenario) %>%
  left_join(scenario.labs) %>%
  select(-scenario_original) %>%
  mutate(Model = "CESM1")

# Calculate annual precipitation (sum)
annual_sums_CESM1 <- series_CESM1 %>%
  select(scenario, Year, Prec, Model) %>%
  group_by(scenario, Year, Model) %>%
  summarise_all(sum) %>%
  melt(id.vars = c("scenario", "Year", "Model"))

## CMCC
files_CMCC <- list.files(di_inputs, pattern = "*_CMCC_reg3")
files_CMCC <- files_CMCC[c(1, 3, 4)]
series_CMCC <- data.frame()
for(i in 1:length(files_CMCC)) {
  temp <- read.table(paste(di_inputs, files_CMCC[i], sep = ""), sep = " ", header = TRUE) %>%
    mutate(scenario = strsplit(files_CMCC[i], split = "_")[[1]][1])
  series_CMCC <- rbind(series_CMCC, temp)
}

series_CMCC <- series_CMCC %>%
  rename(scenario_original = scenario) %>%
  left_join(scenario.labs) %>%
  select(-scenario_original) %>%
  mutate(Model = "CMCC")

# Calculate annual precipitation (sum)
annual_sums_CMCC <- series_CMCC %>%
  select(scenario, Year, Prec, Model) %>%
  group_by(scenario, Year, Model) %>%
  summarise_all(sum) %>%
  melt(id.vars = c("scenario", "Year", "Model"))

## ACCESS
files_ACCESS <- list.files(di_inputs, pattern = "*_ACCESS_reg3")
files_ACCESS <- files_ACCESS[c(1, 3, 4)]
series_ACCESS <- data.frame()
for(i in 1:length(files_ACCESS)) {
  temp <- read.table(paste(di_inputs, files_ACCESS[i], sep = ""), sep = " ", header = TRUE) %>%
    mutate(scenario = strsplit(files_ACCESS[i], split = "_")[[1]][1])
  series_ACCESS <- rbind(series_ACCESS, temp)
}

series_ACCESS <- series_ACCESS %>%
  rename(scenario_original = scenario) %>%
  left_join(scenario.labs) %>%
  select(-scenario_original) %>%
  mutate(Model = "ACCESS")

# Calculate annual precipitation (sum)
annual_sums_ACCESS <- series_ACCESS %>%
  select(scenario, Year, Prec, Model) %>%
  group_by(scenario, Year, Model) %>%
  summarise_all(sum) %>%
  melt(id.vars = c("scenario", "Year", "Model"))

# Annual precipitation plot
precipitation <- rbind(annual_sums_ACCESS, annual_sums_CESM1)
precipitation <- rbind(precipitation, annual_sums_CMCC)
precipitation <- rbind(precipitation, annual_sums_MIROC)

jpeg(file = paste(di, "annual_prec_among_models.jpeg", sep = ""), width=12, height=8, units="in", res=300)
ggplot(data = precipitation, aes(x = Year, y = value, color = Model)) +
  geom_line(size = 0.3, linetype = "dotted") +
  stat_smooth(method = "loess") +
  scale_y_continuous(name = "Annual precipitation (mm)") +
  xlim(1950, 2100) +
  theme_classic() +
  facet_wrap(. ~ scenario) + #
  ggtitle("Subhumid area") +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  # scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines)
dev.off()

# Monthly precipitation
monthly_precipitation <- rbind(series_ACCESS, series_CESM1)
monthly_precipitation <- rbind(monthly_precipitation, series_CMCC)
monthly_precipitation <- rbind(monthly_precipitation, series_MIROC)

monthly_precipitation <- monthly_precipitation %>%
  mutate(date = as.Date(paste(Year, Month, "15", sep = "-"),
                        tryFormats = c("%Y-%m-%d")))

jpeg(file = paste(di, "monthly_prec_among_models.jpeg", sep = ""), width=12, height=8, units="in", res=300)
monthly_precipitation %>%
  filter(Year > 2090) %>%
  ggplot(aes(x = date, y = Prec, color = Model)) +
  geom_line(size = 0.3) +
  geom_point(size = 0.3) +
  scale_y_continuous(name = "Monthly precipitation (mm)") +
  theme_classic() +
  facet_wrap(. ~ scenario, nrow = 3) + #
  ggtitle("Subhumid area") +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  scale_linetype_manual(values = lines)
dev.off()


         