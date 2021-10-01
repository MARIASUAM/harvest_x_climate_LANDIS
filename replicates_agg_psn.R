# Analysis on aggregated tables

mgmt.scenarios <- c("210927_conserv_current",
                    "210927_conserv_rcp45",
                    "210927_conserv_rcp85",
                    "210927_nomanag_current",
                    "210927_nomanag_rcp45",
                    "210927_nomanag_rcp85",
                    "210927_proactive_current",
                    "210927_proactive_rcp45",
                    "210927_proactive_rcp85",
                    "210927_proactiveplus_current",
                    "210927_proactiveplus_rcp45",
                    "210927_proactiveplus_rcp85")

replicates <- c(1:2) # 1:5 when completed

### SETUP 
library(dplyr)
library(ggplot2)
library(reshape2)
library(lubridate)
library(tidyr)

di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/"

cols <- c("conserv" = "#33A02C", # dark green
          "proactive" = "#6A3D9A", # dark purple
          "proactiveplus" = "#FF7F00", # orange
          "nomanag" = "#E31A1C") # red
lines <- c("current" = "solid", "rcp45" = "dotdash", "rcp85" = "dashed")

# Monthly Psn
months <- data.frame(months_ch = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"),
                     months_num = c(1:12))

psn_dense_pines <- data.frame()
for (i in seq_along(mgmt.scenarios)) {
  for (j in seq_along(replicates)) {
  temp <- read.table(paste(di, mgmt.scenarios[i], "_rep", replicates[j], "/results/aggregated_net_psn_dense_pines.txt", sep =""), header = TRUE) %>%
    left_join(months, by = c("Month" = "months_ch")) %>%
    mutate(Harv_scenario = strsplit(as.character(Scenario), split = "_")[[1]][2],
           Clim_scenario = strsplit(as.character(Scenario), split = "_")[[1]][3],
           Replicate = replicates[j],
           Year = Time + 2005,
           Date = as.Date(paste(Year, months_num, "01", sep = "-")))
  psn_dense_pines <- rbind(psn_dense_pines, temp)
  }
}

# Calculate mean and SD accross replicates
acc_rep <- psn_dense_pines %>%
  select(-Scenario, -SD_Net_Psn, -Replicate) %>%
  group_by(Harv_scenario, Clim_scenario, Time, Year, Date, Month, months_num) %>%
  summarise(Mean_acc_rep = mean(Avg_Net_Psn),
            SD_acc_rep = sd(Avg_Net_Psn))

acc_rep %>%
  filter(Time == 0 | Time == 95) %>%
  ggplot(aes(x = months_num, y = Mean_acc_rep, group = as.factor(Year))) +
  geom_line(aes(color = as.factor(Year))) +
  geom_point(aes(color = as.factor(Year))) +
  geom_errorbar(aes(ymin = Mean_acc_rep - SD_acc_rep, 
                    ymax = Mean_acc_rep + SD_acc_rep),
                width= .2, position = position_dodge(.05)) +
  facet_grid(Harv_scenario ~ Clim_scenario) +
  theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_x_discrete("Month", limits = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")) +
  scale_y_continuous("Avg Net Photosynthesis") +
  ggtitle("")

acc_rep %>%
  filter(Time == 5 | Time == 95) %>%
  ggplot(aes(x = months_num, y = Mean_acc_rep, group = as.factor(Year))) +
  geom_line(aes(color = as.factor(Year))) +
  geom_point(aes(color = as.factor(Year))) +
  geom_errorbar(aes(ymin = Mean_acc_rep - SD_acc_rep, 
                    ymax = Mean_acc_rep + SD_acc_rep),
                width= .2, position = position_dodge(.05)) +
  facet_grid(Harv_scenario ~ Clim_scenario) +
  theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_x_discrete("Month", limits = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")) +
  scale_y_continuous("Avg Net Photosynthesis") +
  ggtitle("")

# Calculate average through initial and end years
years_avg <- acc_rep %>%
  filter(Time == 5 | Time == 10 | Time == 15 | 
         Time == 95 | Time == 90 | Time == 85) %>%
  mutate(Period = ifelse(Time > 50, 2, 1)) %>%
  select(-Time, -Date, -SD_acc_rep, -Year) %>%
  group_by(Harv_scenario, Clim_scenario, Period, Month, months_num) %>%
  summarise(Mean_accros_years = mean(Mean_acc_rep),
            SD_accros_years = sd(Mean_acc_rep))
 
years_avg %>%
  ggplot(aes(x = months_num, y = Mean_accros_years, group = as.factor(Period))) +
  geom_line(aes(color = as.factor(Period))) +
  geom_point(aes(color = as.factor(Period))) +
  geom_errorbar(aes(ymin = Mean_accros_years - SD_accros_years, 
                    ymax = Mean_accros_years + SD_accros_years),
                width= 1, position = position_dodge(.04)) +
  facet_grid(Clim_scenario ~ Harv_scenario) +
  theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_x_discrete("Month", limits = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")) +
  scale_y_continuous("Avg Net Photosynthesis accross initial/final years") +
  ggtitle("")

