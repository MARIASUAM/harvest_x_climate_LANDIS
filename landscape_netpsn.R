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

replicates <- c(1:5)

### SETUP 
library(dplyr)
library(ggplot2)
library(reshape2)
library(lubridate)
library(tidyr)

di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/"
outputs_folder <- "210927_outputs/"

cols <- c('#1b9e77','#7570b3')

other_cols <- c("Conservative" = "#33A02C", # dark green
          "Proactive" = "#6A3D9A", # dark purple
          "ProactivePlus" = "#FF7F00", # orange
          "No Management" = "#E31A1C") # red
lines <- c("Current" = "solid", "RCP4.5" = "dotdash", "RCP8.5" = "dashed")

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
  
Harv_scenario.labs <- data.frame(Harv_scenario_original = c("nomanag", "conserv", "proactive", "proactiveplus"),
                                 Harv_scenario = c('No Management', 'Conservative', 'Proactive', 'ProactivePlus'))

Clim_scenario.labs <- data.frame(Clim_scenario_original = c("current", "rcp45", "rcp85"),
                                 Clim_scenario = c("Current", "RCP4.5", "RCP8.5"))

psn_dense_pines <- psn_dense_pines %>%
  rename(Harv_scenario_original = Harv_scenario,
         Clim_scenario_original = Clim_scenario) %>%
  left_join(Harv_scenario.labs) %>%
  left_join(Clim_scenario.labs)

psn_dense_pines$Harv_scenario <- as.factor(psn_dense_pines$Harv_scenario)
levels(psn_dense_pines$Harv_scenario)<- c('No Management', 'Conservative', 'Proactive', 'ProactivePlus')

psn_dense_pines$Clim_scenario <- as.factor(psn_dense_pines$Clim_scenario)
levels(psn_dense_pines$Clim_scenario) <- c("Current", "RCP4.5", "RCP8.5")

# Calculate mean and SD across replicates
acc_rep <- psn_dense_pines %>%
  select(-Scenario, -SD_Net_Psn, -Replicate) %>%
  group_by(Harv_scenario, Clim_scenario, Time, Year, Date, Month, months_num) %>%
  summarise(Mean_acc_rep = mean(Avg_Net_Psn),
            SD_acc_rep = sd(Avg_Net_Psn))

jpeg(file = paste(di, outputs_folder, "avg_netpsn_0_95.jpeg", sep = ""), width=6, height=4, units="in", res=300)
acc_rep %>%
  filter(Time == 0 | Time == 95) %>%
  ggplot(aes(x = months_num, y = Mean_acc_rep, group = as.factor(Year))) +
  geom_line(aes(color = as.factor(Year), lty = as.factor(Year))) +
  geom_point(aes(color = as.factor(Year))) +
  geom_errorbar(aes(ymin = Mean_acc_rep - SD_acc_rep,
                    ymax = Mean_acc_rep + SD_acc_rep),
                width= .2, position = position_dodge(.05)) +
  facet_grid(Clim_scenario ~ Harv_scenario) +
  theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_x_discrete("Month", limits = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")) +
  scale_y_continuous("Avg Net Photosynthesis (g/m2)") +
  scale_color_manual(values = cols) +
  ggtitle("")
dev.off()

jpeg(file = paste(di, outputs_folder, "avg_netpsn_5_95.jpeg", sep = ""), width=6, height=4, units="in", res=300)
acc_rep %>%
  filter(Time == 5 | Time == 95) %>%
  ggplot(aes(x = months_num, y = Mean_acc_rep, group = as.factor(Year))) +
  geom_line(aes(color = as.factor(Year), lty = as.factor(Year))) +
  geom_point(aes(color = as.factor(Year))) +
  geom_errorbar(aes(ymin = Mean_acc_rep - SD_acc_rep, 
                    ymax = Mean_acc_rep + SD_acc_rep),
                width= .2, position = position_dodge(.05)) +
  facet_grid(Clim_scenario ~ Harv_scenario) +
  theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_x_discrete("Month", limits = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")) +
  scale_y_continuous("Avg Net Photosynthesis (g/m2)") +
  scale_color_manual(values = cols) +
  ggtitle("")
dev.off()

# Calculate average through initial and end years
years_avg <- acc_rep %>%
  filter(Time == 0 | Time == 5 | Time == 10 | 
           Time == 95 | Time == 90 | Time == 85) %>%
  mutate(Period = ifelse(Time > 50, "End", "Beginning")) %>%
  select(-Time, -Date, -SD_acc_rep, -Year) %>%
  group_by(Harv_scenario, Clim_scenario, Period, Month, months_num) %>%
  summarise(Mean_accros_years = mean(Mean_acc_rep),
            SD_accros_years = sd(Mean_acc_rep))

jpeg(file = paste(di, outputs_folder, "avg_netpsn_0-10_85-95.jpeg", sep = ""), width=12, height=8, units="in", res=300)
years_avg %>%
  ggplot(aes(x = months_num, y = Mean_accros_years, group = Period)) +
  geom_line(aes(color = Period, lty = Period)) +
  geom_point(aes(color = Period)) +
  geom_ribbon(aes(ymin = Mean_accros_years - SD_accros_years,
                  ymax = Mean_accros_years + SD_accros_years, x = months_num, 
                  fill = Period), alpha = 0.3)+
  facet_grid(Clim_scenario ~ Harv_scenario) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 9),
        legend.text = element_text(size = 12)) +
  scale_x_discrete("Month", limits = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")) +
  scale_y_continuous("Avg Net Photosynthesis (g/m2)") +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  ggtitle("")
dev.off()

years_avg <- acc_rep %>%
  filter(Time == 5 | Time == 10 | Time == 15 | 
         Time == 95 | Time == 90 | Time == 85) %>%
  mutate(Period = ifelse(Time > 50, "End", "Beginning")) %>%
  select(-Time, -Date, -SD_acc_rep, -Year) %>%
  group_by(Harv_scenario, Clim_scenario, Period, Month, months_num) %>%
  summarise(Mean_accros_years = mean(Mean_acc_rep),
            SD_accros_years = sd(Mean_acc_rep))
 
jpeg(file = paste(di, outputs_folder, "avg_netpsn_5-15_85-95.jpeg", sep = ""), width=12, height=8, units="in", res=300)
years_avg %>%
  ggplot(aes(x = months_num, y = Mean_accros_years, group = Period)) +
  geom_line(aes(color = Period, lty = Period)) +
  geom_point(aes(color = Period)) +
  geom_ribbon(aes(ymin = Mean_accros_years - SD_accros_years,
                  ymax = Mean_accros_years + SD_accros_years, x = months_num, 
                  fill = Period), alpha = 0.3)+
  facet_grid(Clim_scenario ~ Harv_scenario) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 9),
        legend.text = element_text(size = 12)) +
  scale_x_discrete("Month", limits = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")) +
  scale_y_continuous("Avg Net Photosynthesis (g/m2)") +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  ggtitle("")
dev.off()

# Calculate annual NetPsn

annual_psn <- psn_dense_pines %>%
  select(Harv_scenario, Clim_scenario, Replicate, Year, Avg_Net_Psn) %>%
  group_by(Harv_scenario, Clim_scenario, Replicate, Year) %>%
  summarise(Annual_Psn = sum(Avg_Net_Psn)) %>%
  select(-Replicate) %>%
  group_by(Harv_scenario, Clim_scenario, Year) %>%
  summarise(Avg_Annual_Psn = mean(Annual_Psn),
            SD_Annual_Psn = sd(Annual_Psn))
  
jpeg(file = paste(di, outputs_folder, "Annual_NetPsn.jpeg", sep = ""), width = 18, height = 12, units="in", res=300)
ggplot(annual_psn, aes(x = Year, y = Avg_Annual_Psn)) +
  geom_line(aes(color = Harv_scenario)) + #linetype = Clim_scenario, 
  geom_point(aes(color = Harv_scenario)) +
  geom_errorbar(aes(ymin = Avg_Annual_Psn - SD_Annual_Psn, 
                    ymax = Avg_Annual_Psn + SD_Annual_Psn), width=.2,
                position = position_dodge(.05)) +
  theme_classic() +
  facet_wrap(Clim_scenario ~ Harv_scenario) +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14)) +
  scale_color_manual(values = other_cols) +
  # scale_linetype_manual(values = lines) +
  ylab("Annual Net Photosynthesis (g/m2)") +
  xlab(NULL)
dev.off()
  

  
  