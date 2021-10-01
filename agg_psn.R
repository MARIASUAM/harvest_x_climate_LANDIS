# Analysis on aggregated tables

mgmt.scenarios <- c("210913_nomanag_current_MIROC5",
                    "210913_conserv_current_MIROC5",
                    "210913_conserv_rcp45_MIROC5",
                    "210913_conserv_rcp85_MIROC5",
                    "210913_nomanag_rcp45_MIROC5",
                    "210913_nomanag_rcp85_MIROC5",
                    "210913_proactive_current_MIROC5",
                    "210913_proactive_rcp45_MIROC5",
                    "210913_proactive_rcp85_MIROC5",
                    "210913_proactiveplus_current_MIROC5",
                    "210913_proactiveplus_rcp45_MIROC5",
                    "210913_proactiveplus_rcp85_MIROC5")

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
psn_pines <- data.frame()
psn_dense_pines <- data.frame()
for (i in 1:length(mgmt.scenarios)) {
  temp <- read.table(paste(di, mgmt.scenarios[i], "/results/aggregated_net_psn_pines.txt", sep =""), header = TRUE) %>%
    left_join(months, by = c("Month" = "months_ch")) %>%
    mutate(Harv_scenario = strsplit(as.character(Scenario), split = "_")[[1]][2],
           Clim_scenario = strsplit(as.character(Scenario), split = "_")[[1]][3],
           Year = Time + 2005,
           Date = as.Date(paste(Year, months_num, "01", sep = "-")))
  psn_pines <- rbind(psn_pines, temp)
  
  temp <- read.table(paste(di, mgmt.scenarios[i], "/results/aggregated_net_psn_dense_pines.txt", sep =""), header = TRUE) %>%
    left_join(months, by = c("Month" = "months_ch")) %>%
    mutate(Harv_scenario = strsplit(as.character(Scenario), split = "_")[[1]][2],
           Clim_scenario = strsplit(as.character(Scenario), split = "_")[[1]][3],
           Year = Time + 2005,
           Date = as.Date(paste(Year, months_num, "01", sep = "-")))
  psn_dense_pines <- rbind(psn_dense_pines, temp)
}

start <- 2005
# jpeg(file = paste(di, "outputs/210921_psn_", start, "_pines_mask.jpeg", sep = ""), width=6, height=4, units="in", res=300)
psn_pines %>%
  filter(Year == start) %>%
  ggplot(aes(x = Month, y = Avg_Net_Psn, group = Scenario)) +
  geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
  geom_point(aes(color = Harv_scenario)) +
  theme_classic() +
  facet_grid(Clim_scenario ~ .) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines) +
  scale_x_discrete("Month", limits = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")) +
  scale_y_continuous("Avg Net Photosynthesis", limits = c(0, 150)) +
  ggtitle(paste("Year", start, sep = " "))
# dev.off()

end <- 2100
# jpeg(file = paste(di, "outputs/210921_psn_", end, "_pines_mask.jpeg", sep = ""), width=6, height=4, units="in", res=300)
psn_pines %>%
  filter(Year == end) %>%
  ggplot(aes(x = Month, y = Avg_Net_Psn, group = Scenario)) +
  geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
  geom_point(aes(color = Harv_scenario)) +
  theme_classic() +
  facet_grid(Clim_scenario ~ .) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines) +
  scale_x_discrete("Month", limits = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")) +
  scale_y_continuous("Avg Net Photosynthesis", limits = c(0, 150)) +
  ggtitle(paste("Year", end, sep = " "))
# dev.off()



start <- 2005
# jpeg(file = paste(di, "outputs/210921_psn_", start, "_dense_pines_mask.jpeg", sep = ""), width=6, height=4, units="in", res=300)
psn_dense_pines %>%
  filter(Year == start) %>%
  ggplot(aes(x = Month, y = Avg_Net_Psn, group = Scenario)) +
  geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
  geom_point(aes(color = Harv_scenario)) +
  theme_classic() +
  facet_grid(Clim_scenario ~ .) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines) +
  scale_x_discrete("Month", limits = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")) +
  scale_y_continuous("Avg Net Photosynthesis", limits = c(0, 150)) +
  ggtitle(paste("Year", start, sep = " "))
# dev.off()

end <- 2100
# jpeg(file = paste(di, "outputs/210921_psn_", end, "_dense_pines_mask.jpeg", sep = ""), width=6, height=4, units="in", res=300)
psn_dense_pines %>%
  filter(Year == end) %>%
  ggplot(aes(x = Month, y = Avg_Net_Psn, group = Scenario)) +
  geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
  geom_point(aes(color = Harv_scenario)) +
  theme_classic() +
  facet_grid(Clim_scenario ~ .) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines) +
  scale_x_discrete("Month", limits = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")) +
  scale_y_continuous("Avg Net Photosynthesis", limits = c(0, 150)) +
  ggtitle(paste("Year", end, sep = " "))
# dev.off()

# jpeg(file = paste(di, "outputs/210913_psn_comparison_dense_pines_mask.jpeg", sep = ""), width=6, height=4, units="in", res=300)
psn_dense_pines %>%
  filter(Time == 0 | Time == 95,
         Harv_scenario == "nomanag") %>%
  ggplot(aes(x = months_num, y = Avg_Net_Psn, group = as.factor(Year))) +
  geom_line(aes(color = as.factor(Year))) +
  geom_point(aes(color = as.factor(Year))) +
  facet_grid(Clim_scenario ~ .) +
  theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_x_discrete("Month", limits = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")) +
  scale_y_continuous("Avg Net Photosynthesis") +
  ggtitle("No management scenario")
# dev.off()


psn_t0 <- psn_dense_pines %>%
  filter(Time == 0,
         Harv_scenario == "nomanag") %>%
  rename(Avg_Net_Psn_t0 = Avg_Net_Psn) %>%
  select(-Scenario, -Time, -SD_Net_Psn, -Harv_scenario, -Year, -Date)
  
psn_t95 <- psn_dense_pines %>%
  filter(Time == 95,
         Harv_scenario == "nomanag") %>%
  rename(Avg_Net_Psn_t95 = Avg_Net_Psn) %>%
  select(-Scenario, -Time, -SD_Net_Psn, -Harv_scenario, -Year, -Date)

psn_dif_due_to_forest <- left_join(psn_t0, psn_t95) %>%
  mutate(Dif_PSN = Avg_Net_Psn_t95 - Avg_Net_Psn_t0)

ggplot(psn_dif_due_to_forest, aes(x = months_num, y = Dif_PSN)) +
  geom_line() +
  geom_point() +
  facet_grid(Clim_scenario ~ .) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_x_discrete("Month", limits = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")) +
  ggtitle("Difference in Net Psn due to forest")

psn_dif_due_to_climate <- spread(psn_t95, Clim_scenario, Avg_Net_Psn_t95) %>%
  mutate(Dif_rcp45 = rcp45 - current,
         Dif_rcp85 = rcp85 - current)
  
ggplot(psn_dif_due_to_climate, aes(x = months_num, y = Dif_rcp45)) +
  geom_line() +
  geom_point() +
  # facet_grid(Clim_scenario ~ .) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_x_discrete("Month", limits = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")) +
  ggtitle("Difference in Net Psn due to climate (rcp45, year 2100)")

ggplot(psn_dif_due_to_climate, aes(x = months_num, y = Dif_rcp85)) +
  geom_line() +
  geom_point() +
  # facet_grid(Clim_scenario ~ .) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_x_discrete("Month", limits = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")) +
  ggtitle("Difference in Net Psn due to climate (rcp85, year 2100)")

