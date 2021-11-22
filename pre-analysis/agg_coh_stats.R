# Analysis of Cohort stats on aggregated tables

mgmt.scenarios <- c("20210921_nomanag_current_MIROC5",
                    "20210921_nomanag_rcp45_MIROC5",
                    "20210921_nomanag_rcp85_MIROC5")

### SETUP 
library(dplyr)
library(ggplot2)
library(reshape2)
library(lubridate)

di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/"

cols <- c("conserv" = "#33A02C", # dark green
          "proactive" = "#6A3D9A", # dark purple
          "proactiveplus" = "#FF7F00", # orange
          "nomanag" = "#E31A1C") # red

lines <- c("current" = "solid", "rcp45" = "dotdash", "rcp85" = "dashed")

# Cohort stats - general variables
coh_stats_pines <- data.frame()
coh_stats_dense_pines <- data.frame()

for (i in 1:length(mgmt.scenarios)) {
  temp <- read.table(paste(di, mgmt.scenarios[i], "/results/aggregated_coh_stats_pines.txt", sep =""), header = TRUE) %>%
    mutate(Harv_scenario = strsplit(as.character(Scenario), split = "_")[[1]][2],
           Clim_scenario = strsplit(as.character(Scenario), split = "_")[[1]][3])
  coh_stats_pines <- rbind(coh_stats_pines, temp)
  
  temp <- read.table(paste(di, mgmt.scenarios[i], "/results/aggregated_coh_stats_dense_pines.txt", sep =""), header = TRUE) %>%
    mutate(Harv_scenario = strsplit(as.character(Scenario), split = "_")[[1]][2],
           Clim_scenario = strsplit(as.character(Scenario), split = "_")[[1]][3])
  coh_stats_dense_pines <- rbind(coh_stats_dense_pines, temp)
}

jpeg(file = paste(di, "outputs/210921_avg_age_count_pines_mask.jpeg", sep = ""), width=6, height=4, units="in", res=300)
coh_stats_pines %>%
  ggplot(aes(x = Time, y = Avg_age_count, group = Scenario)) +
  geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
  geom_point(aes(color = Harv_scenario)) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines) +
  ggtitle("Count: total number of cohorts in cell (same age, different species = 2)")
dev.off()

jpeg(file = paste(di, "outputs/210921_avg_age_count_dense_pines_mask.jpeg", sep = ""), width=6, height=4, units="in", res=300)
coh_stats_dense_pines %>%
  ggplot(aes(x = Time, y = Avg_age_count, group = Scenario)) +
  geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
  geom_point(aes(color = Harv_scenario)) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines) +
  ggtitle("Count: total number of cohorts in cell (same age, different species = 2)")
dev.off()

jpeg(file = paste(di, "outputs/210921_avg_age_rich_pines_mask.jpeg", sep = ""), width=6, height=4, units="in", res=300)
coh_stats_pines %>%
  ggplot(aes(x = Time, y = Avg_age_rich, group = Scenario)) +
  geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
  geom_point(aes(color = Harv_scenario)) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines) +
  ggtitle("Age Richness: unique age classes (same age, different species = 1)")
dev.off()

jpeg(file = paste(di, "outputs/210921_avg_age_rich_dense_pines_mask.jpeg", sep = ""), width=6, height=4, units="in", res=300)
coh_stats_dense_pines %>%
  ggplot(aes(x = Time, y = Avg_age_rich, group = Scenario)) +
  geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
  geom_point(aes(color = Harv_scenario)) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines) +
  ggtitle("Age Richness: unique age classes (same age, different species = 1)")
dev.off()

jpeg(file = paste(di, "outputs/210921_avg_spp_rich_pines_mask.jpeg", sep = ""), width=6, height=4, units="in", res=300)
coh_stats_pines %>%
  ggplot(aes(x = Time, y = Avg_spp_rich, group = Scenario)) +
  geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
  geom_point(aes(color = Harv_scenario)) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines)
dev.off()

jpeg(file = paste(di, "outputs/210921_avg_spp_rich_dense_pines_mask.jpeg", sep = ""), width=6, height=4, units="in", res=300)
coh_stats_dense_pines %>%
  ggplot(aes(x = Time, y = Avg_spp_rich, group = Scenario)) +
  geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
  geom_point(aes(color = Harv_scenario)) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines)
dev.off()

## Stats relative difference
# coh_stats_current <- coh_stats %>%
#   filter(Clim_scenario == "current") %>%
#   mutate(Base_avg_age_count = Avg_age_count,
#          Base_avg_age_even = Avg_age_even,
#          Base_avg_age_rich = Avg_age_rich,
#          Base_avg_spp_rich = Avg_spp_rich) %>%
#   select(Time, Harv_scenario, Base_avg_age_count, Base_avg_age_even,
#          Base_avg_age_rich, Base_avg_spp_rich) 
# 
# coh_stats_rel_difference <- coh_stats %>%
#   filter(Clim_scenario != "current") %>%
#   left_join(coh_stats_current) %>%
#   mutate(Relative_dif_age_count = (Avg_age_count * 100) / Base_avg_age_count,
#          Relative_dif_age_even = (Avg_age_even * 100) / Base_avg_age_even,
#          Relative_dif_age_rich = (Avg_age_rich * 100) / Base_avg_age_rich,
#          Relative_dif_spp_rich = (Avg_spp_rich * 100) / Base_avg_spp_rich)
# 
# coh_stats_rel_difference %>%
#   ggplot(aes(x = Time, y = Relative_dif_age_count, group = Scenario)) +
#   geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
#   geom_point(aes(color = Harv_scenario)) +
#   theme_classic() +
#   theme(legend.position = "bottom") +
#   scale_color_manual(values = cols) +
#   scale_linetype_manual(values = lines)
# 
# coh_stats_rel_difference %>%
#   ggplot(aes(x = Time, y = Relative_dif_age_even, group = Scenario)) +
#   geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
#   geom_point(aes(color = Harv_scenario)) +
#   theme_classic() +
#   theme(legend.position = "bottom") +
#   scale_color_manual(values = cols) +
#   scale_linetype_manual(values = lines)
# 
# coh_stats_rel_difference %>%
#   ggplot(aes(x = Time, y = Relative_dif_age_rich, group = Scenario)) +
#   geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
#   geom_point(aes(color = Harv_scenario)) +
#   theme_classic() +
#   theme(legend.position = "bottom") +
#   scale_color_manual(values = cols) +
#   scale_linetype_manual(values = lines)
# 
# coh_stats_rel_difference %>%
#   ggplot(aes(x = Time, y = Relative_dif_spp_rich, group = Scenario)) +
#   geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
#   geom_point(aes(color = Harv_scenario)) +
#   theme_classic() +
#   theme(legend.position = "bottom") +
#   scale_color_manual(values = cols) +
#   scale_linetype_manual(values = lines)
