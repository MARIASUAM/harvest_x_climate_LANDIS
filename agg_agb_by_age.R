# Analysis of AGB by age on aggregated tables

mgmt.scenarios <- c("20210922_conserv_current_MIROC5",
                    "20210922_nomanag_current_MIROC5",
                    "20210922_proactive_current_MIROC5",
                    "20210922_proactiveplus_current_MIROC5")

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

# Load AGB by age
agb_age_pines <- data.frame()
agb_age_dense_pines <- data.frame()
for (i in 1:length(mgmt.scenarios)) {
  temp <- read.table(paste(di, mgmt.scenarios[i], "/results/aggregated_agbiomass_by_age_pines.txt", sep =""), header = TRUE) %>%
    mutate(Harv_scenario = strsplit(as.character(Scenario), split = "_")[[1]][2],
           Clim_scenario = strsplit(as.character(Scenario), split = "_")[[1]][3])
  agb_age_pines <- rbind(agb_age_pines, temp)
  
  temp <- read.table(paste(di, mgmt.scenarios[i], "/results/aggregated_agbiomass_by_age_dense_pines.txt", sep =""), header = TRUE) %>%
    mutate(Harv_scenario = strsplit(as.character(Scenario), split = "_")[[1]][2],
           Clim_scenario = strsplit(as.character(Scenario), split = "_")[[1]][3])
  agb_age_dense_pines <- rbind(agb_age_dense_pines, temp)
}

# Comparison among scenarios by groups of species
jpeg(file = paste(di, "outputs/20210921_agb_byage_oaks_pines_mask.jpeg", sep = ""), width=6, height=4, units="in", res=300)
agb_age_pines %>%
  filter(Species == "qilex" | 
           Species == "qfaginea" | 
           Species == "qpyrenaica") %>%
  select(Scenario, Harv_scenario, Clim_scenario, Time, Age_class, Total_tn) %>%
  group_by(Scenario, Harv_scenario, Clim_scenario, Time, Age_class) %>%
  summarise(Total_oaks_tn = sum(Total_tn)) %>%
  ggplot(aes(x = Time, y = Total_oaks_tn, group = Scenario)) +
  geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
  geom_point(aes(color = Harv_scenario)) +
  facet_wrap(Age_class ~ ., scales = "free_y") +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines)
dev.off()

jpeg(file = paste(di, "outputs/20210921_agb_byage_oaks_dense_pines_mask.jpeg", sep = ""), width=6, height=4, units="in", res=300)
agb_age_dense_pines %>%
  filter(Species == "qilex" | 
           Species == "qfaginea" | 
           Species == "qpyrenaica") %>%
  select(Scenario, Harv_scenario, Clim_scenario, Time, Age_class, Total_tn) %>%
  group_by(Scenario, Harv_scenario, Clim_scenario, Time, Age_class) %>%
  summarise(Total_oaks_tn = sum(Total_tn)) %>%
  ggplot(aes(x = Time, y = Total_oaks_tn, group = Scenario)) +
  geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
  geom_point(aes(color = Harv_scenario)) +
  facet_wrap(Age_class ~ ., scales = "free_y") +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines)
dev.off()

jpeg(file = paste(di, "outputs/20210921_agb_byage_pines_pines_mask.jpeg", sep = ""), width=6, height=4, units="in", res=300)
agb_age_pines %>%
  filter(Species == "ppinaster" | 
           Species == "pnigra" | 
           Species == "psylvestris" | 
           Species == "phalepensis")  %>%
  select(Scenario, Harv_scenario, Clim_scenario, Time, Age_class, Total_tn) %>%
  group_by(Scenario, Harv_scenario, Clim_scenario, Time, Age_class) %>%
  summarise(Total_pines_tn = sum(Total_tn)) %>%
  ggplot(aes(x = Time, y = Total_pines_tn, group = Scenario)) +
  geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
  geom_point(aes(color = Harv_scenario)) +
  facet_wrap(Age_class ~ ., scales = "free_y") + 
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines)
dev.off()

jpeg(file = paste(di, "outputs/20210921_agb_byage_pines_dense_pines_mask.jpeg", sep = ""), width=6, height=4, units="in", res=300)
agb_age_dense_pines %>%
  filter(Species == "ppinaster" | 
           Species == "pnigra" | 
           Species == "psylvestris" | 
           Species == "phalepensis")  %>%
  select(Scenario, Harv_scenario, Clim_scenario, Time, Age_class, Total_tn) %>%
  group_by(Scenario, Harv_scenario, Clim_scenario, Time, Age_class) %>%
  summarise(Total_pines_tn = sum(Total_tn)) %>%
  ggplot(aes(x = Time, y = Total_pines_tn, group = Scenario)) +
  geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
  geom_point(aes(color = Harv_scenario)) +
  facet_wrap(Age_class ~ ., scales = "free_y") + 
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines)
dev.off()


agb_age_dense_pines %>%
  ggplot(aes(x = Time, y = Total_tn, group = Scenario)) +
  geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
  geom_point(aes(color = Harv_scenario)) +
  facet_wrap(Species ~ Age_class, scales = "free_y", ncol = 4) + 
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines)

# # Comparison among scenarios by species
# agb_age %>%
#   filter(Species == "qilex" | 
#            Species == "qfaginea" | 
#            Species == "qpyrenaica") %>%
#   ggplot(aes(x = Time, y = Total_tn, group = Scenario)) +
#   geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
#   geom_point(aes(color = Harv_scenario)) +
#   facet_wrap(Age_class ~ Species, scales = "free_y") +
#   theme_classic() +
#   theme(legend.position = "bottom") +
#   scale_color_manual(values = cols) +
#   scale_linetype_manual(values = lines)
# 
# agb_age %>%
#   filter(Species == "ppinaster" | 
#            Species == "pnigra" | 
#            Species == "phalepensis" | 
#            Species == "psylvestris") %>%
#   ggplot(aes(x = Time, y = Total_tn, group = Scenario)) +
#   geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
#   geom_point(aes(color = Harv_scenario)) +
#   facet_wrap(Age_class ~ Species, scales = "free_y") +
#   theme_classic() +
#   theme(legend.position = "bottom") +
#   scale_color_manual(values = cols) +
#   scale_linetype_manual(values = lines)
# 
# # Detail: one scenario, all age classes in same plot - lines
# sce <- "210910_nomanag_current_EstRad10_MaxPest10"
# agb_age %>%
#   filter(Species == "ppinaster" |
#            Species == "pnigra" |
#            Species == "psylvestris" | 
#            Species == "phalepensis",
#          Scenario == sce) %>%
#   ggplot(aes(x = Time, y = Total_tn, group = Age_class)) +
#   geom_line(aes(color = Age_class)) +
#   geom_point(aes(color = Age_class)) +
#   facet_wrap(Species ~ ., scales = "free_y") +
#   theme_classic() +
#   theme(legend.position = "bottom") +
#   ggtitle(sce)
# 
# agb_age %>%
#   filter(Species == "qfaginea" |
#            Species == "qilex" |
#            Species == "qpyrenaica",
#          Scenario == sce) %>%
#   ggplot(aes(x = Time, y = Total_tn, group = Age_class)) +
#   geom_line(aes(color = Age_class)) +
#   geom_point(aes(color = Age_class)) +
#   facet_wrap(Species ~ ., scales = "free_y") +
#   theme_classic() +
#   theme(legend.position = "bottom") +
#   ggtitle(sce)
# 
# # Detail: one scenario, all age classes in same plot - cummulative
# agb_age %>%
#   filter(Species == "ppinaster" |
#            Species == "pnigra" |
#            Species == "psylvestris" | 
#            Species == "phalepensis",
#          Scenario == sce) %>%
#   ggplot(aes(x = Time, y = Total_tn, fill = Age_class)) +
#   geom_area() +
#   facet_wrap(Species ~ ., scales = "free_y") +
#   theme_classic() +
#   theme(legend.position = "bottom") +
#   ggtitle(sce)
# 
# agb_age %>%
#   filter(Species == "qilex" |
#            Species == "qfaginea" |
#            Species == "qpyrenaica",
#          Scenario == sce) %>%
#   ggplot(aes(x = Time, y = Total_tn, fill = Age_class)) +
#   geom_area() +
#   facet_wrap(Species ~ ., scales = "free_y") +
#   theme_classic() +
#   theme(legend.position = "bottom") +
#   ggtitle(sce)
# 
## AGB by age relative difference
# agb_byage_current <- agb_age %>%
#   filter(Clim_scenario == "current") %>%
#   mutate(Base_avg_tnha = Avg_tnha) %>%
#   select(-Scenario, -Clim_scenario, -Avg_tnha, -SD_tnha, - Total_tn) 
# 
# agb_byage_rel_difference <- agb_age %>%
#   filter(Clim_scenario != "current") %>%
#   left_join(agb_byage_current) %>%
#   mutate(Relative_dif_avg_tnha = (Avg_tnha * 100) / Base_avg_tnha)
# 
# agb_byage_rel_difference %>%
#   filter(Species == "ppinaster" | 
#            Species == "pnigra" | 
#            Species == "psylvestris"| 
#            Species == "phalepensis") %>%
#   ggplot(aes(x = Time, y = Relative_dif_avg_tnha, group = Scenario)) +
#   geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
#   geom_point(aes(color = Harv_scenario)) +
#   facet_wrap(Age_class ~ Species, scales = "free_y") +
#   theme_classic() +
#   theme(legend.position = "bottom") +
#   scale_color_manual(values = cols) +
#   scale_linetype_manual(values = lines)
