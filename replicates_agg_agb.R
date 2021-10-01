# Analysis of AGB on aggregated tables

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

di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/"
outputs_folder <- "210927_outputs/"

cols <- c("conserv" = "#33A02C", # dark green
          "proactive" = "#6A3D9A", # dark purple
          "proactiveplus" = "#FF7F00", # orange
          "nomanag" = "#E31A1C") # red

lines <- c("current" = "solid", "rcp45" = "dotdash", "rcp85" = "dashed")

groups <- data.frame(Species = c("qfaginea", "qpyrenaica", "qilex", "ppinaster", "pnigra", "psylvestris", "phalepensis", "tall", "medium", "short", "jcommunis", "joxycedrus", "popnigra"),
                     Group = c("Oaks", "Oaks", "Oaks", "P. pinaster", "P. nigra", "P. sylvestris", "P. halepensis", "Other", "Other", "Other", "Other", "Other", "Other"))

# AGB 
all_replicates <- data.frame()
for (i in seq_along(mgmt.scenarios)) {
  for (j in seq_along(replicates)) {
  one_replicate <- read.table(paste(di, mgmt.scenarios[i], "_rep", replicates[j], "/results/aggregated_agbiomass_dense_pines.txt", sep =""), header = TRUE) %>% 
    mutate(Harv_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][2],
           Clim_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][3])
  all_replicates <- rbind(all_replicates, one_replicate)
  }
}

agb_dense_pines <- all_replicates %>%
  select(-Scenario, -SD_tnha) %>%  
  group_by(Harv_scenario, Clim_scenario, Time, Species) %>%
  summarise(Avg_mean_tnha_accross_rep = mean(Avg_tnha),
            SD_mean_tnha_accross_rep = sd(Avg_tnha),
            Avg_total_tn_accross_rep = mean(Total_tn),
            SD_total_tn_accross_rep = sd(Total_tn))

agb_dense_pines$Harv_scenario <- as.factor(agb_dense_pines$Harv_scenario)
levels(agb_dense_pines$Harv_scenario)<- c("nomanag", "conserv", "proactive", "proactiveplus")

# AGB stacked by species
jpeg(file = paste(di, outputs_folder, "agb_dense_pines_mask.jpeg", sep = ""), width=18, height=12, units="in", res=300)
agb_dense_pines %>%
  left_join(groups) %>%
  dplyr::select(-Species) %>%
  group_by(Harv_scenario, Clim_scenario, Time, Group) %>%
  summarise(Total_tn = sum(Avg_total_tn_accross_rep)) %>%
  ggplot(aes(x = Time, y = Total_tn, fill = Group)) +
  geom_area() +
  facet_wrap(Clim_scenario ~ Harv_scenario) + # , scales = "free_y"
  theme_classic() +
  theme(legend.position = "bottom")
dev.off()

# Plotting
jpeg(file = paste(di, outputs_folder, "agb_oaks_pines_mask.jpeg", sep = ""), width=6, height=4, units="in", res=300)
agb_dense_pines %>%
  filter(Species == "qilex" |
           Species == "qfaginea" |
           Species == "qpyrenaica") %>%
  ggplot(aes(x = Time, y = Avg_total_tn_accross_rep)) +
  geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
  geom_point(aes(color = Harv_scenario)) +
  geom_errorbar(aes(ymin = Avg_total_tn_accross_rep - SD_total_tn_accross_rep, 
                    ymax = Avg_total_tn_accross_rep + SD_total_tn_accross_rep), width=.2,
                position = position_dodge(.05)) +
  facet_wrap(Species ~ ., scales = "free_y") + #
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines)
dev.off()

jpeg(file = paste(di, outputs_folder, "agb_pines_pines_mask.jpeg", sep = ""), width=6, height=4, units="in", res=300)
agb_dense_pines %>%
  filter(Species == "ppinaster" |
           Species == "pnigra" |
           Species == "psylvestris"|
           Species == "phalepensis") %>%
  ggplot(aes(x = Time, y = Avg_total_tn_accross_rep)) +
  geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
  geom_point(aes(color = Harv_scenario)) +
  geom_errorbar(aes(ymin = Avg_total_tn_accross_rep - SD_total_tn_accross_rep, 
                    ymax = Avg_total_tn_accross_rep + SD_total_tn_accross_rep), width=.2,
                position = position_dodge(.05)) +
  facet_wrap(Species ~ ., scales = "free_y") +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines)
dev.off()
