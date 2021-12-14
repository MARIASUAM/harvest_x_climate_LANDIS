# Analysis of cohort statistics on aggregated tables

mgmt.scenarios <- c(...) # Folder names with each scenario

replicates <- c(1:5)

### SETUP 
library(dplyr)
library(ggplot2)
library(reshape2)
library(lubridate)

di <- ".../experiments/" # Path to simulations folder
outputs_folder <- "..." # Subfolder for outputs

cols <- c("conserv" = "#33A02C", # dark green
          "proactive" = "#6A3D9A", # dark purple
          "proactiveplus" = "#FF7F00", # orange
          "nomanag" = "#E31A1C") # red

lines <- c("current" = "solid", "rcp45" = "dotdash", "rcp85" = "dashed")

groups <- data.frame(Species = c("qfaginea", "qpyrenaica", "qilex", "ppinaster", "pnigra", "psylvestris", "phalepensis", "tall", "medium", "short", "jcommunis", "joxycedrus", "popnigra"),
                     Group = c("Oaks", "Oaks", "Oaks", "P. pinaster", "P. nigra", "P. sylvestris", "P. halepensis", "Other", "Other", "Other", "Other", "Other", "Other"))

# Cohort stats 
all_replicates <- data.frame()
for (i in seq_along(mgmt.scenarios)) {
  for (j in seq_along(replicates)) {
    one_replicate <- read.table(paste(di, mgmt.scenarios[i], "_rep", replicates[j], "/results/aggregated_coh_stats_dense_pines.txt", sep =""), header = TRUE) %>% 
      mutate(Harv_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][2],
             Clim_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][3])
    all_replicates <- rbind(all_replicates, one_replicate)
  }
}

coh_stats_dense_pines <- all_replicates %>%
  select(-Scenario) %>%  
  group_by(Harv_scenario, Clim_scenario, Time) %>%
  summarise(Avg_mean_count_accross_rep = mean(Avg_age_count),
            SD_mean_count_accross_rep = sd(Avg_age_count),
            Avg_total_med_accross_rep = mean(Avg_age_med),
            SD_total_med_accross_rep = sd(Avg_age_med),
            Avg_total_min_accross_rep = mean(Avg_age_min),
            SD_total_min_accross_rep = sd(Avg_age_min),
            Avg_total_agerich_accross_rep = mean(Avg_age_rich),
            SD_total_agerich_accross_rep = sd(Avg_age_rich),
            Avg_total_agesd_accross_rep = mean(Avg_age_sd),
            SD_total_agesd_accross_rep = sd(Avg_age_sd),
            Avg_total_spprich_accross_rep = mean(Avg_spp_rich),
            SD_total_spprich_accross_rep = sd(Avg_spp_rich))

coh_stats_dense_pines$Harv_scenario <- factor(coh_stats_dense_pines$Harv_scenario, 
                                     levels=c("No Management", "Conservative", "Proactive", "ProactivePlus"))

# Plots
jpeg(file = paste(di, outputs_folder, "count.jpeg", sep = ""), width = 12, height = 8, units="in", res=300)
coh_stats_dense_pines %>%
  ggplot(aes(x = Time, y = Avg_mean_count_accross_rep)) +
  geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
  geom_point(aes(color = Harv_scenario)) +
  geom_errorbar(aes(ymin = Avg_mean_count_accross_rep - SD_mean_count_accross_rep, 
                    ymax = Avg_mean_count_accross_rep + SD_mean_count_accross_rep), width=.2,
                position = position_dodge(.05)) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines) +
  ggtitle("Count: total number of cohorts in cell (same age, different species = 2)")
dev.off()

jpeg(file = paste(di, outputs_folder, "age_richness.jpeg", sep = ""), width = 12, height = 8, units="in", res=300)
coh_stats_dense_pines %>%
  ggplot(aes(x = Time, y = Avg_total_agerich_accross_rep)) +
  geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
  geom_point(aes(color = Harv_scenario)) +
  geom_errorbar(aes(ymin = Avg_total_agerich_accross_rep - SD_total_agerich_accross_rep, 
                    ymax = Avg_total_agerich_accross_rep + SD_total_agerich_accross_rep), width=.2,
                position = position_dodge(.05)) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines) +
  ggtitle("Age richness")
dev.off()

jpeg(file = paste(di, outputs_folder, "spp_richness.jpeg", sep = ""), width = 12, height = 8, units="in", res=300)
coh_stats_dense_pines %>%
  ggplot(aes(x = Time, y = Avg_total_spprich_accross_rep)) +
  geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
  geom_point(aes(color = Harv_scenario)) +
  geom_errorbar(aes(ymin = Avg_total_spprich_accross_rep - SD_total_spprich_accross_rep, 
                    ymax = Avg_total_spprich_accross_rep + SD_total_spprich_accross_rep), width=.2,
                position = position_dodge(.05)) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines) +
  ggtitle("Species richness")
dev.off()
