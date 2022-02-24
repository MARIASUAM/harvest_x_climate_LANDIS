# Figure of forest types based on AGBiomass

### SETUP ###
di <- "/Volumes/GoogleDrive/My Drive/proj_LANDIS/experiments/" # Path to simulations folder
outputs_folder <- "211129_outputs/" # Subfolder for outputs

library(raster)
library(dplyr)
library(ggplot2)
library(reshape2)

cols <- c("nomanag" = "#E31A1C", # red
          "conserv" = "#33A02C", # dark green
          "proactive" = "#6A3D9A", # dark purple
          "proactiveplus" = "#FF7F00") # orange
          
alphas <- c("current" = 0.33, "rcp45" = 0.66, "rcp85" = 1)

# Load file
ft <- read.table(paste0(di, outputs_folder, "Forest_types_percentages_change_new_mixed_class.txt"), header = TRUE, sep = ";")

# Fetch labels
ft$Harv_scenario <- factor(ft$Harv_scenario, 
                                        levels=c("nomanag", "conserv", "proactive", "proactiveplus"))
ft$Clim_scenario <- factor(ft$Clim_scenario, 
                                        levels=c("current", "rcp45", "rcp85"))
ft$Scenario <- factor(ft$Scenario, 
                           levels=c("nomanag_current", "nomanag_rcp45", "nomanag_rcp85", 
                                    "conserv_current", "conserv_rcp45", "conserv_rcp85", 
                                    "proactive_current", "proactive_rcp45", "proactive_rcp85", 
                                    "proactiveplus_current", "proactiveplus_rcp45", "proactiveplus_rcp85"))

# Plot
jpeg(file = paste(di, outputs_folder, "forest_types_change_new_mixed_class.jpeg", sep = ""), width = 12, height = 8, units="in", res=300)
ft %>%
  filter(Forest_type_code != 7, # Empty cells change not shown
         Forest_type_code != 6, # Mixed no dominance change not shown
         Forest_type_code != 5) %>% # Shrublands change not shown
  ggplot(aes(x = as.factor(Forest_type_code), y = Mean_percentage_change)) +
  geom_bar(stat = "identity", position="dodge", aes(colour = Harv_scenario, fill = Harv_scenario, alpha = Clim_scenario)) +
  geom_errorbar(aes(ymin = Mean_percentage_change - SD_percentage_change,
                    ymax = Mean_percentage_change + SD_percentage_change,
                    group = Scenario),
                width= .5, position = position_dodge(.9)) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 24),
        axis.text = element_text(size = 24),
        legend.text = element_text(size = 19)) +
  scale_colour_manual(name = "",
                      values = cols, labels=c('Non-management', 'Conservative', 'Proactive', 'Proactive-plus')) +
  scale_fill_manual(name = "",
                    values = cols, labels=c('Non-management', 'Conservative', 'Proactive', 'Proactive-plus')) +
  scale_alpha_manual(name = "",
                     values = alphas,
                     labels = c('Current', 'RCP4.5', 'RCP8.5')) +
  ylab("Percentage of change (%)") +
  xlab(NULL) +
  scale_x_discrete(breaks = c(1:7),
                   labels = c("Pure pines", "Mixed pine-dom.", "Pure oaks", "Mixed oak-dom.", "Shrublands", "Mixed no dom.", "Empty"))
dev.off()

# Plot total percentages
tot_ft <- read.table(paste0(di, outputs_folder, "Forest_types_total_percentages_new_mixed_class.txt"), header = TRUE, sep = ";") %>%
  select(-Replicate, -Perc_of_change, -Time) %>%
  rename(Beginning = Perc_time0,
         End = Perc_time_end) %>%
  group_by(Scenario, Harv_scenario, Clim_scenario, Forest_type_code) %>%
  summarise(Avg_End = mean(End),
            SD_End = sd(End),
            Avg_Beg = mean(Beginning),
            SD_Beg = sd(Beginning))
write.table(tot_ft, paste0(di, outputs_folder, "Forest_types_avg_sd_total_per_new_mixed_class.txt"), sep = ";")

