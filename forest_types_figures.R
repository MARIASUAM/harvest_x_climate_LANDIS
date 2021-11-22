# Figure of forest types based on AGBiomass

### SETUP ###
di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/"
outputs_folder <- "210927_outputs/"

library(raster)
library(dplyr)
library(ggplot2)

cols <- c("conserv" = "#33A02C", # dark green
          "proactive" = "#6A3D9A", # dark purple
          "proactiveplus" = "#FF7F00", # orange
          "nomanag" = "#E31A1C") # red
lines <- c("current" = "solid", "rcp45" = "dotdash", "rcp85" = "dashed")
alphas <- c("current" = 0.33, "rcp45" = 0.66, "rcp85" = 1)

# Load file
ft <- read.table(paste0(di, outputs_folder, "Forest_types_percentages_change.txt"), header = TRUE, sep = ";")

# Plot
ft$Harv_scenario <- as.factor(ft$Harv_scenario)
levels(ft$Harv_scenario)<- c("nomanag", "conserv", "proactive", "proactiveplus")

jpeg(file = paste(di, outputs_folder, "forest_types_change.jpeg", sep = ""), width = 12, height = 8, units="in", res=300)
ft %>%
  filter(Forest_type_code != 7) %>%
  ggplot(aes(x = as.factor(Forest_type_code), y = Mean_percentage_change)) +
  geom_bar(stat = "identity", position="dodge", aes(colour = Harv_scenario, fill = Harv_scenario, alpha = Clim_scenario)) +
  geom_errorbar(aes(ymin = Mean_percentage_change - SD_percentage_change, 
                    ymax = Mean_percentage_change + SD_percentage_change,
                    group = Scenario),
                width= .5, position = position_dodge(.9)) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14)) +
  scale_colour_manual(name = "",
                      values = cols, labels=c('Conservative', 'Proactive', 'Proactive+', 'No Management')) +
  scale_fill_manual(name = "",
                    values = cols, labels=c('Conservative', 'Proactive', 'Proactive+', 'No Management')) +
  scale_alpha_manual(name = "",
                     values = alphas,
                     labels = c('Current', 'RCP4.5', 'RCP8.5')) +
  ylab("Percentage of change (%)") +
  xlab(NULL) +
  scale_x_discrete(breaks = c(1:7),
                   labels = c("Pure pines", "Pine-dominated", "Pure oaks", "Oak-dominated", "Shrublands", "Mixed", "Empty"))
dev.off()
