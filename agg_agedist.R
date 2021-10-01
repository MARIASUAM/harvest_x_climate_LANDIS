# Analysis of Age distribution on aggregated tables

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

groups <- data.frame(Species = c("qfaginea", "qpyrenaica", "qilex", "ppinaster", "pnigra", "psylvestris", "phalepensis", "tall", "medium", "short", "jcommunis", "joxycedrus", "popnigra"),
                     Group = c("Oaks", "Oaks", "Oaks", "P. pinaster", "P. nigra", "P. sylvestris", "P. halepensis", "Other", "Other", "Other", "Other", "Other", "Other"))

# AgeDist 
agedist_dense_pines <- data.frame()
agedist_pines <- data.frame()
for (i in 1:length(mgmt.scenarios)) {
  temp <- read.table(paste(di, mgmt.scenarios[i], "/results/aggregated_agedist_dense_pines.txt", sep =""), header = TRUE) %>%
    mutate(Harv_scenario = strsplit(as.character(Scenario), split = "_")[[1]][2],
           Clim_scenario = strsplit(as.character(Scenario), split = "_")[[1]][3])
  agedist_dense_pines <- rbind(agedist_dense_pines, temp)
  
  temp <- read.table(paste(di, mgmt.scenarios[i], "/results/aggregated_agedist_pines.txt", sep =""), header = TRUE) %>%
    mutate(Harv_scenario = strsplit(as.character(Scenario), split = "_")[[1]][2],
           Clim_scenario = strsplit(as.character(Scenario), split = "_")[[1]][3])
  agedist_pines <- rbind(agedist_pines, temp)
}

jpeg(file = paste(di, "outputs/210921_agedist_pines_mask.jpeg", sep = ""), width=6, height=4, units="in", res=300)
agedist_pines %>%
  ggplot(aes(x = Time, y = Avg_age)) +
  geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
  geom_point(aes(color = Harv_scenario)) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines)
dev.off()

jpeg(file = paste(di, "outputs/210921_agedist_dense_pines_mask.jpeg", sep = ""), width=6, height=4, units="in", res=300)
agedist_dense_pines %>%
  ggplot(aes(x = Time, y = Avg_age)) +
  geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
  geom_point(aes(color = Harv_scenario)) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines)
dev.off()