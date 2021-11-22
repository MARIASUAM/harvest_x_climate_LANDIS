# Analyse species age

### NAME OF SCENARIOS ####
mgmt.scenarios <- c("210913_conserv_current_MIROC5",
                    "210913_conserv_rcp45_MIROC5",
                    "210913_conserv_rcp85_MIROC5",
                    "210913_nomanag_current_MIROC5",
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
library(raster)

di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/"

spp <- c("pnigra", "ppinaster", "psylvestris", "phalepensis") #, "qilex", "qfaginea", "qpyrenaica")
times <- c(50, 70)

## ANALYSE ONE SPECIES
my_df <- data.frame()
for (i in 1:length(mgmt.scenarios)) {
  # temp_stack <- stack()
  for (h in 1:length(spp)) {
    for (j in 1:length(times)) {
      temp_avg <- raster(paste(di, mgmt.scenarios[i], "/output/cohort-stats/", spp[h], "-AVG-", times[j], ".img", sep = ""))
      temp_avg[temp_avg == 0] <- NA
      temp_max <- raster(paste(di, mgmt.scenarios[i], "/output/cohort-stats/", spp[h], "-MAX-", times[j], ".img", sep = ""))
      temp_max[temp_max == 0] <- NA
      temp_min <- raster(paste(di, mgmt.scenarios[i], "/output/cohort-stats/", spp[h], "-MIN-", times[j], ".img", sep = ""))
      temp_min[temp_min == 0] <- NA
      temp_df <- data.frame(Scenario = mgmt.scenarios[i],
                            Harv_scenario = strsplit(mgmt.scenarios[i], split = "_")[[1]][2],
                            # Clim_scenario = strsplit(mgmt.scenarios[i], split = "_")[[1]][1],
                            Time = times[j],
                            Species = spp[h],
                            AVG = temp_avg[],
                            MAX = temp_max[],
                            MIN = temp_min[]) %>%
        filter(is.na(AVG) == FALSE)
      my_df <- rbind(my_df, temp_df)
    }
  }
}

my_df %>%
  filter(Time == times[2]) %>%
  filter(Species == "ppinaster" | Species == "phalepensis" |
           Species == "psylvestris" | Species == "pnigra" ) %>%
  ggplot(aes(x = MIN, color = Harv_scenario)) + # , linetype = Clim_scenario
  geom_density() +
  facet_wrap(Species ~ .) +
  theme_classic() +
  theme(legend.position = "bottom") +
  ggtitle(paste("Density plot of MIN at time ", times[2], sep = ""))

