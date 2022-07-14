# Age

### SETUP
library(raster)
library(dplyr)

di <- "/Volumes/GoogleDrive/My Drive/proj_LANDIS/experiments/" # Path to simulations folder

all_spp <- c("ppinaster", "pnigra", "phalepensis", "psylvestris", "qilex", "qfaginea", "qpyrenaica", "jcommunis", "joxycedrus", "tall", "medium", "short", "popnigra")
pines_and_oaks <- c("ppinaster", "pnigra", "phalepensis", "psylvestris", "qilex", "qfaginea", "qpyrenaica")
non_pines_and_oaks <- c("jcommunis", "joxycedrus", "tall", "medium", "short", "popnigra")

times <- c(0)

months_ch <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")

# Management scenarios
mgmt.scenarios <- c("211129_nomanag_current_rep1",
                    "211129_conserv_current_rep1",
                    "211129_proactive_current_rep1",
                    "211129_proactiveplus_current_rep1",
                    
                    "211129_nomanag_current_rep2",
                    "211129_conserv_current_rep2",
                    "211129_proactive_current_rep2",
                    "211129_proactiveplus_current_rep2",
                    
                    "211129_nomanag_current_rep3",
                    "211129_conserv_current_rep3",
                    "211129_proactive_current_rep3",
                    "211129_proactiveplus_current_rep3",
                    
                    "211129_nomanag_current_rep4",
                    "211129_conserv_current_rep4",
                    "211129_proactive_current_rep4",
                    "211129_proactiveplus_current_rep4",
                    
                    "211129_nomanag_current_rep5",
                    "211129_conserv_current_rep5",
                    "211129_proactive_current_rep5",
                    "211129_proactiveplus_current_rep5") # Folder names with each replicate

### PINE PLANTATIONS MASKS
dense_pines_mask <- raster(paste(di, "data/dense_pines_mask.img", sep = ""))

### Age Distribution
for (i in 1:length(mgmt.scenarios)) {
  agedist_dense_pines_df <- data.frame()
  for (j in 1:length(times)) {
      temp <- raster(paste(di, mgmt.scenarios[i], "/output/AgeDist/Age_", times[j], ".img", sep = ""))
      temp_dense_pines_masked <- mask(temp, dense_pines_mask) # Mask by pine plantations
      agedist_dense_pines_df <- rbind(agedist_dense_pines_df,
                                  data.frame(Scenario = mgmt.scenarios[i], Time = times[j],
                                             Avg_age = cellStats(temp_dense_pines_masked, mean, na.rm=TRUE),
                                             SD_age = cellStats(temp_dense_pines_masked, sd, na.rm=TRUE)))
  }
  write.table(agedist_dense_pines_df, paste(di, mgmt.scenarios[i], "/results/aggregated_agedist_dense_pines.txt", sep = ""))
}

## Analyse
age_dist <- data.frame()
for (i in 1:length(mgmt.scenarios)) {
  temp <- read.table(paste(di, mgmt.scenarios[i], "/results/aggregated_agedist_dense_pines.txt", sep = "")) %>%
    mutate(Harv_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][2],
           Clim_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][3],
           Replicate = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][4])
  age_dist <- rbind(age_dist, temp)
}
