# Stands diversity

VAR <- "SPP-RICH-"

### NAME OF SCENARIOS ####
mgmt.scenarios <- c("210816_nomanag_current_MIROC5")

### SETUP ###
di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/"
times <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90)

library(raster)
library(ggplot2)
library(rasterVis)
library(dplyr)
library(reshape)

### STANDS MAP
stands <- raster(paste(di, "0. sim_root/inputs_management/stands_vegetation.tif", sep = ""))
# plot(stands)

### Load sample layer
i <- 1
j <- 1
temp <- raster(paste(di, mgmt.scenarios[i], "/output/cohort-stats/", VAR, times[j], ".img", sep = ""))

## ADJUST STANDS MAP TO OUTPUT
extent(stands) <- extent(temp)
origin(stands) <- origin(temp)

### LOAD VARIABLE STACKS AND ADD TO DATAFRAME
stands_df <- data.frame(Cell = 1:length(stands[]), Stand = stands[])
for (i in 1:length(mgmt.scenarios)) {
  temp_stack <- stack()
  for (j in 1:length(times)) {
    temp <- raster(paste(di, mgmt.scenarios[i], "/output/cohort-stats/", VAR, times[j], ".img", sep = ""))
    stands_df[,j+2] <- temp[] 
    temp_stack <- stack(temp_stack, temp)
  }
  names(temp_stack) <- as.character(times)
  assign(paste(strsplit(mgmt.scenarios[i], split = "_")[[1]][2],
               strsplit(mgmt.scenarios[i], split = "_")[[1]][3],
               sep = "_"), temp_stack)
}
colnames(stands_df) <- c("Cell", "Stand", "time_0", "time_10", "time_20", "time_30", "time_40", "time_50", "time_60", "time_70", "time_80", "time_90")

### ANALYSE SPP RICHNESS BY STAND
stands_analysis <- stands_df %>%
  dplyr::select(-Cell) %>%
  filter(is.na(Stand) == FALSE) %>%
  group_by(Stand) %>%
  summarise_all(mean) 
stands_analysis <- as.data.frame(stands_analysis) %>%
  reshape::melt(id.vars = c("Stand"))

ggplot(stands_analysis, aes(x = value)) +
  geom_density() +
  facet_wrap(variable ~ .)
  