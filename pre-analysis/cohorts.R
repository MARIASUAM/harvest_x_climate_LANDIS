# Number of cohorts

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

### SETUP ###
di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/"

times <- c(0, 25, 55, 75, 95)

library(raster)
library(dplyr)

cols <- c("conserv" = "#33A02C", # dark green
          "proactive" = "#6A3D9A", # dark purple
          "proactiveplus" = "#FF7F00", # orange
          "nomanag" = "#E31A1C") # red

lines <- c("current" = "solid", "rcp45" = "dotdash", "rcp85" = "dashed")

### MASKS
# Pines mask
pines_mask <- raster(paste(di, "data/dense_pines_mask.img", sep = ""))
# plot(pines_mask)

active <- data.frame(mask = pines_mask[]) %>%
  filter(is.na(mask) == FALSE) %>%
  group_by(mask) %>%
  summarise(count = n())

# Full active landscape
active_map <- raster(paste(di, "0. sim_root/inputs_basic/ecoregions.tif", sep =""))
active <- data.frame(mask = active_map[]) %>%
  filter(is.na(mask) == FALSE) %>%
  mutate(mask = ifelse(is.na(mask) == FALSE, 1)) %>%
  group_by(mask) %>%
  summarise(count = n())
# plot(active_map)

extent(active_map) <- extent(pines_mask)
origin(active_map) <- origin(pines_mask)

## Load nr cohorts
nr_cohorts <- data.frame()
for (i in 1:length(mgmt.scenarios)) {
  for (j in 1:length(times)) {
    temp <- raster(paste(di, mgmt.scenarios[i], "/output/cohort-stats/AGE-COUNT-", times[j], ".img", sep = ""))
    temp <- mask(temp, pines_mask) # Or active_map
    
    nr_cohorts_in_temp <- data.frame(Harv_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][2],
                                     Clim_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][3],
                                     Time = times[j],
                                     Count = temp[],
                                     mask = pines_mask[]) %>% #active_map
      filter(is.na(mask) != TRUE) %>%
      dplyr::select(-mask) %>%
      group_by(Harv_scenario, Clim_scenario, Time) %>%
      summarise(Total_nr_cohorts = sum(Count))
                                     
    nr_cohorts <- rbind(nr_cohorts, as.data.frame(nr_cohorts_in_temp))
  }
}

ggplot(nr_cohorts, aes(x = Time, y = Total_nr_cohorts)) +
  geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
  geom_point(aes(color = Harv_scenario)) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines)
# +
#   ylim(600000, 1200000)
         
