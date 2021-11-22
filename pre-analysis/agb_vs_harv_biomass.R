# Analysis of AGBiomass and harvested biomass

VAR <- "agbiomass"

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

### SETUP ###
di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/"
times <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90)
species <- c("jcommunis", "joxycedrus", "tall", "medium", "short",
             "ppinaster", "pnigra", "phalepensis", "psylvestris",
             "qilex", "qfaginea", "qpyrenaica", "popnigra")
library(raster)
library(ggplot2)
library(rasterVis)
library(dplyr)

cols <- c("conserv" = "#33A02C", # dark green
          "proactive" = "#6A3D9A", # dark purple
          "proactiveplus" = "#FF7F00", # orange
          "nomanag" = "#E31A1C") # red
lines <- c("current" = "solid", "rcp45" = "dotdash", "rcp85" = "dashed")

### MASKS
pines_mask <- raster(paste(di, "data/pines_mask.img", sep = ""))
# plot(pines_mask)
MAs <- raster(paste(di, "0. sim_root/inputs_management/management_areas.tif", sep =""))
# plot(MAs)
MA1 <- raster(paste(di, "0. sim_root/inputs_management/management_areas.tif", sep =""))
MA1[MA1 != 1] <- NA
# plot(MA1)
MA2 <- raster(paste(di, "0. sim_root/inputs_management/management_areas.tif", sep =""))
MA2[MA2 != 2] <- NA
# plot(MA2)
extent(MA2) <- extent(pines_mask)
origin(MA2) <- origin(pines_mask)

### LOAD VARIABLE STACKS AND GENERATE DATA FRAME
my_df <- data.frame()
for (i in 1:length(mgmt.scenarios)) {
  # total_biomass <- stack()
  for (j in 1:length(times)) {
    temp_stack <- stack()
    for (h in 1:length(species)) {
      temp <- raster(paste(di, mgmt.scenarios[i], "/output/", VAR, "/", species[h], "/AGBiomass", times[j], ".img", sep = ""))
      # temp <- mask(temp, MA2)
      temp_stack <- stack(temp_stack, temp)
    }
    total_biomass_one_timestep_gm2 <- calc(temp_stack, sum)
    total_biomass_one_timestep_tnha <- total_biomass_one_timestep_gm2 / 100
    # total_biomass <- stack(total_biomass, total_biomass_one_timestep)
    my_row <- data.frame(Scenario = mgmt.scenarios[i],
                         Harv_scenario = strsplit(mgmt.scenarios[i], split = "_")[[1]][2],
                         Clim_scenario = strsplit(mgmt.scenarios[i], split = "_")[[1]][3],
                         Time = times[j],
                         Total_biomass_tn = cellStats(total_biomass_one_timestep_tnha, sum))
    my_df <- rbind(my_df, my_row)
    }
  # names(total_biomass) <- as.character(times)
  # assign(mgmt.scenarios[i], total_biomass)
}

# Load harvest outputs and build comparison table
summary_comp <- data.frame()
for (i in 1:length(mgmt.scenarios)) {
  summary <- read.table(paste(di, mgmt.scenarios[i], "/output/harvest/summary-log.csv", sep = ""), header = TRUE, sep = ",") %>%
    mutate(Scenario = mgmt.scenarios[i],
           Harv_scenario = strsplit(mgmt.scenarios[i], split = "_")[[1]][2],
           Clim_scenario = strsplit(mgmt.scenarios[i], split = "_")[[1]][3]) 
  summary_comp <- rbind(summary_comp, summary)
}

harvested_biomass <- summary_comp %>%
  # filter(ManagementArea == 2) %>%
  dplyr::select(Scenario, Harv_scenario, Clim_scenario, Time, TotalBiomassHarvested) %>%
  group_by(Scenario, Harv_scenario, Clim_scenario, Time) %>%
  summarise(Total_harvested_biomass_tn = sum(TotalBiomassHarvested, na.rm = TRUE))

whole_df <- left_join(my_df, harvested_biomass) # If all time steps are considered for agb, harvested_biomass will have NAs, and geom_line breaks at NA values.
only_full_points_df <- inner_join(my_df, harvested_biomass)
  
ggplot(only_full_points_df) +
  geom_point(aes(x = Time, y = Total_biomass_tn, color = "Total_biomass_tn")) +
  geom_line(aes(x = Time, y = Total_biomass_tn, color = "Total_biomass_tn")) +
  geom_point(aes(x = Time, y = Total_harvested_biomass_tn, color = "Total_harvested_biomass_tn")) +
  geom_line(aes(x = Time, y = Total_harvested_biomass_tn, color = "Total_harvested_biomass_tn")) +
  facet_wrap(Scenario ~ .) +
  theme_classic() +
  theme(legend.position = "bottom")
  
  