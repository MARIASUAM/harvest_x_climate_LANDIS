# Analysis of forest types based on AGBiomass

### NAME OF SCENARIOS ####
mgmt.scenarios <- c(...) # Folder names with each scenario

replicates <- c(1:5) 

### SETUP ###
di <- "/Volumes/GoogleDrive/My Drive/proj_LANDIS/experiments/" # Path to simulations folder
outputs_folder <- "211129_outputs/" # Subfolder for outputs

end_year <- 95
times <- c(0, end_year)

library(raster)
library(dplyr)

### PINE PLANTATIONS MASK
pines_mask <- raster(paste(di, "data/dense_pines_mask.img", sep = ""))
# plot(pines_mask)

active <- data.frame(mask = pines_mask[]) %>%
  filter(is.na(mask) == FALSE) %>%
  group_by(mask) %>%
  summarise(count = n())

tot_cells <- active$count

### GENERATE DATA FRAME WITH FOREST TYPES
ft_df <- data.frame()
for (i in seq_along(mgmt.scenarios)) {
  for (j in seq_along(times)) {
    for (h in seq_along(replicates)){
      # Load AGB layers
      pines_agb <- raster(paste(di, mgmt.scenarios[i], "_rep", replicates[h], "/results/Pines_AGB_", times[j], ".asc", sep = ""))
      oaks_agb <- raster(paste(di, mgmt.scenarios[i], "_rep", replicates[h], "/results/Oaks_AGB_", times[j], ".asc", sep = ""))
      shrubs_agb <- raster(paste(di, mgmt.scenarios[i], "_rep", replicates[h], "/results/Shrubs_AGB_", times[j], ".asc", sep = ""))
      total_agb <- raster(paste(di, mgmt.scenarios[i], "_rep", replicates[h], "/results/Total_AGB_", times[j], ".asc", sep = ""))
      
      # Calculate proportion of each group
      prop_pines <- pines_agb / total_agb
      prop_oaks <- oaks_agb / total_agb
      prop_shrubs <- shrubs_agb / total_agb
  
      # Extract to data frame
      raster_to_df <- data.frame(Cell = 1:length(pines_mask[]),
                                 Scenario = paste(strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][2], strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][3], sep = "_"),
                                 Harv_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][2],
                                 Clim_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][3],
                                 Time = times[j],
                                 Replicate = replicates[h],
                                 Mask = pines_mask[],
                                 Total_gm2 = total_agb[],
                                 Prop_pines = prop_pines[],
                                 Prop_oaks = prop_oaks[],
                                 Prop_shrubs = prop_shrubs[]) %>%
        filter(is.na(Mask) == FALSE)
      
      # Reclassify cells by forest types
      classification <- raster_to_df %>%
        mutate(Forest_type_code = ifelse(Total_gm2 == 0, 7, # Empty cells - set a higher threshold?
                                         ifelse(Prop_pines >= 0.9, 1, # "Pure pines", # Mainly pines
                                                ifelse(Prop_oaks >= 0.9, 3, # "Pure oaks", # Mainly oaks
                                                       ifelse(Prop_shrubs >= 0.9, 5, # "Shrublands", 
                                                              ifelse(Prop_pines >= 0.5, 2, # "Pine-dominated",
                                                                     ifelse(Prop_oaks >= 0.5, 4, # "Oak-dominated",
                                                                            ifelse(Prop_shrubs >= 0.5, 5, # "Shrublands",
                                                                                   6))))))))
      
      # Calculate percentage of the landscape covered by each forest type
      perc_ft <- classification %>%
        dplyr::select(Scenario, Harv_scenario, Clim_scenario, Time, Replicate, Forest_type_code) %>%
        group_by(Scenario, Harv_scenario, Clim_scenario, Time, Replicate, Forest_type_code) %>%
        summarise(nr_cells = n()) %>%
        mutate(Percentage = (nr_cells * 100) / tot_cells) %>%
        dplyr::select(Scenario, Harv_scenario, Clim_scenario, Time, Replicate,
                      Forest_type_code, Percentage)
      
      ft_df <- rbind(ft_df, perc_ft)
    }
  }
}

# Calculate percentage of change between initial and final time step
perc_ft_time0 <- ft_df %>%
  filter(Time == 0) %>%
  mutate(Perc_time0 = ifelse(is.na(Percentage) == FALSE, Percentage, 0)) %>%
  dplyr::select(-Time, -Percentage)
perc_ft_time0 <- perc_ft_time0[,c(-1)]

change_perc_ft <- ft_df %>% 
  filter(Time == end_year) %>%
  mutate(Perc_time_end = ifelse(is.na(Percentage) == FALSE, Percentage, 0)) %>%
  dplyr::select(-Time, -Percentage) %>%
  left_join(perc_ft_time0) %>%
  mutate(Perc_of_change = Perc_time_end - Perc_time0)
write.table(change_perc_ft, paste0(di, outputs_folder, "Forest_types_total_percentages_new_mixed_class.txt"), sep = ";")

# Calculate mean across replicates
mean_accross_rep <- change_perc_ft %>%
  dplyr::select(-Replicate) %>%
  group_by(Scenario, Harv_scenario, Clim_scenario, Time, Forest_type_code) %>%
  summarise(Mean_percentage_change = mean(Perc_of_change),
            SD_percentage_change = sd(Perc_of_change),
            Mean_percentage_0 = mean(Perc_time0),
            SD_percentage_0 = sd(Perc_time0),
            Mean_percentage_end = mean(Perc_time_end),
            SD_percentage_end = sd(Perc_time_end))

write.table(mean_accross_rep, paste0(di, outputs_folder, "Forest_types_percentages_change_new_mixed_class.txt"), sep = ";")
