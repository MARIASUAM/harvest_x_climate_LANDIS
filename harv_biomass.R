# Maps of harvested biomass

### NAME OF SCENARIOS ####
mgmt.scenarios <- c(...) # Folder names with each scenario

harv_times <- c(10, 20, 30, 40, 50, 60, 70, 80, 90)

### SETUP ###
di <- ".../experiments/" # Path to simulations folder
outputs_folder <- "..." # Subfolder for outputs

library(raster)
library(dplyr)
library(ggplot2)

cols <- c('#91bfdb','#fc8d59')

# Load cells in focus areas
focus_cells <- read.table(paste0(di, "data/focus_cells.txt"), sep = ";", header = TRUE)

# Calculate number of cells (hectares) in each focus area
nr_cells <- focus_cells %>%
  dplyr::select(-Cell) %>%
  group_by(Area) %>%
  summarise(count = n())
  
# Load harvested biomass maps and extract harvested biomass in focus areas
harv_biomass <- data.frame()
for (i in seq_along(mgmt.scenarios)){
  for (j in seq_along(harv_times)){
    temp <- raster(paste(di, outputs_folder, "Harv_biomass_maps/", mgmt.scenarios[i], "_Harv_biomass_across_replicates_", harv_times[j], ".asc", sep = ""))
    
    temp_df <- data.frame(Cell = 1:length(temp[]),
                             Harvested = temp[]) %>%
      left_join(focus_cells) %>% # Identify those cells within each focus area
      filter(is.na(Area) == FALSE) %>% # Discard cells out of focus area
      dplyr::select(-Cell) %>%
      group_by(Area) %>%
      summarise(Harv_biomass_tn = sum(Harvested) / 100) %>% # Calculate total biomass harvested in each focus area
      left_join(nr_cells) %>%
      mutate(Harv_biomass_tnha = Harv_biomass_tn / count) %>% # Calculate harvested biomass per hectare
      mutate(Harv_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][2],
             Clim_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][3],
             Time = harv_times[j])
    
    harv_biomass <- rbind(harv_biomass, temp_df)
  }
}

# Fetch labels
Harv_scenario.labs <- data.frame(Harv_scenario_original = c("nomanag", "conserv", "proactive", "proactiveplus"),
                                 Harv_scenario = c('No Management', 'Conservative', 'Proactive', 'ProactivePlus'))

Clim_scenario.labs <- data.frame(Clim_scenario_original = c("current", "rcp45", "rcp85"),
                                 Clim_scenario = c("Current", "RCP4.5", "RCP8.5"))

harv_biomass <- harv_biomass %>%
  rename(Harv_scenario_original = Harv_scenario,
         Clim_scenario_original = Clim_scenario) %>%
  left_join(Harv_scenario.labs) %>%
  left_join(Clim_scenario.labs) %>%
  mutate(Year = Time + 2005)

harv_biomass$Harv_scenario <- factor(harv_biomass$Harv_scenario, 
                                     levels=c("No Management", "Conservative", "Proactive", "ProactivePlus"))
harv_biomass$Clim_scenario <- factor(harv_biomass$Clim_scenario, 
                                     levels=c("Current", "RCP4.5", "RCP8.5"))
# Plot
jpeg(file = paste(di, outputs_folder, "Harvest_focus_areas.jpeg", sep = ""), width = 12, height = 8, units="in", res=300)
ggplot(harv_biomass, aes(x = Year, y = Harv_biomass_tnha, fill = Area)) +
  geom_line(aes(color = Area)) +
  geom_point(aes(color = Area)) +
  facet_wrap(Clim_scenario ~ Harv_scenario) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  ylab("Harvested biomass (tn/ha)") +
  scale_color_manual(values = cols) +
  xlab("")
dev.off()
