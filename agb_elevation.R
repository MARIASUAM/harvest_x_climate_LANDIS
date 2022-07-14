# Elevation trend in biomass

### SETUP
di <- "/Volumes/GoogleDrive/My Drive/proj_LANDIS/experiments/" # Path to simulations folder
outputs_folder <- "211129_outputs/" # Subfolder for outputs

library(raster)
library(dplyr)
library(tidyr)
library(ggplot2)

### Load pines mask
pines_mask_asc <- raster(paste(di, "data/dense_pines_mask.asc", sep = ""))

proj3042 <- CRS(SRS_string="EPSG:3042")
crs(pines_mask_asc) <- proj3042

mask_df <- data.frame(Cell = 1:length(pines_mask_asc[]),
                      Mask = pines_mask_asc[])

### Load elevation map
mdt <- raster("/Users/maria.suarez.munoz/Documents/Mapas/MDT25/MDT25.tif")
crs(mdt)

### Crop and aggregate elevation map to fit working resolution
mdt_cropped <- crop(mdt, pines_mask_asc)
mdt_agg <- aggregate(mdt_cropped, fact = 4)

### Create data frame to store elevation values
elev_df <- data.frame(Cell = 1:length(mdt_agg[]),
                      Elevation = mdt_agg[])

### Explore elevation and define elevation ranges
elev_ranges <- c(100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,2100,2200,2300,2400,2500,2600,2700,2800,2900,3000) # create bins

elev_class <- as.data.frame(cbind(elev_df$Elevation, findInterval(elev_df$Elevation, elev_ranges))) %>%
  rename(Elevation = V1, Elev_range = V2) %>%
  mutate(Elev_range = (Elev_range * 100) + 50)

### Load agb maps and store in data frame

mgmt.scenarios <- c("211129_conserv_current",
                    "211129_conserv_rcp45",
                    "211129_conserv_rcp85",
                    "211129_nomanag_current",
                    "211129_nomanag_rcp45",
                    "211129_nomanag_rcp85",
                    "211129_proactive_current",
                    "211129_proactive_rcp45",
                    "211129_proactive_rcp85",
                    "211129_proactiveplus_current",
                    "211129_proactiveplus_rcp45",
                    "211129_proactiveplus_rcp85") # Folder names with each replicate

oaks_and_pines <-c("ppinaster", "pnigra", "phalepensis", "psylvestris", "qilex", "qfaginea", "qpyrenaica")
times <- c(0,95)

df <- data.frame(Cell = 1:length(pines_mask_asc[]))
for (i in 1:length(mgmt.scenarios)) {
  for (j in 1:length(times)) {
    for (h in 1:length(oaks_and_pines)) {
      temp <- raster(paste(di, "211129_outputs/AGB_maps/", mgmt.scenarios[i], "_AGB_", oaks_and_pines[h], "_", times[j], ".asc", sep = ""))
      df_temp <- data.frame(AGB = temp[])
      name <- paste(mgmt.scenarios[i], oaks_and_pines[h], times[j], sep = "_")
      colnames(df_temp) <- name
      
      df <- cbind(df, df_temp)
    }
  }
}

### Rename df columns (problems with numbers)
df_renamed <- df
colnames(df_renamed) <- paste0("X", colnames(df))

### Process data
elev_agb <- elev_df %>% 
  full_join(mask_df) %>% # join elevation and pines mask
  full_join(df_renamed, by = c("Cell" = "XCell")) %>% # join with agb data
  filter(Mask == 1) %>% # filter by mask
  dplyr::select(-Mask, -Cell) %>% # Discard unnecessary cells
  gather(layer, agb, X211129_conserv_current_ppinaster_0:X211129_proactiveplus_rcp85_qpyrenaica_95) # Convert from wide to long

### Create columns for scenarios, species and times; add elevation range class
clean_elev_agb <- elev_agb %>%
  separate(layer, c("first", "Harv_scenario", "Clim_scenario", "Species", "Time")) %>%
  left_join(elev_class)

### Aggregate by elevation class
agg_elev_agb <- clean_elev_agb %>%
  select(-first, -Elevation) %>%
  group_by(Harv_scenario, Clim_scenario, Species, Time, Elev_range) %>%
  summarise(Avg_agb = mean(agb),
            SD_agb = sd(agb)) %>%
  mutate(Year = as.numeric(Time) + 2005)

# write.table(agg_elev_agb, paste0(di, "211129_outputs/landscape_agg_elev_agb.txt"), sep = ";")
# agg_elev_agb <- read.table(paste0(di, "211129_outputs/landscape_agg_elev_agb.txt"), sep = ";")

### Fetch scenario labels
Harv_scenario.labs <- data.frame(Harv_scenario_original = c("nomanag", "conserv", "proactive", "proactiveplus"),
                                 Harv_scenario = c('Non-management', 'Conservative', 'Proactive', 'Proactive-plus'))

Clim_scenario.labs <- data.frame(Clim_scenario_original = c("current", "rcp45", "rcp85"),
                                 Clim_scenario = c("Current", "RCP4.5", "RCP8.5"))

Species.labs <- data.frame(Species = c("ppinaster", "pnigra", "phalepensis", "psylvestris", "qilex", "qfaginea", "qpyrenaica"),
                           Species_latin = c("P. pinaster", "P. nigra", "P. halepensis", "P. sylvestris", "Q. ilex", "Q. faginea", "Q. pyrenaica"))

agg_elev_agb <- agg_elev_agb %>%
  rename(Harv_scenario_original = Harv_scenario,
         Clim_scenario_original = Clim_scenario) %>%
  left_join(Harv_scenario.labs) %>%
  left_join(Clim_scenario.labs) %>%
  left_join(Species.labs)

agg_elev_agb$Harv_scenario <- factor(agg_elev_agb$Harv_scenario, 
                                        levels=c("Non-management", "Conservative", "Proactive", "Proactive-plus"))
agg_elev_agb$Clim_scenario <- factor(agg_elev_agb$Clim_scenario, 
                                        levels=c("Current", "RCP4.5", "RCP8.5"))

### Plot
jpeg(file = paste(di, outputs_folder, "elevation_qpyrenaica.jpeg", sep = ""), width=12, height=4, units="in", res=300)
sp <- "Q. pyrenaica"
agg_elev_agb %>%
  filter(Species_latin == sp,
         Harv_scenario == "Non-management") %>%
  ggplot(aes(x = Elev_range, y = Avg_agb / 100, color = as.factor(Year))) +
  geom_point() +
  geom_line() + 
  # geom_ribbon(aes(ymin = (Avg_agb - SD_agb)/ 100,
  #                 ymax = (Avg_agb + SD_agb)/ 100, x = Elev_range, 
  #                 fill = as.factor(Year)), alpha = 0.3) +
  facet_grid(. ~ Clim_scenario) + #Harv_scenario
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  ylab("Aboveground biomass (tn/ha)") +
  xlab("Elevation") +
  ggtitle(sp)
dev.off()

