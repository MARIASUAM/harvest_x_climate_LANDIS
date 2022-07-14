# Regeneration of oaks and pines

library(ggplot2)
library(raster)
library(rasterVis)

mgmt.scenarios <- c("211129_nomanag_current",
                    "211129_conserv_current",
                    "211129_proactive_current",
                    "211129_proactiveplus_current",
                    "211129_nomanag_rcp45",
                    "211129_conserv_rcp45",
                    "211129_proactive_rcp45",
                    "211129_proactiveplus_rcp45",
                    "211129_nomanag_rcp85",
                    "211129_conserv_rcp85",
                    "211129_proactive_rcp85",
                    "211129_proactiveplus_rcp85") # Folder names with each scenario

# Labels
labels <- data.frame(Scenario = c("211129_nomanag_current",
                                  "211129_conserv_current",
                                  "211129_proactive_current",
                                  "211129_proactiveplus_current",
                                  "211129_nomanag_rcp45",
                                  "211129_conserv_rcp45",
                                  "211129_proactive_rcp45",
                                  "211129_proactiveplus_rcp45",
                                  "211129_nomanag_rcp85",
                                  "211129_conserv_rcp85",
                                  "211129_proactive_rcp85",
                                  "211129_proactiveplus_rcp85"), # Folder names with each scenario
                     Harv_sce = c("Non-management", "Conservative", "Proactive", "Proactive-plus",
                                  "Non-management", "Conservative", "Proactive", "Proactive-plus",
                                  "Non-management", "Conservative", "Proactive", "Proactive-plus"),
                     Clim_sce = c("Current", "Current", "Current", "Current",
                                  "RCP4.5", "RCP4.5", "RCP4.5", "RCP4.5", 
                                  "RCP8.5", "RCP8.5", "RCP8.5", "RCP8.5"))

### SETUP
di <- "/Volumes/GoogleDrive/My Drive/proj_LANDIS/experiments/" # Path to simulations folder
outputs_folder <- "211129_outputs/" # Subfolder for outputs

### PINE PLANTATIONS MASK
pines_mask <- raster(paste(di, "data/dense_pines_mask.img", sep = ""))

# Load maps 
oaks <- stack()
pines <- stack()
for (i in seq_along(mgmt.scenarios)) {
  temp_oaks <- raster(paste0(di, outputs_folder, "AGB_maps/", mgmt.scenarios[i], "_Oaks_first_cohort_AGB_avg_across_replicates_95.asc"))
  names(temp_oaks) <- paste(labels$Harv_sce[i], labels$Clim_sce[i], sep = "-")
  oaks <- stack(oaks, temp_oaks)
  
  temp_pines <- raster(paste0(di, outputs_folder, "AGB_maps/", mgmt.scenarios[i], "_Pines_first_cohort_AGB_avg_across_replicates_95.asc"))
  names(temp_pines) <- paste(labels$Harv_sce[i], labels$Clim_sce[i], sep = "-")
  pines <- stack(pines, temp_pines)
}

oaks <- mask(oaks, pines_mask)
pines <- mask(pines, pines_mask)

# Plot
jpeg(file = paste(di, outputs_folder, "Oaks_AGB_first_cohort_time95.jpeg", sep = ""), width = 18, height = 12, units="in", res=300)
gplot(oaks) +
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable, ncol = 4) +
  coord_equal() +
  scale_fill_gradient2(low = '#d7191c', mid = '#ffffbf', high = '#2c7bb6', na.value = "transparent",
                       limits = c(min(0), max(400))) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.text = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 24),
        legend.key.size = unit(1, "cm")) +
  ggtitle("Oaks") +
  ylab("") +
  xlab("")
dev.off()

jpeg(file = paste(di, outputs_folder, "Pines_AGB_first_cohort_time95.jpeg", sep = ""), width = 18, height = 12, units="in", res=300)
gplot(pines) +
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable, ncol = 4) +
  coord_equal() +
  scale_fill_gradient2(low = '#d7191c', mid = '#ffffbf', high = '#2c7bb6', na.value = "transparent",
                       limits = c(min(0), max(400))) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.text = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 24),
        legend.key.size = unit(1, "cm")) +
  ggtitle("Pines") +
  ylab("") +
  xlab("")
dev.off()
