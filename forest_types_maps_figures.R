# Plot maps of forest types

library(raster)
library(rasterVis)
library(ggplot2)

### NAME OF SCENARIOS ####
mgmt.scenarios <- c("210927_nomanag_current",
                    "210927_conserv_current",
                    "210927_proactive_current",
                    "210927_proactiveplus_current",
                    "210927_nomanag_rcp45",
                    "210927_conserv_rcp45",
                    "210927_proactive_rcp45",
                    "210927_proactiveplus_rcp45",
                    "210927_nomanag_rcp85",
                    "210927_conserv_rcp85",
                    "210927_proactive_rcp85",
                    "210927_proactiveplus_rcp85")

### SETUP ###
di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/"
outputs_folder <- "210927_outputs/"

times <- c(0, 95)

# Load maps per replicate group
replicate <- "rep_5"
time_step <- "time_95"
rep_stack <- stack()
for (i in seq_along(mgmt.scenarios)) {
    temp <- raster(paste(di, outputs_folder, "Forest_types_maps/",
                         strsplit(mgmt.scenarios[i], split = "_")[[1]][2], "_",
                         strsplit(mgmt.scenarios[i], split = "_")[[1]][3], "_",
                         time_step,  "_", replicate, ".tif", sep = ""))
    names(temp) <- paste(strsplit(mgmt.scenarios[i], split = "_")[[1]][2], "_",
                         strsplit(mgmt.scenarios[i], split = "_")[[1]][3],
                         sep = "")
    rep_stack <- stack(rep_stack, temp)
}

# Plot and export
jpeg(file = paste(di, outputs_folder, "Forest_types_maps/", time_step, "_", replicate, ".jpeg", sep = ""), 
     width = 18, height = 12, units="in", res=300)
gplot(rep_stack) + 
  geom_tile(aes(fill = as.factor(value))) +
  facet_wrap(~ variable, ncol = 4) +
  coord_equal() +
  theme_classic() +
  scale_fill_manual(values = c('#d73027','#fc8d59',
                               '#4575b4','#91bfdb',
                               '#ffffbf','#e0f3f8','#f5f5f5'),
                    labels = c("Pure pines", "Pine-dominated", 
                               "Pure oaks", "Oak-dominated",
                               "Shrublands", "Mixed", "Empty"),
                    na.value = "transparent") +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  ggtitle("") +
  ylab("") +
  xlab("")
dev.off()

