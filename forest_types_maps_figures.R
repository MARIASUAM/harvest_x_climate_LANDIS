# Plot maps of forest types

library(raster)
library(rasterVis)
library(ggplot2)
library(dplyr)

### NAME OF SCENARIOS ####
mgmt.scenarios <- c("211129_nomanag_current",
                    "211129_nomanag_rcp45",
                    "211129_nomanag_rcp85",
                    
                    "211129_conserv_current",
                    "211129_conserv_rcp45",
                    "211129_conserv_rcp85",
                    
                    "211129_proactive_current",
                    "211129_proactive_rcp45",
                    "211129_proactive_rcp85",
                    
                    "211129_proactiveplus_current",
                    "211129_proactiveplus_rcp45",
                    "211129_proactiveplus_rcp85") # Folder names with each scenario

### SETUP ###
di <- "/Volumes/GoogleDrive/My Drive/proj_LANDIS/experiments/" # Path to simulations folder
outputs_folder <- "211129_outputs/" # Subfolder for outputs

times <- c(0, 95)

# Load maps per replicate group
replicate <- "rep_1"
time_step <- "time_0"
rep_stack <- stack()
for (i in seq_along(mgmt.scenarios)) {
    temp <- raster(paste(di, outputs_folder, "Forest_types_maps/",
                         strsplit(mgmt.scenarios[i], split = "_")[[1]][2], "_",
                         strsplit(mgmt.scenarios[i], split = "_")[[1]][3], "_",
                         time_step,  "_", replicate, 
                         "new_mixed_class_full_aoi.tif", sep = "")) # "new_mixed_class.tif", sep = ""))
    names(temp) <- paste(strsplit(mgmt.scenarios[i], split = "_")[[1]][2], "_",
                         strsplit(mgmt.scenarios[i], split = "_")[[1]][3],
                         sep = "")
    rep_stack <- stack(rep_stack, temp)
}

# Group together shrublands, mixed and empty 
# rep_stack[rep_stack == 6] <- 5
# rep_stack[rep_stack == 7] <- 5

# Plot and export
# jpeg(file = paste(di, outputs_folder, "Forest_types_maps/", time_step, "_", replicate, "_new_mixed_class.jpeg", sep = ""), width = 18, height = 12, units="in", res=300)
jpeg(file = paste(di, outputs_folder, "Forest_types_maps/211129_nomanag_current_", time_step, "_", replicate, "_new_mixed_class_full_aoi.jpeg", sep = ""), width = 18, height = 12, units="in", res=300)
# jpeg(file = paste(di, outputs_folder, "Forest_types_maps/", time_step, "_", replicate, "_new_mixed_class_full_aoi.jpeg", sep = ""), width = 18, height = 12, units="in", res=300)
gplot(rep_stack) + 
  geom_tile(aes(fill = as.factor(value))) +
  facet_wrap(~ variable, ncol = 4) +
  coord_equal() +
  theme_classic() +
  # New mixed class colors
  scale_fill_manual(values = c('#d95f02', # Pure pines - red (#d73027), another red (#d95f02)
                               '#fc8d62', # Pine-dominated - dark pink (#c51b8a), orange (#fc8d59), another red light (#fc8d62)
                               '#1b9e77', # Pure oaks - blue (#4575b4), another green (#1b9e77)
                               '#66c2a5', # Oak-dominated - medium pink (#f768a1), light blue (#91bfdb), another green light (#66c2a5)
                               # '#636363'), # Others (shrublands, mixed, empty grouped together)
                               '#7570b3', # 5: Shrublands - green (#1a9850), purple (#7570b3)
                               '#e7298a', # 6: Mixed no dom. - white (#ffffff), pink (#e7298a)
                               '#f6e8c3'), # 7 Empty - black (#000000), light yellow (#f6e8c3)
                      labels = c("Pure pines",
                                 "Mixed pine-dom.",
                                 "Pure oaks",
                                 "Mixed oak-dom.",
                                 # "Others"), na.value = "transparent") +
                                 "Shrublands",
                                 "Mixed no dom.",
                                 "Empty"), na.value = "transparent") +
  theme(legend.position = "bottom",
        axis.text = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 20)) +
  ggtitle(replicate) +
  ylab("") +
  xlab("")
dev.off()

# forest type cover at initial conditions

df <- data.frame(Forest_type = temp[]) %>%
  group_by(Forest_type) %>%
  summarise(count = n()) %>%
  filter(is.na(Forest_type) == FALSE)

total <- sum(df$count)  

percentages <- df %>%
  mutate(Percentage = count * 100 / total,
         Forest = c("Pure pines", "Mixed pine-dom.", "Pure oaks", "Mixed oak-dom.","Shrublands","Mixed no dom.","Empty"))
