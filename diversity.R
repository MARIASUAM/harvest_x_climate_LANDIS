# Analysis of diversity

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
end_year <- 95
times <- c(0, end_year) # c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95)
all_spp <-c("ppinaster", "pnigra", "phalepensis", "psylvestris", "qilex", "qfaginea", "qpyrenaica", "jcommunis", "joxycedrus", "tall", "medium", "short", "popnigra")

library(raster)
library(ggplot2)
library(rasterVis)
library(dplyr)
library(reshape)
library(fmsb)
library(tidyr)

cols <- c("conserv" = "#33A02C", # dark green
          "proactive" = "#6A3D9A", # dark purple
          "proactiveplus" = "#FF7F00", # orange
          "nomanag" = "#E31A1C") # red
lines <- c("current" = "solid", "rcp45" = "dotdash", "rcp85" = "dashed")
alphas <- c("current" = 1, "rcp45" = 0.66, "rcp85" = 0.33)

### PINE PLANTATIONS MASK
pines_mask <- raster(paste(di, "data/dense_pines_mask.img", sep = ""))
# plot(pines_mask)
active <- data.frame(mask = pines_mask[]) %>%
  filter(is.na(mask) == FALSE) %>%
  group_by(mask) %>%
  summarise(count = n())
tot_cells <- active$count

# FULL LANDSCAPE MASK
# active_map <- raster(paste(di, "0. sim_root/inputs_basic/ecoregions.tif", sep =""))
# active <- data.frame(mask = active_map[]) %>%
#   filter(is.na(mask) == FALSE) %>%
#   mutate(mask = ifelse(is.na(mask) == FALSE, 1)) %>%
#   group_by(mask) %>%
#   summarise(count = n())
# plot(active_map)
# extent(active_map) <- extent(pines_mask)
# origin(active_map) <- origin(pines_mask)
# tot_cells <- active$count

### GENERATE SPECIES GROUPS BRICKS
diversity_stack <- stack()
div_df <- data.frame()
for (i in 1:length(mgmt.scenarios)) {
  for (j in 1:length(times)) {
    temp_stack <- stack()
    for (h in 1:length(all_spp)) {
      temp <- raster(paste(di, mgmt.scenarios[i], "/output/agbiomass/", all_spp[h], "/AGBiomass", times[j], ".img", sep = ""))
      temp <- mask(temp, pines_mask) # Mask by active_map or pines_mask
      names(temp) <- all_spp[h]
      temp_stack <- stack(temp_stack, temp)
    }
    
    total <- calc(temp_stack, sum)
  
    diversity <- -sum(((temp_stack[[1]] / total) * (log(temp_stack[[1]] / total))),
                      ((temp_stack[[2]] / total) * (log(temp_stack[[2]] / total))),
                      ((temp_stack[[3]] / total) * (log(temp_stack[[3]] / total))),
                      ((temp_stack[[4]] / total) * (log(temp_stack[[4]] / total))),
                      ((temp_stack[[5]] / total) * (log(temp_stack[[5]] / total))),
                      ((temp_stack[[6]] / total) * (log(temp_stack[[6]] / total))),
                      ((temp_stack[[7]] / total) * (log(temp_stack[[7]] / total))),
                      ((temp_stack[[8]] / total) * (log(temp_stack[[8]] / total))),
                      ((temp_stack[[9]] / total) * (log(temp_stack[[9]] / total))),
                      ((temp_stack[[10]] / total) * (log(temp_stack[[10]] / total))),
                      ((temp_stack[[11]] / total) * (log(temp_stack[[11]] / total))),
                      ((temp_stack[[12]] / total) * (log(temp_stack[[12]] / total))),
                      ((temp_stack[[13]] / total) * (log(temp_stack[[13]] / total))), na.rm = TRUE)
      
    names(diversity) <- paste(strsplit(mgmt.scenarios[i], split = "_")[[1]][2], strsplit(mgmt.scenarios[i], split = "_")[[1]][3], times[j], sep = "_")
    diversity_stack <- stack(diversity_stack, diversity)
    raster_to_df <- data.frame(Cell = 1:length(pines_mask[]),
                               Scenario = mgmt.scenarios[i],
                               Harv_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][2],
                               Clim_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][3],
                               Time = times[j],
                               Mask = pines_mask[], # pines_mask or active_map     
                               Shannon_div = diversity[]) %>%
      filter(is.na(Mask) == FALSE)
    div_df <- rbind(div_df, raster_to_df)
  }
}

# Maps
diversity_stack <- mask(diversity_stack, pines_mask) # pines_mask or active_map

jpeg(file = paste(di, "outputs/210913_diversity_dense_pines_mask.jpeg", sep = ""), width = 18, height = 12, units="in", res=300)
gplot(diversity_stack) + 
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable, ncol = 4) + # 
  coord_equal() +
  theme_classic() +
  scale_fill_gradientn(colours = c("cyan", "black", "red"),
                       values = scales::rescale(c(0, 0.5, 1, 1.5, 2)), na.value = "transparent") +
  theme(legend.position = "bottom") +
  ggtitle("Shannon diversity index")
dev.off()

# Aggregated data
agg_diversity <- div_df %>%
  dplyr::select(-Mask, -Cell) %>%
  group_by(Scenario, Time, Harv_scenario, Clim_scenario) %>%
  summarise(Avg_Shannon = mean(Shannon_div))
  
# write.table(agg_diversity, paste(di, "outputs/210913_agg_diversity.csv", sep = ""))
# agg_diversity <- read.table(paste(di, "outputs/210913_agg_diversity.csv", sep = ""), header = TRUE)

jpeg(file = paste(di, "outputs/210913_agg_diversity_dense_pines_mask.jpeg", sep = ""), width=6, height=4, units="in", res=300)
ggplot(agg_diversity, aes(x = Time, y = Avg_Shannon)) +
  geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
  geom_point(aes(color = Harv_scenario)) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines)
dev.off()
  
  
  