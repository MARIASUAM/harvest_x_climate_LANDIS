# Analysis of diversity

### NAME OF SCENARIOS ####
mgmt.scenarios <- c(...) # Folder names with each scenario

replicates <- c(1:5) 

### SETUP ###
di <- ".../experiments/" # Path to simulations folder
outputs_folder <- "..." # Subfolder for outputs

end_year <- 95
times <- c(0, end_year) # c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95)
all_spp <-c("ppinaster", "pnigra", "phalepensis", "psylvestris", "qilex", "qfaginea", "qpyrenaica", "jcommunis", "joxycedrus", "tall", "medium", "short", "popnigra")

library(raster)
library(dplyr)
library(ggplot2)

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

### GENERATE DIVERSITY TABLE
agg_diversity <- data.frame()
for (i in seq_along(mgmt.scenarios)) {
  for (j in seq_along(times)) {
    for (k in seq_along(replicates)){
      temp_stack <- stack()
      for (h in seq_along(all_spp)) {
        temp <- raster(paste(di, mgmt.scenarios[i], "_rep", replicates[k], "/output/agbiomass/", all_spp[h], "/AGBiomass", times[j], ".img", sep = ""))
        temp <- mask(temp, pines_mask) # Mask by active_map or pines_mask
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
        
      raster_to_df <- data.frame(Cell = 1:length(pines_mask[]),
                               Scenario = mgmt.scenarios[i],
                               Harv_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][2],
                               Clim_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][3],
                               Time = times[j],
                               Replicate = replicates[k],
                               Mask = pines_mask[], # pines_mask or active_map     
                               Shannon_div = diversity[]) %>%
        filter(is.na(Mask) == FALSE) %>%
        dplyr::select(-Mask, -Cell) %>%
        group_by(Scenario, Time, Harv_scenario, Clim_scenario, Replicate) %>%
        summarise(Avg_Shannon = mean(Shannon_div)) %>%
        group_by(Scenario, Time, Harv_scenario, Clim_scenario) %>%
        summarise(Avg_mean_Shannon = mean(Avg_Shannon),
                  SD_Shannon = sd(Avg_Shannon))
      agg_diversity <- rbind(agg_diversity, raster_to_df)
    }
  }
}
  
write.table(agg_diversity, paste(di, outputs_folder, "agg_diversity.csv", sep = ""))
# agg_diversity <- read.table(paste(di, outputs_folder, "agg_diversity.csv", sep = ""), header = TRUE)

jpeg(file = paste(di, outputs_folder, "diversity.jpeg", sep = ""), width=6, height=4, units="in", res=300)
ggplot(agg_diversity, aes(x = Time, y = Avg_mean_Shannon)) +
  geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
  geom_point(aes(color = Harv_scenario)) +
  geom_errorbar(aes(ymin = Avg_mean_Shannon - SD_Shannon, 
                    ymax = Avg_mean_Shannon + SD_Shannon), width=.2,
                position = position_dodge(.05)) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines)
dev.off()
  