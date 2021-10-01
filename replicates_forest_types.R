# Analysis of forest types based on AGBiomass

VAR <- "agbiomass"

### NAME OF SCENARIOS ####
mgmt.scenarios <- c("210927_nomanag_current_rep1",
                    "210927_conserv_current_rep1",
                    "210927_proactive_current_rep1",
                    "210927_proactiveplus_current_rep1",
                    "210927_nomanag_rcp45_rep1",
                    "210927_conserv_rcp45_rep1",
                    "210927_proactive_rcp45_rep1",
                    "210927_proactiveplus_rcp45_rep1",
                    "210927_nomanag_rcp85_rep1",
                    "210927_conserv_rcp85_rep1",
                    "210927_proactive_rcp85_rep1",
                    "210927_proactiveplus_rcp85_rep1",
                    
                    "210927_nomanag_current_rep2",
                    "210927_conserv_current_rep2",
                    "210927_proactive_current_rep2",
                    "210927_proactiveplus_current_rep2",
                    "210927_nomanag_rcp45_rep2",
                    "210927_conserv_rcp45_rep2",
                    "210927_proactive_rcp45_rep2",
                    "210927_proactiveplus_rcp45_rep2",
                    "210927_nomanag_rcp85_rep2",
                    "210927_conserv_rcp85_rep2",
                    "210927_proactive_rcp85_rep2",
                    "210927_proactiveplus_rcp85_rep2")
                    
                    # "210927_nomanag_current_rep3",
                    # "210927_conserv_current_rep3",
                    # "210927_proactive_current_rep3",
                    # "210927_proactiveplus_current_rep3",
                    # "210927_nomanag_rcp45_rep3",
                    # "210927_conserv_rcp45_rep3",
                    # "210927_proactive_rcp45_rep3",
                    # "210927_proactiveplus_rcp45_rep3",
                    # "210927_nomanag_rcp85_rep3",
                    # "210927_conserv_rcp85_rep3",
                    # "210927_proactive_rcp85_rep3",
                    # "210927_proactiveplus_rcp85_rep3",
                    # 
                    # "210927_nomanag_current_rep4",
                    # "210927_conserv_current_rep4",
                    # "210927_proactive_current_rep4",
                    # "210927_proactiveplus_current_rep4",
                    # "210927_nomanag_rcp45_rep4",
                    # "210927_conserv_rcp45_rep4",
                    # "210927_proactive_rcp45_rep4",
                    # "210927_proactiveplus_rcp45_rep4",
                    # "210927_nomanag_rcp85_rep4",
                    # "210927_conserv_rcp85_rep4",
                    # "210927_proactive_rcp85_rep4",
                    # "210927_proactiveplus_rcp85_rep4",
                    # 
                    # "210927_nomanag_current_rep5",
                    # "210927_conserv_current_rep5",
                    # "210927_proactive_current_rep5",
                    # "210927_proactiveplus_current_rep5",
                    # "210927_nomanag_rcp45_rep5",
                    # "210927_conserv_rcp45_rep5",
                    # "210927_proactive_rcp45_rep5",
                    # "210927_proactiveplus_rcp45_rep5",
                    # "210927_nomanag_rcp85_rep5",
                    # "210927_conserv_rcp85_rep5",
                    # "210927_proactive_rcp85_rep5",
                    # "210927_proactiveplus_rcp85_rep5")

### SETUP ###
di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/"
outputs_folder <- "210927_outputs/"

end_year <- 95
times <- c(0, end_year)
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

### GENERATE SPECIES GROUPS BRICKS
ft_df <- data.frame()
for (i in 1:length(mgmt.scenarios)) {
  for (j in 1:length(times)) {
    temp_stack <- stack()
    for (h in 1:length(all_spp)) {
      temp <- raster(paste(di, mgmt.scenarios[i], "/output/", VAR, "/", all_spp[h], "/AGBiomass", times[j], ".img", sep = ""))
      temp <- mask(temp, pines_mask) # Mask by active_map or pines_mask
      names(temp) <- all_spp[h]
      temp_stack <- stack(temp_stack, temp)
    }
    
    pines_brick <- calc(temp_stack[[1:4]], sum) # pines layers
    oaks_brick <- calc(temp_stack[[5:7]], sum) # oaks layers
    shrubs_brick <- calc(temp_stack[[8:12]], sum) # shrubs and juniper layers, no Pop. nigra!
    total <- calc(temp_stack, sum)
    
    prop_pines <- pines_brick / total
    prop_oaks <- oaks_brick / total
    prop_shrubs <- shrubs_brick / total

    raster_to_df <- data.frame(Cell = 1:length(pines_mask[]),
                               Scenario = paste(strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][2], strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][3], sep = "_"),
                               Harv_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][2],
                               Clim_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][3],
                               Replicate = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][4],
                               Time = times[j],
                               Mask = pines_mask[],
                               Total_gm2 = total[],
                               Prop_pines = prop_pines[],
                               Prop_oaks = prop_oaks[],
                               Prop_shrubs = prop_shrubs[]) %>%
      filter(is.na(Mask) == FALSE)

    ft_df <- rbind(ft_df, raster_to_df)
  }
}

# Reclassify forest types
ft_classification <- ft_df %>%
  mutate(Forest_type = ifelse(Total_gm2 == 0, "Empty", # Empty cells
                        ifelse(Prop_pines >= 0.9, "Pure pines", # Mainly pines
                         ifelse(Prop_oaks >= 0.9, "Pure oaks", # Mainly oaks
                          ifelse(Prop_shrubs >= 0.9, "Shrublands", 
                           ifelse(Prop_pines >= 0.51, "Pine-dominated",
                            ifelse(Prop_oaks >= 0.51, "Oak-dominated",
                             ifelse(Prop_shrubs >= 0.51, "Shrub-dominated",
                              "Mixed"))))))))

legend <- data.frame(Forest_type = c("Empty", "Pine forests", "Oak forests", "Shrublands", "Pine-dominated", "Oak-dominated", "Shrub-dominated", "Mixed"),
                     Forest_type_code = c(1:8))
  
ft_classification <- ft_classification %>%
  left_join(legend)

# Percentage of the landscape
perc_ft <- ft_classification %>%
  dplyr::select(Scenario, Harv_scenario, Clim_scenario, Replicate, Time, Forest_type) %>%
  group_by(Scenario, Harv_scenario, Clim_scenario, Replicate, Time, Forest_type) %>%
  summarise(nr_cells = n()) %>%
  mutate(percentage = (nr_cells * 100) / tot_cells) %>%
  dplyr::select(Scenario, Harv_scenario, Clim_scenario, Replicate, Time,
         Forest_type, percentage)

write.table(...)

# Percentage change
perc_ft_time0 <- perc_ft %>% 
  filter(Time == 0) %>%
  mutate(Perc_time0 = percentage) %>%
  dplyr::select(-Time, -percentage) 
perc_ft_time0 <- perc_ft_time0[,c(-1)]

change_perc_ft <- perc_ft %>% 
  mutate(Perc_time95 = percentage) %>%
  dplyr::select(-percentage) %>%
  filter(Time != 0) %>%
  left_join(perc_ft_time0) %>%
  mutate(Perc_time0 = ifelse(is.na(Perc_time0) == FALSE, Perc_time0, 0),
         Perc_change = Perc_time95 - Perc_time0)
  
change_perc_ft$Harv_scenario <- as.factor(change_perc_ft$Harv_scenario)
levels(change_perc_ft$Harv_scenario)<- c("nomanag", "conserv", "proactive", "proactiveplus")

# Mean accross replicates
mean_accross_rep <- change_perc_ft %>%
  dplyr::select(-Replicate) %>%
  group_by(Scenario, Harv_scenario, Clim_scenario, Time, Forest_type) %>%
  summarise(Mean_percentage_change = mean(Perc_change),
            SD_percentage_change = sd(Perc_change),
            Mean_percentage_0 = mean(Perc_time0),
            SD_percentage_0 = sd(Perc_time0),
            Mean_percentage_95 = mean(Perc_time95),
            SD_percentage_95 = sd(Perc_time95))

# jpeg(file = paste(di, outputs_folder, "forest_types_pines_mask.jpeg", sep = ""), width = 18, height = 12, units="in", res=300)
mean_accross_rep %>%
  ggplot(aes(x = Forest_type, y = Mean_percentage_change)) +
  geom_bar(stat = "identity", position="dodge", aes(colour = Harv_scenario, fill = Harv_scenario, alpha = Clim_scenario)) +
  geom_errorbar(aes(ymin = Mean_percentage_change - SD_percentage_change, 
                    ymax = Mean_percentage_change + SD_percentage_change),
                    # group = Scenario),
                width= .1, position = position_dodge(.9)) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_colour_manual(values = cols) +
  scale_fill_manual(values = cols) +
  ylab("Percentage of change among pines plantations (%)") +
  xlab(NULL)
# dev.off()



