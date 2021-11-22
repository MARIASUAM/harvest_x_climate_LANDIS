# Analysis of forest types based on AGBiomass

VAR <- "agbiomass"

### NAME OF SCENARIOS ####
mgmt.scenarios <- c("210927_conserv_current_rep1",
                    "210927_conserv_rcp45_rep1",
                    "210927_conserv_rcp85_rep1",
                    "210927_nomanag_current_rep1",
                    "210927_nomanag_rcp45_rep1",
                    "210927_nomanag_rcp85_rep1",
                    "210927_proactive_current_rep1",
                    "210927_proactive_rcp45_rep1",
                    "210927_proactive_rcp85_rep1",
                    "210927_proactiveplus_current_rep1",
                    "210927_proactiveplus_rcp45_rep1",
                    "210927_proactiveplus_rcp85_rep1",
                    "210930_conservplus_current_rep1",
                    "210930_conservplus_rcp45_rep1",
                    "210930_conservplus_rcp85_rep1")

### SETUP ###
di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/"
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

# Full active landscape
active_map <- raster(paste(di, "0. sim_root/inputs_basic/ecoregions.tif", sep =""))
active <- data.frame(mask = active_map[]) %>%
  filter(is.na(mask) == FALSE) %>%
  mutate(mask = ifelse(is.na(mask) == FALSE, 1)) %>%
  group_by(mask) %>%
  summarise(count = n())
plot(active_map)

extent(active_map) <- extent(pines_mask)
origin(active_map) <- origin(pines_mask)
# writeRaster(active_map, paste(di, "outputs/active_map", ".asc", sep = ""))

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
    other_brick <- calc(temp_stack[[8:12]], sum) # shrubs and juniper layers, no Pop. nigra!
    total <- calc(temp_stack, sum)
    
    prop_pines <- pines_brick / total
    prop_oaks <- oaks_brick / total
    prop_other <- other_brick / total

    raster_to_df <- data.frame(Cell = 1:length(pines_mask[]),
                               Scenario = mgmt.scenarios[i],
                               Time = times[j],
                               Mask = pines_mask[], # pines_mask[] or active_map[]
                               Total_gm2 = total[],
                               Prop_pines = prop_pines[],
                               Prop_oaks = prop_oaks[],
                               Prop_other = prop_other[]) %>%
      filter(is.na(Mask) == FALSE)

    ft_df <- rbind(ft_df, raster_to_df)
  }
}

# Check
# ft_check <- ft_df %>%
#   mutate(Total_prop = Prop_pines + Prop_oaks + Prop_other) %>%
#   filter(Total_prop < 0.999999990)

# Reclassify forest types
# ft_full_classification <- ft_df %>%
#   mutate(Forest_type = ifelse(Total_gm2 == 0, "empty", # Empty cells
#                         ifelse(Prop_pines >= 0.8, "mainly_pines", # Mainly pines
#                          ifelse(Prop_oaks >= 0.8, "mainly_oaks", # Mainly oaks
#                           ifelse(Prop_other >= 0.8, "mainly_other", 
#                            ifelse(Prop_pines >= 0.5 & Prop_oaks >= 0.3, "pines_dominating_oaks",
#                             ifelse(Prop_oaks >= 0.5 & Prop_pines >= 0.3, "oaks_dominating_pines",
#                              ifelse(Prop_shrubs >= 0.5 & Prop_oaks >= 0.3, "shrubs_dominating_oaks",
#                               ifelse(Prop_shrubs >= 0.5 & Prop_pines >= 0.3, "shrubs_dominating_pines",
#                                ifelse(Prop_pines >= 0.5, "pines_over_rest",
#                                 ifelse(Prop_oaks >= 0.5, "oaks_over_rest",
#                                   ifelse(Prop_shrubs >= 0.5, "shrubs_over_rest",
#                                    "mixed"))))))))))))

ft_reduced_classification <- ft_df %>%
  mutate(Forest_type = ifelse(Total_gm2 == 0, "Empty", # Empty cells
                        ifelse(Prop_pines >= 0.9, "Pine forests", # Mainly pines
                         ifelse(Prop_oaks >= 0.9, "Oak forests", # Mainly oaks
                          ifelse(Prop_other >= 0.9, "Shrublands", 
                           ifelse(Prop_pines >= 0.51, "Pine-dominated",
                            ifelse(Prop_oaks >= 0.51, "Oak-dominated",
                             ifelse(Prop_other >= 0.51, "Shrub-dominated",
                              "Mixed"))))))))

legend <- data.frame(Forest_type = c("Empty", "Pine forests", "Oak forests", "Shrublands", "Pine-dominated", "Oak-dominated", "Shrub-dominated", "Mixed"),
                     Forest_type_code = c(1:8))
  
ft_reduced_classification <- ft_reduced_classification %>%
  left_join(legend)

# check <- ft_reduced_classification %>%
#   select(Forest_type) %>%
#   group_by(Forest_type) %>%
#   summarise(nr = n())

# Percentage of the landscape
perc_ft <- ft_reduced_classification %>%
  dplyr::select(Scenario, Time, Forest_type) %>%
  group_by(Scenario, Time, Forest_type) %>%
  summarise(nr_cells = n()) %>%
  mutate(percentage = (nr_cells * 100) / tot_cells,
         Harv_scenario = strsplit(as.character(Scenario), split = "_")[[1]][2],
         Clim_scenario = strsplit(as.character(Scenario), split = "_")[[1]][3]) %>%
  dplyr::select(Scenario, Harv_scenario, Clim_scenario, Time,
         Forest_type, percentage)

# perc_ft %>%
#   ggplot(aes(x = Forest_type, y = percentage, group = Harv_scenario)) +
#   geom_bar(stat = "identity", position="dodge", aes(colour = Harv_scenario, fill = Harv_scenario)) +
#   facet_grid(Time ~ Clim_scenario) +
#   theme_classic() +
#   theme(legend.position = "bottom") +
#   scale_colour_manual(values = cols) +
#   scale_fill_manual(values = cols) +
#   # scale_x_discrete(labels) +
#   ylim(0,100)

# Percentage change
perc_ft_time0 <- perc_ft %>% 
  filter(Time == 0) %>%
  mutate(Perc_time0 = percentage) %>%
  dplyr::select(-Time, -percentage) 
perc_ft_time0 <- perc_ft_time0[,c(-1)]

change_perc_ft <- perc_ft %>% 
  filter(Time != 0) %>%
  left_join(perc_ft_time0) %>%
  mutate(Perc_time0 = ifelse(is.na(Perc_time0) == FALSE, Perc_time0, 0),
         Perc_change = percentage - Perc_time0)
  
change_perc_ft$Harv_scenario <- as.factor(change_perc_ft$Harv_scenario)
levels(change_perc_ft$Harv_scenario)<- c("nomanag", "conserv", "proactive", "proactiveplus")

# jpeg(file = paste(di, "210927_outputs/forest_types_conservplus.jpeg", sep = ""), width = 18, height = 12, units="in", res=300)
change_perc_ft %>%
  ggplot(aes(x = Forest_type, y = Perc_change)) +
  geom_bar(stat = "identity", position="dodge", aes(colour = Harv_scenario, fill = Harv_scenario, alpha = Clim_scenario)) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_colour_manual(values = cols) +
  scale_fill_manual(values = cols) +
  ylab("Percentage of change among pines plantations (%)") +
  xlab(NULL)
# dev.off()

# Plot as maps
all_cells <- data.frame(Cell = 1:length(pines_mask[]))

# time 0 
subsample <- ft_reduced_classification %>%
  filter(Scenario == "210913_nomanag_current_MIROC5",
         Time == 0) %>%
  full_join(all_cells) %>%
  arrange(Cell)
time_0_raster <- raster(nrow = nrow(pines_mask), ncol = ncol(pines_mask), 
                     extent(pines_mask), vals = NA)
values(time_0_raster) <- subsample$Forest_type_code
names(time_0_raster) <- "nomanag_current_time_0"

# End times
mgmt_stack <- stack()
for (i in 1:length(mgmt.scenarios)) {
  for (j in 1:length(end_year)) {
    subsample <- ft_reduced_classification %>%
      filter(Scenario == mgmt.scenarios[i],
             Time == end_year) %>%
      full_join(all_cells) %>%
      arrange(Cell)
    
    one_raster <- raster(nrow = nrow(pines_mask), 
                         ncol = ncol(pines_mask), 
                         extent(pines_mask), vals = NA)
    values(one_raster) <- subsample$Forest_type_code
    names(one_raster) <- paste(strsplit(mgmt.scenarios[i], split = "_")[[1]][4],
                               strsplit(mgmt.scenarios[i], split = "_")[[1]][5], sep = "_")
    mgmt_stack <- stack(mgmt_stack, one_raster)
  }
}

jpeg(file = paste(di, "outputs/210913_forest_types_maps_t0_dense_pines_mask.jpeg", sep = ""), width = 18, height = 12, units="in", res=300)
gplot(time_0_raster) + 
  geom_tile(aes(fill = as.factor(value))) +
  coord_equal() +
  theme_classic() +
  scale_fill_manual(values = c("white", "#ed4532",      "#83b761",     "#dfd916",    "#ed4532AA",      "#83b761AA",     "#dfd916AA",       "#43cebe"),
                    labels = c("Empty", "Pine forests", "Oak forests", "Shrublands", "Pine-dominated", "Oak-dominated", "Shrub-dominated", "Mixed")) +
  guides(fill = guide_legend(title = NULL)) +
  theme(legend.position = "none") +
  ggtitle("Time 0")
dev.off()

jpeg(file = paste(di, "outputs/210913_forest_types_maps_t95_dense_pines_mask.jpeg", sep = ""), width = 18, height = 12, units="in", res=300)
gplot(mgmt_stack) + 
  geom_tile(aes(fill = as.factor(value))) +
  facet_wrap(~ variable, ncol = 4) +
  coord_equal() +
  theme_classic() +
  scale_fill_manual(values = c("white", "#ed4532",      "#83b761",     "#dfd916",    "#ed4532AA",      "#83b761AA",     "#dfd916AA",       "#43cebe"),
                    labels = c("Empty", "Pine forests", "Oak forests", "Shrublands", "Pine-dominated", "Oak-dominated", "Shrub-dominated", "Mixed")) +
  guides(fill = guide_legend(title = NULL)) +
  theme(legend.position = "bottom") +
  ggtitle("Time 95")
dev.off()

# Inspect empty cells
empty_cells <- ft_df %>%
  filter(Total_gm2 == 0)

all_cells <- data.frame(Cell = 1:length(pines_mask[]))

for (i in 1:length(mgmt.scenarios)) {
  mgmt_stack <- stack()
  for (j in 1:length(times)) {
    subsample <- empty_cells %>%
      filter(Scenario == mgmt.scenarios[i],
             Time == times[j]) %>%
      full_join(all_cells) %>%
      arrange(Cell)
    
    one_raster <- raster(nrow = nrow(pines_mask), 
                         ncol = ncol(pines_mask), 
                         extent(pines_mask), vals = NA)
    values(one_raster) <- subsample$Total_gm2
    names(one_raster) <- paste("empty_cells", mgmt.scenarios[i], "time", times[j], sep = "_")
    # writeRaster(one_raster, paste(di, "outputs/empty_cells", mgmt.scenarios[i], "_time_", times[j], ".asc", sep = ""))
  }
}
