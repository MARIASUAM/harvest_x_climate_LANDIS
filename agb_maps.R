# AGB maps

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
                    "210927_proactiveplus_rcp85_rep1")

### SETUP
di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/"
outputs_folder <- "210927_outputs/"

library(raster)
library(rasterVis)
library(ggplot2)
all_spp <-c("ppinaster", "pnigra", "phalepensis", "psylvestris", "qilex", "qfaginea", "qpyrenaica", "jcommunis", "joxycedrus", "tall", "medium", "short", "popnigra")

### PINE PLANTATIONS MASKS
pines_mask <- raster(paste(di, "data/pines_mask.img", sep = ""))
dense_pines_mask <- raster(paste(di, "data/dense_pines_mask.img", sep = ""))

### Load AGB maps
reclass_df <- c(0, 0.5, -2,
                0.5, 0.95, -1, # decrease
                0.95, 1.05, 0, # equal
                1.05, 2.5, 1, # increase up x2.5
                2.5, 10, 2, # increase x2.5-x10
                10, Inf, 3) # increase x10-Inf

reclass_m <- matrix(reclass_df,
                    ncol = 3,
                    byrow = TRUE)

dif_stack <- stack()
for (i in 1:length(mgmt.scenarios)) {
  agb0 <- stack()
  agb95 <- stack()
  for (j in 1:length(all_spp)) {
  temp0 <- raster(paste(di, mgmt.scenarios[i], "/output/agbiomass/", all_spp[j], "/AGBiomass0.img", sep = ""))
  temp0 <- temp0 / 100 # g/m2 to tn/ha
  temp0 <- mask(temp0, dense_pines_mask) # Mask by pine plantations
  agb0 <- stack(agb0, temp0)
  
  temp95 <- raster(paste(di, mgmt.scenarios[i], "/output/agbiomass/", all_spp[j], "/AGBiomass95.img", sep = ""))
  temp95 <- temp95 / 100 # g/m2 to tn/ha
  temp95 <- mask(temp95, dense_pines_mask) # Mask by pine plantations
  agb95 <- stack(agb95, temp95)
  }
  total_0 <- calc(agb0, sum)
  total_95 <- calc(agb95, sum)
  
  dif <- (total_95 / total_0)
  # writeRaster(dif, filename = paste(di, outputs_folder, mgmt.scenarios[i], "_agb_difference.asc", sep = ""))
  dif_classified <- reclassify(dif, reclass_m)
  names(dif_classified) <- as.character(paste(strsplit(mgmt.scenarios[i], split = "_")[[1]][2],
                                              strsplit(mgmt.scenarios[i], split = "_")[[1]][3], sep = "_"))
    
  dif_stack <- stack(dif_stack, dif_classified)
}

gplot(dif_stack) + 
  geom_tile(aes(fill = factor(value))) +
  facet_wrap(~ variable, ncol = 4) +
  coord_equal() +
  theme_classic() +
  scale_fill_manual(values=c("#d73027", "#fc8d59", "#fee090", "#e0f3f8", "#91bfdb", "#4575b4"),
                    name="",
                    breaks=c(-2, -1, 0, 1, 2, 3),
                    labels=c("0-0.5", "0.5-0.95", "0.95-1.05", "1.05-2.5", "2.5-10", "10-Inf")) +
  theme(legend.position = "bottom") +
  ggtitle("AGB difference")



  