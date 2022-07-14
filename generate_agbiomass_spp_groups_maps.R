# Generate AGB maps for species groups

library(raster)

mgmt.scenarios <- c("211129_proactive_rcp45_rep6",
                    "211129_proactive_rcp85_rep6") # Folder names with each replicate

### SETUP
di <- "/Volumes/GoogleDrive/My Drive/proj_LANDIS/experiments/" # Path to simulations folder

all_spp <-c("ppinaster", "pnigra", "phalepensis", "psylvestris", 
            "qilex", "qfaginea", "qpyrenaica", 
            "jcommunis", "joxycedrus", "tall", "medium", "short", 
            "popnigra")

times <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95)

# Generate maps of AGB by species groups: pines, oaks and shrubs (including Junipers)
for (i in seq_along(mgmt.scenarios)) {
  for (j in seq_along(times)) {
    temp_stack <- stack()
    for (h in seq_along(all_spp)) {
      temp <- raster(paste(di, mgmt.scenarios[i], "/output/agbiomass/", all_spp[h], "/AGBiomass", times[j], ".img", sep = ""))
      temp_stack <- stack(temp_stack, temp)
    }
    pines_agb <- calc(temp_stack[[1:4]], sum) # pines layers
    names(pines_agb) <- paste0("time_", times[j])
    
    oaks_agb <- calc(temp_stack[[5:7]], sum) # oaks layers
    names(oaks_agb) <- paste0("time_", times[j])
    
    shrubs_agb <- calc(temp_stack[[8:12]], sum) # shrubs and juniper layers, no Pop. nigra!
    names(shrubs_agb) <- paste0("time_", times[j])
    
    writeRaster(pines_agb, paste(di, mgmt.scenarios[i], "/results/Pines_AGB_", 
                                 times[j], ".asc", sep = ""), overwrite = TRUE)
    
    writeRaster(oaks_agb, paste(di, mgmt.scenarios[i], "/results/Oaks_AGB_", 
                                 times[j], ".asc", sep = ""), overwrite = TRUE)
    
    writeRaster(shrubs_agb, paste(di, mgmt.scenarios[i], "/results/Shrubs_AGB_", 
                                 times[j], ".asc", sep = ""), overwrite = TRUE)
  }
}
