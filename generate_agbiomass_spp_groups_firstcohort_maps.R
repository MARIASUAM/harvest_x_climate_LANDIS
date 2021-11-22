# Generate AGB maps for species groups

library(raster)

mgmt.scenarios <- c("210927_nomanag_current_rep2",
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

### SETUP
di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/"

oaks_and_pines <- c("ppinaster", "pnigra", "phalepensis", "psylvestris", 
                    "qilex", "qfaginea", "qpyrenaica")

times <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95)

# Generate maps of AGB of first cohort by species groups: pines and oaks
for (i in seq_along(mgmt.scenarios)) {
  for (j in seq_along(times)) {
    temp_stack <- stack()
    for (h in seq_along(oaks_and_pines)) {
      temp <- raster(paste(di, mgmt.scenarios[i], "/output/biomas-by-age/", oaks_and_pines[h], "-ageclass1-", times[j], ".img", sep = ""))
      temp_stack <- stack(temp_stack, temp)
    }
    pines_agb <- calc(temp_stack[[1:4]], sum) # pines layers
    names(pines_agb) <- paste0("time_", times[j])
    
    oaks_agb <- calc(temp_stack[[5:7]], sum) # oaks layers
    names(oaks_agb) <- paste0("time_", times[j])
    
    writeRaster(pines_agb, paste(di, mgmt.scenarios[i], "/results/Pines_first_cohort_AGB_", 
                                 times[j], ".asc", sep = ""), overwrite = TRUE)
    
    writeRaster(oaks_agb, paste(di, mgmt.scenarios[i], "/results/Oaks_first_cohort_AGB_", 
                                 times[j], ".asc", sep = ""), overwrite = TRUE)
  }
}
