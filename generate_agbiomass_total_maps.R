# Generate total AGB maps

mgmt.scenarios <- c("210927_conserv_rcp85_rep4",
                    "210927_conserv_current_rep5",
                    "210927_conserv_rcp45_rep5",
                    "210927_conserv_rcp85_rep5",
                    "210927_nomanag_current_rep5",
                    "210927_nomanag_rcp45_rep5", 
                    "210927_nomanag_rcp85_rep5",
                    "210927_proactive_current_rep5",
                    "210927_proactive_rcp45_rep5",
                    "210927_proactive_rcp85_rep4",
                    "210927_proactive_rcp85_rep5",
                    "210927_proactiveplus_current_rep5",
                    "210927_proactiveplus_rcp45_rep5",
                    "210927_proactiveplus_rcp85_rep4",
                    "210927_proactiveplus_rcp85_rep5")

### SETUP
di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/"

library(raster)
all_spp <-c("ppinaster", "pnigra", "phalepensis", "psylvestris", "qilex", "qfaginea", "qpyrenaica", "jcommunis", "joxycedrus", "tall", "medium", "short", "popnigra")

times <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95)

# Generate maps
for (i in 1:length(mgmt.scenarios)) {
  for (j in 1:length(times)) {
    temp_stack <- stack()
    for (h in 1:length(all_spp)) {
      temp <- raster(paste(di, mgmt.scenarios[i], "/output/agbiomass/", all_spp[h], "/AGBiomass", times[j], ".img", sep = ""))
      temp_stack <- stack(temp_stack, temp)
    }
    total_agb <- calc(temp_stack, sum)
    writeRaster(total_agb, paste(di, mgmt.scenarios[i], "/results/Total_AGB_", times[j], ".asc", sep = ""))
  }
}
