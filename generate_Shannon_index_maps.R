# Generate Shannon index maps

mgmt.scenarios <- c(...) # Folder names with each replicate

### SETUP
di <- ".../experiments/" # Path to simulations folder

library(raster)
library(ggplot2)

all_spp <-c("ppinaster", "pnigra", "phalepensis", "psylvestris", "qilex", "qfaginea", "qpyrenaica", "jcommunis", "joxycedrus", "tall", "medium", "short", "popnigra")
times <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95)

### PINE PLANTATIONS MASK
pines_mask <- raster(paste(di, "data/dense_pines_mask.img", sep = ""))

### GENERATE DIVERSITY MAPS
for (i in seq_along(mgmt.scenarios)) {
  for (j in seq_along(times)) {
      temp_stack <- stack()
      for (h in seq_along(all_spp)) {
        temp <- raster(paste(di, mgmt.scenarios[i], "/output/agbiomass/", all_spp[h], "/AGBiomass", times[j], ".img", sep = ""))
        temp <- mask(temp, pines_mask)
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
      
      writeRaster(diversity, paste(di, mgmt.scenarios[i], "/results/Shannon_", times[j], ".asc", sep = ""))
  }
}

