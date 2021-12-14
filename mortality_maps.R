# Mortality based on age count and AGB of first cohort

mgmt.scenarios <- c(...) # Folder names with each replicate
 
### SETUP 
library(raster)

di <- ".../experiments/" # Path to simulations folder

spp <- c("ppinaster", "pnigra", "phalepensis", "psylvestris", "qilex", "qfaginea", "qpyrenaica", "jcommunis", "joxycedrus", "short", "medium", "tall", "popnigra") 

timesteps <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95)

### PINE PLANTATIONS MASK
pines_mask <- raster(paste(di, "data/dense_pines_mask.img", sep = ""))

# Matrix presence/absence
m_presence <- c(0, 0, 0,  
                0.05, Inf, 1)
rclmat_presence <- matrix(m_presence, ncol = 3, byrow = TRUE)

### Calculate dead cohorts per species
for (h in seq_along(mgmt.scenarios)) {
  for (k in seq_along(spp)) {
    for (j in 2:length(timesteps)) {
      # Calculate number of cohorts without first class (1-5) for time t1
      first_coh_t1 <- reclassify(raster(paste(di, mgmt.scenarios[h], "/output/biomas-by-age/", spp[k], "-ageclass1-", timesteps[j - 1], ".img", sep = "")), rclmat_presence, include.lowest = TRUE)
      nr_coh_t1 <- raster(paste(di, mgmt.scenarios[h], "/output/CohortsPerspecies/", spp[k], "/cohorts_", timesteps[j - 1], ".img", sep = ""))
      nr_coh_without_first_t1 <- nr_coh_t1 - first_coh_t1
      
      # Calculate number of cohorts without first class (1-5) for time t2
      first_coh_t2 <- reclassify(raster(paste(di, mgmt.scenarios[h], "/output/biomas-by-age/", spp[k], "-ageclass1-", timesteps[j], ".img", sep = "")), rclmat_presence, include.lowest = TRUE)
      nr_coh_t2 <- raster(paste(di, mgmt.scenarios[h], "/output/CohortsPerspecies/", spp[k], "/cohorts_", timesteps[j], ".img", sep = ""))
      nr_coh_without_first_t2 <- nr_coh_t2 - first_coh_t2
      
      # Calculate difference between t1 and t2 in number of cohorts without considering first class (1-10)
      diff_coh <- nr_coh_without_first_t2 - nr_coh_without_first_t1
    
      # Mask by pine plantations 
      diff_coh <- mask(diff_coh, pines_mask)
      
      # Export
      writeRaster(diff_coh, paste(di, mgmt.scenarios[h], "/results/", spp[k], "_diff_cohorts_", timesteps[j], ".asc", sep = ""), overwrite = TRUE)
    }
  }
}

