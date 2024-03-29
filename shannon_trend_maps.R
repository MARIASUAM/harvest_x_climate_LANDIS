# Kendall's test for Shannon index

library(raster)
library(tidyr)
library(dplyr)
library(Kendall)

mgmt.scenarios <- c(...) # Folder names with each replicate (one replicate set)

times <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95) 

### SETUP
di <- ".../experiments/" # Path to simulations folder
outputs_folder <- "..." # Subfolder for outputs

### PINE PLANTATIONS MASK
pines_mask <- raster(paste(di, "data/dense_pines_mask.img", sep = ""))

# Create function for ManKendall test
fun_k <-function(x){return(unlist(MannKendall(x)))}

# Create stack and calculate trend in Shannon index
for (i in seq_along(mgmt.scenarios)){
  shannon_stack <- stack()
  for (j in seq_along(times)){
    # temp <- raster(paste(di, outputs_folder, "Shannon_maps/", mgmt.scenarios[i], "_Shannon_avg_across_replicates_", times[j], ".asc", sep = ""))
    temp <- raster(paste(di, mgmt.scenarios[i], "/results/Shannon_", times[j], ".asc", sep = ""))
    names(temp) <- paste0("time_", times[j])
    shannon_stack <- stack(shannon_stack, temp)
  }
  kendall_result <- calc(shannon_stack, fun_k)
  names(kendall_result) <- c("tau", "p-value", "Kendal_Score", "denominator", "S_variance")
  kendall_result <- mask(kendall_result, pines_mask)
  
  writeRaster(kendall_result[[1]], 
              paste(di, outputs_folder, "Shannon_Kendall/", mgmt.scenarios[i], "_kendall_results_tau.asc", sep = ""), overwrite = TRUE)
  
  writeRaster(kendall_result[[2]], 
              paste(di, outputs_folder, "Shannon_Kendall/", mgmt.scenarios[i], "_kendall_results_pvalue.asc", sep = ""), overwrite = TRUE)
}
