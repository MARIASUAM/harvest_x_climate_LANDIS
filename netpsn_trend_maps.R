# Kendall's test for NetPsn trend

library(raster)
library(tidyr)
library(dplyr)
library(Kendall)

mgmt.scenarios <- c("210927_conserv_current",
                    "210927_conserv_rcp45",
                    "210927_conserv_rcp85",
                    "210927_nomanag_current",
                    "210927_nomanag_rcp45",
                    "210927_nomanag_rcp85",
                    "210927_proactive_current",
                    "210927_proactive_rcp45",
                    "210927_proactive_rcp85",
                    "210927_proactiveplus_current",
                    "210927_proactiveplus_rcp45",
                    "210927_proactiveplus_rcp85")

replicates <- c(1:5)
times <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95)
months <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")

### SETUP
di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/"
outputs_folder <- "210927_outputs/"

### PINE PLANTATIONS MASK
pines_mask <- raster(paste(di, "data/dense_pines_mask.img", sep = ""))

# Create function for ManKendall test
fun_k <-function(x){return(unlist(SeasonalMannKendall(as.ts(x))))}

# Aggregate replicates
for (i in seq_along(mgmt.scenarios)){
  psn_stack <- stack()
  for (j in seq_along(times)){
    for (h in seq_along(months)) {
      temp <- raster(paste(di, outputs_folder, "NetPsn_maps/", mgmt.scenarios[i], "_NetPsn_avg_across_replicates_", times[j], months[h], ".asc", sep = ""))
      names(temp) <- paste0("time_", times[j], "month_", months[h])
      psn_stack <- stack(psn_stack, temp)
    }
  }
  kendall_result <- calc(psn_stack, fun_k)
  names(kendall_result) <- c("tau", "p-value", "Kendal_Score", "denominator", "S_variance")
  kendall_result <- mask(kendall_result, pines_mask)
  
  writeRaster(kendall_result[[1]], 
              paste(di, outputs_folder, "NetPsn_Kendall/", mgmt.scenarios[i], "_kendall_results_tau.asc", sep = ""))
  
  writeRaster(kendall_result[[2]], 
              paste(di, outputs_folder, "NetPsn_Kendall/", mgmt.scenarios[i], "_kendall_results_pvalue.asc", sep = ""))
}
