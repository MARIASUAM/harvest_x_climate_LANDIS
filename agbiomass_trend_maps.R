# Kendall's test for AGB

library(raster)
library(tidyr)
library(dplyr)
library(Kendall)

mgmt.scenarios <- c(...) # Folder names with each scenario

replicates <- c(1:5)
times <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95) 

### SETUP
di <- ".../experiments/" # Path to simulations folder
outputs_folder <- "..." # Subfolder for outputs

### PINE PLANTATIONS MASK
pines_mask <- raster(paste(di, "data/dense_pines_mask.img", sep = ""))

# Create function for ManKendall test
fun_k <-function(x){return(unlist(MannKendall(x)))}

# Create stack and calculate trend in total AGB
for (i in seq_along(mgmt.scenarios)){
  agb_stack <- stack()
  for (j in seq_along(times)){
    temp <- raster(paste(di, outputs_folder, "AGB_maps/", mgmt.scenarios[i], "_AGB_avg_across_replicates_", times[j], ".asc", sep = ""))
    names(temp) <- paste0("time_", times[j])
    agb_stack <- stack(agb_stack, temp)
  }
  kendall_result <- calc(agb_stack, fun_k)
  names(kendall_result) <- c("tau", "p-value", "Kendal_Score", "denominator", "S_variance")
  kendall_result <- mask(kendall_result, pines_mask)

  writeRaster(kendall_result[[1]],
              paste(di, outputs_folder, "AGB_Kendall/", mgmt.scenarios[i], "_kendall_results_tau.asc", sep = ""), overwrite = TRUE)

  writeRaster(kendall_result[[2]],
              paste(di, outputs_folder, "AGB_Kendall/", mgmt.scenarios[i], "_kendall_results_pvalue.asc", sep = ""), overwrite = TRUE)
}

# Create stack and calculate trend in AGB by species groups (run)
for (i in seq_along(mgmt.scenarios)){
  agb_stack_oaks <- stack()
  agb_stack_pines <- stack()
  for (j in seq_along(times)){
    temp_oaks <- raster(paste(di, outputs_folder, "AGB_maps/", mgmt.scenarios[i], "_Oaks_AGB_avg_across_replicates_", times[j], ".asc", sep = ""))
    names(temp_oaks) <- paste0("time_", times[j])
    agb_stack_oaks <- stack(agb_stack_oaks, temp_oaks)

    temp_pines <- raster(paste(di, outputs_folder, "AGB_maps/", mgmt.scenarios[i], "_Pines_AGB_avg_across_replicates_", times[j], ".asc", sep = ""))
    names(temp_pines) <- paste0("time_", times[j])
    agb_stack_pines <- stack(agb_stack_pines, temp_pines)
  }

  kendall_result_oaks <- calc(agb_stack_oaks, fun_k)
  names(kendall_result_oaks) <- c("tau", "p-value", "Kendal_Score", "denominator", "S_variance")
  kendall_result_oaks <- mask(kendall_result_oaks, pines_mask)

  kendall_result_pines <- calc(agb_stack_pines, fun_k)
  names(kendall_result_pines) <- c("tau", "p-value", "Kendal_Score", "denominator", "S_variance")
  kendall_result_pines <- mask(kendall_result_pines, pines_mask)

  writeRaster(kendall_result_oaks[[1]],
              paste(di, outputs_folder, "AGB_Kendall/", mgmt.scenarios[i], "_Oaks_kendall_results_tau.asc", sep = ""), overwrite = TRUE)

  writeRaster(kendall_result_oaks[[2]],
              paste(di, outputs_folder, "AGB_Kendall/", mgmt.scenarios[i], "_Oaks_kendall_results_pvalue.asc", sep = ""), overwrite = TRUE)

  writeRaster(kendall_result_pines[[1]],
              paste(di, outputs_folder, "AGB_Kendall/", mgmt.scenarios[i], "_Pines_kendall_results_tau.asc", sep = ""), overwrite = TRUE)

  writeRaster(kendall_result_pines[[2]],
              paste(di, outputs_folder, "AGB_Kendall/", mgmt.scenarios[i], "_Pines_kendall_results_pvalue.asc", sep = ""), overwrite = TRUE)
}

# Create stack and calculate trend in AGB of first cohort by species groups
for (i in seq_along(mgmt.scenarios)){
  agb_stack_oaks <- stack()
  agb_stack_pines <- stack()
  for (j in seq_along(times)){
    temp_oaks <- raster(paste(di, outputs_folder, "AGB_maps/", mgmt.scenarios[i], "_Oaks_first_cohort_AGB_avg_across_replicates_", times[j], ".asc", sep = ""))
    names(temp_oaks) <- paste0("time_", times[j])
    agb_stack_oaks <- stack(agb_stack_oaks, temp_oaks)

    temp_pines <- raster(paste(di, outputs_folder, "AGB_maps/", mgmt.scenarios[i], "_Pines_first_cohort_AGB_avg_across_replicates_", times[j], ".asc", sep = ""))
    names(temp_pines) <- paste0("time_", times[j])
    agb_stack_pines <- stack(agb_stack_pines, temp_pines)
  }

  kendall_result_oaks <- calc(agb_stack_oaks, fun_k)
  names(kendall_result_oaks) <- c("tau", "p-value", "Kendal_Score", "denominator", "S_variance")
  kendall_result_oaks <- mask(kendall_result_oaks, pines_mask)

  kendall_result_pines <- calc(agb_stack_pines, fun_k)
  names(kendall_result_pines) <- c("tau", "p-value", "Kendal_Score", "denominator", "S_variance")
  kendall_result_pines <- mask(kendall_result_pines, pines_mask)

  writeRaster(kendall_result_oaks[[1]],
              paste(di, outputs_folder, "AGB_Kendall/", mgmt.scenarios[i], "_Oaks_first_cohort_kendall_results_tau.asc", sep = ""), overwrite = TRUE)

  writeRaster(kendall_result_oaks[[2]],
              paste(di, outputs_folder, "AGB_Kendall/", mgmt.scenarios[i], "_Oaks_first_cohort_kendall_results_pvalue.asc", sep = ""), overwrite = TRUE)

  writeRaster(kendall_result_pines[[1]],
              paste(di, outputs_folder, "AGB_Kendall/", mgmt.scenarios[i], "_Pines_first_cohort_kendall_results_tau.asc", sep = ""), overwrite = TRUE)

  writeRaster(kendall_result_pines[[2]],
              paste(di, outputs_folder, "AGB_Kendall/", mgmt.scenarios[i], "_Pines_first_cohort_kendall_results_pvalue.asc", sep = ""), overwrite = TRUE)
}




## Option A (wq package - not available for current R version)
# legend <- data.frame(name = c(paste("AGB.time_", times, sep = "")),
#                      Time_step = times)
# 
# agb_df <- data.frame(Cell = 1:length(agb_stack$time_0[]),
#                      AGB = agb_stack[]) %>%
#   gather(name, AGB, AGB.time_0:AGB.time_95, factor_key = TRUE) %>%
#   full_join(legend) %>%
#   dplyr::select(-name)
#   
# theil <- mannKen(as.ts(agb_df)) # Not working

## Option B (Kendall package)
# fun_k <-function(x){return(unlist(MannKendall(x)))}
# kendall_result <- calc(agb_stack, fun_k)
# 
# kendall_result <- mask(kendall_result, pines_mask)
# plot(kendall_result$layer.1)

## Option C (spatialEco package)
# library(spatialEco)
# trend_kendall <- raster.kendall(agb_stack, p.value = TRUE, 
#                                 z.value = TRUE, intercept = TRUE, 
#                                 confidence = TRUE, tau = TRUE)
# 
# trend_kendall <- mask(trend_kendall, pines_mask)
# 
# plot(trend_kendall$tau) # strange p-values, issue in github open
# trend_kendall 
