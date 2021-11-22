# Generate average maps across replicates for different variables

library(raster)
library(tidyr)
library(dplyr)

# Define scenarios folders and number of replicates
mgmt.scenarios <- c("210927_nomanag_current",
                    "210927_nomanag_rcp45",
                    "210927_nomanag_rcp85",
                    "210927_conserv_current",
                    "210927_conserv_rcp45",
                    "210927_conserv_rcp85",
                    "210927_proactive_current",
                    "210927_proactive_rcp45",
                    "210927_proactive_rcp85",
                    "210927_proactiveplus_current",
                    "210927_proactiveplus_rcp45",
                    "210927_proactiveplus_rcp85")

replicates <- c(1:5)

### SETUP
di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/"
outputs_folder <- "210927_outputs/"

times <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95)
months <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")

# Total AGB (run)
# for (i in seq_along(mgmt.scenarios)){
#   for (j in seq_along(times)){
#     replicate_stack <- stack()
#     for (h in seq_along(replicates)){
#       temp <- raster(paste(di, mgmt.scenarios[i], "_rep", h, "/results/Total_AGB_", times[j], ".asc", sep = ""))
#       replicate_stack <- stack(replicate_stack, temp)
#     }
#     avg_among_replicates <- calc(replicate_stack, mean)
#     names(avg_among_replicates) <- paste("time", times[j], sep = "_")
#     writeRaster(avg_among_replicates, 
#                 paste(di, outputs_folder, "AGB_maps/", mgmt.scenarios[i], "_AGB_avg_across_replicates_", times[j], ".asc", sep = ""))
#   }
# }

# Psn (run)
# for (i in seq_along(mgmt.scenarios)){
#   for (j in seq_along(times)){
#     for (k in seq_along(months)){
#       replicate_stack <- stack()
#       for (h in seq_along(replicates)){
#         temp <- raster(paste(di, mgmt.scenarios[i], "_rep", h, "/output/MonthlyNetPsn/MonthlyNetPsn_", times[j], months[k], ".img", sep = ""))
#         replicate_stack <- stack(replicate_stack, temp)
#       }
#       avg_among_replicates <- calc(replicate_stack, mean)
#       names(avg_among_replicates) <- paste("time", times[j], sep = "_")
#       writeRaster(avg_among_replicates,
#                   paste(di, outputs_folder, "NetPsn_maps/", mgmt.scenarios[i], "_NetPsn_avg_across_replicates_", times[j], months[k], ".asc", sep = ""))
#     }
#   }
# }

# AGB by spp groups
# for (i in seq_along(mgmt.scenarios)){
#   for (j in seq_along(times)){
#     replicate_stack_pines <- stack()
#     replicate_stack_oaks <- stack()
#     # replicate_stack_shrubs <- stack()
#     for (h in seq_along(replicates)){
#       temp_pines <- raster(paste(di, mgmt.scenarios[i], "_rep", h, "/results/Pines_AGB_", times[j], ".asc", sep = ""))
#       replicate_stack_pines <- stack(replicate_stack_pines, temp_pines)
# 
#       temp_oaks <- raster(paste(di, mgmt.scenarios[i], "_rep", h, "/results/Oaks_AGB_", times[j], ".asc", sep = ""))
#       replicate_stack_oaks <- stack(replicate_stack_oaks, temp_oaks)
# 
#       # temp_shrubs <- raster(paste(di, mgmt.scenarios[i], "_rep", h, "/results/Shrubs_AGB_", times[j], ".asc", sep = ""))
#       # replicate_stack_shrubs <- stack(replicate_stack_shrubs, temp_shrubs)
#     }
#     avg_among_replicates_pines <- calc(replicate_stack_pines, mean)
#     avg_among_replicates_oaks <- calc(replicate_stack_oaks, mean)
#     # avg_among_replicates_shrubs <- calc(replicate_stack_shrubs, mean)
# 
#     names(avg_among_replicates_pines) <- paste("time", times[j], sep = "_")
#     names(avg_among_replicates_oaks) <- paste("time", times[j], sep = "_")
#     # names(avg_among_replicates_shrubs) <- paste("time", times[j], sep = "_")
# 
#     writeRaster(avg_among_replicates_pines,
#                 paste(di, outputs_folder, "AGB_maps/", mgmt.scenarios[i], "_Pines_AGB_avg_across_replicates_", times[j], ".asc", sep = ""), overwrite = TRUE)
# 
#     writeRaster(avg_among_replicates_oaks,
#                 paste(di, outputs_folder, "AGB_maps/", mgmt.scenarios[i], "_Oaks_AGB_avg_across_replicates_", times[j], ".asc", sep = ""), overwrite = TRUE)
# 
#     # writeRaster(avg_among_replicates_shrubs,
#                 # paste(di, outputs_folder, mgmt.scenarios[i], "Shrubs_AGB_avg_across_replicates_", times[j], ".asc", sep = ""), overwrite = TRUE)
#   }
# }

# AGB of first cohorts by spp groups
for (i in seq_along(mgmt.scenarios)){
  for (j in seq_along(times)){
    replicate_stack_pines <- stack()
    replicate_stack_oaks <- stack()
    for (h in seq_along(replicates)){
      temp_pines <- raster(paste(di, mgmt.scenarios[i], "_rep", h, "/results/Pines_first_cohort_AGB_", times[j], ".asc", sep = ""))
      replicate_stack_pines <- stack(replicate_stack_pines, temp_pines)
      
      temp_oaks <- raster(paste(di, mgmt.scenarios[i], "_rep", h, "/results/Oaks_first_cohort_AGB_", times[j], ".asc", sep = ""))
      replicate_stack_oaks <- stack(replicate_stack_oaks, temp_oaks)
    }
    avg_among_replicates_pines <- calc(replicate_stack_pines, mean)
    avg_among_replicates_oaks <- calc(replicate_stack_oaks, mean)

    names(avg_among_replicates_pines) <- paste("time", times[j], sep = "_")
    names(avg_among_replicates_oaks) <- paste("time", times[j], sep = "_")

    writeRaster(avg_among_replicates_pines,
                paste(di, outputs_folder, "AGB_maps/", mgmt.scenarios[i], "_Pines_first_cohort_AGB_avg_across_replicates_", times[j], ".asc", sep = ""), overwrite = TRUE)
    
    writeRaster(avg_among_replicates_oaks,
                paste(di, outputs_folder, "AGB_maps/", mgmt.scenarios[i], "_Oaks_first_cohort_AGB_avg_across_replicates_", times[j], ".asc", sep = ""), overwrite = TRUE)
  }
}