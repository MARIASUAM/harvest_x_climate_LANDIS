# Generate aggregated tables for pine mask

### SETUP
library(raster)
library(dplyr)

di <- ".../experiments/" # Path to simulations folder

all_spp <- c("ppinaster", "pnigra", "phalepensis", "psylvestris", "qilex", "qfaginea", "qpyrenaica", "jcommunis", "joxycedrus", "tall", "medium", "short", "popnigra")
pines_and_oaks <- c("ppinaster", "pnigra", "phalepensis", "psylvestris", "qilex", "qfaginea", "qpyrenaica")
non_pines_and_oaks <- c("jcommunis", "joxycedrus", "tall", "medium", "short", "popnigra")

times <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95)
dec_times <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90)
times_no0 <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95)
dec_times_no0 <- c(10, 20, 30, 40, 50, 60, 70, 80, 90)

months_ch <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")

# Management scenarios
mgmt.scenarios <- c(...) # Folder names with each replicate
                  
### PINE PLANTATIONS MASKS
dense_pines_mask <- raster(paste(di, "data/dense_pines_mask.img", sep = ""))

### ABOVEGROUND BIOMASS
for (i in 1:length(mgmt.scenarios)) {
  agb_dense_pines_df <- data.frame()
  for (j in 1:length(times)) {
    for (h in 1:length(all_spp)) {
      temp <- raster(paste(di, mgmt.scenarios[i], "/output/agbiomass/", all_spp[h], "/AGBiomass", times[j], ".img", sep = ""))
      temp <- temp / 100 # g/m2 to tn/ha
      temp_dense_pines_masked <- mask(temp, dense_pines_mask) # Mask by pine plantations

      agb_dense_pines_df <- rbind(agb_dense_pines_df,
                            data.frame(Scenario = mgmt.scenarios[i], Time = times[j], Species = all_spp[h],
                                       Avg_tnha = cellStats(temp_dense_pines_masked, mean, na.rm=TRUE),
                                       SD_tnha = cellStats(temp_dense_pines_masked, sd, na.rm=TRUE),
                                       Total_tn = cellStats(temp_dense_pines_masked, sum, na.rm=TRUE)))
    }
  }
  write.table(agb_dense_pines_df, paste(di, mgmt.scenarios[i], "/results/aggregated_agbiomass_dense_pines.txt", sep = ""))
}

# Monthly Net Photosynthesis
for (i in 1:length(mgmt.scenarios)) {
  net_psn_dense_pines_df <- data.frame()
  for (j in 1:length(times)) {
    for (h in 1:length(months_ch)){
      netpsn <- raster(paste(di, mgmt.scenarios[i], "/output/MonthlyNetPsn/MonthlyNetPsn_", times[j], months_ch[h], ".img", sep = ""))
      netpsn_dense_pines <- mask(netpsn, dense_pines_mask)
      net_psn_dense_pines_df <- rbind(net_psn_dense_pines_df,
                                      data.frame(Scenario = mgmt.scenarios[i], Time = times[j], Month = months_ch[h],
                                                 Avg_Net_Psn = cellStats(netpsn_dense_pines, mean, na.rm=TRUE),
                                                 SD_Net_Psn = cellStats(netpsn_dense_pines, sd, na.rm=TRUE)))
    }
  }
  write.table(net_psn_dense_pines_df, paste(di, mgmt.scenarios[i], "/results/aggregated_net_psn_dense_pines.txt", sep = ""))
}

### ABOVEGROUND BIOMASS BY AGE
# Oaks and pines
age_classes <- data.frame(code = c(1, 2), class = c("1-5", "6-10"))
for (i in 1:length(mgmt.scenarios)) {
  agb_by_age_dense_pines_df <- data.frame()
  for (j in 1:length(dec_times)) {
    for (h in 1:length(pines_and_oaks)) {
      for (k in 1:length(age_classes$code)) {
        temp <- raster(paste(di, mgmt.scenarios[i], "/output/biomas-by-age/", pines_and_oaks[h], "-ageclass", age_classes$code[k], "-", dec_times[j], ".img", sep = ""))
        temp <- temp / 100 # g/m2 to tn/ha
        temp_dense_pines_masked <- mask(temp, dense_pines_mask) # Mask by pine plantations
        agb_by_age_dense_pines_df <- rbind(agb_by_age_dense_pines_df,
                                      data.frame(Scenario = mgmt.scenarios[i], Time = dec_times[j],
                                                 Species = pines_and_oaks[h], Age_class = age_classes$class[k],
                                                 Avg_tnha = cellStats(temp_dense_pines_masked, mean, na.rm=TRUE),
                                                 SD_tnha = cellStats(temp_dense_pines_masked, sd, na.rm=TRUE),
                                                 Total_tn = cellStats(temp_dense_pines_masked, sum, na.rm=TRUE)))
      }
    }
  }
  write.table(agb_by_age_dense_pines_df, paste(di, mgmt.scenarios[i], "/results/aggregated_agbiomass_by_age_dense_pines_oaks_pines.txt", sep = ""))
}

# Rest of species
for (i in 1:length(mgmt.scenarios)) {
  agb_by_age_dense_pines_df <- data.frame()
  for (j in 1:length(dec_times)) {
    for (h in 1:length(non_pines_and_oaks)) {
        temp <- raster(paste(di, mgmt.scenarios[i], "/output/biomas-by-age/", non_pines_and_oaks[h], "-ageclass1", "-", dec_times[j], ".img", sep = ""))
        temp <- temp / 100 # g/m2 to tn/ha
        temp_dense_pines_masked <- mask(temp, dense_pines_mask) # Mask by pine plantations
        agb_by_age_dense_pines_df <- rbind(agb_by_age_dense_pines_df,
                                           data.frame(Scenario = mgmt.scenarios[i], Time = dec_times[j],
                                                      Species = non_pines_and_oaks[h], Age_class = "1-5",
                                                      Avg_tnha = cellStats(temp_dense_pines_masked, mean, na.rm=TRUE),
                                                      SD_tnha = cellStats(temp_dense_pines_masked, sd, na.rm=TRUE),
                                                      Total_tn = cellStats(temp_dense_pines_masked, sum, na.rm=TRUE)))
      }
  }
  write.table(agb_by_age_dense_pines_df, paste(di, mgmt.scenarios[i], "/results/aggregated_agbiomass_by_age_dense_pines.txt", sep = ""))
}

# Harvested biomass
for (i in 1:length(mgmt.scenarios)) {
  harv_biomass_dense_pines_df <- data.frame()
  for (j in 1:length(dec_times_no0)) {
    harv_biomass <- raster(paste(di, mgmt.scenarios[i], "/output/harvest/biomass-removed-", dec_times_no0[j], ".tif", sep = ""))
    harv_biomass_dense_pines <- mask(harv_biomass, dense_pines_mask)
    harv_biomass_dense_pines_df <- rbind(harv_biomass_dense_pines_df,
                                   data.frame(Scenario = mgmt.scenarios[i], Time = dec_times_no0[j],
                                              Avg_harv_biomass = cellStats(harv_biomass_dense_pines, mean, na.rm=TRUE),
                                              SD_harv_biomass = cellStats(harv_biomass_dense_pines, sd, na.rm=TRUE),
                                              Sum_harv_biomass = cellStats(harv_biomass_dense_pines, sum, na.rm=TRUE)))
  }
  write.table(harv_biomass_dense_pines_df, paste(di, mgmt.scenarios[i], "/results/aggregated_harvested_biomass_dense_pines.txt", sep = ""))
}

### MORTALITY
for (i in 1:length(mgmt.scenarios)) {
  mort_dense_pines_df <- data.frame()
  for (j in 1:length(times_no0)) {
    for (h in 1:length(all_spp)) {
      temp <- raster(paste(di, mgmt.scenarios[i], "/results/", all_spp[h], "_diff_cohorts_", times_no0[j], ".asc", sep = ""))
      temp_dense_pines_masked <- mask(temp, dense_pines_mask) # Mask by pine plantations
      temp_dense_pines_masked <- reclassify(temp_dense_pines_masked,
                                            cbind(0, Inf, 0), right=FALSE) # Reclassify by discarding new cohorts (positive values)

      temp_dense_pines_masked <- temp_dense_pines_masked * -1 # Absolute value to get dead cohorts as positive values
      mort_dense_pines_df <- rbind(mort_dense_pines_df,
                                  data.frame(Scenario = mgmt.scenarios[i], Time = times_no0[j], Species = all_spp[h],
                                             Total_dead_cohorts = cellStats(temp_dense_pines_masked, sum, na.rm=TRUE)))
    }
  }
  write.table(mort_dense_pines_df, paste(di, mgmt.scenarios[i], "/results/aggregated_diff_cohorts.txt", sep = ""))
}

##### Other outputs
### Cohort stats - General variables
# for (i in 1:length(mgmt.scenarios)) {
#   coh_var_dense_pines_df <- data.frame()
#   for (j in 1:length(times)) {
#     age_count <- raster(paste(di, mgmt.scenarios[i], "/output/cohort-stats/AGE-COUNT-", times[j], ".img", sep = ""))
#     age_rich <- raster(paste(di, mgmt.scenarios[i], "/output/cohort-stats/AGE-RICH-", times[j], ".img", sep = ""))
#     spp_rich <- raster(paste(di, mgmt.scenarios[i], "/output/cohort-stats/SPP-RICH-", times[j], ".img", sep = ""))
# 
#     age_count_dense_masked <- mask(age_count, dense_pines_mask)
#     age_rich_dense_masked <- mask(age_rich, dense_pines_mask)
#     spp_rich_dense_masked <- mask(spp_rich, dense_pines_mask)
# 
#     coh_var_dense_pines_df <- rbind(coh_var_dense_pines_df,
#                                     data.frame(Scenario = mgmt.scenarios[i], Time = times[j],
#                                                Avg_age_count = cellStats(age_count_dense_masked, mean, na.rm=TRUE),
#                                                Avg_age_rich = cellStats(age_rich_dense_masked, mean, na.rm=TRUE),
#                                                Avg_spp_rich = cellStats(spp_rich_dense_masked, mean, na.rm=TRUE)))
#     }
#   write.table(coh_var_dense_pines_df, paste(di, mgmt.scenarios[i], "/results/aggregated_coh_stats_dense_pines.txt", sep = ""))
# }
# 
# Cohort per species
# for (i in 1:length(mgmt.scenarios)) {
#   cohorts_dense_pines_df <- data.frame()
#   for (j in 1:length(times)) {
#     for (h in 1:length(all_spp)) {
#       temp <- raster(paste(di, mgmt.scenarios[i], "/output/CohortsPerspecies/", all_spp[h], "/cohorts_", times[j], ".img", sep = ""))
#       temp_dense_pines_masked <- mask(temp, dense_pines_mask) # Mask by pine plantations
#       cohorts_dense_pines_df <- rbind(cohorts_dense_pines_df,
#                                   data.frame(Scenario = mgmt.scenarios[i], Time = times[j], Species = all_spp[h],
#                                              Avg_cohorts = cellStats(temp_dense_pines_masked, mean, na.rm = TRUE),
#                                              Nr_cohorts = cellStats(temp_dense_pines_masked, sum, na.rm = TRUE)))
#     }
#   }
#   write.table(cohorts_dense_pines_df, paste(di, mgmt.scenarios[i], "/results/aggregated_cohorts_dense_pines.txt", sep = ""))
# }
# 
### Age Distribution
# for (i in 1:length(mgmt.scenarios)) {
#   agedist_pines_df <- data.frame()
#   agedist_dense_pines_df <- data.frame()
#   for (j in 1:length(times)) {
#       temp <- raster(paste(di, mgmt.scenarios[i], "/output/AgeDist/Age_", times[j], ".img", sep = ""))
#       temp_dense_pines_masked <- mask(temp, dense_pines_mask) # Mask by pine plantations
#       agedist_dense_pines_df <- rbind(agedist_dense_pines_df,
#                                   data.frame(Scenario = mgmt.scenarios[i], Time = times[j],
#                                              Avg_age = cellStats(temp_dense_pines_masked, mean, na.rm=TRUE),
#                                              SD_age = cellStats(temp_dense_pines_masked, sd, na.rm=TRUE)))
#   }
#   write.table(agedist_dense_pines_df, paste(di, mgmt.scenarios[i], "/results/aggregated_agedist_dense_pines.txt", sep = ""))
# }