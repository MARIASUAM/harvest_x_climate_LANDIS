# Generate aggregated tables for pine mask

### SETUP
library(raster)
library(dplyr)

di <- "/Volumes/GoogleDrive/My Drive/proj_LANDIS/experiments/"

all_spp <- c("ppinaster", "pnigra", "phalepensis", "psylvestris", "qilex", "qfaginea", "qpyrenaica", "jcommunis", "joxycedrus", "tall", "medium", "short", "popnigra")
pines_and_oaks <- c("ppinaster", "pnigra", "phalepensis", "psylvestris", "qilex", "qfaginea", "qpyrenaica")

times <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95)
dec_times <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90)
times_no0 <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95)
dec_times_no0 <- c(10, 20, 30, 40, 50, 60, 70, 80, 90)

months_ch <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")

# Management scenarios
mgmt.scenarios <- c(
                    "210927_conserv_current_rep1",
                    # "210927_conserv_current_rep2",
                    # "210927_conserv_current_rep3",
                    # "210927_conserv_current_rep4",
                    # "210927_conserv_current_rep5",
                    "210927_conserv_rcp45_rep1",
                    # "210927_conserv_rcp45_rep2",
                    # "210927_conserv_rcp45_rep3",
                    # "210927_conserv_rcp45_rep4",
                    # "210927_conserv_rcp45_rep5",
                    "210927_conserv_rcp85_rep1",
                    # "210927_conserv_rcp85_rep2",
                    # "210927_conserv_rcp85_rep3",
                    # "210927_conserv_rcp85_rep4",
                    # "210927_conserv_rcp85_rep5",
                    "210927_nomanag_current_rep1",
                    # "210927_nomanag_current_rep2",
                    # "210927_nomanag_current_rep3",
                    # "210927_nomanag_current_rep4",
                    # "210927_nomanag_current_rep5",
                    "210927_nomanag_rcp45_rep1",
                    # "210927_nomanag_rcp45_rep2",
                    # "210927_nomanag_rcp45_rep3",
                    # "210927_nomanag_rcp45_rep4",
                    # "210927_nomanag_rcp45_rep5",
                    "210927_nomanag_rcp85_rep1",
                    # "210927_nomanag_rcp85_rep2",
                    # "210927_nomanag_rcp85_rep3",
                    # "210927_nomanag_rcp85_rep4",
                    # "210927_nomanag_rcp85_rep5",
                    "210927_proactive_current_rep1",
                    # "210927_proactive_current_rep2",
                    # "210927_proactive_current_rep3",
                    # "210927_proactive_current_rep4",
                    # "210927_proactive_current_rep5",
                    "210927_proactive_rcp45_rep1",
                    # "210927_proactive_rcp45_rep2",
                    # "210927_proactive_rcp45_rep3",
                    # "210927_proactive_rcp45_rep4",
                    # "210927_proactive_rcp45_rep5",
                    "210927_proactive_rcp85_rep1",
                    # "210927_proactive_rcp85_rep2",
                    # "210927_proactive_rcp85_rep3",
                    # "210927_proactive_rcp85_rep4",
                    # "210927_proactive_rcp85_rep5",
                    "210927_proactiveplus_current_rep1",
                    # "210927_proactiveplus_current_rep2",
                    # "210927_proactiveplus_current_rep3",
                    # "210927_proactiveplus_current_rep4",
                    # "210927_proactiveplus_current_rep5",
                    "210927_proactiveplus_rcp45_rep1",
                    # "210927_proactiveplus_rcp45_rep2",
                    # "210927_proactiveplus_rcp45_rep3",
                    # "210927_proactiveplus_rcp45_rep4",
                    # "210927_proactiveplus_rcp45_rep5",
                    "210927_proactiveplus_rcp85_rep1") #,
                    # "210927_proactiveplus_rcp85_rep2",
                    # "210927_proactiveplus_rcp85_rep3",
                    # "210927_proactiveplus_rcp85_rep4",
                    # "210927_proactiveplus_rcp85_rep5"
                    )
                  
### PINE PLANTATIONS MASKS
pines_mask <- raster(paste(di, "data/pines_mask.img", sep = ""))
dense_pines_mask <- raster(paste(di, "data/dense_pines_mask.img", sep = ""))

### ABOVEGROUND BIOMASS
for (i in 1:length(mgmt.scenarios)) {
  agb_pines_df <- data.frame()
  agb_dense_pines_df <- data.frame()
  for (j in 1:length(times)) {
    for (h in 1:length(all_spp)) {
      temp <- raster(paste(di, mgmt.scenarios[i], "/output/agbiomass/", all_spp[h], "/AGBiomass", times[j], ".img", sep = ""))
      temp <- temp / 100 # g/m2 to tn/ha
      temp_pines_masked <- mask(temp, pines_mask) # Mask by pine plantations
      temp_dense_pines_masked <- mask(temp, dense_pines_mask) # Mask by pine plantations

      agb_pines_df <- rbind(agb_pines_df,
                            data.frame(Scenario = mgmt.scenarios[i], Time = times[j], Species = all_spp[h],
                                       Avg_tnha = cellStats(temp_pines_masked, mean, na.rm=TRUE),
                                       SD_tnha = cellStats(temp_pines_masked, sd, na.rm=TRUE),
                                       Total_tn = cellStats(temp_pines_masked, sum, na.rm=TRUE)))

      agb_dense_pines_df <- rbind(agb_dense_pines_df,
                            data.frame(Scenario = mgmt.scenarios[i], Time = times[j], Species = all_spp[h],
                                       Avg_tnha = cellStats(temp_dense_pines_masked, mean, na.rm=TRUE),
                                       SD_tnha = cellStats(temp_dense_pines_masked, sd, na.rm=TRUE),
                                       Total_tn = cellStats(temp_dense_pines_masked, sum, na.rm=TRUE)))
    }
  }
  write.table(agb_pines_df, paste(di, mgmt.scenarios[i], "/results/aggregated_agbiomass_pines.txt", sep = ""))
  write.table(agb_dense_pines_df, paste(di, mgmt.scenarios[i], "/results/aggregated_agbiomass_dense_pines.txt", sep = ""))
}

# Monthly Net Photosynthesis
for (i in 1:length(mgmt.scenarios)) {
  net_psn_pines_df <- data.frame()
  net_psn_dense_pines_df <- data.frame()
  for (j in 1:length(times)) {
    for (h in 1:length(months_ch)){
      netpsn <- raster(paste(di, mgmt.scenarios[i], "/output/MonthlyNetPsn/MonthlyNetPsn_", times[j], months_ch[h], ".img", sep = ""))
      netpsn_pines <- mask(netpsn, pines_mask)
      netpsn_dense_pines <- mask(netpsn, dense_pines_mask)
      
      net_psn_pines_df <- rbind(net_psn_pines_df,
                                data.frame(Scenario = mgmt.scenarios[i], Time = times[j], Month = months_ch[h],
                                           Avg_Net_Psn = cellStats(netpsn_pines, mean, na.rm=TRUE),
                                           SD_Net_Psn = cellStats(netpsn_pines, sd, na.rm=TRUE)))
      
      net_psn_dense_pines_df <- rbind(net_psn_dense_pines_df,
                                      data.frame(Scenario = mgmt.scenarios[i], Time = times[j], Month = months_ch[h],
                                                 Avg_Net_Psn = cellStats(netpsn_dense_pines, mean, na.rm=TRUE),
                                                 SD_Net_Psn = cellStats(netpsn_dense_pines, sd, na.rm=TRUE)))
    }
  }
  write.table(net_psn_pines_df, paste(di, mgmt.scenarios[i], "/results/aggregated_net_psn_pines.txt", sep = ""))
  write.table(net_psn_dense_pines_df, paste(di, mgmt.scenarios[i], "/results/aggregated_net_psn_dense_pines.txt", sep = ""))
}

### Age Distribution
for (i in 1:length(mgmt.scenarios)) {
  agedist_pines_df <- data.frame()
  agedist_dense_pines_df <- data.frame()
  for (j in 1:length(times)) {
      temp <- raster(paste(di, mgmt.scenarios[i], "/output/AgeDist/Age_", times[j], ".img", sep = ""))
      temp_pines_masked <- mask(temp, pines_mask) # Mask by pine plantations
      temp_dense_pines_masked <- mask(temp, dense_pines_mask) # Mask by pine plantations
      
      agedist_pines_df <- rbind(agedist_pines_df,
                            data.frame(Scenario = mgmt.scenarios[i], Time = times[j],
                                       Avg_age = cellStats(temp_pines_masked, mean, na.rm=TRUE),
                                       SD_age = cellStats(temp_pines_masked, sd, na.rm=TRUE)))
      
      agedist_dense_pines_df <- rbind(agedist_dense_pines_df,
                                  data.frame(Scenario = mgmt.scenarios[i], Time = times[j],
                                             Avg_age = cellStats(temp_dense_pines_masked, mean, na.rm=TRUE),
                                             SD_age = cellStats(temp_dense_pines_masked, sd, na.rm=TRUE)))
  }
  write.table(agedist_pines_df, paste(di, mgmt.scenarios[i], "/results/aggregated_agedist_pines.txt", sep = ""))
  write.table(agedist_dense_pines_df, paste(di, mgmt.scenarios[i], "/results/aggregated_agedist_dense_pines.txt", sep = ""))
}

### ABOVEGROUND BIOMASS BY AGE
age_classes <- data.frame(code = c(1, 2, 3), class = c("1-10", "10-20", "20-50"))
for (i in 1:length(mgmt.scenarios)) {
  agb_by_age_pines_df <- data.frame()
  agb_by_age_dense_pines_df <- data.frame()
  for (j in 1:length(dec_times)) {
    for (h in 1:length(pines_and_oaks)) {
      for (k in 1:length(age_classes$code)) {
        temp <- raster(paste(di, mgmt.scenarios[i], "/output/biomas-by-age/", pines_and_oaks[h], "-ageclass", age_classes$code[k], "-", dec_times[j], ".img", sep = ""))
        temp <- temp / 100 # g/m2 to tn/ha
        temp_pines_masked <- mask(temp, pines_mask) # Mask by pine plantations
        temp_dense_pines_masked <- mask(temp, dense_pines_mask) # Mask by pine plantations

        agb_by_age_pines_df <- rbind(agb_by_age_pines_df,
                                      data.frame(Scenario = mgmt.scenarios[i], Time = dec_times[j],
                                                 Species = pines_and_oaks[h], Age_class = age_classes$class[k],
                                                 Avg_tnha = cellStats(temp_pines_masked, mean, na.rm=TRUE),
                                                 SD_tnha = cellStats(temp_pines_masked, sd, na.rm=TRUE),
                                                 Total_tn = cellStats(temp_pines_masked, sum, na.rm=TRUE)))

        agb_by_age_dense_pines_df <- rbind(agb_by_age_dense_pines_df,
                                      data.frame(Scenario = mgmt.scenarios[i], Time = dec_times[j],
                                                 Species = pines_and_oaks[h], Age_class = age_classes$class[k],
                                                 Avg_tnha = cellStats(temp_dense_pines_masked, mean, na.rm=TRUE),
                                                 SD_tnha = cellStats(temp_dense_pines_masked, sd, na.rm=TRUE),
                                                 Total_tn = cellStats(temp_dense_pines_masked, sum, na.rm=TRUE)))
      }
    }
  }
  write.table(agb_by_age_pines_df, paste(di, mgmt.scenarios[i], "/results/aggregated_agbiomass_by_age_pines.txt", sep = ""))
  write.table(agb_by_age_dense_pines_df, paste(di, mgmt.scenarios[i], "/results/aggregated_agbiomass_by_age_dense_pines.txt", sep = ""))
}

### Cohort stats - General variables
for (i in 1:length(mgmt.scenarios)) {
  coh_var_pines_df <- data.frame()
  coh_var_dense_pines_df <- data.frame()
  for (j in 1:length(times)) {
    age_count <- raster(paste(di, mgmt.scenarios[i], "/output/cohort-stats/AGE-COUNT-", times[j], ".img", sep = ""))
    age_med <- raster(paste(di, mgmt.scenarios[i], "/output/cohort-stats/AGE-MED-", times[j], ".img", sep = ""))
    age_min <- raster(paste(di, mgmt.scenarios[i], "/output/cohort-stats/AGE-MIN-", times[j], ".img", sep = ""))
    age_rich <- raster(paste(di, mgmt.scenarios[i], "/output/cohort-stats/AGE-RICH-", times[j], ".img", sep = ""))
    age_sd <- raster(paste(di, mgmt.scenarios[i], "/output/cohort-stats/AGE-SD-", times[j], ".img", sep = ""))
    spp_rich <- raster(paste(di, mgmt.scenarios[i], "/output/cohort-stats/SPP-RICH-", times[j], ".img", sep = ""))
    
    age_count_masked <- mask(age_count, pines_mask)
    age_med_masked <- mask(age_med, pines_mask)
    age_min_masked <- mask(age_min, pines_mask)
    age_rich_masked <- mask(age_rich, pines_mask)
    age_sd_masked <- mask(age_sd, pines_mask)
    spp_rich_masked <- mask(spp_rich, pines_mask)

    coh_var_pines_df <- rbind(coh_var_pines_df,
                              data.frame(Scenario = mgmt.scenarios[i], Time = times[j],
                                         Avg_age_count = cellStats(age_count_masked, mean, na.rm=TRUE),
                                         Avg_age_med = cellStats(age_med_masked, mean, na.rm=TRUE),
                                         Avg_age_min = cellStats(age_min_masked, mean, na.rm=TRUE),
                                         Avg_age_rich = cellStats(age_rich_masked, mean, na.rm=TRUE),
                                         Avg_age_sd = cellStats(age_sd_masked, mean, na.rm=TRUE),
                                         Avg_spp_rich = cellStats(spp_rich_masked, mean, na.rm=TRUE)))

    age_count_dense_masked <- mask(age_count, dense_pines_mask)
    age_med_dense_masked <- mask(age_med, dense_pines_mask)
    age_min_dense_masked <- mask(age_min, dense_pines_mask)
    age_rich_dense_masked <- mask(age_rich, dense_pines_mask)
    age_sd_dense_masked <- mask(age_sd, dense_pines_mask)
    spp_rich_dense_masked <- mask(spp_rich, dense_pines_mask)

    coh_var_dense_pines_df <- rbind(coh_var_dense_pines_df,
                                    data.frame(Scenario = mgmt.scenarios[i], Time = times[j],
                                               Avg_age_count = cellStats(age_count_dense_masked, mean, na.rm=TRUE),
                                               Avg_age_med = cellStats(age_med_dense_masked, mean, na.rm=TRUE),
                                               Avg_age_min = cellStats(age_min_dense_masked, mean, na.rm=TRUE),
                                               Avg_age_rich = cellStats(age_rich_dense_masked, mean, na.rm=TRUE),
                                               Avg_age_sd = cellStats(age_sd_dense_masked, mean, na.rm=TRUE),
                                               Avg_spp_rich = cellStats(spp_rich_dense_masked, mean, na.rm=TRUE)))
    }
  write.table(coh_var_pines_df, paste(di, mgmt.scenarios[i], "/results/aggregated_coh_stats_pines.txt", sep = ""))
  write.table(coh_var_dense_pines_df, paste(di, mgmt.scenarios[i], "/results/aggregated_coh_stats_dense_pines.txt", sep = ""))
}

### Cohort stats - Species variables
# for (i in 1:length(mgmt.scenarios)) {
#   spp_age_pines_df <- data.frame()
#   spp_age_dense_pines_df <- data.frame()
#   for (j in 1:length(times)) {
#     for (h in 1:length(pines_and_oaks)) {
#       # max <- raster(paste(di, mgmt.scenarios[i], "/output/cohort-stats/", pines_and_oaks[h], "-MAX-", times[j], ".img", sep = ""))
#       # max[max == 0] <- NA
#       # min <- raster(paste(di, mgmt.scenarios[i], "/output/cohort-stats/", pines_and_oaks[h], "-MIN-", times[j], ".img", sep = ""))
#       # min[min == 0] <- NA
#       # min[min == 20000] <- NA
#       
#       sd <- raster(paste(di, mgmt.scenarios[i], "/output/cohort-stats/", pines_and_oaks[h], "-SD-", times[j], ".img", sep = ""))
# 
#       # max_pines_masked <- mask(max, pines_mask)
#       # min_pines_masked <- mask(min, pines_mask)
#       sd_pines_masked <- mask(sd, pines_mask)
#       
#       spp_age_pines_df <- rbind(spp_age_pines_df,
#                                 data.frame(Scenario = mgmt.scenarios[i], Time = times[j], Species = pines_and_oaks[h],
#                                            # Avg_max_age = cellStats(max_pines_masked, mean, na.rm=TRUE),
#                                            # Avg_min_age = cellStats(min_pines_masked, mean, na.rm=TRUE)))
#                                            Avg_sd_age = cellStats(sd_pines_masked, mean, na.rm=TRUE)))
# 
#       # max_dense_pines_masked <- mask(max, dense_pines_mask)
#       # min_dense_pines_masked <- mask(min, dense_pines_mask)
#       sd_dense_pines_masked <- mask(sd, dense_pines_mask)
# 
#       spp_age_dense_pines_df <- rbind(spp_age_dense_pines_df,
#                                 data.frame(Scenario = mgmt.scenarios[i], Time = times[j], Species = pines_and_oaks[h],
#                                            # Avg_max_age = cellStats(max_dense_pines_masked, mean, na.rm=TRUE),
#                                            # Avg_min_age = cellStats(min_dense_pines_masked, mean, na.rm=TRUE)))
#                                            Avg_sd_age = cellStats(sd_dense_pines_masked, mean, na.rm=TRUE)))
#     }
#   }
#   write.table(spp_age_pines_df, paste(di, mgmt.scenarios[i], "/results/aggregated_spp_age_pines.txt", sep = ""))
#   write.table(spp_age_dense_pines_df, paste(di, mgmt.scenarios[i], "/results/aggregated_spp_age_dense_pines.txt", sep = ""))
# }

# Cohort per species
for (i in 1:length(mgmt.scenarios)) {
  cohorts_pines_df <- data.frame()
  cohorts_dense_pines_df <- data.frame()
  for (j in 1:length(times)) {
    for (h in 1:length(all_spp)) {
      temp <- raster(paste(di, mgmt.scenarios[i], "/output/CohortsPerspecies/", all_spp[h], "/cohorts_", times[j], ".img", sep = ""))
      temp_pines_masked <- mask(temp, pines_mask) # Mask by pine plantations
      temp_dense_pines_masked <- mask(temp, dense_pines_mask) # Mask by pine plantations
      
      cohorts_pines_df <- rbind(cohorts_pines_df,
                            data.frame(Scenario = mgmt.scenarios[i], Time = times[j], Species = all_spp[h],
                                       Avg_cohorts = cellStats(temp_pines_masked, mean, na.rm = TRUE),
                                       Nr_cohorts = cellStats(temp_pines_masked, sum, na.rm = TRUE)))

      cohorts_dense_pines_df <- rbind(cohorts_dense_pines_df,
                                  data.frame(Scenario = mgmt.scenarios[i], Time = times[j], Species = all_spp[h],
                                             Avg_cohorts = cellStats(temp_dense_pines_masked, mean, na.rm = TRUE),
                                             Nr_cohorts = cellStats(temp_dense_pines_masked, sum, na.rm = TRUE)))
    }
  }
  write.table(cohorts_pines_df, paste(di, mgmt.scenarios[i], "/results/aggregated_cohorts_pines.txt", sep = ""))
  write.table(cohorts_dense_pines_df, paste(di, mgmt.scenarios[i], "/results/aggregated_cohorts_dense_pines.txt", sep = ""))
}

# Establishment
# establishment_legend <- data.frame(Code = c(0, 1, 2, 3),
#                                    Establishment = c("No_presence", "Continued_Presence", "Discontinued_Presence", "New_Presence"))
# 
# for (i in 1:length(mgmt.scenarios)) {
#   establishment_pines_df <- data.frame()
#   establishment_dense_pines_df <- data.frame()
#   for (j in 1:length(times_no0)) {
#     for (h in 1:length(all_spp)) {
#       temp <- raster(paste(di, mgmt.scenarios[i], "/output/Establishment/", all_spp[h], "/Est_", times_no0[j], ".img", sep = ""))
# 
#       temp_pines_masked <- mask(temp, pines_mask) # Mask by pine plantations
#       establishment_pines_df <- rbind(establishment_pines_df,
#                                       data.frame(Code = temp_pines_masked[]) %>% # dplyr::filter(is.na(Code) == FALSE) %>%
#                                         group_by(Code) %>% summarise(Nr_cells = n()) %>%
#                                         left_join(establishment_legend, by = c("Code" = "Code")) %>%
#                                         mutate(Scenario = mgmt.scenarios[i], Time = times_no0[j], Species = all_spp[h]) %>%
#                                         dplyr::select(Scenario, Time, Species, Establishment, Nr_cells))
# 
#       temp_dense_pines_masked <- mask(temp, dense_pines_mask) # Mask by pine plantations
#       establishment_dense_pines_df <- rbind(establishment_dense_pines_df,
#                                       data.frame(Code = temp_dense_pines_masked[]) %>% # dplyr::filter(is.na(Code) == FALSE) %>%
#                                         group_by(Code) %>% summarise(Nr_cells = n()) %>%
#                                         left_join(establishment_legend, by = c("Code" = "Code")) %>%
#                                         mutate(Scenario = mgmt.scenarios[i], Time = times_no0[j], Species = all_spp[h]) %>%
#                                         dplyr::select(Scenario, Time, Species, Establishment, Nr_cells))
#     }
#   }
#   write.table(establishment_pines_df, paste(di, mgmt.scenarios[i], "/results/aggregated_establishment_pines.txt", sep = ""))
#   write.table(establishment_dense_pines_df, paste(di, mgmt.scenarios[i], "/results/aggregated_establishment_dense_pines.txt", sep = ""))
# }

# Establishment probability
for (i in 1:length(mgmt.scenarios)) {
  estprob_pines_df <- data.frame()
  estprob_dense_pines_df <- data.frame()
  for (j in 1:length(times)) {
    for (h in 1:length(all_spp)) {
      temp <- raster(paste(di, mgmt.scenarios[i], "/output/Establishment/EstProbability/", all_spp[h], "/EstProb_", times[j], ".img", sep = ""))
      
      temp_pines_masked <- mask(temp, pines_mask) # Mask by pine plantations
      estprob_pines_df <- rbind(estprob_pines_df,
                                data.frame(Scenario = mgmt.scenarios[i], Time = times[j], Species = all_spp[h],
                                           Avg_Est_Prob = cellStats(temp_pines_masked, mean, na.rm = TRUE)))
      
      temp_dense_pines_masked <- mask(temp, dense_pines_mask) # Mask by pine plantations
      estprob_dense_pines_df <- rbind(estprob_dense_pines_df,
                                      data.frame(Scenario = mgmt.scenarios[i], Time = times[j], Species = all_spp[h],
                                                 Avg_Est_Prob = cellStats(temp_dense_pines_masked, mean, na.rm = TRUE)))
    }
  }
  write.table(estprob_pines_df, paste(di, mgmt.scenarios[i], "/results/aggregated_est_prob_pines.txt", sep = ""))
  write.table(estprob_dense_pines_df, paste(di, mgmt.scenarios[i], "/results/aggregated_est_prob_dense_pines.txt", sep = ""))
}

# Harvested biomass
for (i in 1:length(mgmt.scenarios)) {
  harv_biomass_pines_df <- data.frame()
  harv_biomass_dense_pines_df <- data.frame()
  for (j in 1:length(dec_times_no0)) {
    harv_biomass <- raster(paste(di, mgmt.scenarios[i], "/output/harvest/biomass-removed-", dec_times_no0[j], ".tif", sep = ""))
    harv_biomass_pines <- mask(harv_biomass, pines_mask)
    harv_biomass_dense_pines <- mask(harv_biomass, dense_pines_mask)

    harv_biomass_pines_df <- rbind(harv_biomass_pines_df,
                                   data.frame(Scenario = mgmt.scenarios[i], Time = dec_times_no0[j],
                                         Avg_harv_biomass = cellStats(harv_biomass_pines, mean, na.rm=TRUE),
                                         # SD_harv_biomass = cellStats(harv_biomass_pines, sd, na.rm=TRUE)))
                                         Sum_harv_biomass = cellStats(harv_biomass_pines, sum, na.rm=TRUE)))

    harv_biomass_dense_pines_df <- rbind(harv_biomass_dense_pines_df,
                                   data.frame(Scenario = mgmt.scenarios[i], Time = dec_times_no0[j],
                                              Avg_harv_biomass = cellStats(harv_biomass_dense_pines, mean, na.rm=TRUE),
                                              # SD_harv_biomass = cellStats(harv_biomass_dense_pines, sd, na.rm=TRUE))),
                                              Sum_harv_biomass = cellStats(harv_biomass_dense_pines, sum, na.rm=TRUE)))
  }
  write.table(harv_biomass_pines_df, paste(di, mgmt.scenarios[i], "/results/aggregated_harvested_biomass_pines.txt", sep = ""))
  write.table(harv_biomass_dense_pines_df, paste(di, mgmt.scenarios[i], "/results/aggregated_harvested_biomass_dense_pines.txt", sep = ""))
}

### MORTALITY
for (i in 1:length(mgmt.scenarios)) {
  mort_dense_pines_df <- data.frame()
  for (j in 1:length(times_no0)) {
    for (h in 1:length(pines_and_oaks)) {
      temp <- raster(paste(di, mgmt.scenarios[i], "/results/", pines_and_oaks[h], "_diff_cohorts_", times_no0[j], ".asc", sep = ""))
      temp_dense_pines_masked <- mask(temp, dense_pines_mask) # Mask by pine plantations
      temp_dense_pines_masked <- reclassify(temp_dense_pines_masked, 
                                            cbind(0, Inf, 0), right=FALSE) # Reclassify by discarding new cohorts (positive values)
      
      temp_dense_pines_masked <- temp_dense_pines_masked * -1 # Absolute value to get dead cohorts as positive values
      mort_dense_pines_df <- rbind(mort_dense_pines_df,
                                  data.frame(Scenario = mgmt.scenarios[i], Time = times_no0[j], Species = pines_and_oaks[h],
                                             Total_dead_cohorts = cellStats(temp_dense_pines_masked, sum, na.rm=TRUE)))
    }
  }
  write.table(mort_dense_pines_df, paste(di, mgmt.scenarios[i], "/results/aggregated_diff_cohorts.txt", sep = ""))
}
