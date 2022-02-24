# Analysis of harvested biomass at landscape scale

### NAME OF SCENARIOS ####
mgmt.scenarios <- c(...) # Folder names with each scenario


replicates <- c(1:5) 

### SETUP ###
di <- "/Volumes/GoogleDrive/My Drive/proj_LANDIS/experiments/" # Path to simulations folder
outputs_folder <- "211129_outputs/" # Subfolder for outputs

library(dplyr)
library(ggplot2)
library(reshape2)

cols <- c("conserv" = "#33A02C", # dark green
          "proactive" = "#6A3D9A", # dark purple
          "proactiveplus" = "#FF7F00", # orange
          "nomanag" = "#E31A1C") # red
lines <- c("current" = "solid", "rcp45" = "dotdash", "rcp85" = "dashed")

### LOAD SUMMARY TABLES
summaries <- data.frame()
for (i in seq_along(mgmt.scenarios)) {
  for (j in seq_along(replicates)) {
    temp <- read.table(paste(di, mgmt.scenarios[i], "_rep", replicates[j], "/output/harvest/summary-log.csv", sep = ""), sep = ",", header = TRUE) %>%
      mutate(Scenario_name = mgmt.scenarios[i],
             Harv_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][2],
             Clim_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][3],
             Replicate = replicates[j])
    summaries <- rbind(summaries, temp)
  }
}

# Fetch labels
Harv_scenario.labs <- data.frame(Harv_scenario_original = c("nomanag", "conserv", "proactive", "proactiveplus"),
                                 Harv_scenario = c('No management', 'Conservative', 'Proactive', 'Proactive-plus'))

Clim_scenario.labs <- data.frame(Clim_scenario_original = c("current", "rcp45", "rcp85"),
                                 Clim_scenario = c("Current", "RCP4.5", "RCP8.5"))

summaries <- summaries %>%
  rename(Harv_scenario_original = Harv_scenario,
         Clim_scenario_original = Clim_scenario) %>%
  left_join(Harv_scenario.labs) %>%
  left_join(Clim_scenario.labs)

summaries$Harv_scenario <- factor(summaries$Harv_scenario, 
                                  levels=c("No management", "Conservative", "Proactive", "Proactive-plus"))
summaries$Clim_scenario <- factor(summaries$Clim_scenario,
                                  levels=c("Current", "RCP4.5", "RCP8.5"))

# Total biomass and percentage of area harvested in study area
total <- summaries %>%
  select(Scenario_name, Harv_scenario, Clim_scenario, Replicate, Time, ManagementArea, Prescription, HarvestedSites, TotalBiomassHarvested) %>%
  rename(TotalBiomassHarvested_Mg = TotalBiomassHarvested) %>%
  select(-ManagementArea, -Prescription) %>%
  group_by(Scenario_name, Harv_scenario, Clim_scenario, Replicate, Time) %>%
  summarise(tot_HarvestedSites = sum(HarvestedSites),
            tot_TotalBiomassHarvested_Mg = sum(TotalBiomassHarvested_Mg))

tot_by_time <- total %>%
  select(-Replicate) %>%
  group_by(Scenario_name, Harv_scenario, Clim_scenario, Time) %>%
  summarise(avg_tot_HarvestedSites_across_reps = mean(tot_HarvestedSites),
            sd_tot_HarvestedSites_across_reps = sd(tot_HarvestedSites),
            avg_tot_TotalBiomassHarvested_Mg_across_reps = mean(tot_TotalBiomassHarvested_Mg),
            sd_tot_TotalBiomassHarvested_Mg_across_reps = sd(tot_TotalBiomassHarvested_Mg))
  
# jpeg(file = paste(di, outputs_folder, "harv_sites_whole_study_area.jpeg", sep = ""), width = 9, height = 6, units="in", res=300)
ggplot(tot_by_time, aes(x = Time, y = avg_tot_HarvestedSites_across_reps, group = Scenario_name)) +
  geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
  geom_point(aes(color = Harv_scenario)) +
  geom_errorbar(aes(ymin = avg_tot_HarvestedSites_across_reps - sd_tot_HarvestedSites_across_reps, 
                    ymax = avg_tot_HarvestedSites_across_reps + sd_tot_HarvestedSites_across_reps,
                    group = Scenario_name),
                width= .5, position = position_dodge(.9)) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  ylab("Harvested area (has)") +
  xlab("Time (years)") +
  ggtitle("Whole study area")
# dev.off()

ggplot(tot_by_time, aes(x = Time, y = avg_tot_TotalBiomassHarvested_Mg_across_reps, group = Scenario_name)) +
  geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
  geom_point(aes(color = Harv_scenario)) +
  geom_errorbar(aes(ymin = avg_tot_TotalBiomassHarvested_Mg_across_reps - sd_tot_TotalBiomassHarvested_Mg_across_reps, 
                    ymax = avg_tot_TotalBiomassHarvested_Mg_across_reps + sd_tot_TotalBiomassHarvested_Mg_across_reps,
                    group = Scenario_name),
                width= .5, position = position_dodge(.9)) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  ylab("Harvested biomass (Mg)") +
  xlab("Time (years)") +
  ggtitle("Whole study area")

# Harvested biomass by species (in the WHOLE landscape)
spp_harv <- summaries %>%
  select(Time, Scenario_name, Harv_scenario, Clim_scenario, Replicate, 
         BiomassHarvestedMg_jcommunis, BiomassHarvestedMg_joxycedrus, 
         BiomassHarvestedMg_phalepensis, BiomassHarvestedMg_pnigra,
         BiomassHarvestedMg_ppinaster, BiomassHarvestedMg_psylvestris, 
         BiomassHarvestedMg_popnigra, BiomassHarvestedMg_tall,
         BiomassHarvestedMg_medium, BiomassHarvestedMg_short,
         BiomassHarvestedMg_qfaginea, BiomassHarvestedMg_qilex, BiomassHarvestedMg_qpyrenaica) %>%
  group_by(Time, Scenario_name, Harv_scenario, Clim_scenario, Replicate) %>%
  summarise(jcomm = sum(BiomassHarvestedMg_jcommunis),
            joxy = sum(BiomassHarvestedMg_joxycedrus),
            phal = sum(BiomassHarvestedMg_phalepensis),
            pnig = sum(BiomassHarvestedMg_pnigra),
            ppin = sum(BiomassHarvestedMg_ppinaster),
            psyl = sum(BiomassHarvestedMg_psylvestris),
            pop = sum(BiomassHarvestedMg_popnigra),
            tall = sum(BiomassHarvestedMg_tall),
            med = sum(BiomassHarvestedMg_medium),
            sho = sum(BiomassHarvestedMg_short),
            qfag = sum(BiomassHarvestedMg_qfaginea),
            qilex = sum(BiomassHarvestedMg_qilex),
            qpyr = sum(BiomassHarvestedMg_qpyrenaica)) %>%
  select(-Replicate) %>%
  group_by(Time, Scenario_name, Harv_scenario, Clim_scenario) %>%
  summarise(phal_mean = mean(phal),
            pnig_mean = mean(pnig),
            ppin_mean = mean(ppin),
            psyl_mean = mean(psyl),
            phal_sd = sd(phal),
            pnig_sd = sd(pnig),
            ppin_sd = sd(ppin),
            psyl_sd = sd(psyl),
            qfag_mean = mean(qfag),
            qfag_sd = sd(qfag),
            qilex_mean = mean(qilex),
            qilex_sd = sd(qilex),
            qpyr_mean = mean(qpyr),
            qpyr_sd = sd(qpyr),
            tall_mean = mean(tall),
            tall_sd = sd(tall),
            med_mean = mean(med),
            med_sd = sd(med),
            sho_mean = mean(sho),
            sho_sd = sd(sho),
            pop_mean = mean(pop),
            pop_sd = sd(pop),
            jcomm_mean = mean(jcomm),
            jcomm_sd = sd(jcomm),
            joxy_mean = mean(joxy),
            joxy_sd = sd(joxy))
            
spp_harv_fetch <- spp_harv %>%
  select(Time, Scenario_name, Harv_scenario, Clim_scenario, 
         phal_mean, pnig_mean, ppin_mean, psyl_mean) %>%
  melt(id.vars = c("Time", "Scenario_name", "Harv_scenario", "Clim_scenario"))

ggplot(spp_harv_fetch, aes(x = Time, y = value, color = variable)) +
  geom_line() +
  facet_wrap(Clim_scenario ~ Harv_scenario) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14)) +
  ylab("Harvested biomass (??)") + # Not clear in manual whether output maps are in kg/ha or g/m2
  xlab("Time (years)") +
  ggtitle("Whole landscape")

### LOAD TABLES OF AGGREGATED HARVESTED BIOMASS ACROSS REPLICATES
harv_biomass <- data.frame()
for (i in seq_along(mgmt.scenarios)) {
  for (j in seq_along(replicates)) {
    temp <- read.table(paste(di, mgmt.scenarios[i], "_rep", replicates[j], "/results/aggregated_harvested_biomass_dense_pines.txt", sep = "")) %>%
      mutate(Scenario_name = mgmt.scenarios[i],
             Harv_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][2],
             Clim_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][3],
             Replicate = replicates[j])
    harv_biomass <- rbind(harv_biomass, temp)
  }
}

# Fetch labels
harv_biomass <- harv_biomass %>%
  rename(Harv_scenario_original = Harv_scenario,
         Clim_scenario_original = Clim_scenario) %>%
  left_join(Harv_scenario.labs) %>%
  left_join(Clim_scenario.labs)

harv_biomass$Harv_scenario <- factor(harv_biomass$Harv_scenario, 
                                  levels=c("No management", "Conservative", "Proactive", "Proactive-plus"))
harv_biomass$Clim_scenario <- factor(harv_biomass$Clim_scenario,
                                  levels=c("Current", "RCP4.5", "RCP8.5"))

# Calculate total biomass harvested within pine plantations
total_mask <- harv_biomass %>%
  select(Scenario_name, Harv_scenario, Clim_scenario, Replicate, Sum_harv_biomass) %>%
  group_by(Scenario_name, Harv_scenario, Clim_scenario, Replicate) %>%
  summarise(total_harv_biomass = sum(Sum_harv_biomass)) %>%
  select(-Replicate) %>%
  group_by(Scenario_name, Harv_scenario, Clim_scenario) %>%
  summarise(avg_total_harv_biomass = mean(total_harv_biomass), # units?
            sd_total_harv_biomass = sd(total_harv_biomass))

# Calculate biomass harvested through time in pine plantations
agg_harv_biomass <- harv_biomass %>%
  select(-Replicate, -Scenario) %>%
  group_by(Scenario_name, Harv_scenario, Clim_scenario, Time) %>%
  summarise(mean_sum_harv_biomas_across_reps = mean(Sum_harv_biomass),
            sd_sum_harv_biomas_across_reps = sd(Sum_harv_biomass))

# Plot
jpeg(file = paste(di, outputs_folder, "harv_biomass_plantations.jpeg", sep = ""), width =12, height = 9, units="in", res=300)
ggplot(agg_harv_biomass, aes(x = Time, y = mean_sum_harv_biomas_across_reps, group = Scenario_name)) +
  geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
  geom_point(aes(color = Harv_scenario)) +
  geom_errorbar(aes(ymin = mean_sum_harv_biomas_across_reps - sd_sum_harv_biomas_across_reps, 
                    ymax = mean_sum_harv_biomas_across_reps + sd_sum_harv_biomas_across_reps,
                    group = Scenario_name),
                width= .5, position = position_dodge(.9)) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  ylab("Harvested biomass") + # Not clear in manual whether output maps are in kg/ha or g/m2
  xlab("Time (years)") +
  ggtitle("")
dev.off()
