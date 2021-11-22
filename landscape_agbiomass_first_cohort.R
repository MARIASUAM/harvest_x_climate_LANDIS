# Analysis of AGB of first cohorts on aggregated tables

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

### SETUP 
library(dplyr)
library(ggplot2)
library(reshape2)
library(lubridate)

di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/"
outputs_folder <- "210927_outputs/"

groups <- data.frame(Species = c("qfaginea", "qpyrenaica", "qilex", "ppinaster", "pnigra", "psylvestris", "phalepensis", "tall", "medium", "short", "jcommunis", "joxycedrus", "popnigra"),
                     Group = c("Oaks", "Oaks", "Oaks", "Pines", "Pines", "Pines", "Pines", "Other", "Other", "Other", "Other", "Other", "Other"))

species <- data.frame(Species = c("qfaginea", "qpyrenaica", "qilex", "ppinaster", "pnigra", "psylvestris", "phalepensis", "tall", "medium", "short", "jcommunis", "joxycedrus", "popnigra"),
                      Species_latin = c("Q. faginea", "Q. pyrenaica", "Q. ilex", "P. pinaster", "P. nigra", "P. sylvestris", "P. halepensis", "Tall", "Medium", "Short", "J. communis", "J. oxycedrus", "Pop.nigra"))

cols <- c('#91bfdb','#fc8d59')
cols_species <- c('#d73027', '#fc8d59', '#fee090', '#ffffbf', '#e0f3f8', '#4575b4', '#91bfdb')

cols_species <- c("Q. faginea" = "#e0f3f8", 
          "Q. pyrenaica" = "#91bfdb",
          "Q. ilex" = "#4575b4", 
          "P. halepensis" = "#ffffbf",
          "P. nigra" = "#fee090",
          "P. pinaster" = "#fc8d59",
          "P. sylvestris" = "#d73027")

# Load AGB of first cohort for each replicate
all_replicates <- data.frame()
for (i in seq_along(mgmt.scenarios)) {
  for (j in seq_along(replicates)) {
    one_replicate <- read.table(paste(di, mgmt.scenarios[i], "_rep", replicates[j], "/results/aggregated_agbiomass_by_age_dense_pines.txt", sep =""), header = TRUE) %>% 
      mutate(Harv_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][2],
             Clim_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][3],
             Replicate = replicates[j])
    all_replicates <- rbind(all_replicates, one_replicate)
  }
}

# Group by species groups and aggregate
agb_dense_pines <- all_replicates %>%
  left_join(groups) %>%
  filter(Age_class == "1-10") %>%
  select(Harv_scenario, Clim_scenario, Replicate, Time, Group, Avg_tnha) %>%  
  group_by(Harv_scenario, Clim_scenario, Replicate, Time, Group) %>%
  summarise(Group_avg_tnha = sum(Avg_tnha)) %>%
  select(-Replicate) %>%
  group_by(Harv_scenario, Clim_scenario, Time, Group) %>%
  summarise(Mean_group_avg_tnha_across_reps = mean(Group_avg_tnha),
            SD_group_avg_tnha_across_reps = sd(Group_avg_tnha)) %>%
  mutate(Year = Time + 2005)

# Fetch labels
Harv_scenario.labs <- data.frame(Harv_scenario_original = c("nomanag", "conserv", "proactive", "proactiveplus"),
                                 Harv_scenario = c('No Management', 'Conservative', 'Proactive', 'ProactivePlus'))

Clim_scenario.labs <- data.frame(Clim_scenario_original = c("current", "rcp45", "rcp85"),
                                 Clim_scenario = c("Current", "RCP4.5", "RCP8.5"))

agb_dense_pines <- agb_dense_pines %>%
  rename(Harv_scenario_original = Harv_scenario,
         Clim_scenario_original = Clim_scenario) %>%
  left_join(Harv_scenario.labs) %>%
  left_join(Clim_scenario.labs)

agb_dense_pines$Harv_scenario <- as.factor(agb_dense_pines$Harv_scenario)
levels(agb_dense_pines$Harv_scenario)<- c('No Management', 'Conservative', 'Proactive', 'ProactivePlus')

agb_dense_pines$Clim_scenario <- as.factor(agb_dense_pines$Clim_scenario)
levels(agb_dense_pines$Clim_scenario) <- c("Current", "RCP4.5", "RCP8.5")

# AGB stacked by species
ggplot(agb_dense_pines, aes(x = Year, y = Mean_group_avg_tnha_across_reps, fill = Group)) +
  geom_line(aes(color = Group)) +
  geom_point(aes(color = Group)) +
  geom_errorbar(aes(ymin = Mean_group_avg_tnha_across_reps - SD_group_avg_tnha_across_reps, 
                    ymax = Mean_group_avg_tnha_across_reps + SD_group_avg_tnha_across_reps), width=.2,
                position = position_dodge(.05)) +
  facet_wrap(Clim_scenario ~ Harv_scenario) + # , scales = "free_y"
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  ylab("Average biomass (tn/ha)") +
  scale_color_manual(values = cols) +
  xlab("")

# Group by species and aggregate
agb_dense_pines_spp <- all_replicates %>%
  left_join(species) %>%
  filter(Age_class == "1-10") %>%
  select(Harv_scenario, Clim_scenario, Replicate, Time, Species, Species_latin, Avg_tnha) %>%  
  group_by(Harv_scenario, Clim_scenario, Replicate, Time, Species, Species_latin) %>%
  summarise(Group_avg_tnha = sum(Avg_tnha)) %>%
  select(-Replicate) %>%
  group_by(Harv_scenario, Clim_scenario, Time, Species, Species_latin) %>%
  summarise(Mean_group_avg_tnha_across_reps = mean(Group_avg_tnha),
            SD_group_avg_tnha_across_reps = sd(Group_avg_tnha)) %>%
  mutate(Year = Time + 2005) %>%
  rename(Harv_scenario_original = Harv_scenario,
         Clim_scenario_original = Clim_scenario) %>%
  left_join(Harv_scenario.labs) %>%
  left_join(Clim_scenario.labs)

agb_dense_pines_spp$Harv_scenario <- as.factor(agb_dense_pines_spp$Harv_scenario)
levels(agb_dense_pines_spp$Harv_scenario)<- c('No Management', 'Conservative', 'Proactive', 'ProactivePlus')

jpeg(file = paste(di, outputs_folder, "agb_first_cohort.jpeg", sep = ""), width = 18, height = 12, units="in", res=300)
ggplot(agb_dense_pines_spp, aes(x = Year, y = Mean_group_avg_tnha_across_reps)) +
  geom_line(aes(color = Species_latin)) +
  geom_point(aes(color = Species_latin)) +
  geom_errorbar(aes(ymin = Mean_group_avg_tnha_across_reps - SD_group_avg_tnha_across_reps, 
                    ymax = Mean_group_avg_tnha_across_reps + SD_group_avg_tnha_across_reps), width=.2,
                position = position_dodge(.05)) +
  facet_wrap(Clim_scenario ~ Harv_scenario) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  ylab("Average biomass (tn/ha)") +
  scale_color_manual(values = cols_species) +
  xlab("")
dev.off()
