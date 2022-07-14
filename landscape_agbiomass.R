# Analysis of AGB on aggregated tables

mgmt.scenarios <- c("211129_conserv_current",
                    "211129_conserv_rcp45",
                    "211129_conserv_rcp85",
                    "211129_nomanag_current",
                    "211129_nomanag_rcp45",
                    "211129_nomanag_rcp85",
                    "211129_proactive_current",
                    "211129_proactive_rcp45",
                    "211129_proactive_rcp85",
                    "211129_proactiveplus_current",
                    "211129_proactiveplus_rcp45",
                    "211129_proactiveplus_rcp85") # Folder names with each scenario

replicates <- c(1:5)

### SETUP 
library(dplyr)
library(ggplot2)
library(reshape2)
library(lubridate)

di <- "/Volumes/GoogleDrive/My Drive/proj_LANDIS/experiments/" # Path to simulations folder
outputs_folder <- "211129_outputs/" # Subfolder for outputs

groups <- data.frame(Species = c("qfaginea", "qpyrenaica", "qilex", "ppinaster", "pnigra", "psylvestris", "phalepensis", "tall", "medium", "short", "jcommunis", "joxycedrus", "popnigra"),
                     Group = c("Oaks", "Oaks", "Oaks", "P. pinaster", "P. nigra", "P. sylvestris", "P. halepensis", "Other", "Other", "Other", "Other", "Other", "Other"))

species <- data.frame(Species = c("qfaginea", "qpyrenaica", "qilex", "ppinaster", "pnigra", "psylvestris", "phalepensis", "tall", "medium", "short", "jcommunis", "joxycedrus", "popnigra"),
                      Species_latin = c("Q. faginea", "Q. pyrenaica", "Q. ilex", "P. pinaster", "P. nigra", "P. sylvestris", "P. halepensis", "Tall", "Medium", "Short", "J. communis", "J. oxycedrus", "Pop.nigra"))

cols <- c('#e41a1c', '#377eb8', '#4daf4a', '#984ea3', '#ff7f00', '#ffff33', '#a65628')# '#91bfdb','#4575b4','#ffffbf', '#fee090','#fc8d59','#d73027')
cols_species <- c("Q. faginea" = '#e41a1c', # "#e0f3f8", 
                  "Q. pyrenaica" = '#377eb8', # "#91bfdb",
                  "Q. ilex" = '#4daf4a', # "#4575b4", 
                  "P. halepensis" = '#984ea3', # "#ffffbf",
                  "P. nigra" = '#ff7f00', # "#fee090",
                  "P. pinaster" = '#ffff33', # "#fc8d59",
                  "P. sylvestris" = '#a65628')# "#d73027")
cols_pines <- c('#ffffbf', '#fee090','#fc8d59','#d73027')
  
# AGB 
all_replicates <- data.frame()
for (i in seq_along(mgmt.scenarios)) {
  for (j in seq_along(replicates)) {
  one_replicate <- read.table(paste(di, mgmt.scenarios[i], "_rep", replicates[j], "/results/aggregated_agbiomass_dense_pines.txt", sep =""), header = TRUE) %>% 
    mutate(Harv_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][2],
           Clim_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][3])
  all_replicates <- rbind(all_replicates, one_replicate)
  }
}

agb_dense_pines <- all_replicates %>%
  select(-Scenario, -SD_tnha) %>%  
  group_by(Harv_scenario, Clim_scenario, Time, Species) %>%
  summarise(Avg_mean_tnha_accross_rep = mean(Avg_tnha),
            SD_mean_tnha_accross_rep = sd(Avg_tnha),
            Avg_total_tn_accross_rep = mean(Total_tn),
            SD_total_tn_accross_rep = sd(Total_tn)) %>%
  mutate(Year = Time + 2005)

# Fetch labels
Harv_scenario.labs <- data.frame(Harv_scenario_original = c("nomanag", "conserv", "proactive", "proactiveplus"),
                                 Harv_scenario = c('Non-management', 'Conservative', 'Proactive', 'Proactive-plus'))

Clim_scenario.labs <- data.frame(Clim_scenario_original = c("current", "rcp45", "rcp85"),
                                 Clim_scenario = c("Current", "RCP4.5", "RCP8.5"))

agb_dense_pines <- agb_dense_pines %>%
  rename(Harv_scenario_original = Harv_scenario,
         Clim_scenario_original = Clim_scenario) %>%
  left_join(Harv_scenario.labs) %>%
  left_join(Clim_scenario.labs)

agb_dense_pines$Harv_scenario <- factor(agb_dense_pines$Harv_scenario, 
                                     levels=c("Non-management", "Conservative", "Proactive", "Proactive-plus"))
agb_dense_pines$Clim_scenario <- factor(agb_dense_pines$Clim_scenario, 
                                     levels=c("Current", "RCP4.5", "RCP8.5"))

# AGB stacked
# jpeg(file = paste(di, outputs_folder, "total_agb.jpeg", sep = ""), width=18, height=12, units="in", res=300)
agb_dense_pines %>%
  left_join(groups) %>%
  dplyr::select(-Species) %>%
  group_by(Harv_scenario, Clim_scenario, Year, Group) %>%
  summarise(Total_tn = sum(Avg_total_tn_accross_rep)) %>%
  ggplot(aes(x = Year, y = Total_tn, fill = Group)) +
  geom_area() +
  facet_wrap(Clim_scenario ~ Harv_scenario) + # , scales = "free_y"
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  scale_fill_manual(values = cols) +
  ylab("Total biomass (tn)") +
  xlab("")
# dev.off()

# jpeg(file = paste(di, outputs_folder, "mean_agb.jpeg", sep = ""), width=18, height=12, units="in", res=300)
agb_dense_pines %>%
  left_join(groups) %>%
  dplyr::select(-Species) %>%
  group_by(Harv_scenario, Clim_scenario, Time, Group) %>%
  summarise(Mean_tnha = sum(Avg_mean_tnha_accross_rep)) %>%
  ggplot(aes(x = Time, y = Mean_tnha, fill = Group)) +
  geom_area() +
  facet_wrap(Clim_scenario ~ Harv_scenario) + # , scales = "free_y"
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  scale_fill_manual(values = cols) +
  ylab("Mean biomass (tn/ha)") +
  xlab("Time")
# dev.off()

# AGB by species
# jpeg(file = paste(di, outputs_folder, "agb_oaks.jpeg", sep = ""), width=18, height=12, units="in", res=300)
agb_dense_pines %>%
  filter(Species == "qilex" |
           Species == "qfaginea" |
           Species == "qpyrenaica") %>%
  left_join(species) %>%
  ggplot(aes(x = Time, y = Avg_mean_tnha_accross_rep)) +
  geom_line(aes(color = Species_latin)) +
  geom_point(aes(color = Species_latin)) +
  geom_errorbar(aes(ymin = Avg_mean_tnha_accross_rep - SD_mean_tnha_accross_rep,
                    ymax = Avg_mean_tnha_accross_rep + SD_mean_tnha_accross_rep), width=.2,
                position = position_dodge(.05)) +
  facet_wrap(Clim_scenario ~ Harv_scenario) + #
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  scale_color_manual(values = cols_species) +
  scale_linetype_manual(values = lines) +
  ylab("Mean biomass (tn/ha)") +
  xlab("Time")
# dev.off()

# jpeg(file = paste(di, outputs_folder, "agb_pines.jpeg", sep = ""), width=18, height=12, units="in", res=300)
agb_dense_pines %>%
  filter(Species == "ppinaster" |
           Species == "psylvestris" |
           Species == "pnigra" |
           Species == "phalepensis") %>%
  left_join(species) %>%
  ggplot(aes(x = Time, y = Avg_mean_tnha_accross_rep)) +
  geom_line(aes(color = Species_latin)) +
  geom_point(aes(color = Species_latin)) +
  geom_errorbar(aes(ymin = Avg_mean_tnha_accross_rep - SD_mean_tnha_accross_rep,
                    ymax = Avg_mean_tnha_accross_rep + SD_mean_tnha_accross_rep), width=.2,
                position = position_dodge(.05)) +
  facet_wrap(Clim_scenario ~ Harv_scenario) + #
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  scale_color_manual(values = cols_species) +
  scale_linetype_manual(values = lines) +
  ylab("Mean biomass (tn/ha)") +
  xlab("Time")
# dev.off()

jpeg(file = paste(di, outputs_folder, "agb_species_lines_oaks_pines_current_rcp85_revised.jpeg", sep = ""), width=8, height=12, units="in", res=300)
agb_dense_pines %>%
  filter(Species == "qilex" |
           Species == "qfaginea" |
           Species == "qpyrenaica" |
           Species == "ppinaster" |
           Species == "psylvestris" |
           Species == "pnigra" |
           Species == "phalepensis",
         Clim_scenario == "Current" |
         Clim_scenario == "RCP8.5") %>%
  mutate(Year = Time + 2005) %>%
  left_join(species) %>%
  ggplot(aes(x = Year, y = Avg_mean_tnha_accross_rep)) +
  geom_line(aes(color = Species_latin)) +
  geom_point(aes(color = Species_latin)) +
  geom_errorbar(aes(ymin = Avg_mean_tnha_accross_rep - SD_mean_tnha_accross_rep, 
                    ymax = Avg_mean_tnha_accross_rep + SD_mean_tnha_accross_rep), width=.2,
                position = position_dodge(.05)) +
  facet_grid(Harv_scenario ~ Clim_scenario) + #
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 24),
        legend.key.size = unit(1, "cm")) +  
  guides(color = guide_legend(nrow = 3)) +
  scale_color_manual(values = cols_species) +
  scale_linetype_manual(values = lines) +
  ylab("Mean biomass (tn/ha)") +
  xlab("Year")
dev.off()

jpeg(file = paste(di, outputs_folder, "agb_species_lines_others.jpeg", sep = ""), width=18, height=12, units="in", res=300)
agb_dense_pines %>%
  filter(Species == "short" |
           Species == "medium" |
           Species == "tall" |
           Species == "jcommunis" |
           Species == "joxycedrus" |
           Species == "popnigra") %>%
  left_join(species) %>%
  ggplot(aes(x = Time, y = Avg_mean_tnha_accross_rep)) +
  geom_line(aes(color = Species_latin)) +
  geom_point(aes(color = Species_latin)) +
  geom_errorbar(aes(ymin = Avg_mean_tnha_accross_rep - SD_mean_tnha_accross_rep, 
                    ymax = Avg_mean_tnha_accross_rep + SD_mean_tnha_accross_rep), width=.2,
                position = position_dodge(.05)) +
  facet_wrap(Clim_scenario ~ Harv_scenario) + #
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 24),
        legend.key.size = unit(1, "cm")) +  
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines) +
  ylab("Mean biomass (tn/ha)") +
  xlab("Time")
dev.off()

# Total biomass
total <- data.frame()
for (i in seq_along(mgmt.scenarios)) {
  for (j in seq_along(replicates)) {
    one_replicate <- read.table(paste(di, mgmt.scenarios[i], "_rep", replicates[j], "/results/aggregated_agbiomass_dense_pines.txt", sep =""), header = TRUE) %>% 
      mutate(Harv_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][2],
             Clim_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][3],
             Replicate = replicates[j])
    total <- rbind(total, one_replicate)
  }
}

total <- total %>%
  select(Clim_scenario, Harv_scenario, Time, Replicate, Avg_tnha) %>%  
  group_by(Clim_scenario, Harv_scenario, Time, Replicate) %>%
  summarise(All_spp_tnha = sum(Avg_tnha)) %>%
  select(-Replicate) %>%
  group_by(Clim_scenario, Harv_scenario, Time) %>%
  summarise(Avg_all_spp_tnha = mean(All_spp_tnha),
            SD_all_spp_tnha = sd(All_spp_tnha)) %>%
  mutate(Year = Time + 2005,
         Scenario = paste0(Clim_scenario, Harv_scenario)) %>%
  rename(Harv_scenario_original = Harv_scenario,
         Clim_scenario_original = Clim_scenario) %>%
  left_join(Harv_scenario.labs) %>%
  left_join(Clim_scenario.labs)

jpeg(file = paste(di, outputs_folder, "agb_all_species_lines_revised.jpeg", sep = ""), width=12, height=8, units="in", res=300)
total %>%
  ggplot(aes(x = Year, y = Avg_all_spp_tnha, group = Scenario)) +
  geom_line(aes(color = Harv_scenario)) +
  geom_point(aes(color = Harv_scenario)) +
  geom_errorbar(aes(ymin = Avg_all_spp_tnha - SD_all_spp_tnha, 
                    ymax = Avg_all_spp_tnha + SD_all_spp_tnha), width=.2,
                position = position_dodge(.05)) +
  facet_wrap(. ~ Clim_scenario) + #
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 18),
        legend.key.size = unit(0.5, "cm")) +  
  scale_color_manual(values = cols) +
  ylab("Mean total biomass (tn/ha)") +
  xlab("Year")
dev.off()  
  
  
  