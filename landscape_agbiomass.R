# Analysis of AGB on aggregated tables

mgmt.scenarios <- c(...) # Folder names with each scenario

replicates <- c(1:5)

### SETUP 
library(dplyr)
library(ggplot2)
library(reshape2)
library(lubridate)

di <- ".../experiments/" # Path to simulations folder
outputs_folder <- "..." # Subfolder for outputs

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
                                 Harv_scenario = c('No Management', 'Conservative', 'Proactive', 'ProactivePlus'))

Clim_scenario.labs <- data.frame(Clim_scenario_original = c("current", "rcp45", "rcp85"),
                                 Clim_scenario = c("Current", "RCP4.5", "RCP8.5"))

agb_dense_pines <- agb_dense_pines %>%
  rename(Harv_scenario_original = Harv_scenario,
         Clim_scenario_original = Clim_scenario) %>%
  left_join(Harv_scenario.labs) %>%
  left_join(Clim_scenario.labs)

agb_dense_pines$Harv_scenario <- factor(agb_dense_pines$Harv_scenario, 
                                     levels=c("No Management", "Conservative", "Proactive", "ProactivePlus"))
agb_dense_pines$Clim_scenario <- factor(agb_dense_pines$Clim_scenario, 
                                     levels=c("Current", "RCP4.5", "RCP8.5"))

# AGB stacked
jpeg(file = paste(di, outputs_folder, "total_agb.jpeg", sep = ""), width=18, height=12, units="in", res=300)
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
dev.off()

jpeg(file = paste(di, outputs_folder, "mean_agb.jpeg", sep = ""), width=18, height=12, units="in", res=300)
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
dev.off()

# AGB by species
jpeg(file = paste(di, outputs_folder, "agb_oaks.jpeg", sep = ""), width=18, height=12, units="in", res=300)
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
dev.off()

jpeg(file = paste(di, outputs_folder, "agb_pines.jpeg", sep = ""), width=18, height=12, units="in", res=300)
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
dev.off()

jpeg(file = paste(di, outputs_folder, "agb_species_lines_oaks_pines.jpeg", sep = ""), width=18, height=12, units="in", res=300)
agb_dense_pines %>%
  filter(Species == "qilex" |
           Species == "qfaginea" |
           Species == "qpyrenaica" |
           Species == "ppinaster" |
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
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +  
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines) +
  ylab("Mean biomass (tn/ha)") +
  xlab("Time")
dev.off()
