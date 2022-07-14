## Analysis of mortality

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

mgmt.scenarios.one.rep <- c("211129_conserv_current_rep1",
                           "211129_conserv_rcp45_rep1",
                           "211129_conserv_rcp85_rep2",
                           "211129_nomanag_current_rep1",
                           "211129_nomanag_rcp45_rep1",
                           "211129_nomanag_rcp85_rep1",
                           "211129_proactive_current_rep1",
                           "211129_proactive_rcp45_rep1",
                           "211129_proactive_rcp85_rep2",
                           "211129_proactiveplus_current_rep1",
                           "211129_proactiveplus_rcp45_rep2",
                           "211129_proactiveplus_rcp85_rep1")

replicates <- c(1:5)

### SETUP 
library(dplyr)
library(ggplot2)

di <- "/Volumes/GoogleDrive/My Drive/proj_LANDIS/experiments/" # Path to simulations folder
outputs_folder <- "211129_outputs/" # Subfolder for outputs

groups <- data.frame(Species = c("qfaginea", "qpyrenaica", "qilex", "ppinaster", "pnigra", "psylvestris", "phalepensis", "tall", "medium", "short", "jcommunis", "joxycedrus", "popnigra"),
                     Group = c("Oaks", "Oaks", "Oaks", "Pines", "Pines", "Pines", "Pines", "Other", "Other", "Other", "Other", "Other", "Other"))

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

two_cols <- c('#1b9e77','#7570b3')

# Load aggregates tables 
all_replicates <- data.frame()
for (i in seq_along(mgmt.scenarios)) {
  for (j in seq_along(replicates)) {
    one_replicate <- read.table(paste(di, mgmt.scenarios[i], "_rep", replicates[j], "/results/aggregated_diff_cohorts_revised.txt", sep =""), header = TRUE) %>% 
      mutate(Harv_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][2],
             Clim_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][3])
    all_replicates <- rbind(all_replicates, one_replicate)
  }
}

# Load aggregates tables - revised version, only one rep per scenario
all_replicates <- data.frame()
for (i in seq_along(mgmt.scenarios.one.rep)) {
    one_replicate <- read.table(paste(di, mgmt.scenarios.one.rep[i], "/results/aggregated_diff_cohorts_revised.txt", sep =""), header = TRUE) %>% 
      mutate(Harv_scenario = strsplit(as.character(mgmt.scenarios.one.rep[i]), split = "_")[[1]][2],
             Clim_scenario = strsplit(as.character(mgmt.scenarios.one.rep[i]), split = "_")[[1]][3])
    all_replicates <- rbind(all_replicates, one_replicate)
}

# Calculate average across replicates
mort_dense_pines <- all_replicates %>%
  select(-Scenario) %>%  
  group_by(Harv_scenario, Clim_scenario, Time, Species) %>%
  summarise(Avg_dead_coh_accross_rep = mean(Total_dead_cohorts),
            SD_dead_coh_accross_rep = sd(Total_dead_cohorts)) %>%
  mutate(Year = Time + 2005)

# Fetch labels
Harv_scenario.labs <- data.frame(Harv_scenario_original = c("nomanag", "conserv", "proactive", "proactiveplus"),
                                 Harv_scenario = c('No Management', 'Conservative', 'Proactive', 'ProactivePlus'))

Clim_scenario.labs <- data.frame(Clim_scenario_original = c("current", "rcp45", "rcp85"),
                                 Clim_scenario = c("Current", "RCP4.5", "RCP8.5"))

mort_dense_pines <- mort_dense_pines %>%
  rename(Harv_scenario_original = Harv_scenario,
         Clim_scenario_original = Clim_scenario) %>%
  left_join(Harv_scenario.labs) %>%
  left_join(Clim_scenario.labs)

mort_dense_pines$Harv_scenario <- factor(mort_dense_pines$Harv_scenario, 
                                     levels=c("No Management", "Conservative", "Proactive", "ProactivePlus"))
mort_dense_pines$Clim_scenario <- factor(mort_dense_pines$Clim_scenario, 
                                     levels=c("Current", "RCP4.5", "RCP8.5"))

# Plots
# jpeg(file = paste(di, outputs_folder, "dead_cohorts_phalepensis.jpeg", sep = ""), width=18, height=12, units="in", res=300)
mort_dense_pines %>%
  left_join(species) %>%
  filter(Species == "qilex" |  Species == "qfaginea" |  Species == "qpyrenaica" |  Species == "ppinaster" |  Species == "psylvestris" |  Species == "pnigra" |  Species == "phalepensis") %>% #  Species == "qilex" |  Species == "qfaginea" |  Species == "qpyrenaica" |  Species == "ppinaster" |  Species == "psylvestris" |  Species == "pnigra" |  Species == "phalepensis"
  ggplot(aes(x = Year, y = Avg_dead_coh_accross_rep)) +
  geom_line(aes(color = Species_latin)) +
  geom_point(aes(color = Species_latin)) +
  geom_errorbar(aes(ymin = Avg_dead_coh_accross_rep - SD_dead_coh_accross_rep, 
                    ymax = Avg_dead_coh_accross_rep + SD_dead_coh_accross_rep), width = .2,
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
  ylab("Mortality (nr. dead cohorts)") +
  xlab("Time")
# dev.off()

# Relative difference per species
baseline <- mort_dense_pines %>%
  filter(Clim_scenario == "Current") %>%
  select(-Harv_scenario_original, -Clim_scenario_original, -Clim_scenario, -SD_dead_coh_accross_rep) %>%
  rename(Base_dead_coh = Avg_dead_coh_accross_rep)
baseline <- baseline[,-c(1:3)]

clim_scenarios <- mort_dense_pines %>%
  filter(Clim_scenario != "Current") %>%
  select(-Harv_scenario_original, -Clim_scenario_original, -SD_dead_coh_accross_rep) %>%
  rename(Dead_coh = Avg_dead_coh_accross_rep)
clim_scenarios <- clim_scenarios[,-c(1:3)]

relative_diff <- left_join(clim_scenarios, baseline) %>%
  select(Clim_scenario, Harv_scenario, Year, Species, Base_dead_coh, Dead_coh) %>%
  mutate(Diff = Dead_coh - Base_dead_coh,
         Rel_dif = Diff * 100 / Base_dead_coh)

# jpeg(file = paste(di, outputs_folder, "mort_rel_diff_per_species.jpeg", sep = ""), width=18, height=12, units="in", res=300)
relative_diff %>%
  left_join(species) %>%
  filter(Species == "qilex" |  Species == "qfaginea" |  Species == "qpyrenaica" |  Species == "ppinaster" |  Species == "psylvestris" |  Species == "pnigra" |  Species == "phalepensis") %>% #  Species == "qilex" |  Species == "qfaginea" |  Species == "qpyrenaica" |  Species == "ppinaster" |  Species == "psylvestris" |  Species == "pnigra" |  Species == "phalepensis"
  ggplot(aes(x = Year, y = Rel_dif)) +
  geom_line(aes(color = Species_latin)) +
  geom_point(aes(color = Species_latin)) +
  facet_wrap(Clim_scenario ~ Harv_scenario, nrow = 2) + #
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +  
  scale_color_manual(values = cols_species) +
  scale_linetype_manual(values = lines) +
  ylab("Relative difference in dead cohorts (%)") +
  xlab("Year")
# dev.off()

# Relative difference total (all species together)
baseline_tot <- mort_dense_pines %>%
  filter(Clim_scenario == "Current") %>%
  select(-Harv_scenario_original, -Clim_scenario_original, -Clim_scenario, -SD_dead_coh_accross_rep, -Species) %>%
  rename(Base_dead_coh = Avg_dead_coh_accross_rep) %>%
  group_by(Harv_scenario_original, Clim_scenario_original, Time, Year, Harv_scenario) %>%
  summarise(Base_dead_coh_tot = sum(Base_dead_coh))
baseline_tot <- baseline_tot[,-c(1:3)]

clim_scenarios_tot <- mort_dense_pines %>%
  filter(Clim_scenario != "Current") %>%
  select(-Harv_scenario_original, -Clim_scenario_original, -SD_dead_coh_accross_rep, -Species) %>%
  rename(Dead_coh = Avg_dead_coh_accross_rep) %>%
  group_by(Harv_scenario_original, Clim_scenario_original, Time, Year, Harv_scenario, Clim_scenario) %>%
  summarise(Dead_coh_tot = sum(Dead_coh))
clim_scenarios_tot <- clim_scenarios_tot[,-c(1:3)]

relative_diff_tot <- left_join(clim_scenarios_tot, baseline_tot) %>%
  select(Clim_scenario, Harv_scenario, Year, Base_dead_coh_tot, Dead_coh_tot) %>%
  mutate(Diff = Dead_coh_tot - Base_dead_coh_tot,
         Rel_dif = Diff * 100 / Base_dead_coh_tot)

# jpeg(file = paste(di, outputs_folder, "mort_rel_diff_total.jpeg", sep = ""), width=18, height=12, units="in", res=300)
relative_diff_tot %>%
  ggplot(aes(x = Year, y = Rel_dif)) +
  xlim(2015, 2100) +
  ylim(-100,400) +
  geom_line() +
  geom_point() +
  facet_wrap(Clim_scenario ~ Harv_scenario, nrow = 2) + #
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +  
  scale_linetype_manual(values = lines) +
  ylab("Relative difference in dead cohorts (%)") +
  xlab("Year")
# dev.off()

# All species together
total <- data.frame()
for (i in seq_along(mgmt.scenarios)) {
  for (j in seq_along(replicates)) {
    one_replicate <- read.table(paste(di, mgmt.scenarios[i], "_rep", replicates[j], "/results/aggregated_diff_cohorts.txt", sep =""), header = TRUE) %>% 
      mutate(Harv_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][2],
             Clim_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][3],
             Replicate = replicates[j],)
    total <- rbind(total, one_replicate)
  }
}

total <- total %>%
  rename(Harv_scenario_original = Harv_scenario,
         Clim_scenario_original = Clim_scenario) %>%
  left_join(Harv_scenario.labs) %>%
  left_join(Clim_scenario.labs) %>%
  select(-Harv_scenario_original, -Clim_scenario_original, -Scenario)

total$Harv_scenario <- factor(total$Harv_scenario, 
                              levels=c("No Management", "Conservative", "Proactive", "ProactivePlus"))
total$Clim_scenario <- factor(total$Clim_scenario, 
                              levels=c("Current", "RCP4.5", "RCP8.5"))

total <- total %>%
  filter(Species == "ppinaster" |
           Species == "pnigra" |
           Species == "phalepensis" |
           Species == "psylvestris" |
           Species == "qilex" |
           Species == "qfaginea" |
           Species == "qpyrenaica") %>%
  select(-Species) %>%
  group_by(Harv_scenario, Clim_scenario, Replicate, Time) %>%
  summarise(Total_dead_cohorts = sum(Total_dead_cohorts)) 

total_agg <- total %>%
  select(-Replicate) %>%
  group_by(Harv_scenario, Clim_scenario, Time) %>%
  summarise(Avg = mean(Total_dead_cohorts),
            SD = sd(Total_dead_cohorts))

# jpeg(file = paste(di, outputs_folder, "mort_abs_all_spp_rep5.jpeg", sep = ""), width=18, height=12, units="in", res=300)
total %>%
  filter(Replicate == 5) %>%
  ggplot(aes(x = Time + 2005, y = Total_dead_cohorts)) +
  geom_line(size = 0.3) +
  geom_point(size = 0.5) +
  facet_wrap(Clim_scenario ~ Harv_scenario) + #
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +  
  scale_linetype_manual(values = lines) +
  ylab("Dead cohorts (nr.)") +
  xlab("Year")
# dev.off()

# jpeg(file = paste(di, outputs_folder, "mort_abs_all_spp.jpeg", sep = ""), width=18, height=12, units="in", res=300)
total_agg %>%
  # filter(Harv_scenario == "No Management") %>%
  ggplot(aes(x = Time + 2005, y = Avg)) +
  geom_line(size = 0.3) +
  geom_point(size = 0.5) +
  geom_errorbar(aes(ymin = Avg - SD, 
                    ymax = Avg + SD), width = .2,
                position = position_dodge(.05)) +
  facet_wrap(Clim_scenario ~ Harv_scenario, nrow = 3) + #Harv_scenario
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +  
  scale_linetype_manual(values = lines) +
  ylab("Dead cohorts (nr.)") +
  xlab("Year")
# dev.off()

# By species groups
by_spp <- data.frame()
for (i in seq_along(mgmt.scenarios.one.rep)) {
    one_replicate <- read.table(paste(di, mgmt.scenarios.one.rep[i], "/results/aggregated_diff_cohorts_revised.txt", sep =""), header = TRUE) %>% 
      mutate(Harv_scenario = strsplit(as.character(mgmt.scenarios.one.rep[i]), split = "_")[[1]][2],
             Clim_scenario = strsplit(as.character(mgmt.scenarios.one.rep[i]), split = "_")[[1]][3])
    by_spp <- rbind(by_spp, one_replicate)
}

by_spp <- by_spp %>%
  rename(Harv_scenario_original = Harv_scenario,
         Clim_scenario_original = Clim_scenario) %>%
  left_join(Harv_scenario.labs) %>%
  left_join(Clim_scenario.labs) %>%
  select(-Harv_scenario_original, -Clim_scenario_original, -Scenario)

by_spp$Harv_scenario <- factor(by_spp$Harv_scenario, 
                              levels=c("No Management", "Conservative", "Proactive", "ProactivePlus"))
by_spp$Clim_scenario <- factor(by_spp$Clim_scenario, 
                              levels=c("Current", "RCP4.5", "RCP8.5"))

by_spp_groups <- by_spp %>%
  filter(Species == "ppinaster" |
           Species == "pnigra" |
           Species == "phalepensis" |
           Species == "psylvestris" |
           Species == "qilex" |
           Species == "qfaginea" |
           Species == "qpyrenaica") %>%
  left_join(groups) %>%
  select(-Species) %>%
  group_by(Harv_scenario, Clim_scenario, Time, Group) %>%
  summarise(Total_dead_cohorts = sum(Total_dead_cohorts)) 

# jpeg(file = paste(di, outputs_folder, "mort_abs_groups_rep5.jpeg", sep = ""), width=18, height=12, units="in", res=300)
by_spp_groups %>%
  filter(Replicate == 5) %>%
  ggplot(aes(x = Time + 2005, y = Total_dead_cohorts, color = Group)) +
  geom_line(size = 0.3) +
  geom_point(size = 0.5) +
  facet_wrap(Clim_scenario ~ Harv_scenario) + #
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +  
  scale_linetype_manual(values = lines) +
  ylab("Dead cohorts (nr.)") +
  xlab("Year")
# dev.off()

by_spp_groups_agg <- by_spp_groups %>%
  select(-Replicate) %>%
  group_by(Harv_scenario, Clim_scenario, Time, Group) %>%
  summarise(Avg = mean(Total_dead_cohorts),
            SD = sd(Total_dead_cohorts))

jpeg(file = paste(di, outputs_folder, "mort_abs_groups.jpeg", sep = ""), width=10, height=4, units="in", res=300)
# jpeg(file = paste(di, outputs_folder, "mort_abs_groups_full_revised.jpeg", sep = ""), width = 10, height = 12, units="in", res=300)
by_spp_groups %>%
  filter(Harv_scenario == "No Management") %>%
  ggplot(aes(x = Time + 2005, y = Total_dead_cohorts, group = Group)) +
  geom_line(aes(color = Group, linetype = Group)) +
  geom_point(aes(color = Group)) +
  # geom_errorbar(aes(ymin = Avg - SD, 
  #                   ymax = Avg + SD), width = .2,
  #               width= .2, position = position_dodge(.05)) +
  facet_wrap(Clim_scenario ~ Harv_scenario, ncol = 4) + #Harv_scenario
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +  
  scale_color_manual(values = two_cols) +
  scale_y_continuous("Dead cohorts (nr.)") +
  xlab("Year")
dev.off()

# Calculate total number of dead cohorts
by_spp_groups %>%
  select(-Time) %>%
  group_by(Harv_scenario, Clim_scenario, Group) %>%
  summarise(total = sum(Total_dead_cohorts))
  
  
  
  
  