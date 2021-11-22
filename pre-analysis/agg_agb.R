# Analysis of AGB on aggregated tables

mgmt.scenarios <- c("210927_conserv_current_rep1",
                    "210927_conserv_rcp45_rep1",
                    "210927_conserv_rcp85_rep1",
                    "210927_nomanag_current_rep1",
                    "210927_nomanag_rcp45_rep1",
                    "210927_nomanag_rcp85_rep1",
                    "210927_proactive_current_rep1",
                    "210927_proactive_rcp45_rep1",
                    "210927_proactive_rcp85_rep1",
                    "210927_proactiveplus_current_rep1",
                    "210927_proactiveplus_rcp45_rep1",
                    "210927_proactiveplus_rcp85_rep1",
                    "210930_conservplus_current_rep1",
                    "210930_conservplus_rcp45_rep1",
                    "210930_conservplus_rcp85_rep1")

### SETUP 
library(dplyr)
library(ggplot2)
library(reshape2)
library(lubridate)

di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/"

cols <- c("conserv" = "#33A02C", # dark green
          "proactive" = "#6A3D9A", # dark purple
          "proactiveplus" = "#FF7F00", # orange
          "nomanag" = "#E31A1C") # red

lines <- c("current" = "solid", "rcp45" = "dotdash", "rcp85" = "dashed")

groups <- data.frame(Species = c("qfaginea", "qpyrenaica", "qilex", "ppinaster", "pnigra", "psylvestris", "phalepensis", "tall", "medium", "short", "jcommunis", "joxycedrus", "popnigra"),
                     Group = c("Oaks", "Oaks", "Oaks", "P. pinaster", "P. nigra", "P. sylvestris", "P. halepensis", "Other", "Other", "Other", "Other", "Other", "Other"))

# AGB 
agb_dense_pines <- data.frame()
agb_pines <- data.frame()
for (i in 1:length(mgmt.scenarios)) {
  temp <- read.table(paste(di, mgmt.scenarios[i], "/results/aggregated_agbiomass_dense_pines.txt", sep =""), header = TRUE) %>%
    mutate(Harv_scenario = strsplit(as.character(Scenario), split = "_")[[1]][2],
           Clim_scenario = strsplit(as.character(Scenario), split = "_")[[1]][3])
  agb_dense_pines <- rbind(agb_dense_pines, temp)
  
  temp <- read.table(paste(di, mgmt.scenarios[i], "/results/aggregated_agbiomass_pines.txt", sep =""), header = TRUE) %>%
    mutate(Harv_scenario = strsplit(as.character(Scenario), split = "_")[[1]][2],
           Clim_scenario = strsplit(as.character(Scenario), split = "_")[[1]][3])
  agb_pines <- rbind(agb_pines, temp)
}

agb_dense_pines$Harv_scenario <- as.factor(agb_dense_pines$Harv_scenario)
agb_pines$Harv_scenario <- as.factor(agb_pines$Harv_scenario)

levels(agb_dense_pines$Harv_scenario)<- c("nomanag", "conserv", "conservplus", "proactive", "proactiveplus")
levels(agb_pines$Harv_scenario)<- c("nomanag", "conserv", "conservplus", "proactive", "proactiveplus")

# AGB stacked by species
agb_pines %>%
  left_join(groups) %>%
  dplyr::select(-Species, -Scenario, -Avg_tnha, -SD_tnha) %>%
  group_by(Harv_scenario, Clim_scenario, Time, Group) %>%
  summarise(Total_tn = sum(Total_tn)) %>%
  ggplot(aes(x = Time, y = Total_tn, fill = Group)) +
  geom_area() +
  facet_wrap(Clim_scenario ~ Harv_scenario) + # , scales = "free_y"
  theme_classic() +
  theme(legend.position = "bottom")

jpeg(file = paste(di, "210927_outputs/agb_conservplus.jpeg", sep = ""), width=18, height=12, units="in", res=300)
agb_dense_pines %>%
  left_join(groups) %>%
  dplyr::select(-Species, -Scenario, -Avg_tnha, -SD_tnha) %>%
  group_by(Harv_scenario, Clim_scenario, Time, Group) %>%
  summarise(Total_tn = sum(Total_tn)) %>%
  ggplot(aes(x = Time, y = Total_tn, fill = Group)) + # Total_tn / 53367
  geom_area() +
  facet_wrap(Clim_scenario ~ Harv_scenario, ncol = 5) + # , scales = "free_y"
  theme_classic() +
  theme(legend.position = "bottom")
dev.off()

# # Plotting
agb_dense_pines %>%
  filter(Species == "qilex" |
           Species == "qfaginea" |
           Species == "qpyrenaica") %>%
  ggplot(aes(x = Time, y = Total_tn, group = Scenario)) +
  geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
  geom_point(aes(color = Harv_scenario)) +
  facet_wrap(Species ~ ., scales = "free_y") + #
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines)

agb_dense_pines %>%
  filter(Species == "ppinaster" |
           Species == "pnigra" |
           Species == "psylvestris"|
           Species == "phalepensis") %>%
  ggplot(aes(x = Time, y = Total_tn, group = Scenario)) +
  geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
  geom_point(aes(color = Harv_scenario)) +
  facet_wrap(Species ~ ., scales = "free_y") +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines)

# AGB by groups
## oaks
# agb %>%
#   filter(Species == "qilex" | 
#            Species == "qfaginea" | 
#            Species == "qpyrenaica") %>%
#   select(Scenario, Harv_scenario, Clim_scenario, Time, Total_tn) %>%
#   group_by(Scenario, Harv_scenario, Clim_scenario, Time) %>%
#   summarise(Total_oaks_tn = sum(Total_tn)) %>%
#   ggplot(aes(x = Time, y = Total_oaks_tn, group = Scenario)) +
#   geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
#   geom_point(aes(color = Harv_scenario)) +
#   theme_classic() +
#   theme(legend.position = "bottom") +
#   scale_color_manual(values = cols) +
#   scale_linetype_manual(values = lines)
# 
# ## pines
# agb %>%
#   filter(Species == "ppinaster" | 
#            Species == "phalepensis" | 
#            Species == "psylvestris" | 
#            Species == "pnigra") %>%
#   select(Scenario, Harv_scenario, Clim_scenario, Time, Total_tn) %>%
#   group_by(Scenario, Harv_scenario, Clim_scenario, Time) %>%
#   summarise(Total_pines_tn = sum(Total_tn)) %>%
#   ggplot(aes(x = Time, y = Total_pines_tn, group = Scenario)) +
#   geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
#   geom_point(aes(color = Harv_scenario)) +
#   theme_classic() +
#   theme(legend.position = "bottom") +
#   scale_color_manual(values = cols) +
#   scale_linetype_manual(values = lines)
# 
# # Pine species and oaks as groups
# agb_groups <- agb %>%
#   left_join(groups) %>%
#   select(Scenario, Harv_scenario, Clim_scenario, Time, Group, Total_tn) %>%
#   group_by(Scenario, Harv_scenario, Clim_scenario, Time, Group) %>%
#   summarise(Total_biomass_tn = sum(Total_tn))
# 
# agb_groups %>%
#   filter(Group != "Other") %>%
#   ggplot(aes(x = Time, y = Total_biomass_tn, group = Scenario)) +
#   geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
#   geom_point(aes(color = Harv_scenario)) +
#   theme_classic() +
#   facet_wrap(Group ~ ., scales = "free_y") +
#   theme(legend.position = "bottom") +
#   scale_color_manual(values = cols) +
#   scale_linetype_manual(values = lines)
# 
## AGB relative difference
# agb_current <- agb %>%
#   filter(Clim_scenario == "current") %>%
#   mutate(Base_avg_tnha = Avg_tnha) %>%
#   select(-Scenario, -Clim_scenario, -Avg_tnha, -SD_tnha, - Total_tn)
# 
# agb_rel_difference <- agb %>%
#   filter(Clim_scenario != "current") %>%
#   left_join(agb_current) %>%
#   mutate(Relative_dif_avg_tnha = (Avg_tnha * 100) / Base_avg_tnha)
# 
# agb_rel_difference %>%
#   filter(Species == "ppinaster" |
#            Species == "pnigra" |
#            Species == "psylvestris"|
#            Species == "phalepensis") %>%
#   ggplot(aes(x = Time, y = Relative_dif_avg_tnha, group = Scenario)) +
#   geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
#   geom_point(aes(color = Harv_scenario)) +
#   facet_wrap(Species ~ .) + # , scales = "free_y"
#   theme_classic() +
#   theme(legend.position = "bottom") +
#   scale_color_manual(values = cols) +
#   scale_linetype_manual(values = lines)
# 
# agb_rel_difference %>%
#   filter(Species == "qfaginea" |
#            Species == "qilex" |
#            Species == "qpyrenaica") %>%
#   ggplot(aes(x = Time, y = Relative_dif_avg_tnha, group = Scenario)) +
#   geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
#   geom_point(aes(color = Harv_scenario)) +
#   facet_wrap(Species ~ .) + # , scales = "free_y"
#   theme_classic() +
#   theme(legend.position = "bottom") +
#   scale_color_manual(values = cols) +
#   scale_linetype_manual(values = lines)

agb_pines %>%
  left_join(groups) %>%
  dplyr::select(-Species, -Scenario, -Total_tn, -SD_tnha) %>%
  group_by(Harv_scenario, Clim_scenario, Time, Group) %>%
  summarise(Avg_tnha_groups = sum(Avg_tnha)) %>%
  ggplot(aes(x = Time, y = Avg_tnha_groups, fill = Group)) +
  geom_area() +
  facet_wrap(Clim_scenario ~ Harv_scenario) + # , scales = "free_y"
  theme_classic() +
  theme(legend.position = "bottom")
