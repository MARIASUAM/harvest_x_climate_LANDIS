# Analysis of Establishment on aggregated tables

mgmt.scenarios <- c("20210921_nomanag_current_MIROC5",
                    "20210921_nomanag_rcp45_MIROC5",
                    "20210921_nomanag_rcp85_MIROC5")

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

# Establishment
est_pines <- data.frame()
est_dense_pines <- data.frame()
for (i in 1:length(mgmt.scenarios)) {
  temp <- read.table(paste(di, mgmt.scenarios[i], "/results/aggregated_establishment_pines.txt", sep =""), header = TRUE) %>%
    mutate(Harv_scenario = strsplit(as.character(Scenario), split = "_")[[1]][2],
           Clim_scenario = strsplit(as.character(Scenario), split = "_")[[1]][3])
  est_pines <- rbind(est_pines, temp)
  
  temp <- read.table(paste(di, mgmt.scenarios[i], "/results/aggregated_establishment_dense_pines.txt", sep =""), header = TRUE) %>%
    mutate(Harv_scenario = strsplit(as.character(Scenario), split = "_")[[1]][2],
           Clim_scenario = strsplit(as.character(Scenario), split = "_")[[1]][3])
  est_dense_pines <- rbind(est_dense_pines, temp)
}

full_cols <- c("210913_conserv_current_MIROC5" = "green1",
               "210913_conserv_rcp45_MIROC5" = "green2",
               "210913_conserv_rcp85_MIROC5" = "green3",
               "210913_nomanag_current_MIROC5" = "red1",
               "210913_nomanag_rcp45_MIROC5" = "red2",
               "210913_nomanag_rcp85_MIROC5" = "red3",
               "210913_proactive_current_MIROC5" = "mediumpurple1",
               "210913_proactive_rcp45_MIROC5" = "mediumpurple2",
               "210913_proactive_rcp85_MIROC5" = "mediumpurple3",
               "210913_proactiveplus_current_MIROC5" = "orange1",          
               "210913_proactiveplus_rcp45_MIROC5" = "orange2",
               "210913_proactiveplus_rcp85_MIROC5" = "orange3")

full_cols <- c("20210921_nomanag_current_MIROC5" = "red1",
               "20210921_nomanag_rcp45_MIROC5" = "red2",
               "20210921_nomanag_rcp85_MIROC5" = "red3")

est_pines %>%
  filter(is.na(Establishment) == FALSE) %>%
  filter(Time == 5 | Time == 95) %>%
  filter(Species == "ppinaster") %>% 
  ggplot(aes(x = Establishment, y = Nr_cells, fill = Scenario)) +
  geom_bar(stat = "identity", position="dodge") +
  facet_grid(Time ~ Species) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = full_cols)

est_pines %>%
  filter(is.na(Establishment) == FALSE) %>%
  filter(Time == 5 | Time == 95) %>%
  filter(Species == "pnigra") %>% 
  ggplot(aes(x = Establishment, y = Nr_cells, fill = Scenario)) +
  geom_bar(stat = "identity", position="dodge") +
  facet_grid(Time ~ Species) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = full_cols)

est_pines %>%
  filter(is.na(Establishment) == FALSE) %>%
  filter(Time == 5 | Time == 95) %>%
  filter(Species == "psylvestris") %>% 
  ggplot(aes(x = Establishment, y = Nr_cells, fill = Scenario)) +
  geom_bar(stat = "identity", position="dodge") +
  facet_grid(Time ~ Species) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = full_cols)

est_pines %>%
  filter(is.na(Establishment) == FALSE) %>%
  filter(Time == 5 | Time == 95) %>%
  filter(Species == "phalepensis") %>% 
  ggplot(aes(x = Establishment, y = Nr_cells, fill = Scenario)) +
  geom_bar(stat = "identity", position="dodge") +
  facet_grid(Time ~ Species) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = full_cols)

est_pines %>%
  filter(is.na(Establishment) == FALSE) %>%
  filter(Time == 5 | Time == 95) %>%
  filter(Species == "qilex") %>% 
  ggplot(aes(x = Establishment, y = Nr_cells, fill = Scenario)) +
  geom_bar(stat = "identity", position="dodge") +
  facet_grid(Time ~ Species) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = full_cols)

est_pines %>%
  filter(is.na(Establishment) == FALSE) %>%
  filter(Time == 5 | Time == 95) %>%
  filter(Species == "qpyrenaica") %>% 
  ggplot(aes(x = Establishment, y = Nr_cells, fill = Scenario)) +
  geom_bar(stat = "identity", position="dodge") +
  facet_grid(Time ~ Species) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = full_cols)

est_pines %>%
  filter(is.na(Establishment) == FALSE) %>%
  filter(Time == 5 | Time == 95) %>%
  filter(Species == "qfaginea") %>% 
  ggplot(aes(x = Establishment, y = Nr_cells, fill = Scenario)) +
  geom_bar(stat = "identity", position="dodge") +
  facet_grid(Time ~ Species) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = full_cols)




est_dense_pines %>%
  filter(is.na(Establishment) == FALSE) %>%
  filter(Time == 5 | Time == 95) %>%
  filter(Species == "ppinaster") %>% 
  ggplot(aes(x = Establishment, y = Nr_cells, fill = Scenario)) +
  geom_bar(stat = "identity", position="dodge") +
  facet_grid(Time ~ Species) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = full_cols)

est_dense_pines %>%
  filter(is.na(Establishment) == FALSE) %>%
  filter(Time == 5 | Time == 95) %>%
  filter(Species == "pnigra") %>% 
  ggplot(aes(x = Establishment, y = Nr_cells, fill = Scenario)) +
  geom_bar(stat = "identity", position="dodge") +
  facet_grid(Time ~ Species) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = full_cols)

est_dense_pines %>%
  filter(is.na(Establishment) == FALSE) %>%
  filter(Time == 5 | Time == 95) %>%
  filter(Species == "psylvestris") %>% 
  ggplot(aes(x = Establishment, y = Nr_cells, fill = Scenario)) +
  geom_bar(stat = "identity", position="dodge") +
  facet_grid(Time ~ Species) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = full_cols)

est_dense_pines %>%
  filter(is.na(Establishment) == FALSE) %>%
  filter(Time == 5 | Time == 95) %>%
  filter(Species == "phalepensis") %>% 
  ggplot(aes(x = Establishment, y = Nr_cells, fill = Scenario)) +
  geom_bar(stat = "identity", position="dodge") +
  facet_grid(Time ~ Species) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = full_cols)

est_dense_pines %>%
  filter(is.na(Establishment) == FALSE) %>%
  filter(Time == 5 | Time == 95) %>%
  filter(Species == "qilex") %>% 
  ggplot(aes(x = Establishment, y = Nr_cells, fill = Scenario)) +
  geom_bar(stat = "identity", position="dodge") +
  facet_grid(Time ~ Species) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = full_cols)

est_dense_pines %>%
  filter(is.na(Establishment) == FALSE) %>%
  filter(Time == 5 | Time == 95) %>%
  filter(Species == "qpyrenaica") %>% 
  ggplot(aes(x = Establishment, y = Nr_cells, fill = Scenario)) +
  geom_bar(stat = "identity", position="dodge") +
  facet_grid(Time ~ Species) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = full_cols)

est_dense_pines %>%
  filter(is.na(Establishment) == FALSE) %>%
  filter(Time == 5 | Time == 95) %>%
  filter(Species == "qfaginea") %>% 
  ggplot(aes(x = Establishment, y = Nr_cells, fill = Scenario)) +
  geom_bar(stat = "identity", position="dodge") +
  facet_grid(Time ~ Species) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = full_cols)