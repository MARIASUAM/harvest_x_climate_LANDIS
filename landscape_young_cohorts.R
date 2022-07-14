# Age distribution

library(dplyr)
library(ggplot2)

mgmt.scenarios <- c("211129_nomanag_current",
                    "211129_conserv_current",
                    "211129_proactive_current",
                    "211129_proactiveplus_current",
                    "211129_nomanag_rcp45",
                    "211129_conserv_rcp45",
                    "211129_proactive_rcp45",
                    "211129_proactiveplus_rcp45",
                    "211129_nomanag_rcp85",
                    "211129_conserv_rcp85",
                    "211129_proactive_rcp85",
                    "211129_proactiveplus_rcp85") # Folder names with each scenario

di <- "/Volumes/GoogleDrive/My Drive/proj_LANDIS/experiments/" # Path to simulations folder
outputs_folder <- "211129_outputs/" # Subfolder for outputs

age_dist <- data.frame()
for (i in seq_along(mgmt.scenarios)) {
  temp <- read.table(paste(di, mgmt.scenarios[i], "_rep1/output/AgeDist/Age_95Histogram.csv", sep =""), header = TRUE, sep = ",") %>% 
    mutate(Harv_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][2],
           Clim_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][3])
  age_dist <- rbind(age_dist, temp)
}

# Fetch data
age_dist_young <- age_dist %>%
  select(NrOfCohortsAtAge, X.1_15.4., Harv_scenario, Clim_scenario)

age_dist_young %>%
  filter(NrOfCohortsAtAge != "jcommunis",
         NrOfCohortsAtAge != "joxycedrus",
         NrOfCohortsAtAge != "short",
         NrOfCohortsAtAge != "medium",
         NrOfCohortsAtAge != "tall",
         NrOfCohortsAtAge != "Total",
         NrOfCohortsAtAge != "popnigra") %>%
  ggplot(aes(x = NrOfCohortsAtAge, y = X.1_15.4.)) +
  geom_point(aes(color = NrOfCohortsAtAge)) +
  facet_grid(Clim_scenario ~ Harv_scenario) + # , scales = "free_y"
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank())

groups <- age_dist_young %>%
  filter(NrOfCohortsAtAge != "jcommunis",
         NrOfCohortsAtAge != "joxycedrus",
         NrOfCohortsAtAge != "short",
         NrOfCohortsAtAge != "medium",
         NrOfCohortsAtAge != "tall",
         NrOfCohortsAtAge != "Total",
         NrOfCohortsAtAge != "popnigra")%>%
  mutate(Group = ifelse(NrOfCohortsAtAge == "qilex", "oak",
                        ifelse(NrOfCohortsAtAge == "qfaginea", "oak",
                               ifelse(NrOfCohortsAtAge == "qpyrenaica", "oak",
                                      "pine")))) %>%
  select(Group, X.1_15.4., Harv_scenario, Clim_scenario) %>%
  group_by(Group, Harv_scenario, Clim_scenario) %>%
  summarise(tot = sum(X.1_15.4.))

ggplot(groups, aes(x = Group, y = tot)) +
  geom_point(aes(color = Group)) +
  facet_grid(Clim_scenario ~ Harv_scenario) + # , scales = "free_y"
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank())                     
