### SETUP 
library(dplyr)
library(ggplot2)
library(reshape2)
library(lubridate)

di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/"

# Load one set of age histograms

mgmt.scenarios <- data.frame(Scenario = c("210910_nomanag_current", "210910_nomanag_current_sppEstRad_MaxPest02", "210910_nomanag_current_sppEstRad_MaxPest10"),
                             Name = c("nomodification", "sppEstRad_MaxPest02", "sppEstRad_MaxPest10"))

histograms_time25 <- data.frame()
for (i in 1:length(mgmt.scenarios$Scenario)) {
  temp <- read.table(paste(di, mgmt.scenarios$Scenario[i], "/output/AgeDist/Age_25Histogram.csv", sep = ""), header = TRUE, sep = ",") %>%
    mutate(mgmt.scenario = mgmt.scenarios$Name[i])
  histograms_time25 <- rbind(histograms_time25, temp)
}

histograms_time35 <- data.frame()
for (i in 1:length(mgmt.scenarios$Scenario)) {
  temp <- read.table(paste(di, mgmt.scenarios$Scenario[i], "/output/AgeDist/Age_35Histogram.csv", sep = ""), header = TRUE, sep = ",") %>%
    mutate(mgmt.scenario = mgmt.scenarios$Name[i])
  histograms_time35 <- rbind(histograms_time35, temp)
}

histograms_time40 <- data.frame()
for (i in 1:length(mgmt.scenarios$Scenario)) {
  temp <- read.table(paste(di, mgmt.scenarios$Scenario[i], "/output/AgeDist/Age_40Histogram.csv", sep = ""), header = TRUE, sep = ",") %>%
    mutate(mgmt.scenario = mgmt.scenarios$Name[i])
  histograms_time40 <- rbind(histograms_time40, temp)
}

histograms_time50 <- data.frame()
for (i in 1:length(mgmt.scenarios$Scenario)) {
  temp <- read.table(paste(di, mgmt.scenarios$Scenario[i], "/output/AgeDist/Age_50Histogram.csv", sep = ""), header = TRUE, sep = ",") %>%
    mutate(mgmt.scenario = mgmt.scenarios$Name[i])
  histograms_time50 <- rbind(histograms_time50, temp)
}

histograms_time25 %>%
  filter(NrOfCohortsAtAge == "short") %>%
  ggplot(aes(x = mgmt.scenario, y = X.1_8.4.)) +
  geom_bar(stat = "identity") +
  facet_grid(NrOfCohortsAtAge ~ ., scales = "free_y") +
  theme_classic() +
  ggtitle("Time 25")

histograms_time35 %>%
  filter(NrOfCohortsAtAge == "short") %>%
  ggplot(aes(x = mgmt.scenario, y = X.1_9.4.)) +
  geom_bar(stat = "identity") +
  facet_grid(NrOfCohortsAtAge ~ ., scales = "free_y") +
  theme_classic() +
  ggtitle("Time 35")

histograms_time40 %>%
  filter(NrOfCohortsAtAge == "short") %>%
  ggplot(aes(x = mgmt.scenario, y = X.1_9.9.)) +
  geom_bar(stat = "identity") +
  facet_grid(NrOfCohortsAtAge ~ ., scales = "free_y") +
  theme_classic() +
  ggtitle("Time 40")

histograms_time50 %>%
  filter(NrOfCohortsAtAge == "short") %>%
  ggplot(aes(x = mgmt.scenario, y = X.1_10.9.)) +
  geom_bar(stat = "identity") +
  facet_grid(NrOfCohortsAtAge ~ ., scales = "free_y") +
  theme_classic() +
  ggtitle("Time 50")

