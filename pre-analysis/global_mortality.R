# Mortality of the whole study area

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

cols <- c("conserv" = "#33A02C", # dark green
          "proactive" = "#6A3D9A", # dark purple
          "proactiveplus" = "#FF7F00", # orange
          "nomanag" = "#E31A1C") # red

lines <- c("current" = "solid", "rcp45" = "dotdash", "rcp85" = "dashed")

groups <- data.frame(Species = c("qfaginea", "qpyrenaica", "qilex", "ppinaster", "pnigra", "psylvestris", "phalepensis", "tall", "medium", "short", "jcommunis", "joxycedrus", "popnigra"),
                     Group = c("Oaks", "Oaks", "Oaks", "P. pinaster", "P. nigra", "P. sylvestris", "P. halepensis", "Other", "Other", "Other", "Other", "Other", "Other"))

# Load global mortality tables
all_replicates <- data.frame()
for (i in seq_along(mgmt.scenarios)) {
  for (j in seq_along(replicates)) {
    one_replicate <- read.table(paste(di, mgmt.scenarios[i], "_rep", replicates[j], "/output/MortalityTable.txt", sep =""), sep = ",", header = TRUE) %>% 
      mutate(Harv_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][2],
             Clim_scenario = strsplit(as.character(mgmt.scenarios[i]), split = "_")[[1]][3],
             Replicate = replicates[j])
    all_replicates <- rbind(all_replicates, one_replicate)
  }
}

# Average through replicates
gl_mort <- all_replicates %>%
  filter(Cause == " Succession") %>%
  select(-Replicate, -Cause) %>%
  group_by(Harv_scenario, Clim_scenario, Time) %>%
  summarise(jcommunis_avg = mean(jcommunis),
            joxycedrus_avg = mean(joxycedrus),
            phalepensis_avg = mean(phalepensis),
            pnigra_avg = mean(pnigra),
            ppinaster_avg = mean(ppinaster),
            psylvestris_avg = mean(psylvestris),
            popnigra_avg = mean(popnigra),
            qfaginea_avg = mean(qfaginea),
            qilex_avg = mean(qilex),
            qpyrenaica_avg = mean(qpyrenaica),
            short_avg = mean(short),
            medium_avg = mean(medium),
            tall_avg = mean(tall),
            jcommunis_sd = sd(jcommunis),
            joxycedrus_sd = sd(joxycedrus),
            phalepensis_sd = sd(phalepensis),
            pnigra_sd = sd(pnigra),
            ppinaster_sd = sd(ppinaster),
            psylvestris_sd = sd(psylvestris),
            popnigra_sd = sd(popnigra),
            qfaginea_sd = sd(qfaginea),
            qilex_sd = sd(qilex),
            qpyrenaica_sd = sd(qpyrenaica),
            short_sd = sd(short),
            medium_sd = sd(medium),
            tall_sd = sd(tall))

mean_gl_mort <- all_replicates %>%
  filter(Cause == " Succession") %>%
  select(-Replicate, -Cause) %>%
  group_by(Harv_scenario, Clim_scenario, Time) %>%
  summarise_all(mean)

jpeg(file = paste(di, outputs_folder, "Mortality - whole landscape.jpeg", sep = ""), width = 12, height = 8, units="in", res=300)
mean_gl_mort %>%
  melt(id.vars = c("Harv_scenario", "Clim_scenario", "Time")) %>%
  ggplot(aes(x = Time, y = value)) +
  geom_line(aes(linetype = Clim_scenario, color = Harv_scenario)) +
  geom_point(aes(color = Harv_scenario)) +
  theme_classic() +
  facet_wrap(variable ~ .) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lines) +
  ggtitle("Mortality avg across replicates")
dev.off()
