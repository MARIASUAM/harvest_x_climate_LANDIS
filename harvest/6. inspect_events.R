# Inspect incoherences prescriptions map / summary - check Events file

library(ggplot2)
library(dplyr)
library(reshape)
library(raster)

di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/"
scenario <- c("210803b_proactive")

# Load MAs map and calculate surface
MA_map <- "management_areas.tif" # MA map 
MAs <- raster(paste(di, "0. sim_root/inputs_management/", MA_map, sep =""))
# plot(MAs)

MAs_surface <- data.frame(MA = MAs[]) %>%
  mutate(Surface = 1) %>% # each cell is 1 ha
  group_by(MA) %>%
  summarise(MA_surface = sum(Surface)) %>%
  filter(is.na(MA) != TRUE)

# Load events file and calculate % MA harvested under herbivory
events <- read.table(paste(di, scenario, "/output/harvest/event-log.csv", sep = ""), sep = ",", header = TRUE)

herb_events <- events %>%
  filter(Prescription == " Herbivory") %>%
  dplyr::select(Time, ManagementArea, NumberOfSites, HarvestedSites) %>%
  group_by(Time, ManagementArea) %>%
  summarise(Total_NumberOfSites = sum(NumberOfSites),
            Total_HarvestedSites = sum(HarvestedSites)) 

herb_events_perc <- herb_events %>%
  left_join(MAs_surface, by = c("ManagementArea" = "MA")) %>%
  mutate(Perc_over_NumberOfSites = (Total_NumberOfSites * 100) / MA_surface,
         Perc_over_HarvestedSites = (Total_HarvestedSites * 100) / MA_surface)

ggplot(herb_events_perc) +
  geom_point(aes(x = Time, y = Perc_over_NumberOfSites)) +
  facet_grid(ManagementArea ~ .)

ggplot(herb_events_perc) +
  geom_point(aes(x = Time, y = Perc_over_HarvestedSites)) +
  facet_grid(ManagementArea ~ .)

# Load prescriptions maps for comparison
time_steps <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
prescriptions <- stack()
prescriptions_df <- c()
for (i in 1:length(time_steps)){
    temp <- raster(paste(di, scenario, "/output/harvest/prescripts-", time_steps[i], ".tif", sep = ""))
    prescriptions <- stack(prescriptions, temp)
    
    temp_df <- data.frame(Prescripts = temp[]) %>%
      group_by(Prescripts) %>%
      summarise(Total_NumberOfSites = n()) %>%
      mutate(Time = time_steps[i])
    prescriptions_df <- rbind(prescriptions_df, temp_df)
}

# plot(prescriptions$prescripts.10)

# Create prescriptions legend:
legend_proactive <- data.frame(Code = c(0:8),
                               Meaning = c("Inactive", "Non disturbed", "PreThinning",
                                           "Thinning_noQ", "Reg_Cut_noQ",
                                           "Thinning_Q", "Reg_Cut_Q",
                                           "Coppice", "Herbivory"))

# Merge legend
prescriptions_df <- prescriptions_df %>%
  left_join(legend_proactive, by = c("Prescripts" = "Code"))

# Inspect Herbivory according to maps and according to event file
herb_maps <- prescriptions_df %>%
  filter(Meaning == "Herbivory") %>%
  mutate(Source = "Maps") %>%
  dplyr::select(Source, Time, Total_NumberOfSites)

ggplot(herb_maps, aes(x = Time, y = Total_NumberOfSites)) +
  geom_bar(stat = "identity") +
  theme_bw(base_size = 14) +
  ggtitle("Based on maps") +
  scale_y_continuous(limits = c(0, 175000))

herb_events <- events %>%
  filter(Prescription == " Herbivory") %>%
  dplyr::select(Time, NumberOfSites, HarvestedSites) %>%
  group_by(Time) %>%
  summarise(Total_NumberOfSites = sum(NumberOfSites),
            Total_HarvestedSites = sum(HarvestedSites)) %>%
  mutate(Source = "Event") %>%
  dplyr::select(Source, Time, Total_NumberOfSites, Total_HarvestedSites)

ggplot(herb_events, aes(x = Time, y = Total_NumberOfSites)) +
  geom_bar(stat = "identity") +
  theme_bw(base_size = 14) +
  ggtitle("Based on events - NumberOfSites") +
  scale_y_continuous(limits = c(0, 175000))

ggplot(herb_events, aes(x = Time, y = Total_HarvestedSites)) +
  geom_bar(stat = "identity") +
  theme_bw(base_size = 14) +
  ggtitle("Based on events - HarvestedSites") +
  scale_y_continuous(limits = c(0, 175000))

# Another check
total_cells <- MAs_surface$MA_surface[1] + MAs_surface$MA_surface[2]

herb_events <- herb_events %>%
  mutate(Perc_over_total_harve_sites = (Total_HarvestedSites * 100) / total_cells)
  