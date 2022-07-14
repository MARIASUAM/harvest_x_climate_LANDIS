## Compare modelled vs observed data: pre and post bias-correction

### SETUP ###
library(dplyr)
library(reshape)
library(tidyverse)

di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/"

# Load data
data <- read.table(paste0(di, "data/climate/obs_mod_data.txt"), sep = ";") %>%
  filter(is.na(clim_region) == FALSE) %>%
  mutate(clim_region = as.character(clim_region))

# Add climate regions names
Clim_regions <- data.frame(clim_region = c("1", "2","3","4"),
                           Climate_area = c('Subhumid', 'Xeric', 'Mesic', 'Alpine'))

# Calculate averages
avg_data <- data %>%
  dplyr::select(-Year) %>%
  group_by(Month, clim_region) %>%
  summarise_all(mean) %>%
  gather(variable, value, 
         OBS_monthly_prec:MOD_MIROC5, 
         factor_key = TRUE) %>%
  left_join(Clim_regions)

# Plot
jpeg(file = "/Volumes/GoogleDrive/My Drive/proj_LANDIS/experiments/harvest_x_climate_LANDIS/images/prec_pre-correction_revised.jpeg", 
                   width=8, height=6, units="in", res=300)
ggplot(avg_data, aes(x = Month, y = value, colour = variable)) +
  geom_line(aes(linetype = variable, color = variable)) +
  geom_point(size = 0.4) +
  theme_classic() +
  facet_wrap(Climate_area ~ .) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_x_discrete("Month", limits = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")) +
  scale_y_continuous("Monthly precipitation (mm)") +
  ggtitle("Previous to bias-correction")
dev.off()
