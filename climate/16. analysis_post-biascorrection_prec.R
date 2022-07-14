### Analyse bias-corrected data

### SETUP ###
library(dplyr)
library(tidyverse)

di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/"

# Add climate regions names
Clim_regions <- data.frame(clim_region = c(1:4),
                           Climate_area = c('Subhumid', 'Xeric', 'Mesic', 'Alpine'))

## Check original vs bias-corrected data
# Load bias-corrected data
data <- read.table(paste0(di, "data/climate/data_biascorrected2.txt"), sep = ";", header = TRUE) %>%
  dplyr::select(-X) %>%
  dplyr::rename(Original = Prec,
         Adjusted = prec_corr)

# Plot observed vs modeled per month, region and model
jpeg(file = paste(di, "/harvest_x_climate_LANDIS/images/MIROC_adjustment_2.jpeg", sep = ""), width=18, height=12, units="in", res=300)
data %>%
  filter(Model == "MIROC5") %>%
  ggplot(aes(x = Original, y = Adjusted)) +
  geom_point() +
  facet_wrap(Region ~ Month, nrow = 4, ncol = 12) +
  geom_segment(aes(x = 0, y = 0, xend = 500, yend = 500), colour = "red") +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  ggtitle("MIROC model")
dev.off()

jpeg(file = paste(di, "/harvest_x_climate_LANDIS/images/ACCESS_adjustment_2.jpeg", sep = ""), width=18, height=12, units="in", res=300)
data %>%
  filter(Model == "ACCESS") %>%
  ggplot(aes(x = Original, y = Adjusted)) +
  geom_point() +
  facet_wrap(Region ~ Month, nrow = 4, ncol = 12) +
  geom_segment(aes(x = 0, y = 0, xend = 500, yend = 500), colour = "red") +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  ggtitle("ACCESS model")
dev.off()

jpeg(file = paste(di, "/harvest_x_climate_LANDIS/images/CESM1_adjustment_2.jpeg", sep = ""), width=18, height=12, units="in", res=300)
data %>%
  filter(Model == "CESM1") %>%
  ggplot(aes(x = Original, y = Adjusted)) +
  geom_point() +
  facet_wrap(Region ~ Month, nrow = 4, ncol = 12) +
  geom_segment(aes(x = 0, y = 0, xend = 500, yend = 500), colour = "red") +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  ggtitle("CESM1 model")
dev.off()

jpeg(file = paste(di, "/harvest_x_climate_LANDIS/images/CMCC_adjustment_2.jpeg", sep = ""), width=18, height=12, units="in", res=300)
data %>%
  filter(Model == "CMCC") %>%
  ggplot(aes(x = Original, y = Adjusted)) +
  geom_point() +
  facet_wrap(Region ~ Month, nrow = 4, ncol = 12) +
  geom_segment(aes(x = 0, y = 0, xend = 500, yend = 500), colour = "red") +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  ggtitle("CMCC model")
dev.off()

## Analysis of extreme values
ext_max <- data %>%
  filter(quant_prec == 1.0) %>%
  mutate(Region = as.factor(Region),
         Month = as.factor(Month),
         Year = as.factor(Year))
length(ext_max$quant_prec)

ext_min <- data %>%
  filter(quant_prec == 0)
length(ext_min$quant_prec)

## Compare observations and bias-corrected modeled data
# Load observations
obs <- read.table(paste0(di, "data/climate/obs_mod_data.txt"), sep = ";") %>%
  dplyr::select(-MOD_ACCESS, -MOD_CESM1, -MOD_CMCC, -MOD_MIROC5)

# Merge observations and bias-corrected data
merged <- data %>%
  filter(Year >= 1950, Year <= 2005,
         Scenario == "current") %>%
  dplyr::select(-quant_prec, -Original, -Scenario) %>%
  spread(Model, Adjusted) %>%
  dplyr::rename(clim_region = Region) %>%
  full_join(obs) %>%
  dplyr::rename(MOD_ACCESS = ACCESS,
                MOD_CESM1 = CESM1,
                MOD_CMCC = CMCC,
                MOD_MIROC5 = MIROC5) %>%
  dplyr::select(Year, Month, clim_region, OBS_monthly_prec,
                MOD_ACCESS, MOD_CESM1, MOD_CMCC, MOD_MIROC5)

# Calculate monthly average per climate region
avg <- merged %>%
  dplyr::select(-Year) %>%
  group_by(Month, clim_region) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  gather(Variable, Value, OBS_monthly_prec:MOD_MIROC5) %>%
  left_join(Clim_regions)

jpeg(file = "/Volumes/GoogleDrive/My Drive/proj_LANDIS/experiments/harvest_x_climate_LANDIS/images/prec_post-correction_2_revised.jpeg", 
     width=8, height=6, units="in", res=300)
ggplot(avg, aes(x = Month, y = Value, colour = Variable)) +
  geom_line(aes(linetype = Variable, color = Variable)) +
  geom_point(size = 0.4) +
  theme_classic() +
  facet_wrap(Climate_area ~ .) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_x_discrete("Month", limits = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")) +
  scale_y_continuous("Monthly precipitation (mm)") +
  ggtitle("After bias-correction")
dev.off()  
