# Check data used for bias correction

library(dplyr)

di <- "/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/"

data <- read.table(paste0(di, "data/climate/obs_mod_data.txt"), sep = ";")

check <- data %>%
  select(clim_region, Month) %>%
  group_by(clim_region, Month) %>%
  summarise(count = n())

test <- check %>%
  filter(clim_region == 4)

mean(test$count)
