# Analyse mask of pine plantations

library(rgdal)
library(dplyr)
library(raster)

# Load management areas
mgmt_areas <- raster("/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/0. sim_root/inputs_management/management_areas.tif")
mas_surface <- data.frame(MA = mgmt_areas[])%>%
  filter(is.na(MA) != TRUE) %>%
  group_by(MA) %>%
  summarise(cells_in_ma = n())

# Load pine plantations and calculate percentage of cells of pines in each mgmt area
pines_mask <- raster("/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/data/dense_pines_mask.asc")
pines_in_mas <- data.frame(pines = pines_mask[],
                 MA = mgmt_areas[]) %>%
  filter(is.na(pines) != TRUE) %>%
  group_by(MA) %>%
  summarise(cells_of_pines_in_ma = n()) %>%
  left_join(mas_surface) %>%
  mutate(prop_pines = cells_of_pines_in_ma / cells_in_ma)

# Calculate percentage of cells of pines in whole active area
active_cells <- data.frame(MA = mgmt_areas[])
active_cells$MA[is.na(active_cells$MA) == FALSE] <- 1
active_cells <- active_cells %>%
  filter(is.na(MA) == FALSE) %>%
  group_by(MA) %>%
  summarise(nr_cells = n())

pines_surface <- data.frame(pines = pines_mask[])
pines_surface$pines[is.na(pines_surface$pines) == FALSE] <- 1
pines_surface <- pines_surface %>%
  filter(is.na(pines) == FALSE) %>%
  group_by(pines) %>%
  summarise(nr_cells = n())

pines_surface$nr_cells / active_cells$nr_cells
