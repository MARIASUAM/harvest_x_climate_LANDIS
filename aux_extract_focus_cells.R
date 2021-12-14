
### SETUP ###
di <- ".../experiments/data/" # Path to data folder

library(rgdal)
library(dplyr)

proj3042 <- CRS(SRS_string="EPSG:3042")

### FOCUS AREAS
focus_arana <- readOGR(dsn = di, layer = "focus_arana")
# plot(focus_arana)
crs(focus_arana) <- proj3042

focus_marquesado <- readOGR(dsn = di, layer = "focus_marquesado")
# plot(focus_marquesado)
crs(focus_marquesado) <- proj3042

### LOAD REFERENCE RASTER
ref <- raster(paste0(di, "dense_pines_mask.asc"))
# plot(ref)
crs(ref) <- proj3042
extent(ref)

# EXTRACT CELLS WITHIN EACH FOCUS AREA
arana <- raster::mask(ref, focus_arana)  
marquesado <- raster::mask(ref, focus_marquesado) 

cells_arana <- data.frame(Cell = 1:length(aver), Values = arana[]) %>%
  filter(is.na(Values) == FALSE) %>%
  mutate(Area = "Arana") %>%
  select(-Values)
cells_marquesado <- data.frame(Cell = 1:length(aver), Values = marquesado[]) %>%
  filter(is.na(Values) == FALSE) %>%
  mutate(Area = "Marquesado") %>%
  select(-Values)

focus_cells <- rbind(cells_arana, cells_marquesado)

write.table(focus_cells, paste0(di, "focus_cells.txt"), sep = ";", row.names = FALSE)
